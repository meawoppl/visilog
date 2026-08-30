//! The combinational simulation driver.
//!
//! [`Simulator`] takes a parsed [`VerilogModule`], declares every port, net,
//! register and parameter into a [`StateStore`], and then settles the module's
//! continuous assignments to a fixpoint:
//!
//! ```text
//! let mut simulator = Simulator::new(module);
//! simulator.setup()?;
//! simulator.set_input("a", Register::from_u128(3, 4))?;
//! simulator.run()?;
//! simulator.get("sum")?;
//! ```
//!
//! Sequential logic runs through [`Simulator::poke`], which drives an input and
//! then settles: any `always` block sensitive to the resulting edges executes,
//! its non-blocking updates commit, and continuous assignments re-propagate.
//! That repeats until nothing changes, which is Verilog's delta-cycle model.
//!
//! Module hierarchy is still a later milestone; [`Simulator::setup`] reports an
//! instantiation as unsupported rather than pretending to elaborate it.

use std::fmt;

use crate::parsers::{
    assignment::ContinuousAssignment,
    modules::{PortDirection, VerilogModule},
    statements::ModuleStatement,
};
use crate::register::Register;
use crate::simulator::eval::{eval, EvalError};
use crate::simulator::events;
use crate::simulator::exec::{commit_updates, drive, execute_statements, range_width};
use crate::simulator::state_store::StateStore;

/// Ceiling on delta cycles within a single settle. A design that keeps
/// producing edges past this is oscillating, not converging.
const MAX_DELTA_CYCLES: usize = 100;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SimulationError {
    /// A name with no entry in the [`StateStore`].
    UnknownSignal(String),
    /// [`Simulator::set_input`] was handed a name that is not an input port.
    NotAnInput(String),
    /// A module construct this milestone cannot execute.
    Unsupported(&'static str),
    /// An `assign` whose left hand side is not something that can be driven.
    UnsupportedTarget(String),
    /// [`Simulator::setup`] has not run yet.
    NotSetUp,
    /// The continuous assignments never stopped changing, which means the
    /// module contains a combinational loop.
    NoConvergence { passes: usize },
    /// The expression evaluator rejected an assignment's right hand side.
    Eval(EvalError),
}

impl fmt::Display for SimulationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SimulationError::UnknownSignal(name) => write!(f, "no signal named `{}`", name),
            SimulationError::NotAnInput(name) => write!(f, "`{}` is not an input port", name),
            SimulationError::Unsupported(what) => {
                write!(f, "{} is not supported by the combinational simulator", what)
            }
            SimulationError::UnsupportedTarget(text) => {
                write!(f, "cannot drive `{}`", text)
            }
            SimulationError::NotSetUp => write!(f, "the simulator has not been set up"),
            SimulationError::NoConvergence { passes } => write!(
                f,
                "continuous assignments did not settle in {} passes; the module has a combinational loop",
                passes
            ),
            SimulationError::Eval(error) => write!(f, "{}", error),
        }
    }
}

impl std::error::Error for SimulationError {}

impl From<EvalError> for SimulationError {
    fn from(error: EvalError) -> Self {
        SimulationError::Eval(error)
    }
}

/// A module whose continuous assignments can be settled against a set of input
/// values.
pub struct Simulator {
    module: VerilogModule,
    state: StateStore,
    assignments: Vec<ContinuousAssignment>,
    /// Indices into `module.statements`. `AlwaysBlock` is not `Clone`, and
    /// holding references would borrow the module for the simulator's lifetime.
    always_blocks: Vec<usize>,
    initial_blocks: Vec<usize>,
    inputs: Vec<String>,
    is_setup: bool,
}

impl Simulator {
    pub fn new(module: VerilogModule) -> Self {
        Self {
            module,
            state: StateStore::new(),
            assignments: Vec::new(),
            always_blocks: Vec::new(),
            initial_blocks: Vec::new(),
            inputs: Vec::new(),
            is_setup: false,
        }
    }

    /// Declares every signal the module names and collects its continuous
    /// assignments. Signals start out all `x`, the way an undriven Verilog net
    /// does; parameters are folded to their value immediately.
    pub fn setup(&mut self) -> Result<(), SimulationError> {
        self.state = StateStore::new();
        self.assignments.clear();
        self.always_blocks.clear();
        self.initial_blocks.clear();
        self.inputs.clear();
        self.is_setup = false;

        for port in &self.module.ports {
            self.state.declare(port.identifier.name.clone(), port.range);
            if matches!(port.direction, PortDirection::Input) {
                self.inputs.push(port.identifier.name.clone());
            }
        }

        for (index, statement) in self.module.statements.iter().enumerate() {
            match statement {
                ModuleStatement::WireDeclaration(nets) => {
                    for net in nets {
                        self.state
                            .declare(net.identifier().name.clone(), net.range());
                    }
                }
                ModuleStatement::RegisterDeclaration(register) => {
                    self.state
                        .declare(register.name.name.clone(), register.range.unwrap_or((0, 0)));
                }
                ModuleStatement::ParameterDeclaration(parameters) => {
                    for parameter in parameters {
                        let value = eval(&parameter.value, &self.state)?;
                        match parameter.range {
                            Some(range) => self.state.set_ranged(
                                parameter.name.name.clone(),
                                value.resize(range_width(range)),
                                range,
                            ),
                            None => self.state.set(parameter.name.name.clone(), value),
                        }
                    }
                }
                ModuleStatement::Assignment(assignment) => {
                    self.assignments.push(assignment.clone())
                }
                ModuleStatement::AlwaysBlock(_) => self.always_blocks.push(index),
                ModuleStatement::InitialBlock(_) => self.initial_blocks.push(index),
                ModuleStatement::ModuleInstantiation(_) => {
                    return Err(SimulationError::Unsupported("a module instantiation"))
                }
            }
        }

        self.is_setup = true;

        // `initial` blocks run once, at time zero, before anything is driven.
        // Only propagate if one of them actually ran: settling here otherwise
        // would make a module that never converges fail at setup rather than
        // at the point someone asks it to run.
        if !self.initial_blocks.is_empty() {
            let mut pending = Vec::new();
            for &index in &self.initial_blocks {
                if let ModuleStatement::InitialBlock(block) = &self.module.statements[index] {
                    pending.extend(execute_statements(&block.statements, &mut self.state)?);
                }
            }
            commit_updates(pending, &mut self.state)?;
            self.propagate()?;
        }

        Ok(())
    }

    /// Drives an input port. The value is resized to the port's declared width,
    /// keeping the least significant bits.
    pub fn set_input(&mut self, name: &str, value: Register) -> Result<(), SimulationError> {
        if !self.is_setup {
            return Err(SimulationError::NotSetUp);
        }
        if !self.inputs.iter().any(|input| input == name) {
            return Err(SimulationError::NotAnInput(name.to_string()));
        }
        let signal = self
            .state
            .get_signal(name)
            .ok_or_else(|| SimulationError::UnknownSignal(name.to_string()))?;
        let (width, range) = (signal.width(), signal.range());
        self.state.set_ranged(name, value.resize(width), range);
        Ok(())
    }

    /// The current value of any signal: an output port, an input, or an
    /// internal wire.
    pub fn get(&self, name: &str) -> Result<&Register, SimulationError> {
        self.state
            .get(name)
            .ok_or_else(|| SimulationError::UnknownSignal(name.to_string()))
    }

    /// Settles the continuous assignments.
    ///
    /// Continuous assignments form a dataflow graph but are stored in source
    /// order, so one pass over them can leave a consumer holding the stale value
    /// of a producer written later in the file. Passes are repeated until a full
    /// pass changes nothing, and the number of passes taken is returned.
    ///
    /// A pure dataflow graph of `n` assignments needs at most `n` passes to
    /// settle — each pass finalizes at least the next assignment in dependency
    /// order — plus one more to observe that nothing moved. The limit here is
    /// looser than that so a feedback path that latches onto a stable value
    /// still has room, but it is finite: a module that genuinely oscillates
    /// reports [`SimulationError::NoConvergence`] instead of hanging.
    pub fn run(&mut self) -> Result<usize, SimulationError> {
        if !self.is_setup {
            return Err(SimulationError::NotSetUp);
        }
        self.propagate()
    }

    /// Drives an input and settles the whole design: continuous assignments
    /// re-propagate, every `always` block sensitive to the resulting edges
    /// executes, and its non-blocking updates commit. Returns the delta cycles
    /// taken.
    ///
    /// This is the entry point for sequential logic — clocking a design means
    /// poking its clock, since it is the *edge* that wakes an `always` block.
    pub fn poke(&mut self, name: &str, value: Register) -> Result<usize, SimulationError> {
        let before = self.state.clone();
        self.set_input(name, value)?;
        self.propagate()?;
        self.settle(before)
    }

    /// One full clock pulse: low-to-high, then high-to-low. Edge-triggered
    /// logic acts on the rising half.
    pub fn tick(&mut self, clock: &str) -> Result<(), SimulationError> {
        self.poke(clock, Register::from_u128(1, 1))?;
        self.poke(clock, Register::from_u128(0, 1))?;
        Ok(())
    }

    /// Repeatedly wakes `always` blocks until the design stops changing.
    ///
    /// `before` is the state as of the last settled point. The difference
    /// between it and the current state is the set of edges that may wake a
    /// block; running those blocks can move more signals, which is itself a new
    /// set of edges. Verilog calls each of these rounds a delta cycle, and they
    /// repeat until a round produces no edges at all.
    ///
    /// A design that never stops producing edges — a bare `always` block that
    /// keeps toggling, say — reports [`SimulationError::NoConvergence`] rather
    /// than hanging.
    fn settle(&mut self, mut before: StateStore) -> Result<usize, SimulationError> {
        for delta in 1..=MAX_DELTA_CYCLES {
            let edges = events::edges_between(&before, &self.state);
            if edges.is_empty() {
                return Ok(delta - 1);
            }

            // Snapshot before running the blocks, so the next round's edges are
            // exactly what this round moved.
            before = self.state.clone();

            let mut pending = Vec::new();
            for &index in &self.always_blocks {
                if let ModuleStatement::AlwaysBlock(block) = &self.module.statements[index] {
                    if events::always_block_fires(block, &edges) {
                        pending.extend(execute_statements(&block.statements, &mut self.state)?);
                    }
                }
            }

            commit_updates(pending, &mut self.state)?;
            self.propagate()?;
        }

        Err(SimulationError::NoConvergence {
            passes: MAX_DELTA_CYCLES,
        })
    }

    /// Settles the continuous assignments alone. See [`Simulator::run`].
    fn propagate(&mut self) -> Result<usize, SimulationError> {
        let limit = 2 * self.assignments.len() + 4;
        for pass in 1..=limit {
            let mut changed = false;
            for assignment in &self.assignments {
                let value = eval(assignment.rhs(), &self.state)?;
                changed |= drive(&mut self.state, assignment.lhs(), &value)?;
            }
            if !changed {
                return Ok(pass);
            }
        }
        Err(SimulationError::NoConvergence { passes: limit })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::fs;
    use std::path::PathBuf;

    use crate::parsers::modules::parse_module_declaration;

    fn simulator_for(source: &str) -> Simulator {
        let (remaining, module) = parse_module_declaration(source).unwrap();
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        let mut simulator = Simulator::new(module);
        simulator.setup().unwrap();
        simulator
    }

    fn one() -> Register {
        Register::from_u128(1, 1)
    }

    fn zero() -> Register {
        Register::from_u128(0, 1)
    }

    fn simulator_for_example(name: &str) -> Simulator {
        let path: PathBuf = [
            env!("CARGO_MANIFEST_DIR"),
            "src",
            "verilog",
            "examples",
            name,
        ]
        .iter()
        .collect();
        let source = fs::read_to_string(&path).expect("unable to read example");
        simulator_for(&source)
    }

    #[test]
    fn test_simple_adder() {
        let mut simulator = simulator_for(
            r#"
            module adder(
                input [7:0] a,
                input [7:0] b,
                output [7:0] c
            );
                assign c = a + b;
            endmodule
        "#,
        );

        simulator
            .set_input("a", Register::from_u128(200, 8))
            .unwrap();
        simulator.set_input("b", Register::from_u128(0, 8)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("c").unwrap().to_u128(), Some(200));

        // The sum wraps at the width of the operands.
        simulator
            .set_input("b", Register::from_u128(100, 8))
            .unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("c").unwrap().to_u128(), Some(44));
    }

    #[test]
    fn test_simple_module_example() {
        let mut simulator = simulator_for_example("simple_module.v");

        // Every signal starts out unknown, at its declared width.
        assert_eq!(simulator.get("sum").unwrap().to_binary(), "xxxx");

        simulator.set_input("a", Register::from_u128(3, 4)).unwrap();
        simulator.set_input("b", Register::from_u128(5, 4)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("sum").unwrap().to_binary(), "1000");

        // [3:0] + [3:0] wraps rather than carrying out.
        simulator
            .set_input("a", Register::from_u128(15, 4))
            .unwrap();
        simulator.set_input("b", Register::from_u128(2, 4)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("sum").unwrap().to_u128(), Some(1));
    }

    #[test]
    fn test_simple_module_propagates_unknown() {
        let mut simulator = simulator_for_example("simple_module.v");

        // A carry can reach any output bit, so a single unknown input bit makes
        // the whole sum unknown.
        simulator
            .set_input("a", Register::from_binary("001x"))
            .unwrap();
        simulator.set_input("b", Register::from_u128(5, 4)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("sum").unwrap().to_binary(), "xxxx");
    }

    #[test]
    fn test_parity_calculator_example() {
        let mut simulator = simulator_for_example("parity_calculator.v");

        simulator
            .set_input("data", Register::from_binary("10110010"))
            .unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("parity").unwrap().to_binary(), "0");

        simulator
            .set_input("data", Register::from_binary("10110011"))
            .unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("parity").unwrap().to_binary(), "1");

        // Parity has no dominant value, so one unknown bit is enough.
        simulator
            .set_input("data", Register::from_binary("1011001x"))
            .unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("parity").unwrap().to_binary(), "x");
    }

    #[test]
    fn test_multi_stage_dataflow_needs_several_passes() {
        // The assignments are written consumer first, so a single pass would
        // leave `c` holding the stale, still unknown value of `b`.
        let mut simulator = simulator_for(
            r#"
            module chain(
                input [3:0] a,
                output [3:0] c
            );
                wire [3:0] b;
                assign c = b + 1;
                assign b = a + 1;
            endmodule
        "#,
        );

        simulator.set_input("a", Register::from_u128(4, 4)).unwrap();
        let passes = simulator.run().unwrap();

        assert_eq!(simulator.get("b").unwrap().to_u128(), Some(5));
        assert_eq!(simulator.get("c").unwrap().to_u128(), Some(6));
        // Two passes to settle, one more to notice nothing moved.
        assert_eq!(passes, 3);
    }

    #[test]
    fn test_combinational_loop_does_not_converge() {
        // A plain `assign a = ~a;` settles: `~x` is `x`. Case inequality gives a
        // definite answer for an unknown operand, so this really does oscillate.
        let mut simulator = simulator_for(
            r#"
            module oscillator(
                output a
            );
                assign a = a !== 1'b1;
            endmodule
        "#,
        );

        assert_eq!(
            simulator.run(),
            Err(SimulationError::NoConvergence { passes: 6 })
        );
    }

    #[test]
    fn test_bit_and_part_select_targets() {
        let mut simulator = simulator_for(
            r#"
            module packer(
                input [1:0] hi,
                input lo,
                output [3:0] out
            );
                assign out[3:2] = hi;
                assign out[0] = lo;
            endmodule
        "#,
        );

        simulator
            .set_input("hi", Register::from_binary("10"))
            .unwrap();
        simulator
            .set_input("lo", Register::from_binary("1"))
            .unwrap();
        simulator.run().unwrap();

        // Bit 1 is never driven and stays unknown.
        assert_eq!(simulator.get("out").unwrap().to_binary(), "10x1");
    }

    #[test]
    fn test_parameters_are_visible_to_assignments() {
        let mut simulator = simulator_for(
            r#"
            module offset(
                input [7:0] a,
                output [7:0] b
            );
                localparam BIAS = 8'd10;
                assign b = a + BIAS;
            endmodule
        "#,
        );

        simulator.set_input("a", Register::from_u128(5, 8)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("b").unwrap().to_u128(), Some(15));
    }

    #[test]
    fn test_edge_triggered_flip_flop_captures_on_rising_edge_only() {
        let mut simulator = simulator_for(
            r#"
            module dff(
                input clk,
                input d,
                output reg q
            );
                always @(posedge clk) q <= d;
            endmodule
        "#,
        );

        simulator.poke("d", one()).unwrap();
        // No clock edge yet, so the flop has not captured anything.
        assert_eq!(simulator.get("q").unwrap().to_binary(), "x");

        simulator.poke("clk", one()).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_binary(), "1");

        // A falling edge must not capture: drop `d` and clock low again.
        simulator.poke("d", zero()).unwrap();
        simulator.poke("clk", zero()).unwrap();
        assert_eq!(
            simulator.get("q").unwrap().to_binary(),
            "1",
            "a negedge must not capture in a posedge-triggered flop"
        );

        simulator.poke("clk", one()).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_binary(), "0");
    }

    /// A variable bit-select target — `q[addr] <= 1'b1;` — has to survive
    /// parsing as an expression index and then resolve against the runtime
    /// value of `addr`.
    #[test]
    fn test_variable_bit_select_target_writes_the_addressed_bit() {
        let mut simulator = simulator_for(
            r#"
            module bit_writer(
                input clk,
                input rst,
                input [1:0] addr,
                output reg [3:0] q
            );
                always @(posedge clk) begin
                    if (rst)
                        q <= 4'b0000;
                    else
                        q[addr] <= 1'b1;
                end
            endmodule
        "#,
        );

        simulator.set_input("rst", one()).unwrap();
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("q").unwrap().to_binary(), "0000");

        simulator.set_input("rst", zero()).unwrap();
        simulator
            .set_input("addr", Register::from_u128(2, 2))
            .unwrap();
        simulator.tick("clk").unwrap();
        assert_eq!(
            simulator.get("q").unwrap().to_binary(),
            "0100",
            "addr=2 must set bit 2, not a literal index"
        );

        simulator
            .set_input("addr", Register::from_u128(0, 2))
            .unwrap();
        simulator.tick("clk").unwrap();
        assert_eq!(
            simulator.get("q").unwrap().to_binary(),
            "0101",
            "the index follows addr from cycle to cycle"
        );
    }

    #[test]
    fn test_counter_example_counts_and_resets() {
        let mut simulator = simulator_for_example("counter.v");

        // Asynchronous reset: a posedge on `rst` clears the count with no clock.
        simulator.poke("rst", one()).unwrap();
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(0));

        simulator.poke("rst", zero()).unwrap();
        for expected in 1..=5u128 {
            simulator.tick("clk").unwrap();
            assert_eq!(
                simulator.get("count").unwrap().to_u128(),
                Some(expected),
                "count after {} ticks",
                expected
            );
        }

        // 4 bits, so it wraps rather than reaching 16.
        for _ in 6..=16 {
            simulator.tick("clk").unwrap();
        }
        assert_eq!(
            simulator.get("count").unwrap().to_u128(),
            Some(0),
            "a 4-bit counter wraps at 16"
        );

        // Reset again mid-count.
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(1));
        simulator.poke("rst", one()).unwrap();
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(0));
    }

    #[test]
    fn test_complex_module_example_pipelines_and_drives_tristate() {
        // sum <= temp; temp <= a + b; inside one posedge block, so a value
        // takes two clocks to reach `sum`. `data` is a continuous assign that
        // has to re-propagate after each clock.
        let mut simulator = simulator_for_example("complex_module.v");

        simulator.poke("rst", one()).unwrap();
        assert_eq!(simulator.get("sum").unwrap().to_u128(), Some(0));
        simulator.poke("rst", zero()).unwrap();

        simulator.poke("a", Register::from_u128(5, 4)).unwrap();
        simulator.poke("b", Register::from_u128(6, 4)).unwrap();

        // First clock loads temp; sum still holds the old temp.
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("temp").unwrap().to_u128(), Some(11));
        assert_eq!(simulator.get("sum").unwrap().to_u128(), Some(0));

        // Second clock walks it through to sum, and the assign follows.
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("sum").unwrap().to_u128(), Some(11));
        assert_eq!(
            simulator.get("data").unwrap().to_u128(),
            Some(11),
            "sum > 4'b1000, so data should be driven with sum"
        );
    }

    #[test]
    fn test_non_blocking_updates_are_visible_across_blocks() {
        // Two flops in series. With `<=` the second captures the *old* `a`, so
        // a value takes two clocks to walk the pipeline. Were these blocking,
        // `b` would take the new `a` and both would move in one clock.
        let mut simulator = simulator_for(
            r#"
            module pipeline(
                input clk,
                input d,
                output reg a,
                output reg b
            );
                always @(posedge clk) a <= d;
                always @(posedge clk) b <= a;
            endmodule
        "#,
        );

        simulator.poke("d", one()).unwrap();
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("a").unwrap().to_binary(), "1");
        assert_eq!(
            simulator.get("b").unwrap().to_binary(),
            "x",
            "b must capture the pre-clock a, not the new one"
        );

        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("b").unwrap().to_binary(), "1");
    }

    #[test]
    fn test_errors_on_bad_signal_names() {
        let mut simulator = simulator_for(
            r#"
            module buffer(
                input a,
                output b
            );
                assign b = a;
            endmodule
        "#,
        );

        assert_eq!(
            simulator.set_input("b", Register::from_binary("1")),
            Err(SimulationError::NotAnInput("b".to_string()))
        );
        assert_eq!(
            simulator.get("nope"),
            Err(SimulationError::UnknownSignal("nope".to_string()))
        );
    }

    #[test]
    fn test_methods_require_setup() {
        let (_, module) = parse_module_declaration("module empty(); endmodule").unwrap();
        let mut simulator = Simulator::new(module);

        assert_eq!(simulator.run(), Err(SimulationError::NotSetUp));
        assert_eq!(
            simulator.set_input("a", Register::from_binary("1")),
            Err(SimulationError::NotSetUp)
        );
    }
}

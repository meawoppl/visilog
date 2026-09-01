//! The simulation driver.
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
//! [`Simulator::advance`] moves simulated time forward, which is what gives
//! `#delay` meaning: a block that hits a delay suspends and re-queues itself
//! for a later timestamp. That also lets a design clock itself — an
//! `always begin #50 clk = ~clk; end` needs no external stimulus, just time.
//!
//! Module hierarchy is flattened at elaboration time by
//! [`crate::simulator::elaborate`]: a child's signals join the same
//! [`StateStore`] under qualified names (`dut.count`), and a port bound to a
//! plain identifier becomes the very same store entry as the parent signal it
//! was connected to. Hand the simulator more than one module with
//! [`Simulator::with_modules`].

use std::collections::HashMap;
use std::fmt;

use crate::parsers::{assignment::ContinuousAssignment, modules::VerilogModule};
use crate::register::Register;
use crate::simulator::elaborate::{elaborate, BlockKind, TimedBlock};
use crate::simulator::eval::{eval, EvalError};
use crate::simulator::event_queue::{EventQueue, ExecutionCursor};
use crate::simulator::events;
use crate::simulator::exec::{commit_updates, drive, PendingUpdate};
use crate::simulator::program::{self, Resume};
use crate::simulator::state_store::StateStore;

/// Ceiling on delta cycles within a single settle. A design that keeps
/// producing edges past this is oscillating, not converging.
const MAX_DELTA_CYCLES: usize = 100;

/// Ceiling on block resumptions within a single timestamp. A free-running
/// `always` block with no delay in it restarts forever without time moving;
/// this turns that into an error rather than a hang.
const MAX_RESUMPTIONS_PER_TIME: usize = 10_000;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SimulationError {
    /// A name with no entry in the [`StateStore`].
    UnknownSignal(String),
    /// [`Simulator::set_input`] was handed a name that is not an input port.
    NotAnInput(String),
    /// A module construct this milestone cannot execute.
    Unsupported(&'static str),
    /// An instantiation, or a top-level name, that no supplied module defines.
    UnknownModule(String),
    /// A named port connection for a port the instantiated module does not have.
    UnknownPort { module: String, port: String },
    /// A `#(...)` override for a parameter the instantiated module does not
    /// declare.
    UnknownParameter { module: String, parameter: String },
    /// More positional arguments than the module has ports (or parameters).
    TooManyArguments {
        module: String,
        what: &'static str,
        expected: usize,
        found: usize,
    },
    /// An output port connected to something that is not a plain signal. The
    /// child drives it, and there is no way to push a value back out through an
    /// arbitrary expression.
    UndrivablePort {
        instance: String,
        port: String,
        connection: String,
    },
    /// A module that instantiates itself, directly or around a cycle. No amount
    /// of flattening terminates on that.
    RecursiveInstantiation(String),
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
                write!(f, "{} is not supported by the simulator", what)
            }
            SimulationError::UnknownModule(name) => write!(f, "no module named `{}`", name),
            SimulationError::UnknownPort { module, port } => {
                write!(f, "module `{}` has no port `{}`", module, port)
            }
            SimulationError::UnknownParameter { module, parameter } => {
                write!(f, "module `{}` has no parameter `{}`", module, parameter)
            }
            SimulationError::TooManyArguments {
                module,
                what,
                expected,
                found,
            } => write!(
                f,
                "module `{}` has {} {}, but {} were supplied",
                module, expected, what, found
            ),
            SimulationError::UndrivablePort {
                instance,
                port,
                connection,
            } => write!(
                f,
                "output port `{}` of instance `{}` is connected to `{}`, which cannot be driven",
                port, instance, connection
            ),
            SimulationError::RecursiveInstantiation(name) => {
                write!(f, "module `{}` instantiates itself", name)
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

/// A parsed design, elaborated into signals and runnable blocks.
pub struct Simulator {
    /// Every module the design may draw on. Only the top one is walked
    /// directly; the rest are reached through instantiations.
    modules: Vec<VerilogModule>,
    /// The name of the module to elaborate. Resolved in `setup` rather than in
    /// the constructor so that every elaboration error surfaces from one place.
    top: String,
    state: StateStore,
    assignments: Vec<ContinuousAssignment>,
    blocks: Vec<TimedBlock>,
    /// Qualified names of ports that were aliased onto a parent signal, so they
    /// can still be read back even though they hold no state of their own.
    aliases: HashMap<String, String>,
    queue: EventQueue,
    now: i64,
    inputs: Vec<String>,
    is_setup: bool,
}

impl Simulator {
    /// A design of exactly one module, which is therefore the top.
    pub fn new(module: VerilogModule) -> Self {
        let top = module.identifier.name.clone();
        Self::with_modules(vec![module], top)
    }

    /// A design of several modules, elaborated from `top` downwards.
    ///
    /// A module named by an instantiation is looked up here, so the order of
    /// `modules` does not matter. A `top` that names nothing is reported by
    /// [`Simulator::setup`] as [`SimulationError::UnknownModule`].
    pub fn with_modules(modules: Vec<VerilogModule>, top: impl Into<String>) -> Self {
        Self {
            modules,
            top: top.into(),
            state: StateStore::new(),
            assignments: Vec::new(),
            blocks: Vec::new(),
            aliases: HashMap::new(),
            queue: EventQueue::new(),
            now: 0,
            inputs: Vec::new(),
            is_setup: false,
        }
    }

    /// Elaborates the design: declares every signal the top module and
    /// everything it instantiates names, and collects their continuous
    /// assignments and procedural blocks.
    ///
    /// Signals start out all `x`, the way an undriven Verilog net does;
    /// parameters are folded to their value immediately. A child's signals join
    /// the same flat store under qualified names, so `dut.count` is read back
    /// exactly like a local one.
    pub fn setup(&mut self) -> Result<(), SimulationError> {
        self.state = StateStore::new();
        self.assignments.clear();
        self.blocks.clear();
        self.aliases.clear();
        self.queue = EventQueue::new();
        self.now = 0;
        self.inputs.clear();
        self.is_setup = false;

        let top = self
            .modules
            .iter()
            .position(|module| module.identifier.name == self.top)
            .ok_or_else(|| SimulationError::UnknownModule(self.top.clone()))?;
        let elaborated = elaborate(&self.modules, top)?;
        self.state = elaborated.state;
        self.assignments = elaborated.assignments;
        self.blocks = elaborated.blocks;
        self.inputs = elaborated.inputs;
        self.aliases = elaborated.aliases;

        self.is_setup = true;

        // Everything that starts on its own starts at time zero: `initial`
        // blocks, which run once, and free-running `always` blocks, which have
        // no event to wait for. Edge-triggered blocks are not queued — they are
        // woken by `settle`.
        for id in 0..self.blocks.len() {
            if self.blocks[id].kind == BlockKind::Initial || self.blocks[id].free_running {
                self.queue.insert(0, ExecutionCursor::new(id, 0));
            }
        }

        // Drain time zero. This is a no-op for a module with no procedural
        // blocks, which matters: settling unconditionally here would make a
        // module that never converges fail at setup rather than when someone
        // actually asks it to run.
        self.advance(0)?;

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

    /// The current value of any signal: an output port, an input, an internal
    /// wire, or anything inside an instance at its qualified name (`dut.count`).
    ///
    /// A child port that was aliased onto a parent signal has no store entry of
    /// its own, so its qualified spelling is looked up through the alias table
    /// and reads back the signal it shares.
    pub fn get(&self, name: &str) -> Result<&Register, SimulationError> {
        if let Some(register) = self.state.get(name) {
            return Ok(register);
        }
        self.aliases
            .get(name)
            .and_then(|canonical| self.state.get(canonical))
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
        // The marker goes down before the input is written, so writing it is
        // itself an edge. Without that, nothing an edge-triggered block waits
        // on ever appears to move and the design never wakes.
        self.state.clear_changes();
        self.set_input(name, value)?;
        self.propagate()?;
        self.settle()
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
    /// The store journals what is written to it, so the changes it has recorded
    /// since the caller's marker are the set of edges that may wake a block;
    /// running those blocks can move more signals, which is itself a new set of
    /// edges. Verilog calls each of these rounds a delta cycle, and they repeat
    /// until a round produces no edges at all.
    ///
    /// Taking the changes also resets the marker, which is what keeps the
    /// rounds separate: round N+1 reacts to exactly what round N moved, never
    /// to an accumulation across rounds.
    ///
    /// A design that never stops producing edges — a bare `always` block that
    /// keeps toggling, say — reports [`SimulationError::NoConvergence`] rather
    /// than hanging.
    fn settle(&mut self) -> Result<usize, SimulationError> {
        for delta in 1..=MAX_DELTA_CYCLES {
            // Taking the changes here, before the blocks run, is what makes the
            // next round's edges exactly what this round moves.
            let changes = self.state.take_changes();
            let edges = events::edges_from_changes(changes, &self.state);
            if edges.is_empty() {
                return Ok(delta - 1);
            }

            let mut pending = Vec::new();
            for id in 0..self.blocks.len() {
                // A free-running `always` waits on nothing, so `always_block_fires`
                // would report it as firing on every edge. It is driven by time,
                // not by edges, so it is skipped here.
                if self.blocks[id].kind != BlockKind::Always || self.blocks[id].free_running {
                    continue;
                }
                if self.blocks[id].fires(&edges) {
                    let (updates, _) = self.resume_block(id, 0)?;
                    pending.extend(updates);
                }
            }

            commit_updates(pending, &mut self.state)?;
            self.propagate()?;
        }

        Err(SimulationError::NoConvergence {
            passes: MAX_DELTA_CYCLES,
        })
    }

    /// The current simulated time.
    pub fn now(&self) -> i64 {
        self.now
    }

    /// Runs simulated time forward by `duration` time units, executing every
    /// scheduled block resumption along the way.
    ///
    /// This is what makes `#delay` mean something. A block that hits a delay
    /// suspends and re-queues itself for a later timestamp; advancing time is
    /// what brings it back. It is also what drives a self-clocking design —
    /// `always begin #50 clk = ~clk; end` needs no external stimulus at all,
    /// just time.
    pub fn advance(&mut self, duration: i64) -> Result<(), SimulationError> {
        if !self.is_setup {
            return Err(SimulationError::NotSetUp);
        }

        let target = self.now + duration;
        while let Some(time) = self.queue.peek_time() {
            if time > target {
                break;
            }
            self.now = time;

            // Everything the resumptions below move is an edge for the settle
            // that follows them.
            self.state.clear_changes();
            let mut pending = Vec::new();
            let mut resumptions = 0;

            // Everything due at this timestamp runs before time moves on,
            // including anything re-queued for this same instant.
            while self.queue.peek_time() == Some(time) {
                resumptions += 1;
                if resumptions > MAX_RESUMPTIONS_PER_TIME {
                    return Err(SimulationError::NoConvergence {
                        passes: resumptions,
                    });
                }

                let (_, cursor) = self.queue.pop().expect("peeked time must pop");
                let (updates, halted) = self.resume_block(cursor.block, cursor.pc)?;
                pending.extend(updates);

                // A free-running `always` restarts the moment it finishes,
                // which is how `always begin #50 … end` keeps going forever.
                if halted && self.blocks[cursor.block].free_running {
                    self.queue
                        .insert(self.now, ExecutionCursor::new(cursor.block, 0));
                }
            }

            commit_updates(pending, &mut self.state)?;
            self.propagate()?;
            self.settle()?;
        }

        self.now = target;
        Ok(())
    }

    /// Resumes one block, queueing its continuation if it hits a delay. Returns
    /// its deferred updates and whether it ran to the end.
    fn resume_block(
        &mut self,
        id: usize,
        pc: usize,
    ) -> Result<(Vec<PendingUpdate>, bool), SimulationError> {
        match program::resume(&self.blocks[id].program, pc, &mut self.state)? {
            Resume::Halted { pending } => Ok((pending, true)),
            Resume::Suspended { pc, delay, pending } => {
                self.queue
                    .insert(self.now + delay, ExecutionCursor::new(id, pc));
                Ok((pending, false))
            }
        }
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
    fn test_initial_block_stimulus_lands_at_its_scheduled_times() {
        let mut simulator = simulator_for(
            r#"
            module stimulus(
                output reg a,
                output reg b
            );
                initial begin
                    a = 1'b0;
                    #10 a = 1'b1;
                    #10 b = 1'b1;
                end
            endmodule
        "#,
        );

        // The statements before the first delay have already run at time zero.
        assert_eq!(simulator.now(), 0);
        assert_eq!(simulator.get("a").unwrap().to_binary(), "0");
        assert_eq!(simulator.get("b").unwrap().to_binary(), "x");

        // Stopping short of the delay must not run the next statement.
        simulator.advance(9).unwrap();
        assert_eq!(simulator.now(), 9);
        assert_eq!(simulator.get("a").unwrap().to_binary(), "0");

        simulator.advance(1).unwrap();
        assert_eq!(simulator.now(), 10);
        assert_eq!(simulator.get("a").unwrap().to_binary(), "1");
        assert_eq!(simulator.get("b").unwrap().to_binary(), "x");

        simulator.advance(10).unwrap();
        assert_eq!(simulator.get("b").unwrap().to_binary(), "1");
    }

    #[test]
    fn test_free_running_always_block_generates_a_clock() {
        let mut simulator = simulator_for(
            r#"
            module oscillator(
                output reg clk
            );
                initial clk = 1'b0;
                always begin
                    #50 clk = ~clk;
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.get("clk").unwrap().to_binary(), "0");

        simulator.advance(50).unwrap();
        assert_eq!(simulator.get("clk").unwrap().to_binary(), "1");

        simulator.advance(50).unwrap();
        assert_eq!(simulator.get("clk").unwrap().to_binary(), "0");

        // Several periods in one call.
        simulator.advance(150).unwrap();
        assert_eq!(simulator.now(), 250);
        assert_eq!(simulator.get("clk").unwrap().to_binary(), "1");
    }

    #[test]
    fn test_self_clocking_counter_runs_on_time_alone() {
        // No pokes, no ticks — the design drives itself. The free-running block
        // makes the clock, and the edge-triggered block counts its posedges.
        let mut simulator = simulator_for(
            r#"
            module self_clocked(
                output reg [3:0] count,
                output reg clk
            );
                initial begin
                    clk = 1'b0;
                    count = 4'b0000;
                end
                always begin
                    #10 clk = ~clk;
                end
                always @(posedge clk) count <= count + 1;
            endmodule
        "#,
        );

        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(0));

        // clk rises at 10, 30, 50, 70, 90 — five posedges by time 100.
        simulator.advance(100).unwrap();
        assert_eq!(simulator.now(), 100);
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(5));

        simulator.advance(100).unwrap();
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(10));
    }

    #[test]
    fn test_delay_nested_in_a_conditional_schedules_correctly() {
        // The resume point is inside the `if` body, so this only works because
        // blocks compile to a flat program with a program counter.
        let mut simulator = simulator_for(
            r#"
            module gated(
                output reg a,
                output reg done
            );
                initial begin
                    a = 1'b1;
                    if (a) begin
                        #25 done = 1'b1;
                    end
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.get("done").unwrap().to_binary(), "x");
        simulator.advance(24).unwrap();
        assert_eq!(simulator.get("done").unwrap().to_binary(), "x");
        simulator.advance(1).unwrap();
        assert_eq!(simulator.get("done").unwrap().to_binary(), "1");
    }

    #[test]
    fn test_free_running_block_without_a_delay_is_reported_not_hung() {
        // `always begin a = ~a; end` has no delay, so it restarts forever
        // without time advancing. That has to be an error, not a hang.
        let (_, module) = parse_module_declaration(
            r#"
            module spinner(
                output reg a
            );
                initial a = 1'b0;
                always begin
                    a = ~a;
                end
            endmodule
        "#,
        )
        .unwrap();

        let mut simulator = Simulator::new(module);
        assert!(matches!(
            simulator.setup(),
            Err(SimulationError::NoConvergence { .. })
        ));
    }

    #[test]
    fn test_clock_divider_example_resets_and_counts() {
        let mut simulator = simulator_for_example("clock_divider.v");

        // Asynchronous reset clears both the counter and the divided output.
        simulator.poke("rst", one()).unwrap();
        assert_eq!(simulator.get("counter").unwrap().to_u128(), Some(0));
        assert_eq!(simulator.get("divided_clk").unwrap().to_binary(), "0");

        simulator.poke("rst", zero()).unwrap();
        for expected in 1..=5u128 {
            simulator.tick("clk").unwrap();
            assert_eq!(simulator.get("counter").unwrap().to_u128(), Some(expected));
        }

        // The divide threshold is 50_000_000, so the output must not have moved
        // yet. Reaching it by simulation is not practical; the threshold logic
        // itself is covered at a testable scale by the test below.
        assert_eq!(simulator.get("divided_clk").unwrap().to_binary(), "0");

        simulator.poke("rst", one()).unwrap();
        assert_eq!(simulator.get("counter").unwrap().to_u128(), Some(0));
    }

    #[test]
    fn test_divider_pattern_toggles_at_its_threshold() {
        // Same shape as `clock_divider.v` — `counter <= counter + 1;` followed
        // by a nested `if` that also assigns `counter` — but with a threshold a
        // test can actually reach. Both writes are non-blocking, so the later
        // one wins and the counter wraps rather than reaching 4.
        let mut simulator = simulator_for(
            r#"
            module small_divider(
                input clk,
                input rst,
                output reg out
            );
                reg [3:0] counter;
                always @(posedge clk or posedge rst) begin
                    if (rst) begin
                        counter <= 4'b0;
                        out <= 1'b0;
                    end else begin
                        counter <= counter + 1;
                        if (counter == 4'd3) begin
                            counter <= 4'b0;
                            out <= ~out;
                        end
                    end
                end
            endmodule
        "#,
        );

        simulator.poke("rst", one()).unwrap();
        simulator.poke("rst", zero()).unwrap();

        let mut seen = Vec::new();
        for _ in 0..8 {
            simulator.tick("clk").unwrap();
            seen.push((
                simulator.get("counter").unwrap().to_u128().unwrap(),
                simulator.get("out").unwrap().to_binary(),
            ));
        }

        let expected: Vec<(u128, String)> = [1, 2, 3, 0, 1, 2, 3, 0]
            .iter()
            .zip(["0", "0", "0", "1", "1", "1", "1", "0"])
            .map(|(count, out)| (*count as u128, out.to_string()))
            .collect();
        assert_eq!(seen, expected, "counter should wrap at 3 and toggle `out`");
    }

    #[test]
    fn test_spi_controller_example_resets_and_drives_its_outputs() {
        let mut simulator = simulator_for_example("spi_controller.v");

        simulator.poke("rst", one()).unwrap();
        assert_eq!(simulator.get("state").unwrap().to_binary(), "00", "IDLE");
        assert_eq!(simulator.get("data").unwrap().to_binary(), "00000000");
        // assign miso = data[7];
        assert_eq!(simulator.get("miso").unwrap().to_binary(), "0");
        // assign cs = (state == IDLE) ? 1 : 0;
        assert_eq!(simulator.get("cs").unwrap().to_binary(), "1");

        // assign sclk = clk; — a continuous assign straight through.
        simulator.poke("clk", one()).unwrap();
        assert_eq!(simulator.get("sclk").unwrap().to_binary(), "1");
        simulator.poke("clk", zero()).unwrap();
        assert_eq!(simulator.get("sclk").unwrap().to_binary(), "0");

        // The module cannot leave IDLE, and that is faithful rather than a
        // simulator bug: `cs` is an *output* driven from `state`, so IDLE forces
        // `cs` to 1, while the IDLE arm only advances when `cs == 0`. The
        // example has no way to drive `cs` externally.
        simulator.poke("rst", zero()).unwrap();
        for _ in 0..5 {
            simulator.tick("clk").unwrap();
        }
        assert_eq!(
            simulator.get("state").unwrap().to_binary(),
            "00",
            "cs is driven from state, so IDLE is self-latching in this module"
        );
    }

    #[test]
    fn test_every_example_module_simulates() {
        let dir: PathBuf = [env!("CARGO_MANIFEST_DIR"), "src", "verilog", "examples"]
            .iter()
            .collect();
        let mut paths: Vec<PathBuf> = fs::read_dir(dir)
            .expect("unable to read examples")
            .map(|entry| entry.expect("unable to read entry").path())
            .filter(|path| path.is_file())
            .collect();
        paths.sort();
        assert_eq!(paths.len(), 6, "expected six example modules");

        for path in paths {
            let name = path.file_name().unwrap().to_string_lossy().to_string();
            let source = fs::read_to_string(&path).expect("unable to read example");
            let (remaining, module) = parse_module_declaration(&source)
                .unwrap_or_else(|error| panic!("{} should parse: {:?}", name, error));
            assert!(remaining.trim().is_empty(), "{} left {:?}", name, remaining);

            let mut simulator = Simulator::new(module);
            simulator
                .setup()
                .unwrap_or_else(|error| panic!("{} should set up: {}", name, error));

            // Drive whatever stimulus each module happens to have. A module
            // without a given port is fine; anything else is a real failure.
            for (port, value) in [("rst", one()), ("rst", zero()), ("clk", one())] {
                match simulator.poke(port, value) {
                    Ok(_) | Err(SimulationError::NotAnInput(_)) => {}
                    Err(error) => panic!("{} failed driving {}: {}", name, port, error),
                }
            }
            simulator
                .advance(10)
                .unwrap_or_else(|error| panic!("{} should advance time: {}", name, error));
        }
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

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
//! Sequential constructs (`always` / `initial` blocks) and module hierarchy are
//! a later milestone; [`Simulator::setup`] reports them as unsupported rather
//! than pretending to execute them.

use std::fmt;

use crate::parsers::{
    assignment::ContinuousAssignment,
    expr::Expression,
    modules::{PortDirection, VerilogModule},
    statements::ModuleStatement,
};
use crate::register::Register;
use crate::simulator::eval::{eval, EvalError};
use crate::simulator::state_store::StateStore;

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
    inputs: Vec<String>,
    is_setup: bool,
}

impl Simulator {
    pub fn new(module: VerilogModule) -> Self {
        Self {
            module,
            state: StateStore::new(),
            assignments: Vec::new(),
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
        self.inputs.clear();
        self.is_setup = false;

        for port in &self.module.ports {
            self.state.declare(port.identifier.name.clone(), port.range);
            if matches!(port.direction, PortDirection::Input) {
                self.inputs.push(port.identifier.name.clone());
            }
        }

        for statement in &self.module.statements {
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
                ModuleStatement::AlwaysBlock(_) => {
                    return Err(SimulationError::Unsupported("an always block"))
                }
                ModuleStatement::InitialBlock(_) => {
                    return Err(SimulationError::Unsupported("an initial block"))
                }
                ModuleStatement::ModuleInstantiation(_) => {
                    return Err(SimulationError::Unsupported("a module instantiation"))
                }
            }
        }

        self.is_setup = true;
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

fn range_width(range: (i64, i64)) -> usize {
    ((range.0 - range.1).unsigned_abs() + 1) as usize
}

/// Writes `value` into whatever `target` names, reporting whether the stored
/// state actually moved. The value is resized to the width of the target the
/// way a Verilog assignment is: wider values lose their high bits, narrower
/// ones are zero extended.
fn drive(
    state: &mut StateStore,
    target: &Expression,
    value: &Register,
) -> Result<bool, SimulationError> {
    match target {
        Expression::Identifier(id) => {
            let signal = state
                .get_signal(&id.name)
                .ok_or_else(|| SimulationError::UnknownSignal(id.name.clone()))?;
            let (width, range) = (signal.width(), signal.range());
            let value = value.resize(width);
            if signal.register() == &value {
                return Ok(false);
            }
            state.set_ranged(id.name.clone(), value, range);
            Ok(true)
        }
        Expression::BitSelect(id, index) => {
            let index = target_index(state, index)?;
            drive_bits(state, &id.name, &[index], value)
        }
        Expression::PartSelect(id, first, second) => {
            let first = target_index(state, first)?;
            let second = target_index(state, second)?;
            // Indices run most significant bit first, matching the bit order of
            // the register being written.
            let indices: Vec<i64> = if first >= second {
                (second..=first).rev().collect()
            } else {
                (first..=second).collect()
            };
            drive_bits(state, &id.name, &indices, value)
        }
        other => Err(SimulationError::UnsupportedTarget(
            other.to_contracted_string(),
        )),
    }
}

/// A bit index on the left of an assignment has to be a constant, so anything
/// that does not evaluate to a plain number is a target this driver cannot use.
fn target_index(state: &StateStore, expr: &Expression) -> Result<i64, SimulationError> {
    eval(expr, state)?
        .to_u128()
        .and_then(|value| i64::try_from(value).ok())
        .ok_or_else(|| SimulationError::UnsupportedTarget(expr.to_contracted_string()))
}

fn drive_bits(
    state: &mut StateStore,
    name: &str,
    indices: &[i64],
    value: &Register,
) -> Result<bool, SimulationError> {
    let value = value.resize(indices.len());
    let signal = state
        .get_signal_mut(name)
        .ok_or_else(|| SimulationError::UnknownSignal(name.to_string()))?;
    let mut changed = false;
    for (offset, &index) in indices.iter().enumerate() {
        changed |= signal.set_bit(index, value.get_raw()[offset]);
    }
    Ok(changed)
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
    fn test_always_blocks_are_unsupported() {
        let (_, module) = parse_module_declaration(
            r#"
            module latch(
                input clk,
                input d,
                output reg q
            );
                always @(posedge clk) q <= d;
            endmodule
        "#,
        )
        .unwrap();

        let mut simulator = Simulator::new(module);
        assert_eq!(
            simulator.setup(),
            Err(SimulationError::Unsupported("an always block"))
        );
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

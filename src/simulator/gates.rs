//! The built-in gate primitives, and the net resolution they need.
//!
//! A gate is a **continuous driver**: it belongs with the `assign` statements
//! in the settle loop, not with the procedural blocks, and
//! [`Simulator::propagate`](crate::simulator::runner::Simulator) re-evaluates
//! it on every pass until the design stops moving.
//!
//! # Why resolution exists
//!
//! An ordinary `assign` writes its net and that is that, so the simulator has
//! never needed to ask what happens when two drivers disagree. A three-state
//! bus is the construct that forces the question: `bufif1 (bus, a, ena);` and
//! `bufif1 (bus, b, enb);` are two drivers of one net, each `z` when it is not
//! enabled, and *whichever wrote last* is exactly the wrong answer. So a net
//! with a gate on it is resolved instead — every driver contributes a value and
//! a strength, and [`resolve_bit`] decides.
//!
//! Strength is what makes a `pullup` mean anything: it drives a `1` at `pull`
//! strength, which loses to any gate actually driving the net and wins when
//! they are all off. The same mechanism carries `(highz0, strong1)`, the open
//! drain that floats instead of driving its `0`.
//!
//! # What is not modelled
//!
//! A gate's delay is parsed and ignored — the gate settles in zero time along
//! with every other continuous driver. The `r`-prefixed switches (`rnmos`,
//! `rcmos`, …) reduce the strength of what they pass, which is not modelled
//! either, so they behave as their non-resistive counterparts. The bidirectional
//! switches (`tran`, `tranif0`, …) conduct both ways and have no output
//! terminal at all; they are a **named error** at elaboration rather than a
//! driver that quietly does nothing.

use crate::parsers::expr::Expression;
use crate::parsers::gates::{DriveStrength, GateKind, StrengthLevel};
use crate::register::{Register, ONE, X, Z, ZERO};
use crate::simulator::eval::eval;
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::StateStore;

/// A bidirectional pass switch conducts in both directions, so it has no
/// output terminal to drive and cannot be a continuous driver at all.
const BIDIRECTIONAL_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a bidirectional pass switch (`tran` and friends)");

/// One elaborated primitive instance, with its terminals already split into
/// the ones it drives and the ones it reads.
///
/// Which terminals are which is a property of the [`GateKind`] and is settled
/// here, once, so nothing downstream counts terminals again.
pub struct Gate {
    pub kind: GateKind,
    pub strength: DriveStrength,
    /// The terminals this gate drives. Every kind but the pull sources has
    /// exactly one; `buf` and `not` may have several.
    pub outputs: Vec<Expression>,
    /// The terminals it reads, in the order the truth table wants them:
    /// the data first, then any control.
    pub inputs: Vec<Expression>,
}

impl Gate {
    /// Splits a terminal list into outputs and inputs, reporting a terminal
    /// count the kind cannot take.
    ///
    /// The output comes **first** for every kind but `buf` and `not`, whose
    /// single input comes last so that everything before it is an output.
    pub fn new(
        kind: GateKind,
        strength: DriveStrength,
        mut terminals: Vec<Expression>,
    ) -> Result<Gate, SimulationError> {
        let found = terminals.len();
        let too_few = || SimulationError::GateTerminals {
            gate: kind.keyword(),
            found,
        };
        let (outputs, inputs) = match kind {
            GateKind::And
            | GateKind::Nand
            | GateKind::Or
            | GateKind::Nor
            | GateKind::Xor
            | GateKind::Xnor => {
                if found < 2 {
                    return Err(too_few());
                }
                let inputs = terminals.split_off(1);
                (terminals, inputs)
            }
            GateKind::Buf | GateKind::Not => {
                if found < 2 {
                    return Err(too_few());
                }
                let inputs = vec![terminals.pop().expect("at least two terminals")];
                (terminals, inputs)
            }
            GateKind::Bufif0
            | GateKind::Bufif1
            | GateKind::Notif0
            | GateKind::Notif1
            | GateKind::Nmos
            | GateKind::Pmos
            | GateKind::Rnmos
            | GateKind::Rpmos => {
                if found != 3 {
                    return Err(too_few());
                }
                let inputs = terminals.split_off(1);
                (terminals, inputs)
            }
            GateKind::Cmos | GateKind::Rcmos => {
                if found != 4 {
                    return Err(too_few());
                }
                let inputs = terminals.split_off(1);
                (terminals, inputs)
            }
            GateKind::Pullup | GateKind::Pulldown => {
                if found < 1 {
                    return Err(too_few());
                }
                (terminals, Vec::new())
            }
            GateKind::Tran
            | GateKind::Rtran
            | GateKind::Tranif0
            | GateKind::Tranif1
            | GateKind::Rtranif0
            | GateKind::Rtranif1 => return Err(BIDIRECTIONAL_UNSUPPORTED),
        };
        Ok(Gate {
            kind,
            strength,
            outputs,
            inputs,
        })
    }

    /// The bit this gate is driving, given the design's present state.
    ///
    /// A gate terminal is one bit wide, so an input wider than that is read at
    /// its least significant bit — the same bit a scalar connection would name.
    pub fn evaluate(&self, state: &StateStore) -> Result<u8, SimulationError> {
        let mut levels = Vec::with_capacity(self.inputs.len());
        for input in &self.inputs {
            levels.push(least_significant_bit(&eval(input, state)?));
        }
        Ok(gate_output(self.kind, &levels))
    }
}

/// The bit a terminal connection carries: the least significant one, or `x`
/// for the empty register a zero-width expression produces.
fn least_significant_bit(value: &Register) -> u8 {
    value.bit_from_lsb(0).unwrap_or(X)
}

/// `z` read as a *level*. A gate's input is a voltage, and a floating one is
/// not something it can tell from an unknown — which is why `buf` turns a `z`
/// into an `x` where a switch passes it straight through.
fn as_level(code: u8) -> u8 {
    if code == Z {
        X
    } else {
        code
    }
}

fn and2(a: u8, b: u8) -> u8 {
    if a == ZERO || b == ZERO {
        ZERO
    } else if a == ONE && b == ONE {
        ONE
    } else {
        X
    }
}

fn or2(a: u8, b: u8) -> u8 {
    if a == ONE || b == ONE {
        ONE
    } else if a == ZERO && b == ZERO {
        ZERO
    } else {
        X
    }
}

fn xor2(a: u8, b: u8) -> u8 {
    if a > ONE || b > ONE {
        X
    } else {
        a ^ b
    }
}

fn invert(a: u8) -> u8 {
    match a {
        ZERO => ONE,
        ONE => ZERO,
        _ => X,
    }
}

/// A three-state buffer: the data is *buffered*, so a `z` on it comes out `x`.
fn enabled_buffer(data: u8, control: u8, active: u8) -> u8 {
    if control == active {
        as_level(data)
    } else if control == invert_level(active) {
        Z
    } else {
        X
    }
}

/// The other of the two known levels. Only ever called with `0` or `1`.
fn invert_level(code: u8) -> u8 {
    if code == ZERO {
        ONE
    } else {
        ZERO
    }
}

/// A MOS switch: the data is *passed*, so a `z` stays a `z` where a buffer
/// would turn it into an `x`.
fn switch(data: u8, control: u8, active: u8) -> u8 {
    if control == active {
        data
    } else if control == invert_level(active) {
        Z
    } else if data == Z {
        Z
    } else {
        X
    }
}

/// A complementary pair on one node: an n-switch and a p-switch carrying the
/// same data.
///
/// It is deliberately *not* the resolution of the two halves. A switch whose
/// control is unknown passes something between its data and `z`, which reads
/// as `x` on its own but is not one: `cmos(0, 1, x)` is `0` in iverilog, where
/// resolving a strong `0` against a strong `x` would give `x`. Asking whether
/// *either* half conducts sidesteps the ambiguous strength that would take to
/// model.
fn complementary_switch(data: u8, ncontrol: u8, pcontrol: u8) -> u8 {
    if ncontrol == ONE || pcontrol == ZERO {
        data
    } else if ncontrol == ZERO && pcontrol == ONE {
        Z
    } else if data == Z {
        Z
    } else {
        X
    }
}

/// What a primitive drives, given the levels on its input terminals.
///
/// The tables are iverilog's, checked against it rather than read off the LRM:
/// `and(0, x)` is `0` and not `x`, `or(1, z)` is `1`, and a three-state buffer
/// whose control is unknown drives `x` rather than the LRM's weaker `L`/`H`.
pub fn gate_output(kind: GateKind, inputs: &[u8]) -> u8 {
    match kind {
        GateKind::And => inputs.iter().copied().fold(ONE, and2),
        GateKind::Nand => invert(inputs.iter().copied().fold(ONE, and2)),
        GateKind::Or => inputs.iter().copied().fold(ZERO, or2),
        GateKind::Nor => invert(inputs.iter().copied().fold(ZERO, or2)),
        GateKind::Xor => inputs.iter().copied().fold(ZERO, xor2),
        GateKind::Xnor => invert(inputs.iter().copied().fold(ZERO, xor2)),
        GateKind::Buf => as_level(inputs[0]),
        GateKind::Not => invert(inputs[0]),
        GateKind::Bufif1 => enabled_buffer(inputs[0], inputs[1], ONE),
        GateKind::Bufif0 => enabled_buffer(inputs[0], inputs[1], ZERO),
        GateKind::Notif1 => invert_output(enabled_buffer(inputs[0], inputs[1], ONE)),
        GateKind::Notif0 => invert_output(enabled_buffer(inputs[0], inputs[1], ZERO)),
        GateKind::Nmos | GateKind::Rnmos => switch(inputs[0], inputs[1], ONE),
        GateKind::Pmos | GateKind::Rpmos => switch(inputs[0], inputs[1], ZERO),
        GateKind::Cmos | GateKind::Rcmos => complementary_switch(inputs[0], inputs[1], inputs[2]),
        GateKind::Pullup => ONE,
        GateKind::Pulldown => ZERO,
        // A bidirectional switch never reaches here: `Gate::new` refuses it.
        GateKind::Tran
        | GateKind::Rtran
        | GateKind::Tranif0
        | GateKind::Tranif1
        | GateKind::Rtranif0
        | GateKind::Rtranif1 => X,
    }
}

/// A three-state *inverting* buffer inverts what it drives but keeps the `z`
/// it is not driving at all.
fn invert_output(code: u8) -> u8 {
    if code == Z {
        Z
    } else {
        invert(code)
    }
}

/// What a net carries when several drivers reach it.
///
/// A driver contributes nothing when it is floating — either because it is
/// driving `z` or because that half of its strength is `highz`. Of the rest,
/// the strongest wins outright; drivers tied at the strongest level agree on a
/// value or the net is `x`. A net every driver has let go of is `z`.
pub fn resolve_bit(drivers: &[(u8, StrengthLevel)]) -> u8 {
    let mut winner = Z;
    let mut level = StrengthLevel::Highz;
    for &(code, strength) in drivers {
        let strength = if code == Z {
            StrengthLevel::Highz
        } else {
            strength
        };
        if strength == StrengthLevel::Highz {
            continue;
        }
        if winner == Z || strength > level {
            winner = code;
            level = strength;
        } else if strength == level && winner != code {
            winner = X;
        }
    }
    winner
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parsers::identifier::Identifier;

    /// The four-state codes, in the order the tables below vary them.
    const CODES: [u8; 4] = [ZERO, ONE, X, Z];

    fn code_of(character: char) -> u8 {
        match character {
            '0' => ZERO,
            '1' => ONE,
            'x' => X,
            'z' => Z,
            other => panic!("not a four-state bit: {}", other),
        }
    }

    fn shown(code: u8) -> char {
        match code {
            ZERO => '0',
            ONE => '1',
            X => 'x',
            _ => 'z',
        }
    }

    /// Checks a two-input primitive against a sixteen character table, written
    /// with the first input varying slowest: `00 01 0x 0z 10 …`.
    fn check_binary(kind: GateKind, expected: &str) {
        let expected: Vec<u8> = expected.chars().map(code_of).collect();
        assert_eq!(expected.len(), 16, "a table is four codes by four");
        let mut index = 0;
        for a in CODES {
            for b in CODES {
                assert_eq!(
                    shown(gate_output(kind, &[a, b])),
                    shown(expected[index]),
                    "{}({}, {})",
                    kind.keyword(),
                    shown(a),
                    shown(b)
                );
                index += 1;
            }
        }
    }

    /// The tables `iverilog` 12.0 produces, measured rather than derived.
    ///
    /// `and(0, x)` being `0` and not `x` is the whole reason a four-state gate
    /// needs a table: an unknown input that cannot change the answer does not
    /// make the answer unknown. A `z` reaches a logic gate as an `x`.
    #[test]
    fn test_logic_gate_truth_tables() {
        check_binary(GateKind::And, "000001xx0xxx0xxx");
        check_binary(GateKind::Nand, "111110xx1xxx1xxx");
        check_binary(GateKind::Or, "01xx1111x1xxx1xx");
        check_binary(GateKind::Nor, "10xx0000x0xxx0xx");
        check_binary(GateKind::Xor, "01xx10xxxxxxxxxx");
        check_binary(GateKind::Xnor, "10xx01xxxxxxxxxx");
    }

    /// A three-state buffer, with the data first and the control second. It
    /// drives `z` when it is disabled and `x` when the control is unknown —
    /// where the LRM allows the weaker `L`/`H`, iverilog reports `x`.
    #[test]
    fn test_three_state_buffer_truth_tables() {
        check_binary(GateKind::Bufif1, "z0xxz1xxzxxxzxxx");
        check_binary(GateKind::Bufif0, "0zxx1zxxxzxxxzxx");
        check_binary(GateKind::Notif1, "z1xxz0xxzxxxzxxx");
        check_binary(GateKind::Notif0, "1zxx0zxxxzxxxzxx");
    }

    /// A switch *passes* its data where a buffer *reads* it, which is the one
    /// place the two families differ: an `nmos` conducting a `z` gives a `z`,
    /// while a `bufif1` enabled on a `z` gives an `x`.
    #[test]
    fn test_switch_truth_tables() {
        check_binary(GateKind::Nmos, "z0xxz1xxzxxxzzzz");
        check_binary(GateKind::Pmos, "0zxx1zxxxzxxzzzz");
        // The resistive variants pass the same values, only weaker — a
        // strength reduction this simulator does not model.
        check_binary(GateKind::Rnmos, "z0xxz1xxzxxxzzzz");
        check_binary(GateKind::Rpmos, "0zxx1zxxxzxxzzzz");
    }

    /// `cmos` is the case that proves it is not two resolved switches:
    /// `cmos(0, 1, x)` is `0`, where resolving a strong `0` against the `x` a
    /// half-open `pmos` reports would give `x`.
    #[test]
    fn test_cmos_truth_table() {
        let expected = "0zxx00000xxx0xxx\
                        1zxx11111xxx1xxx\
                        xzxxxxxxxxxxxxxx\
                        zzzzzzzzzzzzzzzz";
        let expected: Vec<u8> = expected.chars().map(code_of).collect();
        let mut index = 0;
        for data in CODES {
            for ncontrol in CODES {
                for pcontrol in CODES {
                    assert_eq!(
                        shown(gate_output(GateKind::Cmos, &[data, ncontrol, pcontrol])),
                        shown(expected[index]),
                        "cmos({}, {}, {})",
                        shown(data),
                        shown(ncontrol),
                        shown(pcontrol)
                    );
                    index += 1;
                }
            }
        }
    }

    #[test]
    fn test_unary_gate_truth_tables() {
        for (input, buffered, inverted) in [
            (ZERO, ZERO, ONE),
            (ONE, ONE, ZERO),
            (X, X, X),
            // A buffered `z` is an `x`: the gate reads a level, and a floating
            // input is not one.
            (Z, X, X),
        ] {
            assert_eq!(gate_output(GateKind::Buf, &[input]), buffered);
            assert_eq!(gate_output(GateKind::Not, &[input]), inverted);
        }
    }

    /// An n-input gate takes any number of inputs, and one is legal.
    #[test]
    fn test_arbitrary_arity() {
        assert_eq!(gate_output(GateKind::And, &[ONE, ONE, ONE, ONE]), ONE);
        assert_eq!(gate_output(GateKind::And, &[ONE, ONE, ZERO, X]), ZERO);
        assert_eq!(gate_output(GateKind::Or, &[ZERO, ZERO, X]), X);
        assert_eq!(gate_output(GateKind::Xor, &[ONE, ONE, ONE]), ONE);
        assert_eq!(gate_output(GateKind::And, &[ZERO]), ZERO);
    }

    /// A pull source has no inputs at all — it drives its one value forever.
    #[test]
    fn test_pull_sources() {
        assert_eq!(gate_output(GateKind::Pullup, &[]), ONE);
        assert_eq!(gate_output(GateKind::Pulldown, &[]), ZERO);
    }

    #[test]
    fn test_resolution_of_several_drivers() {
        let strong = StrengthLevel::Strong;
        // A net nothing drives is floating.
        assert_eq!(resolve_bit(&[]), Z);
        assert_eq!(resolve_bit(&[(Z, strong), (Z, strong)]), Z);
        // One driver holding it, the rest let go.
        assert_eq!(
            resolve_bit(&[(Z, strong), (ZERO, strong), (Z, strong)]),
            ZERO
        );
        // Two drivers that agree, and two that do not.
        assert_eq!(resolve_bit(&[(ONE, strong), (ONE, strong)]), ONE);
        assert_eq!(resolve_bit(&[(ZERO, strong), (ONE, strong)]), X);
        assert_eq!(resolve_bit(&[(ZERO, strong), (X, strong)]), X);
    }

    /// The point of strength: a `pullup` loses to anything actually driving
    /// the net and wins when they have all let go.
    #[test]
    fn test_strength_decides() {
        let pull = StrengthLevel::Pull;
        let strong = StrengthLevel::Strong;
        assert_eq!(resolve_bit(&[(ONE, pull), (Z, strong)]), ONE);
        assert_eq!(resolve_bit(&[(ONE, pull), (ZERO, strong)]), ZERO);
        // Order does not matter: the strongest wins whichever way round.
        assert_eq!(resolve_bit(&[(ZERO, strong), (ONE, pull)]), ZERO);
        // Two pulls that disagree are as unresolved as two strong drivers.
        assert_eq!(resolve_bit(&[(ZERO, pull), (ONE, pull)]), X);
        // A `highz` half drives nothing, which is what an open drain is.
        assert_eq!(resolve_bit(&[(ZERO, StrengthLevel::Highz)]), Z);
        assert_eq!(
            resolve_bit(&[(ZERO, StrengthLevel::Highz), (ONE, pull)]),
            ONE
        );
        assert_eq!(
            resolve_bit(&[(ONE, StrengthLevel::Supply), (ZERO, strong)]),
            ONE
        );
    }

    fn terminal(name: &str) -> Expression {
        Expression::Identifier(Identifier::new(name.to_string()))
    }

    fn terminals(names: &[&str]) -> Vec<Expression> {
        names.iter().map(|name| terminal(name)).collect()
    }

    /// Which terminals are outputs is a property of the kind: an n-input gate
    /// writes its output first, and `buf`/`not` write their input last.
    #[test]
    fn test_terminals_split_by_kind() {
        let gate = Gate::new(
            GateKind::And,
            DriveStrength::STRONG,
            terminals(&["out", "a", "b"]),
        )
        .unwrap();
        assert_eq!(gate.outputs, terminals(&["out"]));
        assert_eq!(gate.inputs, terminals(&["a", "b"]));

        let gate = Gate::new(
            GateKind::Buf,
            DriveStrength::STRONG,
            terminals(&["o1", "o2", "in"]),
        )
        .unwrap();
        assert_eq!(gate.outputs, terminals(&["o1", "o2"]));
        assert_eq!(gate.inputs, terminals(&["in"]));

        let gate = Gate::new(
            GateKind::Pullup,
            DriveStrength::PULL,
            terminals(&["net_a", "net_b"]),
        )
        .unwrap();
        assert_eq!(gate.outputs, terminals(&["net_a", "net_b"]));
        assert!(gate.inputs.is_empty());
    }

    /// A terminal count the kind cannot take is a named error, never a gate
    /// that quietly drives nothing.
    #[test]
    fn test_terminal_count_is_checked() {
        assert_eq!(
            Gate::new(GateKind::And, DriveStrength::STRONG, terminals(&["out"])).err(),
            Some(SimulationError::GateTerminals {
                gate: "and",
                found: 1
            })
        );
        assert_eq!(
            Gate::new(
                GateKind::Bufif1,
                DriveStrength::STRONG,
                terminals(&["out", "in"])
            )
            .err(),
            Some(SimulationError::GateTerminals {
                gate: "bufif1",
                found: 2
            })
        );
        assert_eq!(
            Gate::new(
                GateKind::Cmos,
                DriveStrength::STRONG,
                terminals(&["out", "in", "n"])
            )
            .err(),
            Some(SimulationError::GateTerminals {
                gate: "cmos",
                found: 3
            })
        );
    }

    /// A bidirectional switch has no output terminal, so it cannot be a
    /// continuous driver. It stops here by name rather than quietly doing
    /// nothing.
    #[test]
    fn test_a_bidirectional_switch_is_a_named_error() {
        for kind in [
            GateKind::Tran,
            GateKind::Rtran,
            GateKind::Tranif0,
            GateKind::Tranif1,
            GateKind::Rtranif0,
            GateKind::Rtranif1,
        ] {
            assert_eq!(
                Gate::new(kind, DriveStrength::STRONG, terminals(&["a", "b", "c"])).err(),
                Some(BIDIRECTIONAL_UNSUPPORTED)
            );
        }
    }
}

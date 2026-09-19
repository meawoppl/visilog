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
//! The `r`-prefixed switches (`rnmos`, `rcmos`, `rtran`, …) reduce the strength
//! of what they pass, which is not modelled, so they behave as their
//! non-resistive counterparts. A switch's *delay* — `tranif0 #(100)`, which
//! delays the moment the switch opens or closes rather than a value — is parsed
//! and ignored. A `tranif` whose control is `x` or `z` is taken **not** to
//! conduct, where iverilog conducts at an ambiguous strength and gives the far
//! side an `x`; see [`PassSwitch::conducts`].

use crate::parsers::delay::GateDelay;
use crate::parsers::expr::Expression;
use crate::parsers::gates::{DriveStrength, GateKind, StrengthLevel};
use crate::register::{Register, ONE, X, Z, ZERO};
use crate::simulator::eval::eval;
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::StateStore;

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
    /// `#(rise, fall, turn_off)`, when the instantiation named one. A gate is
    /// a continuous driver like a delayed `assign`, so the delay changes
    /// *which* value it asserts rather than whether it asserts one.
    pub delay: Option<GateDelay>,
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
        delay: Option<GateDelay>,
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
            | GateKind::Rtranif1 => {
                unreachable!("a bidirectional switch elaborates to a PassSwitch")
            }
        };
        Ok(Gate {
            kind,
            strength,
            outputs,
            inputs,
            delay,
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

/// One elaborated **bidirectional** pass switch.
///
/// A `tran` has no output terminal, so it is deliberately not a [`Gate`]: it
/// does not *drive* anything, it makes its two terminals **one node**. A node's
/// value is what every driver of every net in it resolves to *together*, which
/// is the same [`resolve_bit`] every other contention already goes through — so
/// the switch changes which contributions are pooled and nothing about the
/// value rule.
///
/// Copying a value from one terminal to the other instead is the shape that
/// looks right and is wrong: once `a` has been copied to `b`, a driver on `a`
/// letting go leaves `b` holding the stale value, which copies straight back.
/// A three-state bus wired through a `tran` would never float again.
pub struct PassSwitch {
    pub kind: GateKind,
    /// The two nets it joins. Each is read as one bit, the way every other
    /// primitive terminal is.
    pub terminals: [Expression; 2],
    /// A `tranif`'s control terminal and the level that makes it conduct.
    /// `None` for `tran`/`rtran`, which always do.
    pub control: Option<(Expression, u8)>,
}

impl PassSwitch {
    /// Splits a bidirectional switch's terminal list, reporting a count the
    /// kind cannot take.
    pub fn new(kind: GateKind, terminals: Vec<Expression>) -> Result<PassSwitch, SimulationError> {
        let found = terminals.len();
        let wrong_count = || SimulationError::GateTerminals {
            gate: kind.keyword(),
            found,
        };
        let active = match kind {
            GateKind::Tran | GateKind::Rtran => None,
            GateKind::Tranif1 | GateKind::Rtranif1 => Some(ONE),
            GateKind::Tranif0 | GateKind::Rtranif0 => Some(ZERO),
            _ => return Err(wrong_count()),
        };
        let wanted = if active.is_some() { 3 } else { 2 };
        if found != wanted {
            return Err(wrong_count());
        }
        let mut terminals = terminals.into_iter();
        let first = terminals.next().expect("the count was just checked");
        let second = terminals.next().expect("the count was just checked");
        let control =
            active.map(|level| (terminals.next().expect("the count was just checked"), level));
        Ok(PassSwitch {
            kind,
            terminals: [first, second],
            control,
        })
    }

    /// Whether the switch joins its two terminals as things stand.
    ///
    /// This is asked **every propagation pass** rather than partitioned once at
    /// elaboration, because a `tranif`'s control moves during the run — corpus
    /// `tran-keeper` gates a switch on the very net the switch is holding up.
    /// A union-find built once would be right for `tran` and silently wrong for
    /// the four `if` forms.
    ///
    /// A control that is `x` or `z` is taken not to conduct. iverilog conducts
    /// at an *ambiguous* strength instead, which gives the far side an `x`
    /// while leaving the driven side alone: measured against iverilog 12.0,
    /// `assign p = 1; tranif1 (p, q, en);` with `en` unknown gives `p=1 q=x`,
    /// where this gives `p=1 q=z`. Saying the far side is unknown needs a
    /// strength that is a *range* rather than a level, which
    /// [`resolve_bit`] has no shape for.
    pub fn conducts(&self, state: &StateStore) -> Result<bool, SimulationError> {
        match &self.control {
            None => Ok(true),
            Some((control, active)) => Ok(least_significant_bit(&eval(control, state)?) == *active),
        }
    }
}

/// The bit a terminal connection carries: the least significant one, or `x`
/// for the empty register a zero-width expression produces.
///
/// A user-defined primitive reads its terminals the same way, which is why
/// this and [`as_level`] are shared rather than written twice.
pub(crate) fn least_significant_bit(value: &Register) -> u8 {
    value.bit_from_lsb(0).unwrap_or(X)
}

/// `z` read as a *level*. A gate's input is a voltage, and a floating one is
/// not something it can tell from an unknown — which is why `buf` turns a `z`
/// into an `x` where a switch passes it straight through.
pub(crate) fn as_level(code: u8) -> u8 {
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

/// The two *wired* net kinds, whose drivers combine by a logic function rather
/// than by strength.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WiredKind {
    /// `wand` / `triand` — a driver at `0` pulls the net down.
    And,
    /// `wor` / `trior` — a driver at `1` pulls the net up.
    Or,
}

/// What a `wand`/`wor` net carries when several drivers reach it.
///
/// A wired net is the one place the strongest driver does **not** simply win:
/// a `wand` is `0` when any driver is `0` however the others are driving, which
/// is the whole point of the net kind. A driver that is floating still
/// contributes nothing, so a net every driver has let go of is `z`, and the
/// dominant level is looked for before `x` so that `wand(0, x)` is `0` rather
/// than unknown — measured against iverilog 12.0 through corpus `triand` and
/// `trior`, whose tables are the ten combinations of two drivers.
///
/// Strength is deliberately not read here. Modelling it would mean the LRM's
/// wired *strength* table beside the value one, and every corpus design that
/// uses a wired net drives it at `strong`.
pub fn resolve_wired(kind: WiredKind, drivers: &[(u8, StrengthLevel)]) -> u8 {
    let dominant = match kind {
        WiredKind::And => ZERO,
        WiredKind::Or => ONE,
    };
    let mut seen = false;
    let mut unknown = false;
    for &(code, strength) in drivers {
        if code == Z || strength == StrengthLevel::Highz {
            continue;
        }
        if code == dominant {
            return dominant;
        }
        seen = true;
        unknown |= code == X;
    }
    match (seen, unknown) {
        (false, _) => Z,
        (true, true) => X,
        (true, false) => invert(dominant),
    }
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

    /// A wired net combines its drivers by a logic function, which is the one
    /// place the strongest driver does not simply win: `wand(0, 1)` is `0`
    /// where an ordinary net gives `x`.
    ///
    /// This is the whole of corpus `triand`'s and `trior`'s own tables — the
    /// ten combinations of two drivers, which those designs assert against
    /// iverilog's answers.
    #[test]
    fn test_wired_nets_resolve_by_a_logic_function() {
        let strong = StrengthLevel::Strong;
        let wired = |kind, codes: &[u8]| {
            let drivers: Vec<(u8, StrengthLevel)> =
                codes.iter().map(|code| (*code, strong)).collect();
            shown(resolve_wired(kind, &drivers))
        };

        let and = |codes: &[u8]| wired(WiredKind::And, codes);
        assert_eq!(and(&[ZERO, ZERO]), '0');
        assert_eq!(and(&[ZERO, ONE]), '0');
        assert_eq!(and(&[ZERO, X]), '0');
        assert_eq!(and(&[ZERO, Z]), '0');
        assert_eq!(and(&[ONE, ONE]), '1');
        assert_eq!(and(&[ONE, X]), 'x');
        assert_eq!(and(&[ONE, Z]), '1');
        assert_eq!(and(&[X, X]), 'x');
        assert_eq!(and(&[X, Z]), 'x');
        assert_eq!(and(&[Z, Z]), 'z');

        let or = |codes: &[u8]| wired(WiredKind::Or, codes);
        assert_eq!(or(&[ZERO, ZERO]), '0');
        assert_eq!(or(&[ZERO, ONE]), '1');
        assert_eq!(or(&[ZERO, X]), 'x');
        assert_eq!(or(&[ZERO, Z]), '0');
        assert_eq!(or(&[ONE, ONE]), '1');
        assert_eq!(or(&[ONE, X]), '1');
        assert_eq!(or(&[ONE, Z]), '1');
        assert_eq!(or(&[X, X]), 'x');
        assert_eq!(or(&[X, Z]), 'x');
        assert_eq!(or(&[Z, Z]), 'z');

        // A `highz` half drives nothing, exactly as it does for a plain net.
        assert_eq!(
            resolve_wired(
                WiredKind::And,
                &[(ZERO, StrengthLevel::Highz), (ONE, strong)]
            ),
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
            None,
        )
        .unwrap();
        assert_eq!(gate.outputs, terminals(&["out"]));
        assert_eq!(gate.inputs, terminals(&["a", "b"]));

        let gate = Gate::new(
            GateKind::Buf,
            DriveStrength::STRONG,
            terminals(&["o1", "o2", "in"]),
            None,
        )
        .unwrap();
        assert_eq!(gate.outputs, terminals(&["o1", "o2"]));
        assert_eq!(gate.inputs, terminals(&["in"]));

        let gate = Gate::new(
            GateKind::Pullup,
            DriveStrength::PULL,
            terminals(&["net_a", "net_b"]),
            None,
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
            Gate::new(
                GateKind::And,
                DriveStrength::STRONG,
                terminals(&["out"]),
                None
            )
            .err(),
            Some(SimulationError::GateTerminals {
                gate: "and",
                found: 1
            })
        );
        assert_eq!(
            Gate::new(
                GateKind::Bufif1,
                DriveStrength::STRONG,
                terminals(&["out", "in"]),
                None
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
                terminals(&["out", "in", "n"]),
                None
            )
            .err(),
            Some(SimulationError::GateTerminals {
                gate: "cmos",
                found: 3
            })
        );
    }

    /// A bidirectional switch has no output terminal, so it is split by
    /// [`PassSwitch::new`] rather than by [`Gate::new`]: two terminals for
    /// `tran`/`rtran`, and two plus a control for the four `if` forms.
    #[test]
    fn test_a_bidirectional_switch_splits_into_two_terminals_and_a_control() {
        for kind in [GateKind::Tran, GateKind::Rtran] {
            let switch = PassSwitch::new(kind, terminals(&["a", "b"])).unwrap();
            assert!(
                switch.control.is_none(),
                "{} has no control",
                kind.keyword()
            );
            assert_eq!(
                PassSwitch::new(kind, terminals(&["a", "b", "c"])).err(),
                Some(SimulationError::GateTerminals {
                    gate: kind.keyword(),
                    found: 3
                })
            );
        }
        for (kind, active) in [
            (GateKind::Tranif1, ONE),
            (GateKind::Rtranif1, ONE),
            (GateKind::Tranif0, ZERO),
            (GateKind::Rtranif0, ZERO),
        ] {
            let switch = PassSwitch::new(kind, terminals(&["a", "b", "c"])).unwrap();
            assert_eq!(
                switch.control.map(|(_, level)| level),
                Some(active),
                "{} conducts on {}",
                kind.keyword(),
                active
            );
            assert_eq!(
                PassSwitch::new(kind, terminals(&["a", "b"])).err(),
                Some(SimulationError::GateTerminals {
                    gate: kind.keyword(),
                    found: 2
                })
            );
        }
    }
}

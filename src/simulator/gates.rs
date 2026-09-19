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
//! A [`Strength`] is a *signed interval* rather than a level, which is what
//! lets a driver say it is **not sure**: a primitive whose control is unknown
//! is either driving its data or turned off, which is the whole span between
//! the two. That is one of the two things [`Gate::driving`] answers; the other
//! is that a MOS switch passes the strength on its *input* rather than
//! declaring one, reduced if it is a resistive form.
//!
//! # What is not modelled
//!
//! A switch's *delay* — `tranif0 #(100)`, which delays the moment the switch
//! opens or closes rather than a value — is parsed and ignored, and a
//! **delayed** gate drives at its declared strength rather than at the one its
//! inputs say this instant: the value in hand is the one that landed `#n` ago
//! and the two would otherwise be out of step. A `tranif` whose control is `x`
//! or `z` is taken **not** to conduct, where iverilog conducts at an ambiguous
//! strength and gives the far side an `x`; see [`PassSwitch::conducts`]. And
//! nothing reduces a strength across a *bidirectional* switch, so a `tran`
//! carries a `supply` through where iverilog drops it to `strong`.

use crate::parsers::delay::GateDelay;
use crate::parsers::expr::Expression;
use crate::parsers::gates::{DriveStrength, GateKind, StrengthLevel};
use crate::register::{Register, ONE, X, Z, ZERO};
use crate::simulator::eval::eval;
use crate::simulator::exec::{resolve_target, ResolvedTarget};
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

    /// The bit this gate is driving and the strength it drives it at, given
    /// the design's present state.
    ///
    /// A gate terminal is one bit wide, so an input wider than that is read at
    /// its least significant bit — the same bit a scalar connection would name.
    pub fn evaluate(&self, state: &StateStore) -> Result<(u8, Driven), SimulationError> {
        let mut levels = Vec::with_capacity(self.inputs.len());
        for input in &self.inputs {
            levels.push(least_significant_bit(&eval(input, state)?));
        }
        let code = gate_output(self.kind, &levels);
        Ok((code, self.driving(&levels, state)?))
    }

    /// The strength this gate is driving at.
    ///
    /// Most gates simply drive at the `(strength0, strength1)` they were
    /// declared with, and the value decides which half applies. The two that
    /// do not are the whole of this function:
    ///
    /// - **A control that is unknown makes the strength ambiguous, not the
    ///   value.** A `bufif1` whose enable is `x` is either driving its data or
    ///   turned off, which is the interval between the two — iverilog 12.0
    ///   prints `StL` for a `0` on the data and `PuL` at `(pull0, pull1)`,
    ///   where a level alone could only say `StX` (corpus `pr544`,
    ///   `pr1787394a`/`b`).
    /// - **A MOS switch passes the strength on its data terminal** rather than
    ///   declaring one, reduced if it is a resistive form. That is what makes
    ///   `pmos (q, w, 1'b0);` carry a `pullup`'s `pull` through to `q`, where
    ///   driving at `strong` would tie with a real `strong` driver and give
    ///   `x` (corpus `resolv1`, `br_gh99t`/`u`).
    fn driving(&self, levels: &[u8], state: &StateStore) -> Result<Driven, SimulationError> {
        let Some(conducting) = conducting(self.kind, levels) else {
            return Ok(Driven::Declared(self.strength));
        };
        if conducting == Conducting::No {
            return Ok(Driven::Bit(Strength::HIGHZ));
        }
        // What the gate drives while it *is* conducting, asked of the same
        // truth table the value comes from so the two cannot disagree.
        let passing = gate_output(self.kind, &conducting_levels(self.kind, levels));
        let open = if is_mos_switch(self.kind) {
            reduced(
                self.kind,
                terminal_strength(&self.inputs[0], passing, state),
            )
        } else {
            Strength::driven(passing, self.strength)
        };
        Ok(Driven::Bit(match conducting {
            Conducting::Yes => open,
            _ => open.or_floating(),
        }))
    }
}

/// The strength one driver of a bit contributes at.
///
/// Most drivers declare a `(strength0, strength1)` and let each bit's own
/// value pick a half; a MOS switch and a primitive with an unknown control
/// both work their strength out for themselves, and a primitive terminal is
/// one bit, so there is nothing per-bit to keep.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Driven {
    Declared(DriveStrength),
    Bit(Strength),
}

impl Driven {
    /// The strength this driver pushes a given four-state bit with.
    pub fn of(&self, code: u8) -> Strength {
        match self {
            Driven::Declared(strength) => Strength::driven(code, *strength),
            Driven::Bit(strength) => *strength,
        }
    }
}

/// Whether a control-led primitive is passing its data, blocking it, or
/// cannot say.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Conducting {
    Yes,
    No,
    Maybe,
}

/// Whether the primitive conducts, or `None` for one with no control at all —
/// a logic gate or a pull source, which always drives at what it declared.
fn conducting(kind: GateKind, levels: &[u8]) -> Option<Conducting> {
    let active = match kind {
        GateKind::Bufif1 | GateKind::Notif1 | GateKind::Nmos | GateKind::Rnmos => ONE,
        GateKind::Bufif0 | GateKind::Notif0 | GateKind::Pmos | GateKind::Rpmos => ZERO,
        // A complementary pair conducts when *either* half does, which is the
        // one place a `cmos` is not the resolution of two switches: a half
        // that is definitely open settles the question whatever the other
        // half's control is doing.
        GateKind::Cmos | GateKind::Rcmos => {
            let n = level_conducts(levels[1], ONE);
            let p = level_conducts(levels[2], ZERO);
            return Some(match (n, p) {
                (Conducting::Yes, _) | (_, Conducting::Yes) => Conducting::Yes,
                (Conducting::No, Conducting::No) => Conducting::No,
                _ => Conducting::Maybe,
            });
        }
        _ => return None,
    };
    Some(level_conducts(levels[1], active))
}

/// Whether a primitive *passes* its data rather than buffering it, which is
/// what makes it carry its source's strength. The `tran` family is not here:
/// a bidirectional switch drives nothing at all and joins two nets instead.
fn is_mos_switch(kind: GateKind) -> bool {
    matches!(
        kind,
        GateKind::Nmos
            | GateKind::Pmos
            | GateKind::Rnmos
            | GateKind::Rpmos
            | GateKind::Cmos
            | GateKind::Rcmos
    )
}

/// Whether the primitive is one of the `r`-prefixed forms, which weaken what
/// they pass.
fn is_resistive(kind: GateKind) -> bool {
    matches!(
        kind,
        GateKind::Rnmos
            | GateKind::Rpmos
            | GateKind::Rcmos
            | GateKind::Rtran
            | GateKind::Rtranif0
            | GateKind::Rtranif1
    )
}

fn level_conducts(control: u8, active: u8) -> Conducting {
    if control == active {
        Conducting::Yes
    } else if control == invert_level(active) {
        Conducting::No
    } else {
        Conducting::Maybe
    }
}

/// The same input levels with every control forced to the value that opens the
/// primitive, so [`gate_output`] answers what it drives *while* it is open.
fn conducting_levels(kind: GateKind, levels: &[u8]) -> Vec<u8> {
    match kind {
        GateKind::Cmos | GateKind::Rcmos => vec![levels[0], ONE, ZERO],
        GateKind::Bufif1 | GateKind::Notif1 | GateKind::Nmos | GateKind::Rnmos => {
            vec![levels[0], ONE]
        }
        _ => vec![levels[0], ZERO],
    }
}

/// The strength on a switch's data terminal.
///
/// A resolved net carries the level its own drivers settled at, which is the
/// whole point — that is what a switch passes on. Anything else answers as an
/// ordinary driver would, at `strong`, and so does a recorded level that no
/// longer describes the value: a write that landed after the resolution which
/// recorded it leaves a level behind that names a strength for a bit the net
/// no longer holds.
fn terminal_strength(terminal: &Expression, code: u8, state: &StateStore) -> Strength {
    let fallback = Strength::driven(code, DriveStrength::STRONG);
    // A terminal that names no storage — `{16'b0, data}`, which an array of
    // instances slices with a shift (corpus `npmos2`, `rnpmos2`) — has no net
    // behind it to read a level off, so it answers as an ordinary driver
    // would. Asking is not an error here the way it is for an *output*
    // terminal, which really does have to be written.
    let Ok(target) = resolve_target(state, terminal) else {
        return fallback;
    };
    let (name, index) = match &target {
        ResolvedTarget::Whole(name) => (name, None),
        ResolvedTarget::Bits { name, indices } => (name, indices.last().copied()),
        _ => return fallback,
    };
    let Some(signal) = state.get_signal(name) else {
        return fallback;
    };
    let Some(levels) = signal.strengths() else {
        return fallback;
    };
    // A whole net names its least significant bit, which is the last of the
    // most-significant-first list; a select names the bit its index maps to.
    let position = match index {
        Some(index) => signal.bit_position(index),
        None => levels.len().checked_sub(1),
    };
    position
        .and_then(|position| levels.get(position).copied())
        .filter(|strength| strength.value() == code)
        .unwrap_or(fallback)
}

/// A strength after a switch has passed it, IEEE 1364-2005 Table 7-8.
///
/// A non-resistive switch drops `supply` to `strong` and leaves everything
/// else alone; an `r`-prefixed one weakens every level. Both bounds of the
/// interval move, since a reduction is about the level rather than the value.
fn reduced(kind: GateKind, strength: Strength) -> Strength {
    let (low, high) = strength.bounds();
    let reduce = |level: i8| {
        let magnitude = match (is_resistive(kind), level.abs()) {
            (false, 7) => 6,
            (false, other) => other,
            (true, 7) | (true, 6) => 5,
            (true, 5) => 3,
            (true, 4) | (true, 3) => 2,
            (true, 2) | (true, 1) => 1,
            (true, _) => 0,
        };
        level.signum() * magnitude
    };
    Strength::span(reduce(low), reduce(high))
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
/// It asks whether *either* half conducts rather than resolving the two, which
/// is the same question [`conducting`] asks about its strength: a half that is
/// definitely open settles the matter whatever the other half's control is
/// doing, so `cmos(0, 1, x)` is a definite `0` where a lone `pmos` with an
/// unknown gate would be the ambiguous `StL`.
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

/// The IEEE 1364-2005 strength level a [`StrengthLevel`] names, on the
/// standard's own `0..=7` scale.
///
/// The grammar only spells the five *drive* strengths, so `Small`, `Medium`
/// and `Large` — the charge strengths a `trireg` declares — have no keyword
/// reaching here. The numbering still has to be the standard's, because `%v`
/// prints the **digits** when a bit's two halves disagree: `65X` is a `strong`
/// `0` against a `pull` `1`, and nothing but 6 and 5 spells that.
pub fn strength_level(level: StrengthLevel) -> i8 {
    match level {
        StrengthLevel::Highz => 0,
        StrengthLevel::Weak => 3,
        StrengthLevel::Pull => 5,
        StrengthLevel::Strong => 6,
        StrengthLevel::Supply => 7,
    }
}

/// The strength of one bit of a net: a **signed interval** over the eight
/// strength levels.
///
/// A negative number is a level driving toward `0`, a positive one toward `1`,
/// and `0` is high impedance — so `St0` is `[-6, -6]`, `Pu1` is `[5, 5]` and a
/// floating bit is `[0, 0]`. A driver that is certain what it drives is a
/// *point*; one that is not is a *range*, and that range is the whole reason a
/// strength cannot be a level. A `bufif1` whose control is unknown drives its
/// data or nothing at all, which is `[-6, 0]` — the `StL` iverilog prints where
/// a level alone could only say `StX`.
///
/// The value follows from the interval rather than riding beside it: a bit is
/// `0` when every level in the range drives toward `0`, `1` when every one
/// drives toward `1`, `z` when the range is only high impedance, and `x` for
/// anything else. `StL` is therefore an `x` as a *value* and a `0`-or-`z` as a
/// strength, which is exactly what `$display("%b,%v", y, y)` prints in corpus
/// `pr544`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Strength {
    lo: i8,
    hi: i8,
}

impl Strength {
    /// A bit nothing is driving.
    pub const HIGHZ: Strength = Strength { lo: 0, hi: 0 };

    /// Both halves of an ordinary gate or `assign`, which drives `strong`.
    pub const STRONG_ZERO: Strength = Strength { lo: -6, hi: -6 };
    pub const STRONG_ONE: Strength = Strength { lo: 6, hi: 6 };
    /// A bit driven `strong` toward both levels at once — a plain `x`.
    pub const STRONG_UNKNOWN: Strength = Strength { lo: -6, hi: 6 };

    /// An interval over the eight levels, in either order.
    pub fn span(first: i8, second: i8) -> Strength {
        let lo = first.min(second).clamp(-7, 7);
        let hi = first.max(second).clamp(-7, 7);
        Strength { lo, hi }
    }

    /// What one driver of a bit contributes, given the code it is driving and
    /// the `(strength0, strength1)` it was declared with.
    ///
    /// An `x` is driven toward **both** levels at once, which is what makes it
    /// the one code whose strength is a range rather than a point; a `z` drives
    /// nothing whatever the declaration says, and a `0` driven by a `highz0`
    /// half is the open drain that floats instead.
    pub fn driven(code: u8, strength: DriveStrength) -> Strength {
        match code {
            ZERO => Strength::span(
                -strength_level(strength.zero),
                -strength_level(strength.zero),
            ),
            ONE => Strength::span(strength_level(strength.one), strength_level(strength.one)),
            X => Strength::span(-strength_level(strength.zero), strength_level(strength.one)),
            _ => Strength::HIGHZ,
        }
    }

    /// The four-state code this strength stands for.
    pub fn value(&self) -> u8 {
        if self.lo == 0 && self.hi == 0 {
            Z
        } else if self.hi < 0 {
            ZERO
        } else if self.lo > 0 {
            ONE
        } else {
            X
        }
    }

    /// The lowest and highest signed levels in the range, weakest-toward-`0`
    /// first.
    pub fn bounds(&self) -> (i8, i8) {
        (self.lo, self.hi)
    }

    /// The same drive, *or nothing at all* — what a primitive whose control is
    /// unknown is doing. It stretches the interval to high impedance, which is
    /// the difference between `StX` and `StL`.
    pub fn or_floating(&self) -> Strength {
        Strength {
            lo: self.lo.min(0),
            hi: self.hi.max(0),
        }
    }
}

/// What one bit of a node carries when two drivers reach it.
///
/// Each driver is a *set* of possible signed levels, so the answer is the set
/// of what every pair of them resolves to — kept as the interval that spans it,
/// which is all `%v` can print and all the next fold needs. Two unambiguous
/// drivers are the overwhelmingly common case and take the first branch: the
/// stronger wins outright, and two that are tied and disagree give an `x` *at
/// that level*, which is the interval `[-s, s]`.
fn resolve_pair(a: Strength, b: Strength) -> Strength {
    if a.lo == a.hi && b.lo == b.hi {
        return resolve_levels(a.lo, b.lo);
    }
    let mut lo = i8::MAX;
    let mut hi = i8::MIN;
    for x in a.lo..=a.hi {
        for y in b.lo..=b.hi {
            let combined = resolve_levels(x, y);
            lo = lo.min(combined.lo);
            hi = hi.max(combined.hi);
        }
    }
    Strength { lo, hi }
}

/// Two *unambiguous* drivers, resolved. The larger magnitude wins; equal
/// magnitudes agree, or the bit is unknown at that level.
fn resolve_levels(x: i8, y: i8) -> Strength {
    let (magnitude_x, magnitude_y) = (x.abs(), y.abs());
    if magnitude_x > magnitude_y {
        Strength { lo: x, hi: x }
    } else if magnitude_y > magnitude_x {
        Strength { lo: y, hi: y }
    } else if x == y {
        Strength { lo: x, hi: x }
    } else {
        Strength {
            lo: -magnitude_x,
            hi: magnitude_x,
        }
    }
}

/// What a net carries when several drivers reach it, as a strength.
///
/// A driver contributes nothing when it is floating — either because it is
/// driving `z` or because that half of its strength is `highz`. Of the rest,
/// the strongest wins outright; drivers tied at the strongest level agree on a
/// value or the net is `x`. A net every driver has let go of is `z`.
pub fn resolve_strength(drivers: impl IntoIterator<Item = Strength>) -> Strength {
    let mut resolved = Strength::HIGHZ;
    for driver in drivers {
        resolved = resolve_pair(resolved, driver);
    }
    resolved
}

/// The four-state bit several drivers settle on — [`resolve_strength`] read as
/// a value.
pub fn resolve_bit(drivers: &[Strength]) -> u8 {
    resolve_strength(drivers.iter().copied()).value()
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
    /// drives `z` when it is disabled and `x` when the control is unknown.
    /// The `L`/`H` the LRM gives an unknown control is a *strength* rather
    /// than a value and lives on [`Gate::driving`]; the value here is the `x`
    /// iverilog reports.
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
        // The resistive variants pass the same *values*, only weaker. The
        // reduction is a strength and is `reduced`'s.
        check_binary(GateKind::Rnmos, "z0xxz1xxzxxxzzzz");
        check_binary(GateKind::Rpmos, "0zxx1zxxxzxxzzzz");
    }

    /// `cmos` asks whether *either* half conducts: `cmos(0, 1, x)` is `0`,
    /// where resolving a strong `0` against the `x` a half-open `pmos` reports
    /// would give `x`.
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

    /// One driver at an ordinary `(strong0, strong1)`.
    fn at(code: u8, level: StrengthLevel) -> Strength {
        Strength::driven(
            code,
            DriveStrength {
                zero: level,
                one: level,
            },
        )
    }

    #[test]
    fn test_resolution_of_several_drivers() {
        let strong = StrengthLevel::Strong;
        // A net nothing drives is floating.
        assert_eq!(resolve_bit(&[]), Z);
        assert_eq!(resolve_bit(&[at(Z, strong), at(Z, strong)]), Z);
        // One driver holding it, the rest let go.
        assert_eq!(
            resolve_bit(&[at(Z, strong), at(ZERO, strong), at(Z, strong)]),
            ZERO
        );
        // Two drivers that agree, and two that do not.
        assert_eq!(resolve_bit(&[at(ONE, strong), at(ONE, strong)]), ONE);
        assert_eq!(resolve_bit(&[at(ZERO, strong), at(ONE, strong)]), X);
        assert_eq!(resolve_bit(&[at(ZERO, strong), at(X, strong)]), X);
    }

    /// The point of strength: a `pullup` loses to anything actually driving
    /// the net and wins when they have all let go.
    #[test]
    fn test_strength_decides() {
        let pull = StrengthLevel::Pull;
        let strong = StrengthLevel::Strong;
        assert_eq!(resolve_bit(&[at(ONE, pull), at(Z, strong)]), ONE);
        assert_eq!(resolve_bit(&[at(ONE, pull), at(ZERO, strong)]), ZERO);
        // Order does not matter: the strongest wins whichever way round.
        assert_eq!(resolve_bit(&[at(ZERO, strong), at(ONE, pull)]), ZERO);
        // Two pulls that disagree are as unresolved as two strong drivers.
        assert_eq!(resolve_bit(&[at(ZERO, pull), at(ONE, pull)]), X);
        // A `highz` half drives nothing, which is what an open drain is.
        assert_eq!(resolve_bit(&[at(ZERO, StrengthLevel::Highz)]), Z);
        assert_eq!(
            resolve_bit(&[at(ZERO, StrengthLevel::Highz), at(ONE, pull)]),
            ONE
        );
        assert_eq!(
            resolve_bit(&[at(ONE, StrengthLevel::Supply), at(ZERO, strong)]),
            ONE
        );
    }

    /// A resolved bit keeps the **level** it was driven at, which is what `%v`
    /// prints and what a level-only rule could not say.
    ///
    /// Measured against iverilog 12.0: `assign (pull1, strong0) net = 4'b0110;`
    /// with `$display("%v", net)` prints `St0_Pu1_Pu1_St0` (corpus
    /// `multi_bit_strength`).
    #[test]
    fn test_a_resolved_bit_keeps_its_level() {
        let open_drain = DriveStrength {
            zero: StrengthLevel::Strong,
            one: StrengthLevel::Pull,
        };
        assert_eq!(
            resolve_strength([Strength::driven(ONE, open_drain)]).bounds(),
            (5, 5)
        );
        assert_eq!(
            resolve_strength([Strength::driven(ZERO, open_drain)]).bounds(),
            (-6, -6)
        );
        // A `pullup` under a driven `strong` keeps the strong level, and holds
        // the net at `pull` once that driver lets go.
        let pull_up = Strength::driven(ONE, DriveStrength::PULL);
        assert_eq!(
            resolve_strength([pull_up, Strength::STRONG_ZERO]).bounds(),
            (-6, -6)
        );
        assert_eq!(
            resolve_strength([pull_up, Strength::HIGHZ]).bounds(),
            (5, 5)
        );
    }

    /// An **ambiguous** driver is a range, and resolving it against another
    /// driver spans what the two could settle on.
    ///
    /// Every case here is a line of corpus `pr544`'s gold file, which is two
    /// `bufif1`s on one net at `(pull0, pull1)` and `(strong0, strong1)`:
    /// a `pull` `0`-or-`z` beside a disabled strong driver is `PuL`, and a
    /// `pull` `x` beside a strong `0`-or-`z` is `65X`.
    #[test]
    fn test_an_ambiguous_driver_spans_what_it_could_be() {
        // A buffer at `pull` whose control is unknown: it drives 0, or floats.
        let maybe_pull_zero = Strength::span(-5, 0);
        assert_eq!(
            resolve_strength([maybe_pull_zero, Strength::HIGHZ]).bounds(),
            (-5, 0)
        );
        assert_eq!(resolve_strength([maybe_pull_zero]).value(), X);
        // `65X`: a strong 0-or-z beside a pull x.
        let maybe_strong_zero = Strength::span(-6, 0);
        let pull_unknown = Strength::span(-5, 5);
        assert_eq!(
            resolve_strength([pull_unknown, maybe_strong_zero]).bounds(),
            (-6, 5)
        );
        // `650`: a strong 0-or-z beside a definite pull 0 is a definite 0,
        // somewhere between the two levels.
        let pull_zero = Strength::span(-5, -5);
        let resolved = resolve_strength([pull_zero, maybe_strong_zero]);
        assert_eq!(resolved.bounds(), (-6, -5));
        assert_eq!(resolved.value(), ZERO);
    }

    fn terminal(name: &str) -> Expression {
        Expression::Identifier(Identifier::new(name.to_string()))
    }

    /// A gate reading its terminals out of a store, so `Gate::evaluate` can be
    /// asked what it drives *and* at what strength.
    fn drives(kind: GateKind, strength: DriveStrength, levels: &[(&str, u8)]) -> (u8, Strength) {
        let mut store = StateStore::new();
        for (name, code) in levels {
            store.declare(*name, (0, 0));
            store.set(*name, Register::from_bits(vec![*code]));
        }
        let mut names: Vec<Expression> = vec![terminal("out")];
        names.extend(levels.iter().map(|(name, _)| terminal(name)));
        store.declare_net("out", (0, 0), false);
        let gate = Gate::new(kind, strength, names, None).expect("a legal terminal count");
        let (code, driven) = gate.evaluate(&store).expect("terminals resolve");
        (code, driven.of(code))
    }

    /// **A control that is unknown makes the *strength* ambiguous, not the
    /// value.** A three-state buffer whose enable is `x` is either driving its
    /// data or turned off, which is the interval between the two.
    ///
    /// Measured against iverilog 12.0 (corpus `pr544`, whose gold file is two
    /// `bufif1`s on one net): `bufif1` at `(pull0, pull1)` with a `0` on its
    /// data and an `x` on its enable prints `x,PuL`, and with a `1` on its
    /// data prints `x,PuH`. The *value* is the `x` it always was.
    #[test]
    fn test_an_unknown_control_drives_an_ambiguous_strength() {
        let pull = DriveStrength::PULL;
        assert_eq!(
            drives(GateKind::Bufif1, pull, &[("d", ZERO), ("e", X)]),
            (X, Strength::span(-5, 0)),
            "PuL"
        );
        assert_eq!(
            drives(GateKind::Bufif1, pull, &[("d", ONE), ("e", X)]),
            (X, Strength::span(0, 5)),
            "PuH"
        );
        // A `notif` inverts what it would drive, so the ambiguity is the other
        // way round.
        assert_eq!(
            drives(
                GateKind::Notif1,
                DriveStrength::STRONG,
                &[("d", ZERO), ("e", X)]
            ),
            (X, Strength::span(0, 6)),
            "StH"
        );
        // An enable that is *known* is not ambiguous at all.
        assert_eq!(
            drives(GateKind::Bufif1, pull, &[("d", ZERO), ("e", ONE)]),
            (ZERO, Strength::span(-5, -5)),
            "Pu0"
        );
        assert_eq!(
            drives(GateKind::Bufif1, pull, &[("d", ZERO), ("e", ZERO)]),
            (Z, Strength::HIGHZ)
        );
    }

    /// A `cmos` conducts when **either** half does, so a half that is
    /// definitely open settles the question whatever the other is doing —
    /// which is why `cmos(0, 1, x)` is a definite `St0` rather than the `StL`
    /// a lone `pmos` with an unknown gate would give.
    ///
    /// Measured against iverilog 12.0, and it is corpus `pr1787394a`: `nmos
    /// n1 (c, b, nctl); pmos p1 (c, b, pctl);` with `b` at `0`, `nctl` at `0`
    /// and `pctl` at `x` prints `c=x(StL)`.
    #[test]
    fn test_a_complementary_pair_conducts_when_either_half_does() {
        let strong = DriveStrength::STRONG;
        assert_eq!(
            drives(GateKind::Cmos, strong, &[("d", ZERO), ("n", ONE), ("p", X)]),
            (ZERO, Strength::span(-6, -6)),
            "St0"
        );
        assert_eq!(
            drives(GateKind::Cmos, strong, &[("d", ZERO), ("n", X), ("p", X)]),
            (X, Strength::span(-6, 0)),
            "StL"
        );
        assert_eq!(
            drives(
                GateKind::Cmos,
                strong,
                &[("d", ZERO), ("n", ZERO), ("p", ONE)]
            ),
            (Z, Strength::HIGHZ)
        );
        // A lone `pmos` with an unknown gate is the ambiguous one.
        assert_eq!(
            drives(GateKind::Pmos, strong, &[("d", ZERO), ("p", X)]),
            (X, Strength::span(-6, 0)),
            "StL"
        );
    }

    /// **A MOS switch passes the strength on its data terminal**, rather than
    /// declaring one of its own — which is what makes a `pullup` reach the far
    /// side of one still recognisably a `pull`.
    ///
    /// Corpus `resolv1` is the case: `pullup (w); pmos (q, w, 1'b0);` beside
    /// `bufif0 (q, 1'b0, g);` answers `q = 0`, because the switch carries
    /// `Pu1` and loses to the buffer's `St0`. Driving at `strong` instead ties
    /// and gives `x`.
    #[test]
    fn test_a_switch_passes_the_strength_on_its_input() {
        let mut store = StateStore::new();
        store.declare_net("w", (0, 0), false);
        store.set("w", Register::from_bits(vec![ONE]));
        store.declare("g", (0, 0));
        store.set("g", Register::from_bits(vec![ZERO]));
        store.declare_net("q", (0, 0), false);
        store.set_strengths("w", vec![Strength::span(5, 5)]);
        let gate = Gate::new(
            GateKind::Pmos,
            DriveStrength::STRONG,
            terminals(&["q", "w", "g"]),
            None,
        )
        .unwrap();
        let (code, driven) = gate.evaluate(&store).unwrap();
        assert_eq!((code, driven.of(code)), (ONE, Strength::span(5, 5)), "Pu1");

        // A resistive one weakens it on the way through: `pull` becomes
        // `weak` (IEEE 1364-2005 Table 7-8).
        let gate = Gate::new(
            GateKind::Rpmos,
            DriveStrength::STRONG,
            terminals(&["q", "w", "g"]),
            None,
        )
        .unwrap();
        let (code, driven) = gate.evaluate(&store).unwrap();
        assert_eq!((code, driven.of(code)), (ONE, Strength::span(3, 3)), "We1");
    }

    /// The reduction tables, IEEE 1364-2005 Table 7-8. A non-resistive switch
    /// drops `supply` to `strong` and leaves everything else; an `r`-prefixed
    /// one weakens every level.
    ///
    /// The resistive column is corpus `rtran`'s gold file read off a chain of
    /// six switches: a `supply` driver reaches them as `Su`, `Pu`, `We`, `Me`,
    /// `Sm`, `Sm`, `Sm`.
    #[test]
    fn test_strength_reduction_tables() {
        for (level, plain, resistive) in [
            (7, 6, 5),
            (6, 6, 5),
            (5, 5, 3),
            (4, 4, 2),
            (3, 3, 2),
            (2, 2, 1),
            (1, 1, 1),
        ] {
            assert_eq!(
                reduced(GateKind::Nmos, Strength::span(level, level)).bounds(),
                (plain, plain),
                "nmos passing level {}",
                level
            );
            assert_eq!(
                reduced(GateKind::Rnmos, Strength::span(-level, -level)).bounds(),
                (-resistive, -resistive),
                "rnmos passing level -{}",
                level
            );
        }
        assert_eq!(
            reduced(GateKind::Rnmos, Strength::HIGHZ),
            Strength::HIGHZ,
            "nothing to weaken"
        );
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

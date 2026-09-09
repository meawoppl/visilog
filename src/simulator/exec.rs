//! Execution of procedural statement bodies.
//!
//! [`execute_statements`] runs a `Vec<ProceduralStatements>` — the body of an
//! `always` or `initial` block — against a [`StateStore`] by compiling it with
//! [`Program::compile`] and running it to completion. It is the piece that
//! knows Verilog's two assignment flavours apart:
//!
//! * **Blocking** (`=`) evaluates its right hand side and writes the target
//!   immediately, so a later statement in the same block observes the new
//!   value.
//! * **Non-blocking** (`<=`) evaluates its right hand side *now*, against the
//!   values in the store as they are at that point, but defers the write. The
//!   deferred writes are returned as [`PendingUpdate`]s and land together when
//!   the caller runs [`commit_updates`].
//!
//! That is why `a <= b; b <= a;` swaps the two signals while `a = b; b = a;`
//! leaves both holding the original `b`.
//!
//! ```text
//! let pending = execute_statements(&block.statements, &mut store, &mut tasks)?;
//! let changed = commit_updates(pending, &mut store)?;
//! ```
//!
//! Deciding *when* a block runs is not this module's job — it executes a body
//! start to finish and hands the deferred writes back to a scheduler. A body
//! that suspends part way through, on a `#delay`, is beyond what a caller of
//! this entry point can express: use [`resume`] directly for that.

use crate::parsers::behavior::ProceduralStatements;
use crate::parsers::expr::Expression;
use crate::register::{Register, REAL_WIDTH};
use crate::simulator::eval::{
    eval, eval_sized, indexed_select_indices, indexed_select_width, EvalError, MAX_SELECT_WIDTH,
    SELF_DETERMINED,
};
use crate::simulator::program::{
    resume, Program, Resume, TaskTable, DELAY_UNSUPPORTED, FORK_TIMING_UNSUPPORTED,
    WAIT_UNSUPPORTED,
};
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::{Drive, DriveLevel, StateStore};
use crate::simulator::tasks::TaskContext;

/// An assignment target after its name and bit indices have been worked out,
/// so that writing it needs no further evaluation.
///
/// Resolving up front is what makes a deferred non-blocking write well defined:
/// the bits it lands on are the ones the left hand side named when the
/// statement ran, not the ones it would name after the rest of the block has
/// finished.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ResolvedTarget {
    /// A whole signal, as in `q <= d;`.
    Whole(String),
    /// A bit or part select, held as the declared bit indices it names, most
    /// significant first: `q[3:1] <= d;` resolves to `[3, 2, 1]`.
    Bits { name: String, indices: Vec<i64> },
    /// One word of a memory, as in `mem[addr] <= d;`. Written the same way as a
    /// bit select and told apart from one by the declaration alone.
    Word { name: String, index: i64 },
    /// A named event, as in `-> done;`. It holds no value, so the write that
    /// lands on it is a trigger and whatever was evaluated for it is dropped.
    Event(String),
    /// `{a, b, c} = v;` — several targets sharing one value, most significant
    /// part first.
    ///
    /// The only target that names more than one signal, which is why the
    /// paths that assume a single one check for it rather than trusting
    /// [`ResolvedTarget::name`].
    Parts(Vec<ResolvedTarget>),
}

impl ResolvedTarget {
    /// The signal this target writes into.
    ///
    /// A [`ResolvedTarget::Parts`] names several, so this reports the first —
    /// which is only ever asked of it by a caller that has already established
    /// the target is a single signal. `is_multiple` is how those callers ask.
    pub fn name(&self) -> &str {
        match self {
            ResolvedTarget::Whole(name) => name,
            ResolvedTarget::Bits { name, .. } => name,
            ResolvedTarget::Word { name, .. } => name,
            ResolvedTarget::Event(name) => name,
            ResolvedTarget::Parts(parts) => parts.first().map_or("", |part| part.name()),
        }
    }

    /// Whether this target names more than one signal.
    pub fn is_multiple(&self) -> bool {
        matches!(self, ResolvedTarget::Parts(_))
    }

    /// How many bits the target holds, which is the width context the right
    /// hand side of the assignment is evaluated in.
    ///
    /// A name the store does not have reports [`SELF_DETERMINED`] rather than
    /// a guess: the write is about to fail with
    /// [`SimulationError::UnknownSignal`] anyway, and a made up width would
    /// change the value it failed with.
    pub fn width(&self, state: &StateStore) -> usize {
        // A `real` target imposes no width at all: it is not a number of bits,
        // so the right hand side sizes itself. `real r; reg [7:0] a, b;
        // r = a * b;` multiplies in eight bits — 200 * 2 is 144 — where a
        // sixty-four bit context would make it 400. iverilog agrees.
        if self.is_real(state) {
            return SELF_DETERMINED;
        }
        match self {
            ResolvedTarget::Whole(name) => state
                .get_signal(name)
                .map_or(SELF_DETERMINED, |signal| signal.width()),
            ResolvedTarget::Bits { indices, .. } => indices.len(),
            // A word is as wide as the memory's element, which is a property of
            // the declaration rather than of the address being written.
            ResolvedTarget::Word { name, .. } => state
                .memory(name)
                .map_or(SELF_DETERMINED, |memory| memory.width()),
            // Nothing is written into an event, so the value a trigger carries
            // is sized by itself and then thrown away.
            ResolvedTarget::Event(_) => SELF_DETERMINED,
            // A concatenation is as wide as its parts add up to, which is what
            // sizes the right hand side that fills it.
            ResolvedTarget::Parts(parts) => parts.iter().map(|part| part.width(state)).sum(),
        }
    }

    /// Whether the target holds a `real`, which is what says a value written
    /// into it has to be *converted* rather than resized.
    ///
    /// A design with no real in it answers without hashing a name, the same
    /// shape `any_signed` and `any_memory` use. A bit or part select is never
    /// real: a `real` has no bits to select from. Neither is a concatenation,
    /// which is a run of bits however they were declared — so a real written
    /// into one is converted to an integer first and then split, rather than
    /// having its IEEE-754 encoding sliced up.
    pub fn is_real(&self, state: &StateStore) -> bool {
        if !state.any_real() {
            return false;
        }
        match self {
            ResolvedTarget::Whole(name) => state
                .get_signal(name)
                .is_some_and(|signal| signal.is_real()),
            ResolvedTarget::Word { name, .. } => {
                state.memory(name).is_some_and(|memory| memory.is_real())
            }
            ResolvedTarget::Bits { .. } | ResolvedTarget::Event(_) | ResolvedTarget::Parts(_) => {
                false
            }
        }
    }
}

/// A non-blocking (`<=`) update: a resolved target plus the value its right
/// hand side produced, waiting to be written.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PendingUpdate {
    target: ResolvedTarget,
    value: Register,
    /// When the write lands, for the one form that is not "at the end of this
    /// delta cycle": `a <= #5 b;`, whose right hand side is read now and whose
    /// write is scheduled without suspending the block.
    ///
    /// `None` is the ordinary non-blocking update, committed by
    /// [`commit_updates`] as soon as the block that queued it stops.
    at: Option<i64>,
}

impl PendingUpdate {
    /// Queues `value` to be written into `target` at the end of this delta
    /// cycle.
    pub fn new(target: ResolvedTarget, value: Register) -> Self {
        PendingUpdate {
            target,
            value,
            at: None,
        }
    }

    /// Queues `value` to be written into `target` at simulated time `at`.
    ///
    /// The value is the one the right hand side had when the statement ran —
    /// that is the whole point of an intra-assignment control — so it is
    /// carried here rather than re-read when the write lands.
    pub fn scheduled(target: ResolvedTarget, value: Register, at: i64) -> Self {
        PendingUpdate {
            target,
            value,
            at: Some(at),
        }
    }

    /// When the write lands, or `None` for an ordinary non-blocking update.
    pub fn at(&self) -> Option<i64> {
        self.at
    }

    /// Where the update will be written.
    pub fn target(&self) -> &ResolvedTarget {
        &self.target
    }

    /// The value the right hand side produced when the statement ran.
    pub fn value(&self) -> &Register {
        &self.value
    }
}

/// Runs `statements` to completion, applying every blocking assignment
/// immediately and collecting every non-blocking one for [`commit_updates`].
///
/// A block that suspends on a `#delay` cannot be reported through this
/// signature — the caller would have nowhere to keep the resume point — so a
/// suspension is [`SimulationError::Unsupported`]. Whatever ran before the
/// delay has already landed in `store`.
pub fn execute_statements(
    statements: &[ProceduralStatements],
    store: &mut StateStore,
    tasks: &mut TaskContext,
) -> Result<Vec<PendingUpdate>, SimulationError> {
    // Nothing that reaches this entry point has a task table to hand: it is
    // the run-to-completion path, and a task's body is inlined by the compiler
    // that elaboration drives. A task enabled here is `UnknownTask` by name.
    let program = Program::compile(statements, &TaskTable::new())?;
    match resume(&program, 0, store, tasks)? {
        Resume::Halted { pending } => Ok(pending),
        Resume::Suspended { .. } => Err(DELAY_UNSUPPORTED),
        Resume::Waiting { .. } => Err(WAIT_UNSUPPORTED),
        // A `disable` of a block this body is inside never gets here — it is a
        // jump. One naming anything else reaches for a block only the driver
        // holds, and this entry point has no driver behind it.
        Resume::Disabled { scope, .. } => Err(SimulationError::UnknownScope(scope)),
        // Threads are the driver's to hand out, and this is the entry point
        // with no driver behind it.
        Resume::Forked { .. } | Resume::BranchDone { .. } => Err(FORK_TIMING_UNSUPPORTED),
    }
}

/// Applies deferred non-blocking updates in the order they were queued,
/// reporting whether any stored state actually moved.
pub fn commit_updates(
    updates: Vec<PendingUpdate>,
    store: &mut StateStore,
) -> Result<bool, SimulationError> {
    let mut changed = false;
    for update in updates {
        // A scheduled write is not this delta cycle's business; the runner
        // holds it until its time comes. Committing it here would land it
        // immediately and lose the delay entirely.
        if update.at.is_some() {
            continue;
        }
        changed |= drive_resolved(store, &update.target, &update.value)?;
    }
    Ok(changed)
}

pub fn range_width(range: (i64, i64)) -> usize {
    ((range.0 - range.1).unsigned_abs() + 1) as usize
}

/// Works out which bits an assignment's left hand side names.
pub fn resolve_target(
    state: &StateStore,
    target: &Expression,
) -> Result<ResolvedTarget, SimulationError> {
    match target {
        Expression::Identifier(id) => {
            if !state.contains(&id.name) {
                // A name that is not a signal may still be a declared event,
                // which `-> done;` writes to. Only a miss on the signal map
                // asks, so an ordinary assignment costs the one hash it did.
                if state.is_event(&id.name) {
                    return Ok(ResolvedTarget::Event(id.name.clone()));
                }
                return Err(SimulationError::UnknownSignal(id.name.clone()));
            }
            Ok(ResolvedTarget::Whole(id.name.clone()))
        }
        Expression::BitSelect(id, index) => {
            // `a[3] = …` writes a bit and `m[3] = …` writes a word; the syntax
            // is the same and the declaration is what decides. `any_memory`
            // answers for a design that declares none without hashing the name.
            if state.any_memory() && state.memory(&id.name).is_some() {
                return Ok(ResolvedTarget::Word {
                    name: id.name.clone(),
                    index: target_index(state, index)?,
                });
            }
            Ok(ResolvedTarget::Bits {
                name: id.name.clone(),
                indices: vec![target_index(state, index)?],
            })
        }
        Expression::PartSelect(id, first, second) => {
            let first = target_index(state, first)?;
            let second = target_index(state, second)?;
            // A nonsense range — `a[1000000:0]`, or one whose bounds came out
            // of a parameter that is not what the design meant — names more
            // bits than any register has. Refusing it here is what stops the
            // `collect` below from trying to allocate the whole span; the
            // evaluator has always guarded its own copy of this.
            let selected = (first - second).unsigned_abs() as usize + 1;
            if selected > MAX_SELECT_WIDTH {
                return Err(EvalError::WidthOverflow(selected).into());
            }
            // Indices run most significant bit first, matching the bit order of
            // the register being written.
            let indices: Vec<i64> = if first >= second {
                (second..=first).rev().collect()
            } else {
                (first..=second).collect()
            };
            Ok(ResolvedTarget::Bits {
                name: id.name.clone(),
                indices,
            })
        }
        Expression::IndexedPartSelect {
            id,
            base,
            width,
            upward,
        } => {
            let span = indexed_select_width(width, state)?;
            // A write through an unknown base has nowhere to land. Reporting it
            // rather than writing somewhere arbitrary keeps the rule that a
            // wrong answer is never produced quietly.
            let indices = indexed_select_indices(base, span, *upward, state)?
                .ok_or_else(|| SimulationError::UnsupportedTarget(target.to_contracted_string()))?;
            Ok(ResolvedTarget::Bits {
                name: id.name.clone(),
                indices,
            })
        }
        // `{a, b, c} = v;` — each part resolved on its own, in source order,
        // which is most significant first.
        Expression::Concatenation(parts) => {
            let mut resolved = Vec::with_capacity(parts.len());
            for part in parts {
                resolved.push(resolve_target(state, part)?);
            }
            Ok(ResolvedTarget::Parts(resolved))
        }
        Expression::Parenthetical(inner) => resolve_target(state, inner),
        other => Err(SimulationError::UnsupportedTarget(
            other.to_contracted_string(),
        )),
    }
}

/// Writes `value` into whatever `target` names, reporting whether the stored
/// state actually moved. The value is resized to the width of the target the
/// way a Verilog assignment is: wider values lose their high bits, narrower
/// ones are zero extended — or sign extended, when the value being assigned is
/// a signed one.
pub fn drive(
    state: &mut StateStore,
    target: &Expression,
    value: &Register,
) -> Result<bool, SimulationError> {
    let target = resolve_target(state, target)?;
    drive_resolved(state, &target, value)
}

/// [`drive`] for a target whose bits have already been worked out.
///
/// This is the ordinary write every assignment goes through, so it is where
/// the precedence rule is enforced: a signal held by a `force` or a procedural
/// `assign` swallows the write outright rather than taking it and being
/// overwritten a moment later. Discarding it is what keeps it out of the
/// store's journal, and so out of the edges that wake blocks.
pub fn drive_resolved(
    state: &mut StateStore,
    target: &ResolvedTarget,
    value: &Register,
) -> Result<bool, SimulationError> {
    drive_at(state, target, value, DriveLevel::Procedural)
}

/// `value` as the type of `target` reads it.
///
/// An integer written into a `real` is converted to a double; a real written
/// into anything else is **rounded**, half away from zero — `i = 1.5;` is 2 and
/// `i = -1.5;` is -2, which is where an assignment differs from `$rtoi`, whose
/// truncation would give 1. Anything else is handed back untouched.
fn matched_to_target(state: &StateStore, target: &ResolvedTarget, value: &Register) -> Register {
    match (target.is_real(state), value.is_real()) {
        (true, false) => Register::from_f64(value.to_f64()),
        (false, true) => {
            // The *target's* width, not the real's sixty-four: a `reg [64:0]`
            // holds `2**64`, and converting into sixty-four bits first would
            // lose the bit that makes it that number (corpus `pr2913404`). A
            // target the store does not know is converted at the width a real
            // has, and the write then fails by name as it would have anyway.
            let width = match target.width(state) {
                SELF_DETERMINED => REAL_WIDTH,
                width => width,
            };
            Register::integer_from_f64(value.to_f64().round(), width)
        }
        _ => value.clone(),
    }
}

/// Which bits of `name` a drive stronger than `level` is holding.
///
/// Precedence is per *bit*, not per signal: `force bus[0] = 1;` holds one bit
/// and leaves every other one writable. A drive on the whole signal, or one
/// whose target cannot be resolved to bits, holds all of it.
enum Held {
    Nothing,
    Bits(Vec<i64>),
    Everything,
}

fn held_bits(state: &StateStore, name: &str, level: DriveLevel) -> Result<Held, SimulationError> {
    // The overwhelmingly common case is a design that forces nothing, and it
    // costs one length compare.
    if !state.has_drives() {
        return Ok(Held::Nothing);
    }
    let drives = state.drives();
    let mut bits: Vec<i64> = Vec::new();
    for drive in drives.iter() {
        if drive.name() != name || drive.level() <= level {
            continue;
        }
        match resolve_target(state, drive.target())? {
            ResolvedTarget::Bits { indices, .. } => bits.extend(indices),
            // A whole signal, a memory word or an event: nothing narrower to
            // say, so the write is refused outright.
            _ => return Ok(Held::Everything),
        }
    }
    if bits.is_empty() {
        Ok(Held::Nothing)
    } else {
        Ok(Held::Bits(bits))
    }
}

/// [`drive_resolved`] for a write made *by* a drive, which lands only if
/// nothing stronger holds the signal.
pub fn drive_at(
    state: &mut StateStore,
    target: &ResolvedTarget,
    value: &Register,
    level: DriveLevel,
) -> Result<bool, SimulationError> {
    // A real and an integer are converted into each other here, before the
    // value is resized, because resizing is what would destroy it: an integer
    // widened to sixty-four bits and then read as a double is a number nothing
    // wrote, and a double truncated to eight bits is not the value it denotes.
    // It has to happen while the value still carries its own signedness too —
    // `-1` in eight bits is -1.0 and not 255.0. It also has to happen *before*
    // a concatenation is split, so that `{a, b} = 2.5;` splits the integer 3
    // rather than the bits of a double.
    let converted;
    let value = if state.any_real() || value.is_real() {
        converted = matched_to_target(state, target, value);
        &converted
    } else {
        value
    };
    // A concatenation is then split before anything else looks at it: each part
    // is a target in its own right, with its own precedence and its own slice
    // of the value, most significant part first.
    if let ResolvedTarget::Parts(parts) = target {
        let total: usize = parts.iter().map(|part| part.width(state)).sum();
        let value = value.coerced(total);
        let codes = value.get_raw().to_vec();
        let mut offset = 0;
        let mut changed = false;
        for part in parts {
            let width = part.width(state);
            let slice = Register::from_bits(codes[offset..offset + width].to_vec());
            changed |= drive_at(state, part, &slice, level)?;
            offset += width;
        }
        return Ok(changed);
    }
    let held = held_bits(state, target.name(), level)?;
    if matches!(held, Held::Everything) {
        return Ok(false);
    }
    match target {
        ResolvedTarget::Whole(name) => {
            let signal = state
                .get_signal(name)
                .ok_or_else(|| SimulationError::UnknownSignal(name.clone()))?;
            let (width, range) = (signal.width(), signal.range());
            let mut value = value.coerced(width);
            // A write over a partly forced signal is *masked*, not refused:
            // `force r[1] = 1; r = 4'b1100;` leaves `1110`, because only bit 1
            // is held. iverilog agrees, and it is the only reading that makes
            // a one-bit force mean one bit.
            if let Held::Bits(bits) = &held {
                let mut codes = value.get_raw().to_vec();
                for index in bits {
                    if let Some(position) = signal.bit_position(*index) {
                        codes[position] = signal.bit(*index);
                    }
                }
                value = Register::from_bits(codes);
            }
            if signal.register() == &value {
                return Ok(false);
            }
            state.set_ranged(name.clone(), value, range);
            Ok(true)
        }
        ResolvedTarget::Bits { name, indices } => {
            // Drop the held bits from the write rather than the whole write:
            // a `force bus[0]` says nothing about `bus[1]`.
            if let Held::Bits(bits) = &held {
                let kept: Vec<(usize, i64)> = indices
                    .iter()
                    .copied()
                    .enumerate()
                    .filter(|(_, index)| !bits.contains(index))
                    .collect();
                if kept.len() != indices.len() {
                    if kept.is_empty() {
                        return Ok(false);
                    }
                    let value = value.coerced(indices.len());
                    let codes: Vec<u8> = kept
                        .iter()
                        .map(|(offset, _)| value.get_raw()[*offset])
                        .collect();
                    let kept: Vec<i64> = kept.into_iter().map(|(_, index)| index).collect();
                    return drive_bits(state, name, &kept, &Register::from_bits(codes));
                }
            }
            drive_bits(state, name, indices, value)
        }
        ResolvedTarget::Word { name, index } => drive_word(state, name, *index, value),
        ResolvedTarget::Event(name) => {
            state.trigger_event(name);
            // A trigger moves no stored value, and saying otherwise would keep
            // the continuous-assignment fixpoint from ever settling. The wake
            // it causes comes from the trigger journal, not from this flag.
            Ok(false)
        }
        // Split above, before precedence was asked about.
        ResolvedTarget::Parts(_) => unreachable!("a concatenation is split first"),
    }
}

/// Installs a procedural continuous drive — an `assign` or a `force` — and
/// applies it straight away.
///
/// The value lands here *before* the drive is recorded, which is what makes an
/// `assign` written while a `force` is in place do nothing visible: the
/// precedence rule is asked about the drives already installed, and the force
/// is one of them.
pub fn install_drive(
    state: &mut StateStore,
    target: &Expression,
    value: &Expression,
    level: DriveLevel,
) -> Result<(), SimulationError> {
    let resolved = resolve_target(state, target)?;
    // A drive is recorded against one signal name, so a concatenation has no
    // place to live. Reporting it is better than installing it on the first
    // part and quietly losing the rest.
    if resolved.is_multiple() {
        return Err(SimulationError::UnsupportedTarget(
            target.to_contracted_string(),
        ));
    }
    let evaluated = eval_sized(value, state, resolved.width(state))?;
    drive_at(state, &resolved, &evaluated, level)?;
    state.install_drive(Drive::new(
        resolved.name(),
        target.clone(),
        value.clone(),
        level,
    ));
    Ok(())
}

/// Re-evaluates a drive and writes what it produces.
///
/// Called both when the drive is first installed and on every pass of the
/// continuous-assignment fixpoint, which is what makes a forced signal follow
/// its expression rather than freeze at the value it had when the `force` ran.
pub fn apply_drive(state: &mut StateStore, drive: &Drive) -> Result<bool, SimulationError> {
    let target = resolve_target(state, drive.target())?;
    let value = eval_sized(drive.value(), state, target.width(state))?;
    drive_at(state, &target, &value, drive.level())
}

/// `release v;` — drops the `force` on `v` and hands the signal back.
///
/// What it falls back to is whatever else has a claim on it: the procedural
/// `assign` underneath, if one is still installed, and otherwise the value the
/// force displaced. That value is still the signal's last *procedural* one,
/// because every write made while it was forced was discarded rather than
/// stored.
pub fn release_drive(state: &mut StateStore, target: &Expression) -> Result<(), SimulationError> {
    let resolved = resolve_target(state, target)?;
    state.remove_drive(resolved.name(), DriveLevel::Force);
    // A `release` puts nothing back. A **net** reverts anyway, because its
    // continuous drivers reach it again on the next pass; a **variable** has no
    // driver, so it keeps the value the force left. That asymmetry is the whole
    // of the rule, and it is what iverilog does: releasing a forced `reg` holding
    // `1010` leaves `1010`, while releasing a forced `wire` returns it to its
    // assignment.
    if let Some(assign) = state.drive(resolved.name(), DriveLevel::Assign).cloned() {
        apply_drive(state, &assign)?;
    }
    Ok(())
}

/// `deassign v;` — drops the procedural continuous assignment on `v`, leaving
/// the value it last produced in place.
pub fn deassign_drive(state: &mut StateStore, target: &Expression) -> Result<(), SimulationError> {
    let resolved = resolve_target(state, target)?;
    state.remove_drive(resolved.name(), DriveLevel::Assign);
    Ok(())
}

/// Writes one word of a memory. An address outside the declared range discards
/// the write, matching what an out-of-range bit select does.
fn drive_word(
    state: &mut StateStore,
    name: &str,
    index: i64,
    value: &Register,
) -> Result<bool, SimulationError> {
    state
        .set_word(name, index, value)
        .ok_or_else(|| SimulationError::UnknownSignal(name.to_string()))
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
    let value = value.coerced(indices.len());
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

    use crate::parsers::assignment::assignment_lhs;
    use crate::parsers::behavior::parse_block;

    /// A store holding each named signal at the width of its binary literal,
    /// declared over `(width - 1, 0)`.
    fn store_with(signals: &[(&str, &str)]) -> StateStore {
        let mut store = StateStore::new();
        for (name, bits) in signals {
            let register = Register::from_binary(bits);
            let range = (register.width() as i64 - 1, 0);
            store.set_ranged(*name, register, range);
        }
        store
    }

    fn block(source: &str) -> Vec<ProceduralStatements> {
        let (remaining, statements) = parse_block(source).expect("block should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        statements
    }

    /// Executes a `begin … end` body and commits its non-blocking updates,
    /// returning whether the commit moved anything.
    fn run(source: &str, store: &mut StateStore) -> Result<bool, SimulationError> {
        let statements = block(source);
        let pending = execute_statements(&statements, store, &mut TaskContext::new())?;
        commit_updates(pending, store)
    }

    fn value(store: &StateStore, name: &str) -> String {
        store.get(name).expect("signal should exist").to_binary()
    }

    #[test]
    fn test_non_blocking_assignments_swap() {
        // Both right hand sides are read before either write lands, so the two
        // signals genuinely exchange values.
        let mut store = store_with(&[("a", "1010"), ("b", "0101")]);
        assert!(run("begin a <= b; b <= a; end", &mut store).unwrap());

        assert_eq!(value(&store, "a"), "0101");
        assert_eq!(value(&store, "b"), "1010");
    }

    #[test]
    fn test_blocking_assignments_do_not_swap() {
        // `a = b` lands before `b = a` is evaluated, so `b` is copied back onto
        // itself and both end up holding the original `b`.
        let mut store = store_with(&[("a", "1010"), ("b", "0101")]);
        assert!(!run("begin a = b; b = a; end", &mut store).unwrap());

        assert_eq!(value(&store, "a"), "0101");
        assert_eq!(value(&store, "b"), "0101");
    }

    #[test]
    fn test_blocking_assignment_is_visible_to_later_statements() {
        let mut store = store_with(&[("a", "0000"), ("b", "0011"), ("c", "0000")]);
        run("begin a = b + 1; c = a + 1; end", &mut store).unwrap();

        assert_eq!(value(&store, "a"), "0100");
        assert_eq!(value(&store, "c"), "0101");
    }

    #[test]
    fn test_non_blocking_assignment_is_not_visible_to_later_statements() {
        // `c` reads the *old* `a`, because `a`'s update has not landed yet.
        let mut store = store_with(&[("a", "0000"), ("b", "0011"), ("c", "0000")]);
        let statements = block("begin a <= b + 1; c <= a + 1; end");
        let pending = execute_statements(&statements, &mut store, &mut TaskContext::new()).unwrap();

        // Nothing has moved until the updates are committed.
        assert_eq!(value(&store, "a"), "0000");
        assert_eq!(pending.len(), 2);
        assert_eq!(pending[0].target(), &ResolvedTarget::Whole("a".to_string()));
        // The queued value is the raw right hand side; it is resized to the
        // target's width when the update lands.
        assert_eq!(pending[1].value().to_u128(), Some(1));

        commit_updates(pending, &mut store).unwrap();
        assert_eq!(value(&store, "a"), "0100");
        assert_eq!(value(&store, "c"), "0001");
    }

    #[test]
    fn test_last_non_blocking_write_to_a_signal_wins() {
        let mut store = store_with(&[("a", "0000"), ("b", "0011"), ("c", "0101")]);
        run("begin a <= b; a <= c; end", &mut store).unwrap();

        assert_eq!(value(&store, "a"), "0101");
    }

    #[test]
    fn test_commit_reports_no_change_when_state_does_not_move() {
        let mut store = store_with(&[("a", "0101"), ("b", "0101")]);
        assert!(!run("begin a <= b; end", &mut store).unwrap());
    }

    #[test]
    fn test_if_takes_the_then_branch() {
        let mut store = store_with(&[("sel", "1"), ("q", "0000"), ("b", "1111")]);
        run("begin if (sel) q = b; else q = 0; end", &mut store).unwrap();

        assert_eq!(value(&store, "q"), "1111");
    }

    #[test]
    fn test_if_with_unknown_condition_takes_the_else_branch() {
        // An `x` condition is false in Verilog, not an "unknown branch".
        let mut store = store_with(&[("sel", "x"), ("q", "0000"), ("b", "1111")]);
        run("begin if (sel) q = b; else q = 4'b0011; end", &mut store).unwrap();

        assert_eq!(value(&store, "q"), "0011");
    }

    #[test]
    fn test_if_with_unknown_condition_and_no_else_does_nothing() {
        let mut store = store_with(&[("sel", "x"), ("q", "0000"), ("b", "1111")]);
        run("begin if (sel) q = b; end", &mut store).unwrap();

        assert_eq!(value(&store, "q"), "0000");
    }

    #[test]
    fn test_if_condition_with_a_known_one_bit_is_true() {
        // `4'b1x00` is ambiguous but definitely non-zero, so it is true.
        let mut store = store_with(&[("sel", "1x00"), ("q", "0000"), ("b", "1111")]);
        run("begin if (sel) q = b; end", &mut store).unwrap();

        assert_eq!(value(&store, "q"), "1111");
    }

    #[test]
    fn test_nested_if() {
        let mut store = store_with(&[("a", "1"), ("b", "0"), ("q", "0000")]);
        run(
            r#"begin
                if (a) begin
                    if (b) q = 4'b0001;
                    else q = 4'b0010;
                end else begin
                    q = 4'b0100;
                end
            end"#,
            &mut store,
        )
        .unwrap();

        assert_eq!(value(&store, "q"), "0010");
    }

    #[test]
    fn test_else_if_chain() {
        let source = r#"begin
                if (a) q = 4'b0001;
                else if (b) q = 4'b0010;
                else if (c) q = 4'b0100;
                else q = 4'b1000;
            end"#;

        let mut store = store_with(&[("a", "0"), ("b", "0"), ("c", "1"), ("q", "0000")]);
        run(source, &mut store).unwrap();
        assert_eq!(value(&store, "q"), "0100");

        // Falling off the end of the chain reaches the final else.
        let mut store = store_with(&[("a", "0"), ("b", "0"), ("c", "0"), ("q", "0000")]);
        run(source, &mut store).unwrap();
        assert_eq!(value(&store, "q"), "1000");

        // An earlier arm wins over a later one that also matches.
        let mut store = store_with(&[("a", "1"), ("b", "1"), ("c", "1"), ("q", "0000")]);
        run(source, &mut store).unwrap();
        assert_eq!(value(&store, "q"), "0001");
    }

    #[test]
    fn test_case_matches_a_literal() {
        let mut store = store_with(&[("sel", "10"), ("q", "0000")]);
        run(
            r#"begin
                case (sel)
                    2'b00: q = 4'b0001;
                    2'b10: q = 4'b0010;
                    default: q = 4'b1111;
                endcase
            end"#,
            &mut store,
        )
        .unwrap();

        assert_eq!(value(&store, "q"), "0010");
    }

    #[test]
    fn test_case_falls_back_to_default() {
        let mut store = store_with(&[("sel", "11"), ("q", "0000")]);
        run(
            r#"begin
                case (sel)
                    2'b00: q = 4'b0001;
                    2'b10: q = 4'b0010;
                    default: q = 4'b1111;
                endcase
            end"#,
            &mut store,
        )
        .unwrap();

        assert_eq!(value(&store, "q"), "1111");
    }

    #[test]
    fn test_case_item_with_several_expressions() {
        let source = r#"begin
                case (sel)
                    2'b00, 2'b01, 2'b10: q = 4'b0011;
                    default: q = 4'b1111;
                endcase
            end"#;

        for bits in ["00", "01", "10"] {
            let mut store = store_with(&[("sel", bits), ("q", "0000")]);
            run(source, &mut store).unwrap();
            assert_eq!(value(&store, "q"), "0011", "sel = {}", bits);
        }

        let mut store = store_with(&[("sel", "11"), ("q", "0000")]);
        run(source, &mut store).unwrap();
        assert_eq!(value(&store, "q"), "1111");
    }

    #[test]
    fn test_case_with_no_match_and_no_default_is_a_no_op() {
        let mut store = store_with(&[("sel", "11"), ("q", "0000")]);
        run(
            r#"begin
                case (sel)
                    2'b00: q = 4'b0001;
                    2'b10: q = 4'b0010;
                endcase
            end"#,
            &mut store,
        )
        .unwrap();

        assert_eq!(value(&store, "q"), "0000");
    }

    #[test]
    fn test_case_subject_with_an_unknown_bit_matches_only_default() {
        // Plain `case` uses `==` semantics, so an `x` never matches a literal.
        let mut store = store_with(&[("sel", "1x"), ("q", "0000")]);
        run(
            r#"begin
                case (sel)
                    2'b10: q = 4'b0010;
                    default: q = 4'b1111;
                endcase
            end"#,
            &mut store,
        )
        .unwrap();

        assert_eq!(value(&store, "q"), "1111");
    }

    #[test]
    fn test_case_arm_can_queue_non_blocking_updates() {
        let mut store = store_with(&[("sel", "01"), ("a", "0000"), ("b", "1100")]);
        run(
            r#"begin
                case (sel)
                    2'b01: begin a <= b; b <= a; end
                    default: a <= 4'b1111;
                endcase
            end"#,
            &mut store,
        )
        .unwrap();

        assert_eq!(value(&store, "a"), "1100");
        assert_eq!(value(&store, "b"), "0000");
    }

    #[test]
    fn test_bit_and_part_select_targets() {
        let mut store = store_with(&[("q", "0000"), ("hi", "11")]);
        run("begin q[3:2] = hi; q[0] <= 1'b1; end", &mut store).unwrap();

        assert_eq!(value(&store, "q"), "1101");
    }

    #[test]
    fn test_deferred_select_writes_leave_undriven_bits_alone() {
        // Two non-blocking writes to disjoint slices of the same signal both
        // land, and the bit neither of them names keeps its old value.
        let mut store = store_with(&[("q", "0000")]);
        run("begin q[3:2] <= 2'b11; q[0] <= 1'b1; end", &mut store).unwrap();

        assert_eq!(value(&store, "q"), "1101");
    }

    #[test]
    fn test_delay_statement_is_unsupported() {
        let mut store = store_with(&[("a", "0000"), ("b", "1111")]);
        assert_eq!(
            run("begin a = b; #5; a = 4'b0001; end", &mut store),
            Err(SimulationError::Unsupported(
                "a delay inside a procedural block"
            ))
        );
        // The statements before the delay still ran.
        assert_eq!(value(&store, "a"), "1111");
    }

    #[test]
    fn test_assignment_delay_is_unsupported() {
        let mut store = store_with(&[("a", "0000"), ("b", "1111")]);
        assert_eq!(
            run("begin a = #5 b; end", &mut store),
            Err(SimulationError::Unsupported(
                "a delay inside a procedural block"
            ))
        );
        assert_eq!(
            run("begin #5 a = b; end", &mut store),
            Err(SimulationError::Unsupported(
                "a delay inside a procedural block"
            ))
        );
    }

    #[test]
    fn test_delay_nested_in_a_branch_is_unsupported() {
        let mut store = store_with(&[("sel", "1"), ("a", "0000")]);
        assert_eq!(
            run("begin if (sel) begin #5; end end", &mut store),
            Err(SimulationError::Unsupported(
                "a delay inside a procedural block"
            ))
        );
    }

    #[test]
    fn test_unknown_target_signal_is_an_error() {
        let mut store = store_with(&[("b", "1111")]);
        assert_eq!(
            run("begin nope = b; end", &mut store),
            Err(SimulationError::UnknownSignal("nope".to_string()))
        );
    }

    #[test]
    fn test_part_select_target_resolves_to_its_bit_indices() {
        let store = store_with(&[("q", "0000")]);
        let (_, target) = assignment_lhs("q[3:1]").unwrap();
        let resolved = resolve_target(&store, &target).unwrap();

        assert_eq!(
            resolved,
            ResolvedTarget::Bits {
                name: "q".to_string(),
                indices: vec![3, 2, 1],
            }
        );
        assert_eq!(resolved.name(), "q");
    }

    /// A part select naming an absurd number of bits is refused rather than
    /// allocated. The evaluator has always guarded this; the write path had
    /// not, so a design whose bounds came out wrong tried to build a vector of
    /// four billion indices and aborted the process.
    #[test]
    fn test_absurd_part_select_target_is_refused() {
        let state = store_with(&[("a", "1010")]);
        let target = assignment_lhs("a[1000000:0]")
            .expect("a part select should parse")
            .1;
        let error =
            resolve_target(&state, &target).expect_err("a million bits is not a target to build");
        assert!(
            matches!(error, SimulationError::Eval(EvalError::WidthOverflow(_))),
            "expected a width overflow, got {:?}",
            error
        );
    }
}

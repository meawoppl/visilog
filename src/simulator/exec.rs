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
use crate::register::Register;
use crate::simulator::eval::{
    eval, eval_sized, indexed_select_indices, indexed_select_width, EvalError, MAX_SELECT_WIDTH,
    SELF_DETERMINED,
};
use crate::simulator::program::{resume, Program, Resume, TaskTable, DELAY_UNSUPPORTED};
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
}

impl ResolvedTarget {
    /// The signal this target writes into.
    pub fn name(&self) -> &str {
        match self {
            ResolvedTarget::Whole(name) => name,
            ResolvedTarget::Bits { name, .. } => name,
            ResolvedTarget::Word { name, .. } => name,
            ResolvedTarget::Event(name) => name,
        }
    }

    /// How many bits the target holds, which is the width context the right
    /// hand side of the assignment is evaluated in.
    ///
    /// A name the store does not have reports [`SELF_DETERMINED`] rather than
    /// a guess: the write is about to fail with
    /// [`SimulationError::UnknownSignal`] anyway, and a made up width would
    /// change the value it failed with.
    pub fn width(&self, state: &StateStore) -> usize {
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
        }
    }
}

/// A non-blocking (`<=`) update: a resolved target plus the value its right
/// hand side produced, waiting to be written.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PendingUpdate {
    target: ResolvedTarget,
    value: Register,
}

impl PendingUpdate {
    /// Queues `value` to be written into `target`.
    pub fn new(target: ResolvedTarget, value: Register) -> Self {
        PendingUpdate { target, value }
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

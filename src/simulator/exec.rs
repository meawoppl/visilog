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
use crate::simulator::eval::eval;
use crate::simulator::program::{resume, Program, Resume, DELAY_UNSUPPORTED};
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::StateStore;
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
}

impl ResolvedTarget {
    /// The signal this target writes into.
    pub fn name(&self) -> &str {
        match self {
            ResolvedTarget::Whole(name) => name,
            ResolvedTarget::Bits { name, .. } => name,
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
    let program = Program::compile(statements)?;
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
                return Err(SimulationError::UnknownSignal(id.name.clone()));
            }
            Ok(ResolvedTarget::Whole(id.name.clone()))
        }
        Expression::BitSelect(id, index) => Ok(ResolvedTarget::Bits {
            name: id.name.clone(),
            indices: vec![target_index(state, index)?],
        }),
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
pub fn drive_resolved(
    state: &mut StateStore,
    target: &ResolvedTarget,
    value: &Register,
) -> Result<bool, SimulationError> {
    match target {
        ResolvedTarget::Whole(name) => {
            let signal = state
                .get_signal(name)
                .ok_or_else(|| SimulationError::UnknownSignal(name.clone()))?;
            let (width, range) = (signal.width(), signal.range());
            let value = value.coerced(width);
            if signal.register() == &value {
                return Ok(false);
            }
            state.set_ranged(name.clone(), value, range);
            Ok(true)
        }
        ResolvedTarget::Bits { name, indices } => drive_bits(state, name, indices, value),
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
}

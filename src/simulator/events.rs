//! Edge detection and sensitivity-list matching.
//!
//! A sequential scheduler runs in steps: it snapshots the [`StateStore`], lets
//! something change it, then asks which `always` blocks that change woke up.
//! This module owns the second half of that question.
//!
//! [`edges_between`] turns a pair of snapshots into a list of [`SignalEdge`]s,
//! and [`control_fires`] decides whether an [`EventControl`] is satisfied by
//! those edges. [`signals_read`] supplies the implicit sensitivity list that
//! `@(*)` needs.
//!
//! # Edges are a property of the least significant bit
//!
//! Verilog defines `posedge` and `negedge` on the LSB of the triggering
//! expression, not on the vector as a whole. A `reg [31:0] counter` stepping
//! from 4 to 5 is a `posedge` of `counter`; stepping from 5 to 7 is no edge at
//! all, because bit 0 never moved. [`SignalEdge::is_posedge`] and
//! [`SignalEdge::is_negedge`] therefore only look at
//! [`Register::bit_from_lsb`] of index 0.
//!
//! # Four-state edges
//!
//! An edge is not simply `0 -> 1`. Following the IEEE 1364 edge table:
//!
//! | | `-> 0` | `-> 1` | `-> x` | `-> z` |
//! | --- | --- | --- | --- | --- |
//! | `0 ->` | — | pos | pos | pos |
//! | `1 ->` | neg | — | neg | neg |
//! | `x ->` | neg | pos | — | — |
//! | `z ->` | neg | pos | — | — |
//!
//! `x -> z` and `z -> x` are changes but not edges, which is why
//! [`EventTriggers::EitherEdge`] is checked against the whole value rather than
//! against the pos/neg predicates.

use std::collections::BTreeSet;

use crate::parsers::behavior::{
    AlwaysBlock, CaseLabel, Event, EventControl, EventTriggers, ProceduralStatements,
    SystemTaskArgument,
};
use crate::parsers::expr::Expression;
use crate::register::{Register, ONE, X, Z, ZERO};
use crate::simulator::state_store::StateStore;

/// One signal's transition across a time step.
///
/// Only produced for signals that actually moved: `before != after` holds for
/// every `SignalEdge` [`edges_between`] returns.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SignalEdge {
    pub name: String,
    pub before: Register,
    pub after: Register,
}

impl SignalEdge {
    pub fn new(name: impl Into<String>, before: Register, after: Register) -> Self {
        SignalEdge {
            name: name.into(),
            before,
            after,
        }
    }

    /// The `(before, after)` least significant bits, or `None` if either value
    /// is zero bits wide and so has no LSB to compare.
    pub fn lsb_transition(&self) -> Option<(u8, u8)> {
        Some((self.before.bit_from_lsb(0)?, self.after.bit_from_lsb(0)?))
    }

    /// Whether the LSB made a `posedge` transition: `0 -> 1`, `0 -> x`,
    /// `0 -> z`, `x -> 1` or `z -> 1`.
    pub fn is_posedge(&self) -> bool {
        matches!(
            self.lsb_transition(),
            Some((ZERO, ONE) | (ZERO, X) | (ZERO, Z) | (X, ONE) | (Z, ONE))
        )
    }

    /// Whether the LSB made a `negedge` transition: `1 -> 0`, `1 -> x`,
    /// `1 -> z`, `x -> 0` or `z -> 0`.
    pub fn is_negedge(&self) -> bool {
        matches!(
            self.lsb_transition(),
            Some((ONE, ZERO) | (ONE, X) | (ONE, Z) | (X, ZERO) | (Z, ZERO))
        )
    }

    /// Whether this edge satisfies `trigger`.
    ///
    /// [`EventTriggers::EitherEdge`] is the level-sensitive entry a bare
    /// `@(a or b)` parses to, so it matches any change of the *whole* value —
    /// including a change confined to the upper bits, and including `x -> z`,
    /// neither of which is a pos or neg edge.
    pub fn matches(&self, trigger: &EventTriggers) -> bool {
        match trigger {
            EventTriggers::PosEdge => self.is_posedge(),
            EventTriggers::NegEdge => self.is_negedge(),
            EventTriggers::EitherEdge => self.before != self.after,
        }
    }
}

/// Every signal whose value differs between two snapshots, sorted by name.
///
/// A signal that appears in only one of the snapshots is skipped: an edge is a
/// transition between two values, and a signal that was just declared (or has
/// gone away) has no second value to transition from. Declaring a signal is not
/// a simulation event.
pub fn edges_between(before: &StateStore, after: &StateStore) -> Vec<SignalEdge> {
    let mut edges = Vec::new();
    for name in before.names() {
        let (Some(old), Some(new)) = (before.get(name), after.get(name)) else {
            continue;
        };
        if old != new {
            edges.push(SignalEdge::new(name, old.clone(), new.clone()));
        }
    }
    edges
}

/// [`edges_between`] for a store that tracked its own writes.
///
/// `changes` is what [`StateStore::take_changes`] reported — each written
/// signal paired with the value it held before — and `after` is that same store
/// now. The result is identical to diffing a snapshot taken at the marker
/// against `after`, because a signal nobody wrote cannot have moved; the
/// difference is that this costs the number of signals that were written rather
/// than the number of signals in the design.
///
/// A write that put the same value back is filtered out here, so both functions
/// agree that a value which did not move is no edge. A name that has since
/// vanished from the store is skipped for the same reason `edges_between` skips
/// one that is missing from a snapshot: an edge needs two values.
pub fn edges_from_changes(changes: Vec<(String, Register)>, after: &StateStore) -> Vec<SignalEdge> {
    changes
        .into_iter()
        .filter_map(|(name, before)| {
            let current = after.get(&name)?;
            if current == &before {
                return None;
            }
            let after = current.clone();
            Some(SignalEdge {
                name,
                before,
                after,
            })
        })
        .collect()
}

/// Whether an `always` block's event control fires given the edges observed.
///
/// `implicit_reads` is only consulted for [`EventControl::Implicit`]; it is the
/// body's read set, which callers normally get from [`signals_read`] once and
/// cache. [`always_block_fires`] is the convenience wrapper that computes it.
///
/// The three variants:
///
/// * [`EventControl::None`] — `always begin … end`, a block with no event
///   control at all. It waits on nothing, so it is always ready to run and this
///   returns `true` unconditionally. Returning `false` would mean such a block
///   never executes, which is simply wrong; not spinning forever is the
///   scheduler's problem, and it has to lean on the delays inside the body (an
///   `always` block with no event control and no delay is a zero-delay
///   infinite loop in real Verilog too).
/// * [`EventControl::Implicit`] — `@(*)`, fires when any signal the body reads
///   changed at all, matching the level sensitivity `@(*)` is shorthand for.
/// * [`EventControl::Events`] — fires if *any* listed event matches an observed
///   edge.
pub fn control_fires(
    control: &EventControl,
    edges: &[SignalEdge],
    implicit_reads: &BTreeSet<String>,
) -> bool {
    match control {
        EventControl::None => true,
        EventControl::Implicit => edges.iter().any(|edge| implicit_reads.contains(&edge.name)),
        EventControl::Events(events) => events.iter().any(|event| event_fires(event, edges)),
    }
}

/// [`control_fires`] for a whole `always` block, deriving the `@(*)` read set
/// from the block's own body.
pub fn always_block_fires(block: &AlwaysBlock, edges: &[SignalEdge]) -> bool {
    let implicit_reads = match block.event_control {
        EventControl::Implicit => signals_read(&block.statements),
        _ => BTreeSet::new(),
    };
    control_fires(&block.event_control, edges, &implicit_reads)
}

/// Whether one sensitivity-list entry matches an observed edge.
///
/// An `Event`'s expression is an [`Expression`] in general but is a bare
/// identifier in practice (`posedge clk`), which is the case handled exactly.
/// For anything more complex the edge of the *evaluated* expression would have
/// to be tracked, which this module has no way to observe; rather than silently
/// never firing, the entry is treated as sensitive to every signal the
/// expression reads and fires when any of them shows a matching edge. That
/// over-approximates — `posedge (a & b)` fires on a `posedge` of either operand
/// — so a block may be woken more often than it should, never less.
fn event_fires(event: &Event, edges: &[SignalEdge]) -> bool {
    let names = event_signals(&event.expression);
    edges
        .iter()
        .any(|edge| names.contains(&edge.name) && edge.matches(&event.trigger))
}

/// The signal names a sensitivity-list entry is sensitive to.
fn event_signals(expression: &Expression) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    collect_expression_reads(expression, &mut names);
    names
}

/// Every signal name read by a body — the implicit sensitivity list for `@(*)`.
///
/// Assignment *targets* are excluded, since writing a signal does not make a
/// block sensitive to it. An index inside a target is still a read, though:
/// `mem[addr] <= d` reads `addr` and `d` and writes `mem`.
///
/// Case item labels count as reads: they are evaluated and compared against the
/// case subject, so a label naming a signal is sensitive to it.
pub fn signals_read(statements: &[ProceduralStatements]) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    collect_statement_reads(statements, &mut names);
    names
}

fn collect_statement_reads(statements: &[ProceduralStatements], names: &mut BTreeSet<String>) {
    for statement in statements {
        match statement {
            // A `#5;` delay reads nothing.
            ProceduralStatements::Delay(_) => {}
            // A delay reads nothing, but the statement it prefixes does.
            ProceduralStatements::Delayed { statements, .. } => {
                collect_statement_reads(statements, names)
            }
            ProceduralStatements::Assignment(assignment) => {
                collect_target_reads(assignment.lhs(), names);
                collect_expression_reads(assignment.rhs(), names);
            }
            ProceduralStatements::If(statement) => {
                collect_expression_reads(&statement.condition, names);
                collect_statement_reads(&statement.then_statements, names);
                if let Some(else_statements) = &statement.else_statements {
                    collect_statement_reads(else_statements, names);
                }
            }
            // A system task reads whatever its arguments name, and a value it
            // prints is as good a reason for an `@(*)` block to wake as one it
            // assigns.
            ProceduralStatements::SystemTask(call) => {
                for argument in &call.arguments {
                    if let SystemTaskArgument::Expression(expression) = argument {
                        collect_expression_reads(expression, names);
                    }
                }
            }
            ProceduralStatements::Case(statement) => {
                collect_expression_reads(&statement.subject, names);
                for item in &statement.items {
                    if let CaseLabel::Expressions(labels) = &item.label {
                        for label in labels {
                            collect_expression_reads(label, names);
                        }
                    }
                    collect_statement_reads(&item.statements, names);
                }
            }
        }
    }
}

/// The reads hiding inside an assignment target. The target's own name is not
/// one of them, but a select index or a part-select bound is.
fn collect_target_reads(target: &Expression, names: &mut BTreeSet<String>) {
    match target {
        Expression::Identifier(_) => {}
        Expression::Parenthetical(inner) => collect_target_reads(inner, names),
        Expression::Concatenation(parts) => {
            for part in parts {
                collect_target_reads(part, names);
            }
        }
        Expression::BitSelect(_, index) => collect_expression_reads(index, names),
        Expression::PartSelect(_, msb, lsb) => {
            collect_expression_reads(msb, names);
            collect_expression_reads(lsb, names);
        }
        // Nothing else is a legal target; treat it as a read rather than drop it.
        other => collect_expression_reads(other, names),
    }
}

fn collect_expression_reads(expression: &Expression, names: &mut BTreeSet<String>) {
    match expression {
        Expression::Constant(_) => {}
        Expression::Identifier(id) => {
            names.insert(id.name.clone());
        }
        Expression::Unary(_, operand) => collect_expression_reads(operand, names),
        Expression::Binary(lhs, _, rhs) => {
            collect_expression_reads(lhs, names);
            collect_expression_reads(rhs, names);
        }
        Expression::Conditional(condition, when_true, when_false) => {
            collect_expression_reads(condition, names);
            collect_expression_reads(when_true, names);
            collect_expression_reads(when_false, names);
        }
        Expression::Parenthetical(inner) => collect_expression_reads(inner, names),
        Expression::Concatenation(parts) => {
            for part in parts {
                collect_expression_reads(part, names);
            }
        }
        // The called function is not a signal, but its arguments are read.
        Expression::FunctionCall(_, arguments) => {
            for argument in arguments {
                collect_expression_reads(argument, names);
            }
        }
        Expression::BitSelect(id, index) => {
            names.insert(id.name.clone());
            collect_expression_reads(index, names);
        }
        Expression::PartSelect(id, msb, lsb) => {
            names.insert(id.name.clone());
            collect_expression_reads(msb, names);
            collect_expression_reads(lsb, names);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::assignment::{ProceduralAssignment, ProceduralAssignmentType};
    use crate::parsers::behavior::{parse_always_block, parse_block, parse_sensitivity_list};
    use crate::parsers::helpers::assert_parses;

    /// An edge on a signal named `s`, written as binary bit strings.
    fn edge(before: &str, after: &str) -> SignalEdge {
        SignalEdge::new(
            "s",
            Register::from_binary(before),
            Register::from_binary(after),
        )
    }

    fn named_edge(name: &str, before: &str, after: &str) -> SignalEdge {
        SignalEdge::new(
            name,
            Register::from_binary(before),
            Register::from_binary(after),
        )
    }

    fn body(source: &str) -> Vec<ProceduralStatements> {
        assert_parses(parse_block, source)
    }

    fn name_set(names: &[&str]) -> BTreeSet<String> {
        names.iter().map(|name| name.to_string()).collect()
    }

    #[test]
    fn test_posedge_covers_every_four_state_transition() {
        for (before, after) in [("0", "1"), ("0", "x"), ("0", "z"), ("x", "1"), ("z", "1")] {
            let edge = edge(before, after);
            assert!(
                edge.is_posedge(),
                "{} -> {} should be a posedge",
                before,
                after
            );
            assert!(
                !edge.is_negedge(),
                "{} -> {} should not be a negedge",
                before,
                after
            );
            assert!(edge.matches(&EventTriggers::PosEdge));
            assert!(!edge.matches(&EventTriggers::NegEdge));
        }
    }

    #[test]
    fn test_negedge_covers_every_four_state_transition() {
        for (before, after) in [("1", "0"), ("1", "x"), ("1", "z"), ("x", "0"), ("z", "0")] {
            let edge = edge(before, after);
            assert!(
                edge.is_negedge(),
                "{} -> {} should be a negedge",
                before,
                after
            );
            assert!(
                !edge.is_posedge(),
                "{} -> {} should not be a posedge",
                before,
                after
            );
            assert!(edge.matches(&EventTriggers::NegEdge));
            assert!(!edge.matches(&EventTriggers::PosEdge));
        }
    }

    /// `x -> z` and `z -> x` are changes but neither a pos nor a neg edge, so a
    /// level-sensitive entry still sees them.
    #[test]
    fn test_unknown_to_high_impedance_is_a_change_but_not_an_edge() {
        for (before, after) in [("x", "z"), ("z", "x")] {
            let edge = edge(before, after);
            assert!(!edge.is_posedge());
            assert!(!edge.is_negedge());
            assert!(edge.matches(&EventTriggers::EitherEdge));
        }
    }

    #[test]
    fn test_a_value_that_did_not_move_is_no_edge_at_all() {
        for value in ["0", "1", "x", "z"] {
            let edge = edge(value, value);
            assert!(!edge.is_posedge());
            assert!(!edge.is_negedge());
            assert!(!edge.matches(&EventTriggers::EitherEdge));
        }
    }

    /// Edges live on the least significant bit of a vector, not on its value.
    #[test]
    fn test_edges_are_detected_on_the_least_significant_bit() {
        // 4 -> 5: bit 0 went 0 -> 1.
        let counting_up = edge("0100", "0101");
        assert!(counting_up.is_posedge());

        // 5 -> 7: bit 1 moved, bit 0 did not. A change, but not an edge.
        let upper_bit_only = edge("0101", "0111");
        assert!(!upper_bit_only.is_posedge());
        assert!(!upper_bit_only.is_negedge());
        assert!(upper_bit_only.matches(&EventTriggers::EitherEdge));

        // 7 -> 6: bit 0 went 1 -> 0.
        let counting_down = edge("0111", "0110");
        assert!(counting_down.is_negedge());

        // The x is in the upper bits, so the LSB transition is a plain 0 -> 1.
        let unknown_upper_bits = edge("x000", "x001");
        assert!(unknown_upper_bits.is_posedge());
    }

    #[test]
    fn test_zero_width_values_have_no_lsb_and_no_edge() {
        let edge = edge("", "");
        assert_eq!(edge.lsb_transition(), None);
        assert!(!edge.is_posedge());
        assert!(!edge.is_negedge());
    }

    #[test]
    fn test_edges_between_reports_only_signals_that_moved() {
        let mut before = StateStore::new();
        before.set("clk", Register::from_binary("0"));
        before.set("data", Register::from_binary("1010"));
        before.set("still", Register::from_binary("11"));

        let mut after = before.clone();
        after.set("clk", Register::from_binary("1"));
        after.set("data", Register::from_binary("1011"));

        let edges = edges_between(&before, &after);
        assert_eq!(
            edges,
            vec![
                named_edge("clk", "0", "1"),
                named_edge("data", "1010", "1011"),
            ]
        );
        assert!(edges.iter().all(|edge| edge.name != "still"));
    }

    #[test]
    fn test_edges_between_skips_signals_present_in_only_one_snapshot() {
        let mut before = StateStore::new();
        before.set("gone", Register::from_binary("0"));
        before.set("kept", Register::from_binary("0"));

        let mut after = StateStore::new();
        after.set("kept", Register::from_binary("1"));
        after.set("fresh", Register::from_binary("1"));

        let edges = edges_between(&before, &after);
        assert_eq!(edges, vec![named_edge("kept", "0", "1")]);
    }

    /// A widened value is a change even when the numeric value is the same.
    #[test]
    fn test_edges_between_treats_a_width_change_as_a_change() {
        let mut before = StateStore::new();
        before.set("bus", Register::from_binary("1"));
        let mut after = StateStore::new();
        after.set("bus", Register::from_binary("01"));

        let edges = edges_between(&before, &after);
        assert_eq!(edges.len(), 1);
        assert!(edges[0].matches(&EventTriggers::EitherEdge));
        // The LSB stayed at 1, so it is not an edge.
        assert!(!edges[0].is_posedge());
        assert!(!edges[0].is_negedge());
    }

    /// A block with no event control waits on nothing, so it is always ready.
    #[test]
    fn test_control_fires_none_always_fires() {
        let no_reads = BTreeSet::new();
        assert!(control_fires(&EventControl::None, &[], &no_reads));
        assert!(control_fires(
            &EventControl::None,
            &[named_edge("clk", "0", "1")],
            &no_reads
        ));
    }

    #[test]
    fn test_control_fires_implicit_follows_the_read_set() {
        let reads = name_set(&["a", "b"]);

        assert!(control_fires(
            &EventControl::Implicit,
            &[named_edge("b", "0000", "0010")],
            &reads
        ));
        assert!(!control_fires(
            &EventControl::Implicit,
            &[named_edge("c", "0", "1")],
            &reads
        ));
        assert!(!control_fires(&EventControl::Implicit, &[], &reads));
    }

    #[test]
    fn test_control_fires_events_matches_one_entry_of_the_list() {
        let control = assert_parses(parse_sensitivity_list, "@(posedge clk or negedge rst)");
        let no_reads = BTreeSet::new();

        // Only rst moved, and in the direction the list asks for.
        assert!(control_fires(
            &control,
            &[named_edge("rst", "1", "0")],
            &no_reads
        ));
        // Only clk moved, in the listed direction.
        assert!(control_fires(
            &control,
            &[named_edge("clk", "0", "1")],
            &no_reads
        ));
        // clk moved the wrong way and rst did not move at all.
        assert!(!control_fires(
            &control,
            &[named_edge("clk", "1", "0")],
            &no_reads
        ));
        // A signal that is not in the list.
        assert!(!control_fires(
            &control,
            &[named_edge("data", "0", "1")],
            &no_reads
        ));
        assert!(!control_fires(&control, &[], &no_reads));
    }

    #[test]
    fn test_control_fires_events_handles_reset_going_unknown() {
        let control = assert_parses(parse_sensitivity_list, "@(posedge clk or negedge rst)");
        let no_reads = BTreeSet::new();

        // 1 -> x is a negedge of rst, which naive `1 -> 0` matching would miss.
        assert!(control_fires(
            &control,
            &[named_edge("rst", "1", "x")],
            &no_reads
        ));
        // x -> 1 is a posedge of clk.
        assert!(control_fires(
            &control,
            &[named_edge("clk", "x", "1")],
            &no_reads
        ));
    }

    #[test]
    fn test_control_fires_level_sensitive_list() {
        let control = assert_parses(parse_sensitivity_list, "@(a or b)");
        let no_reads = BTreeSet::new();

        // Only the upper bit of b moved: no edge, but a level entry still fires.
        assert!(control_fires(
            &control,
            &[named_edge("b", "0101", "0111")],
            &no_reads
        ));
        assert!(!control_fires(
            &control,
            &[named_edge("c", "0", "1")],
            &no_reads
        ));
    }

    /// A non-identifier event expression falls back to its operands.
    #[test]
    fn test_control_fires_on_a_compound_event_expression() {
        let control = assert_parses(parse_sensitivity_list, "@(posedge a & b)");
        let no_reads = BTreeSet::new();

        assert!(control_fires(
            &control,
            &[named_edge("a", "0", "1")],
            &no_reads
        ));
        assert!(control_fires(
            &control,
            &[named_edge("b", "0", "1")],
            &no_reads
        ));
        assert!(!control_fires(
            &control,
            &[named_edge("a", "1", "0")],
            &no_reads
        ));
    }

    #[test]
    fn test_always_block_fires_derives_the_implicit_read_set() {
        let block = assert_parses(parse_always_block, "always @(*) begin y = a & b; end");

        assert!(always_block_fires(&block, &[named_edge("a", "0", "1")]));
        assert!(always_block_fires(&block, &[named_edge("b", "1", "1010")]));
        // `y` is written, not read, so it does not wake its own block.
        assert!(!always_block_fires(&block, &[named_edge("y", "0", "1")]));
        assert!(!always_block_fires(&block, &[]));
    }

    #[test]
    fn test_always_block_fires_for_an_explicit_list() {
        let block = assert_parses(
            parse_always_block,
            "always @(posedge clk) begin q <= d; end",
        );

        assert!(always_block_fires(&block, &[named_edge("clk", "0", "1")]));
        // `d` is read but the list is explicit, so reading it is not enough.
        assert!(!always_block_fires(&block, &[named_edge("d", "0", "1")]));
    }

    /// Every expression form the walker knows about, in one body: parentheses,
    /// binary and unary operators, a conditional, bit and part selects, a
    /// function call's arguments and a concatenation.
    #[test]
    fn test_signals_read_walks_nested_expressions() {
        let statements = body(
            r#"begin
                   y = (a + b[1]) ? f(c, d[7:4]) : ~e;
                   z = {g, h[3]};
               end"#,
        );

        assert_eq!(
            signals_read(&statements),
            name_set(&["a", "b", "c", "d", "e", "g", "h"])
        );
    }

    #[test]
    fn test_signals_read_excludes_assignment_targets() {
        let statements = body(
            r#"begin
                   q <= d;
                   count = count + 1;
               end"#,
        );

        // `q` is only ever written; `count` is read on the right hand side.
        assert_eq!(signals_read(&statements), name_set(&["count", "d"]));
    }

    /// The target's name is not a read, but an index inside the target is:
    /// `mem[addr] <= d` reads `addr` and `d` and writes `mem`.
    ///
    /// The AST is built by hand because the assignment parser only accepts a
    /// literal index on the left of an assignment.
    #[test]
    fn test_signals_read_includes_reads_inside_a_target() {
        let assignment = ProceduralAssignment::new(
            Expression::BitSelect(
                "mem".into(),
                Box::new(Expression::Identifier("addr".into())),
            ),
            ProceduralAssignmentType::NonBlocking,
            None,
            Expression::Identifier("d".into()),
        );

        let statements = vec![ProceduralStatements::Assignment(assignment)];
        assert_eq!(signals_read(&statements), name_set(&["addr", "d"]));
    }

    #[test]
    fn test_signals_read_covers_both_if_branches() {
        let statements = body(
            r#"begin
                   if (rst) begin
                       count <= seed;
                   end else begin
                       count <= count + step;
                   end
               end"#,
        );

        assert_eq!(
            signals_read(&statements),
            name_set(&["count", "rst", "seed", "step"])
        );
    }

    #[test]
    fn test_signals_read_covers_case_subject_labels_and_bodies() {
        let statements = body(
            r#"begin
                   case (state)
                       IDLE: next = start;
                       BUSY, DONE: next = a + b;
                       default: next = 0;
                   endcase
               end"#,
        );

        assert_eq!(
            signals_read(&statements),
            name_set(&["BUSY", "DONE", "IDLE", "a", "b", "start", "state"])
        );
    }

    #[test]
    fn test_signals_read_of_a_delay_only_body_is_empty() {
        let statements = body("begin #10; end");
        assert!(signals_read(&statements).is_empty());
    }
}

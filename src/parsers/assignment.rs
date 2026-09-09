use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::char,
    combinator::{map, opt},
    multi::separated_list0,
    sequence::delimited,
    IResult,
};

use crate::parsers::expr::{
    bit_select, indexed_part_select, part_select, verilog_expression, Expression,
};
use crate::parsers::gates::{drive_strength, DriveStrength};
use crate::parsers::identifier::identifier;

use super::{
    behavior::{assignment_timing, EventControl},
    delay::Delay,
    simple::ws,
};

#[derive(Debug, PartialEq, Clone)]
pub struct ContinuousAssignment {
    lhs: Expression,
    rhs: Expression,
    strength: Option<DriveStrength>,
}

impl ContinuousAssignment {
    pub fn new(lhs: Expression, rhs: Expression) -> Self {
        ContinuousAssignment {
            lhs,
            rhs,
            strength: None,
        }
    }

    pub fn with_strength(
        lhs: Expression,
        rhs: Expression,
        strength: Option<DriveStrength>,
    ) -> Self {
        ContinuousAssignment { lhs, rhs, strength }
    }

    /// The driven target, e.g. the `x` of `assign x = y;`.
    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    /// The driving expression, e.g. the `y` of `assign x = y;`.
    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }

    /// The declared drive strengths, e.g. the `(strong1, highz0)` of
    /// `assign (strong1, highz0) x = y;`. `None` is an assignment that named
    /// none, which drives at `strong` like any other.
    pub fn strength(&self) -> Option<DriveStrength> {
        self.strength
    }
}

/// `assign x = y;`, optionally carrying a drive strength pair and any number of
/// comma-separated targets: `assign (weak1, weak0) a = 1, b = 2;`.
///
/// One `assign` is one strength pair shared by every target it names, the same
/// way one declaration is one width shared by every name in its list. The pair
/// is [`drive_strength`], the very production a gate primitive uses — an
/// `assign` and a `bufif1` declare the same thing and resolve through the same
/// [`resolve_bit`](crate::simulator::gates::resolve_bit).
pub fn parse_continuous_assignment(input: &str) -> IResult<&str, Vec<ContinuousAssignment>> {
    let (input, _) = ws(tag("assign"))(input)?;
    let (mut input, strength) = opt(drive_strength)(input)?;

    let mut assignments = Vec::new();
    loop {
        let (rest, lhs) = ws(assignment_lhs)(input)?;
        let (rest, _) = ws(char('='))(rest)?;
        let (rest, rhs) = verilog_expression(rest)?;
        assignments.push(ContinuousAssignment::with_strength(lhs, rhs, strength));

        match ws(char(','))(rest) {
            Ok((next, _)) => input = next,
            Err(nom::Err::Error(_)) => {
                let (rest, _) = ws(char(';'))(rest)?;
                return Ok((rest, assignments));
            }
            Err(err) => return Err(err),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ProceduralAssignmentType {
    Blocking,
    NonBlocking,
}

/// The timing control written between an assignment's `=` and its right hand
/// side.
///
/// It is *intra-assignment*: the right hand side is evaluated when the
/// statement runs, and only the write waits. That is what tells `a = #5 b;`
/// from `#5 a = b;`, which reads `b` five time units later.
#[derive(Debug, PartialEq, Clone)]
pub enum AssignmentTiming {
    /// `a = #5 b;`
    Delay(Delay),
    /// `a = @(posedge clk) b;`, and with a `repeat` count the event has to
    /// happen that many times before the write lands.
    Event {
        repeat: Option<Expression>,
        control: EventControl,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProceduralAssignment {
    lhs: Expression,
    assignment_type: ProceduralAssignmentType,
    timing: Option<AssignmentTiming>,
    rhs: Expression,
}

impl ProceduralAssignment {
    pub fn new(
        lhs: Expression,
        assignment_type: ProceduralAssignmentType,
        timing: Option<AssignmentTiming>,
        rhs: Expression,
    ) -> Self {
        ProceduralAssignment {
            lhs,
            assignment_type,
            timing,
            rhs,
        }
    }

    /// The assigned target, e.g. the `x` of `x <= y;`.
    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    /// The assigned expression, e.g. the `y` of `x <= y;`.
    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }

    /// Whether this is `=` (blocking) or `<=` (non-blocking). The distinction
    /// drives when the target is updated, so a simulator must respect it.
    pub fn assignment_type(&self) -> &ProceduralAssignmentType {
        &self.assignment_type
    }

    /// What comes between evaluating the right side and updating the target,
    /// e.g. the `#50` of `x = #50 y;` or the `@(posedge clk)` of
    /// `x = @(posedge clk) y;`.
    pub fn timing(&self) -> Option<&AssignmentTiming> {
        self.timing.as_ref()
    }
}

/// `x = y;` / `x <= y;`. A leading `#5` is *not* part of an assignment — a
/// delay prefixes any procedural statement, so `behavior.rs` owns it.
pub fn parse_assignment(input: &str) -> IResult<&str, ProceduralAssignment> {
    let (input, lhs) = ws(assignment_lhs)(input)?;
    let (input, assign_op) = ws(alt((tag("="), tag("<="))))(input)?;
    let (input, timing) = opt(assignment_timing)(input)?;
    let (input, rhs) = verilog_expression(input)?;
    let (input, _) = ws(char(';'))(input)?;

    let assignment_type = match assign_op {
        "=" => ProceduralAssignmentType::Blocking,
        "<=" => ProceduralAssignmentType::NonBlocking,
        _ => unreachable!(),
    };

    Ok((
        input,
        ProceduralAssignment::new(lhs, assignment_type, timing, rhs),
    ))
}

/// The target of an assignment: a whole signal, a bit or part select of one, or
/// a concatenation of those.
///
/// `bit_select` is tried before `part_select` so that a conditional index —
/// `q[a ? b : c]` — is read as a bit select rather than having its `:` mistaken
/// for a part-select separator. Both bounds of a part select are ordinary
/// expressions, so `q[n:m]` and `q[i]` work as well as literal indices.
pub fn assignment_lhs(input: &str) -> IResult<&str, Expression> {
    alt((
        bit_select,
        indexed_part_select,
        part_select,
        map(identifier, Expression::Identifier),
        parse_concatenation,
    ))(input)
}

pub fn parse_concatenation(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            char('{'),
            separated_list0(char(','), ws(assignment_lhs)),
            char('}'),
        ),
        |exprs| Expression::Concatenation(exprs),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::constants::VerilogConstant;
    use crate::parsers::expr::Expression;
    use crate::parsers::gates::StrengthLevel;
    use crate::parsers::helpers::assert_parses_to;
    use crate::parsers::identifier::Identifier;
    use crate::parsers::operators::BinaryOperator;

    fn ident(name: &str) -> Expression {
        Expression::Identifier(Identifier::new(name.to_string()))
    }

    /// Parses one `assign` that names a single target, asserting it consumed
    /// the whole input.
    fn only(input: &str) -> ContinuousAssignment {
        let (remaining, mut assignments) =
            parse_continuous_assignment(input).expect("continuous assignment should parse");
        assert!(remaining.is_empty(), "unparsed input: {}", remaining);
        assert_eq!(assignments.len(), 1, "expected one target in `{}`", input);
        assignments.pop().unwrap()
    }

    #[test]
    fn test_assignment_lhs() {
        let cases = vec![
            (
                "a",
                Expression::Identifier(Identifier::new("a".to_string())),
            ),
            (
                "a[3]",
                Expression::BitSelect(
                    Identifier::new("a".to_string()),
                    Box::new(Expression::Constant(VerilogConstant::from_int(3))),
                ),
            ),
            (
                "a[3:0]",
                Expression::PartSelect(
                    Identifier::new("a".to_string()),
                    Box::new(Expression::Constant(VerilogConstant::from_int(3))),
                    Box::new(Expression::Constant(VerilogConstant::from_int(0))),
                ),
            ),
            (
                "{a, b, c}",
                Expression::Concatenation(vec![
                    Expression::Identifier(Identifier::new("a".to_string())),
                    Expression::Identifier(Identifier::new("b".to_string())),
                    Expression::Identifier(Identifier::new("c".to_string())),
                ]),
            ),
        ];

        for (input, expected) in cases {
            let result = assignment_lhs(input);
            assert!(result.is_ok(), "Failed to parse '{}'", input);
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, "");
            assert_eq!(expr, expected);
        }
    }

    #[test]
    fn test_parse_blocking_assignment() {
        let input = "a = b;";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (remaining, assignment) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(
            assignment.lhs,
            Expression::Identifier(Identifier::new("a".to_string()))
        );
        assert_eq!(
            assignment.rhs,
            Expression::Identifier(Identifier::new("b".to_string()))
        );

        assert_eq!(
            assignment.assignment_type,
            ProceduralAssignmentType::Blocking
        );
    }

    #[test]
    fn test_parse_nonblocking_assignment() {
        let input = "a <= b;";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (remaining, assignment) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(
            assignment.lhs,
            Expression::Identifier(Identifier::new("a".to_string()))
        );
        assert_eq!(
            assignment.rhs,
            Expression::Identifier(Identifier::new("b".to_string()))
        );
    }

    #[test]
    fn test_assignment_lhs_literal_bit_select() {
        assert_parses_to(
            assignment_lhs,
            "a[3]",
            Expression::BitSelect(
                Identifier::new("a".to_string()),
                Box::new(Expression::Constant(VerilogConstant::from_int(3))),
            ),
        );
    }

    #[test]
    fn test_assignment_lhs_literal_part_select() {
        assert_parses_to(
            assignment_lhs,
            "a[3:0]",
            Expression::PartSelect(
                Identifier::new("a".to_string()),
                Box::new(Expression::Constant(VerilogConstant::from_int(3))),
                Box::new(Expression::Constant(VerilogConstant::from_int(0))),
            ),
        );
    }

    /// A variable index is kept as an identifier expression, not folded into a
    /// constant.
    #[test]
    fn test_assignment_lhs_variable_bit_select() {
        assert_parses_to(
            assignment_lhs,
            "q[i]",
            Expression::BitSelect(Identifier::new("q".to_string()), Box::new(ident("i"))),
        );
    }

    #[test]
    fn test_assignment_lhs_expression_bit_select() {
        assert_parses_to(
            assignment_lhs,
            "q[a+1]",
            Expression::BitSelect(
                Identifier::new("q".to_string()),
                Box::new(Expression::Binary(
                    Box::new(ident("a")),
                    BinaryOperator::Addition,
                    Box::new(Expression::Constant(VerilogConstant::from_int(1))),
                )),
            ),
        );
    }

    #[test]
    fn test_assignment_lhs_variable_part_select() {
        assert_parses_to(
            assignment_lhs,
            "q[n:m]",
            Expression::PartSelect(
                Identifier::new("q".to_string()),
                Box::new(ident("n")),
                Box::new(ident("m")),
            ),
        );
    }

    /// `q[a ? b : c]` shares its opening shape with a part select, and the
    /// conditional's `:` looks exactly like a part-select separator. It is a bit
    /// select: `assignment_lhs` tries `bit_select` first, and the whole
    /// conditional is consumed as the index.
    #[test]
    fn test_assignment_lhs_conditional_index_is_a_bit_select() {
        assert_parses_to(
            assignment_lhs,
            "q[a ? b : c]",
            Expression::BitSelect(
                Identifier::new("q".to_string()),
                Box::new(Expression::Conditional(
                    Box::new(ident("a")),
                    Box::new(ident("b")),
                    Box::new(ident("c")),
                )),
            ),
        );
    }

    /// The converse of the case above: a part select whose msb happens to be a
    /// conditional. Parenthesising the conditional ends the index expression at
    /// the `)`, leaving the part-select `:` to be found.
    #[test]
    fn test_assignment_lhs_parenthesised_conditional_part_select() {
        assert_parses_to(
            assignment_lhs,
            "q[(a ? b : c):0]",
            Expression::PartSelect(
                Identifier::new("q".to_string()),
                Box::new(Expression::Parenthetical(Box::new(
                    Expression::Conditional(
                        Box::new(ident("a")),
                        Box::new(ident("b")),
                        Box::new(ident("c")),
                    ),
                ))),
                Box::new(Expression::Constant(VerilogConstant::from_int(0))),
            ),
        );
    }

    #[test]
    fn test_parse_assignment_with_variable_bit_select() {
        let (remaining, assignment) = parse_assignment("q[i] <= 1'b1;").unwrap();
        assert!(remaining.is_empty());
        assert_eq!(
            assignment.lhs,
            Expression::BitSelect(Identifier::new("q".to_string()), Box::new(ident("i")))
        );
    }

    #[test]
    fn test_parse_assignment_with_variable_part_select() {
        let (remaining, assignment) = parse_assignment("q[n:m] <= x;").unwrap();
        assert!(remaining.is_empty());
        assert_eq!(
            assignment.lhs,
            Expression::PartSelect(
                Identifier::new("q".to_string()),
                Box::new(ident("n")),
                Box::new(ident("m")),
            )
        );
    }

    #[test]
    fn test_parse_continuous_assignment_with_variable_bit_select() {
        let assignment = only("assign mem[addr] = data;");
        assert_eq!(
            assignment.lhs,
            Expression::BitSelect(Identifier::new("mem".to_string()), Box::new(ident("addr")))
        );
    }

    #[test]
    fn test_parse_concatenation() {
        let input = "{a, b, c}";
        let result = parse_concatenation(input);
        assert!(result.is_ok());
        let (remaining, expr) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(
            expr,
            Expression::Concatenation(vec![
                Expression::Identifier(Identifier::new("a".to_string())),
                Expression::Identifier(Identifier::new("b".to_string())),
                Expression::Identifier(Identifier::new("c".to_string())),
            ])
        );
    }

    #[test]
    fn test_parse_nonblocking_assignment_with_bit_select() {
        let input = "a[3] <= b;";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (remaining, assignment) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(
            assignment.lhs,
            Expression::BitSelect(
                Identifier::new("a".to_string()),
                Box::new(Expression::Constant(VerilogConstant::from_int(3))),
            )
        );
        assert_eq!(
            assignment.rhs,
            Expression::Identifier(Identifier::new("b".to_string()))
        );
    }

    #[test]
    fn test_parse_nonblocking_assignment_with_part_select() {
        let input = "a[3:0] <= b;";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (remaining, assignment) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(
            assignment.lhs,
            Expression::PartSelect(
                Identifier::new("a".to_string()),
                Box::new(Expression::Constant(VerilogConstant::from_int(3))),
                Box::new(Expression::Constant(VerilogConstant::from_int(0))),
            )
        );
        assert_eq!(
            assignment.rhs,
            Expression::Identifier(Identifier::new("b".to_string()))
        );
    }

    #[test]
    fn test_parse_nonblocking_assignment_with_concatenation() {
        let input = "{a, b, c} <= d;";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (remaining, assignment) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(
            assignment.lhs,
            Expression::Concatenation(vec![
                Expression::Identifier(Identifier::new("a".to_string())),
                Expression::Identifier(Identifier::new("b".to_string())),
                Expression::Identifier(Identifier::new("c".to_string())),
            ])
        );
        assert_eq!(
            assignment.rhs,
            Expression::Identifier(Identifier::new("d".to_string()))
        );
    }

    #[test]
    fn test_parse_continuous_assignment() {
        let assignment = only("assign a = b;");
        assert_eq!(
            assignment.lhs,
            Expression::Identifier(Identifier::new("a".to_string()))
        );
        assert_eq!(
            assignment.rhs,
            Expression::Identifier(Identifier::new("b".to_string()))
        );
    }

    /// An `assign` that named no strength carries none, which the simulator
    /// reads as the `strong` every driver has always had.
    #[test]
    fn test_continuous_assignment_without_a_strength() {
        assert_eq!(only("assign a = b;").strength(), None);
    }

    /// The pair is `gates.rs`'s production, so an `assign` places each half by
    /// the digit it ends with and takes either write order. Getting this wrong
    /// would swap which polarity floats — a wrong answer, not a parse failure.
    #[test]
    fn test_continuous_assignment_strength_in_either_order() {
        let expected = DriveStrength {
            zero: StrengthLevel::Highz,
            one: StrengthLevel::Strong,
        };
        assert_eq!(
            only("assign (strong1, highz0) a = b;").strength(),
            Some(expected)
        );
        assert_eq!(
            only("assign (highz0, strong1) a = b;").strength(),
            Some(expected)
        );
    }

    #[test]
    fn test_continuous_assignment_strength_levels() {
        let cases = [
            (
                "assign (supply1, supply0) a = b;",
                StrengthLevel::Supply,
                StrengthLevel::Supply,
            ),
            (
                "assign (weak0,   weak1) a = b;",
                StrengthLevel::Weak,
                StrengthLevel::Weak,
            ),
            (
                "assign (pull1,strong0) a = b;",
                StrengthLevel::Strong,
                StrengthLevel::Pull,
            ),
            (
                "assign (highz1,  strong0) a = b;",
                StrengthLevel::Strong,
                StrengthLevel::Highz,
            ),
        ];

        for (source, zero, one) in cases {
            assert_eq!(
                only(source).strength(),
                Some(DriveStrength { zero, one }),
                "wrong strength for `{}`",
                source
            );
        }
    }

    /// One `assign` may name several targets, the way one declaration names
    /// several signals.
    #[test]
    fn test_parse_continuous_assignment_list() {
        let (remaining, assignments) =
            parse_continuous_assignment("assign a = 4'd5, b = 4'd8, c = 4'd12;").unwrap();
        assert!(remaining.is_empty());
        assert_eq!(assignments.len(), 3);
        assert_eq!(assignments[0].lhs(), &ident("a"));
        assert_eq!(assignments[1].lhs(), &ident("b"));
        assert_eq!(assignments[2].lhs(), &ident("c"));
    }

    /// The strength belongs to the `assign` rather than to a target, so every
    /// target in its list shares it — the same way one declaration's width is
    /// shared by every name in it.
    #[test]
    fn test_continuous_assignment_list_shares_one_strength() {
        let (remaining, assignments) =
            parse_continuous_assignment("assign (weak1, weak0) a = 1, b = 0;").unwrap();
        assert!(remaining.is_empty());
        let expected = Some(DriveStrength {
            zero: StrengthLevel::Weak,
            one: StrengthLevel::Weak,
        });
        assert_eq!(assignments[0].strength(), expected);
        assert_eq!(assignments[1].strength(), expected);
    }

    #[test]
    fn test_parse_continuous_assignment_with_part_select() {
        let assignment = only("assign a[3:0] = b;");
        assert_eq!(
            assignment.lhs,
            Expression::PartSelect(
                Identifier::new("a".to_string()),
                Box::new(Expression::Constant(VerilogConstant::from_int(3))),
                Box::new(Expression::Constant(VerilogConstant::from_int(0))),
            )
        );
        assert_eq!(
            assignment.rhs,
            Expression::Identifier(Identifier::new("b".to_string()))
        );
    }

    #[test]
    fn test_parse_continuous_assignment_with_concatenation() {
        let assignment = only("assign {a, b, c} = d;");
        assert_eq!(
            assignment.lhs,
            Expression::Concatenation(vec![
                Expression::Identifier(Identifier::new("a".to_string())),
                Expression::Identifier(Identifier::new("b".to_string())),
                Expression::Identifier(Identifier::new("c".to_string())),
            ])
        );
        assert_eq!(
            assignment.rhs,
            Expression::Identifier(Identifier::new("d".to_string()))
        );
    }
}

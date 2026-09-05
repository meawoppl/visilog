use nom::{
    branch::alt, bytes::complete::tag, character::complete::char, combinator::map,
    multi::separated_list0, sequence::delimited, IResult,
};

use crate::parsers::expr::{bit_select, part_select, verilog_expression, Expression};
use crate::parsers::identifier::identifier;

use super::{
    delay::{parse_delay_opt, Delay},
    simple::ws,
};

#[derive(Debug, PartialEq, Clone)]
pub struct ContinuousAssignment {
    lhs: Expression,
    rhs: Expression,
}

impl ContinuousAssignment {
    pub fn new(lhs: Expression, rhs: Expression) -> Self {
        ContinuousAssignment { lhs, rhs }
    }

    /// The driven target, e.g. the `x` of `assign x = y;`.
    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    /// The driving expression, e.g. the `y` of `assign x = y;`.
    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }
}

pub fn parse_continuous_assignment(input: &str) -> IResult<&str, ContinuousAssignment> {
    let (input, _) = ws(tag("assign"))(input)?;
    let (input, lhs) = assignment_lhs(input)?;
    let (input, _) = ws(char('='))(input)?;
    let (input, rhs) = verilog_expression(input)?;
    let (input, _) = ws(char(';'))(input)?;

    Ok((input, ContinuousAssignment::new(lhs, rhs)))
}

#[derive(Debug, PartialEq, Clone)]
pub enum ProceduralAssignmentType {
    Blocking,
    NonBlocking,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProceduralAssignment {
    lhs: Expression,
    assignment_type: ProceduralAssignmentType,
    assignment_delay: Option<Delay>,
    rhs: Expression,
}

impl ProceduralAssignment {
    pub fn new(
        lhs: Expression,
        assignment_type: ProceduralAssignmentType,
        assignment_delay: Option<Delay>,
        rhs: Expression,
    ) -> Self {
        ProceduralAssignment {
            lhs,
            assignment_type,
            assignment_delay,
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

    /// The delay between evaluating the right side and updating the target,
    /// e.g. the `#50` of `x = #50 y;`.
    pub fn assignment_delay(&self) -> Option<&Delay> {
        self.assignment_delay.as_ref()
    }
}

/// `x = y;` / `x <= y;`. A leading `#5` is *not* part of an assignment — a
/// delay prefixes any procedural statement, so `behavior.rs` owns it.
pub fn parse_assignment(input: &str) -> IResult<&str, ProceduralAssignment> {
    let (input, lhs) = ws(assignment_lhs)(input)?;
    let (input, assign_op) = ws(alt((tag("="), tag("<="))))(input)?;
    let (input, assignment_delay) = parse_delay_opt(input)?;
    let (input, rhs) = verilog_expression(input)?;
    let (input, _) = ws(char(';'))(input)?;

    let assignment_type = match assign_op {
        "=" => ProceduralAssignmentType::Blocking,
        "<=" => ProceduralAssignmentType::NonBlocking,
        _ => unreachable!(),
    };

    Ok((
        input,
        ProceduralAssignment::new(lhs, assignment_type, assignment_delay, rhs),
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
    use crate::parsers::helpers::assert_parses_to;
    use crate::parsers::identifier::Identifier;
    use crate::parsers::operators::BinaryOperator;

    fn ident(name: &str) -> Expression {
        Expression::Identifier(Identifier::new(name.to_string()))
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
        let (remaining, assignment) = parse_continuous_assignment("assign mem[addr] = data;")
            .expect("variable bit select should parse as a continuous assignment target");
        assert!(remaining.is_empty());
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
        let input = "assign a = b;";
        let result = parse_continuous_assignment(input);
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
    fn test_parse_continuous_assignment_with_part_select() {
        let input = "assign a[3:0] = b;";
        let result = parse_continuous_assignment(input);
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
    fn test_parse_continuous_assignment_with_concatenation() {
        let input = "assign {a, b, c} = d;";
        let result = parse_continuous_assignment(input);
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
}

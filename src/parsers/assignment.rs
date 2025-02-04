use nom::{
    branch::alt,
    bytes::complete::{tag, take_while_m_n},
    character::complete::{char, multispace0},
    combinator::{map, map_res},
    multi::separated_list0,
    sequence::{delimited, preceded},
    IResult,
};

use crate::parsers::expr::{verilog_expression, Expression};
use crate::parsers::identifier::{identifier, Identifier};

use super::{
    constants::VerilogConstant,
    delay::{parse_delay_opt, Delay},
    simple::ws,
};

#[derive(Debug, PartialEq, Clone)]
pub enum AssignmentType {
    Continuous,
    Procedural,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assignment {
    pre_delay: Option<Delay>,
    lhs: Expression,
    assignment_type: AssignmentType,
    assignment_delay: Option<Delay>,
    rhs: Expression,
}

impl Assignment {
    pub fn new(
        pre_delay: Option<Delay>,
        lhs: Expression,
        assignment_type: AssignmentType,
        assignment_delay: Option<Delay>,
        rhs: Expression,
    ) -> Self {
        Assignment {
            pre_delay,
            lhs,
            assignment_type,
            assignment_delay,
            rhs,
        }
    }
}

pub fn parse_assignment(input: &str) -> IResult<&str, Assignment> {
    let (input, pre_delay) = ws(parse_delay_opt)(input)?;
    let (input, lhs) = assignment_lhs(input)?;
    let (input, assign_op) = ws(alt((tag("="), tag("<="))))(input)?;
    let (input, assignment_delay) = parse_delay_opt(input)?;
    let (input, rhs) = verilog_expression(input)?;
    let (input, _) = ws(char(';'))(input)?;

    let assignment_type = match assign_op {
        "=" => AssignmentType::Continuous,
        "<=" => AssignmentType::Procedural,
        _ => unreachable!(),
    };

    Ok((
        input,
        Assignment::new(pre_delay, lhs, assignment_type, assignment_delay, rhs),
    ))
}

pub fn assignment_lhs(input: &str) -> IResult<&str, Expression> {
    alt((
        map(parse_part_select, |(id, start, end)| {
            Expression::PartSelect(
                id,
                Box::new(Expression::Constant(VerilogConstant::from_int(start))),
                Box::new(Expression::Constant(VerilogConstant::from_int(end))),
            )
        }),
        map(parse_bit_select, |(id, index)| {
            Expression::BitSelect(
                id,
                Box::new(Expression::Constant(VerilogConstant::from_int(index))),
            )
        }),
        map(identifier, Expression::Identifier),
        parse_concatenation,
    ))(input)
}

pub fn parse_bit_select(input: &str) -> IResult<&str, (Identifier, i64)> {
    let (input, id) = identifier(input)?;
    let (input, _) = char('[')(input)?;
    let (input, index) = map_res(
        take_while_m_n(1, 10, |c: char| c.is_digit(10)),
        |s: &str| s.parse::<i64>(),
    )(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, (id, index)))
}

pub fn parse_part_select(input: &str) -> IResult<&str, (Identifier, i64, i64)> {
    let (input, id) = identifier(input)?;
    let (input, _) = char('[')(input)?;
    let (input, start) = map_res(
        take_while_m_n(1, 10, |c: char| c.is_digit(10)),
        |s: &str| s.parse::<i64>(),
    )(input)?;
    let (input, _) = char(':')(input)?;
    let (input, end) = map_res(
        take_while_m_n(1, 10, |c: char| c.is_digit(10)),
        |s: &str| s.parse::<i64>(),
    )(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, (id, start, end)))
}

pub fn parse_concatenation(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            char('{'),
            separated_list0(preceded(multispace0, ws(char(','))), assignment_lhs),
            char('}'),
        ),
        |exprs| Expression::Concatenation(exprs),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::expr::Expression;
    use crate::parsers::identifier::Identifier;

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
    fn test_parse_continuous_assignment() {
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

        assert_eq!(assignment.assignment_type, AssignmentType::Continuous);
    }

    #[test]
    fn test_parse_procedural_assignment() {
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
    fn test_parse_bit_select() {
        let input = "a[3]";
        let result = parse_bit_select(input);
        assert!(result.is_ok());
        let (remaining, (id, index)) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(id, Identifier::new("a".to_string()));
        assert_eq!(index, 3);
    }

    #[test]
    fn test_parse_part_select() {
        let input = "a[3:0]";
        let result = parse_part_select(input);
        assert!(result.is_ok());
        let (remaining, (id, start, end)) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(id, Identifier::new("a".to_string()));
        assert_eq!(start, 3);
        assert_eq!(end, 0);
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
    fn test_parse_procedural_assignment_with_bit_select() {
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
    fn test_parse_procedural_assignment_with_part_select() {
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
    fn test_parse_procedural_assignment_with_concatenation() {
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
}

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::char,
    combinator::{opt, value},
    multi::separated_list1,
    IResult,
};

use super::{
    expr::{verilog_expression, Expression},
    identifier::{identifier, Identifier},
    simple::{range, ws},
};

#[derive(Debug, PartialEq, Clone)]
pub enum ParameterKind {
    Parameter,
    LocalParam,
}

#[derive(Debug, PartialEq)]
pub struct ParameterDeclaration {
    pub kind: ParameterKind,
    pub name: Identifier,
    pub range: Option<(i64, i64)>,
    pub value: Expression,
}

fn parameter_kind(input: &str) -> IResult<&str, ParameterKind> {
    alt((
        value(ParameterKind::LocalParam, tag("localparam")),
        value(ParameterKind::Parameter, tag("parameter")),
    ))(input)
}

fn parameter_assignment(input: &str) -> IResult<&str, (Identifier, Expression)> {
    let (input, name) = ws(identifier)(input)?;
    let (input, _) = ws(char('='))(input)?;
    let (input, value) = verilog_expression(input)?;
    Ok((input, (name, value)))
}

/// Parse a `parameter` / `localparam` declaration, eg.
///
/// `localparam IDLE = 2'b00;`
///
/// `parameter [7:0] WIDTH = 8, DEPTH = 16;`
pub fn parse_parameter_declaration(input: &str) -> IResult<&str, Vec<ParameterDeclaration>> {
    let (input, kind) = ws(parameter_kind)(input)?;
    let (input, range) = opt(ws(range))(input)?;
    let (input, assignments) = separated_list1(ws(char(',')), parameter_assignment)(input)?;
    let (input, _) = ws(char(';'))(input)?;

    let declarations = assignments
        .into_iter()
        .map(|(name, value)| ParameterDeclaration {
            kind: kind.clone(),
            name,
            range,
            value,
        })
        .collect();

    Ok((input, declarations))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::helpers::{assert_parses, assert_parses_to};

    #[test]
    fn test_parse_localparam() {
        assert_parses_to(
            parse_parameter_declaration,
            "localparam IDLE = 2'b00;",
            vec![ParameterDeclaration {
                kind: ParameterKind::LocalParam,
                name: "IDLE".into(),
                range: None,
                value: verilog_expression("2'b00").unwrap().1,
            }],
        );
    }

    #[test]
    fn test_parse_parameter_with_range() {
        assert_parses_to(
            parse_parameter_declaration,
            "parameter [7:0] WIDTH = 8;",
            vec![ParameterDeclaration {
                kind: ParameterKind::Parameter,
                name: "WIDTH".into(),
                range: Some((7, 0)),
                value: verilog_expression("8").unwrap().1,
            }],
        );
    }

    #[test]
    fn test_parse_parameter_list() {
        let declarations = assert_parses(
            parse_parameter_declaration,
            "parameter WIDTH = 8, DEPTH = 16 ;",
        );
        assert_eq!(declarations.len(), 2);
        assert_eq!(declarations[0].name, "WIDTH".into());
        assert_eq!(declarations[1].name, "DEPTH".into());
    }

    #[test]
    fn test_parse_parameter_expression_value() {
        let declarations =
            assert_parses(parse_parameter_declaration, "localparam TOTAL = WIDTH * 2;");
        assert_eq!(declarations.len(), 1);
        assert_eq!(
            declarations[0].value,
            verilog_expression("WIDTH * 2").unwrap().1
        );
    }

    #[test]
    fn test_parameter_declaration_requires_semicolon() {
        assert!(parse_parameter_declaration("localparam IDLE = 2'b00").is_err());
    }
}

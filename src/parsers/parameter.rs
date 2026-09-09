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
    simple::{range, signedness, ws, Range},
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
    pub range: Option<Range>,
    /// Whether the declaration carried a `signed` qualifier, or a type that
    /// implies one. An unqualified parameter takes its value's signedness
    /// instead, which is why this is a plain `bool` rather than an `Option`.
    pub signed: bool,
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
    // `parameter integer p = 1;` — a type name stands where `signed` would,
    // and `integer` is signed by definition. `real` is not accepted here; it
    // is a different value type entirely (issue #93).
    let (input, typed) = opt(ws(tag("integer")))(input)?;
    let (input, signed) = ws(signedness)(input)?;
    let (input, range) = opt(ws(range))(input)?;
    let signed = signed || typed.is_some();
    let (input, assignments) = separated_list1(ws(char(',')), parameter_assignment)(input)?;
    let (input, _) = ws(char(';'))(input)?;

    let declarations = assignments
        .into_iter()
        .map(|(name, value)| ParameterDeclaration {
            kind: kind.clone(),
            name,
            range: range.clone(),
            signed,
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
                signed: false,
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
                range: Some(Range::Constant(7, 0)),
                signed: false,
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

    /// A `signed` qualifier, or an `integer` type name standing where it would.
    /// An unqualified parameter is left alone — it takes its value's
    /// signedness, which is why the field is a plain `bool`.
    #[test]
    fn test_parameter_signedness_qualifiers() {
        let signed = assert_parses(
            parse_parameter_declaration,
            "parameter signed [7:0] p = -1;",
        );
        assert!(signed[0].signed);
        assert_eq!(signed[0].range, Some(Range::Constant(7, 0)));

        let integer = assert_parses(parse_parameter_declaration, "parameter integer p = 1;");
        assert!(integer[0].signed, "`integer` is signed by definition");

        let local = assert_parses(
            parse_parameter_declaration,
            "localparam signed [3:0] q = -2;",
        );
        assert!(local[0].signed);
        assert_eq!(local[0].kind, ParameterKind::LocalParam);

        let plain = assert_parses(parse_parameter_declaration, "parameter p = 1;");
        assert!(!plain[0].signed);
    }
}

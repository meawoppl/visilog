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
    /// Whether the declaration named the type `real`, which makes the value a
    /// double however it was written: `parameter real HALF = 1;` is `1.0`.
    pub real: bool,
    pub value: Expression,
}

/// The type keyword a parameter may carry in place of `signed`: `parameter
/// integer p = 1;`, `parameter real pi = 3.14;`. Each one implies a
/// signedness, and `real` implies a value type as well.
fn parameter_type(input: &str) -> IResult<&str, bool> {
    alt((
        value(true, tag("realtime")),
        value(true, tag("real")),
        value(false, tag("integer")),
    ))(input)
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
    // `parameter integer p = 1;` — a type name stands where `signed` would.
    // Both types it may name are signed, and `real` says the value is a double
    // as well.
    let (input, typed) = opt(ws(parameter_type))(input)?;
    let (input, signed) = ws(signedness)(input)?;
    let (input, range) = opt(ws(range))(input)?;
    let real = typed == Some(true);
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
            real,
            value,
        })
        .collect();

    Ok((input, declarations))
}

/// The qualifiers a parameter port item may carry before its name.
struct ParameterQualifiers {
    kind: ParameterKind,
    signed: bool,
    real: bool,
    range: Option<Range>,
}

impl Default for ParameterQualifiers {
    fn default() -> Self {
        ParameterQualifiers {
            kind: ParameterKind::Parameter,
            signed: false,
            real: false,
            range: None,
        }
    }
}

/// One item of an ANSI parameter port list: optional qualifiers, then
/// `name = value`.
fn parameter_port_item(
    input: &str,
) -> IResult<&str, (Option<ParameterQualifiers>, Identifier, Expression)> {
    let (input, kind) = opt(ws(parameter_kind))(input)?;
    let (input, typed) = opt(ws(parameter_type))(input)?;
    let (input, signed) = ws(signedness)(input)?;
    let (input, declared) = opt(ws(range))(input)?;
    // Only an item that actually said something re-qualifies the ones that
    // follow it; a bare `b = 2` inherits instead.
    let qualifiers = kind.map(|kind| ParameterQualifiers {
        kind,
        signed: signed || typed.is_some(),
        real: typed == Some(true),
        range: declared,
    });
    let (input, (name, value)) = parameter_assignment(input)?;
    Ok((input, (qualifiers, name, value)))
}

/// `#(parameter WIDTH = 8, DEPTH = 4)` — the ANSI parameter port list, written
/// between a module's name and its ports.
///
/// The qualifiers carry *forward*: `#(parameter signed [7:0] a = 1, b = 2)`
/// declares two signed eight-bit parameters, because `b` inherits what `a`
/// said. That is the same rule an ordinary declaration list follows, and it is
/// why an item that names no keyword is not simply given the defaults.
///
/// The result is an ordinary `Vec<ParameterDeclaration>`, so nothing
/// downstream can tell a parameter declared here from one declared in the
/// body — which is what makes an override, an elaboration-time range and a
/// `defparam` work on it unchanged.
pub fn parse_parameter_port_list(input: &str) -> IResult<&str, Vec<ParameterDeclaration>> {
    let (input, _) = ws(char('#'))(input)?;
    let (input, _) = ws(char('('))(input)?;
    let (input, items) = separated_list1(ws(char(',')), parameter_port_item)(input)?;
    let (input, _) = ws(char(')'))(input)?;

    let mut carried = ParameterQualifiers::default();
    let mut declarations = Vec::with_capacity(items.len());
    for (qualifiers, name, value) in items {
        if let Some(qualifiers) = qualifiers {
            carried = qualifiers;
        }
        declarations.push(ParameterDeclaration {
            kind: carried.kind.clone(),
            name,
            range: carried.range.clone(),
            signed: carried.signed,
            real: carried.real,
            value,
        });
    }
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
                real: false,
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
                real: false,
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

    /// `#(parameter W = 8, D = 4)` — the ANSI parameter port list. Qualifiers
    /// carry forward, so a bare `b = 2` inherits what the item before it said.
    #[test]
    fn test_parameter_port_list() {
        let declared = parse_parameter_port_list("#(parameter a = 1, b = 2)")
            .expect("should parse")
            .1;
        assert_eq!(declared.len(), 2);
        assert_eq!(declared[0].name.name, "a");
        assert_eq!(declared[1].name.name, "b");
        assert_eq!(declared[1].kind, ParameterKind::Parameter);

        let qualified =
            parse_parameter_port_list("#(parameter signed [7:0] a = 1, b = 2, parameter c = 3)")
                .expect("should parse")
                .1;
        assert_eq!(qualified.len(), 3);
        // `b` inherits `a`'s `signed [7:0]`.
        assert!(qualified[1].signed);
        assert_eq!(qualified[1].range, qualified[0].range);
        // `c` re-qualifies, so it drops them again.
        assert!(!qualified[2].signed);
        assert_eq!(qualified[2].range, None);
    }
}

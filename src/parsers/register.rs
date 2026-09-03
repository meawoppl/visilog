use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, char},
    combinator::{opt, recognize},
    sequence::pair,
    IResult,
};

use super::{
    identifier::{identifier, Identifier},
    simple::{range, ws_and_comments},
};

#[derive(Debug, PartialEq)]
pub struct RegisterDeclaration {
    pub name: Identifier,
    pub range: Option<(i64, i64)>,
    pub dimensions: Option<(i64, i64)>,
}

fn parse_identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    ))(input)
}

pub fn parse_register_declaration(input: &str) -> IResult<&str, RegisterDeclaration> {
    let (input, _) = tag("reg")(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, width) = opt(range)(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, name) = identifier(input)?;
    let (input, dimensions) = opt(range)(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, _) = char(';')(input)?;

    Ok((
        input,
        RegisterDeclaration {
            name,
            range: width,
            dimensions,
        },
    ))
}

pub fn parse_memory_declaration(input: &str) -> IResult<&str, RegisterDeclaration> {
    let (input, _) = tag("reg")(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, width) = opt(range)(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, name) = identifier(input)?;
    let (input, dimensions) = opt(range)(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, _) = char(';')(input)?;

    Ok((
        input,
        RegisterDeclaration {
            name,
            range: width,
            dimensions,
        },
    ))
}

#[cfg(test)]
mod tests {
    use crate::parsers::helpers::assert_parses_to;

    use super::*;

    /// A non-numeric range bound must be a parse *error*, never a panic.
    ///
    /// `parse_dimensions` used to duplicate `simple::range` using `take_while`
    /// (zero or more digits) followed by `.unwrap()`, so `reg [a:0] x;` matched
    /// an empty digit run and panicked on the failed `parse::<i64>()`. Real
    /// corpus files hit this the moment port-less modules started parsing.
    #[test]
    fn test_non_numeric_range_bounds_error_rather_than_panic() {
        for source in ["reg [a:0] x;", "reg [:0] x;", "reg [7:] x;", "reg [] x;"] {
            assert!(
                parse_register_declaration(source).is_err(),
                "{:?} should fail to parse, not panic",
                source
            );
        }
    }

    #[test]
    fn test_parse_register_declaration() {
        assert_parses_to(
            parse_register_declaration,
            "reg a;",
            RegisterDeclaration {
                name: "a".into(),
                range: None,
                dimensions: None,
            },
        );

        assert_parses_to(
            parse_register_declaration,
            "reg [7:0] a;",
            RegisterDeclaration {
                name: "a".into(),
                range: Some((7, 0)),
                dimensions: None,
            },
        );

        assert_parses_to(
            parse_register_declaration,
            "reg a[7:0];",
            RegisterDeclaration {
                name: "a".into(),
                range: None,
                dimensions: Some((7, 0)),
            },
        );

        assert_eq!(
            parse_register_declaration("reg [15:0] b;"),
            Ok((
                "",
                RegisterDeclaration {
                    name: "b".into(),
                    range: Some((15, 0)),
                    dimensions: None,
                }
            ))
        );

        assert_eq!(
            parse_register_declaration("reg c[15:0];"),
            Ok((
                "",
                RegisterDeclaration {
                    name: "c".into(),
                    range: None,
                    dimensions: Some((15, 0)),
                }
            ))
        );

        assert_eq!(
            parse_register_declaration("reg [31:0] d[0:255];"),
            Ok((
                "",
                RegisterDeclaration {
                    name: "d".into(),
                    range: Some((31, 0)),
                    dimensions: Some((0, 255)),
                }
            ))
        );
    }

    #[test]
    fn test_parse_memory_declaration() {
        assert_parses_to(
            parse_memory_declaration,
            "reg [7:0] memb[0:255];",
            RegisterDeclaration {
                name: "memb".into(),
                range: Some((7, 0)),
                dimensions: Some((0, 255)),
            },
        );

        assert_eq!(
            parse_memory_declaration("reg [15:0] mem[0:1023];"),
            Ok((
                "",
                RegisterDeclaration {
                    name: "mem".into(),
                    range: Some((15, 0)),
                    dimensions: Some((0, 1023)),
                }
            ))
        );

        assert_eq!(
            parse_memory_declaration("reg [31:0] mem32[0:2047];"),
            Ok((
                "",
                RegisterDeclaration {
                    name: "mem32".into(),
                    range: Some((31, 0)),
                    dimensions: Some((0, 2047)),
                }
            ))
        );

        assert_eq!(
            parse_memory_declaration("reg [63:0] mem64[0:4095];"),
            Ok((
                "",
                RegisterDeclaration {
                    name: "mem64".into(),
                    range: Some((63, 0)),
                    dimensions: Some((0, 4095)),
                }
            ))
        );
    }
}

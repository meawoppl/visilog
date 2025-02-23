use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, char, multispace0},
    combinator::{map, opt, recognize},
    sequence::{delimited, pair, preceded},
    IResult,
};

use super::simple::range;

#[derive(Debug, PartialEq)]
pub struct RegisterDeclaration {
    pub name: String,
    pub range: Option<(i64, i64)>,
    pub dimensions: Option<(i64, i64)>,
}

fn parse_identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    ))(input)
}

fn parse_dimensions(input: &str) -> IResult<&str, (i64, i64)> {
    delimited(
        char('['),
        pair(
            map(take_while(|c: char| c.is_digit(10)), |s: &str| {
                s.parse::<i64>().unwrap()
            }),
            preceded(
                char(':'),
                map(take_while(|c: char| c.is_digit(10)), |s: &str| {
                    s.parse::<i64>().unwrap()
                }),
            ),
        ),
        char(']'),
    )(input)
}

pub fn parse_register_declaration(input: &str) -> IResult<&str, RegisterDeclaration> {
    let (input, _) = tag("reg")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, range) = opt(range)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, name) = parse_identifier(input)?;
    let (input, dimensions) = opt(parse_dimensions)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(';')(input)?;

    Ok((
        input,
        RegisterDeclaration {
            name: name.to_string(),
            range,
            dimensions,
        },
    ))
}

pub fn parse_memory_declaration(input: &str) -> IResult<&str, RegisterDeclaration> {
    let (input, _) = tag("reg")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, range) = opt(range)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, name) = parse_identifier(input)?;
    let (input, dimensions) = opt(parse_dimensions)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(';')(input)?;

    Ok((
        input,
        RegisterDeclaration {
            name: name.to_string(),
            range,
            dimensions,
        },
    ))
}

#[cfg(test)]
mod tests {
    use crate::parsers::helpers::assert_parses_to;

    use super::*;

    #[test]
    fn test_parse_register_declaration() {
        assert_parses_to(
            parse_register_declaration,
            "reg a;",
            RegisterDeclaration {
                name: "a".to_string(),
                range: None,
                dimensions: None,
            },
        );

        assert_parses_to(
            parse_register_declaration,
            "reg [7:0] a;",
            RegisterDeclaration {
                name: "a".to_string(),
                range: Some((7, 0)),
                dimensions: None,
            },
        );

        assert_parses_to(
            parse_register_declaration,
            "reg a[7:0];",
            RegisterDeclaration {
                name: "a".to_string(),
                range: None,
                dimensions: Some((7, 0)),
            },
        );

        assert_eq!(
            parse_register_declaration("reg [15:0] b;"),
            Ok((
                "",
                RegisterDeclaration {
                    name: "b".to_string(),
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
                    name: "c".to_string(),
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
                    name: "d".to_string(),
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
                name: "memb".to_string(),
                range: Some((7, 0)),
                dimensions: Some((0, 255)),
            },
        );

        assert_eq!(
            parse_memory_declaration("reg [15:0] mem[0:1023];"),
            Ok((
                "",
                RegisterDeclaration {
                    name: "mem".to_string(),
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
                    name: "mem32".to_string(),
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
                    name: "mem64".to_string(),
                    range: Some((63, 0)),
                    dimensions: Some((0, 4095)),
                }
            ))
        );
    }
}

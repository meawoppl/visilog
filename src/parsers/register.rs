use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, char, multispace0},
    combinator::{map, opt, recognize},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, tuple},
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
    use super::*;

    #[test]
    fn test_parse_register_declaration() {
        assert_eq!(
            parse_register_declaration("reg a;"),
            Ok((
                "",
                RegisterDeclaration {
                    name: "a".to_string(),
                    range: None,
                    dimensions: None,
                }
            ))
        );

        assert_eq!(
            parse_register_declaration("reg [7:0] a;"),
            Ok((
                "",
                RegisterDeclaration {
                    name: "a".to_string(),
                    range: Some((7, 0)),
                    dimensions: None,
                }
            ))
        );

        assert_eq!(
            parse_register_declaration("reg a[7:0];"),
            Ok((
                "",
                RegisterDeclaration {
                    name: "a".to_string(),
                    range: None,
                    dimensions: Some((7, 0)),
                }
            ))
        );
    }

    #[test]
    fn test_parse_memory_declaration() {
        assert_eq!(
            parse_memory_declaration("reg [7:0] memb[0:255];"),
            Ok((
                "",
                RegisterDeclaration {
                    name: "memb".to_string(),
                    range: Some((7, 0)),
                    dimensions: Some((0, 255)),
                }
            ))
        );
    }
}

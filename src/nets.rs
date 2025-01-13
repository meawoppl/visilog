use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{char, multispace0},
    combinator::{map, opt},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

#[derive(Debug, PartialEq)]
pub enum NetType {
    Wire,
    Reg,
}

#[derive(Debug, PartialEq)]
pub struct Net {
    pub net_type: NetType,
    pub range: Option<(i64, i64)>,
    pub names: Vec<String>,
}

fn parse_identifier(input: &str) -> IResult<&str, &str> {
    let first_char = alt((nom::character::complete::alpha1, tag("_")));
    let rest_chars = take_while(|c: char| c.is_alphanumeric() || c == '_');
    map(pair(first_char, rest_chars), |(first, rest): (&str, &str)| {
        format!("{}{}", first, rest)
    })(input)
}

fn parse_range(input: &str) -> IResult<&str, (i64, i64)> {
    delimited(
        char('['),
        pair(
            map(nom::character::complete::digit1, |s: &str| s.parse::<i64>().unwrap()),
            preceded(
                pair(multispace0, char(':')),
                map(nom::character::complete::digit1, |s: &str| s.parse::<i64>().unwrap()),
            ),
        ),
        char(']'),
    )(input)
}

fn parse_net_type(input: &str) -> IResult<&str, NetType> {
    alt((
        map(tag("wire"), |_| NetType::Wire),
        map(tag("reg"), |_| NetType::Reg),
    ))(input)
}

fn parse_net(input: &str) -> IResult<&str, Net> {
    let (input, net_type) = parse_net_type(input)?;
    let (input, _) = multispace0(input)?;
    let (input, range) = opt(parse_range)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, names) = separated_list0(preceded(multispace0, char(',')), parse_identifier)(input)?;
    let (input, _) = preceded(multispace0, char(';'))(input)?;
    Ok((input, Net { net_type, range, names: names.into_iter().map(String::from).collect() }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_identifier() {
        assert_eq!(parse_identifier("a"), Ok(("", "a")));
        assert_eq!(parse_identifier("a1"), Ok(("", "a1")));
        assert_eq!(parse_identifier("_a1"), Ok(("", "_a1")));
        assert!(parse_identifier("1a").is_err());
    }

    #[test]
    fn test_parse_range() {
        assert_eq!(parse_range("[7:0]"), Ok(("", (7, 0))));
        assert_eq!(parse_range("[15:8]"), Ok(("", (15, 8))));
    }

    #[test]
    fn test_parse_net_type() {
        assert_eq!(parse_net_type("wire"), Ok(("", NetType::Wire)));
        assert_eq!(parse_net_type("reg"), Ok(("", NetType::Reg)));
    }

    #[test]
    fn test_parse_net() {
        assert_eq!(
            parse_net("wire a;"),
            Ok((
                "",
                Net {
                    net_type: NetType::Wire,
                    range: None,
                    names: vec!["a".to_string()],
                }
            ))
        );
        assert_eq!(
            parse_net("wire [7:0] b;"),
            Ok((
                "",
                Net {
                    net_type: NetType::Wire,
                    range: Some((7, 0)),
                    names: vec!["b".to_string()],
                }
            ))
        );
        assert_eq!(
            parse_net("wire a, b, c;"),
            Ok((
                "",
                Net {
                    net_type: NetType::Wire,
                    range: None,
                    names: vec!["a".to_string(), "b".to_string(), "c".to_string()],
                }
            ))
        );
    }
}

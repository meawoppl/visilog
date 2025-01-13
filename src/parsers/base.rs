use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{alpha1, char},
    combinator::{map, recognize},
    sequence::{pair, tuple},
    IResult,
};

fn parse_number(input: &str) -> IResult<&str, &str> {
    recognize(pair(opt(char('-')), take_while1(|c: char| c.is_digit(10))))(input)
}

fn parse_identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        take_while1(|c: char| c.is_alphanumeric() || c == '_'),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_number() {
        assert_eq!(parse_number("123"), Ok(("", "123")));
        assert_eq!(parse_number("-123"), Ok(("", "-123")));
        assert!(parse_number("abc").is_err());
    }

    #[test]
    fn test_parse_identifier() {
        assert_eq!(parse_identifier("abc123"), Ok(("", "abc123")));
        assert_eq!(parse_identifier("_abc123"), Ok(("", "_abc123")));
        assert!(parse_identifier("123abc").is_err());
    }
}

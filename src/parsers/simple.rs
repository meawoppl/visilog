use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until, take_while, take_while1},
    combinator::{map, value},
    sequence::{delimited, preceded, tuple},
    IResult,
};

use nom::character::complete::{char, multispace0};

pub fn whitespace(input: &str) -> IResult<&str, &str> {
    take_while(|c: char| c.is_whitespace())(input)
}

pub fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    delimited(multispace0, inner, multispace0)
}

pub fn raw_pos_int(input: &str) -> IResult<&str, i64> {
    map(take_while1(|c: char| c.is_digit(10)), |raw: &str| {
        raw.parse::<i64>().unwrap()
    })(input)
}

pub fn sign(input: &str) -> IResult<&str, &str> {
    alt((tag("+"), tag("-")))(input)
}

pub fn single_line_comment(input: &str) -> IResult<&str, &str> {
    value(
        "", // We don't care about the content of the comment, so we map it to an empty string
        preceded(tag("//"), is_not("\n")),
    )(input)
}

pub fn multi_line_comment(input: &str) -> IResult<&str, &str> {
    delimited(tag("/*"), take_until("*/"), tag("*/"))(input)
}

pub fn comment(input: &str) -> IResult<&str, &str> {
    alt((single_line_comment, multi_line_comment))(input)
}

pub fn range(input: &str) -> IResult<&str, (i64, i64)> {
    delimited(
        char('['),
        tuple((raw_pos_int, preceded(ws(char(':')), raw_pos_int))),
        char(']'),
    )(input)
}

#[cfg(test)]
mod tests {
    use nom::Parser;

    use super::*;

    #[test]
    fn test_comments() {
        assert_eq!(comment("// This is a single line comment"), Ok(("", "")));
        assert_eq!(
            comment("/* This is a multi-line comment */"),
            Ok(("", " This is a multi-line comment "))
        );
        assert_eq!(
            comment("/* This is a \n multi-line \n comment */"),
            Ok(("", " This is a \n multi-line \n comment "))
        );
    }
}

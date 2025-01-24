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
    use super::*;
    use nom::Parser;

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

    #[test]
    fn test_whitespace() {
        assert_eq!(whitespace("   abc"), Ok(("abc", "   ")));
        assert_eq!(whitespace("\t\nabc"), Ok(("abc", "\t\n")));
        assert_eq!(whitespace("abc"), Ok(("abc", "")));
    }

    #[test]
    fn test_ws() {
        let mut parser = ws(tag("abc"));
        assert_eq!(parser("   abc   "), Ok(("", "abc")));
        assert_eq!(parser("\tabc\t"), Ok(("", "abc")));
        assert_eq!(parser("abc"), Ok(("", "abc")));
        assert_eq!(parser("   abc"), Ok(("", "abc")));
        assert_eq!(parser("abc   "), Ok(("", "abc")));
        assert_eq!(parser("   abc   def"), Ok(("def", "abc")));
    }

    #[test]
    fn test_raw_pos_int() {
        assert_eq!(raw_pos_int("123abc"), Ok(("abc", 123)));
        assert_eq!(raw_pos_int("0abc"), Ok(("abc", 0)));
        assert!(raw_pos_int("abc").is_err());
    }

    #[test]
    fn test_sign() {
        assert_eq!(sign("+123"), Ok(("123", "+")));
        assert_eq!(sign("-123"), Ok(("123", "-")));
        assert!(sign("123").is_err());
    }

    #[test]
    fn test_single_line_comment() {
        assert_eq!(
            single_line_comment("// This is a comment\nabc"),
            Ok(("\nabc", ""))
        );
        assert_eq!(single_line_comment("// This is a comment"), Ok(("", "")));
        assert!(single_line_comment("This is not a comment").is_err());
    }

    #[test]
    fn test_multi_line_comment() {
        assert_eq!(
            multi_line_comment("/* This is a comment */abc"),
            Ok(("abc", " This is a comment "))
        );
        assert_eq!(
            multi_line_comment("/* This is a comment */"),
            Ok(("", " This is a comment "))
        );
        assert!(multi_line_comment("This is not a comment").is_err());
    }

    #[test]
    fn test_range() {
        assert_eq!(range("[1:0]abc"), Ok(("abc", (1, 0))));
        assert_eq!(range("[10:5]abc"), Ok(("abc", (10, 5))));
        assert_eq!(range("[0:0]abc"), Ok(("abc", (0, 0))));
        assert_eq!(range("[123:456]abc"), Ok(("abc", (123, 456))));
        assert!(range("abc").is_err());
    }
}

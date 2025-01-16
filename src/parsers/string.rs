use nom::{
    branch::alt,
    bytes::complete::take_while_m_n,
    character::complete::{char, none_of},
    combinator::{map_res, value},
    multi::many0,
    sequence::{delimited, preceded},
    IResult,
};

fn parse_escape_sequence(input: &str) -> IResult<&str, char> {
    preceded(
        char('\\'),
        alt((
            value('\n', char('n')),
            value('\t', char('t')),
            value('\\', char('\\')),
            value('\"', char('"')),
            map_res(
                preceded(char('d'), take_while_m_n(1, 3, |c: char| c.is_digit(8))),
                |octal| u8::from_str_radix(octal, 8).map(|v| v as char),
            ),
            value('%', char('%')),
        )),
    )(input)
}

fn parse_string_content(input: &str) -> IResult<&str, String> {
    many0(alt((parse_escape_sequence, none_of("\""))))(input)
        .map(|(next_input, res)| (next_input, res.into_iter().collect()))
}

pub fn parse_verilog_string(input: &str) -> IResult<&str, String> {
    delimited(char('"'), parse_string_content, char('"'))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_verilog_string() {
        assert_eq!(parse_verilog_string("\"hello\""), Ok(("", "hello".to_string())));
        assert_eq!(parse_verilog_string("\"hello\\nworld\""), Ok(("", "hello\nworld".to_string())));
        assert_eq!(parse_verilog_string("\"hello\\tworld\""), Ok(("", "hello\tworld".to_string())));
        assert_eq!(parse_verilog_string("\"hello\\\\world\""), Ok(("", "hello\\world".to_string())));
        assert_eq!(parse_verilog_string("\"hello\\\"world\""), Ok(("", "hello\"world".to_string())));
        assert_eq!(parse_verilog_string("\"hello\\d123world\""), Ok(("", "hello{world".to_string())));
        assert_eq!(parse_verilog_string("\"hello%world\""), Ok(("", "hello%world".to_string())));
        assert_eq!(parse_verilog_string("\"\""), Ok(("", "".to_string())));
        assert_eq!(parse_verilog_string("\"\\n\\t\\\\\\\"\\d123%\""), Ok(("", "\n\t\\\"{".to_string())));
    }
}

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
            // `\ddd` is up to three *octal* digits naming a character code —
            // the `d`s in the standard's notation stand for digits, they are
            // not a literal `d`. `"\123"` is `S`, as iverilog agrees.
            map_res(take_while_m_n(1, 3, |c: char| c.is_digit(8)), |octal| {
                u8::from_str_radix(octal, 8).map(|v| v as char)
            }),
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

    use crate::parsers::helpers::assert_parses_to;
    #[test]
    fn test_parse_verilog_string() {
        let from_to = vec![
            ("\"hello\"", "hello".to_string()),
            ("\"hello\\nworld\"", "hello\nworld".to_string()),
            ("\"hello\\tworld\"", "hello\tworld".to_string()),
            ("\"hello\\\\world\"", "hello\\world".to_string()),
            ("\"hello\\\"world\"", "hello\"world".to_string()),
            ("\"hello%world\"", "hello%world".to_string()),
            ("\"\"", "".to_string()),
        ];

        for (input, expected) in from_to {
            assert_parses_to(parse_verilog_string, input, expected);
        }
    }

    /// `\\ddd` is a character code in **octal**, up to three digits — the `d`s
    /// in the standard's notation stand for digits, not a literal `d`.
    ///
    /// This test replaces one that was disabled and also wrong: it asserted
    /// `\\d123` produced `{`, which is a literal `d` followed by *decimal* 123.
    /// `iverilog` prints `S` for `"\\123"` and `A` for `"\\101"`, which is
    /// octal, and that is what these assert.
    #[test]
    fn test_octal_character_escapes() {
        assert_parses_to(parse_verilog_string, "\"\\123\"", "S".to_string());
        assert_parses_to(parse_verilog_string, "\"\\101\"", "A".to_string());
        // One and two digit forms, and a digit that is not part of the escape.
        assert_parses_to(parse_verilog_string, "\"\\0\"", "\0".to_string());
        assert_parses_to(parse_verilog_string, "\"\\1011\"", "A1".to_string());
    }

    /// The escapes that are not character codes. iverilog prints
    /// `a<TAB>b\c"d` for the Verilog literal `"a\tb\\c\"d"`.
    #[test]
    fn test_simple_escapes_match_the_reference() {
        assert_parses_to(
            parse_verilog_string,
            "\"a\\tb\\\\c\\\"d\"",
            "a\tb\\c\"d".to_string(),
        );
    }
}

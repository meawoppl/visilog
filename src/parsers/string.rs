use nom::{
    branch::alt,
    bytes::complete::take_while_m_n,
    character::complete::{char, none_of},
    combinator::{map, map_res, value},
    multi::many0,
    sequence::{delimited, preceded},
    IResult,
};

/// A backslash immediately before a newline is a *line continuation*: it and
/// the newline both disappear, so a literal may be written across two lines
/// and still be one string. IEEE 1364-2005 §3.6 says so, and iverilog 12.0
/// prints `ab` for a literal spelled `"a\<newline>b"` and answers `1` for
/// `"a\<newline>b" == "ab"`.
///
/// It is separate from [`parse_escape_sequence`] because it produces *no*
/// character rather than one, which is the one thing that escape cannot say.
/// Without it the backslash falls through to the `none_of("\"")` arm and is
/// kept as a literal backslash, taking the newline with it — the string then
/// quietly holds two characters nothing in the source asked for.
fn line_continuation(input: &str) -> IResult<&str, ()> {
    let rest = input.strip_prefix('\\').ok_or_else(|| {
        nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Char))
    })?;
    // A `\r\n` line ending counts, and its two characters go together.
    let rest = rest
        .strip_prefix("\r\n")
        .or_else(|| rest.strip_prefix('\n'))
        .ok_or_else(|| {
            nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Char))
        })?;
    Ok((rest, ()))
}

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

/// One piece of a string's body: a continuation, which contributes nothing, an
/// escape, or an ordinary character. The continuation is tried **first**,
/// since every alternative past it would claim its backslash.
fn string_piece(input: &str) -> IResult<&str, Option<char>> {
    alt((
        map(line_continuation, |()| None),
        map(parse_escape_sequence, Some),
        map(none_of("\""), Some),
    ))(input)
}

fn parse_string_content(input: &str) -> IResult<&str, String> {
    many0(string_piece)(input)
        .map(|(next_input, res)| (next_input, res.into_iter().flatten().collect()))
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

    /// A backslash immediately before a newline is a line continuation: both
    /// characters disappear and the literal carries on. iverilog 12.0 prints
    /// `[ab]` for `$display("[%s]", "a\<newline>b")` and `1` for
    /// `"a\<newline>b" == "ab"`.
    ///
    /// Corpus `string12` is that comparison. Without the rule the backslash is
    /// kept as an ordinary character and takes the newline with it, so the
    /// string holds two characters nothing in the source asked for — a silent
    /// wrong answer rather than a parse failure.
    #[test]
    fn test_a_backslash_newline_continues_a_string_literal() {
        assert_parses_to(parse_verilog_string, "\"a\\\nb\"", "ab".to_string());
        assert_parses_to(parse_verilog_string, "\"a\\\r\nb\"", "ab".to_string());
        // The corpus case: the continuation falls inside the words, so the
        // leading space on the next line is a real character.
        assert_parses_to(
            parse_verilog_string,
            "\"this\\\n matches\"",
            "this matches".to_string(),
        );
        // A backslash before anything else is still the escape it always was,
        // and `\n` is still a newline rather than a continuation.
        assert_parses_to(parse_verilog_string, "\"a\\nb\"", "a\nb".to_string());
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

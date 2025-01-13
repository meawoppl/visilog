use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until, take_while},
    character::complete::{alpha1, char},
    combinator::{map, recognize, value},
    multi::separated_list1,
    sequence::{delimited, preceded},
    IResult,
};
use nom::{
    bytes::complete::take_while1, character::complete::one_of, combinator::opt, sequence::tuple,
};

use nom::character::complete::multispace0;

use crate::keywords::{keyword_from_string, VerilogKeyword};

#[derive(Debug, PartialEq)]
enum VerilogBaseType {
    Binary,
    Decimal,
    Octal,
    Hexadecimal,
}

fn raw_pos_int(input: &str) -> IResult<&str, i64> {
    map(take_while1(|c: char| c.is_digit(10)), |raw: &str| {
        raw.parse::<i64>().unwrap()
    })(input)
}

fn sign(input: &str) -> IResult<&str, &str> {
    alt((tag("+"), tag("-")))(input)
}

fn single_line_comment(input: &str) -> IResult<&str, &str> {
    value(
        "", // We don't care about the content of the comment, so we map it to an empty string
        preceded(tag("//"), is_not("\n")),
    )(input)
}

fn multi_line_comment(input: &str) -> IResult<&str, &str> {
    delimited(tag("/*"), take_until("*/"), tag("*/"))(input)
}

fn comment(input: &str) -> IResult<&str, &str> {
    alt((single_line_comment, multi_line_comment))(input)
}

// ... (previous code for comments)

fn binary(input: &str) -> IResult<&str, &str> {
    preceded(
        opt(char('_')),
        take_while1(|c: char| c == '0' || c == '1' || c == '_'),
    )(input)
}

fn decimal(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c.is_digit(10))(input)
}

fn hexadecimal(input: &str) -> IResult<&str, &str> {
    preceded(
        opt(char('_')),
        take_while1(|c: char| c.is_digit(16) || c == '_'),
    )(input)
}

fn const_type_char(input: &str) -> IResult<&str, VerilogBaseType> {
    map(one_of("bdohxBDOHX"), |c| {
        match c.to_ascii_lowercase() {
            'b' => VerilogBaseType::Binary,
            'd' => VerilogBaseType::Decimal,
            'o' => VerilogBaseType::Octal,
            'h' => VerilogBaseType::Hexadecimal,
            'x' => VerilogBaseType::Hexadecimal,
            _ => unreachable!(), // Should never happen due to one_of
        }
    })(input)
}

fn sized_const(input: &str) -> IResult<&str, (&str, VerilogBaseType, &str)> {
    tuple((
        decimal,
        preceded(char('\''), const_type_char),
        alt((hexadecimal, decimal)),
    ))(input)
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        alt((alpha1, tag("_"))), // Start with alpha, '_', or '$'
        take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '$'), // Alphanumeric, '_', or '$'
    )))(input)
}

fn identifier_list(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list1(ws(char(',')), ws(identifier))(input)
}

// ... (previous code including keyword_from_str)

fn keyword(input: &str) -> IResult<&str, VerilogKeyword> {
    map(
        recognize(tuple((
            alpha1,
            take_while(|c: char| c.is_alphanumeric() || c == '_'),
        ))),
        |kw| keyword_from_string(kw).unwrap(), // Map the matched word to the enum
    )(input)
}

fn range(input: &str) -> IResult<&str, (i64, i64)> {
    delimited(
        char('['),
        tuple((raw_pos_int, preceded(ws(char(':')), raw_pos_int))),
        char(']'),
    )(input)
}

fn whitespace(input: &str) -> IResult<&str, &str> {
    take_while(|c: char| c.is_whitespace())(input)
}

fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    delimited(multispace0, inner, multispace0)
}

// NOTE(meawoppl) - what fraction of these are used?
#[derive(Debug, PartialEq, Clone)]
pub enum NetType {
    Supply0,
    Supply1,
    Tri,
    Tri0,
    Tri1,
    TriAnd,
    TriOr,
    Wire,
    WireAnd,
    WireOr,
}

fn net_type(input: &str) -> IResult<&str, NetType> {
    alt((
        value(NetType::Wire, tag("wire")),
        value(NetType::WireAnd, tag("wand")),
        value(NetType::WireOr, tag("wor")),
        value(NetType::Tri, tag("tri")),
        value(NetType::TriAnd, tag("triand")),
        value(NetType::TriOr, tag("trior")),
        value(NetType::Supply0, tag("supply0")),
        value(NetType::Supply1, tag("supply1")),
    ))(input)
}

fn net_declaration(input: &str) -> IResult<&str, (NetType, Option<(i64, i64)>, Vec<&str>)> {
    tuple((net_type, ws(opt(range)), ws(identifier_list)))(input)
}

#[cfg(test)]
mod tests {
    use nom::Parser;

    use super::*;
    use crate::keywords::ALL_KEYWORDS;

    #[test]
    fn test_identifiers_valid_first_characters() {
        let valid_identifiers = [
            "var_a", "_var_a", "Var_A", "_Var_A",
            "var_a1", "Var_A1", "_var_a1", "_Var_A1"
        ];
        for id_str in &valid_identifiers {
            assert!(
                identifier(id_str).is_ok(),
                "Valid identifier {} failed to parse",
                id_str
            );
        }
    }

    #[test]
    fn test_identifiers_invalid_first_characters() {
        let invalid_identifiers = ["1var_a", "$var_a", "1Var_A", "$Var_A"];
        for id_str in &invalid_identifiers {
            assert!(
                identifier(id_str).is_err(),
                "Invalid identifier {} should not parse",
                id_str
            );
        }
    }

    #[test]
    fn test_identifiers_mixed_valid_invalid_first_characters() {
        let mixed_identifiers = [
            "var_a$", "var_a1$", "Var_A$", "Var_A1$",
            "_var_a$", "_var_a1$", "_Var_A$", "_Var_A1$"
        ];
        for id_str in &mixed_identifiers {
            assert!(
                identifier(id_str).is_ok(),
                "Mixed identifier {} failed to parse",
                id_str
            );
        }
    }

    #[test]
    fn test_identifiers_length() {
        let valid_identifier = "a".repeat(1024);
        assert!(
            identifier(&valid_identifier).is_ok(),
            "Valid identifier of length 1024 failed to parse"
        );

        let invalid_identifier = "a".repeat(1025);
        assert!(
            identifier(&invalid_identifier).is_err(),
            "Invalid identifier of length 1025 should not parse"
        );
    }

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
    fn test_sized_bits() {
        assert_eq!(
            sized_const("3'b010"),
            Ok(("", ("3", VerilogBaseType::Binary, "010")))
        );
        assert_eq!(
            sized_const("3'd2"),
            Ok(("", ("3", VerilogBaseType::Decimal, "2")))
        );
        assert_eq!(
            sized_const("8'h70"),
            Ok(("", ("8", VerilogBaseType::Hexadecimal, "70")))
        );
        assert_eq!(
            sized_const("9'h1FA"),
            Ok(("", ("9", VerilogBaseType::Hexadecimal, "1FA")))
        );
        assert_eq!(
            sized_const("32'hFACE_47B2"),
            Ok(("", ("32", VerilogBaseType::Hexadecimal, "FACE_47B2")))
        );
        assert_eq!(
            sized_const("8'D234"),
            Ok(("", ("8", VerilogBaseType::Decimal, "234")))
        );
    }

    #[test]
    fn test_all_keywords() {
        for kw in ALL_KEYWORDS {
            assert!(
                keyword_from_string(kw).is_some(),
                "Keyword {} failed to parse",
                kw
            );
            keyword.parse(kw).unwrap();
        }
    }

    #[test]
    #[ignore]
    fn test_identifier_list() {
        identifier_list.parse("a").unwrap();
        identifier_list.parse("a, b, c").unwrap();
    }

    #[test]
    #[ignore]
    fn test_net_declaration() {
        net_declaration.parse("wire z").unwrap();
    }
}

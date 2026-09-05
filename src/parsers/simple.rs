use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_until, take_while, take_while1},
    combinator::{map, not, value},
    multi::many0,
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

use nom::character::complete::{char, multispace1};

pub fn whitespace(input: &str) -> IResult<&str, &str> {
    take_while(|c: char| c.is_whitespace())(input)
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
        preceded(tag("//"), take_till(|c| c == '\n')),
    )(input)
}

pub fn multi_line_comment(input: &str) -> IResult<&str, &str> {
    delimited(tag("/*"), take_until("*/"), tag("*/"))(input)
}

pub fn comment(input: &str) -> IResult<&str, &str> {
    alt((single_line_comment, multi_line_comment))(input)
}

/// A Verilog-2001 attribute instance, `(* full_case, parallel_case *)`.
///
/// An attribute is metadata addressed to a synthesis tool and carries no
/// simulation semantics, so this **discards its body** rather than putting it
/// on the AST — the deliberate trade that lets attributes be skipped exactly
/// where comments are and so appear in every position the LRM allows them
/// (before a module, a statement, a declaration, a port connection, and
/// between the operands of an expression) for the cost of one `alt` arm. A
/// tool that wanted to *read* attributes would have to keep them instead.
///
/// The terminator is the two-character `*)`, not a bare `*`, so a body may
/// contain one: `(* a = 3 * 4 *)`.
///
/// `(*)` is deliberately **not** an attribute — it is the implicit sensitivity
/// list of `always @(*)`, and an attribute instance must carry at least one
/// specification. Without that guard the `(*` of one `@(*)` would pair with
/// the `*)` of the next one and swallow everything between them.
pub fn attribute(input: &str) -> IResult<&str, &str> {
    delimited(
        pair(tag("(*"), not(preceded(whitespace, char(')')))),
        take_until("*)"),
        tag("*)"),
    )(input)
}

/// Consume any run of whitespace, comments and attributes, including an empty
/// one.
pub fn ws_and_comments(input: &str) -> IResult<&str, ()> {
    value((), many0(alt((multispace1, comment, attribute))))(input)
}

/// Wrap a parser so that whitespace, *comments and attributes* on either side
/// of it are skipped. Nearly every parser in the grammar is wrapped in this,
/// which is what makes a comment or an attribute legal anywhere a token
/// boundary is.
pub fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    delimited(ws_and_comments, inner, ws_and_comments)
}

/// A character that may continue an identifier, so a keyword followed by one is
/// not a keyword at all: `signed` in `signed_a`.
fn identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '$'
}

/// The optional `signed` / `unsigned` qualifier a `reg`, net or port
/// declaration may carry, as `true` when the declaration is signed.
///
/// It is *optional* rather than a separate production because every one of
/// those declarations has the same shape with and without it, and absent means
/// unsigned — what every Verilog data type is unless it says otherwise. An
/// `integer` is the exception, and it says so by being an `integer`.
pub fn signedness(input: &str) -> IResult<&str, bool> {
    map(
        nom::combinator::opt(nom::sequence::terminated(
            alt((value(true, tag("signed")), value(false, tag("unsigned")))),
            not(nom::character::complete::satisfy(identifier_char)),
        )),
        |qualifier| qualifier.unwrap_or(false),
    )(input)
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
    use crate::parsers::behavior::{parse_sensitivity_list, procedural_statement, EventControl};
    use crate::parsers::helpers::{assert_parses, assert_parses_to};
    use crate::parsers::modules::parse_module_instantiation_statement;
    use crate::parsers::source::parse_verilog_source;

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
        assert_eq!(comment("// Another single line comment\n"), Ok(("\n", "")));
        assert_eq!(
            comment("/* Another multi-line comment */"),
            Ok(("", " Another multi-line comment "))
        );
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(whitespace("   abc"), Ok(("abc", "   ")));
        assert_eq!(whitespace("\t\nabc"), Ok(("abc", "\t\n")));
        assert_eq!(whitespace("abc"), Ok(("abc", "")));
        assert_eq!(whitespace(" \t\nabc"), Ok(("abc", " \t\n")));
        assert_eq!(whitespace(" \t\n"), Ok(("", " \t\n")));
    }

    #[test]
    fn test_ws() {
        let mut parser = ws(tag("abc"));
        assert_eq!(parser("   abc   "), Ok(("", "abc")));
        assert_eq!(parser("\tabc\t"), Ok(("", "abc")));
        assert_eq!(parser("abc"), Ok(("", "abc")));
        assert_eq!(parser(" \tabc \t"), Ok(("", "abc")));
        assert_eq!(parser(" \tabc"), Ok(("", "abc")));
        assert_eq!(parser("   abc"), Ok(("", "abc")));
        assert_eq!(parser("abc   "), Ok(("", "abc")));
        assert_eq!(parser("   abc   def"), Ok(("def", "abc")));
    }

    #[test]
    fn test_ws_skips_comments() {
        let mut parser = ws(tag("abc"));
        assert_eq!(parser("// a comment\nabc"), Ok(("", "abc")));
        assert_eq!(parser("/* a comment */abc"), Ok(("", "abc")));
        assert_eq!(parser("abc // trailing"), Ok(("", "abc")));
        assert_eq!(parser("abc /* trailing */"), Ok(("", "abc")));
        assert_eq!(
            parser(" /* one */ // two\n abc /* three */ def"),
            Ok(("def", "abc"))
        );
        assert_eq!(
            parser("/*\n * spanning\n * lines\n */ abc"),
            Ok(("", "abc"))
        );
    }

    #[test]
    fn test_raw_pos_int() {
        assert_eq!(raw_pos_int("123abc"), Ok(("abc", 123)));
        assert_eq!(raw_pos_int("0abc"), Ok(("abc", 0)));
        assert!(raw_pos_int("abc").is_err());
        assert_eq!(raw_pos_int("456def"), Ok(("def", 456)));
        assert_eq!(raw_pos_int("789ghi"), Ok(("ghi", 789)));
    }

    #[test]
    fn test_sign() {
        assert_eq!(sign("+123"), Ok(("123", "+")));
        assert_eq!(sign("-123"), Ok(("123", "-")));
        assert!(sign("123").is_err());
        assert_eq!(sign("+456"), Ok(("456", "+")));
        assert_eq!(sign("-789"), Ok(("789", "-")));
    }

    #[test]
    fn test_single_line_comment() {
        assert_eq!(
            single_line_comment("// This is a comment\nabc"),
            Ok(("\nabc", ""))
        );
        assert_eq!(single_line_comment("// This is a comment"), Ok(("", "")));
        assert!(single_line_comment("This is not a comment").is_err());
        assert_eq!(single_line_comment("// Another comment\n"), Ok(("\n", "")));
        assert_eq!(single_line_comment("// Another comment"), Ok(("", "")));
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
        assert_eq!(
            multi_line_comment("/* Another comment */def"),
            Ok(("def", " Another comment "))
        );
        assert_eq!(
            multi_line_comment("/* Another comment */"),
            Ok(("", " Another comment "))
        );
    }

    #[test]
    fn test_ws_and_comments() {
        assert_eq!(ws_and_comments(""), Ok(("", ())));
        assert_eq!(ws_and_comments("abc"), Ok(("abc", ())));
        assert_eq!(ws_and_comments("   \n\t abc"), Ok(("abc", ())));
        assert_eq!(ws_and_comments("// a comment\nabc"), Ok(("abc", ())));
        assert_eq!(ws_and_comments("//\n//\nabc"), Ok(("abc", ())));
        assert_eq!(ws_and_comments("/* a comment */ abc"), Ok(("abc", ())));
        assert_eq!(
            ws_and_comments("\n // one \n /* two */\n\n// three\nabc"),
            Ok(("abc", ()))
        );
        assert_eq!(ws_and_comments("  // trailing"), Ok(("", ())));
    }

    #[test]
    fn test_attribute() {
        assert_eq!(attribute("(* keep *)rest"), Ok(("rest", " keep ")));
        assert_eq!(attribute("(*keep*)"), Ok(("", "keep")));
        // The terminator is `*)`, so a `*` inside the body is just a `*`.
        assert_eq!(
            attribute(r#"(* a = "x", b = 3 * 4 *)tail"#),
            Ok(("tail", r#" a = "x", b = 3 * 4 "#))
        );
        assert_eq!(
            attribute("(*\n full_case,\n parallel_case\n*)"),
            Ok(("", "\n full_case,\n parallel_case\n"))
        );
        assert!(attribute("(a)").is_err());
        assert!(attribute("keep").is_err());
    }

    /// `(*)` is `always @(*)`, not an empty attribute — and neither is `(* )`.
    #[test]
    fn test_attribute_rejects_the_implicit_sensitivity_list() {
        assert!(attribute("(*)").is_err());
        assert!(attribute("(* )").is_err());
        assert!(attribute("(*\n)").is_err());
    }

    /// An attribute that is never closed is a parse error. It must not consume
    /// to end of input, which would silently delete the rest of the file.
    #[test]
    fn test_unterminated_attribute_is_an_error() {
        assert!(attribute("(* keep").is_err());
        assert!(attribute("(* keep * ) more").is_err());
        // `ws` cannot swallow it either: the run stops dead at the `(*`.
        let mut parser = ws(tag("abc"));
        assert!(parser("(* keep abc").is_err());
        assert_eq!(ws_and_comments("(* keep"), Ok(("(* keep", ())));
    }

    #[test]
    fn test_ws_skips_attributes() {
        let mut parser = ws(tag("abc"));
        assert_eq!(parser("(* keep *)abc"), Ok(("", "abc")));
        assert_eq!(parser("abc(* keep *)"), Ok(("", "abc")));
        assert_eq!(parser(" (* a *) /* c */ (* b *) abc"), Ok(("", "abc")));
        assert_eq!(parser("(* a = 3 * 4 *) abc def"), Ok(("def", "abc")));
    }

    #[test]
    fn test_ws_and_comments_skips_attributes() {
        assert_eq!(ws_and_comments("(* keep *)abc"), Ok(("abc", ())));
        assert_eq!(ws_and_comments("(* a *)(* b *)abc"), Ok(("abc", ())));
        assert_eq!(ws_and_comments("  (* keep *)  // c\n abc"), Ok(("abc", ())));
    }

    /// The five positions the LRM allows an attribute in. None of the parsers
    /// below know attributes exist — they inherit them from `ws`.
    #[test]
    fn test_attribute_before_a_module() {
        let modules = assert_parses(
            parse_verilog_source,
            "(* keep *) module m(); endmodule\n(* keep *) module n(); endmodule",
        );
        assert_eq!(modules.len(), 2);
    }

    #[test]
    fn test_attribute_before_a_port_connection() {
        let instantiation = assert_parses(
            parse_module_instantiation_statement,
            "foo f ((* c *) .a(b));",
        );
        assert_eq!(instantiation.module_name.name, "foo");
        assert_eq!(instantiation.instance_name.name, "f");
    }

    #[test]
    fn test_attribute_before_a_statement() {
        assert_parses(
            procedural_statement,
            "(* full_case *) case (x) 1: a = 1; endcase",
        );
        assert_parses(procedural_statement, "(* keep *) a = b;");
        assert_parses(procedural_statement, "(* keep *) if (a) b = 1;");
    }

    /// A declaration parser has no leading `ws` of its own — the module body is
    /// what wraps each statement in one — so this goes through a whole module.
    #[test]
    fn test_attribute_before_a_declaration() {
        let modules = assert_parses(
            parse_verilog_source,
            "module m();\n\
             (* keep *) wire a;\n\
             (* keep *) reg [3:0] q;\n\
             (* keep *) assign a = q;\n\
             endmodule",
        );
        assert_eq!(modules[0].statements.len(), 3);
    }

    #[test]
    fn test_attribute_with_a_string_a_comma_and_a_product() {
        assert_parses(
            parse_verilog_source,
            r#"module m(); (* a = "x", b = 3 * 4 *) wire w; endmodule"#,
        );
    }

    /// The regression risk of treating `(*` as a token: `@(*)` is the implicit
    /// sensitivity list, and two of them in one file must not pair the `(*` of
    /// the first with the `*)` of the second.
    #[test]
    fn test_implicit_sensitivity_lists_still_parse() {
        assert_parses_to(parse_sensitivity_list, "@(*)", EventControl::Implicit);
        let modules = assert_parses(
            parse_verilog_source,
            "module m(); always @(*) a = b; always @(*) c = d; endmodule",
        );
        assert_eq!(modules[0].statements.len(), 2);

        let attributed = assert_parses(
            parse_verilog_source,
            "module m(); (* keep *) always @(*) a = b; always @(*) c = d; endmodule",
        );
        assert_eq!(attributed[0].statements.len(), 2);
    }

    #[test]
    fn test_range() {
        assert_eq!(range("[1:0]abc"), Ok(("abc", (1, 0))));
        assert_eq!(range("[10:5]abc"), Ok(("abc", (10, 5))));
        assert_eq!(range("[0:0]abc"), Ok(("abc", (0, 0))));
        assert_eq!(range("[123:456]abc"), Ok(("abc", (123, 456))));
        assert!(range("abc").is_err());
        assert_eq!(range("[3:2]def"), Ok(("def", (3, 2))));
        assert_eq!(range("[8:4]ghi"), Ok(("ghi", (8, 4))));
    }
}

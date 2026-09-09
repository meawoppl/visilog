use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1, take_while_m_n},
    character::complete::{alpha1, char, multispace1},
    combinator::{map, map_res, opt},
    multi::{many0, separated_list1},
    sequence::tuple,
    sequence::{delimited, pair},
    IResult,
};

use super::{base::RawToken, simple::raw_pos_int, simple::ws};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Identifier { name }
    }
}

impl RawToken for Identifier {
    fn raw_token(&self) -> String {
        self.name.clone()
    }
}

impl From<&str> for Identifier {
    fn from(name: &str) -> Self {
        Identifier::new(name.to_string())
    }
}

impl From<String> for Identifier {
    fn from(name: String) -> Self {
        Identifier::new(name)
    }
}

/// An escaped identifier: `\\` then any run of printable, non-whitespace
/// characters, terminated by whitespace.
///
/// IEEE 1364 §3.7.1: "the backslash and the terminating white space are not
/// considered part of the identifier". So `\\a ` and `a` name the *same*
/// object, which `iverilog` confirms — assigning through one and reading
/// through the other sees the same value. Stripping both here is what makes
/// that fall out, with no special case anywhere downstream.
fn escaped_identifier(input: &str) -> IResult<&str, Identifier> {
    let (input, _) = char('\\')(input)?;
    let (input, name) = take_while1(|c: char| !c.is_whitespace())(input)?;
    // The terminating whitespace belongs to the token, not to what follows.
    let (input, _) = multispace1(input)?;
    Ok((input, Identifier::new(name.to_string())))
}

pub fn identifier(input: &str) -> IResult<&str, Identifier> {
    alt((escaped_identifier, simple_identifier))(input)
}

fn simple_identifier(input: &str) -> IResult<&str, Identifier> {
    map_res(
        tuple((
            alt((alpha1, tag("_"))),
            take_while_m_n(0, 1024, |c: char| {
                c.is_alphanumeric() || c == '_' || c == '$'
            }),
        )),
        |(leading, rest): (&str, &str)| {
            let full_id = format!("{}{}", leading, rest);
            if full_id.len() > 1024 {
                Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::TooLarge,
                )))
            } else {
                Ok(Identifier::new(full_id))
            }
        },
    )(input)
}

/// The index of one generate block a hierarchical name descends through: the
/// `[0]` of `stage[0].u`. It is a literal, because a name is written where no
/// signal has a value yet.
fn scope_index(input: &str) -> IResult<&str, i64> {
    map(pair(opt(char('-')), raw_pos_int), |(sign, value)| {
        if sign.is_some() {
            -value
        } else {
            value
        }
    })(input)
}

/// One `.name` step of a hierarchical name, with the generate index that may
/// stand in front of the dot.
///
/// The index is only part of the *path* when a `.` follows it: `a[3]` is a bit
/// select and `a[3].b` is a name inside the fourth iteration of the generate
/// block `a`. Nothing here skips whitespace, which is what keeps the two
/// apart cheaply — a hierarchical name is written tight, and a bare
/// identifier pays one character comparison to find out it is not one.
fn hierarchical_step(input: &str) -> IResult<&str, (Option<i64>, Identifier)> {
    let (input, index) = opt(delimited(char('['), ws(scope_index), char(']')))(input)?;
    let (input, _) = char('.')(input)?;
    let (input, name) = identifier(input)?;
    Ok((input, (index, name)))
}

/// `dut.count`, `stage[0].u.count` — a name that reaches into an instance or a
/// generate block.
///
/// Hierarchy is flattened into dotted store names at elaboration, so the whole
/// path is folded into a single [`Identifier`] here and resolves like any
/// other name. A name with no dot in it comes back exactly as [`identifier`]
/// read it.
pub fn hierarchical_identifier(input: &str) -> IResult<&str, Identifier> {
    let (rest, first) = identifier(input)?;
    // Only a `.` continues a path, and a bracket only does when a `.` follows
    // the `]` — `a[3]` is a bit select and just `a[3].b` is a name inside a
    // generate block. Asking that here rather than by parsing the index and
    // backtracking is what keeps this off the expression grammar's hot path,
    // where every operand tries this parser several times over.
    let continues = match rest.as_bytes().first() {
        Some(b'.') => true,
        Some(b'[') => rest
            .find(']')
            .is_some_and(|at| rest[at + 1..].starts_with('.')),
        _ => false,
    };
    if !continues {
        return Ok((rest, first));
    }
    let (rest, steps) = many0(hierarchical_step)(rest)?;
    if steps.is_empty() {
        return Ok((rest, first));
    }
    let mut name = first.name;
    for (index, step) in steps {
        if let Some(index) = index {
            name.push('[');
            name.push_str(&index.to_string());
            name.push(']');
        }
        name.push('.');
        name.push_str(&step.name);
    }
    Ok((rest, Identifier::new(name)))
}

pub fn identifier_list(input: &str) -> IResult<&str, Vec<Identifier>> {
    separated_list1(ws(char(',')), ws(identifier))(input)
}

#[cfg(test)]
mod tests {
    use crate::parsers::helpers::assert_parses_to;

    use super::*;
    use nom::Parser;

    /// IEEE 1364 §3.7.1 — the backslash and the terminating whitespace are not
    /// part of the name, so an escaped identifier and the simple identifier it
    /// spells are the *same* object. `iverilog` agrees: assigning through `a`
    /// and reading through `\\a ` sees the same value.
    #[test]
    fn test_escaped_identifiers_drop_the_backslash_and_terminator() {
        assert_parses_to(identifier, "\\a ", "a".into());
        assert_parses_to(identifier, "\\odd*name$ ", "odd*name$".into());
        assert_parses_to(identifier, "\\in[0] ", "in[0]".into());
        // Characters a simple identifier could never carry.
        assert_parses_to(identifier, "\\1st.wire! ", "1st.wire!".into());
    }

    /// The terminator is required — without it there is no way to know where
    /// the name stops, since an escaped identifier may contain almost anything.
    #[test]
    fn test_an_escaped_identifier_needs_its_terminating_whitespace() {
        assert!(identifier("\\a").is_err());
    }

    #[test]
    fn test_identifiers_valid_first_characters() {
        let valid_identifiers = [
            "var_a", "_var_a", "Var_A", "_Var_A", "var_a1", "Var_A1", "_var_a1", "_Var_A1",
            "var_b", "_var_b", "Var_B", "_Var_B", "var_b1", "Var_B1", "_var_b1", "_Var_B1",
        ];
        for id_str in &valid_identifiers {
            assert_parses_to(identifier, id_str, Identifier::new(id_str.to_string()));
        }
    }

    #[test]
    fn test_parse_identifier_valid_first_characters() {
        let valid_identifiers = [
            "my_module",
            "_my_module",
            "My_Module",
            "_My_Module",
            "my_module1",
            "My_Module1",
            "_my_module1",
            "_My_Module1",
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
        let invalid_identifiers = [
            "1var_a",
            "$var_a",
            "1Var_A",
            "$Var_A",
            "1my_module",
            "$my_module",
            "1My_Module",
            "$My_Module",
        ];
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
            "var_a$", "var_a1$", "Var_A$", "Var_A1$", "_var_a$", "_var_a1$", "_Var_A$", "_Var_A1$",
            "var_b$", "var_b1$", "Var_B$", "Var_B1$", "_var_b$", "_var_b1$", "_Var_B$", "_Var_B1$",
        ];
        for id_str in &mixed_identifiers {
            let result = identifier(id_str);

            assert!(
                result.is_ok(),
                "Mixed identifier {} failed to parse",
                id_str
            );

            let unwrapped = result.unwrap();

            assert!(
                unwrapped.0.is_empty(),
                "Mixed identifier {} failed to fully parse",
                id_str
            );

            assert_eq!(unwrapped.1.name, id_str.to_string());
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
    fn test_identifier_list_single() {
        assert_parses_to(identifier_list, "a", vec![Identifier::new("a".to_string())]);
    }

    #[test]
    fn test_identifier_list_double() {
        assert_parses_to(
            identifier_list,
            "a,b",
            vec![
                Identifier::new("a".to_string()),
                Identifier::new("b".to_string()),
            ],
        );
    }

    #[test]
    fn test_identifier_list_multiple() {
        assert_parses_to(
            identifier_list,
            "a, b, c",
            vec![
                Identifier::new("a".to_string()),
                Identifier::new("b".to_string()),
                Identifier::new("c".to_string()),
            ],
        );

        let result = identifier_list.parse("b, c, d").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(
            result.1,
            vec![
                Identifier::new("b".to_string()),
                Identifier::new("c".to_string()),
                Identifier::new("d".to_string())
            ]
        );
    }

    #[test]
    fn test_identifier_list_with_whitespace() {
        assert_parses_to(
            identifier_list,
            " a , b , c ",
            vec![
                Identifier::new("a".to_string()),
                Identifier::new("b".to_string()),
                Identifier::new("c".to_string()),
            ],
        );

        let result = identifier_list.parse(" b , c , d ").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(
            result.1,
            vec![
                Identifier::new("b".to_string()),
                Identifier::new("c".to_string()),
                Identifier::new("d".to_string())
            ]
        );
    }

    #[test]
    fn test_identifier_list_with_invalid_identifier() {
        let result = identifier_list.parse("a, 1b, c");
        // This should only parse the first identifier here...
        assert!(result.is_ok());
        let (rest, identifiers) = result.unwrap();
        assert_eq!(rest, ", 1b, c");
        assert_eq!(identifiers, vec![Identifier::new("a".to_string())]);

        let result = identifier_list.parse("b, 1c, d");
        // This should only parse the first identifier here...
        assert!(result.is_ok());
        let (rest, identifiers) = result.unwrap();
        assert_eq!(rest, ", 1c, d");
        assert_eq!(identifiers, vec![Identifier::new("b".to_string())]);
    }

    #[test]
    fn test_identifier_list_empty() {
        let result = identifier_list.parse("");
        assert!(result.is_err(), "Empty identifier list should not parse");

        let result = identifier_list.parse(" ");
        assert!(result.is_err(), "Empty identifier list should not parse");
    }

    #[test]
    fn test_identifier_with_special_characters() {
        let special_identifiers = [
            "var_a$", "var_a1$", "Var_A$", "Var_A1$", "_var_a$", "_var_a1$", "_Var_A$", "_Var_A1$",
        ];
        for id_str in &special_identifiers {
            let parsed = identifier(id_str);
            assert!(
                parsed.is_ok(),
                "Special identifier {} failed to parse",
                id_str
            );

            assert_eq!(parsed.unwrap().1.name, id_str.to_string());
        }
    }

    #[test]
    fn test_identifier_with_max_length() {
        let max_length_identifier = "a".repeat(1024);
        assert!(
            identifier(&max_length_identifier).is_ok(),
            "Identifier with max length failed to parse"
        );
    }

    #[test]
    fn test_identifier_exceeding_max_length() {
        let exceeding_length_identifier = "a".repeat(1025);
        assert!(
            identifier(&exceeding_length_identifier).is_err(),
            "Identifier exceeding max length should not parse"
        );
    }
}

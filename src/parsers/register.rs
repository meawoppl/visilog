use nom::{
    bytes::complete::tag,
    character::complete::char,
    combinator::opt,
    multi::separated_list1,
    sequence::{pair, preceded},
    IResult,
};

use super::{
    identifier::{identifier, Identifier},
    simple::{range, ws, ws_and_comments},
};

#[derive(Debug, PartialEq)]
pub struct RegisterDeclaration {
    pub name: Identifier,
    pub range: Option<(i64, i64)>,
    pub dimensions: Option<(i64, i64)>,
}

/// One declared name plus the optional address dimension that makes it a
/// memory: `mem [0:255]`.
///
/// The dimension belongs to the *name*, not to the declaration as a whole,
/// which is what makes `reg [7:0] a, mem [0:15];` legal — one width, but only
/// the second name is a memory.
pub fn declared_name(input: &str) -> IResult<&str, (Identifier, Option<(i64, i64)>)> {
    pair(identifier, opt(preceded(ws_and_comments, range)))(input)
}

/// `reg [width]? name [dims]? (, name [dims]?)* ;`
///
/// The width applies to every name in the list. A memory is not a separate
/// production — it is one of these names with a dimension attached — so there
/// is no "memory before register" ordering hazard to get wrong.
pub fn parse_register_declaration(input: &str) -> IResult<&str, Vec<RegisterDeclaration>> {
    let (input, _) = tag("reg")(input)?;
    let (input, width) = ws(opt(range))(input)?;
    let (input, names) = separated_list1(ws(char(',')), ws(declared_name))(input)?;
    let (input, _) = ws(char(';'))(input)?;

    Ok((
        input,
        names
            .into_iter()
            .map(|(name, dimensions)| RegisterDeclaration {
                name,
                range: width,
                dimensions,
            })
            .collect(),
    ))
}

#[cfg(test)]
mod tests {
    use crate::parsers::helpers::assert_parses_to;

    use super::*;

    /// A non-numeric range bound must be a parse *error*, never a panic.
    ///
    /// `parse_dimensions` used to duplicate `simple::range` using `take_while`
    /// (zero or more digits) followed by `.unwrap()`, so `reg [a:0] x;` matched
    /// an empty digit run and panicked on the failed `parse::<i64>()`. Real
    /// corpus files hit this the moment port-less modules started parsing.
    #[test]
    fn test_non_numeric_range_bounds_error_rather_than_panic() {
        for source in ["reg [a:0] x;", "reg [:0] x;", "reg [7:] x;", "reg [] x;"] {
            assert!(
                parse_register_declaration(source).is_err(),
                "{:?} should fail to parse, not panic",
                source
            );
        }
    }

    #[test]
    fn test_parse_register_declaration() {
        assert_parses_to(
            parse_register_declaration,
            "reg a;",
            vec![RegisterDeclaration {
                name: "a".into(),
                range: None,
                dimensions: None,
            }],
        );

        assert_parses_to(
            parse_register_declaration,
            "reg [7:0] a;",
            vec![RegisterDeclaration {
                name: "a".into(),
                range: Some((7, 0)),
                dimensions: None,
            }],
        );

        assert_parses_to(
            parse_register_declaration,
            "reg a[7:0];",
            vec![RegisterDeclaration {
                name: "a".into(),
                range: None,
                dimensions: Some((7, 0)),
            }],
        );

        assert_eq!(
            parse_register_declaration("reg [15:0] b;"),
            Ok((
                "",
                vec![RegisterDeclaration {
                    name: "b".into(),
                    range: Some((15, 0)),
                    dimensions: None,
                }]
            ))
        );

        assert_eq!(
            parse_register_declaration("reg c[15:0];"),
            Ok((
                "",
                vec![RegisterDeclaration {
                    name: "c".into(),
                    range: None,
                    dimensions: Some((15, 0)),
                }]
            ))
        );

        assert_eq!(
            parse_register_declaration("reg [31:0] d[0:255];"),
            Ok((
                "",
                vec![RegisterDeclaration {
                    name: "d".into(),
                    range: Some((31, 0)),
                    dimensions: Some((0, 255)),
                }]
            ))
        );
    }

    #[test]
    fn test_parse_memory_declaration() {
        assert_parses_to(
            parse_register_declaration,
            "reg [7:0] memb[0:255];",
            vec![RegisterDeclaration {
                name: "memb".into(),
                range: Some((7, 0)),
                dimensions: Some((0, 255)),
            }],
        );

        assert_eq!(
            parse_register_declaration("reg [15:0] mem[0:1023];"),
            Ok((
                "",
                vec![RegisterDeclaration {
                    name: "mem".into(),
                    range: Some((15, 0)),
                    dimensions: Some((0, 1023)),
                }]
            ))
        );

        assert_eq!(
            parse_register_declaration("reg [31:0] mem32[0:2047];"),
            Ok((
                "",
                vec![RegisterDeclaration {
                    name: "mem32".into(),
                    range: Some((31, 0)),
                    dimensions: Some((0, 2047)),
                }]
            ))
        );

        assert_eq!(
            parse_register_declaration("reg [63:0] mem64[0:4095];"),
            Ok((
                "",
                vec![RegisterDeclaration {
                    name: "mem64".into(),
                    range: Some((63, 0)),
                    dimensions: Some((0, 4095)),
                }]
            ))
        );
    }

    /// The width in front of a comma-separated list applies to *every* name.
    #[test]
    fn test_register_list_shares_one_width() {
        assert_parses_to(
            parse_register_declaration,
            "reg [4:0] result, b;",
            vec![
                RegisterDeclaration {
                    name: "result".into(),
                    range: Some((4, 0)),
                    dimensions: None,
                },
                RegisterDeclaration {
                    name: "b".into(),
                    range: Some((4, 0)),
                    dimensions: None,
                },
            ],
        );

        assert_parses_to(
            parse_register_declaration,
            "reg a, b, c;",
            vec![
                RegisterDeclaration {
                    name: "a".into(),
                    range: None,
                    dimensions: None,
                },
                RegisterDeclaration {
                    name: "b".into(),
                    range: None,
                    dimensions: None,
                },
                RegisterDeclaration {
                    name: "c".into(),
                    range: None,
                    dimensions: None,
                },
            ],
        );
    }

    /// A dimension is per-name, so a list can mix a plain register and a
    /// memory under one width.
    #[test]
    fn test_register_list_mixes_scalars_and_memories() {
        assert_parses_to(
            parse_register_declaration,
            "reg [7:0] a, mem [0:15], b;",
            vec![
                RegisterDeclaration {
                    name: "a".into(),
                    range: Some((7, 0)),
                    dimensions: None,
                },
                RegisterDeclaration {
                    name: "mem".into(),
                    range: Some((7, 0)),
                    dimensions: Some((0, 15)),
                },
                RegisterDeclaration {
                    name: "b".into(),
                    range: Some((7, 0)),
                    dimensions: None,
                },
            ],
        );
    }

    /// Comments are legal at every token boundary of a declaration list.
    #[test]
    fn test_register_list_tolerates_comments_and_newlines() {
        assert_parses_to(
            parse_register_declaration,
            "reg /*w*/ [3:0] a , // first\n b ;",
            vec![
                RegisterDeclaration {
                    name: "a".into(),
                    range: Some((3, 0)),
                    dimensions: None,
                },
                RegisterDeclaration {
                    name: "b".into(),
                    range: Some((3, 0)),
                    dimensions: None,
                },
            ],
        );
    }

    /// A trailing or leading comma is a malformed list, not a short one.
    #[test]
    fn test_register_list_rejects_dangling_commas() {
        for source in ["reg a, ;", "reg , a;", "reg a,;"] {
            assert!(
                parse_register_declaration(source).is_err(),
                "{:?} should fail to parse",
                source
            );
        }
    }
}

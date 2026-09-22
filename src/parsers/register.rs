use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, satisfy},
    combinator::{not, opt},
    multi::separated_list1,
    sequence::{preceded, terminated},
    IResult,
};

use super::{
    expr::{verilog_expression, Expression},
    identifier::{identifier, Identifier},
    simple::{declared_range, dimensions, signedness, ws, Range},
};

#[derive(Debug, PartialEq)]
pub struct RegisterDeclaration {
    pub name: Identifier,
    pub range: Option<Range>,
    /// The address dimensions that make the name an array, outermost first:
    /// one for `mem [0:255]`, two for `mem [0:3][0:15]`, none for an ordinary
    /// vector.
    pub dimensions: Vec<Range>,
    /// Whether the declaration carried a `signed` qualifier. The qualifier
    /// belongs to the declaration, so every name in `reg signed [3:0] a, b;`
    /// gets it.
    pub signed: bool,
    /// The value a `reg a = expr;` declaration starts with.
    ///
    /// A variable initialiser is applied *once*, at time zero. It is not a
    /// continuous assignment: a later procedural write owns the register from
    /// then on.
    pub init: Option<Expression>,
}

/// One declared name, the address dimensions that make it a memory
/// (`mem [0:255]`, `mem [0:3][0:15]`), and the optional initialiser that gives
/// it a starting value (`a = 0`).
///
/// Both belong to the *name*, not to the declaration as a whole, which is what
/// makes `reg [7:0] a, mem [0:15];` legal — one width, but only the second name
/// is a memory — and what gives `reg a = 0, b = 1;` two different starting
/// values.
pub fn declared_name(input: &str) -> IResult<&str, (Identifier, Vec<Range>, Option<Expression>)> {
    let (input, name) = identifier(input)?;
    let (input, dims) = dimensions(input)?;
    let (input, init) = opt(preceded(ws(char('=')), verilog_expression))(input)?;
    Ok((input, (name, dims, init)))
}

/// `reg`, or SystemVerilog's `logic`, which iverilog 12.0 reads as exactly a
/// `reg` in its default mode — down to refusing a continuous assignment to one
/// with "reg a; cannot be driven by primitives or continuous assignment"
/// (corpus `br_gh1178b`, `br_gh1178c`). It is not a 1364-2005 reserved word,
/// so it needs a word boundary: `logic_level = 1` is still an identifier.
fn register_keyword(input: &str) -> IResult<&str, &str> {
    alt((
        tag("reg"),
        terminated(
            tag("logic"),
            not(satisfy(|c: char| {
                c.is_alphanumeric() || c == '_' || c == '$'
            })),
        ),
    ))(input)
}

/// `reg [width]? name [dims]? (, name [dims]?)* ;`
///
/// The width applies to every name in the list. A memory is not a separate
/// production — it is one of these names with a dimension attached — so there
/// is no "memory before register" ordering hazard to get wrong.
pub fn parse_register_declaration(input: &str) -> IResult<&str, Vec<RegisterDeclaration>> {
    let (input, _) = register_keyword(input)?;
    let (input, signed) = ws(signedness)(input)?;
    let (input, width) = ws(opt(declared_range))(input)?;
    let (input, names) = separated_list1(ws(char(',')), ws(declared_name))(input)?;
    let (input, _) = ws(char(';'))(input)?;

    Ok((
        input,
        names
            .into_iter()
            .map(|(name, dimensions, init)| RegisterDeclaration {
                name,
                range: width.clone(),
                dimensions,
                signed,
                init,
            })
            .collect(),
    ))
}

#[cfg(test)]
mod tests {
    use crate::parsers::helpers::{assert_parses, assert_parses_to};

    use super::*;

    /// `logic` declares what `reg` does, and only as a whole word.
    #[test]
    fn test_logic_is_a_register() {
        assert_eq!(
            assert_parses(parse_register_declaration, "logic [3:0] bus;"),
            assert_parses(parse_register_declaration, "reg [3:0] bus;")
        );
        assert_eq!(
            assert_parses(parse_register_declaration, "logic signed passed = 1'b1;"),
            assert_parses(parse_register_declaration, "reg signed passed = 1'b1;")
        );
        assert!(parse_register_declaration("logical x;").is_err());
    }

    /// A range with a bound missing must be a parse *error*, never a panic.
    ///
    /// `parse_dimensions` used to duplicate `simple::range` using `take_while`
    /// (zero or more digits) followed by `.unwrap()`, so `reg [:0] x;` matched
    /// an empty digit run and panicked on the failed `parse::<i64>()`. Real
    /// corpus files hit this the moment port-less modules started parsing.
    #[test]
    fn test_empty_range_bounds_error_rather_than_panic() {
        for source in ["reg [:0] x;", "reg [7:] x;", "reg [] x;"] {
            assert!(
                parse_register_declaration(source).is_err(),
                "{:?} should fail to parse, not panic",
                source
            );
        }
    }

    /// A bound that is not a literal is a *width*, kept for elaboration to
    /// resolve. This is what `reg [WIDTH-1:0] q;` needs, and it applies to the
    /// address dimension of a memory as much as to the width.
    #[test]
    fn test_expression_range_bounds_are_kept() {
        let declared = assert_parses(parse_register_declaration, "reg [WIDTH-1:0] q;");
        assert_eq!(declared.len(), 1);
        assert!(matches!(declared[0].range, Some(Range::Expressions(_, _))));

        let memory = assert_parses(parse_register_declaration, "reg [7:0] m [0: depth-1];");
        assert!(matches!(memory[0].range, Some(Range::Constant(7, 0))));
        assert!(matches!(
            memory[0].dimensions[..],
            [Range::Expressions(_, _)]
        ));
    }

    /// `reg [7:0] a [0:3][0:15];` — a multi-dimensional unpacked array, whose
    /// dimensions belong to the name and are kept in the order they were
    /// written, outermost first.
    #[test]
    fn test_multi_dimensional_array_keeps_every_dimension() {
        let declared = assert_parses(parse_register_declaration, "reg [7:0] a [0:3][0:15];");
        assert_eq!(declared.len(), 1);
        assert_eq!(declared[0].range, Some(Range::Constant(7, 0)));
        assert_eq!(
            declared[0].dimensions,
            vec![Range::Constant(0, 3), Range::Constant(0, 15)]
        );

        // Whitespace and comments between dimensions are token boundaries like
        // any other.
        let spaced = assert_parses(
            parse_register_declaration,
            "reg [3:0] m [0:1] /* rows */ [0:2][0:3];",
        );
        assert_eq!(spaced[0].dimensions.len(), 3);
    }

    #[test]
    fn test_parse_register_declaration() {
        assert_parses_to(
            parse_register_declaration,
            "reg a;",
            vec![RegisterDeclaration {
                name: "a".into(),
                range: None,
                dimensions: Vec::new(),
                signed: false,
                init: None,
            }],
        );

        assert_parses_to(
            parse_register_declaration,
            "reg [7:0] a;",
            vec![RegisterDeclaration {
                name: "a".into(),
                range: Some(Range::Constant(7, 0)),
                dimensions: Vec::new(),
                signed: false,
                init: None,
            }],
        );

        assert_parses_to(
            parse_register_declaration,
            "reg a[7:0];",
            vec![RegisterDeclaration {
                name: "a".into(),
                range: None,
                dimensions: vec![Range::Constant(7, 0)],
                signed: false,
                init: None,
            }],
        );

        assert_eq!(
            parse_register_declaration("reg [15:0] b;"),
            Ok((
                "",
                vec![RegisterDeclaration {
                    name: "b".into(),
                    range: Some(Range::Constant(15, 0)),
                    dimensions: Vec::new(),
                    signed: false,
                    init: None,
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
                    dimensions: vec![Range::Constant(15, 0)],
                    signed: false,
                    init: None,
                }]
            ))
        );

        assert_eq!(
            parse_register_declaration("reg [31:0] d[0:255];"),
            Ok((
                "",
                vec![RegisterDeclaration {
                    name: "d".into(),
                    range: Some(Range::Constant(31, 0)),
                    dimensions: vec![Range::Constant(0, 255)],
                    signed: false,
                    init: None,
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
                range: Some(Range::Constant(7, 0)),
                dimensions: vec![Range::Constant(0, 255)],
                signed: false,
                init: None,
            }],
        );

        assert_eq!(
            parse_register_declaration("reg [15:0] mem[0:1023];"),
            Ok((
                "",
                vec![RegisterDeclaration {
                    name: "mem".into(),
                    range: Some(Range::Constant(15, 0)),
                    dimensions: vec![Range::Constant(0, 1023)],
                    signed: false,
                    init: None,
                }]
            ))
        );

        assert_eq!(
            parse_register_declaration("reg [31:0] mem32[0:2047];"),
            Ok((
                "",
                vec![RegisterDeclaration {
                    name: "mem32".into(),
                    range: Some(Range::Constant(31, 0)),
                    dimensions: vec![Range::Constant(0, 2047)],
                    signed: false,
                    init: None,
                }]
            ))
        );

        assert_eq!(
            parse_register_declaration("reg [63:0] mem64[0:4095];"),
            Ok((
                "",
                vec![RegisterDeclaration {
                    name: "mem64".into(),
                    range: Some(Range::Constant(63, 0)),
                    dimensions: vec![Range::Constant(0, 4095)],
                    signed: false,
                    init: None,
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
                    range: Some(Range::Constant(4, 0)),
                    dimensions: Vec::new(),
                    signed: false,
                    init: None,
                },
                RegisterDeclaration {
                    name: "b".into(),
                    range: Some(Range::Constant(4, 0)),
                    dimensions: Vec::new(),
                    signed: false,
                    init: None,
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
                    dimensions: Vec::new(),
                    signed: false,
                    init: None,
                },
                RegisterDeclaration {
                    name: "b".into(),
                    range: None,
                    dimensions: Vec::new(),
                    signed: false,
                    init: None,
                },
                RegisterDeclaration {
                    name: "c".into(),
                    range: None,
                    dimensions: Vec::new(),
                    signed: false,
                    init: None,
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
                    range: Some(Range::Constant(7, 0)),
                    dimensions: Vec::new(),
                    signed: false,
                    init: None,
                },
                RegisterDeclaration {
                    name: "mem".into(),
                    range: Some(Range::Constant(7, 0)),
                    dimensions: vec![Range::Constant(0, 15)],
                    signed: false,
                    init: None,
                },
                RegisterDeclaration {
                    name: "b".into(),
                    range: Some(Range::Constant(7, 0)),
                    dimensions: Vec::new(),
                    signed: false,
                    init: None,
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
                    range: Some(Range::Constant(3, 0)),
                    dimensions: Vec::new(),
                    signed: false,
                    init: None,
                },
                RegisterDeclaration {
                    name: "b".into(),
                    range: Some(Range::Constant(3, 0)),
                    dimensions: Vec::new(),
                    signed: false,
                    init: None,
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

    /// The expression a source fragment parses to, so a test can spell an
    /// initialiser the way Verilog does rather than as an AST literal.
    fn expression(source: &str) -> Expression {
        let (rest, expression) =
            verilog_expression(source).expect("the expression should have parsed");
        assert!(rest.is_empty(), "unparsed input: {}", rest);
        expression
    }

    #[test]
    fn test_register_initialiser() {
        assert_parses_to(
            parse_register_declaration,
            "reg [3:0] b = 4'h5;",
            vec![RegisterDeclaration {
                name: "b".into(),
                range: Some(Range::Constant(3, 0)),
                dimensions: Vec::new(),
                signed: false,
                init: Some(expression("4'h5")),
            }],
        );
    }

    /// An initialiser belongs to the *name*, so every register in a list gets
    /// its own starting value.
    #[test]
    fn test_register_initialisers_are_per_name() {
        assert_parses_to(
            parse_register_declaration,
            "reg x = 1, y = 2, z;",
            vec![
                RegisterDeclaration {
                    name: "x".into(),
                    range: None,
                    dimensions: Vec::new(),
                    signed: false,
                    init: Some(expression("1")),
                },
                RegisterDeclaration {
                    name: "y".into(),
                    range: None,
                    dimensions: Vec::new(),
                    signed: false,
                    init: Some(expression("2")),
                },
                RegisterDeclaration {
                    name: "z".into(),
                    range: None,
                    dimensions: Vec::new(),
                    signed: false,
                    init: None,
                },
            ],
        );
    }

    /// `signed` sits between the keyword and the width, and belongs to the
    /// declaration, so every name in the list carries it.
    #[test]
    fn test_register_declaration_signedness() {
        assert_parses_to(
            parse_register_declaration,
            "reg signed [3:0] a, b;",
            vec![
                RegisterDeclaration {
                    name: "a".into(),
                    range: Some(Range::Constant(3, 0)),
                    dimensions: Vec::new(),
                    signed: true,
                    init: None,
                },
                RegisterDeclaration {
                    name: "b".into(),
                    range: Some(Range::Constant(3, 0)),
                    dimensions: Vec::new(),
                    signed: true,
                    init: None,
                },
            ],
        );

        // `unsigned` is the default said out loud, and a name that merely
        // starts with the keyword is a name.
        assert_parses_to(
            parse_register_declaration,
            "reg unsigned x;",
            vec![RegisterDeclaration {
                name: "x".into(),
                range: None,
                dimensions: Vec::new(),
                signed: false,
                init: None,
            }],
        );
        assert_parses_to(
            parse_register_declaration,
            "reg signed_x;",
            vec![RegisterDeclaration {
                name: "signed_x".into(),
                range: None,
                dimensions: Vec::new(),
                signed: false,
                init: None,
            }],
        );
    }
}

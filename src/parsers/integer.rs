//! The keyword-led variable declarations: `integer`, `time`, `real` and
//! `event`.
//!
//! All four share one shape — a keyword, then a comma separated list of names,
//! each with an optional array dimension and an optional initialiser — and
//! differ only in what the keyword means to the simulator. `integer` is a
//! signed 32-bit variable, `time` an unsigned 64-bit one, `real` a 64-bit
//! IEEE-754 double, and `event` a name with no value at all.

use nom::{
    branch::alt, bytes::complete::tag, character::complete::char, multi::separated_list1, IResult,
};

use super::{
    expr::Expression,
    identifier::{identifier, Identifier},
    register::declared_name,
    simple::{ws, Range},
};

/// One name from an `integer a, b[0:3];` declaration.
///
/// An `integer` is a fixed 32-bit value, so it carries no width — only the
/// optional array dimension. It is also *signed*, and says so by being an
/// `integer`, which is why there is no qualifier here to record: `elaborate`
/// declares one signed without asking.
#[derive(Debug, PartialEq)]
pub struct IntegerDeclaration {
    pub name: Identifier,
    pub dimensions: Option<Range>,
    /// The value an `integer i = 0;` declaration starts with, applied once at
    /// time zero the way a `reg` initialiser is.
    pub init: Option<Expression>,
}

/// One name from a `time t, stamps[0:3];` declaration.
///
/// A `time` is a fixed 64-bit *unsigned* value — the width simulated time is
/// counted in — so like an `integer` it carries no width of its own.
#[derive(Debug, PartialEq)]
pub struct TimeDeclaration {
    pub name: Identifier,
    pub dimensions: Option<Range>,
    pub init: Option<Expression>,
}

/// One name from a `real r, samples[2:1];` declaration.
///
/// A `real` is a 64-bit IEEE-754 double, so like an `integer` it carries no
/// width of its own — the type is the whole of it. `realtime` is the same
/// declaration under a longer keyword.
#[derive(Debug, PartialEq)]
pub struct RealDeclaration {
    pub name: Identifier,
    pub dimensions: Option<Range>,
    pub init: Option<Expression>,
}

/// One name from an `event a, b;` declaration.
///
/// A named event is a synchronisation object, not a variable: it has no width,
/// no value and no initialiser — only a name that `-> a;` triggers and
/// `always @(a)` waits on. That is why this carries a name and nothing else.
#[derive(Debug, PartialEq)]
pub struct EventDeclaration {
    pub name: Identifier,
}

/// The shape every one of these declarations shares: the keyword, then the
/// comma separated list of names that `register::declared_name` already reads
/// for `reg`.
fn variable_declaration<'a>(
    keyword: &'static str,
    input: &'a str,
) -> IResult<&'a str, Vec<(Identifier, Option<Range>, Option<Expression>)>> {
    let (input, _) = tag(keyword)(input)?;
    let (input, names) = separated_list1(ws(char(',')), ws(declared_name))(input)?;
    let (input, _) = ws(char(';'))(input)?;
    Ok((input, names))
}

pub fn parse_integer_declaration(input: &str) -> IResult<&str, Vec<IntegerDeclaration>> {
    let (input, names) = variable_declaration("integer", input)?;

    Ok((
        input,
        names
            .into_iter()
            .map(|(name, dimensions, init)| IntegerDeclaration {
                name,
                dimensions,
                init,
            })
            .collect(),
    ))
}

pub fn parse_time_declaration(input: &str) -> IResult<&str, Vec<TimeDeclaration>> {
    let (input, names) = variable_declaration("time", input)?;

    Ok((
        input,
        names
            .into_iter()
            .map(|(name, dimensions, init)| TimeDeclaration {
                name,
                dimensions,
                init,
            })
            .collect(),
    ))
}

/// `real r;` and `realtime t;`, which are the same declaration under two
/// spellings. The longer keyword is tried first, since `real` is a prefix of
/// it.
pub fn parse_real_declaration(input: &str) -> IResult<&str, Vec<RealDeclaration>> {
    let (input, names) = alt((
        |input| variable_declaration("realtime", input),
        |input| variable_declaration("real", input),
    ))(input)?;

    Ok((
        input,
        names
            .into_iter()
            .map(|(name, dimensions, init)| RealDeclaration {
                name,
                dimensions,
                init,
            })
            .collect(),
    ))
}

/// `event a, b;` — a list of names and nothing else. An event has no value, so
/// unlike the declarations above it takes neither a dimension nor an
/// initialiser.
pub fn parse_event_declaration(input: &str) -> IResult<&str, Vec<EventDeclaration>> {
    let (input, _) = tag("event")(input)?;
    let (input, names) = separated_list1(ws(char(',')), ws(identifier))(input)?;
    let (input, _) = ws(char(';'))(input)?;

    Ok((
        input,
        names
            .into_iter()
            .map(|name| EventDeclaration { name })
            .collect(),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_integer_declaration() {
        let input = "integer ident1, ident2, ident3;";
        let result = parse_integer_declaration(input);
        assert!(result.is_ok());
        let (_, identifiers) = result.unwrap();

        let strings: Vec<String> = identifiers.iter().map(|i| i.name.name.clone()).collect();

        assert_eq!(strings, vec!["ident1", "ident2", "ident3"]);

        let input = "integer ident4, ident5;";
        let result = parse_integer_declaration(input);
        assert!(result.is_ok());
        let (_, identifiers) = result.unwrap();

        let strings: Vec<String> = identifiers.iter().map(|i| i.name.name.clone()).collect();

        assert_eq!(strings, vec!["ident4", "ident5"]);

        let input = "integer ident6;";
        let result = parse_integer_declaration(input);
        assert!(result.is_ok());
        let (_, identifiers) = result.unwrap();

        let strings: Vec<String> = identifiers.iter().map(|i| i.name.name.clone()).collect();

        assert_eq!(strings, vec!["ident6"]);
    }

    /// An `integer` carries no width — only an optional array dimension.
    #[test]
    fn test_parse_integer_declaration_shapes() {
        use crate::parsers::helpers::assert_parses_to;

        assert_parses_to(
            parse_integer_declaration,
            "integer i, j;",
            vec![
                IntegerDeclaration {
                    name: "i".into(),
                    dimensions: None,
                    init: None,
                },
                IntegerDeclaration {
                    name: "j".into(),
                    dimensions: None,
                    init: None,
                },
            ],
        );

        assert_parses_to(
            parse_integer_declaration,
            "integer\n  counts [0:3];",
            vec![IntegerDeclaration {
                name: "counts".into(),
                dimensions: Some(Range::Constant(0, 3)),
                init: None,
            }],
        );
    }

    #[test]
    fn test_parse_integer_declaration_invalid_identifier() {
        let input = "integer 123ident;";
        let result = parse_integer_declaration(input);
        assert!(result.is_err());

        let input = "integer ident1, 123ident;";
        let result = parse_integer_declaration(input);
        assert!(result.is_err());

        let input = "integer ident1, ident2, 123ident;";
        let result = parse_integer_declaration(input);
        assert!(result.is_err());
    }

    /// An `integer` takes an initialiser the way a `reg` does, per name.
    #[test]
    fn test_parse_integer_declaration_initialisers() {
        use crate::parsers::expr::verilog_expression;
        use crate::parsers::helpers::assert_parses_to;

        let expression = |source: &str| {
            let (rest, expression) =
                verilog_expression(source).expect("the expression should have parsed");
            assert!(rest.is_empty(), "unparsed input: {}", rest);
            expression
        };

        assert_parses_to(
            parse_integer_declaration,
            "integer i = 0;",
            vec![IntegerDeclaration {
                name: "i".into(),
                dimensions: None,
                init: Some(expression("0")),
            }],
        );

        assert_parses_to(
            parse_integer_declaration,
            "integer i = 0, j;",
            vec![
                IntegerDeclaration {
                    name: "i".into(),
                    dimensions: None,
                    init: Some(expression("0")),
                },
                IntegerDeclaration {
                    name: "j".into(),
                    dimensions: None,
                    init: None,
                },
            ],
        );
    }

    /// A `time` declaration has the shape an `integer` does — a name list, an
    /// optional array dimension, an optional initialiser — because the keyword
    /// is the whole of the type.
    #[test]
    fn test_parse_time_declaration() {
        use crate::parsers::helpers::assert_parses_to;

        assert_parses_to(
            parse_time_declaration,
            "time phdelay;",
            vec![TimeDeclaration {
                name: "phdelay".into(),
                dimensions: None,
                init: None,
            }],
        );

        assert_parses_to(
            parse_time_declaration,
            "time first, marks [0:3];",
            vec![
                TimeDeclaration {
                    name: "first".into(),
                    dimensions: None,
                    init: None,
                },
                TimeDeclaration {
                    name: "marks".into(),
                    dimensions: Some(Range::Constant(0, 3)),
                    init: None,
                },
            ],
        );
    }

    /// `real array3[2:1];` is the corpus shape: a real *array*, whose address
    /// dimension belongs to the name exactly as a memory's does.
    #[test]
    fn test_parse_real_declaration() {
        use crate::parsers::helpers::assert_parses_to;

        assert_parses_to(
            parse_real_declaration,
            "real r;",
            vec![RealDeclaration {
                name: "r".into(),
                dimensions: None,
                init: None,
            }],
        );

        assert_parses_to(
            parse_real_declaration,
            "real array3[2:1];",
            vec![RealDeclaration {
                name: "array3".into(),
                dimensions: Some(Range::Constant(2, 1)),
                init: None,
            }],
        );

        // `realtime` is the same declaration under a longer keyword, so it must
        // not read as `real` followed by a name of `time`.
        assert_parses_to(
            parse_real_declaration,
            "realtime t;",
            vec![RealDeclaration {
                name: "t".into(),
                dimensions: None,
                init: None,
            }],
        );
    }

    /// An event has no width, no dimension and no value, so the declaration is
    /// a list of bare names.
    #[test]
    fn test_parse_event_declaration() {
        use crate::parsers::helpers::assert_parses_to;

        assert_parses_to(
            parse_event_declaration,
            "event event_ident;",
            vec![EventDeclaration {
                name: "event_ident".into(),
            }],
        );

        assert_parses_to(
            parse_event_declaration,
            "event a, b;",
            vec![
                EventDeclaration { name: "a".into() },
                EventDeclaration { name: "b".into() },
            ],
        );
    }

    /// A keyword only introduces a declaration when what follows it is a name
    /// of its own: `timer t1;` is not a `time`, and `realm x;` is not a `real`.
    #[test]
    fn test_keywords_do_not_swallow_longer_identifiers() {
        assert!(parse_time_declaration("timer t1;").is_err());
        assert!(parse_real_declaration("realm x;").is_err());
        assert!(parse_event_declaration("eventual e1;").is_err());
    }

    #[test]
    fn test_parse_integer_declaration_missing_semicolon() {
        let input = "integer ident1, ident2, ident3";
        let result = parse_integer_declaration(input);
        assert!(result.is_err());

        let input = "integer ident1, ident2";
        let result = parse_integer_declaration(input);
        assert!(result.is_err());

        let input = "integer ident1";
        let result = parse_integer_declaration(input);
        assert!(result.is_err());
    }
}

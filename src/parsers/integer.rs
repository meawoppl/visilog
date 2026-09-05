use nom::{bytes::complete::tag, character::complete::char, multi::separated_list1, IResult};

use super::{expr::Expression, identifier::Identifier, register::declared_name, simple::ws};

/// One name from an `integer a, b[0:3];` declaration.
///
/// An `integer` is a fixed 32-bit value, so it carries no width — only the
/// optional array dimension. It is also *signed*, which the simulator does not
/// model yet (issue #96).
#[derive(Debug, PartialEq)]
pub struct IntegerDeclaration {
    pub name: Identifier,
    pub dimensions: Option<(i64, i64)>,
    /// The value an `integer i = 0;` declaration starts with, applied once at
    /// time zero the way a `reg` initialiser is.
    pub init: Option<Expression>,
}

pub fn parse_integer_declaration(input: &str) -> IResult<&str, Vec<IntegerDeclaration>> {
    let (input, _) = tag("integer")(input)?;
    let (input, names) = separated_list1(ws(char(',')), ws(declared_name))(input)?;
    let (input, _) = ws(char(';'))(input)?;

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
                dimensions: Some((0, 3)),
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

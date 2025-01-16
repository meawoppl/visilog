use nom::{
    branch::alt,
    bytes::complete::{tag, take_while_m_n},
    character::complete::{alpha1, char},
    combinator::map_res,
    multi::separated_list1,
    sequence::tuple,
    IResult,
};

use super::simple::ws;


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
}

impl Identifier
{
    pub fn new(name: String) -> Self {
        Identifier { name }
    }
}


pub fn identifier(input: &str) -> IResult<&str, Identifier> {
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

pub fn identifier_list(input: &str) -> IResult<&str, Vec<Identifier>> {
    separated_list1(ws(char(',')), ws(identifier))(input)
}

mod tests {
    use super::*;
    use nom::Parser;

    #[test]
    fn test_identifiers_valid_first_characters() {
        let valid_identifiers = [
            "var_a", "_var_a", "Var_A", "_Var_A", "var_a1", "Var_A1", "_var_a1", "_Var_A1",
            "var_b", "_var_b", "Var_B", "_Var_B", "var_b1", "Var_B1", "_var_b1", "_Var_B1",
        ];
        for id_str in &valid_identifiers {
            let parsed = identifier(id_str);
            assert!(
                parsed.is_ok(),
                "Valid identifier {} failed to parse",
                id_str
            );

            assert_eq!(parsed.unwrap().1.name, id_str.to_string());
        }
    }

    #[test]
    fn test_identifiers_invalid_first_characters() {
        let invalid_identifiers = ["1var_a", "$var_a", "1Var_A", "$Var_A", "1var_b", "$var_b", "1Var_B", "$Var_B"];
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
        let result = identifier_list.parse("a").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(result.1, vec![Identifier::new("a".to_string())]);

        let result = identifier_list.parse("b").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(result.1, vec![Identifier::new("b".to_string())]);
    }

    #[test]
    fn test_identifier_list_double() {
        let result = identifier_list.parse("a,b").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(result.1, vec![
            Identifier::new("a".to_string()), Identifier::new("b".to_string())
        ]);

        let result = identifier_list.parse("b,c").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(result.1, vec![
            Identifier::new("b".to_string()), Identifier::new("c".to_string())
        ]);
    }

    #[test]
    fn test_identifier_list_multiple() {
        let result = identifier_list.parse("a, b, c").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(
            result.1,
            vec![Identifier::new("a".to_string()), Identifier::new("b".to_string()), Identifier::new("c".to_string())]
        );

        let result = identifier_list.parse("b, c, d").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(
            result.1,
            vec![Identifier::new("b".to_string()), Identifier::new("c".to_string()), Identifier::new("d".to_string())]
        );
    }

    #[test]
    fn test_identifier_list_with_whitespace() {
        let result = identifier_list.parse(" a , b , c ").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(
            result.1,
            vec![Identifier::new("a".to_string()), Identifier::new("b".to_string()), Identifier::new("c".to_string())]
        );

        let result = identifier_list.parse(" b , c , d ").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(
            result.1,
            vec![Identifier::new("b".to_string()), Identifier::new("c".to_string()), Identifier::new("d".to_string())]
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
}

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while_m_n},
    character::complete::{alpha1, char},
    combinator::map_res,
    multi::separated_list1,
    sequence::tuple,
    IResult,
};

use super::{base::RawToken, simple::ws};

#[derive(Clone, Debug, PartialEq, Eq)]
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

pub fn parse_bit_select(input: &str) -> IResult<&str, (Identifier, i64)> {
    let (input, id) = identifier(input)?;
    let (input, _) = char('[')(input)?;
    let (input, index) = map_res(
        take_while_m_n(1, 10, |c: char| c.is_digit(10)),
        |s: &str| s.parse::<i64>(),
    )(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, (id, index)))
}

pub fn parse_part_select(input: &str) -> IResult<&str, (Identifier, i64, i64)> {
    let (input, id) = identifier(input)?;
    let (input, _) = char('[')(input)?;
    let (input, start) = map_res(
        take_while_m_n(1, 10, |c: char| c.is_digit(10)),
        |s: &str| s.parse::<i64>(),
    )(input)?;
    let (input, _) = char(':')(input)?;
    let (input, end) = map_res(
        take_while_m_n(1, 10, |c: char| c.is_digit(10)),
        |s: &str| s.parse::<i64>(),
    )(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, (id, start, end)))
}

mod tests {
    use super::*;
    use nom::Parser;

    #[test]
    fn test_identifiers_valid_first_characters() {
        let valid_identifiers = [
            "var_a", "_var_a", "Var_A", "_Var_A", "var_a1", "Var_A1", "_var_a1", "_Var_A1",
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
    fn test_parse_identifier_valid_first_characters() {
        let valid_identifiers = [
            "my_module", "_my_module", "My_Module", "_My_Module",
            "my_module1", "My_Module1", "_my_module1", "_My_Module1"
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
        let invalid_identifiers = ["1var_a", "$var_a", "1Var_A", "$Var_A", "1my_module", "$my_module", "1My_Module", "$My_Module"];
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
    }

    #[test]
    fn test_identifier_list_double() {
        let result = identifier_list.parse("a,b").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(
            result.1,
            vec![
                Identifier::new("a".to_string()),
                Identifier::new("b".to_string())
            ]
        );
    }

    #[test]
    fn test_identifier_list_multiple() {
        let result = identifier_list.parse("a, b, c").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(
            result.1,
            vec![
                Identifier::new("a".to_string()),
                Identifier::new("b".to_string()),
                Identifier::new("c".to_string())
            ]
        );
    }

    #[test]
    fn test_identifier_list_with_whitespace() {
        let result = identifier_list.parse(" a , b , c ").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(
            result.1,
            vec![
                Identifier::new("a".to_string()),
                Identifier::new("b".to_string()),
                Identifier::new("c".to_string())
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
    }

    #[test]
    fn test_identifier_list_empty() {
        let result = identifier_list.parse("");
        assert!(result.is_err(), "Empty identifier list should not parse");
    }

    #[test]
    fn test_parse_bit_select() {
        let result = parse_bit_select("a[3]").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(result.1, (Identifier::new("a".to_string()), 3));
    }

    #[test]
    fn test_parse_part_select() {
        let result = parse_part_select("a[3:0]").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(result.1, (Identifier::new("a".to_string()), 3, 0));
    }
}

use nom::{branch::alt, bytes::complete::{tag, take_while}, character::complete::{alpha1, char}, combinator::recognize, multi::separated_list1, sequence::tuple, IResult};

use super::simple::ws;

pub fn identifier(input: &str) -> IResult<&str, String> {
    let (input, id) = recognize(tuple((
        alt((alpha1, tag("_"))), // Start with alpha or '_'
        take_while(|c: char| c.is_alphanumeric() || c == '_' || c == '$'), // Alphanumeric, '_', or '$'
    )))(input)?;

    let full_id = format!("{}{}", input, id);
    if full_id.len() > 1024 {
        Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::TooLarge)))
    } else {
        Ok((input, full_id))
    }

}

pub fn identifier_list(input: &str) -> IResult<&str, Vec<String>> {
    separated_list1(ws(char(',')), ws(identifier))(input)
}


mod tests {
    use nom::Parser;

    use super::*;


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

            assert_eq!(unwrapped.1, id_str.to_string());
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
    fn test_identifier_list() {
        identifier_list.parse("a").unwrap();
        identifier_list.parse("a, b, c").unwrap();
    }
}

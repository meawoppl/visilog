use nom::{bytes::complete::tag, character::complete::space1, sequence::terminated, IResult};

use super::identifier::identifier_list;

fn parse_integer_declaration(input: &str) -> IResult<&str, (&str, Vec<String>)> {
    let (input, _) = tag("integer")(input)?;
    let (input, _) = space1(input)?;
    let (input, identifiers) = terminated(identifier_list, tag(";"))(input)?;
    Ok((input, ("integer", identifiers)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_integer_declaration() {
        let input = "integer ident1, ident2, ident3;";
        let result = parse_integer_declaration(input);
        assert!(result.is_ok());
        let (_, (keyword, identifiers)) = result.unwrap();
        assert_eq!(keyword, "integer");
        assert_eq!(identifiers, vec!["ident1", "ident2", "ident3"]);
    }

    #[test]
    fn test_parse_integer_declaration_invalid_identifier() {
        let input = "integer 123ident;";
        let result = parse_integer_declaration(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_integer_declaration_missing_semicolon() {
        let input = "integer ident1, ident2, ident3";
        let result = parse_integer_declaration(input);
        assert!(result.is_err());
    }
}

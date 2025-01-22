use nom::{bytes::complete::tag, character::complete::space1, sequence::terminated, IResult};

use super::identifier::{identifier_list, Identifier};

fn parse_integer_declaration(input: &str) -> IResult<&str, (&str, Vec<Identifier>)> {
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

        let strings: Vec<String> = identifiers.iter().map(|i| i.name.to_string()).collect();

        assert_eq!(keyword, "integer");
        assert_eq!(strings, vec!["ident1", "ident2", "ident3"]);

        let input = "integer ident4, ident5;";
        let result = parse_integer_declaration(input);
        assert!(result.is_ok());
        let (_, (keyword, identifiers)) = result.unwrap();

        let strings: Vec<String> = identifiers.iter().map(|i| i.name.to_string()).collect();

        assert_eq!(keyword, "integer");
        assert_eq!(strings, vec!["ident4", "ident5"]);

        let input = "integer ident6;";
        let result = parse_integer_declaration(input);
        assert!(result.is_ok());
        let (_, (keyword, identifiers)) = result.unwrap();

        let strings: Vec<String> = identifiers.iter().map(|i| i.name.to_string()).collect();

        assert_eq!(keyword, "integer");
        assert_eq!(strings, vec!["ident6"]);
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

use nom::{
    bytes::complete::tag, character::complete::{multispace0, space1}, multi::separated_list1, sequence::{preceded, separated_pair, terminated, tuple}, IResult
};

use super::identifier::{ identifier_list};


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
}
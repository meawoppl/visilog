use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::multispace0,
    combinator::map,
    multi::many0,
    sequence::{delimited, preceded, tuple},
    IResult,
};

use crate::parsers::assignment::{parse_continuous_assignment, parse_procedural_assignment};

pub fn parse_initial_block(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
    let (input, _) = tag("initial")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, assignments) = parse_begin_end_block(input)?;
    Ok((input, assignments))
}

pub fn parse_always_block(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
    let (input, _) = tag("always")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, assignments) = parse_begin_end_block(input)?;
    Ok((input, assignments))
}

pub fn parse_begin_end_block(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
    let (input, assignments) = delimited(
        tag("begin"),
        many0(alt((parse_continuous_assignment, parse_procedural_assignment))),
        tag("end"),
    )(input)?;
    Ok((input, assignments))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_initial_block() {
        let input = r#"
            initial begin
                a = 'b1;
                b = 'b0;
            end
        "#;
        let result = parse_initial_block(input);
        assert!(result.is_ok());
        let (remaining, assignments) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(assignments.len(), 2);
    }

    #[test]
    fn test_parse_always_block() {
        let input = r#"
            always begin
                #50 a = ~a;
            end
        "#;
        let result = parse_always_block(input);
        assert!(result.is_ok());
        let (remaining, assignments) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(assignments.len(), 1);
    }
}

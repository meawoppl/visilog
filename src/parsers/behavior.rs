use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::map,
    multi::{many0, many1},
    IResult,
};

use crate::parsers::assignment::parse_assignment;

use super::{
    assignment::Assignment,
    delay::{parse_delay_statement, Delay},
    expr::Expression,
    simple::ws,
};

pub enum EventTriggers {
    PosEdge,
    NegEdge,
    EitherEdge,
}
pub struct Event {
    trigger: EventTriggers,
    expression: Expression,
}

pub enum ProceduralStatements {
    Delay(Delay),
    RegisterDeclaration,
    WireDeclaration,
    InitialBlock,
    AlwaysBlock(Vec<Event>, Vec<ProceduralStatements>),
    Assignment(Assignment),
}

pub fn procedural_statement(input: &str) -> IResult<&str, ProceduralStatements> {
    alt((
        map(parse_assignment, |a| ProceduralStatements::Assignment(a)),
        map(parse_delay_statement, |d| ProceduralStatements::Delay(d)),
    ))(input)
}

pub fn parse_initial_block(input: &str) -> IResult<&str, Vec<ProceduralStatements>> {
    let (input, _) = ws(tag("initial"))(input)?;
    let (input, assignments) = alt((parse_block, many1(procedural_statement)))(input)?;
    Ok((input, assignments))
}

pub fn parse_always_block(input: &str) -> IResult<&str, Vec<ProceduralStatements>> {
    let (input, _) = ws(tag("always"))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, assignments) = alt((parse_block, many1(procedural_statement)))(input)?;
    Ok((input, assignments))
}

pub fn parse_block(input: &str) -> IResult<&str, Vec<ProceduralStatements>> {
    let (input, _) = ws(tag("begin"))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, assignments) = many0(procedural_statement)(input)?;
    let (input, _) = ws(tag("end"))(input)?;
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
            end"#;
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
    #[test]
    fn test_block_or_statement_single() {
        let inputs = vec!["a = b;", "#50;", "#50 a = b;", "a = #50 b;"];

        for input in inputs {
            let result = procedural_statement(input);
            assert!(result.is_ok());
            let (remaining, _) = result.unwrap();
            assert_eq!(remaining, "");
        }
    }

    #[test]
    fn test_block_or_statement_multiple() {
        let input = r#"
            begin
                a = 'b1;
                b = 'b0;
            end
        "#;
        let result = parse_block(input);
        assert!(result.is_ok());
        let (remaining, statements) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(statements.len(), 2);
    }

    #[test]
    fn test_block_or_statement_empty() {
        let input = "begin end";
        let result = parse_block(input);
        assert!(result.is_ok());
    }
}

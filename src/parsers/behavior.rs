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
#[derive(Debug, PartialEq)]
pub enum EventTriggers {
    PosEdge,
    NegEdge,
    EitherEdge,
}

#[derive(Debug, PartialEq)]
pub struct Event {
    trigger: EventTriggers,
    expression: Expression,
}


#[derive(Debug, PartialEq)]
pub struct InitialBlock {
    statements: Vec<ProceduralStatements>,
}
impl InitialBlock {
    pub fn new(statements: Vec<ProceduralStatements>) -> Self {
        InitialBlock { statements }
    }
}

#[derive(Debug, PartialEq)]
pub struct AlwaysBlock {
    trigger_events: Vec<Event>,
    statements: Vec<ProceduralStatements>,
}

impl AlwaysBlock {
    pub fn new(trigger_events: Vec<Event>, statements: Vec<ProceduralStatements>) -> Self {
        AlwaysBlock {
            trigger_events,
            statements,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ProceduralStatements {
    Delay(Delay),
    RegisterDeclaration,
    WireDeclaration,
    InitialBlock(InitialBlock),
    AlwaysBlock(AlwaysBlock),
    Assignment(Assignment),
}

pub fn procedural_statement(input: &str) -> IResult<&str, ProceduralStatements> {
    alt((
        map(parse_assignment, |a| ProceduralStatements::Assignment(a)),
        map(parse_delay_statement, |d| ProceduralStatements::Delay(d)),
    ))(input)
}

pub fn parse_initial_block(input: &str) -> IResult<&str, InitialBlock> {
    let (input, _) = ws(tag("initial"))(input)?;
    let (input, assignments) = alt((parse_block, many1(procedural_statement)))(input)?;
    let initial_block = InitialBlock::new(assignments);
    Ok((input, initial_block))
}

pub fn parse_always_block(input: &str) -> IResult<&str, AlwaysBlock> {
    // TODO(meawoppl) - needs trigger statement parsing
    let (input, _) = ws(tag("always"))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, assignments) = alt((parse_block, many1(procedural_statement)))(input)?;
    
    let block = AlwaysBlock::new(vec![], assignments);
    
    Ok((input, block))
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
        let (remaining, initial_block) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(initial_block.statements.len(), 2);
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
        let (remaining, block) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(block.statements.len(), 1);
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

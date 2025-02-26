use nom::{
    branch::alt, bytes::complete::tag, character::complete::{multispace0, multispace1}, combinator::{map, opt}, multi::{many0, many1, separated_list0, separated_list1}, sequence::{delimited, tuple}, IResult
};

use crate::parsers::assignment::parse_assignment;
use crate::parsers::identifier::identifier;

use super::{
    assignment::{ContinuousAssignment, ProceduralAssignment}, delay::{parse_delay_statement, Delay}, expr::{verilog_expression, Expression}, identifier::{self, Identifier}, simple::ws
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
    pub statements: Vec<ProceduralStatements>,
}
impl InitialBlock {
    pub fn new(statements: Vec<ProceduralStatements>) -> Self {
        InitialBlock { statements }
    }
}

#[derive(Debug, PartialEq)]
pub struct AlwaysBlock {
    /// None if the trigger is `@(*)`
    /// Some if the trigger is a (possibly empty) list of events
    pub trigger_events: Option<Vec<Event>>,
    pub statements: Vec<ProceduralStatements>,
}

impl AlwaysBlock {
    pub fn new(trigger_events: Option<Vec<Event>>, statements: Vec<ProceduralStatements>) -> Self {
        AlwaysBlock {
            trigger_events,
            statements,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ProceduralStatements {
    Delay(Delay),
    Assignment(ProceduralAssignment),
}

pub enum ProceduralBlock {
    InitialBlock(InitialBlock),
    AlwaysBlock(AlwaysBlock),
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

pub fn edge_type(input: &str) -> IResult<&str,EventTriggers> {
    map(opt(alt((tag("posedge"), tag("negedge")))), |res| {
        match res {
            Some("posedge") => EventTriggers::PosEdge,
            Some("negedge") => EventTriggers::NegEdge,
            Some(_) => unreachable!(),
            None => EventTriggers::EitherEdge,            
        }
    })(input)
}   

pub fn trigger_expression(input: &str)-> IResult<&str, Event> {
    let (input, trigger) = edge_type(input)?;
    let (input, expression) = verilog_expression(input)?;

    Ok((input, Event{ trigger, expression}))
}

pub fn trigger_block(input: &str) -> IResult<&str, Vec<Event>> {
    let (input, _) = tag("@(")(input)?;
    let (input, _) = multispace0(input)?;
    let delimited_or = delimited(multispace1, tag("or"), multispace1);
    let (input, events) = separated_list0(delimited_or, trigger_expression)(input)?;
    let (input, _) = tag(")")(input)?;
    Ok((input, events))
}

pub fn parse_always_block(input: &str) -> IResult<&str, AlwaysBlock> {
    // TODO(meawoppl) - needs trigger statement parsing
    let (input, _) = ws(tag("always"))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, trigger_events) = opt(trigger_block)(input)?;
    let (input, assignments) = alt((parse_block, many1(procedural_statement)))(input)?;

    let block = AlwaysBlock::new(trigger_events, assignments);

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
    use crate::parsers::helpers::assert_parses;

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

    #[test]
    fn test_always_block_event_permutation() {
        let test_cases = vec![
            "always @(*) begin end",
            "always @(a) begin end",
            "always @() begin end",
            "always @(a) b = c;",
            "always @(a, b) begin c = a & b; d = a | b; end",
            "always @(clk) begin if (reset) begin q <= 0; end else begin q <= d; end end",
            "always @(a, b) begin reg temp; temp = a & b; out = temp; end",
        ];

        for input in test_cases {
            let result = parse_always_block(input);
            assert_parses(parse_always_block, input);
        }

    }

}

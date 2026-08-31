use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, multispace1},
    combinator::{map, opt, peek, value},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, preceded, terminated},
    IResult,
};

use crate::parsers::assignment::parse_assignment;

use super::{
    assignment::ProceduralAssignment,
    delay::{parse_delay_statement, Delay},
    expr::{verilog_expression, Expression},
    simple::ws,
};
#[derive(Debug, PartialEq, Clone)]
pub enum EventTriggers {
    PosEdge,
    NegEdge,
    EitherEdge,
}

#[derive(Debug, PartialEq)]
pub struct Event {
    pub trigger: EventTriggers,
    pub expression: Expression,
}

impl Event {
    pub fn new(trigger: EventTriggers, expression: Expression) -> Self {
        Event {
            trigger,
            expression,
        }
    }
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

/// How an `always` block is triggered. The three forms are distinct constructs
/// and simulate differently.
#[derive(Debug, PartialEq)]
pub enum EventControl {
    /// `always begin … end` — no event control, the body runs continuously.
    None,
    /// `always @(*)` — implicitly sensitive to every signal read in the body.
    Implicit,
    /// `always @(posedge clk or negedge rst)` — an explicit sensitivity list.
    Events(Vec<Event>),
}

#[derive(Debug, PartialEq)]
pub struct AlwaysBlock {
    pub event_control: EventControl,
    pub statements: Vec<ProceduralStatements>,
}

impl AlwaysBlock {
    pub fn new(event_control: EventControl, statements: Vec<ProceduralStatements>) -> Self {
        AlwaysBlock {
            event_control,
            statements,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_statements: Vec<ProceduralStatements>,
    pub else_statements: Option<Vec<ProceduralStatements>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CaseLabel {
    Default,
    Expressions(Vec<Expression>),
}

#[derive(Debug, PartialEq)]
pub struct CaseItem {
    pub label: CaseLabel,
    pub statements: Vec<ProceduralStatements>,
}

#[derive(Debug, PartialEq)]
pub struct CaseStatement {
    pub subject: Expression,
    pub items: Vec<CaseItem>,
}

#[derive(Debug, PartialEq)]
pub enum ProceduralStatements {
    Delay(Delay),
    Assignment(ProceduralAssignment),
    If(IfStatement),
    Case(CaseStatement),
}

pub enum ProceduralBlock {
    InitialBlock(InitialBlock),
    AlwaysBlock(AlwaysBlock),
}

pub fn procedural_statement(input: &str) -> IResult<&str, ProceduralStatements> {
    alt((
        map(parse_if_statement, |i| ProceduralStatements::If(i)),
        map(parse_case_statement, |c| ProceduralStatements::Case(c)),
        map(parse_assignment, |a| ProceduralStatements::Assignment(a)),
        map(parse_delay_statement, |d| ProceduralStatements::Delay(d)),
    ))(input)
}

/// The body of a conditional or case arm: either a `begin`…`end` block or a
/// single statement.
fn statement_body(input: &str) -> IResult<&str, Vec<ProceduralStatements>> {
    alt((parse_block, map(procedural_statement, |s| vec![s])))(input)
}

fn parenthesized_expression(input: &str) -> IResult<&str, Expression> {
    delimited(ws(char('(')), verilog_expression, ws(char(')')))(input)
}

pub fn parse_if_statement(input: &str) -> IResult<&str, IfStatement> {
    let (input, _) = ws(tag("if"))(input)?;
    let (input, condition) = parenthesized_expression(input)?;
    let (input, then_statements) = statement_body(input)?;
    let (input, else_statements) = opt(preceded(ws(tag("else")), statement_body))(input)?;

    Ok((
        input,
        IfStatement {
            condition,
            then_statements,
            else_statements,
        },
    ))
}

fn parse_case_label(input: &str) -> IResult<&str, CaseLabel> {
    alt((
        // The peek keeps an identifier like `default_state` from being read as
        // the `default` keyword, which alt() could not back out of.
        value(
            CaseLabel::Default,
            terminated(ws(tag("default")), peek(char(':'))),
        ),
        map(
            separated_list1(ws(char(',')), verilog_expression),
            CaseLabel::Expressions,
        ),
    ))(input)
}

fn parse_case_item(input: &str) -> IResult<&str, CaseItem> {
    let (input, label) = parse_case_label(input)?;
    let (input, _) = ws(char(':'))(input)?;
    let (input, statements) = statement_body(input)?;

    Ok((input, CaseItem { label, statements }))
}

pub fn parse_case_statement(input: &str) -> IResult<&str, CaseStatement> {
    let (input, _) = ws(tag("case"))(input)?;
    let (input, subject) = parenthesized_expression(input)?;
    let (input, items) = many1(parse_case_item)(input)?;
    let (input, _) = ws(tag("endcase"))(input)?;

    Ok((input, CaseStatement { subject, items }))
}

fn parse_edge(input: &str) -> IResult<&str, EventTriggers> {
    terminated(
        alt((
            value(EventTriggers::PosEdge, tag("posedge")),
            value(EventTriggers::NegEdge, tag("negedge")),
        )),
        multispace1,
    )(input)
}

fn parse_event(input: &str) -> IResult<&str, Event> {
    let (input, trigger) = opt(parse_edge)(input)?;
    let (input, expression) = verilog_expression(input)?;

    Ok((
        input,
        Event::new(trigger.unwrap_or(EventTriggers::EitherEdge), expression),
    ))
}

/// Events in a sensitivity list are separated by `,` or by the `or` keyword.
/// The trailing whitespace is required for `or` so that an identifier like
/// `origin` is not read as a separator plus an event.
fn event_separator(input: &str) -> IResult<&str, &str> {
    ws(alt((tag(","), terminated(tag("or"), multispace1))))(input)
}

/// Parse an event control expression: `@(posedge clk or negedge rst)`,
/// `@(a, b)` or `@(*)`. The wildcard form yields `EventControl::Implicit`,
/// which is distinct from a block that carries no event control at all.
pub fn parse_sensitivity_list(input: &str) -> IResult<&str, EventControl> {
    let (input, _) = ws(char('@'))(input)?;
    delimited(
        ws(char('(')),
        alt((
            map(ws(char('*')), |_| EventControl::Implicit),
            map(
                separated_list1(event_separator, parse_event),
                EventControl::Events,
            ),
        )),
        ws(char(')')),
    )(input)
}

pub fn parse_initial_block(input: &str) -> IResult<&str, InitialBlock> {
    let (input, _) = ws(tag("initial"))(input)?;
    let (input, assignments) = alt((parse_block, many1(procedural_statement)))(input)?;
    let initial_block = InitialBlock::new(assignments);
    Ok((input, initial_block))
}

pub fn parse_always_block(input: &str) -> IResult<&str, AlwaysBlock> {
    let (input, _) = ws(tag("always"))(input)?;
    let (input, event_control) = map(opt(parse_sensitivity_list), |control| {
        control.unwrap_or(EventControl::None)
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, assignments) = alt((parse_block, many1(procedural_statement)))(input)?;

    let block = AlwaysBlock::new(event_control, assignments);

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
    use crate::parsers::helpers::{assert_parses, assert_parses_to};

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

    fn identifier_expression(name: &str) -> Expression {
        Expression::Identifier(name.into())
    }

    #[test]
    fn test_parse_sensitivity_list_edges() {
        assert_parses_to(
            parse_sensitivity_list,
            "@(posedge clk or negedge rst)",
            EventControl::Events(vec![
                Event::new(EventTriggers::PosEdge, identifier_expression("clk")),
                Event::new(EventTriggers::NegEdge, identifier_expression("rst")),
            ]),
        );
    }

    #[test]
    fn test_parse_sensitivity_list_levels() {
        let expected = EventControl::Events(vec![
            Event::new(EventTriggers::EitherEdge, identifier_expression("a")),
            Event::new(EventTriggers::EitherEdge, identifier_expression("b")),
        ]);
        assert_parses_to(parse_sensitivity_list, "@(a or b)", expected);

        assert_parses_to(
            parse_sensitivity_list,
            "@( a , b )",
            EventControl::Events(vec![
                Event::new(EventTriggers::EitherEdge, identifier_expression("a")),
                Event::new(EventTriggers::EitherEdge, identifier_expression("b")),
            ]),
        );
    }

    #[test]
    fn test_parse_sensitivity_list_wildcard() {
        assert_parses_to(parse_sensitivity_list, "@(*)", EventControl::Implicit);
        assert_parses_to(parse_sensitivity_list, "@( * )", EventControl::Implicit);
    }

    #[test]
    fn test_parse_sensitivity_list_single_event() {
        assert_parses_to(
            parse_sensitivity_list,
            "@(posedge clk)",
            EventControl::Events(vec![Event::new(
                EventTriggers::PosEdge,
                identifier_expression("clk"),
            )]),
        );
    }

    #[test]
    fn test_parse_always_block_with_sensitivity_list() {
        let block = assert_parses(
            parse_always_block,
            r#"always @(posedge clk or posedge rst) begin
                   count <= 4'b0000;
               end"#,
        );
        assert_eq!(
            block.event_control,
            EventControl::Events(vec![
                Event::new(EventTriggers::PosEdge, identifier_expression("clk")),
                Event::new(EventTriggers::PosEdge, identifier_expression("rst")),
            ])
        );
        assert_eq!(block.statements.len(), 1);
    }

    #[test]
    fn test_parse_always_block_single_statement_body() {
        let block = assert_parses(parse_always_block, "always @(*) a = b;");
        assert_eq!(block.event_control, EventControl::Implicit);
        assert_eq!(block.statements.len(), 1);
    }

    /// The three `always` forms are different constructs and must not share a
    /// representation.
    #[test]
    fn test_always_forms_are_distinguishable() {
        let implicit = assert_parses(parse_always_block, "always @(*) a = b;");
        let uncontrolled = assert_parses(parse_always_block, "always begin a = b; end");
        let edge_triggered = assert_parses(parse_always_block, "always @(posedge clk) a <= b;");

        assert_eq!(implicit.event_control, EventControl::Implicit);
        assert_eq!(uncontrolled.event_control, EventControl::None);
        assert_eq!(
            edge_triggered.event_control,
            EventControl::Events(vec![Event::new(
                EventTriggers::PosEdge,
                identifier_expression("clk")
            )])
        );

        assert_ne!(implicit.event_control, uncontrolled.event_control);
        assert_ne!(implicit.event_control, edge_triggered.event_control);
        assert_ne!(uncontrolled.event_control, edge_triggered.event_control);
    }

    /// A level-sensitive list is still an explicit list, distinct from `@(*)`.
    #[test]
    fn test_explicit_list_is_not_implicit() {
        let block = assert_parses(parse_always_block, "always @(a or b) c = a & b;");
        assert_ne!(block.event_control, EventControl::Implicit);
    }

    #[test]
    fn test_parse_if_statement_without_else() {
        let statement = assert_parses(parse_if_statement, "if (rst) count <= 0;");
        assert_eq!(statement.condition, identifier_expression("rst"));
        assert_eq!(statement.then_statements.len(), 1);
        assert_eq!(statement.else_statements, None);
    }

    #[test]
    fn test_parse_if_else_with_blocks() {
        let statement = assert_parses(
            parse_if_statement,
            r#"if (rst) begin
                   count <= 4'b0000;
               end else begin
                   count <= count + 1;
                   done <= 1'b1;
               end"#,
        );
        assert_eq!(statement.then_statements.len(), 1);
        assert_eq!(statement.else_statements.unwrap().len(), 2);
    }

    #[test]
    fn test_parse_else_if_chain() {
        let statement = assert_parses(
            parse_if_statement,
            r#"if (a == 0) begin
                   x <= 1;
               end else if (a == 1) begin
                   x <= 2;
               end else begin
                   x <= 3;
               end"#,
        );

        let else_statements = statement.else_statements.expect("expected an else branch");
        assert_eq!(else_statements.len(), 1);
        match &else_statements[0] {
            ProceduralStatements::If(nested) => {
                assert_eq!(nested.then_statements.len(), 1);
                assert!(nested.else_statements.is_some());
            }
            other => panic!("Expected a nested if statement, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_nested_if_inside_block() {
        let block = assert_parses(
            parse_block,
            r#"begin
                   counter <= counter + 1;
                   if (counter == 32'd50000000) begin
                       counter <= 32'b0;
                   end
               end"#,
        );
        assert_eq!(block.len(), 2);
    }

    #[test]
    fn test_parse_case_statement() {
        let statement = assert_parses(
            parse_case_statement,
            r#"case (state)
                   IDLE: begin
                       state <= TRANSFER;
                   end
                   TRANSFER: state <= DONE;
                   default: begin
                       state <= IDLE;
                   end
               endcase"#,
        );

        assert_eq!(statement.subject, identifier_expression("state"));
        assert_eq!(statement.items.len(), 3);
        assert_eq!(
            statement.items[0].label,
            CaseLabel::Expressions(vec![identifier_expression("IDLE")])
        );
        assert_eq!(statement.items[1].statements.len(), 1);
        assert_eq!(statement.items[2].label, CaseLabel::Default);
    }

    #[test]
    fn test_parse_case_statement_multiple_labels() {
        let statement = assert_parses(
            parse_case_statement,
            r#"case (op)
                   2'b00, 2'b01: result <= 0;
               endcase"#,
        );

        assert_eq!(statement.items.len(), 1);
        match &statement.items[0].label {
            CaseLabel::Expressions(labels) => assert_eq!(labels.len(), 2),
            other => panic!("Expected expression labels, got {:?}", other),
        }
    }

    #[test]
    fn test_case_statement_as_procedural_statement() {
        let statement = assert_parses(
            procedural_statement,
            r#"case (state)
                   IDLE: state <= DONE;
               endcase"#,
        );
        assert!(matches!(statement, ProceduralStatements::Case(_)));
    }

    #[test]
    fn test_sensitivity_list_or_is_not_an_identifier_prefix() {
        assert_parses_to(
            parse_sensitivity_list,
            "@(a or origin)",
            EventControl::Events(vec![
                Event::new(EventTriggers::EitherEdge, identifier_expression("a")),
                Event::new(EventTriggers::EitherEdge, identifier_expression("origin")),
            ]),
        );
    }

    #[test]
    fn test_case_label_default_is_not_an_identifier_prefix() {
        let statement = assert_parses(
            parse_case_statement,
            r#"case (state)
                   default_state: state <= IDLE;
               endcase"#,
        );
        assert_eq!(
            statement.items[0].label,
            CaseLabel::Expressions(vec![identifier_expression("default_state")])
        );
    }
}

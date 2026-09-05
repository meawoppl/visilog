use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, multispace1, satisfy},
    combinator::{map, not, opt, peek, value},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, preceded, terminated},
    IResult,
};

use crate::parsers::assignment::parse_assignment;

use super::{
    assignment::{assignment_lhs, ProceduralAssignment, ProceduralAssignmentType},
    delay::{parse_delay, parse_delay_statement, Delay},
    expr::{system_name, verilog_expression, Expression},
    identifier::{identifier, identifier_list, Identifier},
    simple::{range, signedness, ws, ws_and_comments},
    string::parse_verilog_string,
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

/// Which of the three `case` forms a statement is, i.e. how a label is
/// compared against the subject. The syntax is identical; only the comparison
/// differs.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CaseKind {
    /// `case` — an exact comparison, in which an `x` or `z` on either side
    /// never matches.
    Exact,
    /// `casez` — a `z` (or `?`) bit on either side matches any value.
    WildcardZ,
    /// `casex` — an `x` or `z` bit on either side matches any value.
    WildcardXz,
}

#[derive(Debug, PartialEq)]
pub struct CaseStatement {
    pub kind: CaseKind,
    pub subject: Expression,
    pub items: Vec<CaseItem>,
}

/// `for (i = 0; i < 4; i = i + 1) …` — the three header parts are an
/// initialising assignment, a condition and a stepping assignment. The two
/// assignments are *assignments*, not expressions: the `;` between the parts
/// belongs to the header, so they cannot carry one of their own.
#[derive(Debug, PartialEq)]
pub struct ForStatement {
    pub initializer: ProceduralAssignment,
    pub condition: Expression,
    pub step: ProceduralAssignment,
    pub statements: Vec<ProceduralStatements>,
}

/// `while (a) …` — the condition is re-evaluated before every iteration.
#[derive(Debug, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub statements: Vec<ProceduralStatements>,
}

/// `repeat (4) …` — the count is evaluated **once**, on entry, and never
/// again, so a body that moves one of its operands does not change how many
/// iterations are left.
#[derive(Debug, PartialEq)]
pub struct RepeatStatement {
    pub count: Expression,
    pub statements: Vec<ProceduralStatements>,
}

/// One argument of a system task call.
///
/// A format string is a plain string literal rather than an [`Expression`] —
/// the expression grammar has no string operand — and `$time` is a system
/// *function*, which is likewise not an expression operand.
#[derive(Debug, PartialEq)]
pub enum SystemTaskArgument {
    /// A double-quoted literal, as in `$display("PASSED")`.
    String(String),
    /// A nested system function, named without its `$`: `$display("%0d", $time)`.
    SystemFunction(String),
    /// An ordinary expression.
    Expression(Expression),
}

/// `$display("count = %0d", count);` — a system task call, named without its
/// `$`. Which names are meaningful is the simulator's business, not the
/// parser's.
#[derive(Debug, PartialEq)]
pub struct SystemTaskCall {
    pub name: String,
    pub arguments: Vec<SystemTaskArgument>,
}

#[derive(Debug, PartialEq)]
pub enum ProceduralStatements {
    Delay(Delay),
    /// `#5 a = 1;`, `#5 begin … end` — a statement prefixed by a delay.
    ///
    /// The delay belongs to the *statement*, not to any one statement kind, so
    /// it wraps a body rather than living as a field on an assignment. The
    /// body is a single statement or a `begin`…`end` block, which is why it is
    /// a list.
    Delayed {
        delay: Delay,
        statements: Vec<ProceduralStatements>,
    },
    Assignment(ProceduralAssignment),
    If(IfStatement),
    Case(CaseStatement),
    For(ForStatement),
    While(WhileStatement),
    Repeat(RepeatStatement),
    /// `forever …` — a body with an unconditional back-jump and nothing that
    /// ends it, so only a `#delay` in it lets time move.
    Forever(Vec<ProceduralStatements>),
    SystemTask(SystemTaskCall),
}

pub enum ProceduralBlock {
    InitialBlock(InitialBlock),
    AlwaysBlock(AlwaysBlock),
}

pub fn procedural_statement(input: &str) -> IResult<&str, ProceduralStatements> {
    alt((
        map(parse_if_statement, |i| ProceduralStatements::If(i)),
        map(parse_case_statement, |c| ProceduralStatements::Case(c)),
        // `for` is a prefix of `forever`, so the longer keyword is tried first.
        parse_forever_statement,
        map(parse_for_statement, |f| ProceduralStatements::For(f)),
        map(parse_while_statement, |w| ProceduralStatements::While(w)),
        map(parse_repeat_statement, |r| ProceduralStatements::Repeat(r)),
        map(parse_system_task, |t| ProceduralStatements::SystemTask(t)),
        map(parse_assignment, |a| ProceduralStatements::Assignment(a)),
        // `#5;` is a statement in its own right, so it is tried before the
        // prefix form, whose body would have nothing to match.
        map(parse_delay_statement, |d| ProceduralStatements::Delay(d)),
        parse_delayed_statement,
    ))(input)
}

/// `#5 <statement>` — a delay prefixing any procedural statement, including a
/// `begin`…`end` block, an `if` or a `case`.
fn parse_delayed_statement(input: &str) -> IResult<&str, ProceduralStatements> {
    let (input, delay) = ws(parse_delay)(input)?;
    let (input, statements) = statement_body(input)?;
    Ok((input, ProceduralStatements::Delayed { delay, statements }))
}

/// The body of a conditional or case arm: either a `begin`…`end` block or a
/// single statement.
fn statement_body(input: &str) -> IResult<&str, Vec<ProceduralStatements>> {
    alt((parse_block, map(procedural_statement, |s| vec![s])))(input)
}

fn parenthesized_expression(input: &str) -> IResult<&str, Expression> {
    delimited(ws(char('(')), verilog_expression, ws(char(')')))(input)
}

/// A bare `$name` argument: `$display("%0d", $time)`.
///
/// A `$name` that *is* followed by an argument list is a system function call
/// and belongs to the expression grammar — `$display("%0d", $signed(a))` —
/// so this form stops at the parenthesis and lets the expression layer take it.
fn bare_system_function(input: &str) -> IResult<&str, String> {
    terminated(system_name, peek(not(char('('))))(input)
}

fn system_task_argument(input: &str) -> IResult<&str, SystemTaskArgument> {
    alt((
        map(parse_verilog_string, SystemTaskArgument::String),
        map(bare_system_function, SystemTaskArgument::SystemFunction),
        map(verilog_expression, SystemTaskArgument::Expression),
    ))(input)
}

/// `$display("a = %0d", a);`, `$finish;` — a system task call as a statement.
/// The argument list is optional, and may be empty.
pub fn parse_system_task(input: &str) -> IResult<&str, SystemTaskCall> {
    let (input, name) = ws(system_name)(input)?;
    let (input, arguments) = opt(delimited(
        ws(char('(')),
        separated_list0(char(','), ws(system_task_argument)),
        ws(char(')')),
    ))(input)?;
    let (input, _) = ws(char(';'))(input)?;

    Ok((
        input,
        SystemTaskCall {
            name,
            arguments: arguments.unwrap_or_default(),
        },
    ))
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

/// The keyword that opens a `case` statement. `case` is a prefix of both
/// wildcard forms, so it is tried last.
fn parse_case_keyword(input: &str) -> IResult<&str, CaseKind> {
    ws(alt((
        value(CaseKind::WildcardZ, tag("casez")),
        value(CaseKind::WildcardXz, tag("casex")),
        value(CaseKind::Exact, tag("case")),
    )))(input)
}

pub fn parse_case_statement(input: &str) -> IResult<&str, CaseStatement> {
    let (input, kind) = parse_case_keyword(input)?;
    let (input, subject) = parenthesized_expression(input)?;
    let (input, items) = many1(parse_case_item)(input)?;
    let (input, _) = ws(tag("endcase"))(input)?;

    Ok((
        input,
        CaseStatement {
            kind,
            subject,
            items,
        },
    ))
}

/// A character that could continue an identifier, and so cannot immediately
/// follow a keyword.
fn identifier_char(input: &str) -> IResult<&str, char> {
    satisfy(|c: char| c.is_alphanumeric() || c == '_' || c == '$')(input)
}

/// A keyword token, with the leading whitespace skipped and a word boundary
/// after it.
///
/// `if` and `case` are always followed by punctuation, so they need no
/// boundary; `forever`, `while` and `repeat` are followed by a statement, so
/// without one `forever_more = 1;` would read as `forever` plus an assignment.
/// The trailing whitespace is deliberately left for the caller: skipping it
/// here would put the boundary check on the wrong side of it.
fn keyword<'a>(input: &'a str, word: &str) -> IResult<&'a str, ()> {
    let (input, _) = ws_and_comments(input)?;
    let (input, _) = tag(word)(input)?;
    let (input, _) = peek(not(identifier_char))(input)?;
    Ok((input, ()))
}

/// The assignment in a `for` header: `i = 0`, `i = i + 1`. It is not a
/// statement and so carries no `;` of its own — the two separators belong to
/// the header.
fn for_assignment(input: &str) -> IResult<&str, ProceduralAssignment> {
    let (input, lhs) = ws(assignment_lhs)(input)?;
    let (input, operator) = ws(alt((tag("<="), tag("="))))(input)?;
    let (input, rhs) = verilog_expression(input)?;

    let assignment_type = match operator {
        "<=" => ProceduralAssignmentType::NonBlocking,
        _ => ProceduralAssignmentType::Blocking,
    };

    Ok((
        input,
        ProceduralAssignment::new(lhs, assignment_type, None, rhs),
    ))
}

/// `for (i = 0; i < 4; i = i + 1) <statement>`.
pub fn parse_for_statement(input: &str) -> IResult<&str, ForStatement> {
    let (input, _) = keyword(input, "for")?;
    let (input, _) = ws(char('('))(input)?;
    let (input, initializer) = for_assignment(input)?;
    let (input, _) = ws(char(';'))(input)?;
    let (input, condition) = verilog_expression(input)?;
    let (input, _) = ws(char(';'))(input)?;
    let (input, step) = for_assignment(input)?;
    let (input, _) = ws(char(')'))(input)?;
    let (input, statements) = statement_body(input)?;

    Ok((
        input,
        ForStatement {
            initializer,
            condition,
            step,
            statements,
        },
    ))
}

/// `while (a) <statement>`.
pub fn parse_while_statement(input: &str) -> IResult<&str, WhileStatement> {
    let (input, _) = keyword(input, "while")?;
    let (input, condition) = parenthesized_expression(input)?;
    let (input, statements) = statement_body(input)?;

    Ok((
        input,
        WhileStatement {
            condition,
            statements,
        },
    ))
}

/// `repeat (4) <statement>`.
pub fn parse_repeat_statement(input: &str) -> IResult<&str, RepeatStatement> {
    let (input, _) = keyword(input, "repeat")?;
    let (input, count) = parenthesized_expression(input)?;
    let (input, statements) = statement_body(input)?;

    Ok((input, RepeatStatement { count, statements }))
}

/// `forever <statement>`.
fn parse_forever_statement(input: &str) -> IResult<&str, ProceduralStatements> {
    let (input, _) = keyword(input, "forever")?;
    let (input, statements) = statement_body(input)?;

    Ok((input, ProceduralStatements::Forever(statements)))
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

/// One variable a `function` declares: an argument or a body-local.
///
/// A function's *own name* is one of these too — it is the variable the body
/// assigns to return a value — which is why the return width and the width of
/// a local are described by the same thing.
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionVariable {
    pub name: Identifier,
    pub range: (i64, i64),
    pub signed: bool,
}

/// `function [7:0] do_add; input [7:0] a; do_add = a + 1; endfunction`
///
/// The arguments may be written either the 1995 way, as `input` declarations
/// *inside* the body, or the 2001 way, as a parenthesised list after the name.
/// Both fill [`arguments`](FunctionDeclaration::arguments) in call order, so
/// nothing downstream can tell them apart.
#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    /// The width of the value the function returns, which is the width of the
    /// variable its own name stands for.
    pub range: (i64, i64),
    pub signed: bool,
    pub arguments: Vec<FunctionVariable>,
    /// Body-local `reg` and `integer` declarations.
    pub locals: Vec<FunctionVariable>,
    pub statements: Vec<ProceduralStatements>,
}

/// The width and signedness a declaration spells out, and whether it spelled
/// out anything at all.
///
/// `explicit` is what tells a declaration apart from a statement — a function
/// item that names neither a direction nor a type is not a declaration — and
/// what makes the 2001 argument list's `f(input [7:0] a, b)` give `b` the type
/// of the element before it.
#[derive(Debug, PartialEq, Clone, Copy)]
struct DeclaredType {
    range: (i64, i64),
    signed: bool,
    explicit: bool,
}

impl Default for DeclaredType {
    fn default() -> Self {
        DeclaredType {
            range: (0, 0),
            signed: false,
            explicit: false,
        }
    }
}

/// The type part of a variable declaration: an optional storage keyword, an
/// optional `signed`, and either an `integer` or a range.
///
/// `integer` is written *instead of* a range and carries its own width and
/// signedness, so a declaration never has both.
fn declared_type(input: &str) -> IResult<&str, DeclaredType> {
    let (input, storage) = opt(alt((
        |i| keyword(i, "reg"),
        |i| keyword(i, "wire"),
        |i| keyword(i, "time"),
    )))(input)?;
    let (input, integer) = opt(|i| keyword(i, "integer"))(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, signed) = signedness(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, declared) = opt(range)(input)?;

    Ok((
        input,
        DeclaredType {
            range: match (integer.is_some(), declared) {
                (true, _) => (31, 0),
                (false, Some(declared)) => declared,
                (false, None) => (0, 0),
            },
            // An `integer` is signed by being an `integer`.
            signed: signed || integer.is_some(),
            explicit: storage.is_some() || integer.is_some() || declared.is_some() || signed,
        },
    ))
}

/// The `input` keyword that marks a function argument.
///
/// A function has inputs and nothing else — its result is its name — so an
/// `output` or `inout` in one is not a function this parser knows how to read.
fn function_input(input: &str) -> IResult<&str, ()> {
    keyword(input, "input")
}

/// One item inside a function body: `input [7:0] a;`, `reg [3:0] tmp;`,
/// `integer i;`. The `bool` is whether it is an argument.
fn function_item(input: &str) -> IResult<&str, (bool, Vec<FunctionVariable>)> {
    let (input, argument) = opt(function_input)(input)?;
    let (input, declared) = declared_type(input)?;
    // Neither a direction nor a type: this is a statement, not a declaration.
    if argument.is_none() && !declared.explicit {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }
    let (input, names) = identifier_list(input)?;
    let (input, _) = ws(char(';'))(input)?;

    Ok((
        input,
        (
            argument.is_some(),
            names
                .into_iter()
                .map(|name| FunctionVariable {
                    name,
                    range: declared.range,
                    signed: declared.signed,
                })
                .collect(),
        ),
    ))
}

/// One element of a 2001 argument list: `input [7:0] a`, or a bare `b` that
/// inherits the element before it.
fn ansi_function_argument(input: &str) -> IResult<&str, (DeclaredType, Identifier)> {
    let (input, _) = opt(function_input)(input)?;
    let (input, declared) = declared_type(input)?;
    let (input, name) = ws(identifier)(input)?;
    Ok((input, (declared, name)))
}

/// `(input [7:0] a, b, input c)` — the 2001 argument list. An element that
/// declares neither a direction nor a type takes both from the element before
/// it, which is what makes `a` and `b` above the same width.
fn ansi_function_arguments(input: &str) -> IResult<&str, Vec<FunctionVariable>> {
    let (input, elements) = delimited(
        ws(char('(')),
        separated_list0(char(','), ws(ansi_function_argument)),
        ws(char(')')),
    )(input)?;

    let mut inherited = DeclaredType::default();
    let mut arguments = Vec::with_capacity(elements.len());
    for (declared, name) in elements {
        if declared.explicit {
            inherited = declared;
        }
        arguments.push(FunctionVariable {
            name,
            range: inherited.range,
            signed: inherited.signed,
        });
    }
    Ok((input, arguments))
}

/// `function [range] name; <declarations> <statements> endfunction`.
///
/// The return type may be a range, an `integer`, or nothing at all — a
/// function that declares no width returns one bit.
pub fn parse_function_declaration(input: &str) -> IResult<&str, FunctionDeclaration> {
    let (input, _) = keyword(input, "function")?;
    // `automatic` says a call gets its own copy of the locals, which is what a
    // frame per call already gives every function here.
    let (input, _) = opt(|i| keyword(i, "automatic"))(input)?;
    let (input, returns) = declared_type(input)?;
    let (input, name) = ws(identifier)(input)?;
    let (input, ansi) = opt(ansi_function_arguments)(input)?;
    let (input, _) = ws(char(';'))(input)?;
    let (input, items) = many0(function_item)(input)?;
    // The LRM allows one statement, which is a `begin`…`end` block when the
    // body does more than one thing; `many0` also lets an empty function be
    // written, and a body that runs several statements without a block.
    let (input, statements) = alt((parse_block, many0(procedural_statement)))(input)?;
    let (input, _) = ws(tag("endfunction"))(input)?;

    let mut arguments = ansi.unwrap_or_default();
    let mut locals = Vec::new();
    for (is_argument, variables) in items {
        if is_argument {
            arguments.extend(variables);
        } else {
            locals.extend(variables);
        }
    }

    Ok((
        input,
        FunctionDeclaration {
            name,
            range: returns.range,
            signed: returns.signed,
            arguments,
            locals,
            statements,
        },
    ))
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
    fn test_parse_casez_statement() {
        let statement = assert_parses(
            parse_case_statement,
            r#"casez (a)
                   2'b0?: b = 1;
                   2'b1?, 2'b?1: b = 2;
                   default: b = 0;
               endcase"#,
        );

        assert_eq!(statement.kind, CaseKind::WildcardZ);
        assert_eq!(statement.items.len(), 3);
        match &statement.items[1].label {
            CaseLabel::Expressions(labels) => assert_eq!(labels.len(), 2),
            other => panic!("Expected expression labels, got {:?}", other),
        }
        assert_eq!(statement.items[2].label, CaseLabel::Default);
    }

    #[test]
    fn test_parse_casex_statement() {
        let statement = assert_parses(
            parse_case_statement,
            r#"casex (a)
                   2'b1x: b = 1;
                   default: b = 0;
               endcase"#,
        );

        assert_eq!(statement.kind, CaseKind::WildcardXz);
        assert_eq!(statement.subject, identifier_expression("a"));
        assert_eq!(statement.items.len(), 2);
    }

    #[test]
    fn test_plain_case_keeps_its_exact_kind() {
        let statement = assert_parses(
            parse_case_statement,
            r#"case (a)
                   1: b = 1;
               endcase"#,
        );
        assert_eq!(statement.kind, CaseKind::Exact);
    }

    #[test]
    fn test_wildcard_case_statements_are_procedural_statements() {
        for source in [
            "casez (a) 2'b0?: b = 1; endcase",
            "casex (a) 2'b1x: b = 1; endcase",
        ] {
            let statement = assert_parses(procedural_statement, source);
            assert!(matches!(statement, ProceduralStatements::Case(_)));
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

    #[test]
    fn test_parse_system_task_with_a_format_string_and_arguments() {
        let call = assert_parses(parse_system_task, r#"$display("a = %0d", a, $time);"#);
        assert_eq!(call.name, "display");
        assert_eq!(
            call.arguments,
            vec![
                SystemTaskArgument::String("a = %0d".to_string()),
                SystemTaskArgument::Expression(identifier_expression("a")),
                SystemTaskArgument::SystemFunction("time".to_string()),
            ]
        );
    }

    #[test]
    fn test_parse_system_task_argument_lists_that_are_absent_or_empty() {
        assert_eq!(
            assert_parses(parse_system_task, "$finish;").arguments,
            vec![]
        );
        assert_eq!(
            assert_parses(parse_system_task, "$display ( ) ;").arguments,
            vec![]
        );
    }

    #[test]
    fn test_system_task_is_a_procedural_statement_anywhere_a_statement_is() {
        for source in [
            r#"$display("hi");"#,
            r#"if (a) $display("hi"); else $write("bye");"#,
        ] {
            assert!(
                procedural_statement(source).is_ok(),
                "did not parse: {}",
                source
            );
        }

        let statements = assert_parses(parse_block, r#"begin a = 'b1; $display("%b", a); end"#);
        assert!(matches!(statements[1], ProceduralStatements::SystemTask(_)));
    }

    /// A delay prefixes a *statement*, not an assignment, so every statement
    /// form can carry one.
    #[test]
    fn test_a_delay_prefixes_any_procedural_statement() {
        for source in [
            "#5 a = 1;",
            "#5 a <= 1;",
            r#"#5 $display("x");"#,
            "#5 begin a = 1; b = 2; end",
            "#5 if (a) b = 1;",
            "#5 if (a) b = 1; else b = 0;",
            "#5 case (a) 1: b = 1; default: b = 0; endcase",
            "# 5 a = 1;",
            "#/* later */5 a = 1;",
            "#5 #3 a = 1;",
        ] {
            let statement = assert_parses(procedural_statement, source);
            assert!(
                matches!(statement, ProceduralStatements::Delayed { .. }),
                "{} should be a delayed statement, got {:?}",
                source,
                statement
            );
        }
    }

    #[test]
    fn test_a_delayed_block_keeps_every_statement_in_it() {
        let statement = assert_parses(procedural_statement, "#5 begin a = 1; b = 2; end");
        match statement {
            ProceduralStatements::Delayed { delay, statements } => {
                assert_eq!(delay, Delay::new(5));
                assert_eq!(statements.len(), 2);
            }
            other => panic!("expected a delayed statement, got {:?}", other),
        }
    }

    /// `#5;` waits and does nothing else, which is a different statement from
    /// `#5 <something>`.
    #[test]
    fn test_a_bare_delay_is_not_a_delayed_statement() {
        for source in ["#5;", "# 12 ;", "#0;"] {
            let statement = assert_parses(procedural_statement, source);
            assert!(
                matches!(statement, ProceduralStatements::Delay(_)),
                "{} should be a bare delay, got {:?}",
                source,
                statement
            );
        }
    }

    /// The delay nested in the arm is what a statement-index resume point
    /// could not address; the parser has to keep it inside the arm.
    #[test]
    fn test_a_delay_nests_inside_an_if_and_a_case_arm() {
        let statement = assert_parses(procedural_statement, "if (a) #5 b = 1; else #7 b = 0;");
        let ProceduralStatements::If(conditional) = statement else {
            panic!("expected an if statement");
        };
        assert!(matches!(
            conditional.then_statements[0],
            ProceduralStatements::Delayed { .. }
        ));
        let else_statements = conditional
            .else_statements
            .expect("expected an else branch");
        assert!(matches!(
            else_statements[0],
            ProceduralStatements::Delayed { .. }
        ));

        let statement = assert_parses(
            procedural_statement,
            "case (a) 1: #5 b = 1; default: #7 b = 0; endcase",
        );
        let ProceduralStatements::Case(case) = statement else {
            panic!("expected a case statement");
        };
        for item in &case.items {
            assert!(matches!(
                item.statements[0],
                ProceduralStatements::Delayed { .. }
            ));
        }
    }

    #[test]
    fn test_whitespace_between_a_hash_and_its_value_in_a_block() {
        let statements = assert_parses(parse_block, "begin # 3 a = 1; # 4 ; end");
        assert_eq!(statements.len(), 2);
    }

    #[test]
    fn test_a_dollar_name_is_still_not_an_ordinary_identifier() {
        // The `$` is a token of its own, not a loosening of `identifier`: a
        // `$name` names the simulator, so it parses as a system function call
        // and never as a signal, and it is not a target an assignment can
        // drive.
        assert!(crate::parsers::identifier::identifier("$time").is_err());
        assert_parses_to(
            verilog_expression,
            "$time",
            Expression::SystemFunctionCall("time".to_string(), vec![]),
        );
        assert!(procedural_statement("$display = 1;").is_err());
    }

    /// Every loop form, with a single-statement body and with a `begin`…`end`
    /// one.
    #[test]
    fn test_parse_every_loop_form() {
        let inputs = vec![
            "for (i = 0; i < 4; i = i + 1) a = i;",
            "for (i = 0; i < 4; i = i + 1) begin a = i; b = i; end",
            "while (a) b = 1;",
            "while (a) begin b = 1; a = 0; end",
            "repeat (4) a = 1;",
            "repeat (n + 1) begin a = 1; end",
            "forever a = 1;",
            "forever begin #5 a = ~a; end",
        ];

        for input in inputs {
            assert_parses(procedural_statement, input);
        }
    }

    #[test]
    fn test_for_header_is_two_assignments_and_a_condition() {
        let statement = assert_parses(parse_for_statement, "for (i = 0; i < 4; i = i + 1) a = i;");

        assert_eq!(statement.initializer.lhs().to_contracted_string(), "i");
        assert_eq!(statement.initializer.rhs().to_contracted_string(), "0");
        assert_eq!(statement.condition.to_contracted_string(), "i < 4");
        assert_eq!(statement.step.lhs().to_contracted_string(), "i");
        assert_eq!(statement.step.rhs().to_contracted_string(), "i + 1");
        assert_eq!(statement.statements.len(), 1);
    }

    /// The header separators belong to the header, so a `;` after the step is
    /// not part of it — `for (i = 0; i < 4; i = i + 1;)` is not a `for` loop.
    #[test]
    fn test_a_for_header_assignment_carries_no_semicolon() {
        assert!(parse_for_statement("for (i = 0; i < 4; i = i + 1;) a = i;").is_err());
    }

    #[test]
    fn test_loops_nest() {
        let statement = assert_parses(
            parse_for_statement,
            "for (i = 0; i < 4; i = i + 1) begin repeat (2) while (go) a = a + 1; end",
        );

        let ProceduralStatements::Repeat(repeat) = &statement.statements[0] else {
            panic!("expected a repeat, got {:?}", statement.statements[0]);
        };
        assert!(matches!(
            repeat.statements[0],
            ProceduralStatements::While(_)
        ));
    }

    /// `for` is a prefix of `forever`, and both are prefixes of an identifier
    /// that starts with them. The `(` after `for` and the word boundary after
    /// `forever` are what keep the three apart.
    #[test]
    fn test_a_loop_keyword_does_not_swallow_a_longer_identifier() {
        assert!(matches!(
            assert_parses(procedural_statement, "forever_more = 1;"),
            ProceduralStatements::Assignment(_)
        ));
        assert!(matches!(
            assert_parses(procedural_statement, "format = 1;"),
            ProceduralStatements::Assignment(_)
        ));
        assert!(matches!(
            assert_parses(procedural_statement, "repeat_count = 1;"),
            ProceduralStatements::Assignment(_)
        ));
        assert!(matches!(
            assert_parses(procedural_statement, "while_ready = 1;"),
            ProceduralStatements::Assignment(_)
        ));
        assert!(matches!(
            assert_parses(procedural_statement, "forever a = 1;"),
            ProceduralStatements::Forever(_)
        ));
    }

    #[test]
    fn test_a_loop_is_a_legal_statement_inside_a_block() {
        let statements = assert_parses(
            parse_block,
            r#"begin
                total = 0;
                for (i = 0; i < 4; i = i + 1) total = total + i;
                forever #5 clk = ~clk;
            end"#,
        );

        assert_eq!(statements.len(), 3);
        assert!(matches!(statements[1], ProceduralStatements::For(_)));
        assert!(matches!(statements[2], ProceduralStatements::Forever(_)));
    }

    /// Comments are legal wherever a token boundary is, and a loop header is
    /// all token boundaries.
    #[test]
    fn test_comments_inside_a_loop_header() {
        assert_parses(
            procedural_statement,
            "for /*a*/ ( /*b*/ i = 0 /*c*/ ; /*d*/ i < 4 ; i = i + 1 ) /*e*/ a = i;",
        );
        assert_parses(procedural_statement, "repeat /*n*/ (4) a = 1;");
    }

    /// The 1995 form: the arguments are `input` declarations *inside* the
    /// body, and the function returns by assigning to its own name.
    #[test]
    fn test_parse_function_declaration_1995_style() {
        let function = assert_parses(
            parse_function_declaration,
            "function [7:0] do_add; input [7:0] a; do_add = a + 1; endfunction",
        );

        assert_eq!(function.name, "do_add".into());
        assert_eq!(function.range, (7, 0));
        assert_eq!(function.arguments.len(), 1);
        assert_eq!(function.arguments[0].name, "a".into());
        assert_eq!(function.arguments[0].range, (7, 0));
        assert!(function.locals.is_empty());
        assert_eq!(function.statements.len(), 1);
    }

    /// The 2001 form puts the same arguments in a header list, and an element
    /// that declares no type of its own takes the one before it.
    #[test]
    fn test_parse_function_declaration_2001_style() {
        let function = assert_parses(
            parse_function_declaration,
            "function [3:0] pick(input [3:0] a, b); pick = a & b; endfunction",
        );

        assert_eq!(function.arguments.len(), 2);
        assert_eq!(function.arguments[0].name, "a".into());
        assert_eq!(function.arguments[1].name, "b".into());
        assert!(function.arguments.iter().all(|a| a.range == (3, 0)));
    }

    /// A body-local variable is a declaration, not a statement, and is kept
    /// apart from the arguments.
    #[test]
    fn test_parse_function_locals_and_return_types() {
        let function = assert_parses(
            parse_function_declaration,
            r#"function integer count_ones;
                   input [3:0] value;
                   integer i;
                   reg [3:0] seen;
                   begin
                       count_ones = 0;
                       for (i = 0; i < 4; i = i + 1) count_ones = count_ones + value[i];
                   end
               endfunction"#,
        );

        // `integer` is a 32 bit signed variable, written instead of a range.
        assert_eq!(function.range, (31, 0));
        assert!(function.signed);
        assert_eq!(function.arguments.len(), 1);
        assert_eq!(
            function
                .locals
                .iter()
                .map(|local| local.name.name.as_str())
                .collect::<Vec<_>>(),
            vec!["i", "seen"]
        );
        assert_eq!(function.locals[0].range, (31, 0));
        assert_eq!(function.locals[1].range, (3, 0));
    }

    /// A function that declares no width returns one bit, and one that
    /// declares no arguments takes none.
    #[test]
    fn test_parse_function_declaration_minimal() {
        let function = assert_parses(parse_function_declaration, "function f; f = 1; endfunction");

        assert_eq!(function.range, (0, 0));
        assert!(function.arguments.is_empty());
        assert_eq!(function.statements.len(), 1);
    }

    /// A `signed` function and a `signed` argument keep the qualifier.
    #[test]
    fn test_parse_function_signedness() {
        let function = assert_parses(
            parse_function_declaration,
            "function signed [7:0] neg; input signed [7:0] a; neg = -a; endfunction",
        );

        assert!(function.signed);
        assert!(function.arguments[0].signed);
    }
}

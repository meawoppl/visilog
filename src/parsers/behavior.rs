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
    assignment::{
        assignment_lhs, AssignmentTiming, ProceduralAssignment, ProceduralAssignmentType,
    },
    constants::VerilogConstant,
    delay::{parse_delay, parse_delay_statement, Delay},
    expr::{system_name, verilog_expression, Expression},
    identifier::{identifier, identifier_list, Identifier},
    keywords::is_reserved_word,
    simple::{range, signedness, ws, ws_and_comments, Range},
    string::parse_verilog_string,
};
#[derive(Debug, PartialEq, Clone)]
pub enum EventTriggers {
    PosEdge,
    NegEdge,
    EitherEdge,
}

#[derive(Debug, PartialEq, Clone)]
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
#[derive(Debug, PartialEq, Clone)]
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

/// `begin : name … end` or `fork : name … join` — a block of statements,
/// optionally named.
///
/// A name is what makes the block a scope of its own: only a named block may
/// declare variables, and [`locals`](BlockStatement::locals) is always empty
/// for an unnamed one. The name also spells those variables in the flat store,
/// `block_id.tmp` the way a task's are `load.data`.
#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    /// `None` for a plain `begin`…`end` nested inside another block.
    pub name: Option<Identifier>,
    /// The variables the block declares, which only a named block may have.
    pub locals: Vec<FunctionVariable>,
    /// The statements: run in order for a `begin`, one branch each for a
    /// `fork`.
    pub statements: Vec<ProceduralStatements>,
}

/// `wait (expr) statement` — the statement runs once the expression is true.
///
/// It suspends the block the way a `#delay` does, but is resumed by a *value*
/// rather than by time: a `wait` whose condition is already true does not
/// suspend at all.
#[derive(Debug, PartialEq)]
pub struct WaitStatement {
    pub condition: Expression,
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
    /// A slot with nothing in it: the gap in `$display("a",, b)`.
    ///
    /// Not a no-op — it renders as exactly one space, which is how a design
    /// separates two values without a format string. It has to be a variant
    /// rather than an absence, because dropping it would silently close the
    /// gap it exists to make.
    Empty,
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
    /// `assign v = e;` written *inside* a procedural block — a procedural
    /// continuous assignment, which is a different construct from the
    /// module-level `assign` that
    /// [`ContinuousAssignment`](crate::parsers::assignment::ContinuousAssignment)
    /// carries. It installs a continuous drive on `v` that overrides ordinary
    /// procedural writes until a `deassign`.
    Assign {
        target: Expression,
        value: Expression,
    },
    /// `deassign v;` — removes the drive an `assign` installed. Whatever value
    /// it last produced stays until something else writes it.
    Deassign(Expression),
    /// `force v = e;` — a continuous drive that overrides *everything*,
    /// including a procedural continuous assignment, until a `release`.
    Force {
        target: Expression,
        value: Expression,
    },
    /// `release v;` — drops the `force`.
    Release(Expression),
    If(IfStatement),
    Case(CaseStatement),
    For(ForStatement),
    While(WhileStatement),
    Repeat(RepeatStatement),
    /// `forever …` — a body with an unconditional back-jump and nothing that
    /// ends it, so only a `#delay` in it lets time move.
    Forever(Vec<ProceduralStatements>),
    SystemTask(SystemTaskCall),
    /// `begin … end`, with or without a name of its own.
    Block(BlockStatement),
    /// `fork … join` — the statements are branches that run concurrently and
    /// the block continues once every one of them has finished.
    Fork(BlockStatement),
    /// `wait (expr) statement` — suspend until the expression is true.
    Wait(WaitStatement),
    /// `@(posedge clk) statement` — an event control written in front of a
    /// statement rather than in front of a whole `always` block.
    EventControlled {
        control: EventControl,
        statements: Vec<ProceduralStatements>,
    },
    /// `my_task(a, b);` or a bare `my_task;` — a task enable.
    ///
    /// A task returns nothing, so this is a statement rather than an
    /// [`Expression`]: the values it produces come back through its `output`
    /// and `inout` arguments.
    TaskEnable {
        name: Identifier,
        arguments: Vec<Expression>,
    },
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
        map(parse_wait_statement, |w| ProceduralStatements::Wait(w)),
        // A block nested inside another one is a statement like any other, and
        // a named one is a scope with variables of its own.
        map(sequential_block, ProceduralStatements::Block),
        map(parallel_block, ProceduralStatements::Fork),
        map(parse_system_task, |t| ProceduralStatements::SystemTask(t)),
        parse_event_trigger,
        parse_event_controlled_statement,
        // The four keyword-led drive statements. They cannot be confused with
        // an ordinary assignment — `assign v = 2;` reads as the identifier
        // `assign` followed by `v`, which is not an assignment at all — but
        // they are keyword-led, so they belong with the rest of that family.
        parse_procedural_drive,
        parse_procedural_undrive,
        map(parse_assignment, |a| ProceduralStatements::Assignment(a)),
        // `#5;` is a statement in its own right, so it is tried before the
        // prefix form, whose body would have nothing to match.
        map(parse_delay_statement, |d| ProceduralStatements::Delay(d)),
        parse_delayed_statement,
        parse_task_enable,
    ))(input)
}

/// `-> e;` — trigger a named event.
///
/// It is lowered to a blocking assignment to the event's name rather than to a
/// statement kind of its own, because a trigger and a write then reach the
/// simulator down the same path: `exec::resolve_target` asks the store what a
/// name is, and only the store knows this one was declared `event`. The value
/// assigned is never read. Assigning to an event is illegal Verilog, so
/// nothing that was already legal is given a second meaning here.
fn parse_event_trigger(input: &str) -> IResult<&str, ProceduralStatements> {
    let (input, _) = ws(tag("->"))(input)?;
    let (input, name) = ws(identifier)(input)?;
    let (input, _) = ws(char(';'))(input)?;

    Ok((
        input,
        ProceduralStatements::Assignment(ProceduralAssignment::new(
            Expression::Identifier(name),
            ProceduralAssignmentType::Blocking,
            None,
            Expression::Constant(VerilogConstant::from_int(1)),
        )),
    ))
}

/// `my_task(a, b);`, or a bare `my_task;` for a task that takes no arguments.
///
/// It is tried after every other statement form because a bare identifier
/// followed by `;` is the loosest shape a statement has: everything else is
/// led by a keyword, by a `$name`, or by an assignment's `=`.
fn parse_task_enable(input: &str) -> IResult<&str, ProceduralStatements> {
    let (input, name) = ws(identifier)(input)?;
    // `wait (a);` has exactly this shape, and so does every statement form the
    // grammar has yet to learn. A task's name is an identifier, and a reserved
    // word is not one.
    if is_reserved_word(&name.name) {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }
    let (input, arguments) = opt(delimited(
        ws(char('(')),
        separated_list0(char(','), ws(verilog_expression)),
        ws(char(')')),
    ))(input)?;
    let (input, _) = ws(char(';'))(input)?;

    Ok((
        input,
        ProceduralStatements::TaskEnable {
            name,
            arguments: arguments.unwrap_or_default(),
        },
    ))
}

/// `#5 <statement>` — a delay prefixing any procedural statement, including a
/// `begin`…`end` block, an `if` or a `case`.
fn parse_delayed_statement(input: &str) -> IResult<&str, ProceduralStatements> {
    let (input, delay) = ws(parse_delay)(input)?;
    let (input, statements) = statement_body(input)?;
    Ok((input, ProceduralStatements::Delayed { delay, statements }))
}

/// A bare `;` — the null statement, which does nothing.
///
/// It is deliberately **not** a [`ProceduralStatements`] variant: it compiles
/// to no instructions at all, so the places that admit one produce an empty
/// statement list instead and nothing downstream has to learn a node that
/// means "nothing". The parser consumes the `;`, so a `many0` over it always
/// makes progress.
fn null_statement(input: &str) -> IResult<&str, ()> {
    value((), ws(char(';')))(input)
}

/// `assign v = e;` or `force v = e;` — the two procedural statements that
/// install a continuous drive on a variable.
///
/// The grammar is the module-level `assign`'s, but the statement is a
/// different construct: this one is executed when the block reaches it, and it
/// overrides what ordinary procedural assignments to `v` do until it is taken
/// away again.
fn parse_procedural_drive(input: &str) -> IResult<&str, ProceduralStatements> {
    let (input, forced) = alt((
        value(false, |i| keyword(i, "assign")),
        value(true, |i| keyword(i, "force")),
    ))(input)?;
    let (input, target) = ws(assignment_lhs)(input)?;
    let (input, _) = ws(char('='))(input)?;
    let (input, driver) = verilog_expression(input)?;
    let (input, _) = ws(char(';'))(input)?;

    Ok((
        input,
        if forced {
            ProceduralStatements::Force {
                target,
                value: driver,
            }
        } else {
            ProceduralStatements::Assign {
                target,
                value: driver,
            }
        },
    ))
}

/// `deassign v;` or `release v;` — the two statements that take a drive away.
fn parse_procedural_undrive(input: &str) -> IResult<&str, ProceduralStatements> {
    let (input, forced) = alt((
        value(false, |i| keyword(i, "deassign")),
        value(true, |i| keyword(i, "release")),
    ))(input)?;
    let (input, target) = ws(assignment_lhs)(input)?;
    let (input, _) = ws(char(';'))(input)?;

    Ok((
        input,
        if forced {
            ProceduralStatements::Release(target)
        } else {
            ProceduralStatements::Deassign(target)
        },
    ))
}

/// The body of a conditional or case arm: a `begin`…`end` block, a null
/// statement (`else ;`), or a single statement.
fn statement_body(input: &str) -> IResult<&str, Vec<ProceduralStatements>> {
    alt((
        parse_block,
        map(null_statement, |_| Vec::new()),
        map(procedural_statement, |s| vec![s]),
    ))(input)
}

/// A run of statements, with null statements dropped.
///
/// Each alternative consumes at least its own `;`, so the `many0` cannot spin.
fn statement_run(input: &str) -> IResult<&str, Vec<ProceduralStatements>> {
    map(
        many0(alt((
            map(null_statement, |_| None),
            map(procedural_statement, Some),
        ))),
        |statements| statements.into_iter().flatten().collect(),
    )(input)
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

/// One argument, or the empty slot between two commas.
///
/// The empty alternative is last and matches without consuming anything, so it
/// only wins where nothing else could — which is exactly the `,,` gap and the
/// `$display(,)` edge case.
fn system_task_argument(input: &str) -> IResult<&str, SystemTaskArgument> {
    alt((
        map(parse_verilog_string, SystemTaskArgument::String),
        map(bare_system_function, SystemTaskArgument::SystemFunction),
        map(verilog_expression, SystemTaskArgument::Expression),
        |rest| Ok((rest, SystemTaskArgument::Empty)),
    ))(input)
}

/// `$display("a = %0d", a);`, `$finish;` — a system task call as a statement.
/// The argument list is optional, and may be empty.
pub fn parse_system_task(input: &str) -> IResult<&str, SystemTaskCall> {
    let (input, name) = ws(system_name)(input)?;
    // An empty *list* — `$finish()` — is told from an empty *argument* by the
    // `)` arriving first. Without that, `system_task_argument`'s always-matching
    // empty alternative would read `()` as one blank argument and print a space.
    let (input, arguments) = opt(preceded(
        ws(char('(')),
        alt((
            map(ws(char(')')), |_| Vec::new()),
            terminated(
                separated_list1(char(','), ws(system_task_argument)),
                ws(char(')')),
            ),
        )),
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

/// An identifier that is not a keyword.
///
/// A bare `@ev` event control ends at whatever follows the name, so without
/// this an `@` in front of a keyword-led statement would read the keyword as
/// the event it waits on.
fn unreserved_identifier(input: &str) -> IResult<&str, Identifier> {
    let (rest, name) = identifier(input)?;
    if is_reserved_word(&name.name) {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }
    Ok((rest, name))
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
/// `@(a, b)`, `@(*)`, `@*` or a bare `@ev`. The wildcard forms yield
/// `EventControl::Implicit`, which is distinct from a block that carries no
/// event control at all.
///
/// The parenthesised list is tried first, so `@(*)` is the wildcard rather
/// than `@` followed by a parenthesised something. The bare identifier form is
/// last: it is the loosest, and only an identifier — `@ posedge clk` without
/// parentheses is not Verilog.
pub fn parse_sensitivity_list(input: &str) -> IResult<&str, EventControl> {
    let (input, _) = ws(char('@'))(input)?;
    alt((
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
        ),
        map(ws(char('*')), |_| EventControl::Implicit),
        map(ws(unreserved_identifier), |name| {
            EventControl::Events(vec![Event::new(
                EventTriggers::EitherEdge,
                Expression::Identifier(name),
            )])
        }),
    ))(input)
}

pub fn parse_initial_block(input: &str) -> IResult<&str, InitialBlock> {
    let (input, _) = ws(tag("initial"))(input)?;
    let (input, assignments) = alt((
        parse_block,
        map(null_statement, |_| Vec::new()),
        many1(procedural_statement),
    ))(input)?;
    let initial_block = InitialBlock::new(assignments);
    Ok((input, initial_block))
}

pub fn parse_always_block(input: &str) -> IResult<&str, AlwaysBlock> {
    let (input, _) = ws(tag("always"))(input)?;
    let (input, event_control) = map(opt(parse_sensitivity_list), |control| {
        control.unwrap_or(EventControl::None)
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, assignments) = alt((
        parse_block,
        map(null_statement, |_| Vec::new()),
        many1(procedural_statement),
    ))(input)?;

    let block = AlwaysBlock::new(event_control, assignments);

    Ok((input, block))
}

/// A `begin`…`end` block as a statement list.
///
/// An unnamed block is *flattened* into the statements it holds: it is a
/// grouping and nothing else, so nothing downstream has to know it was
/// written. A named one keeps its node, because its name is the scope its
/// variables live in.
pub fn parse_block(input: &str) -> IResult<&str, Vec<ProceduralStatements>> {
    let (input, block) = sequential_block(input)?;
    Ok((
        input,
        match block.name {
            None => block.statements,
            Some(_) => vec![ProceduralStatements::Block(block)],
        },
    ))
}

/// `begin [: name] [declarations] statements end`.
fn sequential_block(input: &str) -> IResult<&str, BlockStatement> {
    block_between(input, "begin", "end")
}

/// `fork [: name] [declarations] statements join`.
fn parallel_block(input: &str) -> IResult<&str, BlockStatement> {
    block_between(input, "fork", "join")
}

/// The shared shape of the two block forms, which differ only in their
/// keywords.
///
/// The opening keyword has been consumed by the time the body is read, so
/// nothing else can match this text: a missing closing keyword is a hard
/// failure, and the position it carries points at the first statement the
/// grammar could not read rather than at the `begin` itself.
fn block_between<'a>(
    input: &'a str,
    open: &'static str,
    close: &'static str,
) -> IResult<&'a str, BlockStatement> {
    let (input, _) = keyword(input, open)?;
    let (input, name) = opt(preceded(ws(char(':')), ws(identifier)))(input)?;
    // Only a named block is a scope, and only a scope may declare variables.
    let (input, locals) = match &name {
        Some(_) => map(many0(block_item), |items| {
            items.into_iter().flatten().collect()
        })(input)?,
        None => (input, Vec::new()),
    };
    let (input, statements) = statement_run(input)?;
    let (input, _) = ws(tag(close))(input).map_err(|error: nom::Err<_>| match error {
        nom::Err::Error(inner) => nom::Err::Failure(inner),
        other => other,
    })?;

    Ok((
        input,
        BlockStatement {
            name,
            locals,
            statements,
        },
    ))
}

/// One variable declaration inside a named block: `reg [7:0] tmp;`,
/// `integer i, j;`.
///
/// Like [`function_item`] it gives up unless it saw a type, which is what lets
/// `many0` stop at the first statement of the block.
fn block_item(input: &str) -> IResult<&str, Vec<FunctionVariable>> {
    let (input, declared) = declared_type(input)?;
    if !declared.explicit {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }
    let (input, names) = identifier_list(input)?;
    let (input, _) = ws(char(';'))(input)?;

    Ok((
        input,
        names
            .into_iter()
            .map(|name| FunctionVariable {
                name,
                range: declared.range.clone(),
                signed: declared.signed,
            })
            .collect(),
    ))
}

/// `wait (expr) statement`, including `wait (expr);` with no statement at all.
fn parse_wait_statement(input: &str) -> IResult<&str, WaitStatement> {
    let (input, _) = keyword(input, "wait")?;
    let (input, condition) = parenthesized_expression(input)?;
    let (input, statements) = statement_body(input)?;

    Ok((
        input,
        WaitStatement {
            condition,
            statements,
        },
    ))
}

/// `@(posedge clk) statement`, `@ev;` — an event control in front of a
/// statement rather than in front of a whole `always` block.
fn parse_event_controlled_statement(input: &str) -> IResult<&str, ProceduralStatements> {
    let (input, control) = parse_sensitivity_list(input)?;
    let (input, statements) = statement_body(input)?;
    Ok((
        input,
        ProceduralStatements::EventControlled {
            control,
            statements,
        },
    ))
}

/// The timing control written between an assignment's `=` and its right hand
/// side: `a = #5 b;`, `a = @(posedge clk) b;`, `a = repeat (3) @(clk) b;`.
///
/// The grammar for it lives here because the event forms are this module's,
/// while the value it produces belongs to the assignment that carries it.
pub(crate) fn assignment_timing(input: &str) -> IResult<&str, AssignmentTiming> {
    alt((map(parse_delay, AssignmentTiming::Delay), event_timing))(input)
}

/// `@(posedge clk)`, or `repeat (3) @(ev)` — the event half of an
/// intra-assignment timing control. The count says how many times the event
/// has to happen before the write lands.
fn event_timing(input: &str) -> IResult<&str, AssignmentTiming> {
    let (input, repeat) = opt(preceded(|i| keyword(i, "repeat"), parenthesized_expression))(input)?;
    let (input, control) = parse_sensitivity_list(input)?;
    Ok((input, AssignmentTiming::Event { repeat, control }))
}

/// One variable a `function` or a `task` declares: an argument or a
/// body-local.
///
/// A function's *own name* is one of these too — it is the variable the body
/// assigns to return a value — which is why the return width and the width of
/// a local are described by the same thing.
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionVariable {
    pub name: Identifier,
    pub range: Range,
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
    pub range: Range,
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
#[derive(Debug, PartialEq, Clone)]
struct DeclaredType {
    range: Range,
    signed: bool,
    explicit: bool,
}

impl Default for DeclaredType {
    fn default() -> Self {
        DeclaredType {
            range: Range::SINGLE_BIT,
            signed: false,
            explicit: false,
        }
    }
}

/// The type part of a variable declaration: an optional storage keyword, an
/// optional `signed`, and either an `integer` or a range.
///
/// `integer` and `time` are written *instead of* a range and carry their own
/// width — 32 bits signed and 64 bits unsigned respectively — so a declaration
/// never has both.
fn declared_type(input: &str) -> IResult<&str, DeclaredType> {
    let (input, storage) = opt(alt((
        value(false, |i| keyword(i, "reg")),
        value(false, |i| keyword(i, "wire")),
        value(true, |i| keyword(i, "time")),
    )))(input)?;
    let (input, integer) = opt(|i| keyword(i, "integer"))(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, signed) = signedness(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, declared) = opt(range)(input)?;

    Ok((
        input,
        DeclaredType {
            // `storage` is `Some(true)` for a `time`, whose 64 bits are what
            // the keyword means rather than a range it was written with.
            range: match (integer.is_some(), storage, &declared) {
                (true, _, _) => Range::Constant(31, 0),
                (false, _, Some(declared)) => declared.clone(),
                (false, Some(true), None) => Range::Constant(63, 0),
                (false, _, None) => Range::SINGLE_BIT,
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
                    range: declared.range.clone(),
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
            range: inherited.range.clone(),
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
    let (input, statements) = alt((parse_block, statement_run))(input)?;
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

/// Which way a task argument is copied.
///
/// A function has inputs and nothing else; a task is a statement, so it hands
/// results back through its arguments instead of through a return value.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TaskDirection {
    Input,
    Output,
    Inout,
}

impl TaskDirection {
    /// Whether the caller's expression is copied *into* the argument when the
    /// task starts.
    pub fn copies_in(self) -> bool {
        matches!(self, TaskDirection::Input | TaskDirection::Inout)
    }

    /// Whether the argument is copied *back* to the caller when the task
    /// returns.
    pub fn copies_back(self) -> bool {
        matches!(self, TaskDirection::Output | TaskDirection::Inout)
    }
}

/// One argument of a task: a variable, plus which way it is copied.
#[derive(Debug, PartialEq, Clone)]
pub struct TaskArgument {
    pub direction: TaskDirection,
    pub variable: FunctionVariable,
}

/// `task load; input [7:0] a; output [7:0] b; b = a + 1; endtask`
///
/// The arguments may be written either the 1995 way, as direction declarations
/// *inside* the body, or the 2001 way, as a parenthesised list after the name.
/// Both fill [`arguments`](TaskDeclaration::arguments) in call order.
#[derive(Debug, PartialEq)]
pub struct TaskDeclaration {
    pub name: Identifier,
    pub arguments: Vec<TaskArgument>,
    /// Body-local `reg` and `integer` declarations.
    pub locals: Vec<FunctionVariable>,
    pub statements: Vec<ProceduralStatements>,
}

/// The direction keyword that marks a task argument.
fn task_direction(input: &str) -> IResult<&str, TaskDirection> {
    alt((
        value(TaskDirection::Input, |i| keyword(i, "input")),
        value(TaskDirection::Output, |i| keyword(i, "output")),
        value(TaskDirection::Inout, |i| keyword(i, "inout")),
    ))(input)
}

/// One item inside a task body: `input [7:0] a;`, `reg [3:0] tmp;`. The
/// direction is `None` for a body-local.
///
/// Like [`function_item`] this gives up unless it saw a direction or a type,
/// which is what lets `many0` stop at the first statement of the body.
fn task_item(input: &str) -> IResult<&str, (Option<TaskDirection>, Vec<FunctionVariable>)> {
    let (input, direction) = opt(task_direction)(input)?;
    let (input, declared) = declared_type(input)?;
    if direction.is_none() && !declared.explicit {
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
            direction,
            names
                .into_iter()
                .map(|name| FunctionVariable {
                    name,
                    range: declared.range.clone(),
                    signed: declared.signed,
                })
                .collect(),
        ),
    ))
}

/// One element of a 2001 task argument list: `output [7:0] b`, or a bare `c`
/// that inherits the element before it.
fn ansi_task_argument(
    input: &str,
) -> IResult<&str, (Option<TaskDirection>, DeclaredType, Identifier)> {
    let (input, direction) = opt(task_direction)(input)?;
    let (input, declared) = declared_type(input)?;
    let (input, name) = ws(identifier)(input)?;
    Ok((input, (direction, declared, name)))
}

/// `(input [7:0] a, b, output c)` — the 2001 argument list.
///
/// An element that names neither a direction nor a type takes both from the
/// element before it, so `a` and `b` above are both eight bits. Naming a
/// direction *resets* the type, which is why `c` is one bit rather than eight
/// — the same reading iverilog takes.
fn ansi_task_arguments(input: &str) -> IResult<&str, Vec<TaskArgument>> {
    let (input, elements) = delimited(
        ws(char('(')),
        separated_list0(char(','), ws(ansi_task_argument)),
        ws(char(')')),
    )(input)?;

    let mut inherited = DeclaredType::default();
    let mut direction = TaskDirection::Input;
    let mut arguments = Vec::with_capacity(elements.len());
    for (declared_direction, declared, name) in elements {
        if let Some(declared_direction) = declared_direction {
            direction = declared_direction;
            inherited = DeclaredType::default();
        }
        if declared.explicit {
            inherited = declared;
        }
        arguments.push(TaskArgument {
            direction,
            variable: FunctionVariable {
                name,
                range: inherited.range.clone(),
                signed: inherited.signed,
            },
        });
    }
    Ok((input, arguments))
}

/// `task name; <declarations> <statements> endtask`.
///
/// A name declared twice — `input [7:0] x;` followed by `reg [7:0] x;`, which
/// is how the 1995 form spells out an argument's data type — is one argument,
/// not an argument and a local: the later declaration refines the type the
/// direction introduced.
pub fn parse_task_declaration(input: &str) -> IResult<&str, TaskDeclaration> {
    let (input, _) = keyword(input, "task")?;
    let (input, _) = opt(|i| keyword(i, "automatic"))(input)?;
    let (input, name) = ws(identifier)(input)?;
    let (input, ansi) = opt(ansi_task_arguments)(input)?;
    let (input, _) = ws(char(';'))(input)?;
    let (input, items) = many0(task_item)(input)?;
    let (input, statements) = alt((parse_block, statement_run))(input)?;
    let (input, _) = ws(tag("endtask"))(input)?;

    let mut arguments = ansi.unwrap_or_default();
    let mut locals: Vec<FunctionVariable> = Vec::new();
    for (direction, variables) in items {
        for variable in variables {
            let declared = arguments
                .iter()
                .position(|argument| argument.variable.name == variable.name);
            match (declared, direction) {
                // A direction the argument already has: the type it names is
                // the one that counts.
                (Some(index), None) => arguments[index].variable = variable,
                (Some(index), Some(direction)) => arguments[index].direction = direction,
                // A direction for something already declared as a local: it was
                // an argument all along, and takes its position here.
                (None, Some(direction)) => {
                    let variable = match locals.iter().position(|l| l.name == variable.name) {
                        Some(index) => locals.remove(index),
                        None => variable,
                    };
                    arguments.push(TaskArgument {
                        direction,
                        variable,
                    });
                }
                (None, None) => locals.push(variable),
            }
        }
    }

    Ok((
        input,
        TaskDeclaration {
            name,
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

    /// A bare `;` is a legal statement that does nothing, so it leaves no node
    /// behind: a block containing one is a block of the statements around it.
    #[test]
    fn test_a_null_statement_parses_and_produces_no_node() {
        let statements = assert_parses(parse_block, "begin ; a = 1; ; b = 2; ;; end");
        assert_eq!(statements.len(), 2);

        let empty = assert_parses(parse_block, "begin ; end");
        assert!(empty.is_empty());

        // A `;` after a statement that already ate its own is a second,
        // separate null statement rather than a parse error.
        let trailing = assert_parses(parse_block, "begin a = 1;; end");
        assert_eq!(trailing.len(), 1);
    }

    /// `else ;` is an `if` whose else branch is a null statement, which is an
    /// empty branch rather than an absent one.
    #[test]
    fn test_a_null_statement_is_a_conditional_branch() {
        let statement = assert_parses(procedural_statement, "if (a) b = 1; else ;");
        match statement {
            ProceduralStatements::If(conditional) => {
                assert_eq!(conditional.then_statements.len(), 1);
                assert_eq!(conditional.else_statements, Some(Vec::new()));
            }
            other => panic!("expected an if statement, got {:?}", other),
        }

        let empty_then = assert_parses(procedural_statement, "if (a) ; else b = 1;");
        match empty_then {
            ProceduralStatements::If(conditional) => {
                assert!(conditional.then_statements.is_empty());
                assert_eq!(conditional.else_statements.map(|arm| arm.len()), Some(1));
            }
            other => panic!("expected an if statement, got {:?}", other),
        }
    }

    /// The other bodies a statement may fill: a case arm, a loop, and the
    /// whole of an `initial` or `always` block.
    #[test]
    fn test_a_null_statement_fills_any_statement_body() {
        for source in [
            "case (x) 1: ; default: a = 1; endcase",
            "for (i = 0; i < 4; i = i + 1) ;",
            "while (a) ;",
            "repeat (4) ;",
            "#5 ;",
        ] {
            assert_parses(procedural_statement, source);
        }

        let initial = assert_parses(parse_initial_block, "initial ;");
        assert!(initial.statements.is_empty());

        let always = assert_parses(parse_always_block, "always @(a) ;");
        assert!(always.statements.is_empty());
    }

    /// End to end: a null statement runs as nothing at all, and the statements
    /// around it still run.
    #[test]
    fn test_a_null_statement_simulates_as_a_no_op() {
        let source = "module m(); reg a; reg b;\n\
                      initial begin ; a = 1; ; if (a) ; else b = 0; b = 1; ; end\n\
                      endmodule";
        let (remaining, module) =
            crate::parsers::modules::parse_module_declaration(source).expect("module should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);

        let mut simulator = crate::simulator::runner::Simulator::new(module);
        simulator.setup().expect("setup should succeed");
        simulator.advance(1).expect("advance should succeed");

        assert_eq!(simulator.get("a").expect("a should exist").to_binary(), "1");
        assert_eq!(simulator.get("b").expect("b should exist").to_binary(), "1");
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
        assert_eq!(function.range, Range::Constant(7, 0));
        assert_eq!(function.arguments.len(), 1);
        assert_eq!(function.arguments[0].name, "a".into());
        assert_eq!(function.arguments[0].range, Range::Constant(7, 0));
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
        assert!(function
            .arguments
            .iter()
            .all(|a| a.range == Range::Constant(3, 0)));
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
        assert_eq!(function.range, Range::Constant(31, 0));
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
        assert_eq!(function.locals[0].range, Range::Constant(31, 0));
        assert_eq!(function.locals[1].range, Range::Constant(3, 0));
    }

    /// A function that declares no width returns one bit, and one that
    /// declares no arguments takes none.
    #[test]
    fn test_parse_function_declaration_minimal() {
        let function = assert_parses(parse_function_declaration, "function f; f = 1; endfunction");

        assert_eq!(function.range, Range::Constant(0, 0));
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

    /// A task reads its arguments the 1995 way, as direction declarations
    /// inside the body, and keeps them in call order.
    #[test]
    fn test_parse_task_declaration_1995_style() {
        let task = assert_parses(
            parse_task_declaration,
            r#"task load;
                 input [7:0] a;
                 output [7:0] b;
                 inout c;
                 reg [7:0] tmp;
                 begin
                   tmp = a;
                   b = tmp + 1;
                 end
               endtask"#,
        );

        assert_eq!(task.name, "load".into());
        assert_eq!(task.arguments.len(), 3);
        assert_eq!(task.arguments[0].direction, TaskDirection::Input);
        assert_eq!(task.arguments[0].variable.range, Range::Constant(7, 0));
        assert_eq!(task.arguments[1].direction, TaskDirection::Output);
        assert_eq!(task.arguments[2].direction, TaskDirection::Inout);
        assert_eq!(task.arguments[2].variable.range, Range::SINGLE_BIT);
        assert_eq!(task.locals.len(), 1);
        assert_eq!(task.locals[0].name, "tmp".into());
        assert_eq!(task.statements.len(), 2);
    }

    /// The 2001 form puts the arguments in a parenthesised list. A bare name
    /// inherits the element before it; naming a direction *resets* the type,
    /// so `c` below is one bit rather than eight — which is how iverilog reads
    /// it too.
    #[test]
    fn test_parse_task_declaration_2001_style() {
        let task = assert_parses(
            parse_task_declaration,
            "task load(input [7:0] a, b, output c); c = a + b; endtask",
        );

        assert_eq!(task.arguments.len(), 3);
        assert_eq!(task.arguments[0].variable.range, Range::Constant(7, 0));
        assert_eq!(task.arguments[1].variable.name, "b".into());
        assert_eq!(task.arguments[1].variable.range, Range::Constant(7, 0));
        assert_eq!(task.arguments[1].direction, TaskDirection::Input);
        assert_eq!(task.arguments[2].direction, TaskDirection::Output);
        assert_eq!(task.arguments[2].variable.range, Range::SINGLE_BIT);
    }

    /// A task that takes nothing may still be written with an empty argument
    /// list, and one with an empty body parses to no statements at all.
    #[test]
    fn test_parse_task_declaration_minimal() {
        let task = assert_parses(parse_task_declaration, "task foo(); endtask");

        assert!(task.arguments.is_empty());
        assert!(task.locals.is_empty());
        assert!(task.statements.is_empty());
    }

    /// The 1995 form may spell an argument's data type out separately, which
    /// makes one argument rather than an argument and a local.
    #[test]
    fn test_parse_task_argument_declared_twice_is_one_argument() {
        let task = assert_parses(
            parse_task_declaration,
            "task t; input x; reg [7:0] x; t = x; endtask",
        );

        assert_eq!(task.arguments.len(), 1);
        assert_eq!(task.arguments[0].direction, TaskDirection::Input);
        assert_eq!(task.arguments[0].variable.range, Range::Constant(7, 0));
        assert!(task.locals.is_empty());
    }

    /// A `time` variable is 64 bits by being a `time`, the way an `integer` is
    /// 32 by being an `integer`.
    #[test]
    fn test_parse_task_time_argument_is_sixty_four_bits() {
        let task = assert_parses(
            parse_task_declaration,
            "task t; output time stamp; integer i; stamp = 0; endtask",
        );

        assert_eq!(task.arguments[0].variable.range, Range::Constant(63, 0));
        assert!(!task.arguments[0].variable.signed);
        assert_eq!(task.locals[0].range, Range::Constant(31, 0));
        assert!(task.locals[0].signed);
    }

    /// A task enable is a statement, with or without an argument list.
    #[test]
    fn test_parse_task_enable() {
        assert_parses_to(
            procedural_statement,
            "my_task(a, 1);",
            ProceduralStatements::TaskEnable {
                name: "my_task".into(),
                arguments: vec![
                    Expression::Identifier("a".into()),
                    Expression::Constant(VerilogConstant::from_int(1)),
                ],
            },
        );

        assert_parses_to(
            procedural_statement,
            "my_task;",
            ProceduralStatements::TaskEnable {
                name: "my_task".into(),
                arguments: Vec::new(),
            },
        );
    }

    /// A bare identifier followed by `;` is the loosest statement shape there
    /// is, so the enable has to be tried last: every keyword-led statement and
    /// every assignment still reads as itself.
    #[test]
    fn test_task_enable_does_not_shadow_other_statements() {
        let statements = assert_parses(
            parse_block,
            r#"begin
                 a = 1;
                 forever_more = 2;
                 $display("x");
                 disable_me;
                 #5;
               end"#,
        );

        assert!(matches!(
            statements.as_slice(),
            [
                ProceduralStatements::Assignment(_),
                ProceduralStatements::Assignment(_),
                ProceduralStatements::SystemTask(_),
                ProceduralStatements::TaskEnable { .. },
                ProceduralStatements::Delay(_),
            ]
        ));
    }

    /// `assign` inside a block is a *procedural* continuous assignment, and it
    /// reads as one rather than as an assignment to a signal called `assign`.
    #[test]
    fn test_parse_procedural_assign() {
        let statement = assert_parses(procedural_statement, "assign v = 2;");

        let ProceduralStatements::Assign { target, .. } = statement else {
            panic!("expected a procedural assign, got {:?}", statement);
        };
        assert_eq!(target, identifier_expression("v"));

        // The place the corpus writes it: as the whole body of an `always`.
        let block = assert_parses(parse_always_block, "always @(a) assign v = 2;");
        assert!(matches!(
            block.statements[0],
            ProceduralStatements::Assign { .. }
        ));
    }

    #[test]
    fn test_parse_procedural_deassign() {
        let statement = assert_parses(procedural_statement, "deassign v;");
        assert_eq!(
            statement,
            ProceduralStatements::Deassign(identifier_expression("v"))
        );

        let block = assert_parses(parse_always_block, "always @(a) deassign v;");
        assert!(matches!(
            block.statements[0],
            ProceduralStatements::Deassign(_)
        ));
    }

    #[test]
    fn test_parse_force() {
        let statement = assert_parses(procedural_statement, "force v[2] = a & b;");

        let ProceduralStatements::Force { target, .. } = statement else {
            panic!("expected a force, got {:?}", statement);
        };
        assert!(matches!(target, Expression::BitSelect(_, _)));
    }

    #[test]
    fn test_parse_release() {
        let statements = assert_parses(parse_block, "begin force v = 1; release v; end");

        assert!(matches!(statements[0], ProceduralStatements::Force { .. }));
        assert_eq!(
            statements[1],
            ProceduralStatements::Release(identifier_expression("v"))
        );
    }

    /// The four keywords need a word boundary after them, or a signal whose
    /// name merely starts with one would be swallowed.
    #[test]
    fn test_a_drive_keyword_does_not_swallow_a_longer_identifier() {
        for source in [
            "assignment = 1;",
            "forced = 1;",
            "released = 1;",
            "deassigned = 1;",
        ] {
            let statement = assert_parses(procedural_statement, source);
            assert!(
                matches!(statement, ProceduralStatements::Assignment(_)),
                "{} should be an ordinary assignment, got {:?}",
                source,
                statement
            );
        }
    }

    #[test]
    fn test_named_block_declares_its_own_variables() {
        let statements = assert_parses(
            parse_block,
            "begin : block_id reg [7:0] tmp; integer i, j; tmp = 1; end",
        );

        let [ProceduralStatements::Block(block)] = statements.as_slice() else {
            panic!("expected one named block, got {:?}", statements);
        };
        assert_eq!(block.name, Some("block_id".into()));
        assert_eq!(
            block
                .locals
                .iter()
                .map(|local| local.name.name.as_str())
                .collect::<Vec<_>>(),
            ["tmp", "i", "j"]
        );
        assert_eq!(block.locals[0].range, Range::Constant(7, 0));
        // An `integer` is 32 bits and signed by being an `integer`.
        assert_eq!(block.locals[1].range, Range::Constant(31, 0));
        assert!(block.locals[1].signed);
        assert_eq!(block.statements.len(), 1);
    }

    /// An unnamed block is grouping and nothing else, so it leaves no node
    /// behind — but a nested one is still a statement in its own right.
    #[test]
    fn test_a_nested_block_is_a_statement() {
        let statements = assert_parses(parse_block, "begin a = 1; begin b = 2; c = 3; end end");

        let [ProceduralStatements::Assignment(_), ProceduralStatements::Block(inner)] =
            statements.as_slice()
        else {
            panic!("expected an assignment and a block, got {:?}", statements);
        };
        assert_eq!(inner.name, None);
        assert!(inner.locals.is_empty());
        assert_eq!(inner.statements.len(), 2);
    }

    #[test]
    fn test_fork_join_parses_its_branches() {
        let statement = assert_parses(procedural_statement, "fork a = 1; b = 2; join");

        let ProceduralStatements::Fork(block) = statement else {
            panic!("expected a fork, got {:?}", statement);
        };
        assert_eq!(block.name, None);
        assert_eq!(block.statements.len(), 2);

        let statement = assert_parses(procedural_statement, "fork : f reg t; t = 1; a = t; join");
        let ProceduralStatements::Fork(block) = statement else {
            panic!("expected a named fork, got {:?}", statement);
        };
        assert_eq!(block.name, Some("f".into()));
        assert_eq!(block.locals.len(), 1);
        assert_eq!(block.statements.len(), 2);
    }

    #[test]
    fn test_wait_statement_parses_with_and_without_a_body() {
        let statement = assert_parses(procedural_statement, "wait (foo) a = 1;");
        let ProceduralStatements::Wait(wait) = statement else {
            panic!("expected a wait, got {:?}", statement);
        };
        assert_eq!(wait.condition, identifier_expression("foo"));
        assert_eq!(wait.statements.len(), 1);

        // `wait (foo) ;` waits and then does nothing, which is a null
        // statement and so leaves no node behind.
        let statement = assert_parses(procedural_statement, "wait (foo) ;");
        let ProceduralStatements::Wait(wait) = statement else {
            panic!("expected a wait, got {:?}", statement);
        };
        assert!(wait.statements.is_empty());
    }

    #[test]
    fn test_statement_level_event_control_parses() {
        let statement = assert_parses(procedural_statement, "@(posedge clk) a = 1;");
        let ProceduralStatements::EventControlled {
            control,
            statements,
        } = statement
        else {
            panic!("expected an event control, got {:?}", statement);
        };
        assert_eq!(
            control,
            EventControl::Events(vec![Event::new(
                EventTriggers::PosEdge,
                identifier_expression("clk")
            )])
        );
        assert_eq!(statements.len(), 1);

        // `@ev;` waits and does nothing, and the bare name is an event
        // control just as a parenthesised one is.
        let statement = assert_parses(procedural_statement, "@ev;");
        let ProceduralStatements::EventControlled {
            control,
            statements,
        } = statement
        else {
            panic!("expected an event control, got {:?}", statement);
        };
        assert_eq!(
            control,
            EventControl::Events(vec![Event::new(
                EventTriggers::EitherEdge,
                identifier_expression("ev")
            )])
        );
        assert!(statements.is_empty());
    }

    /// `@*`, `@(*)` and a bare `@ev` are all event controls, and the two
    /// wildcard spellings mean the same thing.
    #[test]
    fn test_the_three_sensitivity_list_spellings() {
        assert_parses_to(parse_sensitivity_list, "@(*)", EventControl::Implicit);
        assert_parses_to(parse_sensitivity_list, "@*", EventControl::Implicit);
        assert_parses_to(
            parse_sensitivity_list,
            "@ ev",
            EventControl::Events(vec![Event::new(
                EventTriggers::EitherEdge,
                identifier_expression("ev"),
            )]),
        );
    }

    /// A bare `@` event control takes an identifier, and a keyword is not one:
    /// without that guard `always @* begin … end` would read `begin` as the
    /// event it waits on.
    #[test]
    fn test_a_bare_event_control_does_not_take_a_keyword() {
        let block = assert_parses(parse_always_block, "always @* begin a = b; end");
        assert_eq!(block.event_control, EventControl::Implicit);
    }

    #[test]
    fn test_intra_assignment_event_control_parses() {
        let statement = assert_parses(procedural_statement, "value1 = @ ev 4'h5;");
        let ProceduralStatements::Assignment(assignment) = statement else {
            panic!("expected an assignment, got {:?}", statement);
        };
        let Some(AssignmentTiming::Event { repeat, control }) = assignment.timing() else {
            panic!("expected an event timing, got {:?}", assignment.timing());
        };
        assert_eq!(*repeat, None);
        assert_eq!(
            *control,
            EventControl::Events(vec![Event::new(
                EventTriggers::EitherEdge,
                identifier_expression("ev")
            )])
        );

        let statement = assert_parses(procedural_statement, "value1 = repeat ( 5 ) @ ev 4'h5;");
        let ProceduralStatements::Assignment(assignment) = statement else {
            panic!("expected an assignment, got {:?}", statement);
        };
        let Some(AssignmentTiming::Event { repeat, .. }) = assignment.timing() else {
            panic!("expected an event timing, got {:?}", assignment.timing());
        };
        assert_eq!(
            *repeat,
            Some(Expression::Constant(VerilogConstant::from_int(5)))
        );
    }

    #[test]
    fn test_intra_assignment_delay_is_kept_on_the_assignment() {
        let statement = assert_parses(procedural_statement, "a = #5 b;");
        let ProceduralStatements::Assignment(assignment) = statement else {
            panic!("expected an assignment, got {:?}", statement);
        };
        assert_eq!(
            assignment.timing(),
            Some(&AssignmentTiming::Delay(Delay::new(5)))
        );
    }
}

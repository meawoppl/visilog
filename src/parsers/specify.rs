//! `specify … endspecify` — module path delays, timing checks and the
//! constants they are written with.
//!
//! ```verilog
//! specify
//!   specparam tRise = 6, tFall = 7;
//!   (A => Z)        = (0.1, 0.2);
//!   (a, b, ci *> s) = 1.5;
//!   if (fast) (b => q) = 1;
//!   (posedge cp => (q +: d)) = (tRise, tFall);
//!   $setup(posedge d, posedge clk, 10, notifier);
//! endspecify
//! ```
//!
//! # What is simulated, and what is not
//!
//! A **module path delay changes only *when* a value arrives, never what the
//! value is**, and this simulator has no timing model at all: every continuous
//! driver settles in zero time. So a path is *recorded here and not
//! simulated*, which produces the same values at different edge times rather
//! than a wrong answer — the same trade already taken deliberately for a gate
//! delay, and the only category where a no-op is the honest reading.
//!
//! Two things inside a `specify` block are **not** inert, and neither is
//! treated as though it were:
//!
//! - A `specparam` declares a real constant that expressions elsewhere may
//!   name, so it becomes an actual parameter at elaboration rather than being
//!   discarded. The exception is one whose value is a *real* number — the one
//!   thing a four-state `Register` cannot hold — which is kept as the text it
//!   was written as and declares nothing.
//! - A **timing check** reports a violation, which needs the timing model the
//!   paths would need. One is recorded and never run, so no violation is
//!   invented and none is claimed to have been checked.
//!
//! Every form is parsed *structurally*. There is deliberately no "skip to the
//! next `;`" fallback: anything inside a `specify` block that is not one of
//! the forms below is a parse error, so an unsupported construct is visible
//! rather than swallowed.

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, satisfy},
    combinator::{map, not, opt, peek, recognize, value},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

use super::{
    behavior::EventTriggers,
    expr::{bit_select, indexed_part_select, part_select, verilog_expression, Expression},
    identifier::{identifier, Identifier},
    simple::{range, ws, ws_and_comments, Range},
};

/// A whole `specify` block: its constants, its paths and its timing checks,
/// each kept in the order it was written.
#[derive(Debug, PartialEq)]
pub struct SpecifyBlock {
    pub specparams: Vec<SpecParam>,
    /// Recorded, never simulated — a path delay moves an edge in time and this
    /// simulator has no time for it to move in.
    pub paths: Vec<PathDeclaration>,
    /// Recorded, never run. A check reports a violation of a timing
    /// constraint, which needs the same model a path delay would.
    pub checks: Vec<TimingCheck>,
}

/// `specparam tRise = 6;` — a constant, declared inside a `specify` block but
/// visible to the whole module.
#[derive(Debug, PartialEq)]
pub struct SpecParam {
    pub name: Identifier,
    /// `specparam [7:0] w = 8'hFF;` — a specparam may be given a width like
    /// any other declaration.
    pub range: Option<Range>,
    pub value: SpecParamValue,
}

/// What a `specparam` was set to.
#[derive(Debug, PartialEq, Clone)]
pub enum SpecParamValue {
    Expression(Expression),
    /// `0.9` — kept as the text it was written as. A real number is not
    /// something a four-state `Register` can hold, and the only thing that
    /// could use one is the path delay that is not simulated anyway.
    Real(String),
}

/// `if (cond)` or `ifnone` in front of a path.
#[derive(Debug, PartialEq, Clone)]
pub enum PathCondition {
    If(Expression),
    /// The path taken when no `if` condition on the same connection holds.
    IfNone,
}

/// The `+` or `-` written in front of the arrow: which way the path inverts.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Polarity {
    Positive,
    Negative,
}

/// One `(inputs => outputs) = delays;`.
#[derive(Debug, PartialEq)]
pub struct PathDeclaration {
    pub condition: Option<PathCondition>,
    /// `(posedge clk => …)` — an edge-sensitive path.
    pub edge: Option<EventTriggers>,
    pub inputs: Vec<Expression>,
    pub polarity: Option<Polarity>,
    /// `*>`, the full connection: every input reaches every output. `=>` is
    /// the parallel one, input to output in order.
    pub full: bool,
    pub outputs: Vec<Expression>,
    /// `(q +: d)` — the data source an edge-sensitive path carries.
    pub data_source: Option<Expression>,
    /// One entry per delay written: `(rise, fall)` is two, and each may be a
    /// `min:typ:max` triple of its own.
    pub delays: Vec<PathDelay>,
}

/// One delay of a path's delay list, which may be a `min:typ:max` triple.
#[derive(Debug, PartialEq)]
pub struct PathDelay {
    pub terms: Vec<DelayTerm>,
}

/// One number of a delay, which may be real.
#[derive(Debug, PartialEq)]
pub enum DelayTerm {
    Expression(Expression),
    Real(String),
}

/// `$setup(posedge d, posedge clk, 10, notifier);` — recorded, never run.
#[derive(Debug, PartialEq)]
pub struct TimingCheck {
    /// The task's name without its `$`.
    pub name: String,
    /// One entry per position written. A blank argument keeps its place, since
    /// the position is what says which argument it is.
    pub arguments: Vec<Option<TimingCheckArgument>>,
}

/// One argument of a timing check: a value, possibly with an edge in front of
/// it and a `&&&` condition after it.
#[derive(Debug, PartialEq)]
pub struct TimingCheckArgument {
    pub edge: Option<EventTriggers>,
    pub value: DelayTerm,
    pub condition: Option<Expression>,
}

/// The timing checks 1364-2005 defines. A `$name` that is not one of these is
/// a parse error rather than a check nothing carries out.
const TIMING_CHECKS: &[&str] = &[
    "setuphold",
    "setup",
    "hold",
    "recrem",
    "recovery",
    "removal",
    "timeskew",
    "fullskew",
    "skew",
    "width",
    "period",
    "nochange",
];

/// A character that may continue an identifier, so a keyword followed by one
/// is not a keyword at all: `if` in `ifdef`.
fn identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '$'
}

/// A whole keyword, which the word after it may not run into.
fn keyword(word: &'static str) -> impl Fn(&str) -> IResult<&str, &str> {
    move |input: &str| terminated(tag(word), peek(not(satisfy(identifier_char))))(input)
}

/// `0.9`, `1.0`, `0.500` — a real number, which the expression grammar has no
/// operand for.
///
/// Only the fixed-point spelling is accepted, because that is the whole of
/// what a delay is ever written as; an exponent would need a value to be
/// computed with, and there is none here.
fn real_number(input: &str) -> IResult<&str, String> {
    map(
        recognize(tuple((digit1, char('.'), digit1))),
        str::to_string,
    )(input)
}

/// One number of a delay: real, or anything the expression grammar reads —
/// which is how `tRise` names a `specparam`.
fn delay_term(input: &str) -> IResult<&str, DelayTerm> {
    alt((
        map(real_number, DelayTerm::Real),
        map(verilog_expression, DelayTerm::Expression),
    ))(input)
}

/// `1`, `1.5` or `0.1:0.2:0.3` — one delay, with its optional `min:typ:max`
/// alternatives.
fn path_delay(input: &str) -> IResult<&str, PathDelay> {
    map(separated_list1(ws(char(':')), delay_term), |terms| {
        PathDelay { terms }
    })(input)
}

/// `= (1, 2)` or `= 1.5` — a path's delay list, bracketed or bare.
fn path_delay_value(input: &str) -> IResult<&str, Vec<PathDelay>> {
    let (input, _) = ws(char('='))(input)?;
    alt((
        delimited(
            ws(char('(')),
            separated_list1(ws(char(',')), path_delay),
            ws(char(')')),
        ),
        map(path_delay, |delay| vec![delay]),
    ))(input)
}

/// A terminal of a path: a port name, or a bit or part of one.
///
/// It is deliberately *not* a general expression: `b *> a` would otherwise
/// read as a multiplication of `b` by whatever follows, and the parse that
/// results is a wrong tree rather than an error.
fn terminal(input: &str) -> IResult<&str, Expression> {
    alt((
        bit_select,
        indexed_part_select,
        part_select,
        map(identifier, Expression::Identifier),
    ))(input)
}

fn terminal_list(input: &str) -> IResult<&str, Vec<Expression>> {
    separated_list1(ws(char(',')), terminal)(input)
}

fn edge_identifier(input: &str) -> IResult<&str, EventTriggers> {
    alt((
        value(EventTriggers::PosEdge, keyword("posedge")),
        value(EventTriggers::NegEdge, keyword("negedge")),
    ))(input)
}

fn polarity(input: &str) -> IResult<&str, Polarity> {
    alt((
        value(Polarity::Positive, char('+')),
        value(Polarity::Negative, char('-')),
    ))(input)
}

/// The destination of a path: a terminal list, or the bracketed
/// `(q +: d)` an edge-sensitive path writes, which names the data source
/// feeding the output.
fn path_destination(input: &str) -> IResult<&str, (Vec<Expression>, Option<Expression>)> {
    alt((
        delimited(
            ws(char('(')),
            map(
                tuple((
                    terminal_list,
                    opt(ws(polarity)),
                    preceded(ws(char(':')), verilog_expression),
                )),
                |(outputs, _, source)| (outputs, Some(source)),
            ),
            ws(char(')')),
        ),
        map(terminal_list, |outputs| (outputs, None)),
    ))(input)
}

/// `(a, b => q) = (1, 2);` and every other shape of path declaration, with the
/// `if`/`ifnone` in front of it already read.
fn path_declaration(input: &str) -> IResult<&str, PathDeclaration> {
    let (input, condition) = opt(alt((
        map(
            preceded(
                ws(keyword("if")),
                delimited(ws(char('(')), verilog_expression, ws(char(')'))),
            ),
            PathCondition::If,
        ),
        value(PathCondition::IfNone, ws(keyword("ifnone"))),
    )))(input)?;

    let (input, _) = ws(char('('))(input)?;
    let (input, edge) = opt(ws(edge_identifier))(input)?;
    let (input, inputs) = ws(terminal_list)(input)?;
    let (input, polarity) = opt(polarity)(input)?;
    let (input, full) = alt((value(false, tag("=>")), value(true, tag("*>"))))(input)?;
    let (input, (outputs, data_source)) = ws(path_destination)(input)?;
    let (input, _) = ws(char(')'))(input)?;
    let (input, delays) = path_delay_value(input)?;
    let (input, _) = ws(char(';'))(input)?;

    Ok((
        input,
        PathDeclaration {
            condition,
            edge,
            inputs,
            polarity,
            full,
            outputs,
            data_source,
            delays,
        },
    ))
}

/// `specparam tRise = 6, tFall = 7;` — one declaration, any number of names.
fn specparam_declaration(input: &str) -> IResult<&str, Vec<SpecParam>> {
    let (input, _) = ws(keyword("specparam"))(input)?;
    let (input, declared) = opt(ws(range))(input)?;
    let (input, assignments) = separated_list1(
        ws(char(',')),
        pair(
            ws(identifier),
            preceded(
                ws(char('=')),
                alt((
                    map(real_number, SpecParamValue::Real),
                    map(verilog_expression, SpecParamValue::Expression),
                )),
            ),
        ),
    )(input)?;
    let (input, _) = ws(char(';'))(input)?;
    Ok((
        input,
        assignments
            .into_iter()
            .map(|(name, value)| SpecParam {
                name,
                range: declared.clone(),
                value,
            })
            .collect(),
    ))
}

/// The name of a timing check, without its `$`.
fn timing_check_name(input: &str) -> IResult<&str, String> {
    let (input, _) = char('$')(input)?;
    for check in TIMING_CHECKS {
        if let Ok((rest, name)) = keyword(check)(input) {
            return Ok((rest, name.to_string()));
        }
    }
    Err(nom::Err::Error(nom::error::Error::new(
        input,
        nom::error::ErrorKind::Tag,
    )))
}

/// One argument: `posedge clk &&& enable`, `10`, `notifier`, or nothing at all.
fn timing_check_argument(input: &str) -> IResult<&str, Option<TimingCheckArgument>> {
    let (input, _) = ws_and_comments(input)?;
    let (input, edge) = opt(ws(edge_identifier))(input)?;
    let (input, value) = opt(delay_term)(input)?;
    let Some(value) = value else {
        // A blank argument keeps its place: which argument a timing check is
        // reading is decided by position.
        let (input, _) = ws_and_comments(input)?;
        return Ok((input, None));
    };
    let (input, condition) = opt(preceded(ws(tag("&&&")), verilog_expression))(input)?;
    let (input, _) = ws_and_comments(input)?;
    Ok((
        input,
        Some(TimingCheckArgument {
            edge,
            value,
            condition,
        }),
    ))
}

fn timing_check(input: &str) -> IResult<&str, TimingCheck> {
    let (input, name) = ws(timing_check_name)(input)?;
    let (input, arguments) = delimited(
        ws(char('(')),
        separated_list0(char(','), timing_check_argument),
        ws(char(')')),
    )(input)?;
    let (input, _) = ws(char(';'))(input)?;
    Ok((input, TimingCheck { name, arguments }))
}

/// One item of a block, before it is sorted into the three lists.
enum SpecifyItem {
    Params(Vec<SpecParam>),
    Path(PathDeclaration),
    Check(TimingCheck),
}

fn specify_item(input: &str) -> IResult<&str, SpecifyItem> {
    alt((
        map(specparam_declaration, SpecifyItem::Params),
        map(timing_check, SpecifyItem::Check),
        map(path_declaration, SpecifyItem::Path),
    ))(input)
}

/// Parses `specify … endspecify`.
pub fn parse_specify_block(input: &str) -> IResult<&str, SpecifyBlock> {
    let (input, _) = ws(keyword("specify"))(input)?;
    let (input, items) = many0(ws(specify_item))(input)?;
    let (input, _) = ws(keyword("endspecify"))(input)?;

    let mut block = SpecifyBlock {
        specparams: Vec::new(),
        paths: Vec::new(),
        checks: Vec::new(),
    };
    for item in items {
        match item {
            SpecifyItem::Params(params) => block.specparams.extend(params),
            SpecifyItem::Path(path) => block.paths.push(path),
            SpecifyItem::Check(check) => block.checks.push(check),
        }
    }
    Ok((input, block))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::helpers::assert_parses;

    fn block(source: &str) -> SpecifyBlock {
        assert_parses(parse_specify_block, source)
    }

    /// The two arrows, the two delay spellings and a list on either side.
    #[test]
    fn test_simple_paths() {
        let parsed = block(
            "specify
               (A => Z) = (0.1, 0.2);
               (A *> C) = 100;
               (a, b, ci => co) = 1.5;
               (ina, inb *> out) = 0.4;
               (I => O) = (2);
             endspecify",
        );
        assert_eq!(parsed.paths.len(), 5);
        assert!(!parsed.paths[0].full);
        assert!(parsed.paths[1].full);
        assert_eq!(parsed.paths[0].delays.len(), 2);
        assert_eq!(
            parsed.paths[0].delays[0].terms,
            vec![DelayTerm::Real("0.1".to_string())]
        );
        // A comma-separated source list reaches one destination.
        assert_eq!(parsed.paths[2].inputs.len(), 3);
        assert_eq!(parsed.paths[2].outputs.len(), 1);
        // A bare delay is one delay, bracketed or not.
        assert_eq!(parsed.paths[1].delays.len(), 1);
        assert_eq!(parsed.paths[4].delays.len(), 1);
    }

    /// A polarity operator sits against the arrow, and a `min:typ:max` triple
    /// is one delay rather than three.
    #[test]
    fn test_polarity_and_triples() {
        let parsed = block(
            "specify
               (a1 +=> zn) = (0.500, 0.500);
               (a2 -=> zn) = (0.500, 0.500);
               (in +=> out) = (0.1:0.1:0.2, 0.1:0.1:0.2);
             endspecify",
        );
        assert_eq!(parsed.paths[0].polarity, Some(Polarity::Positive));
        assert_eq!(parsed.paths[1].polarity, Some(Polarity::Negative));
        assert_eq!(parsed.paths[2].delays.len(), 2);
        assert_eq!(parsed.paths[2].delays[0].terms.len(), 3);
    }

    /// A state-dependent path, both spellings, and the edge-sensitive form
    /// with the data source its destination carries.
    #[test]
    fn test_conditional_and_edge_sensitive_paths() {
        let parsed = block(
            "specify
               if (fast) (b => q) = 1;
               if (b === 1'b1 && ci === 1'b0) (a => s) = (1, 2);
               ifnone (a => s) = (1.1, 1.9);
               (posedge cp => (q +: d)) = (3, 2);
               (negedge cdn => (q +: 1'b0)) = (0, 3);
             endspecify",
        );
        assert_eq!(parsed.paths.len(), 5);
        assert!(matches!(
            parsed.paths[0].condition,
            Some(PathCondition::If(_))
        ));
        assert!(matches!(
            parsed.paths[2].condition,
            Some(PathCondition::IfNone)
        ));
        assert_eq!(parsed.paths[3].edge, Some(EventTriggers::PosEdge));
        assert!(parsed.paths[3].data_source.is_some());
        assert_eq!(parsed.paths[4].edge, Some(EventTriggers::NegEdge));
    }

    /// A `specparam` declaration is a list, and a real value is kept as it was
    /// written because nothing here can hold one.
    #[test]
    fn test_specparams() {
        let parsed = block(
            "specify
               specparam tR = 100,
                         tF = 150;
               specparam th = 0.9;
               specparam [7:0] wide = 8'hFF;
               (clk, d => q) = (tR, tF);
             endspecify",
        );
        assert_eq!(parsed.specparams.len(), 4);
        assert_eq!(parsed.specparams[0].name, "tR".into());
        assert_eq!(
            parsed.specparams[2].value,
            SpecParamValue::Real("0.9".to_string())
        );
        assert_eq!(parsed.specparams[3].range, Some(Range::Constant(7, 0)));
        // A delay may name one, which is why a specparam has to be declared.
        assert!(matches!(
            parsed.paths[0].delays[0].terms[0],
            DelayTerm::Expression(_)
        ));
    }

    /// A timing check is read structurally: its edges, its `&&&` condition and
    /// its notifier all keep their places.
    #[test]
    fn test_timing_checks() {
        let parsed = block(
            "specify
               $setup(posedge d, posedge clk &&& enable, 10, notifier);
               $width(posedge clk, 5);
               $period(negedge clk, 20);
             endspecify",
        );
        assert_eq!(parsed.checks.len(), 3);
        assert_eq!(parsed.checks[0].name, "setup");
        assert_eq!(parsed.checks[0].arguments.len(), 4);
        let reference = parsed.checks[0].arguments[1].as_ref().unwrap();
        assert_eq!(reference.edge, Some(EventTriggers::PosEdge));
        assert!(reference.condition.is_some());
        assert_eq!(parsed.checks[1].name, "width");
    }

    /// An empty block is legal, and comments are legal at every token boundary
    /// inside one.
    #[test]
    fn test_empty_and_commented_blocks() {
        assert!(block("specify endspecify").paths.is_empty());
        let parsed = block(
            "specify // the paths
               (A => Z) /* rise, fall */ = (1, 2); // one path
             endspecify",
        );
        assert_eq!(parsed.paths.len(), 1);
    }

    /// Anything the block does not recognise is a parse error, never a
    /// statement quietly skipped to the next `;`.
    #[test]
    fn test_an_unrecognised_item_is_rejected() {
        assert!(parse_specify_block("specify pulsestyle_onevent z; endspecify").is_err());
        assert!(parse_specify_block("specify $bogus(a, b); endspecify").is_err());
        assert!(parse_specify_block("specify (A => Z); endspecify").is_err());
        assert!(parse_specify_block("specify A => Z = 1; endspecify").is_err());
    }
}

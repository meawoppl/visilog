//! User-defined primitives — `primitive … endprimitive`.
//!
//! ```verilog
//! primitive mux (q, sel, a, b);
//!   output q;
//!   input  sel, a, b;
//!   table
//!     0 1 ? : 1 ;
//!     0 0 ? : 0 ;
//!     1 ? 1 : 1 ;
//!   endtable
//! endprimitive
//! ```
//!
//! A UDP is a truth table with one output — always the *first* terminal — and
//! any number of inputs, each one bit wide. It is instantiated exactly the way
//! a module is, so it is parsed into a [`VerilogModule`] whose single statement
//! is the table: the header is the same production a module's is, the ports
//! reconcile the same way, and the instantiation grammar, the module library
//! and the elaborator's port binding all work on it unchanged. What tells the
//! two apart afterwards is the one statement — nothing else has to know.
//!
//! # Combinational and sequential
//!
//! A **combinational** row is `<input levels> : <output>`. A **sequential** one
//! carries the primitive's current state as a third field —
//! `<inputs> : <state> : <next>` — may name an *edge* rather than a level
//! (`(01)`, `r`, `*`), and may answer `-`, meaning "hold". A sequential UDP
//! declares its output `reg`, and `initial q = 1'b0;` gives that register a
//! starting value.
//!
//! Both forms parse here. Only the combinational one simulates; a sequential
//! instance is a named error at elaboration, because an edge is a question
//! about the *previous* input values and a continuous driver is only ever
//! handed the present ones.
//!
//! # Symbols
//!
//! A level is `0`, `1`, `x`, `?` (any of the three) or `b` (either known one);
//! an input's `z` is read as an `x` before it is matched, so no table symbol
//! ever names one. They are written without separators — `?? 0` is three
//! fields — which is why each is a single character and an edge is bracketed.

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{anychar, char, satisfy},
    combinator::{map, not, opt, peek},
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated},
    IResult,
};

use crate::register::{ONE, X, ZERO};

use super::{
    expr::{verilog_expression, Expression},
    identifier::{identifier, Identifier},
    modules::{
        parse_port_declaration, parse_port_header, reconcile_ports, NetType, Port, PortDirection,
        VerilogModule,
    },
    simple::{ws, ws_and_comments},
    statements::ModuleStatement,
};

/// One input column's level: what a table entry may say about a single bit.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LevelSymbol {
    Zero,
    One,
    /// `x` — matches an unknown input, and so also a floating one.
    Unknown,
    /// `?` — any of `0`, `1` and `x`.
    Any,
    /// `b` — either known level, but not an unknown one.
    Binary,
}

impl LevelSymbol {
    /// The symbol a character spells, or `None` for anything else.
    fn from_char(character: char) -> Option<LevelSymbol> {
        match character {
            '0' => Some(LevelSymbol::Zero),
            '1' => Some(LevelSymbol::One),
            'x' | 'X' => Some(LevelSymbol::Unknown),
            '?' => Some(LevelSymbol::Any),
            'b' | 'B' => Some(LevelSymbol::Binary),
            _ => None,
        }
    }

    /// Whether this symbol matches a four-state level.
    ///
    /// A `z` never reaches here: an input's `z` is read as an `x` before it is
    /// matched, which is what makes `x 1 : 0` fire for a floating input.
    pub fn matches(&self, code: u8) -> bool {
        match self {
            LevelSymbol::Zero => code == ZERO,
            LevelSymbol::One => code == ONE,
            LevelSymbol::Unknown => code == X,
            LevelSymbol::Any => true,
            LevelSymbol::Binary => code == ZERO || code == ONE,
        }
    }
}

/// A transition on one input column, which only a sequential UDP may name.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum EdgeSymbol {
    /// `(01)`, `(?1)`, `(bx)` — an explicit pair, and what `r` and `f` are
    /// shorthand for.
    Pair(LevelSymbol, LevelSymbol),
    /// `p` — `(01)`, `(0x)` or `(x1)`.
    Positive,
    /// `n` — `(10)`, `(1x)` or `(x0)`.
    Negative,
    /// `*` — any change at all, which is `(??)`.
    Any,
}

/// One input column of one row.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum InputSymbol {
    Level(LevelSymbol),
    Edge(EdgeSymbol),
}

/// What a row drives.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OutputSymbol {
    Zero,
    One,
    Unknown,
    /// `-` — hold the current state, which only a sequential UDP has.
    NoChange,
}

/// One row of the table.
#[derive(Debug, PartialEq, Clone)]
pub struct UdpRow {
    /// One symbol per input terminal, in declaration order. The parser
    /// guarantees the count, so nothing downstream re-checks it.
    pub inputs: Vec<InputSymbol>,
    /// The primitive's current state, present exactly for a sequential UDP.
    pub current: Option<LevelSymbol>,
    pub output: OutputSymbol,
}

/// A whole `table … endtable`, plus what the declarations around it said about
/// the output.
#[derive(Debug, PartialEq, Clone)]
pub struct UdpTable {
    /// Whether the output is a `reg`, and so whether the primitive has a state
    /// of its own. Every row carries a `current` field exactly when this is
    /// set.
    pub sequential: bool,
    /// `initial q = 1'b0;` — the starting value of a sequential UDP's state.
    pub initial: Option<Expression>,
    pub rows: Vec<UdpRow>,
}

impl UdpTable {
    /// What the table drives, given one level per input.
    ///
    /// Measured against iverilog 12.0 rather than derived: when rows disagree
    /// — which the LRM leaves undefined — a `0` beats a `1` and both beat an
    /// `x`, whatever order they were written in. That also makes a row whose
    /// output is `x` indistinguishable from no row at all, since a combination
    /// nothing matches is already `x`.
    pub fn combinational_output(&self, levels: &[u8]) -> u8 {
        for wanted in [OutputSymbol::Zero, OutputSymbol::One] {
            if self
                .rows
                .iter()
                .any(|row| row.output == wanted && row.matches(levels))
            {
                return match wanted {
                    OutputSymbol::Zero => ZERO,
                    _ => ONE,
                };
            }
        }
        X
    }
}

impl UdpRow {
    /// Whether every input column of this row matches the levels given.
    ///
    /// An edge never matches: it is a question about the previous input
    /// values, which a combinational lookup does not have. Only a sequential
    /// UDP writes one, and a sequential UDP does not reach here.
    fn matches(&self, levels: &[u8]) -> bool {
        self.inputs
            .iter()
            .zip(levels)
            .all(|(symbol, &code)| match symbol {
                InputSymbol::Level(level) => level.matches(code),
                InputSymbol::Edge(_) => false,
            })
    }
}

/// A character that may continue an identifier, so a keyword followed by one
/// is not a keyword at all: `table` in `table_size`.
fn identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '$'
}

/// A whole keyword, which the word after it may not run into.
fn keyword(word: &'static str) -> impl Fn(&str) -> IResult<&str, &str> {
    move |input: &str| terminated(tag(word), peek(not(satisfy(identifier_char))))(input)
}

/// One level symbol, written as a single character.
fn level_symbol(input: &str) -> IResult<&str, LevelSymbol> {
    let (rest, character) = anychar(input)?;
    LevelSymbol::from_char(character)
        .map(|level| (rest, level))
        .ok_or_else(|| nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::OneOf)))
}

/// `(01)`, `(?1)`, `(bx)` — a bracketed transition.
fn edge_pair(input: &str) -> IResult<&str, EdgeSymbol> {
    let (input, _) = char('(')(input)?;
    let (input, from) = ws(level_symbol)(input)?;
    let (input, to) = ws(level_symbol)(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, EdgeSymbol::Pair(from, to)))
}

/// An edge, bracketed or written as one of the five shorthands.
fn edge_symbol(input: &str) -> IResult<&str, EdgeSymbol> {
    alt((
        edge_pair,
        map(char('*'), |_| EdgeSymbol::Any),
        map(satisfy(|c| c == 'r' || c == 'R'), |_| {
            EdgeSymbol::Pair(LevelSymbol::Zero, LevelSymbol::One)
        }),
        map(satisfy(|c| c == 'f' || c == 'F'), |_| {
            EdgeSymbol::Pair(LevelSymbol::One, LevelSymbol::Zero)
        }),
        map(satisfy(|c| c == 'p' || c == 'P'), |_| EdgeSymbol::Positive),
        map(satisfy(|c| c == 'n' || c == 'N'), |_| EdgeSymbol::Negative),
    ))(input)
}

/// One input column: an edge, or a level.
fn input_symbol(input: &str) -> IResult<&str, InputSymbol> {
    alt((
        map(edge_symbol, InputSymbol::Edge),
        map(level_symbol, InputSymbol::Level),
    ))(input)
}

impl OutputSymbol {
    /// What a row answers with. `-` is only legal in a sequential table, which
    /// the row check in [`parse_primitive_declaration`] enforces.
    fn from_char(character: char) -> Option<OutputSymbol> {
        match character {
            '0' => Some(OutputSymbol::Zero),
            '1' => Some(OutputSymbol::One),
            'x' | 'X' => Some(OutputSymbol::Unknown),
            '-' => Some(OutputSymbol::NoChange),
            _ => None,
        }
    }
}

/// One row: the input columns, then the output — with the current state
/// between them when the primitive is sequential.
///
/// The two shapes are not told apart until the field *after* the first colon
/// has been read, because a state field and an output field look alike.
fn table_row(input: &str) -> IResult<&str, UdpRow> {
    let (input, inputs) = many1(ws(input_symbol))(input)?;
    let (input, _) = ws(char(':'))(input)?;
    let (input, second) = ws(anychar)(input)?;
    let (input, third) = opt(preceded(ws(char(':')), ws(anychar)))(input)?;
    let (input, _) = ws(char(';'))(input)?;
    let (current, output) = match third {
        Some(last) => (
            Some(LevelSymbol::from_char(second).ok_or_else(|| malformed(input))?),
            OutputSymbol::from_char(last).ok_or_else(|| malformed(input))?,
        ),
        None => (
            None,
            OutputSymbol::from_char(second).ok_or_else(|| malformed(input))?,
        ),
    };
    Ok((
        input,
        UdpRow {
            inputs,
            current,
            output,
        },
    ))
}

/// A body item that is not the table.
enum PrimitiveItem {
    Ports(Vec<Port>),
    /// `reg q;` — the output has a state, so the primitive is sequential.
    Register(Identifier),
    /// `initial q = 1'b0;` — that state's starting value.
    Initial(Expression),
}

/// `initial q = 1'b0;`, which only a sequential UDP may write.
fn primitive_initial(input: &str) -> IResult<&str, Expression> {
    let (input, _) = keyword("initial")(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, _) = identifier(input)?;
    let (input, _) = ws(char('='))(input)?;
    let (input, value) = ws(verilog_expression)(input)?;
    let (input, _) = ws(char(';'))(input)?;
    Ok((input, value))
}

/// `reg q;` — a UDP register has no width, no list and no initialiser, so it
/// is its own small production rather than the module-level one.
fn primitive_register(input: &str) -> IResult<&str, Identifier> {
    let (input, _) = keyword("reg")(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = ws(char(';'))(input)?;
    Ok((input, name))
}

fn primitive_item(input: &str) -> IResult<&str, PrimitiveItem> {
    alt((
        map(parse_port_declaration, PrimitiveItem::Ports),
        map(primitive_register, PrimitiveItem::Register),
        map(primitive_initial, PrimitiveItem::Initial),
    ))(input)
}

/// A parse failure that stops the whole declaration, for a `primitive` whose
/// text is syntactically fine but does not describe a table.
fn malformed(input: &str) -> nom::Err<nom::error::Error<&str>> {
    nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::Verify))
}

/// Parses `primitive name (terminals); … endprimitive` into the module-shaped
/// value the rest of the front end works on.
pub fn parse_primitive_declaration(input: &str) -> IResult<&str, VerilogModule> {
    let (input, _) = ws(keyword("primitive"))(input)?;
    let (input, name) = ws(identifier)(input)?;
    let (input, header) = parse_port_header(input)?;
    let (input, _) = ws(char(';'))(input)?;

    let (input, items) = many0(ws(primitive_item))(input)?;
    let (input, rows) = delimited(
        ws(keyword("table")),
        many0(ws(table_row)),
        ws(keyword("endtable")),
    )(input)?;
    let (input, _) = ws(keyword("endprimitive"))(input)?;

    let mut declared = Vec::new();
    let mut registered = None;
    let mut initial = None;
    for item in items {
        match item {
            PrimitiveItem::Ports(ports) => declared.extend(ports),
            PrimitiveItem::Register(name) => registered = Some(name),
            PrimitiveItem::Initial(value) => initial = Some(value),
        }
    }
    let ports = reconcile_ports(header, declared).map_err(|_| malformed(input))?;

    // The output is the first terminal, and the only one: that is what a UDP
    // is, so a header that says otherwise is not one.
    let Some((output, inputs)) = ports.split_first() else {
        return Err(malformed(input));
    };
    if output.direction != PortDirection::Output
        || inputs.is_empty()
        || inputs
            .iter()
            .any(|port| port.direction != PortDirection::Input)
    {
        return Err(malformed(input));
    }

    let sequential = registered.is_some()
        || output.net_type == Some(NetType::Reg)
        || rows.iter().any(|row| row.current.is_some());
    for row in &rows {
        if row.inputs.len() != inputs.len()
            || row.current.is_some() != sequential
            || (!sequential
                && (row.output == OutputSymbol::NoChange
                    || row
                        .inputs
                        .iter()
                        .any(|symbol| matches!(symbol, InputSymbol::Edge(_)))))
        {
            return Err(malformed(input));
        }
    }
    if initial.is_some() && !sequential {
        return Err(malformed(input));
    }

    Ok((
        input,
        VerilogModule {
            identifier: name,
            ports,
            statements: vec![ModuleStatement::PrimitiveTable(UdpTable {
                sequential,
                initial,
                rows,
            })],
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::helpers::assert_parses;
    use crate::register::Z;

    fn table_of(source: &str) -> UdpTable {
        let mut module = assert_parses(parse_primitive_declaration, source);
        assert_eq!(module.statements.len(), 1, "a UDP has one statement");
        match module.statements.remove(0) {
            ModuleStatement::PrimitiveTable(table) => table,
            other => panic!("expected a table, got {:?}", other),
        }
    }

    const MUX: &str = "
        primitive mux (q, sel, a, b);
          output q;
          input sel, a, b;
          table
            0 1 ? : 1 ;
            0 0 ? : 0 ;
            1 ? 1 : 1 ;
            1 ? 0 : 0 ;
          endtable
        endprimitive
    ";

    /// The header and the body declarations reconcile exactly the way a
    /// Verilog-1995 module's do, and what comes out is a module with one
    /// statement in it.
    #[test]
    fn test_a_primitive_is_a_module_with_a_table() {
        let module = assert_parses(parse_primitive_declaration, MUX);
        assert_eq!(module.identifier, "mux".into());
        assert_eq!(module.ports.len(), 4);
        assert_eq!(module.ports[0].direction, PortDirection::Output);
        assert_eq!(module.ports[0].identifier, "q".into());
        assert!(module.ports[1..]
            .iter()
            .all(|port| port.direction == PortDirection::Input));
        assert!(matches!(
            module.statements.as_slice(),
            [ModuleStatement::PrimitiveTable(_)]
        ));
    }

    /// An ANSI header is the other spelling of the same thing, and `output reg`
    /// in one is what makes the primitive sequential.
    #[test]
    fn test_an_ansi_header() {
        let table = table_of(
            "primitive m2 (output out, input a, input b);
               table
                 0 0 : 0;
                 1 ? : 1;
               endtable
             endprimitive",
        );
        assert!(!table.sequential);
        assert_eq!(table.rows.len(), 2);

        let table = table_of(
            "primitive dff (output reg q, input c, input d);
               table
                 p 0 : ? : 0 ;
                 p 1 : ? : 1 ;
                 n ? : ? : - ;
               endtable
             endprimitive",
        );
        assert!(table.sequential);
        assert_eq!(table.rows[2].output, OutputSymbol::NoChange);
    }

    /// Every symbol a row may carry, including the ones written without a
    /// separator between them.
    #[test]
    fn test_row_symbols() {
        let table = table_of(
            "primitive jk (q, cp, j, k);
               output q;
               reg q;
               input cp, j, k;
               initial q = 1'b0;
               table
                 (01) ?? : ? : 1 ;
                 (?0) b* : 0 : - ;
                 r    1x : 1 : 0 ;
                 f    n0 : x : x ;
               endtable
             endprimitive",
        );
        assert!(table.sequential);
        assert!(table.initial.is_some());
        assert_eq!(
            table.rows[0].inputs,
            vec![
                InputSymbol::Edge(EdgeSymbol::Pair(LevelSymbol::Zero, LevelSymbol::One)),
                InputSymbol::Level(LevelSymbol::Any),
                InputSymbol::Level(LevelSymbol::Any),
            ]
        );
        assert_eq!(
            table.rows[1].inputs,
            vec![
                InputSymbol::Edge(EdgeSymbol::Pair(LevelSymbol::Any, LevelSymbol::Zero)),
                InputSymbol::Level(LevelSymbol::Binary),
                InputSymbol::Edge(EdgeSymbol::Any),
            ]
        );
        assert_eq!(table.rows[1].current, Some(LevelSymbol::Zero));
        assert_eq!(
            table.rows[2].inputs[0],
            InputSymbol::Edge(EdgeSymbol::Pair(LevelSymbol::Zero, LevelSymbol::One))
        );
        assert_eq!(
            table.rows[3].inputs,
            vec![
                InputSymbol::Edge(EdgeSymbol::Pair(LevelSymbol::One, LevelSymbol::Zero)),
                InputSymbol::Edge(EdgeSymbol::Negative),
                InputSymbol::Level(LevelSymbol::Zero),
            ]
        );
    }

    /// Comments are legal at every token boundary here as everywhere else,
    /// which matters: a table is almost always written with its columns
    /// labelled by one.
    #[test]
    fn test_comments_in_a_table() {
        let table = table_of(
            "primitive p (o, a, b); // a comment
               output o;
               input  a, b;
               table
               // a b : o
                  0 0 : 1 ;   // both low
                  ? 1 : 0 ;
               endtable
             endprimitive",
        );
        assert_eq!(table.rows.len(), 2);
    }

    /// A row whose shape disagrees with the rest of the declaration is a parse
    /// error, not a row that quietly means something else.
    #[test]
    fn test_a_malformed_table_is_rejected() {
        // Three input columns declared, two written.
        assert!(parse_primitive_declaration(
            "primitive p (o, a, b, c);
               output o; input a, b, c;
               table 0 0 : 1 ; endtable
             endprimitive"
        )
        .is_err());
        // `-` needs a state to hold.
        assert!(parse_primitive_declaration(
            "primitive p (o, a);
               output o; input a;
               table 0 : - ; endtable
             endprimitive"
        )
        .is_err());
        // An edge needs one too.
        assert!(parse_primitive_declaration(
            "primitive p (o, a);
               output o; input a;
               table (01) : 1 ; endtable
             endprimitive"
        )
        .is_err());
        // A sequential table's rows all carry a state, or none do.
        assert!(parse_primitive_declaration(
            "primitive p (o, a);
               output o; reg o; input a;
               table 0 : ? : 1 ; 1 : 0 ; endtable
             endprimitive"
        )
        .is_err());
        // The first terminal is the output, and there is exactly one.
        assert!(parse_primitive_declaration(
            "primitive p (o, a);
               input o, a;
               table 0 0 : 1 ; endtable
             endprimitive"
        )
        .is_err());
    }

    /// The lookup iverilog 12.0 performs, measured against it: `?` matches an
    /// unknown input, a `z` is read as an `x` before matching, and a
    /// combination nothing matches is `x`.
    #[test]
    fn test_combinational_lookup() {
        let table = table_of(MUX);
        assert_eq!(table.combinational_output(&[ZERO, ONE, ZERO]), ONE);
        assert_eq!(table.combinational_output(&[ZERO, ZERO, ONE]), ZERO);
        assert_eq!(table.combinational_output(&[ONE, ZERO, ONE]), ONE);
        // `sel` unknown matches no row at all.
        assert_eq!(table.combinational_output(&[X, ONE, ONE]), X);
        // A `?` column matches an unknown.
        assert_eq!(table.combinational_output(&[ZERO, ONE, X]), ONE);
    }

    /// Where rows disagree — which the LRM leaves undefined — iverilog answers
    /// `0` before `1` and either before `x`, whichever order they were written
    /// in. A row whose output is `x` therefore says nothing a missing row does
    /// not already say.
    #[test]
    fn test_conflicting_rows_follow_iverilog() {
        for source in [
            "primitive p (o, a); output o; input a;
               table ? : 1 ; 0 : 0 ; endtable endprimitive",
            "primitive p (o, a); output o; input a;
               table 0 : 0 ; ? : 1 ; endtable endprimitive",
        ] {
            let table = table_of(source);
            assert_eq!(table.combinational_output(&[ZERO]), ZERO);
            assert_eq!(table.combinational_output(&[ONE]), ONE);
        }
        let table = table_of(
            "primitive p (o, a); output o; input a;
               table 0 : x ; ? : 1 ; endtable endprimitive",
        );
        assert_eq!(table.combinational_output(&[ZERO]), ONE);
    }

    /// A `b` column is the two known levels and nothing else, and no column at
    /// all matches a `z` — which is exactly why the caller has to read a
    /// floating input as an `x` before it asks.
    #[test]
    fn test_binary_and_unknown_columns() {
        assert!(LevelSymbol::Binary.matches(ZERO));
        assert!(LevelSymbol::Binary.matches(ONE));
        assert!(!LevelSymbol::Binary.matches(X));
        assert!(!LevelSymbol::Binary.matches(Z));
        assert!(LevelSymbol::Unknown.matches(X));
        assert!(!LevelSymbol::Unknown.matches(Z));
        assert!(LevelSymbol::Any.matches(X));
    }

    /// A word that merely starts with a keyword is an ordinary identifier.
    #[test]
    fn test_keywords_are_whole_words() {
        assert!(parse_primitive_declaration("primitives p (o, a);").is_err());
    }
}

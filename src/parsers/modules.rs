use std::collections::HashMap;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, satisfy},
    combinator::{map, not, opt},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

use super::{
    assignment::ContinuousAssignment,
    constants::VerilogConstant,
    delay::delay_operand,
    expr::{verilog_expression, Expression},
    gates::{GateInstance, GateInstantiation, GateKind},
    identifier::{identifier, Identifier},
    keywords::is_reserved_word,
    parameter::parse_parameter_port_list,
    preprocessor::Timescale,
    simple::{range, signedness, ws, ws_and_comments, Range},
    statements::{parse_module_statement, ModuleStatement},
};

#[derive(Debug, PartialEq)]
pub struct VerilogModule {
    pub identifier: Identifier,
    pub ports: Vec<Port>,
    pub statements: Vec<ModuleStatement>,
    /// The `` `timescale `` in force where this module was written, which is
    /// what `$printtimescale` reports. `None` is the default, `1s / 1s`.
    ///
    /// A backtick directive is not part of the grammar, so the parser cannot
    /// see one and always leaves this `None`;
    /// [`parse_expanded`](crate::parsers::source::parse_expanded) stamps it
    /// afterwards from the positions the preprocessor recorded. A module built
    /// by a test therefore carries the default, which is what a design with no
    /// directive in it has.
    pub timescale: Option<Timescale>,
    /// The `` `unconnected_drive `` in force where this module was written,
    /// which says what an *unconnected input port* of it reads: `Some(true)`
    /// for `pull1`, `Some(false)` for `pull0`, and `None` — the default — for
    /// the `z` a port nothing drives otherwise has.
    ///
    /// It is stamped exactly the way [`VerilogModule::timescale`] is, and for
    /// the same reason: the directive is positional and the grammar never sees
    /// one. It belongs to the module's *declaration* rather than to the
    /// instantiation, which is what IEEE 1364-2005 §19.9 asks for.
    pub unconnected_drive: Option<bool>,
}

#[derive(Debug, PartialEq)]
pub struct Port {
    pub direction: PortDirection,
    pub net_type: Option<NetType>,
    pub range: Range,
    pub identifier: Identifier,
    /// Whether the declaration carried a `signed` qualifier —
    /// `input signed [3:0] a`.
    pub signed: bool,
    /// The default value of `output reg [31:0] x = 1;`.
    ///
    /// It belongs to the *name* rather than to the declaration, exactly as a
    /// `reg` or `wire` initialiser does, so `output reg x = 1, y = 2;` gives
    /// the two ports different starting values. A variable port takes it once,
    /// at elaboration; a net port takes it as a continuous assignment — the
    /// same split `reg a = e;` and `wire a = e;` already make.
    pub init: Option<Expression>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PortDirection {
    Input,
    Output,
    InOut,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NetType {
    Wire,
    Reg,
}

/// A character that may continue an identifier, so a keyword followed by one
/// is not a keyword at all: `input` in `inputs`, `reg` in `reg_a`.
fn identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '$'
}

fn parse_port_direction(input: &str) -> IResult<&str, PortDirection> {
    terminated(
        alt((
            map(tag("input"), |_| PortDirection::Input),
            map(tag("output"), |_| PortDirection::Output),
            map(tag("inout"), |_| PortDirection::InOut),
        )),
        not(satisfy(identifier_char)),
    )(input)
}

/// The type a port declaration may name, and the width that comes with it.
///
/// `wire` and `reg` bring no width of their own — the declaration's own range
/// says it — while `integer` and `time` *are* a width, the same fixed widths
/// they carry as ordinary declarations: thirty-two bits signed and sixty-four
/// unsigned. An `output integer d;` is a variable, so all three of the latter
/// come back as [`NetType::Reg`].
fn parse_net_type(input: &str) -> IResult<&str, (NetType, Option<PortType>)> {
    terminated(
        alt((
            map(tag("wire"), |_| (NetType::Wire, None)),
            map(tag("reg"), |_| (NetType::Reg, None)),
            map(tag("logic"), |_| (NetType::Reg, None)),
            map(tag("integer"), |_| (NetType::Reg, Some(PortType::Integer))),
            map(tag("time"), |_| (NetType::Reg, Some(PortType::Time))),
        )),
        not(satisfy(identifier_char)),
    )(input)
}

/// A port's keyword-led data type, which fixes its width and its signedness.
#[derive(Debug, PartialEq, Clone, Copy)]
enum PortType {
    /// Thirty-two bits, signed.
    Integer,
    /// Sixty-four bits, unsigned.
    Time,
}

impl PortType {
    fn range(self) -> Range {
        match self {
            PortType::Integer => Range::Constant(31, 0),
            PortType::Time => Range::Constant(63, 0),
        }
    }

    fn signed(self) -> bool {
        matches!(self, PortType::Integer)
    }
}

fn parse_port(input: &str) -> IResult<&str, Port> {
    let (input, direction) = parse_port_direction(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, declared) = opt(parse_net_type)(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, signed) = signedness(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, range) = opt(range)(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, identifier) = identifier(input)?;
    let (input, init) = opt(preceded(ws(char('=')), verilog_expression))(input)?;
    let (net_type, declared_type) = match declared {
        Some((net_type, declared_type)) => (Some(net_type), declared_type),
        None => (None, None),
    };
    Ok((
        input,
        Port {
            direction,
            net_type,
            range: declared_type
                .map(PortType::range)
                .or(range)
                .unwrap_or(Range::SINGLE_BIT),
            identifier,
            signed: signed || declared_type.is_some_and(PortType::signed),
            init,
        },
    ))
}

/// What a port declaration says before the name, kept so the ports after it
/// can inherit it.
#[derive(Clone)]
struct PortQualifiers {
    direction: PortDirection,
    net_type: Option<NetType>,
    range: Range,
    signed: bool,
}

/// One entry of an ANSI header after the first: either a fresh declaration, or
/// a bare name that inherits the one before it. A default value belongs to the
/// name, so it is never inherited.
fn parse_port_item(
    input: &str,
) -> IResult<&str, (Option<PortQualifiers>, Identifier, Option<Expression>)> {
    if let Ok((rest, port)) = parse_port(input) {
        let qualifiers = PortQualifiers {
            direction: port.direction,
            net_type: port.net_type,
            range: port.range,
            signed: port.signed,
        };
        return Ok((rest, (Some(qualifiers), port.identifier, port.init)));
    }
    let (input, name) = ws(identifier)(input)?;
    let (input, init) = opt(preceded(ws(char('=')), verilog_expression))(input)?;
    Ok((input, (None, name, init)))
}

/// An ANSI header. The **first** entry must carry a direction — that is what
/// tells this header from the Verilog-1995 spelling, which is bare names — and
/// every entry after it may either re-declare or inherit.
///
/// `module m(input clk, reset, input [7:0] d, output reg [7:0] q);` declares
/// `reset` as a one-bit input, because it inherits what `clk` said, and `d`
/// re-qualifies. That is the same carry-forward rule an ordinary declaration
/// list follows.
fn parse_ports(input: &str) -> IResult<&str, Vec<Port>> {
    let (input, _) = ws(char('('))(input)?;
    // `module m();` — an empty ANSI list, with nothing to inherit from.
    if let Ok((rest, _)) = ws(char(')'))(input) {
        return Ok((rest, Vec::new()));
    }
    let (input, first) = parse_port(input)?;
    let (input, rest) = many0(preceded(ws(char(',')), parse_port_item))(input)?;
    let (input, _) = ws(char(')'))(input)?;

    let mut carried = PortQualifiers {
        direction: first.direction,
        net_type: first.net_type,
        range: first.range.clone(),
        signed: first.signed,
    };
    let mut ports = Vec::with_capacity(rest.len() + 1);
    ports.push(Port {
        direction: carried.direction,
        net_type: carried.net_type,
        range: carried.range.clone(),
        identifier: first.identifier,
        signed: carried.signed,
        init: first.init,
    });
    for (qualifiers, identifier, init) in rest {
        if let Some(qualifiers) = qualifiers {
            carried = qualifiers;
        }
        ports.push(Port {
            direction: carried.direction,
            net_type: carried.net_type,
            range: carried.range.clone(),
            identifier,
            signed: carried.signed,
            init,
        });
    }
    Ok((input, ports))
}

/// One reference inside a port expression: `b`, `arg[119:96]`, `a[0]`.
#[derive(Debug, PartialEq, Clone)]
pub(crate) struct PortReference {
    name: Identifier,
    select: Option<PortSelect>,
}

#[derive(Debug, PartialEq, Clone)]
enum PortSelect {
    /// `arg[119:96]` — a constant range, which the header's width is made of.
    Part(Range),
    /// `a[0]`.
    Bit(Expression),
}

impl PortReference {
    /// The reference as the expression an assignment or a switch terminal
    /// names it by.
    fn expression(&self) -> Expression {
        let bound = |value: &i64| Box::new(Expression::Constant(VerilogConstant::from_int(*value)));
        match &self.select {
            None => Expression::Identifier(self.name.clone()),
            Some(PortSelect::Bit(index)) => {
                Expression::BitSelect(self.name.clone(), Box::new(index.clone()))
            }
            Some(PortSelect::Part(Range::Constant(high, low))) => {
                Expression::PartSelect(self.name.clone(), bound(high), bound(low))
            }
            Some(PortSelect::Part(Range::Expressions(high, low))) => {
                Expression::PartSelect(self.name.clone(), high.clone(), low.clone())
            }
            Some(PortSelect::Part(Range::Packed(_, _))) => {
                unreachable!("a header select is read with `range`, which reads one bracket")
            }
        }
    }
}

/// One entry of a Verilog-1995 header.
///
/// IEEE 1364-2005's `port` is a *port expression* — a reference to what the
/// body declares, a part of one, or a concatenation of those — optionally
/// behind an external name: `.a({b, c})`. An entry may also be left blank.
/// Nearly every header is the first shape only, which is `Named`.
#[derive(Debug, PartialEq, Clone)]
pub(crate) enum HeaderPort {
    /// `a` — a port that is the whole of what the body declares under the
    /// same name.
    Named(Identifier),
    /// `.a(b)`, `{a, b}`, `arg[119:96]`, `.a()` or a blank entry — a port whose
    /// connection is *carried* to the body's declarations rather than being
    /// one of them. See [`reconcile_ports`].
    Expression {
        name: Option<Identifier>,
        parts: Vec<PortReference>,
    },
}

fn port_reference(input: &str) -> IResult<&str, PortReference> {
    let (input, name) = identifier(input)?;
    let (input, select) = opt(preceded(
        ws_and_comments,
        alt((
            map(range, PortSelect::Part),
            map(
                delimited(ws(char('[')), verilog_expression, ws(char(']'))),
                PortSelect::Bit,
            ),
        )),
    ))(input)?;
    Ok((input, PortReference { name, select }))
}

fn port_expression(input: &str) -> IResult<&str, Vec<PortReference>> {
    alt((
        delimited(
            ws(char('{')),
            separated_list1(ws(char(',')), ws(port_reference)),
            ws(char('}')),
        ),
        map(port_reference, |reference| vec![reference]),
    ))(input)
}

fn header_port(input: &str) -> IResult<&str, HeaderPort> {
    alt((
        map(
            preceded(
                char('.'),
                pair(
                    identifier,
                    delimited(ws(char('(')), opt(ws(port_expression)), ws(char(')'))),
                ),
            ),
            |(name, parts)| HeaderPort::Expression {
                name: Some(name),
                parts: parts.unwrap_or_default(),
            },
        ),
        map(port_expression, |mut parts| {
            if parts.len() == 1 && parts[0].select.is_none() {
                HeaderPort::Named(parts.remove(0).name)
            } else {
                HeaderPort::Expression { name: None, parts }
            }
        }),
        // A blank entry — `(a, , b)`, or the trailing one of `(a, )`.
        map(ws_and_comments, |_| HeaderPort::Expression {
            name: None,
            parts: Vec::new(),
        }),
    ))(input)
}

/// A Verilog-1995 header, whose directions and widths are declared as
/// statements in the body.
fn parse_port_names(input: &str) -> IResult<&str, Vec<HeaderPort>> {
    delimited(
        ws(char('(')),
        separated_list1(char(','), ws(header_port)),
        ws(char(')')),
    )(input)
}

/// The two spellings of a module header. Both are normalised to a single
/// `Vec<Port>` by [`reconcile_ports`] before the module is handed on, so
/// nothing downstream has to know which one was written.
#[derive(Debug, PartialEq)]
pub(crate) enum PortHeader {
    /// `module m(input wire [3:0] a);` — direction and width in the header.
    Ansi(Vec<Port>),
    /// `module m(a, b);` — port expressions, nearly always plain names.
    NonAnsi(Vec<HeaderPort>),
}

pub(crate) fn parse_port_header(input: &str) -> IResult<&str, PortHeader> {
    alt((
        map(parse_ports, PortHeader::Ansi),
        map(parse_port_names, PortHeader::NonAnsi),
    ))(input)
}

/// A Verilog-1995 body port declaration: `input C;`, `output reg [11:0] h, g;`.
/// One declaration may name several ports, which then share its direction,
/// net type and width.
pub fn parse_port_declaration(input: &str) -> IResult<&str, Vec<Port>> {
    let (input, direction) = parse_port_direction(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, declared) = opt(parse_net_type)(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, signed) = signedness(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, port_range) = opt(range)(input)?;
    let (input, names) = separated_list1(ws(char(',')), ws(declared_port))(input)?;
    let (input, _) = ws(char(';'))(input)?;
    let (net_type, declared_type) = match declared {
        Some((net_type, declared_type)) => (Some(net_type), declared_type),
        None => (None, None),
    };
    // The width and the signedness of an `integer` or a `time` are the type's,
    // not the declaration's, exactly as they are for an ordinary one.
    let port_range = declared_type.map(PortType::range).or(port_range);
    let signed = signed || declared_type.is_some_and(PortType::signed);
    Ok((
        input,
        names
            .into_iter()
            .map(|(identifier, init)| Port {
                direction,
                net_type,
                range: port_range.clone().unwrap_or(Range::SINGLE_BIT),
                identifier,
                signed,
                init,
            })
            .collect(),
    ))
}

/// One declared port name plus the default value that belongs to it.
fn declared_port(input: &str) -> IResult<&str, (Identifier, Option<Expression>)> {
    let (input, name) = identifier(input)?;
    let (input, init) = opt(preceded(ws(char('=')), verilog_expression))(input)?;
    Ok((input, (name, init)))
}

/// Why a module's header and its body port declarations do not agree.
#[derive(Debug, PartialEq)]
pub enum PortReconciliationError {
    /// A header name that no body declaration gives a direction.
    MissingDirection(Identifier),
    /// The same port declared twice.
    Duplicate(Identifier),
    /// The port expression at this header position cannot be carried to the
    /// body: its parts disagree about direction, its width is not made of
    /// constants, its external name is also a name the body declares, or it
    /// is an `inout` that is not a single reference.
    PortExpression(usize),
}

/// Rejects a name that appears more than once in the same list.
fn check_unique(names: &[Identifier]) -> Result<(), PortReconciliationError> {
    for (at, name) in names.iter().enumerate() {
        if names[..at].contains(name) {
            return Err(PortReconciliationError::Duplicate(name.clone()));
        }
    }
    Ok(())
}

/// A module's ports in header order, and the body direction declarations no
/// header port claims.
#[derive(Debug, PartialEq)]
pub(crate) struct ReconciledPorts {
    pub ports: Vec<Port>,
    /// `module test; output reg a;` — a direction declared for a name the
    /// header does not list. That is not a port, because nothing can connect
    /// to it, and iverilog 12.0 reads it as the ordinary declaration it would
    /// be without the direction: a net, or a variable when it names `reg`,
    /// `integer` or `time` — silently, even under `-Wall`. It stays in the
    /// body as a [`ModuleStatement::PortDeclaration`] and `elaborate` declares
    /// it as a local.
    pub locals: Vec<Port>,
    /// What carries a port expression's connection to the declarations it
    /// names — see [`reconcile_ports`].
    pub connections: Vec<ModuleStatement>,
}

/// Folds a module's header and its body declarations into the one
/// `Vec<Port>` an ANSI header produces directly, in header order.
///
/// A `reg` declaration that names a port is *not* one of `declared` — it is an
/// ordinary body statement, and an output backed by a register is exactly what
/// it means — so it is neither a conflict nor a second port here.
pub(crate) fn reconcile_ports(
    header: PortHeader,
    declared: Vec<Port>,
) -> Result<ReconciledPorts, PortReconciliationError> {
    let entries = match header {
        PortHeader::Ansi(ports) => {
            let names: Vec<Identifier> = ports.iter().map(|p| p.identifier.clone()).collect();
            check_unique(&names)?;
            // An ANSI port has its direction already, so a body declaration
            // of the same name is a second one.
            if let Some(again) = declared
                .iter()
                .find(|port| names.contains(&port.identifier))
            {
                return Err(PortReconciliationError::Duplicate(again.identifier.clone()));
            }
            let locals: Vec<Identifier> = declared.iter().map(|p| p.identifier.clone()).collect();
            check_unique(&locals)?;
            return Ok(ReconciledPorts {
                ports,
                locals: declared,
                connections: Vec::new(),
            });
        }
        PortHeader::NonAnsi(entries) => entries,
    };

    // A name the header lists a second time is one net reached through two
    // ports (`module id(a, a); inout a;`, corpus `inout`), so the repeat is
    // carried like any other port expression; `.a(a)` is just `a`.
    let mut named: Vec<Identifier> = Vec::new();
    let entries: Vec<HeaderPort> = entries
        .into_iter()
        .map(|entry| match entry {
            HeaderPort::Named(name) if named.contains(&name) => HeaderPort::Expression {
                name: None,
                parts: vec![PortReference { name, select: None }],
            },
            HeaderPort::Expression {
                name: Some(name),
                parts,
            } if !named.contains(&name)
                && parts.len() == 1
                && parts[0].select.is_none()
                && parts[0].name == name =>
            {
                named.push(name.clone());
                HeaderPort::Named(name)
            }
            HeaderPort::Named(name) => {
                named.push(name.clone());
                HeaderPort::Named(name)
            }
            other => other,
        })
        .collect();

    let mut ports: Vec<Option<Port>> = entries.iter().map(|_| None).collect();
    let mut locals: Vec<Port> = Vec::new();
    for port in declared {
        let at = entries
            .iter()
            .position(|entry| matches!(entry, HeaderPort::Named(name) if *name == port.identifier));
        let Some(at) = at else {
            if locals
                .iter()
                .any(|local| local.identifier == port.identifier)
            {
                return Err(PortReconciliationError::Duplicate(port.identifier));
            }
            locals.push(port);
            continue;
        };
        if ports[at].is_some() {
            return Err(PortReconciliationError::Duplicate(port.identifier));
        }
        ports[at] = Some(port);
    }

    let mut connections = Vec::new();
    let mut resolved = Vec::with_capacity(entries.len());
    for (at, entry) in entries.iter().enumerate() {
        let port = match entry {
            HeaderPort::Named(name) => ports[at]
                .take()
                .ok_or_else(|| PortReconciliationError::MissingDirection(name.clone()))?,
            HeaderPort::Expression { name, parts } => {
                let declared = |reference: &PortReference| {
                    ports
                        .iter()
                        .flatten()
                        .chain(resolved.iter())
                        .chain(locals.iter())
                        .find(|port: &&Port| port.identifier == reference.name)
                };
                carried_port(at, name.as_ref(), parts, declared, &mut connections)?
            }
        };
        resolved.push(port);
    }
    Ok(ReconciledPorts {
        ports: resolved,
        locals,
        connections,
    })
}

/// The port a header [`HeaderPort::Expression`] stands for, and the statement
/// that carries its connection to the declarations the expression names.
///
/// The port is an ordinary one — external name or `$port<position>`, which no
/// design identifier can spell — and what it names in the body is left to be
/// declared as locals. The connection then runs in the port's direction: an
/// `input` is `assign {parts} = port;`, an `output` is `assign port =
/// {parts};`, and an `inout` is a `tran` per bit, because a port that is read
/// as well as written cannot be carried by one assignment. Everything past the
/// parser therefore sees ports, locals, assignments and switches it already
/// knew, and nothing learns that the header was not a list of names. A blank
/// entry is a one-bit input that nothing inside reads.
fn carried_port<'a>(
    at: usize,
    name: Option<&Identifier>,
    parts: &[PortReference],
    declared: impl Fn(&PortReference) -> Option<&'a Port>,
    connections: &mut Vec<ModuleStatement>,
) -> Result<Port, PortReconciliationError> {
    let refused = || PortReconciliationError::PortExpression(at);
    let identifier = match name {
        Some(name) => name.clone(),
        None => Identifier::new(format!("$port{at}")),
    };
    if declared(&PortReference {
        name: identifier.clone(),
        select: None,
    })
    .is_some()
    {
        return Err(refused());
    }

    let mut direction = None;
    let mut width = 0;
    let mut range = None;
    for part in parts {
        let port = declared(part)
            .ok_or_else(|| PortReconciliationError::MissingDirection(part.name.clone()))?;
        if direction.is_some_and(|direction| direction != port.direction) {
            return Err(refused());
        }
        direction = Some(port.direction);
        // A port that is the whole of one declaration takes its range as
        // written, parameters and all; anything else is a sum of widths,
        // which have to be constants to be added up here.
        if parts.len() == 1 && part.select.is_none() {
            range = Some(port.range.clone());
            continue;
        }
        width += match &part.select {
            None => port.range.constant().map(range_width),
            Some(PortSelect::Part(select)) => select.constant().map(range_width),
            Some(PortSelect::Bit(_)) => Some(1),
        }
        .ok_or_else(refused)?;
    }
    let direction = direction.unwrap_or(PortDirection::Input);
    // Only a port that is read as well as written can share its net with
    // another: two inputs onto one net would have the child drive its parent.
    if let [repeat] = parts {
        if name.is_none() && repeat.select.is_none() && direction != PortDirection::InOut {
            return Err(PortReconciliationError::Duplicate(repeat.name.clone()));
        }
    }
    let range = match range {
        Some(range) => range,
        None if parts.is_empty() => Range::SINGLE_BIT,
        None => Range::Constant(width - 1, 0),
    };
    let port_expression = Expression::Identifier(identifier.clone());
    let inside = match parts {
        [] => None,
        [part] => Some(part.expression()),
        parts => Some(Expression::Concatenation(
            parts.iter().map(PortReference::expression).collect(),
        )),
    };
    if let Some(inside) = inside {
        connections.push(match direction {
            PortDirection::Input => ModuleStatement::Assignment(vec![ContinuousAssignment::new(
                inside,
                port_expression,
            )]),
            PortDirection::Output => ModuleStatement::Assignment(vec![ContinuousAssignment::new(
                port_expression,
                inside,
            )]),
            PortDirection::InOut => {
                if parts.len() != 1 {
                    return Err(refused());
                }
                ModuleStatement::GateInstantiation(vec![GateInstantiation {
                    kind: GateKind::Tran,
                    strength: None,
                    delay: None,
                    instance: GateInstance {
                        name: None,
                        range: (range != Range::SINGLE_BIT).then(|| range.clone()),
                        terminals: vec![port_expression, inside],
                    },
                }])
            }
        });
    }
    Ok(Port {
        direction,
        net_type: None,
        range,
        identifier,
        signed: false,
        init: None,
    })
}

/// How many bits `[high:low]` spans, whichever way round it was written.
fn range_width((high, low): (i64, i64)) -> i64 {
    (high - low).abs() + 1
}

/// Parses `module name (ports); … endmodule`.
///
/// The port list is optional: `module top;` is legal Verilog and is how most
/// testbenches are written, since a top-level module has nothing to connect to.
/// It may be written either way round — ANSI, with the directions in the
/// header, or Verilog-1995, with bare names in the header and the directions
/// declared in the body. Both leave the same `Vec<Port>` behind.
pub fn parse_module_declaration(input: &str) -> IResult<&str, VerilogModule> {
    let (input, _) = ws(tag("module"))(input)?;
    let (input, mod_identifier) = ws(identifier)(input)?;
    // `module m #(parameter W = 8) (…);` — an ANSI parameter port list. It
    // becomes ordinary parameter *statements* below, so nothing downstream can
    // tell one declared here from one declared in the body.
    let (input, parameter_ports) = opt(parse_parameter_port_list)(input)?;
    let (input, header) = map(opt(parse_port_header), |header| {
        header.unwrap_or(PortHeader::Ansi(Vec::new()))
    })(input)?;
    let (input, _) = ws(tag(";"))(input)?;
    let (input, body) = many0(ws(parse_module_statement))(input)?;
    // Past `module <name>;` no other production can match this text, so a
    // missing `endmodule` is a hard failure rather than a backtrack. That is
    // also what makes the diagnostic useful: the position it carries is where
    // the body stopped — the first statement the grammar could not read —
    // instead of the `module` keyword the caller would otherwise report.
    let (input, _) = ws(tag("endmodule"))(input).map_err(|error: nom::Err<_>| match error {
        nom::Err::Error(inner) => nom::Err::Failure(inner),
        other => other,
    })?;

    // A body port declaration *is* a port, so it is lifted out of the body
    // rather than left in it as a second description of the same thing.
    let mut declared = Vec::new();
    let mut statements = Vec::new();
    // The parameter ports go in front of the body, because a body declaration
    // may be written in terms of one — `#(parameter W = 8)` with
    // `reg [W-1:0] r;` below it — and elaboration reads them in order.
    let header_parameters = parameter_ports.is_some();
    if let Some(parameters) = parameter_ports {
        statements.push(ModuleStatement::ParameterDeclaration(parameters));
    }
    for statement in body {
        match statement {
            ModuleStatement::PortDeclaration(ports) => declared.extend(ports),
            other => statements.push(other),
        }
    }
    let ReconciledPorts {
        ports,
        locals,
        connections,
    } = reconcile_ports(header, declared).map_err(|_| {
        nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::Verify))
    })?;
    // In front of the body, where a port's own declaration would be: a `reg`
    // naming the same name is a second declaration of it, and has to run
    // after this one to give the variable its `x`.
    if !locals.is_empty() {
        statements.insert(
            usize::from(header_parameters),
            ModuleStatement::PortDeclaration(locals),
        );
    }
    statements.extend(connections);

    Ok((
        input,
        VerilogModule {
            identifier: mod_identifier,
            ports,
            statements,
            timescale: None,
            unconnected_drive: None,
        },
    ))
}

#[derive(Debug, PartialEq, Clone)]
pub enum ModuleInitArguments {
    NoArgs,
    /// `dut u(a, b)` — connections bound in port-declaration order.
    ///
    /// An element is `None` when the position was written blank —
    /// `two U7 (,)`, `two U8 (w3,)`, `two U9 (,w4)` — which leaves that port
    /// unconnected. A blank keeps its *place* in the list rather than being
    /// dropped: the whole meaning of a positional list is the index, so
    /// dropping one would silently bind every later connection to the wrong
    /// port.
    Positional(Vec<Option<Expression>>),
    Keyword(HashMap<Identifier, Expression>),
}

/// One element of a positional argument list, absent when it was written
/// blank. Whitespace and comments stand in for the expression, so `( , )` is
/// two blanks rather than a parse error.
fn positional_argument(input: &str) -> IResult<&str, Option<Expression>> {
    alt((
        map(verilog_expression, Some),
        map(ws_and_comments, |_| None),
    ))(input)
}

pub fn parse_positional_arguments(input: &str) -> IResult<&str, ModuleInitArguments> {
    let (rest, args) = separated_list1(tag(","), positional_argument)(input)?;
    // A single blank element is an *empty* argument block — `()` — not a
    // one-element list with a gap in it. Rejecting it here is what leaves
    // `parse_arguments` reporting `NoArgs`.
    if args.len() == 1 && args[0].is_none() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::SeparatedList,
        )));
    }
    Ok((rest, ModuleInitArguments::Positional(args)))
}

/// `.name(expr)`, or `.name()` — a named connection left blank.
///
/// A blank one is `None` rather than an error: for a port it means
/// *unconnected*, and for a parameter override it means *take the default*.
/// Both are expressed by the name simply not reaching the map, which is why
/// the caller drops it rather than storing an absence.
fn kw_arg(input: &str) -> IResult<&str, (Identifier, Option<Expression>)> {
    let (input, _) = tag(".")(input)?;
    let (input, identifier) = identifier(input)?;
    let (input, expression) =
        delimited(ws(char('(')), opt(ws(verilog_expression)), ws(char(')')))(input)?;
    Ok((input, (identifier, expression)))
}

pub fn parse_keyword_arguments(input: &str) -> IResult<&str, ModuleInitArguments> {
    map(separated_list1(tag(","), ws(kw_arg)), |args| {
        let mut map = HashMap::new();
        for (id, expr) in args {
            // A blank connection is the *absence* of a binding, so it is
            // dropped rather than stored: an unconnected port and a parameter
            // left at its default are both "this name was never bound".
            if let Some(expr) = expr {
                map.insert(id, expr);
            }
        }
        ModuleInitArguments::Keyword(map)
    })(input)
}

/// Parse a block of arguments, either positional or keyword, without delimiters
/// eg.
///
/// positional: `1,2,3`
///
/// keyword: `.a(1),.b(2),.c(3)`
pub fn parse_arguments(input: &str) -> IResult<&str, ModuleInitArguments> {
    map(
        opt(alt((parse_keyword_arguments, parse_positional_arguments))),
        |args| args.unwrap_or(ModuleInitArguments::NoArgs),
    )(input)
}

/// Parse a block of arguments, either positional or keyword
/// eg.
///
/// positional: (1,2,3)
///
/// keyword: (.a(1),.b(2),.c(3))
///
/// An empty block may hold whitespace and comments — `my_module ( );` is
/// how corpus `pr985` writes one — which no argument claims, so the closing
/// parenthesis skips them itself.
fn argument_block(input: &str) -> IResult<&str, ModuleInitArguments> {
    delimited(
        tag("("),
        parse_arguments,
        preceded(ws_and_comments, tag(")")),
    )(input)
}

/// The `#` an instantiation may carry: `#(.WIDTH(8))`, `#(8)` or a bare `#8`.
///
/// The unparenthesised form is a *delay value* — one number or name, nothing
/// more, exactly as it is in front of a statement. It means two different
/// things depending on what is being instantiated, and the grammar cannot tell
/// which: on a UDP it is the instance's delay, and on a module it sets the
/// first parameter. So it is parsed as the one-element positional override it
/// looks like, and `elaborate` reads it as a delay once it knows the child is
/// a primitive — which is the first point at which anything does know.
fn param_block(input: &str) -> IResult<&str, ModuleInitArguments> {
    let (input, _) = tag("#")(input)?;
    // `#` and its block are separate tokens, the same way `#` and a delay
    // value are: `bar # (.WIDTH(8)) u (…)` is legal.
    let (input, _) = ws_and_comments(input)?;
    alt((
        argument_block,
        map(delay_operand, |value| {
            ModuleInitArguments::Positional(vec![Some(value)])
        }),
    ))(input)
}

#[derive(Debug, PartialEq, Clone)]
pub struct ModuleInstantiation {
    pub module_name: Identifier,
    /// `None` for `p (q, d);` — legal for a user-defined primitive, whose
    /// instance name IEEE 1364-2005 makes optional, and not for a module.
    /// Only the module being instantiated can say which it is, so the parser
    /// accepts both and `elaborate` names the one and refuses the other, which
    /// is where iverilog 12.0 draws the line too ("Instantiation of module
    /// child requires an instance name").
    pub instance_name: Option<Identifier>,
    /// `inv u[3:0] (o, i);` — an array of instances, one per index. `None` for
    /// an ordinary instantiation.
    pub range: Option<Range>,
    pub parameters: ModuleInitArguments, // NB(meawoppl) parameters have tighter bounds than arguments (we don't check)
    pub arguments: ModuleInitArguments,
}

/// One instance of an instantiation list: an optional name, the array range
/// that only a named instance may carry, and the connections.
fn module_instance(
    input: &str,
) -> IResult<&str, (Option<Identifier>, Option<Range>, ModuleInitArguments)> {
    let (input, named) = opt(|input| {
        let (input, name) = identifier(input)?;
        let (input, _) = ws_and_comments(input)?;
        let (input, range) = opt(range)(input)?;
        let (input, _) = ws_and_comments(input)?;
        Ok((input, (name, range)))
    })(input)?;
    let (input, arguments) = argument_block(input)?;
    let (instance_name, range) = match named {
        Some((name, range)) => (Some(name), range),
        None => (None, None),
    };
    Ok((input, (instance_name, range, arguments)))
}

/// `vdff #(.size(10)) mod_a (.out(a), …), mod_b (.out(b), …);` — one module
/// name and one parameter block shared by a list of instances, the way one
/// `reg [4:0]` is shared by every name in its list.
pub fn parse_module_instantiation_statement(
    input: &str,
) -> IResult<&str, Vec<ModuleInstantiation>> {
    let (input, module_name) = identifier(input)?;
    let (input, _) = ws_and_comments(input)?;

    let (input, parameters) = map(opt(param_block), |params| {
        params.unwrap_or(ModuleInitArguments::NoArgs)
    })(input)?;
    let (input, _) = ws_and_comments(input)?;

    let (input, instances) =
        separated_list1(ws(char(',')), terminated(module_instance, ws_and_comments))(input)?;
    // Without an instance name the statement is only a name and an argument
    // block, which is also the shape of a keyword-led form the grammar does
    // not know — so a reserved word is never taken for the module.
    if instances[0].0.is_none() && is_reserved_word(&module_name.name) {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Verify,
        )));
    }

    let (input, _) = ws(tag(";"))(input)?;

    Ok((
        input,
        instances
            .into_iter()
            .map(|(instance_name, range, arguments)| ModuleInstantiation {
                module_name: module_name.clone(),
                instance_name,
                range,
                parameters: parameters.clone(),
                arguments,
            })
            .collect(),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::helpers::{assert_parses, assert_parses_to};
    use std::fs;

    /// The single instance a one-instance statement declares.
    fn one_instance(source: &str) -> ModuleInstantiation {
        let mut instances = assert_parses(parse_module_instantiation_statement, source);
        assert_eq!(instances.len(), 1, "{}", source);
        instances.remove(0)
    }

    #[test]
    fn test_parse_port_direction() {
        assert_parses_to(parse_port_direction, "input", PortDirection::Input);
        assert_parses_to(parse_port_direction, "output", PortDirection::Output);
        assert_parses_to(parse_port_direction, "inout", PortDirection::InOut);
    }

    #[test]
    fn test_parse_net_type() {
        assert_parses_to(parse_net_type, "wire", (NetType::Wire, None));
        assert_parses_to(parse_net_type, "reg", (NetType::Reg, None));
        // `logic` is a `reg` as iverilog 12.0 reads it, and `logic_x` is a name.
        assert_parses_to(parse_net_type, "logic", (NetType::Reg, None));
        assert!(parse_net_type("logic_x").is_err());
        assert_parses_to(
            parse_net_type,
            "integer",
            (NetType::Reg, Some(PortType::Integer)),
        );
        assert_parses_to(parse_net_type, "time", (NetType::Reg, Some(PortType::Time)));
    }

    #[test]
    /// `output integer d;` and `output time e;` — a port whose keyword *is*
    /// its width, the same fixed widths an ordinary declaration gives them.
    #[test]
    fn test_a_port_may_name_a_data_type() {
        assert_parses_to(
            parse_port,
            "output integer d",
            Port {
                direction: PortDirection::Output,
                net_type: Some(NetType::Reg),
                range: Range::Constant(31, 0),
                identifier: "d".into(),
                signed: true,
                init: None,
            },
        );
        assert_parses_to(
            parse_port,
            "output time e",
            Port {
                direction: PortDirection::Output,
                net_type: Some(NetType::Reg),
                range: Range::Constant(63, 0),
                identifier: "e".into(),
                signed: false,
                init: None,
            },
        );
        // And in the Verilog-1995 body spelling, where one declaration may
        // name several ports and they share the type.
        let ports = assert_parses(parse_port_declaration, "output integer d, f;");
        assert_eq!(ports.len(), 2);
        for port in &ports {
            assert_eq!(port.range, Range::Constant(31, 0));
            assert!(port.signed);
        }
    }

    /// A default value belongs to the *name*, so `output reg x = 1, y = 2;`
    /// gives the two ports different starting values — and a bare name after
    /// one inherits the qualifiers but not the value.
    #[test]
    fn test_a_port_may_carry_a_default_value() {
        let ports = assert_parses(parse_port_declaration, "output reg [31:0] x = 1, y = 2;");
        assert_eq!(ports.len(), 2);
        assert!(ports[0].init.is_some());
        assert!(ports[1].init.is_some());
        assert_ne!(ports[0].init, ports[1].init);

        let module = assert_parses(
            parse_module_declaration,
            "module m(output reg [31:0] x = 1, y = 2); endmodule",
        );
        assert_eq!(module.ports.len(), 2);
        assert_eq!(module.ports[1].range, Range::Constant(31, 0));
        assert!(module.ports[0].init.is_some());
        assert!(module.ports[1].init.is_some());
    }

    #[test]
    fn test_parse_port() {
        assert_parses_to(
            parse_port,
            "input wire a",
            Port {
                direction: PortDirection::Input,
                net_type: Some(NetType::Wire),
                range: Range::Constant(0, 0),
                identifier: "a".into(),
                signed: false,
                init: None,
            },
        );
        assert_parses_to(
            parse_port,
            "output reg b",
            Port {
                direction: PortDirection::Output,
                net_type: Some(NetType::Reg),
                range: Range::Constant(0, 0),
                identifier: "b".into(),
                signed: false,
                init: None,
            },
        );
        assert_parses_to(
            parse_port,
            "inout c",
            Port {
                direction: PortDirection::InOut,
                net_type: None,
                range: Range::Constant(0, 0),
                identifier: "c".into(),
                signed: false,
                init: None,
            },
        );
    }

    #[test]
    fn test_parse_ports() {
        assert_parses_to(
            parse_ports,
            "( input wire a, output reg b, inout c )",
            vec![
                Port {
                    direction: PortDirection::Input,
                    net_type: Some(NetType::Wire),
                    range: Range::Constant(0, 0),
                    identifier: "a".into(),
                    signed: false,
                    init: None,
                },
                Port {
                    direction: PortDirection::Output,
                    net_type: Some(NetType::Reg),
                    range: Range::Constant(0, 0),
                    identifier: "b".into(),
                    signed: false,
                    init: None,
                },
                Port {
                    direction: PortDirection::InOut,
                    net_type: None,
                    range: Range::Constant(0, 0),
                    identifier: "c".into(),
                    signed: false,
                    init: None,
                },
            ],
        );
    }

    #[test]
    fn test_parse_module_without_a_port_list() {
        // `module top;` — how most testbenches are written, since a top-level
        // module has nothing to connect to.
        let module = assert_parses(
            parse_module_declaration,
            "module top;\n  assign a = b;\nendmodule",
        );
        assert_eq!(module.identifier, "top".into());
        assert!(module.ports.is_empty());
        assert_eq!(module.statements.len(), 1);
    }

    #[test]
    fn test_empty_port_list_and_absent_port_list_agree() {
        let with_parens = assert_parses(parse_module_declaration, "module m(); endmodule");
        let without = assert_parses(parse_module_declaration, "module m; endmodule");
        assert_eq!(with_parens, without);
    }

    #[test]
    fn test_an_opened_port_list_must_still_be_well_formed() {
        // An opening paren commits to a port list; `opt` must not let a
        // malformed one be silently abandoned.
        assert!(parse_module_declaration("module m(input a; endmodule").is_err());
    }

    #[test]
    fn test_parse_minimal_module_declaration() {
        let input = r#"
            module my_module (
                input wire a,
                output wire b
            );
            endmodule
        "#;
        let result = parse_module_declaration(input);
        assert!(result.is_ok());
        let (remaining, module) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(module.identifier, "my_module".into());
        assert_eq!(module.ports.len(), 2);
        assert_eq!(module.ports[0].identifier, "a".into());
        assert_eq!(module.ports[1].identifier, "b".into());

        assert_eq!(module.statements.len(), 0);
    }

    #[test]
    fn test_simple_adder() {
        let input = r#"
            module adder(
                input [7:0] a,
                input [7:0] b,
                output [7:0] c
            );
                assign c = a + b;
            endmodule
        "#;

        let (remaining, module) = parse_module_declaration(input).unwrap();
        assert!(remaining.trim().is_empty());
        assert_eq!(module.statements.len(), 1);
    }

    #[test]
    fn test_parse_verilog_examples() {
        let example_files_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join("verilog")
            .join("examples");
        let example_files = fs::read_dir(example_files_dir)
            .expect("Unable to read directory")
            .filter_map(|entry| {
                let entry = entry.expect("Unable to read entry");
                let path = entry.path();
                if path.is_file() {
                    Some(path.to_string_lossy().to_string())
                } else {
                    None
                }
            })
            .map(|path| {
                let content = fs::read_to_string(path).expect("Unable to read file");

                assert_parses(parse_module_declaration, &content)
            })
            .collect::<Vec<_>>();

        println!("{:?}", example_files);
    }

    /// Issue #87: a comment is legal wherever whitespace is, including inside a
    /// port list — between ports, around the parens, and mid-declaration.
    #[test]
    fn test_comments_in_a_port_list() {
        let module = assert_parses(
            parse_module_declaration,
            r#"
            module commented /* named */ ( // the port list opens here
                input wire a,   // the first port
                /* the second port, on its own line */
                input /* the direction is done */ reg [7:0] b,
                /*
                 * a block comment that spans
                 * several lines, mid-declaration
                 */
                output c // the last port
            ) /* after the ports */ ;
            endmodule
            "#,
        );
        assert_eq!(module.identifier, "commented".into());
        assert_eq!(module.ports.len(), 3);
        assert_eq!(module.ports[1].identifier, "b".into());
        assert_eq!(module.ports[1].range, Range::Constant(7, 0));
        assert_eq!(module.ports[2].identifier, "c".into());
    }

    /// A port width may be written in terms of a parameter, in either header
    /// style. The bound is carried as an expression and resolved at
    /// elaboration, where the parameter has a value.
    #[test]
    fn test_a_port_width_may_be_an_expression() {
        let ansi = assert_parses(
            parse_module_declaration,
            "module m(input [WIDTH-1:0] a, output [0:COUNT-1] b); endmodule",
        );
        assert!(matches!(ansi.ports[0].range, Range::Expressions(_, _)));
        assert!(matches!(ansi.ports[1].range, Range::Expressions(_, _)));

        let non_ansi = assert_parses(
            parse_module_declaration,
            "module m(a, b); input [WIDTH-1:0] a; output [7:0] b; endmodule",
        );
        assert!(matches!(non_ansi.ports[0].range, Range::Expressions(_, _)));
        assert!(matches!(non_ansi.ports[1].range, Range::Constant(7, 0)));
    }

    /// A comment between statements, between a statement and its semicolon, and
    /// between the last statement and `endmodule`. A `/* … */` around a whole
    /// statement comments it out.
    #[test]
    fn test_comments_in_a_module_body() {
        let module = assert_parses(
            parse_module_declaration,
            r#"
            module body_comments(input a, input b, output c);
                // a leading line comment
                wire d; /* trailing a declaration */
                /* assign d = 0; */
                assign d = a & b /* before the semicolon */;
                assign c = /* inside the expression */ d;
                // the last word before endmodule
            endmodule
            "#,
        );
        assert_eq!(module.statements.len(), 3);
    }

    /// Comments inside procedural code: a sensitivity list, a `begin … end`
    /// block, and the arms of a `case`.
    #[test]
    fn test_comments_in_procedural_code() {
        let module = assert_parses(
            parse_module_declaration,
            r#"
            module procedural_comments(input clk, input rst, output reg [1:0] q);
                always @(posedge clk /* the clock */ or posedge rst) begin // opens
                    // right after begin
                    if (rst) /* held in reset */ q <= 0;
                    else q <= q + 1;
                    // right before end
                end

                always @(*) begin
                    case (q) // the subject
                        // before the first arm
                        0 /* the label */ : q = 1;
                        1: q = 2; // after an arm
                        /* a whole arm, commented out
                        2: q = 3;
                        */
                        default /* the catch-all */ : q = 0;
                        // after the last arm
                    endcase
                end
            endmodule
            "#,
        );
        assert_eq!(module.statements.len(), 2);

        // The commented-out arm is not one of them.
        let case = match &module.statements[1] {
            ModuleStatement::AlwaysBlock(block) => match &block.statements[0] {
                crate::parsers::behavior::ProceduralStatements::Case(case) => case,
                other => panic!("expected a case statement, got {:?}", other),
            },
            other => panic!("expected an always block, got {:?}", other),
        };
        assert_eq!(case.items.len(), 3);
    }

    /// A comment in a module instantiation, between the module name, the
    /// parameter block, the instance name and the argument list.
    #[test]
    fn test_comments_in_a_module_instantiation() {
        let instantiation = one_instance(
            "counter /* the module */ dut /* the instance */ ( .clk(clk), .q(q) ) ; // done",
        );
        assert_eq!(instantiation.module_name, "counter".into());
        assert_eq!(instantiation.instance_name, Some("dut".into()));
    }

    /// `bar #345 bar1();` is the unparenthesised `#`, which is a *delay value*
    /// — one number or name — and reads as a one-element positional override.
    /// `foo #1 bar1(1'b0);` against a module whose first parameter is `n` is
    /// exactly how iverilog reads it (corpus `pr3194155`).
    #[test]
    fn test_an_unparenthesised_hash_is_one_positional_parameter() {
        let instantiation = one_instance("bar #345 bar1();");
        assert_eq!(instantiation.module_name, "bar".into());
        assert_eq!(instantiation.instance_name, Some("bar1".into()));
        assert_eq!(
            instantiation.parameters,
            ModuleInitArguments::Positional(vec![Some(
                verilog_expression("345")
                    .expect("a constant should parse")
                    .1
            )])
        );

        // A name is a delay value too, and `#` is its own token.
        let named = one_instance("bar # tPD u (o, i);");
        assert_eq!(
            named.parameters,
            ModuleInitArguments::Positional(vec![Some(Expression::Identifier("tPD".into()))])
        );

        // The parenthesised forms are unchanged.
        let block = one_instance("bar #(456) bar2();");
        assert_eq!(
            block.parameters,
            ModuleInitArguments::Positional(vec![Some(
                verilog_expression("456")
                    .expect("a constant should parse")
                    .1
            )])
        );
    }

    fn abc_123() -> ModuleInitArguments {
        let mut expected: HashMap<Identifier, Expression> = HashMap::new();

        expected.insert("a".into(), verilog_expression("1".into()).unwrap().1);
        expected.insert("b".into(), verilog_expression("2".into()).unwrap().1);
        expected.insert("c".into(), verilog_expression("3".into()).unwrap().1);
        ModuleInitArguments::Keyword(expected)
    }

    #[test]
    fn test_parse_kw_args() {
        let examples = vec![
            ".a(1),.b(2),.c(3)",
            ".a(1),.b(2),.c(3)",
            ".a( 1 ), .b(2),.c(3) ",
        ];

        for example in examples {
            assert_parses_to(parse_keyword_arguments, example, abc_123());
        }
    }

    #[test]
    fn test_parse_pos_args() {
        let arg_list_examples = vec![
            "1,2,3",
            "1, 2, 3",
            "a + b, foo, bar",
            "1/0, foo, bar, baz, qux",
        ];

        for example in arg_list_examples {
            let res = assert_parses(parse_positional_arguments, example);

            match res {
                ModuleInitArguments::Positional(args) => {
                    let comma_count = example.chars().filter(|&c| c == ',').count();
                    if comma_count == 0 {
                        assert_eq!(args.len(), 0);
                    } else {
                        assert_eq!(args.len(), comma_count + 1);
                    }
                }
                _ => {
                    panic!("Expected positional arguments, got {:?}", res);
                }
            }
        }
    }

    /// A blank element leaves that *position* unconnected, so it keeps its
    /// place in the list rather than being dropped.
    #[test]
    fn test_a_blank_positional_argument_keeps_its_place() {
        let w3 = || Expression::Identifier(Identifier::new("w3".to_string()));
        let w4 = || Expression::Identifier(Identifier::new("w4".to_string()));

        for (source, expected) in [
            ("two U7 (,);", vec![None, None]),
            ("two U8 (w3,);", vec![Some(w3()), None]),
            ("two U9 (,w4);", vec![None, Some(w4())]),
            ("three Ug (,,);", vec![None, None, None]),
            (
                "three Uh ( w3 , , w4 );",
                vec![Some(w3()), None, Some(w4())],
            ),
            ("two Ui (w3,w4);", vec![Some(w3()), Some(w4())]),
        ] {
            let instantiation = one_instance(source);
            assert_eq!(
                instantiation.arguments,
                ModuleInitArguments::Positional(expected),
                "{}",
                source
            );
        }

        // An empty block is still `NoArgs` — one blank is no argument list at
        // all, not a one-element list with a gap in it.
        let none = one_instance("two Uj ();");
        assert_eq!(none.arguments, ModuleInitArguments::NoArgs);
    }

    /// A blank connection has to reach elaboration as an *absent* one: the
    /// port is undriven, which is `z`, rather than bound to whatever the next
    /// argument was.
    #[test]
    fn test_a_blank_positional_connection_leaves_its_port_unconnected() {
        let child = r#"
            module two(
                input [3:0] first,
                input [3:0] second
            );
            endmodule
        "#;
        let top = r#"
            module top();
                wire [3:0] w4 = 4'b0101;
                two U9 (,w4);
                two U7 (,);
            endmodule
        "#;

        let mut modules = crate::parsers::source::parse_verilog_source(top)
            .expect("top should parse")
            .1;
        modules.extend(
            crate::parsers::source::parse_verilog_source(child)
                .expect("child should parse")
                .1,
        );

        let mut simulator = crate::simulator::runner::Simulator::with_modules(modules, "top");
        simulator.setup().expect("setup should succeed");
        simulator.run().expect("run should settle");

        // The blank leaves the *first* port floating and binds `w4` to the
        // second — not the other way round.
        assert_eq!(
            simulator
                .get("U9.first")
                .expect("U9.first exists")
                .to_binary(),
            "zzzz"
        );
        assert_eq!(
            simulator
                .get("U9.second")
                .expect("U9.second exists")
                .to_binary(),
            "0101"
        );
        assert_eq!(
            simulator
                .get("U7.first")
                .expect("U7.first exists")
                .to_binary(),
            "zzzz"
        );
        assert_eq!(
            simulator
                .get("U7.second")
                .expect("U7.second exists")
                .to_binary(),
            "zzzz"
        );
    }

    #[test]
    fn test_parse_named_args() {
        let arg_list_examples = vec![
            ".a(1),.b(2),.c(3)",
            ".a(1),.b(2),.c(3)",
            ".a( 1 ), .b(2),.c(3) ",
        ];

        for example in arg_list_examples {
            assert_parses_to(parse_keyword_arguments, example, abc_123());
        }
    }

    #[test]
    fn test_parse_arguments() {
        let examples = vec!["1,2,3", "1, 2, 3", "1, 2,  3 "];

        for example in examples {
            let res = assert_parses(parse_arguments, example);
            match res {
                ModuleInitArguments::Positional(mapping) => {
                    assert_eq!(mapping.len(), 3);
                }
                _ => {
                    panic!("Expected Positional arguments");
                }
            }
        }
    }
    #[test]
    fn test_parse_argument_block() {
        let examples = vec![
            ("()", ModuleInitArguments::NoArgs),
            (
                "(1,2,3)",
                ModuleInitArguments::Positional(vec![
                    Some(verilog_expression("1".into()).unwrap().1),
                    Some(verilog_expression("2".into()).unwrap().1),
                    Some(verilog_expression("3".into()).unwrap().1),
                ]),
            ),
            (
                "(1, 2, 3)",
                ModuleInitArguments::Positional(vec![
                    Some(verilog_expression("1".into()).unwrap().1),
                    Some(verilog_expression("2".into()).unwrap().1),
                    Some(verilog_expression("3".into()).unwrap().1),
                ]),
            ),
            ("(.a(1),.b(2),.c(3))", abc_123()),
        ];

        for (txt, expected) in examples {
            assert_parses_to(argument_block, txt, expected);
        }
    }

    #[test]
    fn test_parse_param_block() {
        assert_parses_to(param_block, "#(.a(1),.b(2),.c(3))", abc_123());
    }

    #[test]
    fn test_module_instantiation() {
        let test_statements = vec![
            "adder my_adder ();",
            "adder my_adder (1,2,3);",
            "adder my_adder (.a(1),.b(2),.c(3));",
            "adder #() my_adder (1,2,3);",
            "adder #(1) my_adder (.a(in_a),.b(in_b),.c(sum));",
            "adder #(.PARAM1(1),.PARAM2(2)) my_adder (.a(in_a),.b(in_b),.c(sum));",
        ];

        for input in test_statements {
            let res = one_instance(input);
            assert_eq!(res.module_name, "adder".into());
            assert_eq!(res.instance_name, Some("my_adder".into()));
        }
    }

    /// A Verilog-1995 header of plain names.
    fn names_header(names: &[&str]) -> PortHeader {
        PortHeader::NonAnsi(
            names
                .iter()
                .map(|name| HeaderPort::Named((*name).into()))
                .collect(),
        )
    }

    fn named_port(direction: PortDirection, name: &str) -> Port {
        Port {
            direction,
            net_type: None,
            range: Range::Constant(0, 0),
            identifier: name.into(),
            signed: false,
            init: None,
        }
    }

    /// Issue #104: the Verilog-1995 header lists names only, and the body
    /// declarations supply the direction and the width.
    #[test]
    fn test_parse_non_ansi_port_header() {
        let module = assert_parses(
            parse_module_declaration,
            r#"
            module addwide ( C, h );
                input C;
                output [11:0] h;
                reg [11:0] h;
                always @(posedge C) h <= h + 1;
            endmodule
            "#,
        );
        assert_eq!(module.identifier, "addwide".into());
        assert_eq!(
            module.ports,
            vec![
                named_port(PortDirection::Input, "C"),
                Port {
                    direction: PortDirection::Output,
                    net_type: None,
                    range: Range::Constant(11, 0),
                    identifier: "h".into(),
                    signed: false,
                    init: None,
                },
            ]
        );
        // The direction declarations became ports; the `reg` — which is what
        // makes `h` an output backed by a register — is still a body statement.
        assert_eq!(module.statements.len(), 2);
        assert!(matches!(
            module.statements[0],
            ModuleStatement::RegisterDeclaration(_)
        ));
    }

    /// The two spellings of the same header normalise to the same ports, so
    /// nothing downstream can tell them apart.
    #[test]
    fn test_the_two_header_styles_agree() {
        let ansi = assert_parses(
            parse_module_declaration,
            "module m(input wire [3:0] a, output reg b); endmodule",
        );
        let non_ansi = assert_parses(
            parse_module_declaration,
            "module m(a, b); input wire [3:0] a; output reg b; endmodule",
        );
        assert_eq!(ansi, non_ansi);
    }

    /// `signed` follows the net type and precedes the width, in both header
    /// styles.
    #[test]
    fn test_ports_carry_a_signed_qualifier() {
        assert_parses_to(
            parse_port,
            "input wire signed [3:0] a",
            Port {
                direction: PortDirection::Input,
                net_type: Some(NetType::Wire),
                range: Range::Constant(3, 0),
                identifier: "a".into(),
                signed: true,
                init: None,
            },
        );

        assert_parses_to(
            parse_port_declaration,
            "output signed [11:0] h, g;",
            vec![
                Port {
                    direction: PortDirection::Output,
                    net_type: None,
                    range: Range::Constant(11, 0),
                    identifier: "h".into(),
                    signed: true,
                    init: None,
                },
                Port {
                    direction: PortDirection::Output,
                    net_type: None,
                    range: Range::Constant(11, 0),
                    identifier: "g".into(),
                    signed: true,
                    init: None,
                },
            ],
        );

        // A port named after the keyword is still a port.
        assert_parses_to(
            parse_port,
            "input signedness",
            Port {
                direction: PortDirection::Input,
                net_type: None,
                range: Range::Constant(0, 0),
                identifier: "signedness".into(),
                signed: false,
                init: None,
            },
        );
    }

    #[test]
    fn test_parse_port_declaration_names_several_ports() {
        assert_parses_to(
            parse_port_declaration,
            "output reg [11:0] h, g;",
            vec![
                Port {
                    direction: PortDirection::Output,
                    net_type: Some(NetType::Reg),
                    range: Range::Constant(11, 0),
                    identifier: "h".into(),
                    signed: false,
                    init: None,
                },
                Port {
                    direction: PortDirection::Output,
                    net_type: Some(NetType::Reg),
                    range: Range::Constant(11, 0),
                    identifier: "g".into(),
                    signed: false,
                    init: None,
                },
            ],
        );
    }

    /// A direction keyword is only a keyword when a whole token, so a module
    /// whose instance name merely starts with one is not a port declaration.
    #[test]
    fn test_a_direction_keyword_must_be_a_whole_token() {
        let module = assert_parses(
            parse_module_declaration,
            "module m(a); input a; inputs inst (a); endmodule",
        );
        assert_eq!(module.ports.len(), 1);
        assert!(matches!(
            module.statements[0],
            ModuleStatement::ModuleInstantiation(_)
        ));
    }

    #[test]
    fn test_a_header_name_needs_a_direction() {
        assert_eq!(
            reconcile_ports(
                names_header(&["a", "b"]),
                vec![named_port(PortDirection::Input, "a")],
            ),
            Err(PortReconciliationError::MissingDirection("b".into()))
        );
        assert!(parse_module_declaration("module m(a, b); input a; endmodule").is_err());
    }

    /// A direction for a name the header does not list is not a port —
    /// nothing can connect to it — but the declaration it would be without
    /// the direction. iverilog 12.0 accepts all three spellings below
    /// silently, `-Wall` included, and reads `output reg a` in
    /// `module test;` as a variable (corpus `module_output_port_var2`).
    #[test]
    fn test_a_direction_outside_the_header_is_a_local() {
        assert_eq!(
            reconcile_ports(
                names_header(&["a"]),
                vec![
                    named_port(PortDirection::Input, "a"),
                    named_port(PortDirection::Output, "b"),
                ],
            ),
            Ok(ReconciledPorts {
                ports: vec![named_port(PortDirection::Input, "a")],
                locals: vec![named_port(PortDirection::Output, "b")],
                connections: Vec::new(),
            })
        );

        for source in [
            "module m(a); input a; output b; endmodule",
            "module m(input a); output b; endmodule",
            "module test; output reg a; output integer d; endmodule",
        ] {
            let module = assert_parses(parse_module_declaration, source);
            let locals: Vec<&str> = module
                .statements
                .iter()
                .filter_map(|statement| match statement {
                    ModuleStatement::PortDeclaration(locals) => Some(locals),
                    _ => None,
                })
                .flatten()
                .map(|local| local.identifier.name.as_str())
                .collect();
            assert!(!locals.is_empty(), "{}", source);
            assert!(
                module
                    .ports
                    .iter()
                    .all(|port| !locals.contains(&port.identifier.name.as_str())),
                "{}",
                source
            );
        }
    }

    /// The statements a module declaration carried a header's port
    /// expressions with, rendered back as source for a readable assertion.
    fn carried(module: &VerilogModule) -> Vec<String> {
        module
            .statements
            .iter()
            .filter_map(|statement| match statement {
                ModuleStatement::Assignment(assignments) => Some(format!(
                    "assign {} = {}",
                    assignments[0].lhs().to_contracted_string(),
                    assignments[0].rhs().to_contracted_string()
                )),
                ModuleStatement::GateInstantiation(gates) => Some(format!(
                    "tran {:?} ({})",
                    gates[0].instance.range,
                    gates[0]
                        .instance
                        .terminals
                        .iter()
                        .map(Expression::to_contracted_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )),
                _ => None,
            })
            .collect()
    }

    /// IEEE 1364-2005's `port` is a *port expression*, optionally behind an
    /// external name. Each is normalised to an ordinary port whose connection
    /// is carried to the body's declarations — `assign` inward for an input,
    /// outward for an output — and what it names in the body becomes a local
    /// (corpus `contrib8.2`, `pr377`, `pr3197917`, `port-test2`).
    #[test]
    fn test_a_header_port_may_be_an_expression() {
        let module = assert_parses(
            parse_module_declaration,
            "module c(.a({b, c}), q[3:0], ); input [10:0] b; input c; output [7:0] q; endmodule",
        );
        let ports: Vec<(&str, PortDirection, Range)> = module
            .ports
            .iter()
            .map(|port| {
                (
                    port.identifier.name.as_str(),
                    port.direction,
                    port.range.clone(),
                )
            })
            .collect();
        assert_eq!(
            ports,
            vec![
                ("a", PortDirection::Input, Range::Constant(11, 0)),
                ("$port1", PortDirection::Output, Range::Constant(3, 0)),
                ("$port2", PortDirection::Input, Range::SINGLE_BIT),
            ]
        );
        assert_eq!(
            carried(&module),
            vec!["assign {b, c} = a", "assign $port1 = q[3:0]"]
        );
        // What the expressions named is declared in the body, not as a port.
        assert!(matches!(
            &module.statements[0],
            ModuleStatement::PortDeclaration(locals) if locals.len() == 3
        ));
    }

    /// `module id(a, a); inout a;` — one net reached through two ports, which
    /// a `tran` carries because an `inout` is read as well as written (corpus
    /// `inout`, `br_gh1178b`). Two *inputs* sharing a net would have the
    /// child drive its parent, so a repeat of anything else is still refused.
    #[test]
    fn test_an_inout_may_be_listed_twice() {
        let module = assert_parses(
            parse_module_declaration,
            "module net_connect #(parameter W = 1) (w, w); inout wire [W-1:0] w; endmodule",
        );
        assert_eq!(module.ports.len(), 2);
        assert_eq!(module.ports[0].identifier, "w".into());
        assert_eq!(module.ports[1].identifier, "$port1".into());
        assert_eq!(module.ports[1].direction, PortDirection::InOut);
        assert_eq!(module.ports[1].range, module.ports[0].range);
        assert_eq!(carried(&module).len(), 1);
        assert!(carried(&module)[0].ends_with("($port1, w)"));

        assert!(parse_module_declaration("module m(a, a); input a; endmodule").is_err());
    }

    /// What a port expression cannot say is refused rather than guessed: parts
    /// that disagree about direction, a width made of parameters that would
    /// have to be summed, and an external name the body also declares.
    #[test]
    fn test_a_port_expression_that_cannot_be_carried_is_refused() {
        for source in [
            "module m({a, b}); input a; output b; endmodule",
            "module m #(parameter W = 2) ({a, b}); input [W-1:0] a; input b; endmodule",
            "module m(.b(a[1:0])); input [3:0] a; input b; endmodule",
            "module m(.p({a, b})); inout a, b; endmodule",
        ] {
            assert!(parse_module_declaration(source).is_err(), "{}", source);
        }
    }

    #[test]
    fn test_a_port_cannot_be_declared_twice() {
        assert_eq!(
            reconcile_ports(
                names_header(&["a"]),
                vec![
                    named_port(PortDirection::Input, "a"),
                    named_port(PortDirection::Output, "a"),
                ],
            ),
            Err(PortReconciliationError::Duplicate("a".into()))
        );
        assert!(parse_module_declaration("module m(a); input a; input a; endmodule").is_err());
        assert!(parse_module_declaration("module m; output b; output b; endmodule").is_err());

        // The same name twice in the header itself is the same mistake.
        assert_eq!(
            reconcile_ports(
                names_header(&["a", "a"]),
                vec![named_port(PortDirection::Input, "a")],
            ),
            Err(PortReconciliationError::Duplicate("a".into()))
        );

        // An ANSI port already has its direction, so the body cannot give it
        // a second one.
        assert_eq!(
            reconcile_ports(
                PortHeader::Ansi(vec![named_port(PortDirection::Input, "a")]),
                vec![named_port(PortDirection::Input, "a")],
            ),
            Err(PortReconciliationError::Duplicate("a".into()))
        );
        assert!(parse_module_declaration("module m(input a); input a; endmodule").is_err());
    }

    /// `p (Q, D);` — a user-defined primitive's instance name is optional, so
    /// the parser takes a missing one for any instantiation and leaves the
    /// module-or-primitive question to elaboration (corpus `pr298`,
    /// `pr3587570`).
    #[test]
    fn test_an_instance_name_may_be_missing() {
        let unnamed = one_instance("passthrough (o1, !i);");
        assert_eq!(unnamed.module_name, "passthrough".into());
        assert_eq!(unnamed.instance_name, None);
        assert!(unnamed.range.is_none());
        let delayed = one_instance("BUFG #(6, 2) (o, i);");
        assert_eq!(delayed.instance_name, None);
        assert_ne!(delayed.parameters, ModuleInitArguments::NoArgs);
    }

    /// Without an instance name a statement is only a name and an argument
    /// block, so a reserved word is never read as the module it instantiates.
    #[test]
    fn test_an_unnamed_instance_is_not_a_keyword() {
        assert!(parse_module_instantiation_statement("wait (a);").is_err());
        assert!(parse_module_instantiation_statement("initial (a);").is_err());
    }

    /// `u_dff ff0(q0, d, c), ff1(q1, d, q0);` — one statement, several
    /// instances sharing the module name and the parameter block (corpus
    /// `udp_sched`).
    #[test]
    fn test_an_instantiation_is_a_list() {
        let instances = assert_parses(
            parse_module_instantiation_statement,
            "u_dff #(1) ff0(q0, 1'b1, clk), ff1 (q1, 1'b1, q0) , (q2, 1'b1, q1);",
        );
        let names: Vec<_> = instances
            .iter()
            .map(|instance| instance.instance_name.clone())
            .collect();
        assert_eq!(names, vec![Some("ff0".into()), Some("ff1".into()), None]);
        assert!(instances
            .iter()
            .all(|instance| instance.module_name == "u_dff".into()
                && instance.parameters == instances[0].parameters));
    }

    /// An empty argument list may be written with whitespace or a comment
    /// inside it, for an instance's ports and for a parameter override alike.
    #[test]
    fn test_an_empty_argument_list_may_hold_whitespace() {
        for source in [
            "child_module my_module ( );",
            "child_module my_module (\n);",
            "child_module #( ) my_module (/* none */);",
            "child_module #(8, 8 'h10) my_module ( );",
        ] {
            let instances = assert_parses(parse_module_instantiation_statement, source);
            assert_eq!(instances.len(), 1, "{}", source);
            assert_eq!(
                instances[0].arguments,
                ModuleInitArguments::NoArgs,
                "{}",
                source
            );
        }
    }

    /// `inv u[3:0] (o, i);` — a range after the instance name makes an array
    /// of instances; without one it is an ordinary instantiation.
    #[test]
    fn test_instance_arrays_parse() {
        let arrayed = one_instance("inv u[3:0] (o, i);");
        assert!(arrayed.range.is_some());
        let spaced = one_instance("prim U [wid-1:0] (Q, D, C);");
        assert!(spaced.range.is_some());
        let single = one_instance("inv u (o, i);");
        assert!(single.range.is_none());
    }
}

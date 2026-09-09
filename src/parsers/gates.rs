//! Verilog's built-in gate and switch primitives.
//!
//! ```verilog
//! and    g1 (out, a, b);        // n inputs, the output written first
//! not    g2 (o1, o2, in);       // n outputs, the input written last
//! bufif1    (out, in, enable);  // three-state, and no instance name at all
//! xor    #5 (out, a, b);        // an optional delay
//! pullup (weak1) pu [3:0] (bus);// a strength, and an array of instances
//! ```
//!
//! A primitive is *keyword led*, which is what lets it be told from a module
//! instantiation — an identifier followed by an argument block — without the
//! two shadowing one another. The keyword is read as a whole identifier token
//! and then looked up, so `notify` is never the `not` primitive and `bufif1`
//! is never `buf` with a stray `if1`.
//!
//! The instance name is **optional**, unlike a module instantiation's, and one
//! statement may declare several instances: `nand n1 (a, b), n2 (c, d);`. Like
//! every other declaration list in this front end that returns a `Vec`, one per
//! instance, sharing the type, strength and delay written once at the front.

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, satisfy},
    combinator::{map, not, opt, peek},
    multi::separated_list1,
    sequence::{delimited, terminated},
    IResult,
};

use super::{
    delay::{parse_gate_delay, Delay},
    expr::{verilog_expression, Expression},
    identifier::{identifier, Identifier},
    simple::{range, ws, ws_and_comments, Range},
};

/// Which primitive was instantiated.
///
/// The three families behave differently enough that the simulator switches on
/// this rather than on a terminal count: an n-input gate has one output and
/// takes the rest as inputs, `buf`/`not` have one *input* and take the rest as
/// outputs, and a switch passes its input through — `z` and all — where a
/// three-state buffer converts a `z` input to `x`.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum GateKind {
    // n-input gates: one output, written first.
    And,
    Nand,
    Or,
    Nor,
    Xor,
    Xnor,
    // n-output gates: one input, written last.
    Buf,
    Not,
    // Three-state buffers: output, input, control.
    Bufif0,
    Bufif1,
    Notif0,
    Notif1,
    // MOS switches: output, input, control.
    Nmos,
    Pmos,
    Rnmos,
    Rpmos,
    // CMOS switches: output, input, n-control, p-control.
    Cmos,
    Rcmos,
    // Pull sources: n outputs, no inputs.
    Pullup,
    Pulldown,
    // Bidirectional pass switches.
    Tran,
    Rtran,
    Tranif0,
    Tranif1,
    Rtranif0,
    Rtranif1,
}

/// Every primitive keyword, paired with the kind it names.
const GATE_KEYWORDS: &[(&str, GateKind)] = &[
    ("and", GateKind::And),
    ("nand", GateKind::Nand),
    ("or", GateKind::Or),
    ("nor", GateKind::Nor),
    ("xor", GateKind::Xor),
    ("xnor", GateKind::Xnor),
    ("buf", GateKind::Buf),
    ("not", GateKind::Not),
    ("bufif0", GateKind::Bufif0),
    ("bufif1", GateKind::Bufif1),
    ("notif0", GateKind::Notif0),
    ("notif1", GateKind::Notif1),
    ("nmos", GateKind::Nmos),
    ("pmos", GateKind::Pmos),
    ("rnmos", GateKind::Rnmos),
    ("rpmos", GateKind::Rpmos),
    ("cmos", GateKind::Cmos),
    ("rcmos", GateKind::Rcmos),
    ("pullup", GateKind::Pullup),
    ("pulldown", GateKind::Pulldown),
    ("tran", GateKind::Tran),
    ("rtran", GateKind::Rtran),
    ("tranif0", GateKind::Tranif0),
    ("tranif1", GateKind::Tranif1),
    ("rtranif0", GateKind::Rtranif0),
    ("rtranif1", GateKind::Rtranif1),
];

impl GateKind {
    /// The primitive a keyword names, or `None` for an ordinary identifier.
    pub fn from_keyword(word: &str) -> Option<GateKind> {
        GATE_KEYWORDS
            .iter()
            .find(|(keyword, _)| *keyword == word)
            .map(|(_, kind)| *kind)
    }

    /// The keyword this primitive is written with, which is what an error
    /// about it names.
    pub fn keyword(&self) -> &'static str {
        GATE_KEYWORDS
            .iter()
            .find(|(_, kind)| kind == self)
            .map(|(keyword, _)| *keyword)
            .expect("every gate kind comes from the keyword table")
    }
}

/// How hard a driver pushes, which is what decides a net with more than one of
/// them. `supply` beats `strong` beats `pull` beats `weak`, and `highz` drives
/// nothing at all.
///
/// The charge strengths (`large`, `medium`, `small`) belong to a `trireg` and
/// are not part of a gate's drive strength, so they are deliberately absent.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum StrengthLevel {
    Highz,
    Weak,
    Pull,
    Strong,
    Supply,
}

/// A gate's `(strength0, strength1)` pair. The two halves are independent —
/// `(highz0, strong1)` is an open-drain driver, which drives a `1` and floats
/// instead of driving a `0`.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct DriveStrength {
    pub zero: StrengthLevel,
    pub one: StrengthLevel,
}

impl DriveStrength {
    /// What a gate drives with when nothing else is written: `strong0`,
    /// `strong1`.
    pub const STRONG: DriveStrength = DriveStrength {
        zero: StrengthLevel::Strong,
        one: StrengthLevel::Strong,
    };

    /// What a `pullup` or `pulldown` drives with by default.
    pub const PULL: DriveStrength = DriveStrength {
        zero: StrengthLevel::Pull,
        one: StrengthLevel::Pull,
    };

    /// The strength this driver pushes a given four-state bit with.
    ///
    /// An `x` is driven by both halves at once, so it takes the stronger of
    /// them; a `z` drives nothing whatever the declaration says.
    pub fn of(&self, code: u8) -> StrengthLevel {
        match code {
            crate::register::ZERO => self.zero,
            crate::register::ONE => self.one,
            crate::register::X => self.zero.max(self.one),
            _ => StrengthLevel::Highz,
        }
    }
}

/// One instance out of a primitive statement.
#[derive(Debug, PartialEq, Clone)]
pub struct GateInstance {
    /// `and g1 (…)` names the instance; `and (…)` does not, which a module
    /// instantiation may never do.
    pub name: Option<Identifier>,
    /// `and g1 [3:0] (…)` — an array of instances, one per index in the range.
    /// A bound may be an expression (`[N-1:0]`), so the count is only known
    /// once elaboration has the parameters in scope.
    pub range: Option<Range>,
    /// The terminals, in the order written. Which of them are outputs is a
    /// property of [`GateKind`], not of the list.
    pub terminals: Vec<Expression>,
}

/// A primitive instance together with the type, strength and delay of the
/// statement that declared it.
#[derive(Debug, PartialEq, Clone)]
pub struct GateInstantiation {
    pub kind: GateKind,
    /// `(strong0, pull1)`, absent when the statement did not say.
    pub strength: Option<DriveStrength>,
    /// `#5`, or the rise delay of `#(2, 3)`.
    ///
    /// The simulator settles a gate the way it settles every other continuous
    /// driver — in zero time — so this is parsed but not scheduled. It is kept
    /// rather than discarded because rebuilding it would mean re-reading the
    /// source.
    pub delay: Option<Delay>,
    pub instance: GateInstance,
}

impl GateInstantiation {
    /// The strength this instance drives with: what it declared, or the
    /// default for its kind.
    pub fn drive_strength(&self) -> DriveStrength {
        self.strength.unwrap_or(match self.kind {
            GateKind::Pullup | GateKind::Pulldown => DriveStrength::PULL,
            _ => DriveStrength::STRONG,
        })
    }
}

/// A character that may continue an identifier, so a keyword followed by one
/// is not a keyword at all: `not` in `notify`.
fn identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '$'
}

/// The leading keyword. Read as a whole identifier and then looked up, so the
/// table needs no ordering: `bufif1` can never be read as `buf`.
fn gate_type(input: &str) -> IResult<&str, GateKind> {
    let (rest, word) = identifier(input)?;
    match GateKind::from_keyword(&word.name) {
        Some(kind) => Ok((rest, kind)),
        None => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

/// One `strong0`-style keyword.
fn strength_keyword(input: &str) -> IResult<&str, (StrengthLevel, u8)> {
    terminated(
        alt((
            map(tag("supply0"), |_| (StrengthLevel::Supply, 0)),
            map(tag("supply1"), |_| (StrengthLevel::Supply, 1)),
            map(tag("strong0"), |_| (StrengthLevel::Strong, 0)),
            map(tag("strong1"), |_| (StrengthLevel::Strong, 1)),
            map(tag("pull0"), |_| (StrengthLevel::Pull, 0)),
            map(tag("pull1"), |_| (StrengthLevel::Pull, 1)),
            map(tag("weak0"), |_| (StrengthLevel::Weak, 0)),
            map(tag("weak1"), |_| (StrengthLevel::Weak, 1)),
            map(tag("highz0"), |_| (StrengthLevel::Highz, 0)),
            map(tag("highz1"), |_| (StrengthLevel::Highz, 1)),
        )),
        peek(not(satisfy(identifier_char))),
    )(input)
}

/// `(strong0, pull1)`, or the single strength a `pullup` may carry.
///
/// The two halves are written in either order, so each keyword places itself
/// by the digit it ends with rather than by its position. A half that was not
/// written keeps the default for the kind, which is why this is tried against
/// the terminal list at all: `(out, a, b)` matches no strength keyword and
/// falls through to being the terminals.
///
/// A gate is not the only driver that may declare one — `assign (pull1, pull0)
/// x = y;` is the same token in front of a continuous assignment — so this is
/// public, and it is the single definition of the production.
pub fn drive_strength(input: &str) -> IResult<&str, DriveStrength> {
    let (input, written) = delimited(
        ws(char('(')),
        separated_list1(ws(char(',')), strength_keyword),
        ws(char(')')),
    )(input)?;
    if written.len() > 2 {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::TooLarge,
        )));
    }
    let mut strength = DriveStrength::STRONG;
    for (level, half) in written {
        if half == 0 {
            strength.zero = level;
        } else {
            strength.one = level;
        }
    }
    Ok((input, strength))
}

/// `g1 (a, b)`, `g1 [3:0] (a, b)` or just `(a, b)`.
fn gate_instance(input: &str) -> IResult<&str, GateInstance> {
    let (input, _) = ws_and_comments(input)?;
    let (input, name) = opt(identifier)(input)?;
    let (input, range) = match name {
        // A range belongs to a *named* instance: `(a, b)[3:0]` is not a thing.
        Some(_) => opt(ws(range))(input)?,
        None => (input, None),
    };
    let (input, terminals) = delimited(
        ws(char('(')),
        separated_list1(char(','), ws(verilog_expression)),
        ws(char(')')),
    )(input)?;
    Ok((
        input,
        GateInstance {
            name,
            range,
            terminals,
        },
    ))
}

/// A whole primitive statement, which declares one instance or several.
pub fn parse_gate_instantiation(input: &str) -> IResult<&str, Vec<GateInstantiation>> {
    let (input, _) = ws_and_comments(input)?;
    let (input, kind) = gate_type(input)?;
    let (input, _) = ws_and_comments(input)?;
    // A strength is tried before the terminal list because both start with a
    // `(`. It only ever matches strength keywords, which are reserved words, so
    // a terminal list can never be mistaken for one.
    let (input, strength) = opt(drive_strength)(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, delay) = opt(parse_gate_delay)(input)?;
    let (input, instances) = separated_list1(ws(char(',')), gate_instance)(input)?;
    let (input, _) = ws(char(';'))(input)?;

    Ok((
        input,
        instances
            .into_iter()
            .map(|instance| GateInstantiation {
                kind,
                strength,
                delay: delay.clone(),
                instance,
            })
            .collect(),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::helpers::assert_parses;

    fn one(source: &str) -> GateInstantiation {
        let mut gates = assert_parses(parse_gate_instantiation, source);
        assert_eq!(gates.len(), 1, "expected one instance: {}", source);
        gates.remove(0)
    }

    #[test]
    fn test_every_keyword_round_trips() {
        for (keyword, kind) in GATE_KEYWORDS {
            assert_eq!(GateKind::from_keyword(keyword), Some(*kind));
            assert_eq!(kind.keyword(), *keyword);
        }
        assert_eq!(GateKind::from_keyword("notify"), None);
        assert_eq!(GateKind::from_keyword("android"), None);
    }

    /// The keyword is a whole token, so a name that merely starts with one is
    /// an ordinary identifier and the statement is not a primitive at all.
    #[test]
    fn test_a_longer_name_is_not_a_keyword() {
        assert!(parse_gate_instantiation("notify u1 (a, b);").is_err());
        assert!(parse_gate_instantiation("andgate u1 (a, b);").is_err());
        // `bufif1` is its own keyword rather than `buf` followed by `if1`.
        assert_eq!(one("bufif1 (o, i, e);").kind, GateKind::Bufif1);
        assert_eq!(one("buf (o, i);").kind, GateKind::Buf);
    }

    #[test]
    fn test_an_instance_name_is_optional() {
        let named = one("and g1 (out, a, b);");
        assert_eq!(named.instance.name, Some("g1".into()));
        assert_eq!(named.instance.terminals.len(), 3);

        let anonymous = one("pullup (net);");
        assert_eq!(anonymous.instance.name, None);
        assert_eq!(anonymous.instance.terminals.len(), 1);
    }

    #[test]
    fn test_arbitrary_arity() {
        assert_eq!(one("or g (out, a, b, c, d);").instance.terminals.len(), 5);
        assert_eq!(one("not (o1, o2, in);").instance.terminals.len(), 3);
    }

    /// One statement may declare several instances, and every one of them
    /// keeps the type, strength and delay written at the front.
    #[test]
    fn test_a_statement_declares_a_list() {
        let gates = assert_parses(
            parse_gate_instantiation,
            "nand #3 n1 (w1, a, b), n2 (w2, c, d);",
        );
        assert_eq!(gates.len(), 2);
        assert!(gates.iter().all(|gate| gate.kind == GateKind::Nand));
        assert!(gates.iter().all(|gate| gate.delay == Some(Delay::new(3))));
        assert_eq!(gates[0].instance.name, Some("n1".into()));
        assert_eq!(gates[1].instance.name, Some("n2".into()));
    }

    #[test]
    fn test_delays() {
        assert_eq!(one("xor #5 g (o, a, b);").delay, Some(Delay::new(5)));
        assert_eq!(one("xor #(5) (o, a, b);").delay, Some(Delay::new(5)));
        // A rise/fall pair keeps the rise delay, the way `#(min:typ:max)`
        // keeps the typical one.
        assert_eq!(one("buf #(2, 7) (o, i);").delay, Some(Delay::new(2)));
        assert_eq!(one("buf #(2, 7, 9) (o, i);").delay, Some(Delay::new(2)));
        assert_eq!(
            one("buf #(1:2:3, 7) (o, i);").delay,
            Some(Delay::triple(1, 2, 3))
        );
        assert_eq!(one("and (o, a, b);").delay, None);
    }

    /// A strength is written before the delay and looks like a terminal list.
    /// Nothing but a strength keyword matches, so the two never collide.
    #[test]
    fn test_drive_strengths() {
        assert_eq!(
            one("buf (strong0, pull1) b0 (o, i);").strength,
            Some(DriveStrength {
                zero: StrengthLevel::Strong,
                one: StrengthLevel::Pull,
            })
        );
        // Either order.
        assert_eq!(
            one("not (highz1, strong0) (o, i);").strength,
            Some(DriveStrength {
                zero: StrengthLevel::Strong,
                one: StrengthLevel::Highz,
            })
        );
        // A `pullup` may name only the half it drives.
        assert_eq!(
            one("pulldown (weak0) pd (net);").strength,
            Some(DriveStrength {
                zero: StrengthLevel::Weak,
                one: StrengthLevel::Strong,
            })
        );
        assert_eq!(one("and (o, a, b);").strength, None);
        // With a strength *and* a delay, which is the order the LRM writes.
        let both = one("buf (weak0, weak1) #(1, 1) (o, a);");
        assert_eq!(both.delay, Some(Delay::new(1)));
        assert_eq!(both.strength.map(|s| s.zero), Some(StrengthLevel::Weak));
        assert_eq!(both.instance.terminals.len(), 2);
    }

    /// A driver's strength depends on the bit it is driving, which is what
    /// makes `(highz0, strong1)` an open drain.
    #[test]
    fn test_strength_of_a_bit() {
        let open_drain = DriveStrength {
            zero: StrengthLevel::Highz,
            one: StrengthLevel::Strong,
        };
        assert_eq!(open_drain.of(crate::register::ZERO), StrengthLevel::Highz);
        assert_eq!(open_drain.of(crate::register::ONE), StrengthLevel::Strong);
        // An `x` is driven by both halves, so it takes the stronger.
        assert_eq!(open_drain.of(crate::register::X), StrengthLevel::Strong);
        // A `z` drives nothing whatever the declaration says.
        assert_eq!(open_drain.of(crate::register::Z), StrengthLevel::Highz);
    }

    #[test]
    fn test_an_array_of_instances() {
        let arrayed = one("bufif1 buffer[7:0] (value2, value1, 1'b1);");
        assert_eq!(arrayed.instance.name, Some("buffer".into()));
        assert_eq!(arrayed.instance.range, Some(Range::Constant(7, 0)));
        // Whitespace between the name and the range is a token boundary like
        // any other.
        assert_eq!(
            one("pullup u8 [7:0] (bus);").instance.range,
            Some(Range::Constant(7, 0))
        );
        assert_eq!(one("pullup u8 (bus);").instance.range, None);
        // A bound may be an expression, resolved once elaboration knows the
        // parameters — an array of instances is sized like any other range.
        assert!(matches!(
            one("buf drv [N-1:0] (out, in);").instance.range,
            Some(Range::Expressions(_, _))
        ));
    }

    /// A terminal is an expression, not just a name.
    #[test]
    fn test_expression_terminals() {
        let gate = one("and (out, a & b, 1'b1);");
        assert_eq!(gate.instance.terminals.len(), 3);
        let selected = one("bufif0 drv0 (bus[0], D, E);");
        assert_eq!(selected.instance.terminals.len(), 3);
    }

    /// Comments are legal at every token boundary, as everywhere else.
    #[test]
    fn test_comments_between_tokens() {
        let gate = one("and /* g */ g1 /* n */ ( out , a , b ) ; // done");
        assert_eq!(gate.instance.name, Some("g1".into()));
        assert_eq!(gate.instance.terminals.len(), 3);
    }
}

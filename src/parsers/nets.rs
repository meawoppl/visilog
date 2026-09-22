use nom::{
    bytes::complete::tag, character::complete::char, combinator::opt, multi::separated_list1,
    sequence::preceded, IResult,
};

use super::{
    delay::{parse_gate_delay, GateDelay},
    expr::{verilog_expression, Expression},
    gates::{drive_strength, DriveStrength},
    identifier::{identifier, Identifier},
    simple::{declared_range, dimensions, signedness, ws, Range},
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NetType {
    Supply0,
    Supply1,
    Tri,
    Tri0,
    Tri1,
    TriAnd,
    TriOr,
    /// `uwire` — a wire the LRM allows exactly one driver. Nothing here counts
    /// drivers, so it resolves exactly as a `wire` does.
    Uwire,
    Wire,
    WireAnd,
    WireOr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Net {
    identifier: Identifier,
    range: Range,
    net_type: NetType,
    /// The `#(rise, fall, turn_off)` of `wire #5 a = b;`, the same `delay3` an
    /// `assign` writes and scheduled the same way: a declaration assignment
    /// is a continuous assignment, and this is its delay.
    delay: Option<GateDelay>,
    /// The `(weak0, weak1)` of `wire (weak0, weak1) a = b;` — the strength
    /// the declaration assignment drives at, exactly as an `assign`'s does.
    strength: Option<DriveStrength>,
    /// Whether the declaration carried a `signed` qualifier.
    signed: bool,
    /// The driver a `wire a = expr;` declaration carries.
    ///
    /// A net initialiser is shorthand for a *continuous assignment*, not a
    /// one-off starting value: the net follows `expr` for the whole
    /// simulation. It belongs to the name rather than to the declaration, so
    /// `wire x = 1, y = 2;` gives `x` and `y` different drivers.
    init: Option<Expression>,
    /// The address dimensions of `wire [1:0] bus[3:0];` — an array of nets,
    /// outermost first, and empty for an ordinary net.
    ///
    /// They belong to the *name* rather than to the declaration, exactly as a
    /// `reg` memory's do, which is what makes `wire a, bus[3:0];` legal and
    /// what keeps this from being a second net production to order against the
    /// first.
    dimensions: Vec<Range>,
}

impl Net {
    pub fn new(identifier: Identifier, range: Range, net_type: NetType) -> Self {
        Net {
            identifier,
            range,
            net_type,
            delay: None,
            strength: None,
            signed: false,
            init: None,
            dimensions: Vec::new(),
        }
    }

    /// The same net, continuously driven by `init`.
    pub fn with_init(mut self, init: Expression) -> Self {
        self.init = Some(init);
        self
    }

    /// The same net, declared with a delay.
    pub fn with_delay(mut self, delay: GateDelay) -> Self {
        self.delay = Some(delay);
        self
    }

    /// The same net, declared with a drive strength.
    pub fn with_strength(mut self, strength: DriveStrength) -> Self {
        self.strength = Some(strength);
        self
    }

    /// The delay the declaration named, if it named one.
    pub fn delay(&self) -> Option<&GateDelay> {
        self.delay.as_ref()
    }

    /// The drive strength the declaration named, if it named one.
    pub fn strength(&self) -> Option<DriveStrength> {
        self.strength
    }

    /// The same net, declared `signed`.
    pub fn with_signedness(mut self, signed: bool) -> Self {
        self.signed = signed;
        self
    }

    /// Whether the declaration read `wire signed [3:0] a;`.
    pub fn is_signed(&self) -> bool {
        self.signed
    }

    pub fn identifier(&self) -> &Identifier {
        &self.identifier
    }

    pub fn range(&self) -> &Range {
        &self.range
    }

    pub fn init(&self) -> Option<&Expression> {
        self.init.as_ref()
    }

    /// The address dimensions, when the declaration named an array of nets.
    pub fn dimensions(&self) -> &[Range] {
        &self.dimensions
    }

    /// Which flavour of net this declaration named. `supply0`/`supply1` and
    /// `tri0`/`tri1` drive themselves, so elaboration has to ask.
    pub fn kind(&self) -> NetType {
        self.net_type
    }
}

pub fn net_type(input: &str) -> nom::IResult<&str, NetType> {
    use nom::{branch::alt, bytes::complete::tag, combinator::value};

    alt((
        value(NetType::Uwire, tag("uwire")),
        value(NetType::Wire, tag("wire")),
        value(NetType::WireAnd, tag("wand")),
        value(NetType::WireOr, tag("wor")),
        value(NetType::TriAnd, tag("triand")),
        value(NetType::TriOr, tag("trior")),
        value(NetType::Tri0, tag("tri0")),
        value(NetType::Tri1, tag("tri1")),
        value(NetType::Tri, tag("tri")),
        value(NetType::Supply0, tag("supply0")),
        value(NetType::Supply1, tag("supply1")),
    ))(input)
}

/// One declared net name plus the optional expression that drives it.
fn declared_net(input: &str) -> IResult<&str, (Identifier, Vec<Range>, Option<Expression>)> {
    let (input, name) = identifier(input)?;
    let (input, dims) = dimensions(input)?;
    let (input, init) = opt(preceded(ws(char('=')), verilog_expression))(input)?;
    Ok((input, (name, dims, init)))
}

pub fn net_declaration(input: &str) -> IResult<&str, Vec<Net>> {
    let (input, net_type) = net_type(input)?;
    let (input, strength) = opt(drive_strength)(input)?;
    let (input, signed) = ws(signedness)(input)?;
    let (input, range) = ws(opt(declared_range))(input)?;
    let (input, delay) = opt(parse_gate_delay)(input)?;
    let (input, names) = separated_list1(ws(char(',')), ws(declared_net))(input)?;
    let (input, _) = ws(tag(";"))(input)?;

    let nets: Vec<Net> = names
        .into_iter()
        .map(|(identifier, dimensions, init)| Net {
            identifier,
            net_type: net_type.clone(),
            range: range.clone().unwrap_or(Range::SINGLE_BIT),
            delay: delay.clone(),
            strength,
            signed,
            init,
            dimensions,
        })
        .collect();

    Ok((input, nets))
}

#[cfg(test)]
mod tests {
    use crate::parsers::{delay::Delay, gates::StrengthLevel, helpers::assert_parses_to};

    use super::*;

    #[test]
    fn test_net_type() {
        let tests = vec![
            ("wire", NetType::Wire),
            ("wand", NetType::WireAnd),
            ("wor", NetType::WireOr),
            ("tri", NetType::Tri),
            ("triand", NetType::TriAnd),
            ("trior", NetType::TriOr),
            ("supply0", NetType::Supply0),
            ("supply1", NetType::Supply1),
            ("tri0", NetType::Tri0),
            ("tri1", NetType::Tri1),
            ("uwire", NetType::Uwire),
        ];
        for (input, expected) in tests {
            assert_parses_to(net_type, input, expected);
        }

        assert!(net_type("invalid").is_err());
    }

    #[test]
    fn test_uwire_declaration() {
        assert_parses_to(
            net_declaration,
            "uwire [31:0] foo;",
            vec![Net::new(
                "foo".into(),
                Range::Constant(31, 0),
                NetType::Uwire,
            )],
        );
    }

    #[test]
    fn test_net_declaration() {
        assert_parses_to(
            net_declaration,
            "wire [7:0] a, b, c;",
            vec![
                Net::new("a".into(), Range::Constant(7, 0), NetType::Wire),
                Net::new("b".into(), Range::Constant(7, 0), NetType::Wire),
                Net::new("c".into(), Range::Constant(7, 0), NetType::Wire),
            ],
        );

        assert_parses_to(
            net_declaration,
            "tri0 a, b, c ;",
            vec![
                Net::new("a".into(), Range::Constant(0, 0), NetType::Tri0),
                Net::new("b".into(), Range::Constant(0, 0), NetType::Tri0),
                Net::new("c".into(), Range::Constant(0, 0), NetType::Tri0),
            ],
        );

        assert_parses_to(
            net_declaration,
            "tri1 [3:0] x, y, z;",
            vec![
                Net::new("x".into(), Range::Constant(3, 0), NetType::Tri1),
                Net::new("y".into(), Range::Constant(3, 0), NetType::Tri1),
                Net::new("z".into(), Range::Constant(3, 0), NetType::Tri1),
            ],
        );
    }

    /// `signed` sits between the net type and the width, and belongs to the
    /// declaration rather than to a name, so every net in the list gets it.
    #[test]
    fn test_net_declaration_signedness() {
        assert_parses_to(
            net_declaration,
            "wire signed [7:0] a, b;",
            vec![
                Net::new("a".into(), Range::Constant(7, 0), NetType::Wire).with_signedness(true),
                Net::new("b".into(), Range::Constant(7, 0), NetType::Wire).with_signedness(true),
            ],
        );

        // `unsigned` is the default said out loud.
        assert_parses_to(
            net_declaration,
            "wire unsigned [7:0] a;",
            vec![Net::new("a".into(), Range::Constant(7, 0), NetType::Wire)],
        );

        // A name that merely starts with the keyword is a name.
        assert_parses_to(
            net_declaration,
            "wire signed_value;",
            vec![Net::new(
                "signed_value".into(),
                Range::Constant(0, 0),
                NetType::Wire,
            )],
        );
    }

    #[test]
    fn test_net_declaration_delay_array() {
        let result = net_declaration("wire [7:0] #10  z;");
        assert!(result.is_ok());
        let (_, nets) = result.unwrap();
        assert_eq!(nets.len(), 1);
        let net = &nets[0];
        let expected = Net::new(
            Identifier::new("z".to_string()),
            Range::Constant(7, 0),
            NetType::Wire,
        )
        .with_delay(GateDelay::single(Delay::new(10)));
        assert_eq!(net, &expected);
    }

    #[test]
    fn test_net_declaration_array() {
        let result = net_declaration("wire [7:0] z;");
        assert!(result.is_ok());
        let (_, nets) = result.unwrap();
        assert_eq!(nets.len(), 1);
        let net = &nets[0];

        let expected_net = Net::new(
            Identifier::new("z".to_string()),
            Range::Constant(7, 0),
            NetType::Wire,
        );
        assert_eq!(net, &expected_net);
    }

    #[test]
    fn test_net_decl_simple() {
        let result = net_declaration("wire z;");
        assert!(result.is_ok());
        let (_, nets) = result.unwrap();
        assert_eq!(nets.len(), 1);
        let net = &nets[0];

        let expected_net = Net::new(
            Identifier::new("z".to_string()),
            Range::Constant(0, 0),
            NetType::Wire,
        );
        assert_eq!(net, &expected_net);
    }

    #[test]
    fn test_net_declaration_multiple_identifiers() {
        let result = net_declaration("wire [7:0] #5  a, b, c;");
        assert!(result.is_ok());
        let (_, nets) = result.unwrap();
        assert_eq!(nets.len(), 3);

        for net in nets {
            assert_eq!(net.net_type, NetType::Wire);
            assert_eq!(net.delay, Some(GateDelay::single(Delay::new(5))));
            assert_eq!(net.range, Range::Constant(7, 0));
        }
    }

    /// A declaration assignment carries the same `delay3` an `assign` does,
    /// and its value is an expression: `#(period/3)`, `#idly`, `#1.1`.
    #[test]
    fn test_net_declaration_delay_is_an_expression() {
        for source in [
            "wire #(period/3) trace = drive;",
            "wire #idly int = in;",
            "wire #1.1 first = in;",
            "wire [5:0] #1 base = in;",
            "wire #(2, 3) a = b;",
        ] {
            let (rest, nets) = net_declaration(source).expect(source);
            assert!(rest.is_empty(), "unparsed input: {}", rest);
            assert!(nets[0].delay().is_some(), "{} lost its delay", source);
        }
        assert_eq!(
            net_declaration("wire #(2, 3) a = b;").unwrap().1[0].delay(),
            Some(&GateDelay::of(Delay::new(2), Delay::new(3), None))
        );
    }

    /// `wire (weak0, weak1) value = pullval;` — the strength sits between the
    /// net type and everything else, and belongs to the declaration.
    #[test]
    fn test_net_declaration_strength() {
        let weak = DriveStrength {
            zero: StrengthLevel::Weak,
            one: StrengthLevel::Weak,
        };
        assert_parses_to(
            net_declaration,
            "wire (weak0, weak1) value = pullval;",
            vec![
                Net::new("value".into(), Range::Constant(0, 0), NetType::Wire)
                    .with_strength(weak)
                    .with_init(expression("pullval")),
            ],
        );
        assert_parses_to(
            net_declaration,
            "wire (pull1, strong0) signed [3:0] #2 a = b;",
            vec![Net::new("a".into(), Range::Constant(3, 0), NetType::Wire)
                .with_strength(DriveStrength {
                    zero: StrengthLevel::Strong,
                    one: StrengthLevel::Pull,
                })
                .with_signedness(true)
                .with_delay(GateDelay::single(Delay::new(2)))
                .with_init(expression("b"))],
        );
    }

    /// The expression a source fragment parses to, so a test can spell an
    /// initialiser the way Verilog does rather than as an AST literal.
    fn expression(source: &str) -> Expression {
        let (rest, expression) =
            verilog_expression(source).expect("the expression should have parsed");
        assert!(rest.is_empty(), "unparsed input: {}", rest);
        expression
    }

    #[test]
    fn test_net_declaration_initialiser() {
        assert_parses_to(
            net_declaration,
            "wire a = 1'b1;",
            vec![Net::new("a".into(), Range::Constant(0, 0), NetType::Wire)
                .with_init(expression("1'b1"))],
        );

        assert_parses_to(
            net_declaration,
            "wire [3:0] q = a + b;",
            vec![Net::new("q".into(), Range::Constant(3, 0), NetType::Wire)
                .with_init(expression("a + b"))],
        );
    }

    /// An initialiser belongs to the *name*, so every net in a list gets its
    /// own driver — and a name without one is still an undriven net.
    #[test]
    fn test_net_initialisers_are_per_name() {
        assert_parses_to(
            net_declaration,
            "wire x = 1, y = 2;",
            vec![
                Net::new("x".into(), Range::Constant(0, 0), NetType::Wire)
                    .with_init(expression("1")),
                Net::new("y".into(), Range::Constant(0, 0), NetType::Wire)
                    .with_init(expression("2")),
            ],
        );

        assert_parses_to(
            net_declaration,
            "wire [7:0] a = 8'h0f, b;",
            vec![
                Net::new("a".into(), Range::Constant(7, 0), NetType::Wire)
                    .with_init(expression("8'h0f")),
                Net::new("b".into(), Range::Constant(7, 0), NetType::Wire),
            ],
        );
    }

    /// `wire [1:0] bus[3:0];` — an array of nets. The address dimension
    /// belongs to the *name*, exactly as a `reg` memory's does, which is what
    /// makes `wire a, bus[3:0];` legal in one declaration.
    #[test]
    fn test_net_arrays() {
        let nets = net_declaration("wire [1:0] arr[2:1];")
            .expect("should parse")
            .1;
        assert_eq!(nets.len(), 1);
        assert_eq!(nets[0].dimensions(), [Range::Constant(2, 1)]);

        let mixed = net_declaration("wire a, bus[3:0];")
            .expect("should parse")
            .1;
        assert_eq!(mixed.len(), 2);
        assert!(mixed[0].dimensions().is_empty());
        assert_eq!(mixed[1].dimensions(), [Range::Constant(3, 0)]);

        let signed = net_declaration("wire signed [2:0] n [0:3];")
            .expect("should parse")
            .1;
        assert_eq!(signed[0].dimensions(), [Range::Constant(0, 3)]);
        assert!(signed[0].is_signed());
    }

    /// `wire [1:0] grid [0:3][0:7];` — several address dimensions on one net
    /// name, the array-of-nets half of `reg [7:0] a [0:3][0:15];`.
    #[test]
    fn test_net_array_keeps_every_dimension() {
        let nets = net_declaration("wire [1:0] grid [0:3][0:7];")
            .expect("should parse")
            .1;
        assert_eq!(
            nets[0].dimensions(),
            [Range::Constant(0, 3), Range::Constant(0, 7)]
        );
    }
}

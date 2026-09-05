use nom::{
    bytes::complete::tag,
    character::complete::char,
    combinator::opt,
    multi::separated_list1,
    sequence::{pair, preceded},
    IResult,
};

use super::{
    expr::{verilog_expression, Expression},
    identifier::{identifier, Identifier},
    simple::{range, signedness, ws},
};

#[derive(Debug, PartialEq, Clone)]
pub enum NetType {
    Supply0,
    Supply1,
    Tri,
    Tri0,
    Tri1,
    TriAnd,
    TriOr,
    Wire,
    WireAnd,
    WireOr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Net {
    identifier: Identifier,
    range: (i64, i64),
    net_type: NetType,
    delay: u32,
    /// Whether the declaration carried a `signed` qualifier.
    signed: bool,
    /// The driver a `wire a = expr;` declaration carries.
    ///
    /// A net initialiser is shorthand for a *continuous assignment*, not a
    /// one-off starting value: the net follows `expr` for the whole
    /// simulation. It belongs to the name rather than to the declaration, so
    /// `wire x = 1, y = 2;` gives `x` and `y` different drivers.
    init: Option<Expression>,
}

impl Net {
    pub fn new(identifier: Identifier, range: (i64, i64), net_type: NetType, delay: u32) -> Self {
        Net {
            identifier,
            range,
            net_type,
            delay,
            signed: false,
            init: None,
        }
    }

    /// The same net, continuously driven by `init`.
    pub fn with_init(mut self, init: Expression) -> Self {
        self.init = Some(init);
        self
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

    pub fn range(&self) -> (i64, i64) {
        self.range
    }

    pub fn init(&self) -> Option<&Expression> {
        self.init.as_ref()
    }
}

pub fn net_type(input: &str) -> nom::IResult<&str, NetType> {
    use nom::{branch::alt, bytes::complete::tag, combinator::value};

    alt((
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

fn parse_delay(input: &str) -> IResult<&str, u32> {
    use nom::{
        bytes::complete::tag, character::complete::digit1, combinator::map_res, sequence::preceded,
    };

    map_res(preceded(tag("#"), digit1), |s: &str| s.parse::<u32>())(input)
}

/// One declared net name plus the optional expression that drives it.
fn declared_net(input: &str) -> IResult<&str, (Identifier, Option<Expression>)> {
    pair(identifier, opt(preceded(ws(char('=')), verilog_expression)))(input)
}

pub fn net_declaration(input: &str) -> IResult<&str, Vec<Net>> {
    let (input, net_type) = net_type(input)?;
    let (input, signed) = ws(signedness)(input)?;
    let (input, range) = ws(opt(range))(input)?;
    let (input, delay) = opt(parse_delay)(input)?;
    let (input, names) = separated_list1(ws(char(',')), ws(declared_net))(input)?;
    let (input, _) = ws(tag(";"))(input)?;

    let nets: Vec<Net> = names
        .into_iter()
        .map(|(identifier, init)| Net {
            identifier,
            net_type: net_type.clone(),
            range: range.unwrap_or((0, 0)),
            delay: delay.unwrap_or(0),
            signed,
            init,
        })
        .collect();

    Ok((input, nets))
}

#[cfg(test)]
mod tests {
    use crate::parsers::helpers::assert_parses_to;

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
        ];
        for (input, expected) in tests {
            assert_parses_to(net_type, input, expected);
        }

        assert!(net_type("invalid").is_err());
    }

    #[test]
    fn test_net_declaration() {
        assert_parses_to(
            net_declaration,
            "wire [7:0] a, b, c;",
            vec![
                Net::new("a".into(), (7, 0), NetType::Wire, 0),
                Net::new("b".into(), (7, 0), NetType::Wire, 0),
                Net::new("c".into(), (7, 0), NetType::Wire, 0),
            ],
        );

        assert_parses_to(
            net_declaration,
            "tri0 a, b, c ;",
            vec![
                Net::new("a".into(), (0, 0), NetType::Tri0, 0),
                Net::new("b".into(), (0, 0), NetType::Tri0, 0),
                Net::new("c".into(), (0, 0), NetType::Tri0, 0),
            ],
        );

        assert_parses_to(
            net_declaration,
            "tri1 [3:0] x, y, z;",
            vec![
                Net::new("x".into(), (3, 0), NetType::Tri1, 0),
                Net::new("y".into(), (3, 0), NetType::Tri1, 0),
                Net::new("z".into(), (3, 0), NetType::Tri1, 0),
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
                Net::new("a".into(), (7, 0), NetType::Wire, 0).with_signedness(true),
                Net::new("b".into(), (7, 0), NetType::Wire, 0).with_signedness(true),
            ],
        );

        // `unsigned` is the default said out loud.
        assert_parses_to(
            net_declaration,
            "wire unsigned [7:0] a;",
            vec![Net::new("a".into(), (7, 0), NetType::Wire, 0)],
        );

        // A name that merely starts with the keyword is a name.
        assert_parses_to(
            net_declaration,
            "wire signed_value;",
            vec![Net::new("signed_value".into(), (0, 0), NetType::Wire, 0)],
        );
    }

    fn test_parse_delay() {
        assert_eq!(parse_delay("#10"), Ok(("", 10)));
        assert_eq!(parse_delay("#0"), Ok(("", 0)));
        assert!(parse_delay("10").is_err());
    }

    #[test]
    fn test_net_declaration_delay_array() {
        let result = net_declaration("wire [7:0] #10  z;");
        assert!(result.is_ok());
        let (_, nets) = result.unwrap();
        assert_eq!(nets.len(), 1);
        let net = &nets[0];
        let expected = Net::new(Identifier::new("z".to_string()), (7, 0), NetType::Wire, 10);
        assert_eq!(net, &expected);
    }

    #[test]
    fn test_net_declaration_array() {
        let result = net_declaration("wire [7:0] z;");
        assert!(result.is_ok());
        let (_, nets) = result.unwrap();
        assert_eq!(nets.len(), 1);
        let net = &nets[0];

        let expected_net = Net::new(Identifier::new("z".to_string()), (7, 0), NetType::Wire, 0);
        assert_eq!(net, &expected_net);
    }

    #[test]
    fn test_net_decl_simple() {
        let result = net_declaration("wire z;");
        assert!(result.is_ok());
        let (_, nets) = result.unwrap();
        assert_eq!(nets.len(), 1);
        let net = &nets[0];

        let expected_net = Net::new(Identifier::new("z".to_string()), (0, 0), NetType::Wire, 0);
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
            assert_eq!(net.delay, 5);
            assert_eq!(net.range, (7, 0));
        }
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
            vec![Net::new("a".into(), (0, 0), NetType::Wire, 0).with_init(expression("1'b1"))],
        );

        assert_parses_to(
            net_declaration,
            "wire [3:0] q = a + b;",
            vec![Net::new("q".into(), (3, 0), NetType::Wire, 0).with_init(expression("a + b"))],
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
                Net::new("x".into(), (0, 0), NetType::Wire, 0).with_init(expression("1")),
                Net::new("y".into(), (0, 0), NetType::Wire, 0).with_init(expression("2")),
            ],
        );

        assert_parses_to(
            net_declaration,
            "wire [7:0] a = 8'h0f, b;",
            vec![
                Net::new("a".into(), (7, 0), NetType::Wire, 0).with_init(expression("8'h0f")),
                Net::new("b".into(), (7, 0), NetType::Wire, 0),
            ],
        );
    }
}

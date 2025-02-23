use nom::{bytes::complete::tag, combinator::opt, sequence::tuple, IResult};

use super::{
    identifier::{identifier_list, Identifier},
    simple::{range, ws},
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
}

impl Net {
    pub fn new(identifier: Identifier, range: (i64, i64), net_type: NetType, delay: u32) -> Self {
        Net {
            identifier,
            range,
            net_type,
            delay,
        }
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

fn net_declaration(input: &str) -> IResult<&str, Vec<Net>> {
    let (input, net_type) = net_type(input)?;
    let (input, range) = ws(opt(range))(input)?;
    let (input, delay) = opt(parse_delay)(input)?;
    let (input, identifiers) = ws(identifier_list)(input)?;
    let (input, _) = ws(tag(";"))(input)?;

    let nets: Vec<Net> = identifiers
        .iter()
        .map(|identifier| Net {
            identifier: identifier.clone(),
            net_type: net_type.clone(),
            range: range.unwrap_or((0, 0)),
            delay: delay.unwrap_or(0),
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
}

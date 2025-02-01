use nom::{combinator::opt, sequence::tuple, IResult};

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

pub fn net_type(input: &str) -> nom::IResult<&str, NetType> {
    use nom::{branch::alt, bytes::complete::tag, combinator::value};

    alt((
        value(NetType::Wire, tag("wire")),
        value(NetType::WireAnd, tag("wand")),
        value(NetType::WireOr, tag("wor")),
        value(NetType::TriAnd, tag("triand")),
        value(NetType::TriOr, tag("trior")),
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
    use super::*;

    use nom::Parser;

    #[test]
    fn test_net_type() {
        assert_eq!(net_type("wire"), Ok(("", NetType::Wire)));
        assert_eq!(net_type("wand"), Ok(("", NetType::WireAnd)));
        assert_eq!(net_type("wor"), Ok(("", NetType::WireOr)));
        assert_eq!(net_type("tri"), Ok(("", NetType::Tri)));
        assert_eq!(net_type("triand"), Ok(("", NetType::TriAnd)));
        assert_eq!(net_type("trior"), Ok(("", NetType::TriOr)));
        assert_eq!(net_type("supply0"), Ok(("", NetType::Supply0)));
        assert_eq!(net_type("supply1"), Ok(("", NetType::Supply1)));
        assert!(net_type("invalid").is_err());
    }

    #[test]
    fn test_parse_delay() {
        assert_eq!(parse_delay("#10"), Ok(("", 10)));
        assert_eq!(parse_delay("#0"), Ok(("", 0)));
        assert!(parse_delay("10").is_err());
    }

    #[test]
    fn test_net_declaration() {
        let result = net_declaration("wire [7:0] #10  z");
        assert!(result.is_ok());
        let (_, nets) = result.unwrap();
        assert_eq!(nets.len(), 1);
        let net = &nets[0];

        assert_eq!(net.net_type, NetType::Wire);
        assert_eq!(net.delay, 10);
        assert_eq!(net.identifier, Identifier::new("z".to_string()));
        assert_eq!(net.range, (7, 0));

        let result = net_declaration("wire [7:0] z");
        assert!(result.is_ok());
        let (_, nets) = result.unwrap();
        assert_eq!(nets.len(), 1);
        let net = &nets[0];

        assert_eq!(net.net_type, NetType::Wire);
        assert_eq!(net.delay, 0);
        assert_eq!(net.identifier, Identifier::new("z".to_string()));
        assert_eq!(net.range, (7, 0));
    }
}

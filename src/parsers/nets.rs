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
        value(NetType::Tri0, tag("tri0")),
        value(NetType::Tri1, tag("tri1")),
    ))(input)
}

fn net_declaration(input: &str) -> IResult<&str, (NetType, Option<(i64, i64)>, Vec<Identifier>)> {
    tuple((net_type, ws(opt(range)), ws(identifier_list)))(input)
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
        assert_eq!(net_type("tri0"), Ok(("", NetType::Tri0)));
        assert_eq!(net_type("tri1"), Ok(("", NetType::Tri1)));
        assert!(net_type("invalid").is_err());
    }

    #[test]
    fn test_net_declaration() {
        assert_eq!(
            net_declaration("wire [7:0] a, b, c"),
            Ok((
                "",
                (
                    NetType::Wire,
                    Some((7, 0)),
                    vec![
                        Identifier::new("a".to_string()),
                        Identifier::new("b".to_string()),
                        Identifier::new("c".to_string())
                    ]
                )
            ))
        );

        assert_eq!(
            net_declaration("tri0 a, b, c"),
            Ok((
                "",
                (
                    NetType::Tri0,
                    None,
                    vec![
                        Identifier::new("a".to_string()),
                        Identifier::new("b".to_string()),
                        Identifier::new("c".to_string())
                    ]
                )
            ))
        );

        assert_eq!(
            net_declaration("tri1 [3:0] x, y, z"),
            Ok((
                "",
                (
                    NetType::Tri1,
                    Some((3, 0)),
                    vec![
                        Identifier::new("x".to_string()),
                        Identifier::new("y".to_string()),
                        Identifier::new("z".to_string())
                    ]
                )
            ))
        );
    }
}

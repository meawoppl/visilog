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
    use nom::{
        branch::alt,
        bytes::complete::tag,
        combinator::value,
    };

    alt((
        value(NetType::Wire, tag("wire")),
        value(NetType::WireAnd, tag("wand")),
        value(NetType::WireOr, tag("wor")),
        value(NetType::Tri, tag("tri")),
        value(NetType::TriAnd, tag("triand")),
        value(NetType::TriOr, tag("trior")),
        value(NetType::Supply0, tag("supply0")),
        value(NetType::Supply1, tag("supply1")),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

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
}

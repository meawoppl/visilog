use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, multispace0},
    combinator::{map, opt, recognize},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

use super::identifier::identifier;
use super::simple::range;

#[derive(Debug, PartialEq)]
pub enum Operand {
    Number(String),
    Net(String),
    Register(String),
    NetBitSelect(String, usize),
    RegisterBitSelect(String, usize),
    NetPartSelect(String, usize, usize),
    RegisterPartSelect(String, usize, usize),
    MemoryElement(String, usize),
}

fn parse_number(input: &str) -> IResult<&str, Operand> {
    map(recognize(digit1), |num: &str| Operand::Number(num.to_string()))(input)
}

fn parse_net(input: &str) -> IResult<&str, Operand> {
    map(identifier, |id: String| Operand::Net(id))(input)
}

fn parse_register(input: &str) -> IResult<&str, Operand> {
    map(identifier, |id: String| Operand::Register(id))(input)
}

fn parse_bit_select(input: &str) -> IResult<&str, usize> {
    delimited(tag("["), map(digit1, |num: &str| num.parse::<usize>().unwrap()), tag("]"))(input)
}

fn parse_part_select(input: &str) -> IResult<&str, (usize, usize)> {
    delimited(
        tag("["),
        tuple((
            map(digit1, |num: &str| num.parse::<usize>().unwrap()),
            preceded(tag(":"), map(digit1, |num: &str| num.parse::<usize>().unwrap())),
        )),
        tag("]"),
    )(input)
}

fn parse_net_bit_select(input: &str) -> IResult<&str, Operand> {
    map(
        pair(identifier, parse_bit_select),
        |(id, index)| Operand::NetBitSelect(id, index),
    )(input)
}

fn parse_register_bit_select(input: &str) -> IResult<&str, Operand> {
    map(
        pair(identifier, parse_bit_select),
        |(id, index)| Operand::RegisterBitSelect(id, index),
    )(input)
}

fn parse_net_part_select(input: &str) -> IResult<&str, Operand> {
    map(
        pair(identifier, parse_part_select),
        |(id, (msb, lsb))| Operand::NetPartSelect(id, msb, lsb),
    )(input)
}

fn parse_register_part_select(input: &str) -> IResult<&str, Operand> {
    map(
        pair(identifier, parse_part_select),
        |(id, (msb, lsb))| Operand::RegisterPartSelect(id, msb, lsb),
    )(input)
}

fn parse_memory_element(input: &str) -> IResult<&str, Operand> {
    map(
        pair(identifier, parse_bit_select),
        |(id, index)| Operand::MemoryElement(id, index),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_number() {
        assert_eq!(parse_number("123"), Ok(("", Operand::Number("123".to_string()))));
    }

    #[test]
    fn test_parse_net() {
        assert_eq!(parse_net("net1"), Ok(("", Operand::Net("net1".to_string()))));
    }

    #[test]
    fn test_parse_register() {
        assert_eq!(parse_register("reg1"), Ok(("", Operand::Register("reg1".to_string()))));
    }

    #[test]
    fn test_parse_net_bit_select() {
        assert_eq!(
            parse_net_bit_select("net1[3]"),
            Ok(("", Operand::NetBitSelect("net1".to_string(), 3)))
        );
    }

    #[test]
    fn test_parse_register_bit_select() {
        assert_eq!(
            parse_register_bit_select("reg1[3]"),
            Ok(("", Operand::RegisterBitSelect("reg1".to_string(), 3)))
        );
    }

    #[test]
    fn test_parse_net_part_select() {
        assert_eq!(
            parse_net_part_select("net1[7:0]"),
            Ok(("", Operand::NetPartSelect("net1".to_string(), 7, 0)))
        );
    }

    #[test]
    fn test_parse_register_part_select() {
        assert_eq!(
            parse_register_part_select("reg1[7:0]"),
            Ok(("", Operand::RegisterPartSelect("reg1".to_string(), 7, 0)))
        );
    }

    #[test]
    fn test_parse_memory_element() {
        assert_eq!(
            parse_memory_element("mem1[3]"),
            Ok(("", Operand::MemoryElement("mem1".to_string(), 3)))
        );
    }
}

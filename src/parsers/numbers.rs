use nom::character::complete::char;
use nom::{bytes::complete::take_while1, combinator::opt, sequence::preceded, IResult};

pub fn binary(input: &str) -> IResult<&str, &str> {
    preceded(
        opt(char('_')),
        take_while1(|c: char| c == '0' || c == '1' || c == '_'),
    )(input)
}

pub fn decimal(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c.is_digit(10))(input)
}

pub fn hexadecimal(input: &str) -> IResult<&str, &str> {
    preceded(
        opt(char('_')),
        take_while1(|c: char| c.is_digit(16) || c == '_'),
    )(input)
}

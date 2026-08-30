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

/// The digits of a based constant. Any base's digits are accepted, along with
/// the unknown/high-impedance values `x` and `z` (and `?`, which is a synonym
/// for `z`).
pub fn based_digits(input: &str) -> IResult<&str, &str> {
    preceded(
        opt(char('_')),
        take_while1(|c: char| c.is_digit(16) || matches!(c, 'x' | 'X' | 'z' | 'Z' | '?' | '_')),
    )(input)
}

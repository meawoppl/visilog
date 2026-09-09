use nom::branch::alt;
use nom::bytes::complete::take_while;
use nom::character::complete::{char, digit1, one_of};
use nom::combinator::recognize;
use nom::sequence::{pair, tuple};
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

/// A decimal digit run with the `_` separators Verilog allows inside one. It
/// has to *start* with a digit — `_5` is not a number — which is what keeps
/// this from claiming the leading `_` of an identifier.
fn unsigned_number(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        digit1,
        take_while(|c: char| c.is_ascii_digit() || c == '_'),
    ))(input)
}

/// `0.9`, `1e3`, `1.5e-3`, `1_000.5` — a real number, in the two spellings
/// IEEE 1364 gives it: a fixed point number, or a mantissa with an exponent.
///
/// Digits are required on **both** sides of the `.`, so `.5` and `3.` are not
/// real numbers — which is what lets `a[3].b` still read as a hierarchical
/// name and `3.` as a number followed by something else. This is the only real
/// number parser there is: the expression grammar, a `specify` path delay and
/// a `specparam` all come here, so they cannot disagree about what one is.
pub fn real_number(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        unsigned_number,
        alt((
            recognize(tuple((char('.'), unsigned_number, opt(exponent)))),
            exponent,
        )),
    )))(input)
}

/// The `e-3` of `1.5e-3`. Its digits are not optional, so `1e` is not a real
/// number and falls back to the integer `1`.
fn exponent(input: &str) -> IResult<&str, &str> {
    recognize(tuple((one_of("eE"), opt(one_of("+-")), unsigned_number)))(input)
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

use nom::{
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::{map_res, opt},
    IResult,
};

use super::{numbers::decimal, simple::ws};

#[derive(Debug, PartialEq, Clone)]
pub struct Delay {
    delay: i64,
}

impl Delay {
    pub fn new(delay: i64) -> Self {
        Delay { delay }
    }
}

pub fn parse_delay(input: &str) -> IResult<&str, Delay> {
    let (input, _) = tag("#")(input)?;
    let (input, delay) = map_res(decimal, |s: &str| s.parse::<i64>())(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, Delay::new(delay)))
}

pub fn parse_delay_opt(input: &str) -> IResult<&str, Option<Delay>> {
    opt(parse_delay)(input)
}

pub fn parse_delay_statement(input: &str) -> IResult<&str, Delay> {
    let (input, delay) = ws(parse_delay)(input)?;
    let (input, _) = ws(tag(";"))(input)?;
    Ok((input, delay))
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_delay() {
        assert_eq!(parse_delay("#10"), Ok(("", Delay::new(10))));
        assert_eq!(parse_delay("#123"), Ok(("", Delay::new(123))));
        assert_eq!(parse_delay("#0"), Ok(("", Delay::new(0))));
        assert!(parse_delay("10").is_err());
        assert!(parse_delay("#abc").is_err());
    }

    #[test]
    fn test_parse_delay_opt() {
        assert_eq!(parse_delay_opt("#10"), Ok(("", Some(Delay::new(10)))));
        assert_eq!(parse_delay_opt("#123"), Ok(("", Some(Delay::new(123)))));
        assert_eq!(parse_delay_opt("#0"), Ok(("", Some(Delay::new(0)))));
        assert_eq!(parse_delay_opt("10"), Ok(("10", None)));
        assert_eq!(parse_delay_opt(""), Ok(("", None)));
    }

    #[test]
    fn test_parse_delay_statement() {
        assert_eq!(parse_delay_statement("#10;"), Ok(("", Delay::new(10))));
        assert_eq!(parse_delay_statement("#123 ;"), Ok(("", Delay::new(123))));
        assert_eq!(parse_delay_statement("#0;"), Ok(("", Delay::new(0))));
        assert!(parse_delay_statement("#10").is_err());
        assert!(parse_delay_statement("10;").is_err());
    }
}

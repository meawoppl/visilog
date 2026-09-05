use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, map_res, opt},
    sequence::delimited,
    IResult,
};

use super::{
    numbers::decimal,
    simple::{ws, ws_and_comments},
};

/// A delay, which Verilog writes either as one value (`#10`) or as a
/// `min:typ:max` triple (`#(2:10:17)`).
///
/// All three values are kept, because which one a simulator uses is a *run*
/// option (`+mindelays` / `+typdelays` / `+maxdelays`), not something the
/// grammar decides. Discarding two of them at parse time would make that option
/// unimplementable without re-reading the source.
#[derive(Debug, PartialEq, Clone)]
pub struct Delay {
    minimum: i64,
    typical: i64,
    maximum: i64,
}

impl Delay {
    /// A plain `#10`, whose three values are all the same.
    pub fn new(delay: i64) -> Self {
        Delay {
            minimum: delay,
            typical: delay,
            maximum: delay,
        }
    }

    /// A `min:typ:max` triple.
    pub fn triple(minimum: i64, typical: i64, maximum: i64) -> Self {
        Delay {
            minimum,
            typical,
            maximum,
        }
    }

    /// The number of time units to wait.
    ///
    /// This is the **one** place a delay mode is chosen: everything that
    /// schedules a delay goes through here, so honouring `+mindelays` or
    /// `+maxdelays` later is a change to this function and to nothing else.
    /// The default, and what this returns, is the typical value.
    pub fn ticks(&self) -> i64 {
        self.typical
    }

    /// The `min` of a `min:typ:max` triple; the value itself for a plain delay.
    pub fn minimum(&self) -> i64 {
        self.minimum
    }

    /// The `typ` of a `min:typ:max` triple; the value itself for a plain delay.
    pub fn typical(&self) -> i64 {
        self.typical
    }

    /// The `max` of a `min:typ:max` triple; the value itself for a plain delay.
    pub fn maximum(&self) -> i64 {
        self.maximum
    }
}

/// `#10`, `# 10`, `#/* wait */10`, `#(10)`, `#(2:10:17)` — a delay term.
///
/// The `#` and its value are separate tokens, so whitespace and comments are
/// legal between them just as they are anywhere else.
pub fn parse_delay(input: &str) -> IResult<&str, Delay> {
    let (input, _) = tag("#")(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, delay) = alt((parenthesised_delay, map(delay_value, Delay::new)))(input)?;
    let (input, _) = ws_and_comments(input)?;
    Ok((input, delay))
}

/// One unsigned delay value.
fn delay_value(input: &str) -> IResult<&str, i64> {
    map_res(decimal, |s: &str| s.parse::<i64>())(input)
}

/// `(2:10:17)` or `(10)` — the parenthesised form, which is the only place a
/// `min:typ:max` triple is legal. The triple is tried first: the single-value
/// branch would match `2` and then choke on the `:`, and an `alt` inside a
/// `delimited` gets no second chance once the closing paren fails.
fn parenthesised_delay(input: &str) -> IResult<&str, Delay> {
    delimited(
        tag("("),
        alt((delay_triple, map(ws(delay_value), Delay::new))),
        tag(")"),
    )(input)
}

/// `2:10:17` — min, typical and max, in that order.
fn delay_triple(input: &str) -> IResult<&str, Delay> {
    let (input, minimum) = ws(delay_value)(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, typical) = ws(delay_value)(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, maximum) = ws(delay_value)(input)?;
    Ok((input, Delay::triple(minimum, typical, maximum)))
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

    use crate::parsers::modules::parse_module_declaration;
    use crate::simulator::runner::Simulator;

    #[test]
    fn test_parse_delay() {
        assert_eq!(parse_delay("#10"), Ok(("", Delay::new(10))));
        assert_eq!(parse_delay("#123"), Ok(("", Delay::new(123))));
        assert_eq!(parse_delay("#0"), Ok(("", Delay::new(0))));
        assert!(parse_delay("10").is_err());
        assert!(parse_delay("#abc").is_err());
    }

    /// The `#` and its value are separate tokens, so the usual token-boundary
    /// rules apply between them.
    #[test]
    fn test_parse_delay_allows_whitespace_after_the_hash() {
        assert_eq!(parse_delay("# 3"), Ok(("", Delay::new(3))));
        assert_eq!(parse_delay("#\n   12"), Ok(("", Delay::new(12))));
        assert_eq!(parse_delay("#/* wait */5"), Ok(("", Delay::new(5))));
        assert_eq!(parse_delay_statement("# 3;"), Ok(("", Delay::new(3))));
        assert_eq!(parse_delay_statement("# 12 ;"), Ok(("", Delay::new(12))));
        // A `#` with nothing to delay by is still not a delay.
        assert!(parse_delay("# ;").is_err());
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

    #[test]
    fn test_a_parenthesised_delay_is_a_plain_delay() {
        assert_eq!(parse_delay("#(10)"), Ok(("", Delay::new(10))));
        assert_eq!(parse_delay("# ( 10 )"), Ok(("", Delay::new(10))));
        assert_eq!(parse_delay_statement("#(10);"), Ok(("", Delay::new(10))));
    }

    /// A `min:typ:max` triple keeps all three values — the min and the max are
    /// what a `+mindelays` / `+maxdelays` run option would need.
    #[test]
    fn test_a_min_typ_max_triple_keeps_every_value() {
        let delay = parse_delay("#(2:10:17)").expect("triple should parse").1;
        assert_eq!(delay, Delay::triple(2, 10, 17));
        assert_eq!(delay.minimum(), 2);
        assert_eq!(delay.typical(), 10);
        assert_eq!(delay.maximum(), 17);
        // Typical is the default, so it is what gets scheduled.
        assert_eq!(delay.ticks(), 10);
    }

    #[test]
    fn test_a_triple_tolerates_whitespace_and_comments() {
        assert_eq!(
            parse_delay("# ( 2 : 10 : 17 )"),
            Ok(("", Delay::triple(2, 10, 17)))
        );
        assert_eq!(
            parse_delay("#(1/* min */:2:/* max */3)"),
            Ok(("", Delay::triple(1, 2, 3)))
        );
        assert_eq!(
            parse_delay_statement("#(2:10:17) ;"),
            Ok(("", Delay::triple(2, 10, 17)))
        );
        // Two of the three is not a triple.
        assert!(parse_delay("#(2:10)").is_err());
    }

    /// A triple is legal in every position a plain delay is: as a statement of
    /// its own, as a statement prefix, and before the right hand side of an
    /// assignment.
    #[test]
    fn test_a_triple_parses_everywhere_a_delay_does() {
        for body in [
            "initial #(2:10:17) a = 1;",
            "initial begin #(2:10:17); a = 1; end",
            "initial #(2:10:17) begin a = 1; end",
            "always value1 = # (2:10:17) 4'h5 ;",
        ] {
            let source = format!("module m(); reg a; reg [3:0] value1; {} endmodule", body);
            let (remaining, _) = parse_module_declaration(&source)
                .unwrap_or_else(|error| panic!("{} should parse: {:?}", body, error));
            assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        }
    }

    /// The typical value is the one that reaches the event queue: `a` is still
    /// `x` at time 9 and set at time 10, so neither the min nor the max was
    /// used.
    #[test]
    fn test_a_triple_simulates_using_its_typical_value() {
        let source = "module m(); reg a; initial #(2:10:17) a = 1; endmodule";
        let (remaining, module) = parse_module_declaration(source).expect("module should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        let mut simulator = Simulator::new(module);
        simulator.setup().expect("setup should succeed");

        simulator.advance(9).expect("advance should succeed");
        assert_eq!(
            simulator.get("a").expect("a should exist").to_binary(),
            "x",
            "the minimum, 2, must not have been used"
        );

        simulator.advance(1).expect("advance should succeed");
        assert_eq!(
            simulator.get("a").expect("a should exist").to_binary(),
            "1",
            "the typical value, 10, should have fired"
        );
    }
}

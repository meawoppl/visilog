use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt},
    multi::separated_list1,
    sequence::delimited,
    IResult,
};

use super::{
    constants::{verilog_const, VerilogConstant},
    expr::{verilog_expression, Expression},
    identifier::hierarchical_identifier,
    simple::{ws, ws_and_comments},
};
use crate::register::Register;
use crate::simulator::eval::{eval, EvalError};
use crate::simulator::state_store::StateStore;

/// A delay, which Verilog writes either as one value (`#10`) or as a
/// `min:typ:max` triple (`#(2:10:17)`).
///
/// All three values are kept, because which one a simulator uses is a *run*
/// option (`+mindelays` / `+typdelays` / `+maxdelays`), not something the
/// grammar decides. Discarding two of them at parse time would make that option
/// unimplementable without re-reading the source.
///
/// Each one is an **expression**, not a number. `#tPD`, `#(period/2)` and
/// `always #(clk_period/2) clk = ~clk;` are all ordinary Verilog, and the value
/// a parameter or a variable holds does not exist until the design elaborates
/// — so the value is worked out where it is *waited on* rather than where it is
/// written.
#[derive(Debug, PartialEq, Clone)]
pub struct Delay {
    minimum: Expression,
    typical: Expression,
    maximum: Expression,
}

impl Delay {
    /// A plain `#10`, whose three values are all the same.
    pub fn new(delay: i64) -> Self {
        Delay::from_expression(Expression::Constant(VerilogConstant::from_int(delay)))
    }

    /// A single delay term, which stands for all three.
    pub fn from_expression(delay: Expression) -> Self {
        Delay {
            minimum: delay.clone(),
            typical: delay.clone(),
            maximum: delay,
        }
    }

    /// A `min:typ:max` triple.
    pub fn triple(minimum: i64, typical: i64, maximum: i64) -> Self {
        Delay::triple_of(
            Expression::Constant(VerilogConstant::from_int(minimum)),
            Expression::Constant(VerilogConstant::from_int(typical)),
            Expression::Constant(VerilogConstant::from_int(maximum)),
        )
    }

    /// A `min:typ:max` triple written as three expressions.
    pub fn triple_of(minimum: Expression, typical: Expression, maximum: Expression) -> Self {
        Delay {
            minimum,
            typical,
            maximum,
        }
    }

    /// The number of time units to wait, worked out against `store`.
    ///
    /// This is the **one** place a delay mode is chosen: everything that
    /// schedules a delay goes through here, so honouring `+mindelays` or
    /// `+maxdelays` later is a change to this function and to nothing else.
    /// The default, and what this returns, is the typical value.
    ///
    /// A delay that evaluates to `x` or `z` is zero, which is what iverilog
    /// does with one — there is no length of time an unknown stands for, and
    /// refusing to run would stop a design over a value it never waits on.
    pub fn ticks(&self, store: &StateStore) -> Result<i64, EvalError> {
        Ok(Delay::to_ticks(&eval(&self.typical, store)?))
    }

    /// The three expressions, for a pass that rewrites the names in them.
    pub fn expressions_mut(&mut self) -> [&mut Expression; 3] {
        [&mut self.minimum, &mut self.typical, &mut self.maximum]
    }

    /// The three expressions, for a pass that only reads them — collecting the
    /// signals a delay depends on, say.
    pub fn expressions(&self) -> [&Expression; 3] {
        [&self.minimum, &self.typical, &self.maximum]
    }

    /// The `min` of a `min:typ:max` triple; the value itself for a plain delay.
    pub fn minimum(&self) -> &Expression {
        &self.minimum
    }

    /// The `typ` of a `min:typ:max` triple; the value itself for a plain delay.
    pub fn typical(&self) -> &Expression {
        &self.typical
    }

    /// The `max` of a `min:typ:max` triple; the value itself for a plain delay.
    pub fn maximum(&self) -> &Expression {
        &self.maximum
    }

    /// A delay is unsigned and a resumption is scheduled at `now + ticks`, so
    /// this is clamped rather than wrapped: a value too wide to be an `i64`
    /// would otherwise come back negative and queue a block *before* the one
    /// that scheduled it.
    fn to_ticks(value: &Register) -> i64 {
        value.to_u128().unwrap_or(0).min(i64::MAX as u128) as i64
    }
}

/// `#10`, `# 10`, `#/* wait */10`, `#tPD`, `#(10)`, `#(period/2)`,
/// `#(2:10:17)` — a delay term.
///
/// The `#` and its value are separate tokens, so whitespace and comments are
/// legal between them just as they are anywhere else. The **unparenthesised**
/// form is a number or a name and nothing more, exactly as the LRM has it: a
/// full expression there would run on into the statement the delay prefixes.
pub fn parse_delay(input: &str) -> IResult<&str, Delay> {
    let (input, _) = tag("#")(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, delay) = alt((
        parenthesised_delay,
        map(delay_operand, Delay::from_expression),
    ))(input)?;
    let (input, _) = ws_and_comments(input)?;
    Ok((input, delay))
}

/// `#5`, `#(2, 7)`, `#(2, 7, 9)`, `#(1:2:3, 4:5:6)` — the delay a *gate*
/// primitive carries.
///
/// A gate writes up to three delays rather than one — rise, fall and turn-off —
/// which is the whole difference from [`parse_delay`]. Only the first is kept:
/// the simulator settles a gate in zero time along with every other continuous
/// driver, so nothing downstream can tell one delay from three, and a `Delay`
/// that pretended to hold all of them would be a shape no caller reads.
pub fn parse_gate_delay(input: &str) -> IResult<&str, Delay> {
    let (input, _) = tag("#")(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, delays) = alt((
        delimited(
            tag("("),
            separated_list1(tag(","), ws(delay_term)),
            tag(")"),
        ),
        map(delay_operand, |delay| vec![Delay::from_expression(delay)]),
    ))(input)?;
    let (input, _) = ws_and_comments(input)?;
    Ok((
        input,
        delays
            .into_iter()
            .next()
            .expect("a separated list holds at least one delay"),
    ))
}

/// One delay written inside parentheses, in either of the two spellings a
/// single value may take.
fn delay_term(input: &str) -> IResult<&str, Delay> {
    alt((
        delay_triple,
        map(verilog_expression, Delay::from_expression),
    ))(input)
}

/// The value an *unparenthesised* delay may take: an unsigned number, or the
/// name of a parameter or a variable.
///
/// It is deliberately not a full expression. A delay prefixes a statement and
/// the two are separated by nothing but whitespace, so `#5 a = 1;` would give
/// an expression parser `5 a` to chew on and `#2 -> ev;` would give it `2 - >`.
/// The LRM says the same thing: an expression is legal here only inside
/// parentheses.
fn delay_operand(input: &str) -> IResult<&str, Expression> {
    alt((
        map(verilog_const, Expression::Constant),
        map(hierarchical_identifier, Expression::Identifier),
    ))(input)
}

/// `(2:10:17)`, `(10)` or `(period / 2)` — the parenthesised form, which is
/// the only place a `min:typ:max` triple or a compound expression is legal.
/// The triple is tried first: the single-value branch would match `2` and then
/// choke on the `:`, and an `alt` inside a `delimited` gets no second chance
/// once the closing paren fails.
fn parenthesised_delay(input: &str) -> IResult<&str, Delay> {
    delimited(tag("("), ws(delay_term), tag(")"))(input)
}

/// `2:10:17` — min, typical and max, in that order.
fn delay_triple(input: &str) -> IResult<&str, Delay> {
    let (input, minimum) = ws(verilog_expression)(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, typical) = ws(verilog_expression)(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, maximum) = ws(verilog_expression)(input)?;
    Ok((input, Delay::triple_of(minimum, typical, maximum)))
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

    use crate::parsers::identifier::Identifier;
    use crate::parsers::modules::parse_module_declaration;
    use crate::simulator::runner::Simulator;

    fn constant(value: i64) -> Expression {
        Expression::Constant(VerilogConstant::from_int(value))
    }

    #[test]
    fn test_parse_delay() {
        assert_eq!(parse_delay("#10"), Ok(("", Delay::new(10))));
        assert_eq!(parse_delay("#123"), Ok(("", Delay::new(123))));
        assert_eq!(parse_delay("#0"), Ok(("", Delay::new(0))));
        assert!(parse_delay("10").is_err());
        assert!(parse_delay("#;").is_err());
    }

    /// `#tPD` is a delay naming a parameter or a variable, which is what makes
    /// a delay an expression rather than a number.
    #[test]
    fn test_a_delay_may_name_a_parameter() {
        assert_eq!(
            parse_delay("#tPD"),
            Ok((
                "",
                Delay::from_expression(Expression::Identifier(Identifier::new("tPD".to_string())))
            ))
        );
    }

    /// Inside parentheses a delay is a whole expression: `#(period / 2)` is
    /// how nearly every corpus clock generator is written.
    #[test]
    fn test_a_parenthesised_delay_may_be_an_expression() {
        let delay = parse_delay("#(period / 2)")
            .expect("an expression delay should parse")
            .1;
        let mut store = StateStore::new();
        store.set("period".to_string(), Register::from_u128(20, 32));
        assert_eq!(delay.ticks(&store), Ok(10));
    }

    /// The unparenthesised form stops at the name or the number, so the
    /// statement the delay prefixes is still there to be parsed.
    #[test]
    fn test_an_unparenthesised_delay_does_not_run_into_the_statement() {
        assert_eq!(parse_delay("#5 a = 1;").unwrap().0, "a = 1;");
        assert_eq!(parse_delay("#n -> ev;").unwrap().0, "-> ev;");
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
        assert_eq!(delay.minimum(), &constant(2));
        assert_eq!(delay.typical(), &constant(10));
        assert_eq!(delay.maximum(), &constant(17));
        // Typical is the default, so it is what gets scheduled.
        assert_eq!(delay.ticks(&StateStore::new()), Ok(10));
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

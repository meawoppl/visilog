//! Simulating a user-defined primitive.
//!
//! A UDP is a truth table, and a combinational one is a **continuous driver**:
//! it belongs with the gates and the `assign` statements in the settle loop,
//! and [`Simulator::propagate`](crate::simulator::runner::Simulator)
//! re-evaluates it on every pass until the design stops moving. Its output net
//! is resolved between all of its drivers exactly as a gate's is, so a UDP and
//! a `bufif1` may share a bus without either one having to know.
//!
//! # What a lookup does
//!
//! Each terminal is one bit. An input's `z` is read as an `x` before anything
//! is matched — a table has no symbol for a floating input — and then the rows
//! are consulted by [`UdpTable::combinational_output`], whose rule is
//! iverilog's: a `0` row beats a `1` row and either beats an unmatched
//! combination, which is `x`.
//!
//! # What is not modelled
//!
//! A **sequential** UDP is a named error rather than a driver that quietly
//! does nothing. Its rows ask about the *previous* value of an input — `(01)`
//! is a question no lookup of the present levels can answer — and its output
//! is a register the primitive owns rather than a function of what is on its
//! terminals now. Both are outside what a continuous driver is handed, so a
//! design that instantiates one stops here, by name.

use crate::parsers::expr::Expression;
use crate::parsers::primitive::UdpTable;
use crate::simulator::eval::eval;
use crate::simulator::gates::{as_level, least_significant_bit};
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::StateStore;

/// One elaborated combinational UDP instance, with its terminals split the way
/// the declaration split them: the output first, the inputs after.
pub struct Udp {
    /// The primitive's name, so an error about this instance can say which
    /// table it came from.
    pub name: String,
    pub output: Expression,
    pub inputs: Vec<Expression>,
    /// The table itself. Owned rather than borrowed: elaboration hands the
    /// simulator a flat model that outlives the modules it was built from.
    pub table: UdpTable,
}

impl Udp {
    /// The bit this instance is driving, given the design's present state.
    ///
    /// A terminal is one bit wide, so an input wider than that is read at its
    /// least significant bit — the same bit a scalar connection would name.
    pub fn evaluate(&self, state: &StateStore) -> Result<u8, SimulationError> {
        let mut levels = Vec::with_capacity(self.inputs.len());
        for input in &self.inputs {
            levels.push(as_level(least_significant_bit(&eval(input, state)?)));
        }
        Ok(self.table.combinational_output(&levels))
    }
}

#[cfg(test)]
mod tests {
    use crate::parsers::source::parse_verilog_source;
    use crate::register::{Register, ONE, X, Z, ZERO};
    use crate::simulator::runner::{SimulationError, Simulator};

    /// A one-bit stimulus value.
    fn bit(code: u8) -> Register {
        Register::from_bits(vec![code])
    }

    fn shown(code: u8) -> char {
        match code {
            ZERO => '0',
            ONE => '1',
            X => 'x',
            _ => 'z',
        }
    }

    fn simulator_for(source: &str) -> Simulator {
        let (remaining, modules) = parse_verilog_source(source).expect("design should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        let mut simulator = Simulator::with_modules(modules, "main");
        simulator.setup().expect("design should elaborate");
        simulator
    }

    const MUX: &str = r#"
        primitive mux (q, sel, a, b);
          output q;
          input sel, a, b;
          table
            0 1 ? : 1 ;
            0 0 ? : 0 ;
            1 ? 1 : 1 ;
            1 ? 0 : 0 ;
            x 0 0 : 0 ;
            x 1 1 : 1 ;
          endtable
        endprimitive

        module main(input sel, input a, input b, output q);
          mux u (q, sel, a, b);
        endmodule
    "#;

    /// A UDP instance is a continuous driver: its output follows its inputs
    /// through the same settle loop a gate's does.
    #[test]
    fn test_a_primitive_drives_its_output() {
        let mut simulator = simulator_for(MUX);
        for (sel, a, b, expected) in [
            (ZERO, ONE, ZERO, ONE),
            (ZERO, ZERO, ONE, ZERO),
            (ONE, ZERO, ONE, ONE),
            (ONE, ONE, ZERO, ZERO),
            // Both data inputs agree, so an unknown select still decides.
            (X, ONE, ONE, ONE),
            // They disagree, and no row matches at all.
            (X, ONE, ZERO, X),
            // A `?` column matches an unknown input.
            (ZERO, ONE, X, ONE),
        ] {
            simulator.poke("sel", bit(sel)).unwrap();
            simulator.poke("a", bit(a)).unwrap();
            simulator.poke("b", bit(b)).unwrap();
            assert_eq!(
                simulator.get("q").unwrap().to_binary(),
                shown(expected).to_string(),
                "mux({}, {}, {})",
                shown(sel),
                shown(a),
                shown(b)
            );
        }
    }

    /// A floating input is read as an `x`, which is the one conversion a table
    /// cannot express: there is no `z` symbol to write.
    #[test]
    fn test_a_floating_input_is_read_as_unknown() {
        let mut simulator = simulator_for(MUX);
        simulator.poke("sel", bit(ZERO)).unwrap();
        simulator.poke("a", bit(Z)).unwrap();
        simulator.poke("b", bit(ZERO)).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_binary(), "x");
    }

    /// A sequential UDP is a named error at elaboration, never a driver that
    /// quietly settles on something plausible.
    #[test]
    fn test_a_sequential_primitive_is_a_named_error() {
        let source = r#"
            primitive latch (q, e, d);
              output q;
              reg q;
              input e, d;
              table
                1 1 : ? : 1 ;
                1 0 : ? : 0 ;
                0 ? : ? : - ;
              endtable
            endprimitive

            module main(input e, input d, output q);
              latch u (q, e, d);
            endmodule
        "#;
        let (_, modules) = parse_verilog_source(source).expect("design should parse");
        let mut simulator = Simulator::with_modules(modules, "main");
        assert_eq!(
            simulator.setup().err(),
            Some(SimulationError::SequentialPrimitive("latch".to_string()))
        );
    }
}

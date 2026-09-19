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
//! # A sequential UDP
//!
//! One whose output is a `reg` keeps a state of its own and has rows that ask
//! about an **edge** — `(01)`, `p`, `*` — which is a question about what an
//! input *was*. [`UdpMemory`] answers it: the inputs as they stood at the last
//! lookup and the output the primitive holds, behind a `RefCell` so the lookup
//! can still be made through the shared reference `propagate` holds.
//! Remembering on the instance rather than in the simulator is what keeps a
//! sequential UDP an ordinary continuous driver — nothing in the settle loop
//! had to learn that one exists.
//!
//! Evaluating one twice with nothing moved in between is harmless: no edge is
//! seen, so the output holds, which is what lets the fixpoint re-ask it as
//! often as it likes. The rules themselves are
//! [`UdpTable::sequential_output`]'s.

use std::cell::RefCell;

use crate::parsers::delay::GateDelay;
use crate::parsers::expr::Expression;
use crate::parsers::primitive::UdpTable;
use crate::register::X;
use crate::simulator::eval::eval;
use crate::simulator::gates::{as_level, least_significant_bit};
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::StateStore;

/// One elaborated UDP instance, with its terminals split the way the
/// declaration split them: the output first, the inputs after.
pub struct Udp {
    /// The primitive's name, so an error about this instance can say which
    /// table it came from.
    pub name: String,
    pub output: Expression,
    pub inputs: Vec<Expression>,
    /// The table itself. Owned rather than borrowed: elaboration hands the
    /// simulator a flat model that outlives the modules it was built from.
    pub table: UdpTable,
    /// What a **sequential** instance remembers between lookups; `None` for a
    /// combinational one, which remembers nothing.
    pub memory: Option<RefCell<UdpMemory>>,
    /// The `#(...)` the instantiation wrote, which on a primitive is a delay
    /// rather than a parameter override. A UDP is a continuous driver exactly
    /// as a gate is, so it is the same [`GateDelay`] and goes through the same
    /// `DelayedDrive` machinery.
    pub delay: Option<GateDelay>,
}

/// What a sequential UDP carries from one lookup to the next.
#[derive(Debug, Clone)]
pub struct UdpMemory {
    /// The inputs at the last lookup, which an edge column is measured
    /// against. `None` until the first one: before that nothing has had a
    /// chance to move, so no row with an edge in it can match.
    previous: Option<Vec<u8>>,
    /// The output the primitive holds.
    state: u8,
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
        let Some(memory) = &self.memory else {
            return Ok(self.table.combinational_output(&levels));
        };
        let mut memory = memory.borrow_mut();
        let next = self
            .table
            .sequential_output(memory.previous.as_deref(), &levels, memory.state);
        memory.previous = Some(levels);
        memory.state = next;
        Ok(next)
    }

    /// The memory a new instance of `table` starts with, or `None` when the
    /// table is combinational.
    ///
    /// The starting state is the table's `initial` value when it gave one and
    /// `x` when it did not, which is what iverilog prints for a flip-flop that
    /// has not yet been clocked.
    pub fn memory_for(
        table: &UdpTable,
        state: &StateStore,
    ) -> Result<Option<RefCell<UdpMemory>>, SimulationError> {
        if !table.sequential {
            return Ok(None);
        }
        let initial = match &table.initial {
            Some(value) => as_level(least_significant_bit(&eval(value, state)?)),
            None => X,
        };
        Ok(Some(RefCell::new(UdpMemory {
            previous: None,
            state: initial,
        })))
    }
}

#[cfg(test)]
mod tests {
    use crate::parsers::source::parse_verilog_source;
    use crate::register::{Register, ONE, X, Z, ZERO};
    use crate::simulator::runner::Simulator;

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

    /// A sequential UDP keeps its own state and answers edges: a flip-flop,
    /// whose rows name a clock edge, and a latch, whose rows are level
    /// sensitive, driven through one stimulus.
    ///
    /// Every line is what iverilog 12.0 prints for this design — including
    /// `t7`, where the clock goes `1 -> x`: no row speaks to that edge, and an
    /// input change nothing matches drives `x` rather than holding.
    #[test]
    fn test_sequential_primitives_keep_state_and_answer_edges() {
        let source = r#"
            primitive dff(output reg q, input d, input clk);
              table
              // d clk : q : q+
                 0 (01) : ? : 0;
                 1 (01) : ? : 1;
                 ? (?0) : ? : -;
                 * ?    : ? : -;
              endtable
            endprimitive
            primitive latch(output reg q, input d, input en);
              table
                 1 1 : ? : 1;
                 0 1 : ? : 0;
                 ? 0 : ? : -;
              endtable
            endprimitive
            module tb;
              reg d, clk, en;
              wire q, l;
              dff u(q, d, clk);
              latch v(l, d, en);
              initial begin
                d = 0; clk = 0; en = 0;
                #1 $display("t1 q=%b l=%b", q, l);
                d = 1; #1 $display("t2 q=%b l=%b", q, l);
                clk = 1; #1 $display("t3 q=%b l=%b", q, l);
                d = 0; #1 $display("t4 q=%b l=%b", q, l);
                en = 1; #1 $display("t5 q=%b l=%b", q, l);
                d = 1; #1 $display("t6 q=%b l=%b", q, l);
                clk = 1'bx; #1 $display("t7 q=%b l=%b", q, l);
              end
            endmodule
        "#;
        let (_, modules) = parse_verilog_source(source).expect("design should parse");
        let mut simulator = Simulator::with_modules(modules, "tb");
        simulator.setup().expect("should set up");
        simulator.advance(8).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "t1 q=x l=x\nt2 q=x l=x\nt3 q=1 l=x\nt4 q=1 l=x\n\
             t5 q=1 l=0\nt6 q=1 l=1\nt7 q=x l=1\n"
        );
    }

    /// Inputs that move together are taken one at a time. A set/reset latch
    /// whose `initial` is 1 sees `(x, x) -> (0, 0)` at time zero, which passes
    /// through `(0, x)` — a combination no row covers — so its state goes to
    /// `x` rather than holding the 1 that `(0, 0)` alone would have kept.
    ///
    /// iverilog 12.0 prints `t1 q=x`, `t2 q=0`, `t3 q=0` for this design.
    #[test]
    fn test_simultaneous_input_changes_are_taken_one_at_a_time() {
        let source = r#"
            primitive srff(q, s, r);
              output q;
              reg q;
              input s, r;
              initial q = 1'b1;
              table
                1 0 : ? : 1;
                0 1 : ? : 0;
                0 0 : ? : -;
              endtable
            endprimitive
            module tb;
              reg s, r;
              wire q;
              srff u(q, s, r);
              initial begin
                s = 0; r = 0;
                #1 $display("t1 q=%b", q);
                r = 1; #1 $display("t2 q=%b", q);
                r = 0; #1 $display("t3 q=%b", q);
              end
            endmodule
        "#;
        let (_, modules) = parse_verilog_source(source).expect("design should parse");
        let mut simulator = Simulator::with_modules(modules, "tb");
        simulator.setup().expect("should set up");
        simulator.advance(4).expect("time should advance");
        assert_eq!(simulator.output().text(), "t1 q=x\nt2 q=0\nt3 q=0\n");
    }
}

//! Throughput benchmarks for the hot paths called out in issue #80.
//!
//! Each benchmark isolates a different suspected cost so that an optimization
//! can be attributed rather than guessed at:
//!
//! - `tick/*` drive whole sequential designs, the headline number
//! - `eval/*` isolate expression evaluation and `Register` arithmetic
//! - `parse/*` cover the front end, which no optimization work should regress

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::hint::black_box;

use visilog::parsers::expr::verilog_expression;
use visilog::parsers::modules::parse_module_declaration;
use visilog::register::Register;
use visilog::simulator::eval::eval;
use visilog::simulator::runner::Simulator;
use visilog::simulator::state_store::StateStore;

const COUNTER: &str = include_str!("../src/verilog/examples/counter.v");
const CLOCK_DIVIDER: &str = include_str!("../src/verilog/examples/clock_divider.v");
const SIMPLE_MODULE: &str = include_str!("../src/verilog/examples/simple_module.v");
const SPI_CONTROLLER: &str = include_str!("../src/verilog/examples/spi_controller.v");

fn simulator_for(source: &str) -> Simulator {
    let (_, module) = parse_module_declaration(source).expect("example should parse");
    let mut simulator = Simulator::new(module);
    simulator.setup().expect("example should set up");
    simulator
}

fn one() -> Register {
    Register::from_u128(1, 1)
}

fn zero() -> Register {
    Register::from_u128(0, 1)
}

/// Reset a design so ticks start from a known state rather than all-`x`.
fn reset(simulator: &mut Simulator) {
    simulator.poke("rst", one()).expect("rst should drive");
    simulator.poke("rst", zero()).expect("rst should drive");
}

/// Clocking whole designs. `clock_divider` carries a 32-bit counter and
/// `spi_controller` a `case` statement, so the three widen coverage of what a
/// tick actually costs.
fn bench_tick(criterion: &mut Criterion) {
    let mut group = criterion.benchmark_group("tick");
    group.throughput(Throughput::Elements(1));

    for (name, source) in [
        ("counter_4bit", COUNTER),
        ("clock_divider_32bit", CLOCK_DIVIDER),
        ("spi_controller_fsm", SPI_CONTROLLER),
    ] {
        group.bench_function(BenchmarkId::from_parameter(name), |bencher| {
            let mut simulator = simulator_for(source);
            reset(&mut simulator);
            bencher.iter(|| simulator.tick("clk").expect("tick should run"));
        });
    }
    group.finish();
}

/// Driving a purely combinational design: one input write plus a full
/// continuous-assignment settle.
fn bench_combinational(criterion: &mut Criterion) {
    criterion.bench_function("poke/simple_module", |bencher| {
        let mut simulator = simulator_for(SIMPLE_MODULE);
        let mut n = 0u128;
        bencher.iter(|| {
            n = (n + 1) % 16;
            simulator
                .poke("a", Register::from_u128(n, 4))
                .expect("poke should drive")
        });
    });
}

/// Expression evaluation on its own, which is where `Register`'s
/// one-byte-per-bit representation and per-operation allocation show up.
fn bench_eval(criterion: &mut Criterion) {
    let mut group = criterion.benchmark_group("eval");

    let mut store = StateStore::new();
    store.set_ranged("a", Register::from_u128(0xA6, 8), (7, 0));
    store.set_ranged("b", Register::from_u128(0x3C, 8), (7, 0));
    store.set_ranged("wide", Register::from_u128(0xDEAD_BEEF, 32), (31, 0));

    for (name, source) in [
        ("add_8bit", "a + b"),
        ("add_32bit", "wide + wide"),
        ("reduction_xor", "^wide"),
        ("nested_arithmetic", "((a + b) * 2) - (a & b)"),
        ("conditional", "(a > b) ? (a - b) : (b - a)"),
    ] {
        let (_, expression) = verilog_expression(source).expect("expression should parse");
        group.bench_function(BenchmarkId::from_parameter(name), |bencher| {
            bencher.iter(|| eval(black_box(&expression), black_box(&store)).expect("should eval"));
        });
    }
    group.finish();
}

/// The front end. Optimization work on the simulator should leave this alone;
/// it is here to catch an accidental regression.
fn bench_parse(criterion: &mut Criterion) {
    let mut group = criterion.benchmark_group("parse");
    for (name, source) in [("counter", COUNTER), ("spi_controller", SPI_CONTROLLER)] {
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_function(BenchmarkId::from_parameter(name), |bencher| {
            bencher.iter(|| parse_module_declaration(black_box(source)).expect("should parse"));
        });
    }
    group.finish();
}

criterion_group!(
    benches,
    bench_tick,
    bench_combinational,
    bench_eval,
    bench_parse
);
criterion_main!(benches);

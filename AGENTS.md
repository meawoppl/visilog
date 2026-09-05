# visilog — guide for agents

A Verilog parser and simulator written in Rust. The parser is built on
[`nom`](https://docs.rs/nom/7) parser combinators; the simulator elaborates the parsed
AST — including module hierarchy — and runs it against simulated time.

This file is the repo guide for coding agents. `CLAUDE.md` is a symlink to it.

## Commands

```bash
cargo build          # build
cargo test           # run the full suite (all tests are inline unit tests)
cargo bench          # criterion benchmarks — see benches/simulation.rs
cargo fmt            # format — run before every push
cargo fmt --check    # what CI enforces
```

All tests are inline `#[cfg(test)]` modules; there is no integration-test directory. The
suite runs in well under a second — run it after every change.

**The crate is a library plus a stub binary.** `src/lib.rs` exports the modules; `src/main.rs`
is still an empty `fn main() {}`, so `cargo run` does nothing and there is no CLI yet.
Verify work through tests. The lib target is what lets `benches/` import the crate, and it
is also why `cargo build` emits only a handful of warnings — before it existed, every
public item read as dead code and the count was over 250.

**Performance is a stated goal, so measure changes.** `cargo bench` covers ticking whole
designs, expression evaluation, and parsing. `parse/*` is there as a regression guard: work
on the simulator should leave it alone.

## Layout

```
src/
  lib.rs               the library root; exports everything below
  main.rs              stub binary, currently empty
  git_utils.rs         shallow-clones + caches external repos (unused — see issue #78)
  register.rs          4-state (0/1/x/z) value type, packed into two bit planes
  parsers/             the Verilog front end — see below
  simulator/           elaboration and the event-driven run loop — see below
  verilog/examples/    sample .v files, walked by two corpus tests
benches/
  simulation.rs        criterion throughput benchmarks
```

### `src/parsers/`

Roughly bottom-up. Each file owns one slice of Verilog grammar and carries its own
`#[cfg(test)] mod tests`.

| File | Owns |
| --- | --- |
| `simple.rs` | whitespace, comments, `raw_pos_int`, `range`, and the `ws` combinator |
| `helpers.rs` | `assert_parses` / `assert_parses_to` test helpers |
| `numbers.rs` | raw binary / decimal / hex digit runs |
| `constants.rs` | sized and based literals (`8'hFF`, `'b1`) → `VerilogConstant` |
| `string.rs` | double-quoted string literals |
| `identifier.rs` | `Identifier`, identifier lists, bit/part select |
| `keywords.rs` | the `VerilogKeyword` enum and lookup |
| `operators.rs` | `UnaryOperator` / `BinaryOperator` and their token parsers |
| `expr.rs` | the expression grammar — the biggest and trickiest file |
| `delay.rs` | `#<n>` delay terms |
| `nets.rs` | `wire`/`tri`/... declarations → `Net` |
| `register.rs` | `reg` and memory declarations → `RegisterDeclaration` |
| `integer.rs` | `integer a, b;` declarations |
| `assignment.rs` | `ContinuousAssignment` (`assign x = y;`) and `ProceduralAssignment` (`x = y;`, `x <= y;`) |
| `parameter.rs` | `parameter` / `localparam` declarations → `ParameterDeclaration` |
| `behavior.rs` | `initial` / `always` blocks, sensitivity lists, `begin…end`, `if`/`else`, `case`, `$system_task(…)` calls |
| `statements.rs` | `ModuleStatement` — the union of things legal in a module body |
| `modules.rs` | `module … endmodule`, ports, and module instantiation |
| `source.rs` | `parse_verilog_source` — a whole file of modules — and `ModuleLibrary`, the name → module index |
| `base.rs` | the `RawToken` trait |

### Expression parsing

`expr.rs` implements operator precedence as a chain of nested layers, one function per
precedence level, each calling the next-tighter one:

```
unary → exp → mul_div → add_sub → shift → relational → equality
      → bitwise_and → bitwise_xor_xnor → bitwise_or
      → logical_and → logical_or → conditional
```

`verilog_expression` is the public entry point and simply calls the loosest layer
(`conditional_layer`). The layers are numbered in comments (`// Layer 1:` … `// Layer 14:`).

**To add or fix an operator, edit the layer that owns its precedence.** Adding it at the
wrong layer silently produces a wrong parse tree rather than a parse error, and the tests
that catch this are the associativity/precedence assertions at the bottom of `expr.rs`.

### `src/simulator/`

Partly built. **Combinational simulation works end to end**: `Simulator::setup` declares every
port, net, register and parameter into a `StateStore`, and `Simulator::run` settles the
module's continuous `assign` statements to a fixpoint, returning the number of passes it
took. Because the assignments are stored in source order rather than dependency order, a
single pass is not enough — `run` repeats passes until one changes nothing, and reports
`SimulationError::NoConvergence` once it hits its pass limit so a combinational loop is an
error rather than a hang.

Sequential logic runs through `Simulator::poke` (drive an input, then settle) and the
`tick` helper (one clock pulse). Settling is a delta-cycle loop: take the changes the
`StateStore` journalled since the last round, wake every `always` block sensitive to those
edges, commit their non-blocking updates, re-propagate the continuous assignments, repeat
until a round produces no edges. `counter.v` and `complex_module.v` simulate end to end.

**The `StateStore` tracks its own writes.** Every write records the value it displaced, and
`take_changes` hands that list over and starts a fresh one — so `settle` calling it is both
"what moved" and "the marker the next round measures from". `events::edges_from_changes`
turns that list into `SignalEdge`s and costs the number of signals written rather than the
number of signals in the design; `events::edges_between` is the two-snapshot equivalent and
must stay off the hot path.

**`poke`, not `set_input`, is what drives sequential logic** — an `always` block wakes on
an *edge*, so a value that is written without settling produces no edge and nothing runs.

Procedural bodies run through **one** engine, in `program.rs`. `Program::compile` flattens
a statement tree into a linear instruction list whose control flow is carried by jumps, so
a resume point is just a program counter — which is what makes a `#delay` nested inside an
`if` or `case` arm suspendable. `resume(&program, pc, &mut store, &mut tasks)` runs until the block
halts or hits a delay. The `TaskContext` is where a `$display` prints and where a `$finish`
is recorded; `resume` stops the block the moment one lands. `exec::execute_statements` is a
thin wrapper that compiles, resumes from `0`, and reports `Unsupported` if the block
suspends, because its callers have nowhere to keep the resume point yet.

`Simulator::advance` moves simulated time forward, which is what gives `#delay` meaning: a
block that hits a delay suspends and re-queues itself on the `EventQueue` for a later
timestamp. It is also what lets a design clock itself — `always begin #50 clk = ~clk; end`
needs no external stimulus, only time. `initial` blocks and free-running `always` blocks
are queued at time zero; edge-triggered blocks are woken by `settle` instead, so they are
deliberately skipped in the time wheel (`EventControl::None` reports as firing on *every*
edge, so a free-running block must not also be edge-driven).

**Module hierarchy is flattened at elaboration, in `elaborate.rs`.** `Simulator::setup`
walks the instantiation tree and inlines every child into the *same* flat `StateStore`,
assignment list and block list, so nothing about hierarchy survives into the run loop and
an instanced design costs what the hand-flattened equivalent costs. An instance's internal
signals take a dotted name — `dut.count`, `mid.leaf.count` — and a port bound to a plain
identifier is *aliased*: it and the parent's signal are one store entry, resolved
statically, so there is no propagation step between them and no value can go stale. A port
bound to a general expression (`.a(x + 1)`) cannot be aliased; an input gets its own signal
plus a continuous assignment from the parent, and an output reports `UndrivablePort`. An
unconnected input is declared `z`. `Simulator::with_modules(modules, top)` is how a design
of more than one module is handed over; `Simulator::new(module)` still takes a single
module as its own top.

**System tasks print into a buffer, not to stdout.** `$display`, `$write`, `$finish` and
`$time` are compiled to an `Instruction::Task` and carried out by
`tasks::TaskContext`, which the `Simulator` owns: `simulator.output()` hands back
everything the design printed, so "did this design print `PASSED`?" is a plain assertion —
which is exactly what a self-checking corpus test needs. `$finish` sets a flag rather than
exiting the process; `advance` and `poke` become no-ops once it is set, and `now` stops
where it stopped. Which `$name`s exist is decided at *compile* time by `TaskCall::compile`,
so an unrecognised task is an error naming it rather than a silent no-op — a design that
quietly printed nothing would look just like one that passed. `$strobe` and `$monitor` are
rejected by name for the same reason: their output is deferred to the end of a time step,
which nothing schedules yet.

Still unsupported: intra-assignment delays (`a = #5 b;` — the held right hand side does not
fit in a program counter) and concatenation as an assignment target. Parameter overrides
cannot change a width, because `simple.rs::range` only parses literal integers, so
`output [WIDTH-1:0] q` does not parse at all. `signals.rs` is built but still unwired.

| File | Role |
| --- | --- |
| `elaborate.rs` | `elaborate` — flattens a module hierarchy into one `StateStore`, one assignment list and one block list, with qualified names and aliased ports; also owns `TimedBlock` and `rename_expression` |
| `eval.rs` | `eval(&Expression, &StateStore) -> Result<Register, EvalError>` — the four-state expression evaluator |
| `events.rs` | `edges_between` / `control_fires` / `always_block_fires` / `signals_read` — edge detection and sensitivity matching |
| `exec.rs` | `execute_statements` / `commit_updates` — the run-to-completion entry point, plus `PendingUpdate` and the shared `drive` / `resolve_target` helpers |
| `program.rs` | `Program::compile` / `resume` — statement trees flattened to jump-threaded instructions, so a block can suspend on a `#delay` and resume by program counter |
| `runner.rs` | `Simulator` — `new()` / `with_modules()` / `setup()` / `set_input()` / `poke()` / `run()` / `advance()` / `get()`, the driver |
| `tasks.rs` | `TaskCall` / `TaskContext` / `Output` — system tasks, their format strings, and the buffer they print into |
| `state_store.rs` | `StateStore` — signal name → `SignalState`, backed by `register::Register`, plus the change journal `take_changes` / `clear_changes` drive |
| `event_queue.rs` | time-ordered `EventQueue` of `ExecutionCursor`s: `insert` / `pop` / `peek_time`, FIFO within one timestamp |
| `signals.rs` | `Signal` trait plus `FiniteSignal` / `InfiniteSignal` test stimulus |
| `validator.rs` | `validate_module` / `gather_definitions` |

## Measuring progress: the ivtest corpus

`tests/ivtest_corpus.rs` measures the front end and simulator against Icarus Verilog's own
regression suite. Its `regress-vlg.list` is the subset iverilog's authors describe as
"tests that should work using any simulator that supports standard Verilog (1364-2005)",
so it scores us against someone else's expectations rather than our own.

The corpus is **GPL-2.0** and this crate is MIT, so it is cloned rather than vendored:

```bash
git clone --depth 1 --filter=blob:none --sparse --branch v13_0 \
    https://github.com/steveicarus/iverilog ~/.cache/visilog/ivtest
cd ~/.cache/visilog/ivtest && git sparse-checkout set ivtest
cargo test --test ivtest_corpus -- --ignored --nocapture
```

**Clone the pinned tag.** `.github/workflows/closure.yml` pins `IVTEST_REF`, currently
`v13_0`, so the denominator does not drift under the trend line — the corpus has 1513
`normal` entries at `v12_0`, 1519 at `v13_0` and 1521 on `master`. A local clone at a
different ref will disagree with CI for no interesting reason. Bump the two together.

Every corpus test is `#[ignore]`d, so `rust.yml` never depends on that clone. A separate
`closure.yml` workflow does clone it and reports the number on each PR — deliberately a
different job, because a metric must not be able to block a merge when an external
repository is unreachable. `VISILOG_IVTEST` overrides the path.

The harness prints one machine-readable `CORPUS_METRICS …` line for CI to grep. The
human-readable table above it is free to change; that line is the contract.

**Closure — the `PASSED` count — is the headline metric, not `parsed`.** The corpus is
self-checking: a test prints `PASSED` when it is satisfied. Parsing a file therefore says
nothing about whether the simulator got the right answer, and counting parses overstates
progress by roughly a factor of two. `ivtest_corpus_closure_rate` reports the whole funnel
— parsed, elaborated, ran, then `PASSED` / wrong answer / silent.

**A wrong answer is worth more attention than a parse failure.** A file that runs and
prints `FAILED` is one the simulator understood well enough to execute and still got wrong,
which is a correctness bug rather than a missing feature. Those are printed **by name** for
exactly that reason.

Three control tests are *not* ignored and run in normal CI: a known-good design must parse,
a self-checking design must reach `PASSED`, and a deliberately wrong one must be reported as
a wrong answer. They exist so a low corpus score can never be a harness bug misreported as
a simulator limitation — the first draft of the closure metric read `0%`, and only a control
distinguishes that from a real result.

The blocker tables in `ivtest_corpus_parse_rate` are text heuristics, not parser
diagnostics. **They go stale as features land** — a row counting files that *contain* a
construct cannot move once that construct is supported. Prune a row when its feature ships;
the "sample of unexplained rejections" exists to point at whatever the heuristics no longer
explain.

## Conventions

**Parser signature.** Everything is a free function `fn(&str) -> IResult<&str, T>`.
Prefer this plain form over returning `impl FnMut` — it keeps parsers usable as function
pointers, which the `helpers.rs` assertions require.

**Whitespace.** Use `ws(inner)` from `simple.rs`, which wraps a parser in
`ws_and_comments` on both sides, so it skips comments as well as whitespace. Attach
whitespace to the *elements* of a list rather than to the separator —
`separated_list0(char(','), ws(item))` handles space on both sides of the comma, whereas
putting `multispace0` on the separator only eats one side and leaves the next element
starting with a space.

**Tests live next to the code** in an inline `#[cfg(test)] mod tests`. Use the helpers:

```rust
use crate::parsers::helpers::{assert_parses, assert_parses_to};

assert_parses_to(verilog_expression, "a + b", expected_ast);
assert_parses(parse_module_declaration, source);   // asserts no leftover input
```

Both assert the parser consumed the *entire* input, which is the failure these parsers
hit most often. A parser that returns `Ok` with unconsumed trailing input is almost
always a bug — assert on the remainder, not just `is_ok()`.

`expr.rs` also has an injection fuzz test that splices random whitespace *and comments*
into known expressions using a seeded `StdRng` (seed 42) and re-parses. Every token in the
expressions it uses is one character wide, so any insertion point is a token boundary; the
`//` filler carries its own newline, because without one it would swallow the rest of the
expression. If you touch whitespace handling in an expression layer, that test is your
tripwire.

## Gotchas

- **`cargo build` emits ~160 warnings**, nearly all `dead_code` — the parser and simulator
  types have no non-test consumer yet because `main.rs` is a stub. This is expected and
  not something to "fix" by deleting code. It does mean a genuine new warning is easy to
  miss; check the warning count or grep for your file specifically.
- **Duplicate definitions exist.** `NetType` is defined in *both* `parsers/nets.rs` and
  `parsers/modules.rs`. Check which one is in scope before assuming a change took effect.
  (The former duplicate `Register` in `state_store.rs` is gone — there is now one
  `Register`, in `src/register.rs`. The former duplicate `parse_bit_select` /
  `parse_part_select` in `identifier.rs` and `assignment.rs` are gone too — `expr.rs`'s
  `bit_select` / `part_select` are now the single definitions, used by both
  `operand_no_ws` and `assignment_lhs`.)
- **A bit select is tried before a part select.** Both start `identifier [ expression`,
  and since the index is a full expression, a conditional index (`q[a ? b : c]`) contains
  a `:` that looks just like a part-select separator. `bit_select` first means the
  conditional wins; write `q[(a ? b : c):0]` when you mean a part select with a
  conditional bound.
- **Comments are skipped by `ws`**, which is `delimited(ws_and_comments, inner,
  ws_and_comments)` — one skipper, used both by `parse_verilog_source` between modules and
  by every parser inside one. A comment is therefore legal anywhere a token boundary is.
  Two consequences: a parser that consumes whitespace with a bare `multispace0` instead of
  `ws`/`ws_and_comments` is a hole where a comment is still rejected, and `ws_and_comments`
  must keep using `multispace1` inside its `alt`, or `many0` matches empty and panics.
  Still rejected: a comment *inside* a token that is separated by a bare `multispace1` —
  `posedge/*c*/clk` and `or/*c*/rst` in a sensitivity list (`behavior.rs`).
- **Module instantiation must stay last in `parse_module_statement`'s `alt(...)`.** An
  instantiation is just an identifier followed by an argument block, so putting it earlier
  lets it shadow every keyword-led statement form.
- **`src/verilog/examples/*.v` are the corpus, and two tests walk the whole directory.**
  `test_parse_verilog_examples` in `modules.rs` asserts every file parses with nothing left
  over; `test_every_example_module_simulates` in `runner.rs` asserts every file also
  elaborates, accepts stimulus, and advances time. Dropping a new `.v` file in there is the
  cheapest way to add coverage — and the fastest way to break the suite. Both tests assert
  the file count, so adding one means updating that number deliberately.
- **`spi_controller.v` cannot leave its IDLE state, and that is the module, not a bug.**
  `cs` is an *output* driven by `assign cs = (state == IDLE) ? 1 : 0;`, while the FSM's
  IDLE arm only advances when `cs == 0` — so IDLE self-latches and there is no external way
  to drive `cs`. Don't "fix" the simulator over it.
- **`clock_divider.v`'s threshold is 50,000,000**, which no test can reach by simulation.
  The nested-`if` divider pattern it uses is covered instead by
  `test_divider_pattern_toggles_at_its_threshold`, a divide-by-4 of the same shape.
- **An `always` block's trigger is an `EventControl` enum** (`behavior.rs`), not a bare
  list: `None` for `always begin … end`, `Implicit` for `@(*)`, and `Events(Vec<Event>)`
  for an explicit sensitivity list. The three forms simulate differently, so keep them
  distinct — don't collapse `@(*)` into an empty `Events` list.
- **`git_utils.rs`'s only test is disabled** (its `#[test]` is commented out) because it
  hits the network. Don't re-enable it in CI without gating it.
- **A parser range is already `(i64, i64)`**, constant-folded at parse time, so a
  parameter cannot determine a width: `output [WIDTH-1:0] q` does not parse. A parameter
  override therefore changes a child's *behaviour*, never its widths, until the front end
  grows expression ranges. `range` also rejects whitespace inside the brackets, so
  `output [ 0:0] c;` still does not parse.
- **A declaration is a *list*, and every declaration parser returns a `Vec`.**
  `reg [4:0] a, b;`, `wire a, b, c;` and `integer i, j;` all share one width (or, for an
  `integer`, one fixed 32-bit width) across every name, so `parse_register_declaration`,
  `net_declaration` and `parse_integer_declaration` each return a `Vec` and the matching
  `ModuleStatement` variants wrap one. **A memory is not a separate production** — the
  address dimension belongs to the *name* (`register::declared_name`), which is what makes
  `reg [7:0] a, mem [0:15];` legal and what removes the "try the memory form first"
  ordering hazard that two near-identical `reg`-led parsers would otherwise create.
  An `integer` is signed, which nothing models yet (issue #96).
- **Both module header styles are normalised to `Vec<Port>` at parse time.**
  `parse_module_declaration` reads an ANSI header (`module m(input wire [3:0] a);`) or a
  Verilog-1995 one (`module m(a, h);` plus `input a; output [11:0] h;` in the body), lifts
  the body direction declarations out of `statements`, and reconciles them against the
  header names. Nothing downstream can tell the two apart, which is why `elaborate` needs
  no notion of either. Mixing them, a header name with no direction, a direction naming
  something absent from the header, and a port declared twice are all `nom::Err::Failure`.
  A `reg` naming a port is *not* a second declaration of it — an output backed by a
  register is one signal, and the `reg` stays an ordinary body statement.
- **Flattening rewrites names on the compiled `Program`, not on the statement tree.**
  `AlwaysBlock` and `ProceduralStatements` are not `Clone`, but `Instruction` owns its
  `Expression`s, so `Program::rename` is what re-points a child's body at the parent's
  store. A `TimedBlock` therefore carries its own owned `EventControl` and a precomputed
  `@(*)` read set rather than an index back into `module.statements`.
- **A `$name` is its own token, not an identifier.** `identifier` still rejects a leading
  `$`, so `$foo` is legal only where `procedural_statement` allows a system task. A format
  string is likewise a `SystemTaskArgument::String`, not an `Expression` — the expression
  grammar has no string operand — and `$time` is a `SystemTaskArgument::SystemFunction`.
- **A `#delay` is a statement *prefix*, not a field on an assignment.**
  `#5 a = 1;`, `#5 $display(…);`, `#5 begin … end`, `#5 if (…) …` and
  `#5 case (…) … endcase` all parse to `ProceduralStatements::Delayed { delay,
  statements }`, which wraps a `statement_body` — a single statement or a
  `begin`…`end` block. A bare `#5;` stays `ProceduralStatements::Delay` and is
  tried first in `procedural_statement`'s `alt`, because the prefix form's body
  would have nothing to match. `program.rs` compiles `Delayed` to an
  `Instruction::Delay` followed by the body inline, so a delay nested in an
  `if` or `case` arm suspends and resumes by program counter like any other.
  Intra-assignment delay (`a = #5 b;`) is still a field on
  `ProceduralAssignment` and still rejected at compile time.
- **`#` and its value are separate tokens.** `parse_delay` skips whitespace and
  comments between them, so `# 3;` and `#/* wait */5` parse. This is worth
  roughly +24 corpus files on its own.
- **`Register::to_decimal` accumulates into a machine integer**, so it overflows on
  anything wider than about 31 bits. `tasks.rs` formats decimals through `to_u128`
  instead; do the same rather than reaching for `to_decimal` on a real signal.
- **`nom` is pinned to 7.x.** The 8.x API differs substantially; don't upgrade casually.

## Git workflow

- **Never commit directly to `main`.** Always work on a branch.
- Branch names are prefixed with the username and use dashes:
  `meawoppl/short-description`.
- Add files individually (`git add path/to/file`); do not use `git add -A`.
- Run `cargo fmt` before pushing. CI enforces `cargo fmt --all -- --check`.
- Keep commit titles to 10 words or fewer.
- Don't leave dead code behind, and don't leave comments describing code that was removed.
- Don't add backward-compatibility shims unless they were asked for.
- Open a PR and share the PR link.

## CI

`.github/workflows/rust.yml` runs on pushes to `main` and on PRs targeting `main`:
`Swatinem/rust-cache` for the cargo cache, then `cargo fmt --all -- --check`,
`cargo build --verbose`, and `cargo test --verbose`.

The toolchain is pinned by `rust-toolchain.toml` (currently `1.96.0`), which rustup
honors for both local builds and CI. To move forward, bump `channel` there, run
`cargo fmt`, and commit any resulting reformatting in the same change.

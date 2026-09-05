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
| `preprocessor.rs` | the backtick directives — a lexical pass that runs *before* the grammar |
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

### The preprocessor

`preprocessor.rs` is **not a nom layer** — the backtick directives are not part of the
module grammar, they are text substitution over the file. `Preprocessor::preprocess`
consumes source text and produces source text; `parse_verilog_source` never sees a
backtick.

`source.rs::parse_source` is the front end's default entry point and does both halves:
preprocess, then parse. `ModuleLibrary::from_source` goes through it, so preprocessing is
**implicit, not opt-in** — a caller holding a `.v` file has no way to know whether it uses
a directive, and for a file that does not the output is byte-for-byte the input.
`parse_verilog_source` is still exported as the grammar alone, which is what every inline
parser test uses.

**Expansion produces a `SourceMap` alongside the text.** Once a macro body is spliced in,
an offset in the output does not correspond to an offset in the input, so every emission
records its origin as it is written. `map.locate(offset)` gives back a file, a line, and —
when the text came out of a macro — the macro's name, with the line naming the
*invocation* rather than a position in the body. `parse_source` uses it to turn nom's
"here is the input I stopped at" into `<file>:<line> (expanding \`MACRO)`. It was built in
from the start deliberately: it cannot be reconstructed afterwards.

Supported: `` `define `` (object-like, function-like, argument defaults, `\` line
continuations), `` `undef ``/`` `undefineall ``, `` `ifdef ``/`` `ifndef ``/`` `elsif ``/
`` `else ``/`` `endif `` including nesting, `` `timescale `` (recorded on `Preprocessed`
and on `ModuleLibrary::timescale`, not discarded — #81 needs a real one), `` `include ``
with a search path set by `Preprocessor::with_include_dir`, the `` `" ``/`` `\`" ``/`` `` ``
escapes, and the `` `__FILE__ ``/`` `__LINE__ `` builtins. `IGNORED_DIRECTIVES` skips
`` `begin_keywords ``, `` `celldefine ``, `` `default_nettype `` and the rest of the
pragma-like set together with the rest of their line.

Gotchas that are load-bearing:

- **A backslash-newline in a macro body becomes a real newline**, not nothing. The
  smallest corpus file has a `//` comment whose continuation backslash is *inside* the
  comment; joining the lines without a newline would let the comment swallow the body.
- **An undefined macro is an error naming it**, never an empty expansion. One corpus file
  (`undef.v`) reads 1364-2001 the other way and relies on the empty expansion; it is the
  only file that fails preprocessing for that reason, and the trade is deliberate.
- **Recursion is caught by name**, not by a depth counter: `expanding` holds the macros
  currently being expanded, so `` `A `` → `` `B `` → `` `A `` is `RecursiveMacro`.
- **A parameter list has to touch the macro name.** `` `define A (x) `` defines `A` as the
  text `(x)`; `` `define A(x) `` defines a macro of one argument.
- **An escaped identifier is skipped whole.** `\`~!-` is a legal Verilog identifier and the
  backtick in it is not a directive.

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

**System tasks print into a buffer, not to stdout.** `$display`, `$write` and `$finish`
are compiled to an `Instruction::Task` and carried out by
`tasks::TaskContext`, which the `Simulator` owns: `simulator.output()` hands back
everything the design printed, so "did this design print `PASSED`?" is a plain assertion —
which is exactly what a self-checking corpus test needs. `$finish` sets a flag rather than
exiting the process; `advance` and `poke` become no-ops once it is set, and `now` stops
where it stopped. Which `$name`s exist is decided at *compile* time by `TaskCall::compile`,
so an unrecognised task is an error naming it rather than a silent no-op — a design that
quietly printed nothing would look just like one that passed. `$strobe` and `$monitor` are
rejected by name for the same reason: their output is deferred to the end of a time step,
which nothing schedules yet.

**A system *function* is an expression operand, and `eval` implements it.** `$time`,
`$stime`, `$signed`, `$unsigned`, `$random`, `$bits` and `$clog2` parse anywhere an
operand is legal — `a = $random;`, `if ($time > 5)`, `assign y = $signed(a) | b;` — as
`Expression::SystemFunctionCall(name, args)`, the name carried without its `$`. That is
deliberately *not* `Expression::FunctionCall`, which names a function the design declares
and resolves down a different path. A `$name` nothing implements is
`EvalError::UnknownSystemFunction`, never a zero, and a wrong argument count is
`EvalError::SystemFunctionArity`.

`eval` is handed a `&StateStore` and nothing else, so the two system functions that are
not pure functions of their arguments reach the simulation *through the store*:
`StateStore::set_time` carries the clock `$time` reads — `Simulator::advance` moves it
with `now`, and it is the only clock, which is why `TaskContext` no longer holds one —
and `StateStore::next_random` / `seed_random` own the `$random` stream. The stream is a
`RefCell<StdRng>` seeded from a fixed constant (`DEFAULT_RANDOM_SEED`, 0), so a design
that draws random stimulus draws the *same* stimulus on every run and a self-checking
test can assert on it; `$random(seed)` restarts the stream from the seed, but does not
write the seed back the way a real simulator's `inout` argument does.

`$signed` / `$unsigned` are the identity on the bits today. Signedness is unmodelled
(issue #96), so only the width half of the cast is real — this is documented rather than
faked, which is why the corpus's `signed5`, `br_gh99r`, `br_gh199a` and `pr2138979` now
run and honestly report a wrong answer instead of silently not parsing.

Still unsupported: intra-assignment delays (`a = #5 b;` — the held right hand side does not
fit in a program counter) and concatenation as an assignment target. Parameter overrides
cannot change a width, because `simple.rs::range` only parses literal integers, so
`output [WIDTH-1:0] q` does not parse at all. `signals.rs` is built but still unwired.

| File | Role |
| --- | --- |
| `elaborate.rs` | `elaborate` — flattens a module hierarchy into one `StateStore`, one assignment list and one block list, with qualified names and aliased ports; also owns `TimedBlock` and `rename_expression` |
| `eval.rs` | `eval(&Expression, &StateStore) -> Result<Register, EvalError>` — the four-state expression evaluator, including the `$name` system functions and the `SYSTEM_FUNCTIONS` table naming them |
| `events.rs` | `edges_between` / `control_fires` / `always_block_fires` / `signals_read` — edge detection and sensitivity matching |
| `exec.rs` | `execute_statements` / `commit_updates` — the run-to-completion entry point, plus `PendingUpdate` and the shared `drive` / `resolve_target` helpers |
| `program.rs` | `Program::compile` / `resume` — statement trees flattened to jump-threaded instructions, so a block can suspend on a `#delay` and resume by program counter |
| `runner.rs` | `Simulator` — `new()` / `with_modules()` / `setup()` / `set_input()` / `poke()` / `run()` / `advance()` / `get()`, the driver |
| `tasks.rs` | `TaskCall` / `TaskContext` / `Output` — system tasks, their format strings, and the buffer they print into |
| `state_store.rs` | `StateStore` — signal name → `SignalState`, backed by `register::Register`, plus the change journal `take_changes` / `clear_changes` drive, the simulated clock `$time` reads, and the `$random` stream |
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

The harness runs the corpus through `front_end`, which is `Preprocessor` + `parse_expanded`
rather than `parse_source`, because the corpus files `` `include `` one another by paths
relative to `ivtest/` and `ivtest/ivltests/`. `judge` keeps its bare
`judge(source: &str)` signature so the three control tests exercise exactly the corpus
path; `judge_with` is the one that takes the configured preprocessor.

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
- **`(*` is ambiguous with `always @(*)`, not with a parenthesised expression.** `*` is not
  a unary operator in Verilog, so `( *foo` is not the collision — but without a guard the
  `(*` of one `@(*)` pairs with the `*)` of the *next* one and swallows everything between
  them. `simple.rs::attribute` therefore requires `(*` **not** followed by `)`, which is
  also what the LRM says: an `attribute_instance` must carry at least one `attr_spec`, so
  `(*)` is never an attribute.
- **Attribute bodies are discarded.** They are synthesis metadata with no simulation
  meaning, so `ws_and_comments` skips them exactly as it skips comments. Anything that
  later wants to *read* an attribute has to stop throwing them away first.
- **A delay may be a `min:typ:max` triple.** `#(2:10:17)` parses and `Delay` keeps all
  three values; `Delay::ticks()` returns the *typical* one and is the single place the
  selection happens, so a `+mindelays`/`+maxdelays` mode is a one-function change. Delay
  values are still literal decimals — `#tPD` and `#(a:b:c)` with identifiers do not parse.
- **System task names are decomposed, not enumerated.** `split_task_name` peels an optional
  `f` prefix (takes a descriptor) and an optional `b`/`h`/`o` suffix (the default radix), so
  `$display`, `$writeh`, `$fdisplayb` and friends all come from one table. `$finish` and
  `$time` are matched as whole words first, since `finish`'s `f` is not the prefix. A
  descriptor other than stdout is a **named error**, not a silent no-op — there is no
  `$fopen`, so no other channel can legitimately be open.
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
- **`case`, `casez` and `casex` differ only in the comparison.** One `CaseKind`
  (`behavior.rs`) rides on `CaseStatement` and on every `Instruction::JumpIfMatch`, and
  `program.rs`'s `case_matches` switches on it: `Exact` keeps `==` semantics, where an
  `x`/`z` on either side is never a match, while the wildcard forms compare for *identity*
  with the don't-care bits masked out — `Register::matches_ignoring_z` / `_xz`, which read
  the don't-care mask straight off the `unknown` bit plane. A wildcard counts on **either
  side**, so a `z` in the subject is as much a don't-care as one in the label; testing only
  the label half is the easy mistake. `casez` still tells an `x` apart from a `0`.
  The `case` tag is a prefix of both keywords, so `parse_case_keyword` tries it last.
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
- **A declaration initialiser belongs to the *name*, and `wire` and `reg` mean opposite
  things by it.** `wire a = expr;` is shorthand for a declaration *plus a continuous
  assignment*: `elaborate` pushes it onto the same list an explicit `assign` uses, so the
  net follows its operands for the whole simulation. `reg a = expr;` and `integer i =
  expr;` are a starting value applied *once* — a single store write during elaboration —
  so a later procedural write owns the register and the initialiser does not fight it.
  Getting these the same way round is the substance of the feature; a `wire` initialiser
  that behaves like a one-shot looks right in a smoke test and is wrong in a real design.
  `register::declared_name` carries the initialiser next to the memory dimension, which is
  what makes `wire x = 1, y = 2;` give the two names different drivers.
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
  `$`; the token parser is `expr.rs::system_name`, and `behavior.rs` shares it so there is
  one definition of what a `$name` looks like. A format string is a
  `SystemTaskArgument::String`, not an `Expression` — the expression grammar has no string
  operand. A *bare* `$name` argument (`$display("%0d", $time)`) is still a
  `SystemTaskArgument::SystemFunction`, but only because `bare_system_function` refuses one
  followed by `(`: `$display("%0d", $signed(a))` is an ordinary expression argument.
  `TaskCall::compile` turns the bare form into an `Expression::SystemFunctionCall` after
  checking it against `eval::SYSTEM_FUNCTIONS`, so a name nothing implements is still
  rejected at compile time and `$time` has exactly one implementation.
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
- **A loop is a jump, and `program.rs` already had the shape for it.** `for`, `while`,
  `repeat` and `forever` are `ProceduralStatements` variants alongside `If` and `Case`, and
  each compiles to the flat instruction list's own control flow: `while` is
  `top: JumpIfFalse(c, end); B; Jump(top)`, `for` is that with the initialiser in front and
  the step before the back-jump, `forever` is `top: B; Jump(top)`. A `#delay` in a body
  therefore suspends and resumes by program counter like any other. A `for` header
  assignment is an *assignment* with no `;` of its own — `parse_assignment` insists on one,
  so `behavior.rs::for_assignment` is a separate production.
- **`repeat` evaluates its count once, into a hidden store signal.** `Instruction::RepeatInit`
  writes `$repeat$<index>` and `RepeatNext` counts it down, because a body that suspends
  returns from `resume` entirely: the program counter and the `StateStore` are the only
  state a resumption has, so a loop-local counter would be lost. A Verilog identifier
  cannot start with `$`, and `Program::rename` qualifies the name like any other signal, so
  two instances of one module count separately. An `x` count runs zero iterations.
- **A zero-delay loop is bounded inside `resume`, not by the runner.** `MAX_DELTA_CYCLES`
  and `MAX_RESUMPTIONS_PER_TIME` both count *returns* from `resume`, and `forever a = 1;`
  never returns, so `program.rs::MAX_INSTRUCTIONS` is the bound that sees it and reports
  `NoConvergence`. Its test uses an empty body on purpose — one that did work per iteration
  would spend the whole budget doing it and cost the suite a second.
- **`forever`, `while` and `repeat` need a word boundary; `for` needs its `(`.** All three
  are followed by a *statement* rather than punctuation, so without
  `behavior.rs::keyword`'s trailing `peek(not(identifier_char))`, `forever_more = 1;` reads
  as `forever` plus an assignment. `for` is also a prefix of `forever`, so the longer
  keyword is tried first in `procedural_statement`'s `alt`.
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

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
| `simple.rs` | whitespace, comments, `raw_pos_int`, `Range` and the `range` parser, `signedness`, and the `ws` combinator |
| `helpers.rs` | `assert_parses` / `assert_parses_to` test helpers |
| `numbers.rs` | raw binary / decimal / hex digit runs |
| `constants.rs` | sized and based literals (`8'hFF`, `'b1`) → `VerilogConstant` |
| `string.rs` | double-quoted string literals |
| `identifier.rs` | `Identifier`, identifier lists, bit/part select |
| `keywords.rs` | the `VerilogKeyword` enum and lookup for what SystemVerilog added, and `is_reserved_word` for the IEEE 1364-2005 set |
| `operators.rs` | `UnaryOperator` / `BinaryOperator` and their token parsers |
| `expr.rs` | the expression grammar — the biggest and trickiest file |
| `delay.rs` | `#<n>` delay terms, and `parse_gate_delay` for a gate's rise/fall list |
| `nets.rs` | `wire`/`tri`/... declarations → `Net` |
| `gates.rs` | the built-in primitives — `GateKind`, `DriveStrength`, `GateInstantiation` |
| `generate.rs` | `generate … endgenerate`, `genvar` and `defparam` — the shapes, never the decisions |
| `register.rs` | `reg` and memory declarations → `RegisterDeclaration` |
| `integer.rs` | the keyword-led variable declarations: `integer`, `time`, `real` and `event` |
| `assignment.rs` | `ContinuousAssignment` (`assign x = y;`), its optional `gates.rs` drive strength, and `ProceduralAssignment` (`x = y;`, `x <= y;`) |
| `parameter.rs` | `parameter` / `localparam` declarations → `ParameterDeclaration` |
| `behavior.rs` | `initial` / `always` blocks, sensitivity lists, `begin…end`, `if`/`else`, `case`, `$system_task(…)` calls, `function … endfunction`, `task … endtask` and the task enable, and the four procedural drive statements (`assign` / `deassign` / `force` / `release`) |
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

**A memory is stored apart from the signals, and that is what tells a bit select
from a word select.** `reg [7:0] mem [0:255];` declares 256 registers, and
`elaborate` puts them in the `StateStore`'s *memory* map rather than its signal
map — a name is in one or the other, never both. `a[3]` and `m[3]` are the same
syntax and only the declaration says which is meant, so that is exactly how the
declaration reaches `eval`: the `BitSelect` arm looks the name up as a signal
first and reads a *bit*, and only a miss looks in the memory map and reads a
whole *word*. An ordinary bit select therefore costs the one hash it always
cost. `exec::resolve_target` asks the same question the other way round —
`StateStore::any_memory` answers for a design that declares no memory without
hashing anything — and produces a `ResolvedTarget::Word` instead of
`ResolvedTarget::Bits`. An unwritten word reads `x` like any undriven register,
and an address outside the declared range reads `x` and discards a write, which
is what an out-of-range *bit* select already did. `integer i [0:3];` is a memory
of 32-bit signed words by the same path.

A memory write **is** journalled, in a list of its own: `always @(bus[index[0]])`
has to wake when `index[0]` moves. The journal keeps one before/after pair per
memory *name* rather than per word, which over-approximates in the direction
`event_fires` already does — a block may wake more often than it should, never
less.

**`$readmemh` writes a memory, which is why `TaskContext::run` takes a `&mut
StateStore`.** A system task used to be an output and nothing else; loading a
memory is the one that is not, and widening the one signature was the whole
structural change. The file format is whitespace-separated words with `//` and
`/* */` comments and `@<hex>` address jumps, and the load runs from `start`
towards `finish` — which default to the memory's *declared* first and last
addresses, so `mem [7:0]` loads downwards exactly as its declaration reads.
Whether the design named a `finish` decides what a file with more words than
that means: an explicit one is an instruction to stop there (`$readmemh(f, mem,
0, 3)` against an eight word file loads four and leaves the rest alone, which is
what corpus `readmemh3` asserts), a defaulted one is a description of the memory
and a file too big for it is a named error. `$writememh` / `$writememb` are the
reverse, one word per line.

**A relative data path is resolved against the process working directory and
then against `Simulator::add_search_path`.** A `Simulator` is built from parsed
modules and never learns which *file* they came from, so it cannot resolve
"next to the design" on its own; a caller that does know says so. Finding
nothing is an error naming the file and every directory tried — never an empty
memory, which would leave the design reading `x` and look exactly like one that
simply ran.

**A width is resolved at elaboration, which is the first point at which it exists.**
A `Range` reaches `elaborate` as two expressions, and `Elaborator::resolve_range` turns
them into the two numbers the `StateStore` declares a signal with — evaluating them against
the store, which by then holds the parameters in scope *including any the parent
overrode*. That is the whole point: `vector #(.WIDTH(16)) large (…)` makes `large`'s
registers sixteen bits where `small`'s are four. A `Range::Constant` was folded where it
was written and costs nothing here.

Which is why **parameters are declared before anything else** — `walk` runs them ahead of
the ports and the other declarations, since `output [WIDTH-1:0] q` has no width until
`WIDTH` has a value, while keeping their order among themselves so a parameter may be
written in terms of the one above it. Parameters and functions are circular in general (a
parameter's value may be a call, a function's return width may be a parameter), and the
knot is cut by evaluating the parameters first, *holding back* any that would not evaluate,
compiling the functions, and retrying the ones held back. A second failure is the error,
and it is the one the first attempt would have reported.

A bound that is not a constant is `SimulationError::UnresolvedRange`, which names the bound
as written: `reg [n-1:0] q;` for an `n` nothing declares stops with ``range bound `n - 1`
is not a constant``. A width the simulator picked for itself would be wrong for the whole
run and would look exactly like nothing having gone wrong.

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
quietly printed nothing would look just like one that passed.

**The end-of-timestep slot is `Simulator::end_of_timestep`, and it lives where
`settle` already returns.** `$strobe` and `$monitor` both report *after*
everything else in a timestep has run, so they need a moment when the design has
stopped moving — which is exactly a settled `settle`. The two places a timestep
can end are therefore the two places `settle` is called from: the end of one
timestamp's work in `advance`, and the end of a `poke`, which settles the design
at the time it is already at. A design that uses neither task pays
`TaskContext::has_deferred` — a load and a branch — per timestep, which is why
the hook is a *question asked of the context* rather than a call into it.

A `$strobe` is queued as its compiled `TaskCall` and rendered by `flush`, not
when it ran, so it reports what the rest of the step went on to do. **Arming a
`$monitor` does not print either**: it reports at the end of a timestep, and
that includes the one it was armed in, so `a = 1; $monitor("%b", a);` reports
the 1 rather than whatever `a` held before the block ran. `Monitor::snapshot` is
therefore an `Option` — `None` is "has not printed yet", which is what owes that
first line to the next flush — and after a line is printed the flush
re-evaluates the arguments and prints only when one has moved, so a step that
changes nothing it reads produces no line. Printing at arm time instead shifts
every line of a monitored design by one and starts it with a row of `x`s
(corpus `shift1`, `pr632`).

The **clock is not one of the values watched**: `TaskContext::snapshot` skips a
`$time` / `$stime` / `$realtime` argument, because a `$monitor` watches the
variables it prints and time moves every timestep — one that reported the clock
would never stop (corpus `br_ml20150315`). Only one monitor is ever armed — a
second `$monitor` replaces the first — and `$monitoroff` / `$monitoron` toggle
it, with `$monitoron` reporting immediately and re-basing the snapshot the way
the LRM asks. A `$monitoroff` in the *same* timestep as a change suppresses that
timestep's line, where iverilog still prints it (corpus `monitor4`, which is a
`vvp` test rather than a scored one).

**Formatting is per digit, and the case of an unknown one says whether it
mixes.** `Radix::render` goes through `tasks::digits`, which renders four bits
at a time for `%h` and three for `%o`, so `12'b0000_0000_00xx` is `00X` rather
than a single `x`: a digit whose bits *agree* — all `x`, or all `z` — prints in
lower case, and one that mixes an unknown bit with a known one, or an `x` with a
`z`, prints in upper case, because the lower case letter would claim the whole
digit was unknown. An `x` outranks a `z`. `%b` never shows a capital, since a
binary digit is one bit and cannot mix, and `%d` takes the same rule over the
*whole* value — `4'bzzxx` is `X` and `4'b00zz` is `Z` (corpus `disp_dec`). A
short top digit is judged on the bits it has rather than on a padded nibble, so
`5'bxxxxx` is `xx`.

**A `%` field is C's, and its leading zero is a fill rather than a width.**
`%08d` pads with zeros where `%8d` pads with spaces, and the zeros go after a
minus sign (`-0000010`). What is left after that zero is the width, so `%0d` and
`%0h` ask for width *zero*, which means the narrowest rendering the value
allows: in a base that pads with digits that is the value with its leading zeros
dropped, one digit kept when they are all zero, and the trim stopping at an
unknown digit since dropping one would move the value's bits (corpus
`disp_leading_z`, `disp_parm`, `test_width`, `test_extended`).

**A default `%d` field leaves room for a sign, and a default `%s` field is the
vector's own bytes.** `decimal_width` sizes an unsigned value by `2**bits - 1`
and a signed one by its most *negative* value, `-2**(bits-1)`, so an `integer`
prints in eleven columns rather than ten (corpus `pr1746848`, `test_dispwided`,
`pr1002a`). `%s` pads to `bits / 8` characters, so a thirty-two bit register
holding `"A"` is `"   A"`, and `%0s` is that text unpadded; `ascii` drops
*leading* NULs but renders an interior one as a space, since it is a character
the vector really has.

**`$timeformat` sets how `%t` renders, but nothing rescales it.** `precision`
fractional digits, then the suffix, right-aligned in `min_width` (twenty by
default), with an explicit `%12t` overriding `min_width` and `%0t` meaning no
padding at all. The `units` argument is range-checked and then taken to name the
unit a tick already *is*: the clock counts ticks and nothing hands `Simulator`
the `` `timescale `` the preprocessor recorded, so there is no second unit to
convert between. That is the identity for the `` `timescale 1ns `` plus
`$timeformat(-9, …)` pairing that covers nearly every design using either, and
wrong by a power of ten when they disagree — corpus `timeform1` is the case.

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

**Signedness is modelled.** `reg signed [3:0] a;`,
`wire signed`, `input signed`, an `integer`, `4'sd12` and a bare decimal like `42` are all
signed; everything else is unsigned. It rides on the `Register` a lookup produces — a
register is bits *plus how to read them* — and `$signed` / `$unsigned` are real casts that
set that bit and change nothing else. It changes the answer in exactly five places: `/`,
`%`, `>>>`, the relational operators, and the widening that happens when two operands of
different widths meet or when a value is written into a wider target.

**Widths are context-determined, and the target is where the context comes from.**
Verilog sizes most expressions by the thing being assigned to: `reg [15:0] w; reg [7:0] a,
b; w = a * b;` widens `a` and `b` to sixteen bits *before* multiplying, so the whole
product survives. Sizing each operand by itself gives an eight bit product and then pads a
plausible wrong number, which is why these were silent failures rather than loud ones.

The target's width reaches `eval` through `eval_sized(expr, store, width)`. Both callers
resolve the left hand side *first* and ask it how wide it is —
`ResolvedTarget::width(&store)` — so `program.rs`'s `Blocking` / `NonBlocking` and
`runner.rs`'s `propagate` push the same number down the same path. Everything else
(`case` subjects, conditions, task arguments, function arguments) still goes through
`eval`, which is self-determined.

Inside `eval_in_context` the width is a **lower bound**, not an exact size: an operand is
padded out to it and otherwise left alone. That is exactly Verilog's "the larger of the
self-determined width and the context", and it makes `SELF_DETERMINED` — a bound of zero —
the same code path rather than a second one. Which operands the bound reaches is
`OperandRule`, the same table signedness uses: it reaches `+ - * / % & | ^ ~^`, unary
`+ - ~`, both arms of a `?:`, and the *left* operand of a shift or a `**`; it does not
reach a shift's right operand, a `?:` condition, or anything inside a concatenation —
`c = { a**b };` is the four bit power, `c = a**b;` is the sixteen bit one (corpus
`pr2823711`).

**A comparison is the exception that needs measuring.** Its answer is one unsigned bit
whatever the context, but its two operands are context-determined *with respect to each
other*: sized to the wider of the two and read signed only when both are. `assign wide =
a/b;` checked against `a/b` therefore divides in sixteen bits on both sides — without that
rule a division by zero is sixteen `x`s on one side and eight padded with zeros on the
other (corpus `pr2722339a`). This is the one place `expression_width` — the self-determined
width walk — is called, and `sized_within` keeps it off the hot path: only an operand that
carries out an operation at its own width can tell being widened before from being widened
after, so `state == 3'b010` measures nothing.

The same mutual context is why an unsigned operand demotes a signed one *before* the
widening, and that reaches all the way down: `(a >>> 1) === 4'b1111` is false for a signed
`a`, because the unsigned literal makes the whole comparison unsigned and an unsigned
`>>>` is a plain `>>`. Compare against `4'sb1111` when the arithmetic shift is the point.

`eval` still tops out at `MAX_ARITHMETIC_WIDTH` (128 bits), and now that a target's width
actually reaches the operator that limit is *reported* rather than quietly ignored:
`reg signed [128:0] res; res = in1 ** in2;` is `EvalError::WidthOverflow(129)` where it
used to compute a wrong answer in 32 bits (corpus `pr2352834`).

Still not modelled: a `?:` whose arms disagree about signedness takes the "signed only if
both" rule, where iverilog reads `1 ? ~a >>> 5 : 0` as signed (corpus `br_gh37`,
`pr1913937`). The widths in that expression are right; only the sign is not.

**A design's own functions are compiled at elaboration and called from `eval`.**
`function [7:0] f; input [7:0] a; f = a + 1; endfunction` parses in both the 1995 form
(arguments as `input` declarations inside the body) and the 2001 one (`f(input [7:0] a)`),
and `elaborate` turns each one into a `program::FunctionDefinition`: the compiled body,
plus the frame variables it needs — its result (the function's own name), its arguments,
its locals — and the design signals it reads. The definitions live on the `StateStore`,
because `eval` is handed a `&StateStore` and nothing else, and that is where a call has to
find its body.

**A call runs against a frame, which is a `StateStore` of its own.**
`FunctionDefinition::call` builds one holding the result variable, the arguments, the
locals, and *copies* of the design signals the body reads, then runs the body through the
same `resume` every other procedural body goes through. Nothing the body writes reaches
the design — which is exactly why a call can be made from an evaluator holding a shared
reference — and every call gets its own frame, so **recursion works**: `fact(n) = n *
fact(n - 1)` returns 120 for 5. `MAX_CALL_DEPTH` (64) is what makes a function that never
reaches its base case `EvalError::FunctionCallDepth` rather than a stack overflow; the
bound is deliberately well under the ~120 an unoptimised build actually survives.

The read set is why a frame costs the *function* rather than the design, and it is closed
over the call graph (`close_reads`): a function that calls another has to copy in what
that one reads too, or the inner call's frame would be missing it. An `@(*)` block's
implicit sensitivity list is extended the same way — a block whose only reader of a signal
is a call still has to wake when that signal moves.

Because a frame is thrown away, a function body that would need to be seen from outside is
a **named error at elaboration**, never a silent no-op: a `#delay`, a system task (its
output would go into a `TaskContext` nobody reads), a non-blocking assignment (its write
lands after the call has ended), an assignment to a signal outside the function, and
`$random` (the stream it would advance is the frame's). Those five are the whole list, and
`$display` inside a function is the one worth revisiting — it needs an output sink the
evaluator can reach.

**A signal can have more than one source, and `StateStore` says which one wins.**
`assign v = e;` and `force v = e;` written *inside* a procedural block install a continuous
drive that outlives the statement, `deassign` and `release` take it away again, and the
rule is `force` beats procedural `assign` beats an ordinary write. That is `DriveLevel`,
and `StateStore::permits_write` is where it is enforced — asked by `exec::drive_at`, which
every assignment in the simulator goes through. A write that loses is **discarded**, not
applied and overwritten a moment later: that is what keeps it out of the change journal,
and so out of the edges that wake blocks. A design that forces nothing pays a
`Vec::is_empty` for the question, the same shape `any_signed` and `any_memory` use.

The drives live on the store because a running procedural block is handed nothing else,
and they are re-evaluated by `Simulator::propagate` alongside the module's own `assign`
statements — one fixpoint, not two, which is what makes a forced signal *follow* its
expression when an operand moves. `propagate` holds them through `StateStore::drives`, an
`Rc` handle, so the list is still in the store while it is being written through: an
`assign` underneath a `force` has to be able to see the force to know its own write goes
nowhere.

`release` puts back what the `force` displaced — still the signal's last *procedural*
value, since writes made while it was forced never landed — unless a procedural `assign`
is still installed, in which case that one takes over. iverilog reads the LRM the other
way for a variable and leaves the forced value in place (corpus `pr1477190`).

**A task is inlined where it is enabled, which is what makes a `#delay` inside one
work.** `task load; input [7:0] a; output [7:0] b; b = a + 1; endtask` parses in the same
two forms a `function` does, and `my_task(x, y);` — or a bare `my_task;` — is a
`ProceduralStatements::TaskEnable`, a *statement*: a task returns nothing, so its results
come back through its `output` and `inout` arguments rather than through a value. It is
deliberately not the shape a function takes. A function is compiled once and *called*
against a frame; a task may consume time, and the only state a suspension keeps is a
program counter and the `StateStore`, so a call that could suspend has nowhere to leave
itself. `Program::splice` therefore copies the compiled body into the caller's own
instruction list, with every jump target and every `$repeat$` counter shifted by where it
landed — after which a delay inside a task is the delay machinery that was already there,
and nothing in `resume` had to learn about tasks at all.

An enable is the copy-in, the body, and the copy-out: an `input` or `inout` argument is
written before the body runs, an `output` or `inout` one is written back to the caller's
variable *after* it, which is where the LRM and iverilog both put it (`tk(a, b, a)` with
`inout` `a` leaves the caller's `a` holding what the task left in it). Both halves are
ordinary `Instruction::Blocking`s, so a copy is sized and signed by its target the way
every other assignment is, and an argument the caller passes as a constant fails as an
`UnsupportedTarget` only if the task tries to write it back.

**A task's variables are static, and are ordinary store entries under a dotted name.**
`elaborate::declare_tasks` puts the arguments and locals of task `load` in the store as
`load.a`, `load.b` — qualified per instance like anything else, so two instances of a
module count separately, and shared between two enables of one task exactly as the LRM
says a non-`automatic` task's storage is. That is also what tells a task local apart from
a design signal of the same name: `compile_task` renames the body through the task's own
names *only*, and `Program::rename_local` skips the ranges already spliced in from a
nested enable — renaming a body twice would re-point a signal the inner task read at a
variable the outer one happens to spell the same way.

Those declarations are the *only* place a task's widths are recorded, and they go through
`resolve_range` like every other declaration, so `input [WIDTH-1:0] a;` is sized from the
parameters in scope and a parent's override reaches it. A `TaskParameter` therefore keeps
the argument's name and direction and nothing else: an assignment to it is sized and
signed by the store entry, and a second copy of the width here could only disagree with
the first.

Three things about an enable are named errors rather than silent no-ops: a task the module
does not declare (`UnknownTask`), the wrong number of arguments (`TaskArity`), and a task
that enables itself directly or around a cycle (`RecursiveTask`) — inlining does not
terminate on one, and a static task's storage means real Verilog cannot recurse either.
`declare_tasks` compiles in dependency order by repeating until a pass compiles nothing
new, so a task may enable one declared further down the file.

Still unsupported: `disable` (both `disable <task>` and the named-block form — cancelling a
block that has already suspended is not something a program counter alone can express);
a hierarchical enable (`instance.task(…)`); a task enabled from inside a `function`, which
is rejected by the function body analysis rather than by a check of its own; and
intra-assignment delays (`a = #5 b;` — the held right hand side does not fit in a program
counter) and concatenation as an assignment target. A drive is also tracked per signal *name* rather than per bit, so
`force bus[0] = 1;` blocks a write to `bus[1]` as well — corpus `pr1832097a`, `pr245`,
`pr527` and the `_pv` pair are that one gap. `signals.rs` is built but still unwired.

**A gate primitive is a continuous driver, and it is what made net resolution
necessary.** `and g1 (out, a, b);` elaborates to a `simulator::gates::Gate` on the same
`propagate` fixpoint the `assign` statements settle in — not to a procedural block — so its
output follows its inputs for the whole simulation and its writes journal edges like any
other. The keyword is what tells one from a module instantiation, so
`parse_gate_instantiation` sits **immediately before** `parse_module_instantiation_statement`
in `parse_module_statement`'s `alt`: an instantiation is an identifier followed by an
argument block and would otherwise read `and` as a module name.

An ordinary `assign` writes its net and that is the end of it, which is why the simulator
never had to ask what two drivers of one net mean. A three-state bus forces the question —
`bufif1 (bus, a, ena);` beside `bufif1 (bus, b, enb);` are two drivers, each `z` when it is
not enabled, and *whichever wrote last* is exactly the wrong answer. So `elaborate` records
`resolved_nets`, the names a gate drives, and `propagate` routes every continuous driver of
one of those — gates *and* `assign` statements alike — into `resolve_contributions` instead
of writing it. Each **bit** is settled on its own by `gates::resolve_bit`: a driver that is
`z` contributes nothing, the strongest of the rest wins outright, and drivers tied at the
strongest level either agree or the bit is `x`. A bit no driver reaches keeps what it held,
so a driver of `bus[0]` says nothing about `bus[1]`. A design with no gates in it pays a
`HashSet::is_empty` for the question, the same shape `any_signed` and `any_memory` use.

Strength is what makes a `pullup` mean anything: it drives at `pull` where a gate drives at
`strong`, so it holds a net every buffer has let go of and loses the moment one drives.
`(strong0, pull1)` parses and reaches the same rule, and because the two halves are
independent, `(highz0, strong1)` is a real open drain — it drives its `1` and floats instead
of driving its `0`, leaving a `pulldown` in charge.

**`StrengthLevel` and `DriveStrength` are net-driver concepts, not gate ones**, even though
`parsers/gates.rs` is where the grammar for them lives and `gates::drive_strength` is the
single parser for the token. A gate is simply the first driver form that could declare one.
`assign (pull1, pull0) x = y;` is the same token in front of a continuous assignment, and
it is wired up through two seams and no machinery of its own: the strength rides on
`ContinuousAssignment::strength()`, and `propagate`'s assignment arm hands it to
`Contribution` in place of the `DriveStrength::STRONG` an assignment used to be assumed to
have. Everything past that point — `Contribution`, `resolve_contributions`, `resolve_bit` —
already took any driver at any strength.

**A strength-bearing `assign` is a resolved net even in a design with no gate in it**,
which is what `elaborate::push_assignment` adds to `resolved_nets` beside what
`push_gate` does. `highz` is a half that does not drive at all, so
`assign (strong1, highz0) x = 4'b1010;` is `1z1z` — and only `resolve_bit` knows that,
where writing the net directly would put the `0`s straight through. `None` on a
`ContinuousAssignment` is an `assign` that named no strength, which stays an ordinary
write at `strong` exactly as it always was, so a design that declares no strength anywhere
still pays the `HashSet::is_empty`.

**The truth tables were measured against iverilog 12.0, not read off the LRM.** `and(0, x)`
is `0` rather than `x` — an unknown input that cannot change the answer does not make the
answer unknown — and `or(1, z)` is `1`. A `z` reaching a *logic* gate is read as an `x`,
which is the one place the switch family differs: `nmos` conducting a `z` passes a `z`,
where `bufif1` enabled on a `z` gives an `x`. A three-state buffer whose control is unknown
drives `x`, where the LRM allows the weaker `L`/`H`. `cmos` is deliberately **not** two
resolved switches — `cmos(0, 1, x)` is `0`, where resolving a strong `0` against the `x` a
half-open `pmos` reports would give `x` — so it asks whether *either* half conducts instead.

An **array of instances** (`bufif1 drv [7:0] (bus, data, enable);`) is expanded at
elaboration into one gate per index, because nothing downstream has a notion of an instance
at all. Its bounds are a `simple::Range` like a declaration's, so they go through
`resolve_range` and `buf drv [N-1:0] (…)` is sized by the parameters in scope. A terminal
one bit wide is shared by every instance and a terminal exactly as wide
as the array is sliced a bit apiece; the two are told apart by `expression_width`, and a
terminal that is neither — or one that is wide but not a plain signal, like
`{16'b0, data}` — is `SimulationError::GateArrayTerminal` rather than a silent
misconnection. Both the array and the terminal are walked from their least significant end,
so which way round either range was declared cannot matter.

Not modelled, each by name where it can be: **a gate delay is parsed and ignored** — the
gate settles in zero time along with every other continuous driver, and `#(rise, fall)`
keeps only the first value. The **bidirectional** switches (`tran`, `tranif0`, `rtranif1`, …)
conduct both ways and have no output terminal, so `Gate::new` refuses them by name. The
`r`-prefixed switches pass the same values as their non-resistive counterparts because the
strength *reduction* is not modelled, and neither is strength *propagation* through a switch
at all: corpus `resolv1` needs a `pmos` to carry a `pullup`'s `pull` strength through to its
output, which would mean the store carrying a strength per bit beside its value.

**A `generate` region is unrolled at elaboration, which is the same thing
flattening an instance is, one level down.** `parsers/generate.rs` captures the
*shape* — the loop, the branch, the case, the labels — and decides nothing, because a
loop bound may be a parameter and a parameter has no value until elaboration.
`Elaborator::expand_generate` then turns a region into a list of ordinary
`ModuleStatement`s, each paired with the [`Scope`] its block gave it, and those go
through exactly the two passes — declare, then build — that the module's own statements
go through. Nothing about a generate block survives into the run loop, so an unrolled
design costs what the hand-written equivalent costs.

**Where it sits in `walk` is deliberate**: after the parameters, the tasks and the
functions, because a loop bound, an `if` condition and a `case` subject are made of
those; and *before* the declaration passes, because what a region unrolls **to** is
declarations. A `parameter` written inside a block is the one thing evaluated as the
block unrolls rather than in the pass that follows — a nested loop's bound may be made of
it. A `function` or a `task` inside a block is a named error rather than a silent
omission: `walk` compiled the module's subprograms before it got here, so one written
inside a block would simply be missing from the store a call looks in.

**A generate block is a *nested* scope, and that is the whole difference between it and
an instance.** A module cannot see out of itself, so everything a module names is its
own; a generate block can, so only what it *declares* is its own and everything else
belongs to the module around it. That is `Scope::locals` — the names the blocks in scope
declare, mapped to the store entries they took — and it is what `resolve` asks first. It
is keyed by the **head** segment of a name, so a reference that reaches into a nested
block (`inner[0].sig`) is qualified by the block that declares `inner`. The list is
`declared_names`: the signals and parameters a block declares, the *instances* it
creates, and the *labels* of the blocks nested in it, because a hierarchical reference
reaches through all three.

**A named block's label is the scope, and a loop indexes it**: `stage[0].u.count` is how
a testbench reaches inside, and it is literally the store key. An unnamed block still
gets a scope — `genblk1` — because two iterations of an unnamed loop body would otherwise
declare the same names twice.

**A genvar is an elaboration-time integer and never reaches the `StateStore`.** It is
*substituted*, not renamed: `substitute_genvars` replaces the identifier node with a
constant one, which is why it cannot go through `rename_expression` and why `program.rs`
grew `Program::substitute` beside `Program::rename`. Substitution happens **before**
qualification in `renamed`, since a genvar resolves to no signal at all. That is also
what makes a range bound written inside a loop work — `reg [i:0] r;` reaches
`resolve_range` with `i` already a number — and what tells `.a(i)` (a constant) from
`.a(x)` (a signal to alias) at a port connection. A runaway loop is
`GenerateLoopBound` after `MAX_GENERATE_ITERATIONS`: every iteration is a real copy of
the body, so it is an allocation nothing survives rather than a hang.

**`defparam` is collected before the build pass and applied where the instance is
made.** The path it names — `dut.WIDTH`, `mid.leaf.WIDTH`, `stage[0].u.WIDTH` — is
already the flat spelling the parameter ends up under, so the two meet with no
translation; `Elaborator::defparams` holds them and `instantiate` *removes* the ones that
name the instance it is creating, which is what makes an override beat a `#(...)` on the
same instantiation the way the LRM asks. A `defparam` still in the map when `elaborate`
returns named nothing, and that is `SimulationError::UnappliedDefparam` — an override
that quietly did not happen leaves the design running on the value it was told not to
use.

**A hierarchical name is one identifier, folded at parse time.**
`identifier::hierarchical_identifier` reads `dut.count` and `stage[0].u.count` into a
single `Identifier` whose name is the whole dotted path, which is exactly the store key
flattening produced — so nothing downstream had to learn about hierarchy. An index
belongs to the *path* only when a `.` follows it, which is what tells `a[3]` (a bit
select) from `a[3].b` (a name inside the fourth iteration of generate block `a`); nothing
in the production skips whitespace, so an ordinary name pays one character comparison to
find out it is not a hierarchical one. An **absolute** name starts at the top module by
name — `main.dut.count` — and the top module is the root of the flat name space and
carries no prefix, so `Scope::resolve` drops that leading segment. A scope of its own
shadows it, which is why the `locals` lookup is asked first.

**An output bound to a select is the alias run backwards.** `.y(bus[i])` — how a generate
loop wires an instance per bit — cannot be aliased, because the port and `bus[i]` are not
one store entry. The port keeps a signal of its own and a continuous assignment carries
it *out* to the bit, which is `Binding::Driving` against `Binding::Driven`'s inward
direction. An output bound to something that cannot be written at all (a concatenation,
`a + 1`) is still `UndrivablePort`, and so is an `inout` bound to a select: it is read as
well as written, and one assignment only runs one way.

Still not modelled: a generate loop bound that reads a *signal* evaluates it rather than
refusing — an `x` runs zero iterations where iverilog reports a non-constant bound, and
`eval` cannot tell a parameter from a net through the store to say otherwise. `generate`
items written outside `generate`/`endgenerate`, an `inout` bound to a select, and a
`defparam` whose path indexes something other than a generate block are all unsupported.

**`time` is a variable, `event` is not, and `real` is a named refusal.** `time t;` is a 64
bit *unsigned* register and nothing else — `elaborate` declares it at a fixed width the way
it declares an `integer` at 32 — so it round-trips through the store, through the memory
map (`time marks [0:3];`) and through `$display` with no other machinery. A named event has
no value at all: `StateStore` keeps a third namespace for the names, and the whole of an
event's state is the *trigger journal*, the list of events fired since the last marker.
`settle` takes it beside the change and memory journals, and `events::trigger_edges` turns
each entry into a synthesised one-bit `0 -> 1` edge under the event's own name. That is
what makes `always @(e)` fire **exactly once** per `-> e;` — the round that takes the
trigger is the only round that can see it, where a value left standing in the store would
wake the block again on every delta cycle. Reading an event is `EvalError::EventAsValue`,
the same shape as `MemoryAsValue`: the name exists, it simply is not a value. A `real`
**parses and then stops** — `elaborate` reports an `Unsupported` naming it — because
IEEE-754 floating point is not what a `Register` holds (there is no `x` in a float) and the
expression grammar has no floating point literal to feed one with anyway. The declaration
is read regardless so that a design using one says that is why it stopped, rather than
dying on unfamiliar syntax several lines earlier.
| File | Role |
| --- | --- |
| `elaborate.rs` | `elaborate` — flattens a module hierarchy into one `StateStore`, one assignment list and one block list, with qualified names and aliased ports; also owns `TimedBlock`, `rename_expression`, `resolve_range` (a declared width against the parameters in scope), the unrolling of a `generate` region and the application of a `defparam`, and the compiling of a `function` into a `FunctionDefinition` and of a `task` into a `TaskDefinition` |
| `eval.rs` | `eval(&Expression, &StateStore) -> Result<Register, EvalError>` — the four-state expression evaluator, plus `eval_sized` for an assignment's right hand side; signedness *and* width (`expression_is_signed` / `expression_width` / `operand_rule` / `widened`), the `$name` system functions and the `SYSTEM_FUNCTIONS` table naming them, and `call_function` for the design's own |
| `events.rs` | `edges_between` / `edges_from_changes` / `memory_edges` / `trigger_edges` / `control_fires` / `always_block_fires` / `signals_read` — edge detection and sensitivity matching |
| `gates.rs` | `Gate` — one elaborated primitive, its terminals split into outputs and inputs; `gate_output`, the four-state truth tables; and `resolve_bit`, the strength-ordered net resolution |
| `exec.rs` | `execute_statements` / `commit_updates` — the run-to-completion entry point, plus `PendingUpdate` and the shared `drive` / `resolve_target` helpers; also `drive_at`, where drive precedence is enforced, and `install_drive` / `apply_drive` / `release_drive` / `deassign_drive` |
| `program.rs` | `Program::compile` / `resume` — statement trees flattened to jump-threaded instructions, so a block can suspend on a `#delay` and resume by program counter; also `FunctionDefinition::call`, which runs one of those programs against a frame, and `TaskDefinition` / `Program::splice`, which inlines one into another |
| `runner.rs` | `Simulator` — `new()` / `with_modules()` / `setup()` / `set_input()` / `poke()` / `run()` / `advance()` / `get()` / `add_search_path()`, the driver, plus `end_of_timestep()`, the slot the deferred tasks report in |
| `tasks.rs` | `TaskCall` / `TaskContext` / `Output` / `TimeFormat` — system tasks, their format strings, the buffer they print into, the deferred `$strobe` queue and the one armed `$monitor`, and the `$readmemh` / `$writememh` memory file format |
| `state_store.rs` | `StateStore` — signal name → `SignalState` (value, declared range and declared signedness), backed by `register::Register`; memory name → `Memory`, in a second map, which is the whole bit-versus-word disambiguation; event name in a third, valueless namespace with the trigger journal `trigger_event` / `take_triggers`; plus the change journal `take_changes` / `clear_changes` drive, the memory journal `take_memory_changes`, the simulated clock `$time` reads, the `$random` stream, the design's `FunctionDefinition`s, the `frame()` a call runs in, and the installed `Drive`s with the `DriveLevel` precedence rule `permits_write` answers |
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
human-readable table above it is free to change; that line is the contract. Its keys are
`total parsed elaborated ran passed wrong silent gold gold_match gold_mismatch gold_silent
closure`. **Only ever add a key** — CI greps for them by name, so renaming or dropping one
breaks the trend line.

**Closure — `PASSED` plus gold match — is the headline metric, not `parsed`.** Parsing a
file says nothing about whether the simulator got the right answer, and counting parses
overstates progress by roughly a factor of two. `ivtest_corpus_closure_rate` reports the
whole funnel — parsed, elaborated, ran, then closure / wrong answer / gold mismatch /
silent. The `closure=` key on the `CORPUS_METRICS` line is the number.

**The corpus validates a test one of two ways, and the list says which.** Most entries are
self-checking and print `PASSED` when satisfied. The other 357 carry a fourth or fifth
field `gold=<file>`, and are validated by comparing their output to
`<corpus>/ivtest/gold/<file>` — they never print `PASSED` at all, so scoring them by that
word alone undercounted closure by 45. `Entry` therefore keeps the gold filename, and the
two populations get outcomes of their own (`GoldMatch` / `GoldMismatch`) so they stay
distinguishable in the report. `gold=` is scanned for across every field past the
directory, not read from a fixed position, because the optional top-module name comes
first when an entry has one (`shellho1 normal ivltests top gold=shellho1.gold`).

Three rules make a gold comparison mean something (`gold_lines` / `first_difference`):

- **Trailing whitespace is trimmed per line, leading whitespace is not.** Column alignment
  is exactly what a lot of these `$display` tests check.
- **`VCD info: dumpfile … opened for output.` is dropped from both sides.** Seven gold
  files carry it; visilog has no waveform dumper, so keeping it would fail those on an
  unimplemented side effect rather than on the output the test is about.
- **A trailing newline is not a difference**, which comes free from `str::lines`.

**A design that printed nothing is `Silent`, never a gold match** — not even against an
empty gold file, since "produced exactly the right emptiness" and "never reached its own
checks" are indistinguishable from the harness's side. No `normal` entry names an empty
gold file today, so in practice the rule only reroutes designs that printed nothing against
a gold file that expected something; `gold_silent=` counts them so the choice is visible
rather than an invisible subtraction from closure.

**A wrong answer is worth more attention than a parse failure.** A file that runs and
prints `FAILED`, or that runs and produces output differing from its gold file, is one the
simulator understood well enough to execute and still got wrong — a correctness bug rather
than a missing feature. Both lists are printed **by name** for exactly that reason, and the
gold mismatches carry a first-difference line (`line N: expected … got …`) for the leading
few, which is what makes them actionable without reading the corpus by hand.

Not every gold mismatch is a simulator bug: seven of them (`br1007`, `br_gh127a`…`f`)
have gold files whose first lines are iverilog's own *compiler warnings*
(`./ivltests/br1007.v:15: warning: …`), which visilog has no diagnostic channel to emit.
They are left in the list rather than filtered out, because a rule that dropped anything
looking like a diagnostic would also drop real output — but read the first-difference line
before treating one as a correctness bug.

Four control tests are *not* ignored and run in normal CI: a known-good design must parse,
a self-checking design must reach `PASSED`, a deliberately wrong one must be reported as a
wrong answer, and a fourth drives both halves of the gold comparator — matching output
scores `GoldMatch`, differing output scores `GoldMismatch` and names the line. They exist
so a low corpus score can never be a harness bug misreported as a simulator limitation —
the first draft of the closure metric read `0%`, and only a control distinguishes that from
a real result. A comparator stuck on "match" would invent 357 passes; one stuck on
"mismatch" would look exactly like 357 genuine wrong answers.

The harness runs the corpus through `front_end`, which is `Preprocessor` + `parse_expanded`
rather than `parse_source`, because the corpus files `` `include `` one another by paths
relative to `ivtest/` and `ivtest/ivltests/`. `judge` keeps its bare
`judge(source: &str)` signature so the control tests exercise exactly the corpus
path; `judge_with` is the one that takes the configured preprocessor and the gold text.

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
  A **gate** writes up to three delays rather than one — `#(rise, fall, turn_off)` — which
  is what `parse_gate_delay` is for; it keeps the first and drops the rest, since nothing
  downstream schedules a gate delay at all.
- **System task names are decomposed, not enumerated.** `split_task_name` peels an optional
  `f` prefix (takes a descriptor) and an optional `b`/`h`/`o` suffix (the default radix), so
  `$display`, `$writeh`, `$fdisplayb`, `$strobeh`, `$fmonitor` and `$readmemb` all come from
  one table. The whole words are matched *first*, and there are five: `$finish` and
  `$timeformat` because `finish`'s `f` is not the prefix, `$monitoroff` because its trailing
  `f` is not one either, and `$time` and `$monitoron` alongside them. `$readmem` and
  `$readmemo` are consequently names nothing implements — the radix suffix is the file
  format rather than a default, so only `b` and `h` spell a task. A descriptor other than
  stdout is a **named error**, not a silent no-op — there is no `$fopen`, so no other
  channel can legitimately be open.
- **Module instantiation must stay last in `parse_module_statement`'s `alt(...)`.** An
  instantiation is just an identifier followed by an argument block, so putting it earlier
  lets it shadow every keyword-led statement form. A gate primitive is one of those
  keyword-led forms and looks *exactly* like an instantiation once the keyword is past, so
  `parse_gate_instantiation` sits directly before it and nothing may be put between them.
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
- **A declared range holds *expressions*, and a literal one is folded where it is
  written.** `simple.rs::Range` is `Constant(i64, i64)` when both bounds were literals and
  `Expressions(..)` when either was not, so `reg [WIDTH-1:0] q;` and `output [0:count-1] y`
  parse and `[7:0]` still costs nothing. The literal parser is tried first and cannot
  mis-fire on an expression: the whole range is bracket delimited, so `[7-1:0]` fails it at
  the `-` with nothing consumed and falls through. Every position *inside* the brackets is
  a token boundary, so `[ 7:0]`, `[7 : 0]`, `[7:0 ]` and `[0: count-1]` all parse.
- **A declaration is a *list*, and every declaration parser returns a `Vec`.**
  `reg [4:0] a, b;`, `wire a, b, c;` and `integer i, j;` all share one width (or, for an
  `integer`, one fixed 32-bit width) across every name, so `parse_register_declaration`,
  `net_declaration` and `parse_integer_declaration` each return a `Vec` and the matching
  `ModuleStatement` variants wrap one. **A memory is not a separate production** — the
  address dimension belongs to the *name* (`register::declared_name`), which is what makes
  `reg [7:0] a, mem [0:15];` legal and what removes the "try the memory form first"
  ordering hazard that two near-identical `reg`-led parsers would otherwise create.
  A `signed` / `unsigned` qualifier belongs to the *declaration* rather than to a name, so
  every name in the list shares it; an `integer` is signed by being an `integer`.
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
- **Signedness rides on the value, and is not part of equality.** A `Register` is bits
  plus how to read them, and that is the only copy of the flag: `SignalState` keeps the
  *declared* signedness by re-stamping it onto the register on every write, so a value
  cannot bring its own and a write can never change a declaration. Carrying it on the
  value is what lets `exec::drive_resolved` sign extend an assignment whose right hand
  side it only ever sees as a `Register`, and what lets `eval` learn a signal's
  signedness by looking the name up rather than asking a second question. `Register`'s
  `PartialEq` and `Hash` are hand written to ignore the flag: `4'sb1111` and `4'b1111`
  are the same four bits, and a store that called them different would journal an edge
  where nothing moved.
- **Signedness propagates *down* as well as up.** `expression_is_signed` walks a
  subexpression without evaluating it, because Verilog decides an operation's signedness
  before evaluating its operands and then pushes the answer back down: `(a >>> 1) | u` is
  unsigned because `u` is, and that makes the `>>>` inside it a plain `>>` even though `a`
  was declared signed (corpus `pr3104254`). A bottom-up rule alone gets that wrong. The
  operand classification lives in one place, `eval::operand_rule`, and
  `expression_is_signed` and `eval_in_context` both read it so they cannot disagree.
  Two things keep that walk off the hot path, and both matter: `StateStore::any_signed`
  answers for every identifier in a design that declares nothing signed without hashing a
  name, and the "an unsigned context demotes the result" step lives in the *leaf* arms of
  `eval_in_context` rather than in a shared epilogue — an operand evaluated in an unsigned
  context already comes back unsigned, so there is nothing for an operator to demote.
  Putting it in the epilogue instead costs about 40% on `bench eval`.
- **A width propagates down the same way a sign does, and through the same table.**
  `eval_in_context` carries a `width` beside `signed_context`, and `operand_rule` decides
  which operands each reaches — so an operator can never be context-determined for one and
  self-determined for the other. The width is a *lower bound*, which is what makes
  `SELF_DETERMINED` (zero) the ordinary case rather than a second code path, and
  `eval_sized` the only way a target's width gets in. Keeping the padding cheap is
  deliberate: `widened` is `#[inline(always)]` over a `#[cold] pad`, `widened_result` hands
  a `Result` straight back rather than unwrapping and rewrapping a 70-odd byte `Register`,
  and `sized_within` stops a comparison of two plain signals from measuring anything.
  Undoing any of those costs 5–10% on `bench eval` on its own.
- **A comparison sizes and signs its two operands against *each other*.** It is
  self-determined as far as the expression around it goes — one unsigned bit, always — but
  `a/b` beside a sixteen bit net divides in sixteen bits, and a signed operand beside an
  unsigned one is zero padded rather than sign extended. That mutual context reaches all
  the way down, so `(a >>> 1) === 4'b1111` is *false* for a signed `a`: the unsigned
  literal makes the comparison unsigned and an unsigned `>>>` is a plain `>>`. Write
  `4'sb1111` when the arithmetic shift is what is being tested — that is why
  `test_signed_declarations_simulate` does.
- **`Register::to_decimal` accumulates into a machine integer**, so it overflows on
  anything wider than about 31 bits. `tasks.rs` formats decimals through `to_u128`
  instead; do the same rather than reaching for `to_decimal` on a real signal.
- **A function's own name is a variable, and now so is its qualified spelling.** The body
  returns a value by assigning to it, so `elaborate` gives the function's name, its
  arguments and its locals frame names (`dut.f`, `dut.f.a`) and renames the compiled body
  through a resolver that knows the difference between one of those and a design signal.
  That is also why `rename_expression` qualifies the name of a *called* function: a
  function belongs to the instance that declares it, and two instances of one module have
  a definition each. A `$name` is still never qualified — it is the simulator's, not the
  design's.
- **What a function body may not do is checked once, at elaboration.** `analyse_function_body`
  walks the compiled instructions and rejects a `#delay`, a system task, a non-blocking
  assignment, a write to anything the function does not declare, and `$random` — each with
  a name. Every one of them is something a frame would silently swallow, and a call that
  quietly did nothing is the hardest kind of wrong answer to find. The same walk is what
  produces the read set a call copies into its frame, so adding a new `Instruction` means
  teaching `BodyNames` about it or a function will stop seeing what it reads.
- **A function item is told from a statement by whether it declares a type.**
  `behavior.rs::function_item` gives up unless it saw a direction or a storage keyword —
  `input`, `reg`, `wire`, `integer`, `time`, `signed`, or a range — which is what lets
  `many0` stop at the first statement of the body. A body is then a `begin`…`end` block or
  a bare list of statements, the same `alt` `initial` uses. A function's range is a
  `simple.rs::range` like any other, so `function [W-1:0] f;` is sized from a parameter
  exactly as a `reg` is.
- **A memory and a signal cannot share a name, and nothing else tells `m[3]`
  from `a[3]`.** `StateStore` keeps two maps and `elaborate` decides which one a
  declaration lands in by whether it carried an address dimension. Anything that
  wants to know whether a name is a memory has to ask the store — the
  `Expression` says nothing, and a `BitSelect` on a memory is not distinguishable
  from one on a vector in the AST. A bare memory name, or a part select of one,
  is `EvalError::MemoryAsValue` rather than `UnknownIdentifier`: the name does
  exist. `MAX_MEMORY_DEPTH` in `elaborate.rs` makes a nonsense dimension a named
  error rather than an allocation nothing survives.
- **A select may be separated from the name it selects from.** `v [0]` and `v [3:0]` are
  `v[0]` and `v[3:0]` — `expr.rs`'s `bit_select` / `part_select` skip whitespace and
  comments before the `[`. That widening is safe where the unary junction's is not: `[` is
  not an operator, so nothing else can claim it. `operand_no_ws` still refuses whitespace
  between a unary operator and its operand, which is what tells `a && b` from `a & &b`.
- **A based literal is three tokens.** The size, the base designator and the digits are
  separated by whitespace and comments exactly as `#` is from its delay value, so `5'h 0`
  and `5 'h0` parse. The `'` and its base letter are *one* token — `5 ' h0` is not a
  literal — which is also what the LRM says.
- **A null statement leaves no node behind.** A bare `;` is a legal statement that compiles
  to nothing, so `behavior.rs::null_statement` returns `()` rather than a
  `ProceduralStatements` variant: `statement_body` gives an empty `Vec` for `else ;`, and
  `statement_run` — what `parse_block` and a function body use — drops it from the list.
  Nothing downstream had to learn a node meaning "nothing". Each alternative consumes its
  own `;`, so the `many0` cannot spin.
- **An `assign`'s strength resolves per bit over the whole net, not over its right hand
  side.** A `0` the *target's* width introduced therefore floats exactly as one the
  expression produced does: `assign (strong1, highz0) a = 1'b1;` on a `wire [3:0]` is
  `zzz1` rather than `0001`. **Contention between two assignments is not an error** — they
  resolve by level, so `pull` loses to `strong`, and drivers tied at the strongest level
  agree on a bit or that bit is `x`: `4'b1100` against `4'b1010` is `1xx0`. That is
  iverilog's answer and it is `resolve_bit`'s, so there is deliberately no second,
  differently shaped rule for the same question.
- **An `assign` is a declaration list too.** `assign a = 4'd5, b = 4'd8;` is two targets
  under one keyword, so `parse_continuous_assignment` returns a `Vec` like every
  declaration parser and `ModuleStatement::Assignment` wraps one. The strength belongs to
  the `assign` rather than to a target, so every target in the list shares it, the same way
  every name in a `reg [4:0] a, b;` shares one width.
- **A blank port connection keeps its position.** `two U7 (,)`, `two U8 (w3,)` and
  `two U9 (,w4)` leave a port unconnected, so `ModuleInitArguments::Positional` holds
  `Vec<Option<Expression>>` and `elaborate::connections` filters the `None`s out *after*
  zipping against the ports. Dropping a blank at parse time instead would silently bind
  every later connection to the wrong port. A single blank is `NoArgs` — `()` is an empty
  argument list, not a one-element list with a gap. A blank *named* connection (`.a()`) and
  a blank in a module *header* (`module m(a,);`) are still parse errors.
- **`-> e;` is parsed as an assignment to the event's name.** There is no statement kind
  for a trigger and no instruction for one: a trigger and an ordinary write reach the
  simulator down the same path, and `exec::resolve_target` is where the store is asked
  which it is — `ResolvedTarget::Event` for a name declared `event`, which
  `drive_resolved` fires while dropping the value that was evaluated for it. Assigning to
  an event is illegal Verilog, so nothing that was already legal is given a second
  meaning. The trigger is deliberately reported as **not** a change: saying otherwise
  would keep the continuous assignment fixpoint from ever settling.
- **A task enable is the loosest statement shape there is, so it must not claim a reserved
  word.** `my_task;` and `my_task(a);` are a bare identifier followed by `;` or by an
  argument list — which is also exactly what `wait (a);` looks like, and what every
  statement form the grammar has yet to learn will look like. `parse_task_enable` is tried
  last in `procedural_statement`'s `alt` *and* rejects anything `keywords::is_reserved_word`
  knows, so `wait (1);` is still a parse error rather than a task nothing declared. Without
  that guard five corpus files stop being parse failures and become `UnknownTask` failures
  instead, which is a worse answer wearing a better one's clothes.
- **A `time` variable inside a `function` or `task` is 64 bits by being a `time`**, the way
  an `integer` is 32 by being an `integer`. `behavior.rs::declared_type` reads the keyword
  rather than treating it as a bare storage class, so `output time stamp;` is a 64-bit
  unsigned argument.
- **A generate block's items go through the same `declare` and `build` passes the
  module's own statements do**, in the scope its label gave them. Adding a
  `ModuleStatement` variant that means something inside a generate block means teaching
  `declared_names`/`declared_by` about it too, or a name it declares will resolve
  outwards to the module and collide with a sibling iteration's.
- **A genvar is substituted, not renamed**, and the substitution runs *first*. It is not
  a name that resolves to anything, so qualifying it would produce a signal nothing
  declares — which is a silent `x` rather than an error. `Program::substitute` is the
  instruction-list half of it; `TaskCall::substitute` the `$display` argument half.
- **An unnamed generate block is numbered from a counter on the `Elaborator`**, and a
  loop takes its label *once*, before the iterations — numbering per iteration would give
  `genblk1[0]`, `genblk2[1]` and defeat the point.
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

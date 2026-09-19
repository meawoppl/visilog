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
| `numbers.rs` | raw binary / decimal / hex digit runs, and `real_number` — the one place a real number is spelled out |
| `constants.rs` | sized and based literals (`8'hFF`, `'b1`) → `VerilogConstant` |
| `string.rs` | double-quoted string literals |
| `identifier.rs` | `Identifier`, identifier lists, bit/part select |
| `keywords.rs` | the `VerilogKeyword` enum and lookup for what SystemVerilog added, and `is_reserved_word` for the IEEE 1364-2005 set |
| `operators.rs` | `UnaryOperator` / `BinaryOperator` and their token parsers |
| `expr.rs` | the expression grammar — the biggest and trickiest file |
| `delay.rs` | `#<n>` delay terms, whose values are `Expression`s evaluated against the store, and `parse_gate_delay` for the `delay3` a gate or an `assign` writes |
| `nets.rs` | `wire`/`tri`/... declarations → `Net` |
| `gates.rs` | the built-in primitives — `GateKind`, `DriveStrength`, `GateInstantiation` |
| `generate.rs` | `generate … endgenerate`, `genvar` and `defparam` — the shapes, never the decisions |
| `primitive.rs` | `primitive … endprimitive` — a user-defined primitive and its truth table |
| `specify.rs` | `specify … endspecify` — path delays, timing checks and `specparam` |
| `register.rs` | `reg` and memory declarations → `RegisterDeclaration` |
| `integer.rs` | the keyword-led variable declarations: `integer`, `time`, `real`/`realtime` and `event` |
| `assignment.rs` | `ContinuousAssignment` (`assign x = y;`), its optional `gates.rs` drive strength and its optional `#delay`, and `ProceduralAssignment` (`x = y;`, `x <= y;`) |
| `parameter.rs` | `parameter` / `localparam` declarations → `ParameterDeclaration` |
| `behavior.rs` | `initial` / `always` blocks, sensitivity lists, `begin…end` and `fork…join` — named or not — `if`/`else`, `case`, `wait`, a statement-level event control, `$system_task(…)` calls, `function … endfunction`, `task … endtask` and the task enable, and the four procedural drive statements (`assign` / `deassign` / `force` / `release`) |
| `statements.rs` | `ModuleStatement` — the union of things legal in a module body |
| `modules.rs` | `module … endmodule`, ports, and module instantiation |
| `source.rs` | `parse_verilog_source` — a whole file of modules — `parse_verilog_source_located`, which pairs each with the offset that places it against the `` `timescale `` directives, and `ModuleLibrary`, the name → module index |
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
and on `ModuleLibrary::timescale`, and handed to `Simulator::set_timescale` for a waveform
dump's `$timescale`, *and* positionally in `Preprocessed::timescales` so each module knows
the one it was written at), `` `unconnected_drive ``/`` `nounconnected_drive `` (recorded
positionally the same way, in `Preprocessed::unconnected_drives`), `` `resetall `` (which
restores `Preprocessor::with_default_timescale` and clears the unconnected drive),
`` `include ``
with a search path set by `Preprocessor::with_include_dir`, the `` `" ``/`` `\`" ``/`` `` ``
escapes, and the `` `__FILE__ ``/`` `__LINE__ `` builtins. `IGNORED_DIRECTIVES` skips
`` `begin_keywords ``, `` `celldefine ``, `` `default_nettype `` and the rest of the
pragma-like set together with the rest of their line — `` `resetall `` and
`` `unconnected_drive `` are no longer among them, because both change what the design
means.

**A positional directive is recorded, not acted on, and the two halves meet in
`parse_expanded`.** `` `timescale `` and `` `unconnected_drive `` both apply to *what
follows them* and the grammar never sees either, so the preprocessor records a list of
`(offset, value)` pairs into the **expanded** text and `parse_verilog_source_located` pairs
each module with the offset it started at. Anything else positional wants exactly that
shape: a `Vec` on `Preprocessed`, a `…_at(offset)` accessor, a field on `VerilogModule`
that is `None` for a module built by a test, and one line in `parse_expanded`.
`skip_directive_argument` hands its argument back rather than only skipping it, which is
how `` `unconnected_drive `` reads `pull0`/`pull1` while getting the same whitespace and
comment skipping every other one-argument directive gets — corpus `br_gh782c` writes block
comments between the directive and its argument and across the newline after it. An
argument that is neither is a **named error**, never a silent `z`.

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
towards `finish`. **A defaulted `start` is the lowest address and a defaulted `finish` the
highest**, so an unranged load runs *upward* whichever way round the memory was declared —
`mem [7:0]` fills `mem[0]` first. That is IEEE 1364-2005, which reversed the 1364-2001 rule
of following the declaration; iverilog 12.0 warns about exactly this and takes the 2005
reading, and the corpus is a 2005 suite. A `start` with no `finish` also runs upward, while
two explicit bounds still set the direction — `$readmemh(f, mem, 5, 3)` loads `mem[5]`,
`mem[4]`, `mem[3]` (all three measured against iverilog).
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

**Writing has the mirror seam, `Simulator::set_output_directory`.** A relative
`$fopen` or `$writemem…` name hangs off it, and it defaults to the process
working directory so a caller that never said otherwise gets what it always got.
It lives on the `StateStore` (`resolve_write_path`) rather than on the
`TaskContext`, because `$fopen` is a system *function* and `eval` is handed the
store and nothing else. The corpus harness points it at a scratch directory
under the temp directory with `work/` already made inside — which is exactly what
iverilog's own test driver does before it runs one, and what keeps a corpus run
from scattering files through the repository.

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
unconnected input is declared `z` — unless the module was *declared* inside an
`` `unconnected_drive pull0 `` / `pull1` region, which is what
`VerilogModule::unconnected_drive` carries to `declare_port` (IEEE 1364-2005 §19.9, corpus
`uncon_drive`, `br_gh782c`). The directive belongs to the module's declaration and not to
the instantiation, which is why it travels with the port rather than with the connection.
`Simulator::with_modules(modules, top)` is how a design
of more than one module is handed over; `Simulator::new(module)` still takes a single
module as its own top.

**A port and the parent's signal have to agree about signedness to be one entry.** A
store entry carries *one* signedness — a value is bits plus how to read them — so
`input signed [31:0] a` bound to a plain `reg [31:0]` cannot be aliased onto it: the
child would read its own port unsigned and `a <= b` would rank `32'h80000000` above
`32'h7fffffff` (corpus `pr1033`, whose whole comment is that complaint). `can_alias` is
that question, asked before `plain_identifier`'s answer is used, and a port that fails it
takes the arrangement a port bound to an *expression* already had — its own entry, at its
own declared signedness, with a continuous assignment carrying the value across. An
`inout` is the exception and stays aliased: it is read as well as written, and one
assignment only runs one way.

**A port is only aliased when it is the same number of bits as what it was bound to**,
because one store entry has nowhere to extend or truncate. `Elaborator::reconcile_port_widths`
turns a mismatched one back into exactly the shape a port bound to an expression has — its
own entry plus a continuous assignment in the port's own direction — so the conversion is
an assignment's: a signed port sign extends, an unsigned one zero extends, and a narrower
target truncates. Measured against iverilog 12.0, which warns about each connection and
then agrees bit for bit (corpus `pr2121536`, `pr2121536b`, whose signed output aliasing
zero extended; and `pr1866215b`, whose data line now matches). It runs **after the
parameters are declared**, which is the first moment a port's range has a value at all, and
it leaves an `inout` aliased whatever its width — that one is read as well as written and
one assignment only runs one way.

The assignment it creates is **the only driver the elaborator adds on its own account**, so
it cannot know whether the net it lands on is driven already. A design that miswires a
port's direction drives both ends — iverilog coerces the port to `inout` and warns — and
two plain drivers of one net overwrite each other every pass and never settle, so the net
is named a `resolved_net` and both go through `resolve_contributions` instead, where an
undriven `z` contributes nothing (corpus `br_gh127c`, `br_gh127f`, which stopped
elaborating without it).

**System tasks print into a buffer, not to stdout.** `$display`, `$write` and `$finish`
are compiled to an `Instruction::Task` and carried out by
`tasks::TaskContext`, which the `Simulator` owns: `simulator.output()` hands back
everything the design printed, so "did this design print `PASSED`?" is a plain assertion —
which is exactly what a self-checking corpus test needs. `$finish` sets a flag rather than
exiting the process; `advance` and `poke` become no-ops once it is set, and `now` stops
where it stopped.

**`$finish` ends the simulation at the end of the timestep it ran in, not at the instant
it ran.** It stops the block that called it — `resume` returns the moment one lands — and
nothing more is *scheduled*: `settled_resume` queues no free-running restart, no `#delay`
resumption, no `wait` and no `fork` branch once the flag is set. But everything already
queued at that instant still runs, and the deferred tasks still report, so an
`always #10` beside an `initial #30 $finish` still runs at 30 and the `$monitor` still
prints that step's line (measured against iverilog 12.0; corpus `pr243`, whose last line
is exactly that). Skipping the rest of the round instead loses both. The scheduling guard
is what makes it terminate: without it a free-running block whose body ends in `$finish`
restarts at the same instant for ever (corpus `always3.1.6D`). Which `$name`s exist is decided at *compile* time by `TaskCall::compile`,
so an unrecognised task is an error naming it rather than a silent no-op — a design that
quietly printed nothing would look just like one that passed. The buffer is an `Output`,
whose text sits behind an `Rc<RefCell<String>>` so that a *handle* to it can be given to
the `StateStore` — which is how a `$display` written inside a function body prints; see
"A call runs against a frame".

**`$stop` resolves to the same `SystemTask::Finish`**, because a `Simulator` has no console
to hand control back to — it is a library object whose caller is waiting on `advance`. That
is exactly the case iverilog spells `vvp -n`, whose own help text reads
"Non-interactive (`$stop` = `$finish`)", so the equivalence is iverilog's rather than one
invented here. Left to its debugger `vvp` instead prints a `** VVP Stop(N) **` banner
naming the source line and — reading an empty console — carries on; no gold file in the
corpus records that banner, and visilog has no line numbers to name in one.

**A descriptor is a bit mask, and bit 0 is the buffer.** `$fopen("work/a.txt")` hands back
a *multi-channel descriptor* — one hot, allocated from bit 1 upwards, so the first file is
2, the second 4, the third 8 — and `$fdisplay`/`$fwrite`/`$fmonitor`/`$fstrobe` write to
every channel whose bit is set. Bit 0 is standard output, which is why a corpus design
writes `$fdisplay(fp|1, …)`: that is what puts the same line in the file *and* in the
buffer `simulator.output()` hands back. Without the mask a file-writing design's output
would never reach a test at all. `$fclose` frees the bit for the next `$fopen` (corpus
`fopen2` opens a fourth file after closing its second and asserts it gets the second's bit
back), and `$fflush` pushes the buffered writer out — with no argument at all, every one of
them. The two-argument `$fopen(name, "w")` is the other form and returns a **file**
descriptor: bit 31 set over a number allocated from 3, since 0, 1 and 2 are the standard
streams. Reading takes that second form — see below. `$fread` and `$ferror` are still
names nothing implements.

**`$fopen` is a system *function*, so the file table lives on the `StateStore`.** `eval` is
handed a `&StateStore` and nothing else, so a table on the `TaskContext` would be
unreachable from the one place a descriptor is produced — this is the same reasoning that
put the `$random` stream there, and it has the same shape: a `RefCell` behind an `Rc`, so
opening a file is a shared-reference operation and a function call's *frame* shares the
table rather than throwing it away. `TaskContext::run` already took a `&mut StateStore`, so
the writing side needed nothing.

**A file that cannot be opened is 0, and a channel nothing opened is dropped.** Both are
deliberate departures from "everything unimplemented is a named error", because neither is
unimplemented: `if (fp == 0)` is how a design reports a failed open *itself*, and an error
would take that report away from it, while iverilog answers a write to an unopened channel
with a warning on standard error and carries on. What stays an error is a descriptor that
is not a fully **known** value, and a `$f…` task with no descriptor at all.

**`$sformat` and `$swrite` format into a register rather than printing.** The rendering is
the same `render` every other task goes through; the text then becomes a bit vector of
eight bit characters and is *driven* like any other assignment, so the target's width does
the rest — a wider one is zero extended on the left, which `%s` renders back as leading
spaces, and a narrower one keeps the **last** characters (`"abcdef"` into a `reg [15:0]` is
`"ef"`, measured against iverilog 12.0). The one difference between the two spellings is
which argument is the format string: `$sformat`'s second argument always is, even when it
is a `reg` holding one, where `$swrite` follows the `$display` rule that only a literal is.

**`$sscanf` and `$fscanf` are system functions that write through their arguments.**
`code = $sscanf(s, "%d %h", a, b)` hands back a count *and* fills `a` and `b` — the one
kind of expression that changes the store, while `eval` is handed a `&StateStore`. So
`eval` resolves each argument to a `ResolvedTarget` up front, the scan runs, and the
values are queued with `StateStore::owe_fill`, behind a `RefCell` like the file table.
`program::resume` drains the queue at the **top of every instruction**: that is the first
moment the statement that evaluated the call has finished with it, so `code` is written
first and both are in place before the next statement reads either — including after the
block's last statement, since the drain comes before the fetch of `Halt`. A design with no
scan pays one `RefCell` length check per instruction. A frame gets a fresh queue rather
than a shared one, so a `$sscanf` in a function fills the function's own variables. A
continuous assignment is re-evaluated every pass, so one that owes a fill is refused by
name in `propagate` rather than reading a file an unpredictable number of times — which
is the same check that refuses a *function* with a side effect there, since one hands its
writes back through this queue too.

The conversions live in `scan.rs`, one engine over a `Source` that is either the string
or the file's `Reader`, and every rule was measured against iverilog 12.0:

- **`-1` means there was nothing to read at all, decided once before the format is
  walked.** `$sscanf("", "%d", a)` is `-1` and `$sscanf("  ", "%d", a)` is `0`. Every
  later failure — a conversion or a literal that does not match — stops the scan and
  answers the count so far, having written what it converted and left the rest alone.
  That distinction is the whole reason a design can loop on the answer.
- **Whitespace in the format matches a run of whitespace or none; any other character
  must match exactly with nothing skipped in front of it** (`","` does not match
  `"  ,"`). Every conversion but `%c` skips leading whitespace itself.
- `%d` is an optional sign then digits, `_` between them but not leading, or a single
  `x`/`z`/`?` standing for the whole value at the target's width. `%b`/`%o`/`%h`/`%x`
  treat `x`, `z` and `?` as digits within the run. `%f`/`%e`/`%g` are C's number with
  iverilog's two departures: a trailing point is fine (`"2."`) and an `e` with no digits
  after it fails the whole conversion (`"2.ea"` is no match). `%s` is a run of
  non-whitespace, landing at the target's low end. A width (`%5s`) counts consumed
  characters, `*` suppresses the store and the count but not the match, and `%%` is a
  literal. An unknown format string is `-1` (corpus `scanf4`).
- **Named errors, not zeros:** `%t` (it rounds to the `$timeformat` precision, which is
  on the `TaskContext`, and scales by the timescale nothing models), `%u` and `%z` (raw
  binary, which the text-shaped buffer cannot carry — corpus `sscanf_u`/`_z`,
  `fscanf_u`/`_z`), `%m`, an unknown conversion, and a format asking for more arguments
  than it was given.

**A file opened for reading is a `Reader` with one byte of push-back.** `peek` reads a
byte into the slot and `bump` consumes it, so a scan that stops at a character it does not
want leaves the stream *before* it and `$ftell` answers what C does; `$ungetc` is the same
slot filled by hand. `$fgets` reads up to the target's width in bytes, stopping after a
newline, answers the count and leaves the target alone at end of file. `$feof` is sticky
like C's — reaching the last byte is not end of file, a read that finds nothing is — and
`$fseek`/`$rewind` clear it. `$fopen(name, "r")` resolves a relative name through the same
search path `$readmemh` uses, which is why that list moved from the `TaskContext` onto the
`StateStore`: `$fopen` is evaluated by `eval`.

**Reading something that is not a readable file is end of file**, because that is what
iverilog answers and what a design can act on: a multi-channel descriptor (write-only by
construction), a file opened for writing, and one nothing has open are all `-1` from
`$fscanf`/`$fgetc`/`$ftell` and `0` from `$fgets`. A file opened in an **update** mode
(`r+`, `w+`, `a+`) is the exception and a named error: it asks for a handle that reads and
writes at once, which is not implemented, and a silent `-1` would look exactly like a file
that had simply run out.

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

**A value change dump is the one artifact a design writes for a person, and it lives
in `vcd.rs`.** `$dumpfile`, `$dumpvars`, `$dumpon`, `$dumpoff`, `$dumpall`,
`$dumpflush` and `$dumplimit` are all implemented, and every detail of the text was
measured against iverilog 12.0: identifiers are base 94 over `!`..`~` with the least
significant digit first (`!`, `"`, … `~`, `!"`), a scalar is `0!`, a vector is
`b1010 "`, a real is `r1.5 #`, and a vector's leading digits are trimmed the way a
reader re-extends them — a run of `0`s goes unless an `x` or `z` is under it (then one
`0` stays), a run of `x`s or `z`s collapses to one, and a leading `1` is never
touched (`00zzzzzz` is `b0zzzzzz`, `xx01` is `bx01`). `$dumpoff` writes every variable
as `x` (`bx` for a vector, `rNaN` for a real), and time that passes while it is off
leaves no section at all.

**A time section is driven by the change journal, not by a scan.** `settle` already
takes `StateStore::take_changes` once per delta cycle; while a dump is armed it hands
the same names to `VcdDump::note_changes`, which marks the dumped variables they read
as dirty — one hash lookup per *written* name. `Simulator::end_of_timestep` is where
the section is written, because it is the moment the design has stopped moving, and
only a dirty variable whose value really differs from what the file last said gets a
line. That is also why `a = 1; a = 0; a = 1;` in one timestep is one line, which is
what iverilog writes. A design that dumps nothing asks `TaskContext::is_dumping` —
an `Option` check — once per settle round.

**The file opens at the first `$dumpvars`, and the header waits for the end of that
timestep.** `$dumpfile` alone writes nothing, which is what iverilog does; `$dumpvars`
opens it through the store's `FileTable` — the same table and the same
`set_output_directory` `$fopen` uses, so a corpus run writes into the scratch
directory — and prints `VCD info: dumpfile <name> opened for output.` at once, where
iverilog prints it, ahead of whatever the block prints next. The `$scope` tree and the
opening `$dumpvars` block are written at the end of the timestep, so a second
`$dumpvars` in the same timestep still adds to them; one in a *later* timestep is
ignored with iverilog's `VCD warning: $dumpvars ignored, previously called at simtime
N`. No `$dumpfile` means `dump.vcd`. The final `#<time>` is written when the design
calls `$finish`, and `advance` flushes the file before it returns, so a waveform read
after a run is the whole run.

**What a `$dumpvars` argument names is resolved against the flat store.** The top
module is the store's root and carries no prefix, so `top`, `top.u1` and `u1` resolve
by stripping it; a name that is a signal dumps that signal, one that prefixes signals
dumps the scope to the level asked for (`0` is all the way down), and `arr[4]` dumps
one memory word under iverilog's escaped name `\arr[4]`. A name that is none of these
is a named `SystemTask` error, never an empty waveform. Memories are not dumped by a
scope, and neither are events or the hidden `$repeat$` / `$hold$` slots. An instance
port aliased onto its parent's signal is declared under its own name with the parent's
identifier, which is how iverilog shows one store entry under two names — so it writes
no second line.

Where it deliberately differs from iverilog: `$timescale` states the design's *unit*
rather than its precision, because the clock counts ticks of the unit and nothing
rescales them (`` `timescale 1ns/1ps `` gives `1ns` and `#5`, where iverilog gives
`1ps` and `#5000`); `$date` is ISO 8601 UTC; variables are in the store's sorted order
grouped into one scope tree rather than one tree per `$dumpvars` call; a parameter is a
store signal like any other and so is dumped as a `reg` in the `$dumpvars` block rather
than as a `$var parameter` in a `$comment` block; an `integer` is a `reg [31:0]`; and
an aliased port is declared with its parent's `wire`/`reg` kind, since the alias table
keeps only the name. `SignalState::is_net` is the one field added for the dump — a net
flag set at declaration and carried across every write, so `$var wire` and `$var reg`
come from the declaration rather than from a guess.

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
the vector really has. A string **literal** goes through the same vector rather
than being printed as its own text, which is the only way `%s` and `%0s` of
`"\000a\000b"` come out as `" a b"` and `"a b"` and `%s` of `""` comes out as one
space — a literal is a value wherever a number is wanted, and `%s` was the one
specifier that did not treat it as one (corpus `string13`, `string14`, both of
which iverilog 12.0 itself fails).

**A `` `timescale `` belongs to a *module*, not to a file.** The directive applies to
whatever follows it and `` `resetall `` puts the default back, so one file may hold
several modules at several scales — which is what `$printtimescale` reports and what
`Preprocessed::timescale`, a single field, could not say. The preprocessor therefore
records `timescales`, a list of `(offset, scale)` pairs into the **expanded** text, and
`parse_verilog_source_located` pairs each module with the offset it started at;
`parse_expanded` is where the two halves meet, because the grammar never sees a directive
at all. `VerilogModule::timescale` is the result, and it is `None` — the default, `1s /
1s` — for a module built by anything but that path. `Preprocessor::with_default_timescale`
is iverilog's `+timescale+1ns/1ps`, and it is what `` `resetall `` restores: a design that
resets goes back to what the *simulator* was told, not to `1s / 1s` (corpus `pr1403406a`).
`` `resetall `` is consequently the one pragma-like directive that is not inert and is no
longer in `IGNORED_DIRECTIVES`.

**`$printtimescale` is a question about the hierarchy, which flattening has just thrown
away**, so `Elaborated::instances` carries it over: every instance under the hierarchical
name a design writes it under — `top`, `top.dut`, `top.mid.leaf` — beside its module's
scale. `TaskContext::describe_scopes` takes that list *and* a second one of every module
by its own name, because a module the design never instantiated still has a scale and
still answers (`$printtimescale(othertop)` in corpus `pr1701855`).

The resolution rule is measured, not guessed. A name is tried **as written** first,
shedding one trailing segment at a time until what is left names an instance or a module,
and only then qualified by the calling scope — the other way round, `othertop` would
become `top.othertop`, shed its tail and answer for `top`. What is printed is the *whole*
name rather than the prefix that matched (`top.ipval` reports as `(top.ipval)` at `top`'s
scale), and a bare `$printtimescale` reports the instance the call sits in, which is its
`%m` scope with any named block shed off. Two renderings come from the store rather than
from the text: a bit select of a **vector** is written back as the one-bit part select it
stands for (`rgval[0:0]`) while a word of a **memory** keeps its single index
(`rgarr[0]`), and only the declaration tells those apart.

Its argument is kept as the **text it was written as**, at `TaskCall::compile` time. It
names a scope rather than a value — an instance, a module, a task, an event — several of
which cannot be evaluated at all, and `TaskCall::rename` would otherwise rewrite the
identifier into a flat store name and lose the hierarchy the answer is about.

Where it falls short of iverilog: a name whose *tail* names nothing is answered for the
scope it lies in rather than refused, because the calling instance always matches some
prefix — iverilog resolves the whole path through VPI and reports one that does not exist.
And iverilog's own `Command File: Warning: default timescale is being set multiple times.`
is a compiler diagnostic visilog has no channel for, which is the whole of why corpus
`pr1403406b` is a gold mismatch while `pr1403406`, `pr1403406a`, `pr1701855` and
`pr1701855b` match.

**`$timeformat`'s `units` is a scale factor, and `%t` is the one place the
`` `timescale `` reaches the output.** The clock counts ticks of the **unit** of the
module a call was written in — `TaskContext::tick_fs`, which asks the same `scale_of`
`$printtimescale` does — and `%t` restates one of those in the power of ten the design
named, so `` `timescale 1ns `` with `$timeformat(-6, …)` prints `10` as `0` and with
`$timeformat(-12, …)` prints it as `10000`. Both ends are held in **femtoseconds**,
because a `` `timescale `` term is `1`, `10` or `100` of a unit and only the finest unit
makes every ratio an exact integer.

A design that never called `$timeformat` prints in the **finest precision** any
`` `timescale `` in it declared (`TaskContext::default_time_units`), which is what the
LRM asks for: `` `timescale 1ns/100ps `` renders `$time` of 5 as `50`. That is also why
a design with no directive at all is unchanged — a module with no `` `timescale `` is at
`1s / 1s`, so the tick and the display unit are both a second and the ratio is one.

The scaling of an **integer** time is exact and **truncated** — 1500 ticks of `1ns` at
`$timeformat(-6, 0, …)` is `1`, not `2`, and at `$timeformat(-6, 1, …)` is `1.5` — while
a **real** one (`$realtime`, or a literal) is scaled as a double and rounded by the field
width, since it carries a fraction of a tick. Both were measured against iverilog 12.0,
which takes the two paths as well. `precision` fractional digits, then the suffix,
right-aligned in `min_width` (twenty by default), with an explicit `%12t` overriding
`min_width` and `%0t` meaning no padding at all.

What is still not rescaled is *time itself*: `#5` advances five ticks whatever the
module's unit is, so a design mixing `` `timescale 1ns `` and `` `timescale 1us ``
modules runs both at the same rate where iverilog would not (#209).

**A system *function* is an expression operand, and `eval` implements it.** `$time`,
`$stime`, `$signed`, `$unsigned`, `$random`, `$fopen`, `$bits` and `$clog2` parse anywhere an
operand is legal — `a = $random;`, `if ($time > 5)`, `assign y = $signed(a) | b;` — as
`Expression::SystemFunctionCall(name, args)`, the name carried without its `$`. That is
deliberately *not* `Expression::FunctionCall`, which names a function the design declares
and resolves down a different path. A `$name` nothing implements is
`EvalError::UnknownSystemFunction`, never a zero, and a wrong argument count is
`EvalError::SystemFunctionArity`.

**`$stime` is unsigned, like the `$time` beside it**, because `time` is an unsigned type
— and that is the field `%d` gives it: iverilog 12.0 prints `$display($stime)` in ten
columns where an `integer` takes eleven (corpus `pr2842621`). iverilog is inconsistent
about it in the other direction — `($stime - 2) < 0` is *true* there, so it reads the
same call as signed in arithmetic while printing it at the unsigned width, and `$clog2`
has the same split (`$bits` is unsigned in both). No one flag gives both answers, and
the unsigned reading is the one that matches the type.

`eval` is handed a `&StateStore` and nothing else, so the system functions that are
not pure functions of their arguments reach the simulation *through the store*:
`StateStore::set_time` carries the clock `$time` reads — `Simulator::advance` moves it
with `now`, and it is the only clock, which is why `TaskContext` no longer holds one —
`StateStore::open_channel` / `open_descriptor` own the files `$fopen` opens, and
`StateStore::next_random` owns the `$random` stream.

**`$random` is IEEE 1364-2005 17.9.3's generator, transcribed rather than chosen.** The
point of that algorithm is that every simulator draws the *same* numbers from the same
seed, so `state_store::random_from_seed` is the standard's reference C line for line —
its `69069 * seed + 1` step, its `float`-flavoured scaling through `uniform`, and its
truncation toward zero. The whole of the stream's state is one 32 bit seed
(`DEFAULT_RANDOM_SEED`, 0, which `uniform` maps to a stream of its own), so a design that
draws random stimulus draws the same stimulus on every run *and* the stimulus iverilog
draws — corpus `pr556` prints 256 unseeded draws and `pr995` 93 seeded seed/value pairs,
and either one catches any departure at all.

**The two forms differ only in where the state is kept.** A bare `$random` advances the
store's seed; `$random(seed)` reads the *design's* variable and writes the next seed back
through it, because the argument is an `inout` — which is what makes `for (…) r =
$random(s);` a sequence rather than one number repeated. The write-back goes on
`StateStore::owe_fill`, the queue `$sscanf` already used, so it lands at the next
instruction boundary and the statement after the call reads the new seed. A seed that is
not a writable target is `EvalError::RandomSeed` naming it, never a stream that cannot
move; an unknown bit of one reads as `0`, which is what iverilog takes from a four-state
value asked for as an integer. A **second** draw in the same statement reads the seed out
of that queue rather than out of the signal — `StateStore::pending_fill`, the same
question a second call to a function with a side effect already asked — so
`{$random(s), $random(s), $random(s), $random(s)}` is four different numbers (corpus
`concat3`).

**The plus-args are the one thing a design learns about its own invocation, and only a
caller knows them.** `$test$plusargs("opt")` is a *prefix* match over the whole `+name=value`
word — `+option=1` answers it, and `""` answers whenever there is any plus-arg at all —
and `$value$plusargs("option=%h", v)` matches the same way and writes the text after the
prefix into `v`. They live on the `StateStore` (`set_plusargs` / `plusargs`) for the reason
`$fopen` and `$random` do: both are system *functions*, so `eval` is the only thing that
can read them. `Simulator::add_plusarg` is the seam, beside `add_search_path`, and it takes
the word with or without its `+`; the conversions themselves are `plusargs.rs`.

**An empty plus-arg list is an answer, not a failure.** visilog has no CLI, so a design
run through the library sees nothing unless its caller says otherwise — `$test` is `0` and
`$value` is `0` with its target **untouched**, which is exactly the branch
`if (!$value$plusargs(…))` exists to take. The corpus harness reads the `+` fields out of
the entry's own line in `regress-vlg.list`, which is where iverilog's `RegressionList.pm`
reads them from; without that, four self-checking designs would run their checks against
an option they had been told they were not given.

**The conversions are deliberately not `scan.rs`'s**, and every rule was measured against
iverilog 12.0. Text that will not convert is `x` at the target's width **and the call still
answers `1`** — a plus-arg is a value the design was handed, so reading it is not optional —
where a `$sscanf` would stop and report how far it got. An *empty* value is `0` rather than
`x`, and iverilog draws no warning for that one. `_` is a separator anywhere, including
leading; a sign negates the value in whatever base it was written and the answer is sign
extended (`+neg=-1_00` read as `%h` into a `reg [7:0]` is `8'h00`); `x` and `z` are digits
of the base for `%b`/`%o`/`%h`/`%x` and stand for the whole value for `%d`, where `?` is
**not** one of them; `%e`/`%f`/`%g` take the longest numeric prefix, so `9.825units` is
`9.825`; `%s` lands at the low end of its target. The conversion letter's case does not
matter and `%x` is `%h`. A format with no `%` in it is `EvalError::PlusArgs` naming it,
never a quiet `0` — that would look exactly like a plus-arg nobody gave.

Two things iverilog does that this does not. It prints a `WARNING:` line on **standard
output** naming the offending text and the source line, which visilog has no line numbers
to name. And it applies the write before the statement that made the call has finished, so
`$display("%h", $value$plusargs(…), v)` prints the *new* `v`, where here the fill lands at
the next instruction boundary like `$sscanf`'s and prints the old one.

**Signedness is modelled.** `reg signed [3:0] a;`,
`wire signed`, `input signed`, an `integer`, `4'sd12` and a bare decimal like `42` are all
signed; everything else is unsigned. **A decimal is signed by having no base designator,
not by having no size** — `1` is signed and `'d1` is not, which is IEEE 1364-2005's line
and iverilog 12.0's (`('d1 - 'd2) < 0` is false where `(1 - 2) < 0` is true, and
`parameter tp = 'd1;` prints in ten `%d` columns where `parameter tp = 1;` prints in
eleven — corpus `pr812`, `param-extend`). `VerilogConstant::based` is what tells the two
apart, since both reach the type as an unsized decimal; a *sized* literal always has a
base, so `VerilogConstant::new` reads it off the size and only an unsized one has to say
so. It rides on the `Register` a lookup produces — a
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

**But a context-determined operator sizes its two operands against *each other* as well,
and the context is only the third number in that maximum.** `+ - * / % & | ^ ~^` are
carried out at the widest of the context, the left operand and the right one, so a *narrow*
operand nested beside a wide one has to be widened before it runs rather than padded
afterwards: `(c & ~(1'b1 << 0)) & b` for four bit `c` and `b` is `1110`, because the `~`
inverts four bits — inverting one and padding with zeros gives `0000` (corpus
`pr2985542`). `(a + b + 0) >> 1` for two sixteen bit `a` and `b` is the same rule in the
other direction: the unsized `0` is thirty-two bits, so the addition does not wrap at
sixteen and `16'h8000 + 16'h8000` survives to be shifted (corpus `pr1570635b`,
`pr3098439a`, `pr1913937`).

Both halves of how that is asked are load-bearing for the hot path. **An operand is only
measured for the one beside it**, never for itself, and only when it is `sized_within` —
an operand comes back at least as wide as it is on its own whatever it was asked for, so
measuring it for itself would change nothing, and one that is not `sized_within` is padded
the same way whichever end the padding happens at. And **the right operand reads its share
off the left one's *evaluated* width** rather than measuring it, because by then the answer
is free: the left value is already as wide as the widest of the three. So `count + 1`
measures nothing at all and an expression of two plain operands costs one branch. Measuring
both sides instead doubled `bench eval/nested_arithmetic`.

`other_operand_width` is the one seam, and the one thing it adds to `expression_width` is
that a **real** operand reports `SELF_DETERMINED`: a real has no width to share, and
widening the integer beside it to sixty-four bits reads `-180` as a twenty-digit unsigned
number (corpus `pr1574175`, where `-180 + bits*(360.0/63.0)` and `bits*(360.0/63.0) - 180`
agree only once each has wrapped at the target's thirty-two). That question cannot go
through `StateStore::any_real` — that flag answers for *declarations* and this expression
has no real declared anywhere — so it is asked of the expression, after the measurement and
only when the measurement came back `REAL_WIDTH`.

`benches/simulation.rs`'s `eval/nested_arithmetic` is `((a + b) * 2) - (a & b)` over two
eight bit signals, and the unsized `2` makes the whole expression **thirty-two bits** —
iverilog answers 416 with `$bits` of 32, where evaluating it in eight bits answers 160. The
benchmark is therefore permanently slower than it was by about the ratio of the two widths,
doing four times the bit work on a one-byte-per-bit `Register`. Read a move in that number
as a change in the *width* the expression settles on before reading it as a change in
speed.

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
both" rule, where iverilog reads `1 ? ~a >>> 5 : 0` as signed (corpus `br_gh37`). The
widths in that expression are right; only the sign is not. A `?:` also does not yet size
its two *arms* against each other the way a `Shared` binary now sizes its operands — the
arms both get the context and nothing measures the one beside.

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
same `resume` every other procedural body goes through. The body's writes land in the
frame — which is exactly why a call can be made from an evaluator holding a shared
reference — and every call gets its own frame, so **recursion works**: `fact(n) = n *
fact(n - 1)` returns 120 for 5. `MAX_CALL_DEPTH` (64) is what makes a function that never
reaches its base case `EvalError::FunctionCallDepth` rather than a stack overflow; the
bound is deliberately well under the ~120 an unoptimised build actually survives.

The read set is why a frame costs the *function* rather than the design, and it is closed
over the call graph (`close_reads`): a function that calls another has to copy in what
that one reads too, or the inner call's frame would be missing it. An `@(*)` block's
implicit sensitivity list is extended the same way — a block whose only reader of a signal
is a call still has to wake when that signal moves.

**Two things a body does are visible from outside the frame, and both reach the design
through the store, because the store is all `eval` is handed.**

A **`$display`** prints into the design's own buffer. `Output` holds its text behind an
`Rc<RefCell<String>>` — the shape the `$random` stream and the `FileTable` already use —
and `Simulator::setup` hands the store the handle its `TaskContext` prints into, so
`FunctionDefinition::call` can give the body a `TaskContext::printing_into` that buffer.
The line therefore lands *as it is printed*, which is what makes the ordering right with
no draining anywhere: `$display("outer %0d", f(1));` prints `f`'s line **first**, which is
what iverilog 12.0 does (corpus `function2`, `disblock2`, `pr355`). Only the tasks that
print *now* are allowed — `TaskCall::prints_now` — since a `$strobe`, a `$monitor`, a
`$dumpvars` or a `$finish` all leave state on a `TaskContext` the call outlives.

Linking the store to that buffer **after** elaboration is deliberate: the store the
parameters were evaluated against has a buffer of its own, so a **constant** function's
output disappears. That is iverilog's behaviour, and corpus `constfunc13` (constant calls)
and `mixed_width_case` (the same design at run time) print the same thirteen lines for
exactly that reason.

A **write to a design signal** goes on `StateStore::owe_fill`, the queue a `$sscanf` write
already used, and is carried out at the next instruction boundary. `FunctionDefinition`
therefore keeps `writes` beside `reads` — a written name is *in* `reads` too, since a
frame has to hold a signal to write it — and `close_reads` closes both over the call
graph, because an inner call hands its write back into the *outer* call's frame and the
outer call has to pass it on. Two calls in one expression see each other's writes:
`{ufunc(0), ufunc(0)}` has nothing between them to drain the queue, so `call` seeds its
frame through `StateStore::pending_fill` first (corpus `concat3`, `pr2842621_std`).

**Neither may reach a continuous assignment.** `propagate` re-evaluates every assignment
until the design settles, so a function called from one would print — or write — an
unpredictable number of times. Both are refused by name there, the second through the
`owes_fills` check that was already in place and the first through a length compare on the
output buffer either side of the pass. That is corpus `br948` and `concat4`, and it is the
same trade the fill rule already made: two of a line iverilog prints once is a wrong
answer wearing a working simulator's clothes.

What is left is the list a frame really would swallow, and each is still a **named error
at elaboration**: a `#delay`, a *deferred* system task, a non-blocking assignment, a
`force` or procedural `assign`, a `wait` or event control, a `disable` of a scope outside
the body, and `$random` (the stream it would advance is the frame's). A write to a design
**memory** is not on the list and does not need to be — the frame does not copy memories
in, so the body's own write is the ordinary `UnknownSignal` it would be anywhere else.

**An undriven net reads `z`; an untouched variable reads `x`.** The difference is not
cosmetic — a variable with no assignment is unknown because nothing has *said* what it
is, while a net with no driver is high-impedance because nothing is *driving* it, and a
three-state bus depends on the distinction. `StateStore::declare_net` fills with `z` and
`declare_signed` with `x`; `Port::net_type` is what picks between them, so `output reg q`
is a variable while a plain `output` is a net. A `reg` in the *body* naming a port says
the same thing and its declaration runs after the port's, overwriting the fill, so both
spellings land on `x` with no special case. An array of nets gets the same treatment
through `Memory::of_nets`.

**A `reg` behind an *aliased* port wins the fill, because it is the net's driver.**
`wire w; child u (w);` against `output reg w` inside the child is one store entry, so
only one of the two declarations can fill it — and it is the child's: the `reg` drives
that net, and what a driver has not said yet is `x` rather than `z`. A port bound to an
expression already had this (it has an entry of its own), and the alias case is
`StateStore::redeclare_as_variable`, called from `declare_port` for the header spelling
and from `declare_local` for `output w; reg w;` in the body. It keeps the width aliasing
gave the entry — the *parent's* — and changes only the fill and the net flag. Corpus
`pr1792108`, `pr1645518` and `memidx` are that rule, and iverilog 12.0 was measured for
all three spellings: `ansi=x body=x float=z`.

**A name nothing declares that is *wired to something* is a net, not an error.** That is
IEEE 1364-2005 §4.5, and `Elaborator::declare_implicit_nets` is where it happens: the
three places are a module instance's port connection, a gate or primitive terminal, and
the left hand side of a continuous assignment — `assign w = 1'b1;` with no `wire w;` above
it declares `w`. The net is one bit wide whatever it is wired to, which is iverilog's
answer too (it warns about the width and pads), and it is declared under exactly what
`Scope::resolve` answers for the name, because that is what the build pass asks for a
moment later. The pass runs *after* every explicit declaration — only the names nothing
declared are left by then — and *before* the build pass, which is what looks them up.

It reaches inside a connection **expression**, because iverilog does: `.a(yy + 1)` for an
undeclared `yy` reads `z` rather than refusing to elaborate. What it deliberately does not
reach is a name under a *select* (`not g (bus[0], a);`), a called function's name, or a
`$name` — an implicit net is scalar, so standing one in for a missing `wire [7:0]` would
turn a forgotten declaration into a silent out-of-range read. That is the whole of
`operand_names`. Worth +7 closure on its own, and the reason `` `default_nettype `` sitting
in `IGNORED_DIRECTIVES` is now a real gap rather than a harmless one: a file that asks for
`none` gets implicit nets anyway where iverilog would report the undeclared name.

**`supply0`/`supply1` and `tri0`/`tri1` drive themselves.** They are held as
`Elaborated::pulled_nets` and seeded as one more `Contribution` on every propagation pass
— a `supply` at `supply` strength, a `tri0`/`tri1` at `pull` — rather than as a value
written into the store, because a permanent driver is exactly what they are. The strength
machinery then decides between them and everything else with no second rule: `tri0 c;
assign c = d;` reads `0` while `d` is `z` and `1` once `d` is `1`, purely because `strong`
outranks `pull`. A pulled net that is also a *port bound to a parent signal* has no entry
of its own, so the pull is recorded against the entry it aliases — getting that wrong
costs the design its elaboration rather than just its answer.

**A signal can have more than one source, and `StateStore` says which one wins.**
`assign v = e;` and `force v = e;` written *inside* a procedural block install a continuous
drive that outlives the statement, `deassign` and `release` take it away again, and the
rule is `force` beats procedural `assign` beats an ordinary write. That is `DriveLevel`,
and `exec::held_bits` is where it is enforced — asked by `exec::drive_at`, which
every assignment in the simulator goes through. A write that loses is **discarded**, not
applied and overwritten a moment later: that is what keeps it out of the change journal,
and so out of the edges that wake blocks. A design that forces nothing pays a
`Vec::is_empty` for the question, the same shape `any_signed` and `any_memory` use.

**Precedence is per *bit*, not per signal name.** `force r[1] = 1;` holds one bit and
leaves the rest writable, so a whole-signal write over a partly forced signal is
**masked** rather than refused: `r = 4'b1100` on a `reg [3:0]` forced at bit 1 leaves
`1110`, and a bit select write drops only the held indices from the write rather than
losing all of it. `held_bits` resolves a drive's target to bits — which is why it needs
the store — and only does so when a drive is actually installed.

The drives live on the store because a running procedural block is handed nothing else,
and they are re-evaluated by `Simulator::propagate` alongside the module's own `assign`
statements — one fixpoint, not two, which is what makes a forced signal *follow* its
expression when an operand moves. `propagate` holds them through `StateStore::drives`, an
`Rc` handle, so the list is still in the store while it is being written through: an
`assign` underneath a `force` has to be able to see the force to know its own write goes
nowhere.

**A drive is recorded per *target*, not per signal name.** `force bus[0] = 1;` and
`force bus[3:2] = 2'b11;` hold different parts of one vector and both stand at once, and
`release bus[0];` leaves the `[3:2]` force standing — which is the same per-bit rule
`held_bits` already answered writes with, applied to the drive list itself. Keying the
list by signal name instead made the second `force` silently evict the first and the
first `release` take both away (corpus `force_release_reg_pv`, `force_release_wire_pv`,
`force_release_wire8_pv`, `assign_deassign_pv`). A new drive goes on the **end**, so
where two do overlap the later one is applied last and wins, which falls out of the order
rather than needing a rule of its own. `exec::remove_drives_over` is the release half: it
resolves each drive's target and drops the ones whose bits the release names, which is
why the decision is made in `exec` and applied by `StateStore::retain_drives` — resolving
a target needs the evaluator, and the store has no evaluator.

**A concatenation is a drive target like any other, and the drive holds every signal it
names.** `assign {a, b, c, d} = 4'h2;` written inside a block is one drive under four
names (`Drive::names` / `Drive::covers`), because the precedence rule is asked *per
signal* — a write to `b` has to find it. Installing it on the first part and losing the
rest is the wrong answer that the old named error was there to avoid; `ResolvedTarget::Parts`
already split the value, so the only thing missing was somewhere for the drive to live.
`exec::held_by` is the per-part half of `held_bits` and `exec::target_bits` /
`released_covers` the per-part half of the release rule, so `deassign {a, b, c, d};` takes
the drive its `assign` put in and a release of one part of a wider one still leaves it
standing. Corpus `assign3.2D`, `assign3.2E`, measured against iverilog 12.0.

A concatenation is also a target inside a **function** body — `{swap[3:0], swap[7:4]} =
{hi, lo};` over the function's own result variable is corpus `constfunc14` — so
`elaborate::assigned_names` asks each part for the signal it writes where `assigned_name`
asked the target for one.

**A `release` puts nothing back**, and the asymmetry that follows is the whole rule: a
**net** reverts because its continuous drivers reach it again on the next pass, while a
**variable** has no driver and so keeps the value the force left it holding. A net is
therefore *floated* at the release (`exec::floats_when_released`): `propagate` re-asserts
every continuous driver in the same settle round, so a driven net is restored before
anything reads the `z` and the change journal shows no edge — while a net **nothing**
drives really does read `z`, which leaving the forced value standing could not say
(corpus `pr1735836`). "On the next
pass" is load-bearing and is a known gap (#235): iverilog re-resolves the net at the
`release` itself, so a design that reads the net in the *same* timestep sees the driver's
value where this reads the value the force left (corpus `pr1832097a`). A releasing
`reg` forced to `1010` stays `1010`; a releasing `wire` returns to its assignment. That
is what iverilog does, and it is why there is no "displaced value" recorded anywhere — a
`release` simply removes the drive, unless a procedural `assign` is still installed
underneath, in which case that one takes over.

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

Still unsupported: a hierarchical enable (`instance.task(…)`); a task enabled from inside a
`function`, which is rejected by the function body analysis rather than by a check of its
own. `signals.rs` is built but still unwired.

**A `disable` is a jump when it can be, and a cancellation when it cannot.** A named block
and an inlined task body each occupy a *range* of the compiled instruction list, and
`Program::scopes` records it — which is all "terminate that scope" needs, because the LRM
says execution continues with the statement following the block and a resume point here is
a program counter. So `disable body;` written *inside* `body` is `pc = end`: the early
return from a task, and the common case in the corpus. Nothing else about it is special —
a `disable` inside a `for` inside the block leaves the whole block, because the jump is out
of the range rather than out of a loop.

The name is resolved against the enclosing scopes at **compile** time, innermost first
(`enclosing_scope`), so `disable wait_loop` written in task `t` means `t.wait_loop` and the
same word at the top of a module means `wait_loop`. A name matching no enclosing scope is
left bare and looked for among every block's scopes when it runs. Both the scope table and
the `Instruction::Disable` go through `Program::rename_scopes` — beside the instruction
rename, not inside it, because a block label is not a signal and must not travel through a
map of a task's locals.

A `disable` of a scope the block is **not** inside cannot be a jump, because the block that
is inside it is suspended somewhere only the driver can reach. That is `Resume::Disabled`,
and `Simulator::cancel_scope` answers it: every cursor in the `EventQueue` and every entry
in `waiting` whose program counter falls in that scope's range is taken out and re-queued
at the scope's `end`, *at the current time*. Re-queueing rather than resuming inline is
what puts the cancelled block's remaining output after the output of the block that
disabled it, which is where iverilog puts it. A free-running `always` whose whole body was
the disabled block then reaches its `Halt` and restarts — which is exactly how a design
writes a restartable thread (corpus `pr987`), and it falls out of the rule
`resume_block` already had rather than needing one of its own.

Two things about it are deliberate. Disabling a scope that **exists but is not running**
anywhere is a no-op, because that is what the LRM asks for: `always #6 disable foo;` cancels
whichever enable of `foo` happens to be in flight and says nothing about the times none is.
Disabling a scope the design has **nowhere** is `SimulationError::UnknownScope` — a design
that thought it cancelled something and did not is the hardest kind of wrong answer to
find. Inside a `function` the second rule is checked at elaboration instead
(`Program::nonlocal_disable`), since a frame has no driver behind it to cancel anything
with.

**An edge is a property of the least significant bit of the *triggering expression*, so
a sensitivity entry that names a select is asked about those bits.** `always @(posedge
clear[i])` — one block per bit, which is how a generate loop writes an asynchronous clear
— is a `posedge` of bit `i` and nothing to do with bit 0 of the whole vector: `clear`
moving `0000 -> 0010` answers `yes` for `clear[1]` and `no` for `clear`. `events::narrowed`
is that question, and it takes the `&StateStore` `control_fires` now carries because both
halves of the answer live there — the index (a substituted genvar is a constant, but a
parameter or a signal is not) and the *declared range* the index has to be mapped through.
A bit select and a part select are what it narrows; anything else — `posedge (a & b)`, an
index that is not a number — keeps the over-approximating whole-signal reading
`event_fires` has always documented. Corpus `pr1623097` and `automatic_events3`.

**A block is sensitive only while it is *parked* at its event control, so a write it
makes on its way through cannot wake it.** The block was not listening when the event
happened, and by the time it comes back the event is in the past — which is why
`always @(a or c) begin a = ~a; end` runs once per *external* change rather than for ever,
and why corpus `event_list3`'s combinational block prints one pair of lines and not two.
`TimedBlock::writes` is the set of names the body assigns, computed once at elaboration
beside `implicit_reads`, and `Simulator::ran_before` / `ran_now` — one flag per block,
swapped at the top of every settle round — say whether the block was running when they
moved. An edge on a name in both is dropped from what that block is offered. It is the
same rule `EventWatch`'s snapshot already gave a mid-block `@`, applied to the whole
block's own arming point.

Two things about it are deliberate. The set is **static**, so a signal the block *could*
write is suppressed whether or not it did — which matches the LRM in the direction that
matters, since anything moving while the block ran is missed anyway. And it is cleared
after one round, because a write journalled in round N becomes an edge in round N+1 and
nowhere else. An `@(*)` block gets an empty set: its read set already leaves its own
targets out.

**A block can suspend on the design as well as on the clock, and `Resume::Waiting` is
how.** `wait (c) S` and `@(posedge clk) S` are both suspensions that no timestamp brings
back, so they are not on the `EventQueue` at all: `Simulator::waiting` holds them, and
every settle round offers each one what just moved. The two are resumed differently on
purpose. A **condition** is a value that is still there, so the block is simply re-entered
and its own `Instruction::Wait` re-evaluates it — which is also why a `wait` on something
already true costs one evaluation and no suspension at all. An **edge** is not: it is gone
by the time anything could look for it, so `EventWatch` snapshots the signals the control
names *at the moment the wait is armed* and asks on each round whether they have moved
since.

That snapshot is the whole reason the arming moment means anything. A settle round sees
everything the timestep moved, including what the waiting block itself wrote on its way to
the wait — `clk = 0; @(negedge clk) …` would otherwise be woken by its own write (corpus
`dff1`). A named event has no value to snapshot, so a trigger is matched the other way, out
of the round's own trigger journal, which is why `settle` keeps `trigger_edges` separate
from the rest of its edges rather than folding them in.

Two consequences worth keeping straight. An `always` block part way through a wait is
**not** started again by `settle`: it has not finished the run it is on, and a second copy
of it would give the design two writers of everything it assigns. And a *free-running*
block that halts restarts immediately — that rule lives in `Simulator::resume_block` rather
than in `advance`, because `always value = @(ev) 5;` finishes inside a settle round and has
to arm itself again there (corpus `always3.1.1I`, `br991a`).

**A named block is a scope, and its variables are dotted names like a task's.**
`begin : blk reg [7:0] tmp; … end` declares `blk.tmp` in the flat store — `dut.blk.tmp`
inside an instance, `first.inner.i` for a block inside a block — so a local that shadows a
design signal is a second entry rather than a write to the first. `elaborate` declares them
by walking the statement tree (`declare_block_locals`) and `Program::compile_block` renames
exactly the instructions the body compiled to (`Program::rename_range`), which is the same
division of labour `declare_tasks` and `compile_task` already had. An **unnamed** block is
grouping and nothing else, so `parse_block` flattens it into the statements it holds and
nothing downstream learns it was written.

**`fork`/`join` is one thread per branch, and the join resumes at the *maximum* of their
finish times.** `Instruction::Fork { branches, join }` names the instruction each branch
starts at and the one the block carries on at, and the branch bodies are laid out between
the two, each ending in an `Instruction::JoinBranch`. So a branch is an ordinary run of the
same instruction list, and a `#delay`, a `wait`, an event control or a nested `fork` inside
one needs nothing new: a branch is a program counter like any other.

The driver owns the threads, because it owns the queue they go on. `Resume::Forked` hands
the branch entry points out; `Simulator::settled_resume` opens a `ForkJoin` record — where
the block picks up, and how many branches have still to arrive — and queues one
`ExecutionCursor::branch` per branch *at the current time*. `ExecutionCursor` therefore
carries a `fork: Option<usize>`, which is the identity a branch signals its arrival with;
`Resume::BranchDone` decrements, and the last arrival re-queues the parent, again at the
current time. That is what makes the join the maximum rather than the sum — each branch
arrives at whatever instant its own delays took it to, and the one that arrives last is by
definition the latest.

**A `fork` nested inside a branch needs no second mechanism**, because the record holds a
whole *cursor* rather than a program counter: the inner join's parent is the branch cursor,
`fork` field and all, so arriving at the inner join re-queues a thread that is still a
branch of the outer one.

**Branches that consume no time are still compiled as a plain block**, and that is a
deliberate two-pass shape rather than a leftover. Such branches cannot tell sequential from
concurrent — each runs to completion without giving another a turn either way — so
`compile_fork` compiles the block first, asks whether what came out contains anything that
suspends, and only then throws it away (`Program::mark` / `rewind`) and compiles it again as
threads. That keeps the common case free of the driver round trip a thread costs, and it
keeps a `fork` legal inside a `function` and inside `exec::execute_statements`, neither of
which has a driver to spawn anything (both report `FORK_TIMING_UNSUPPORTED` if one reaches
them). Asking the *instructions* rather than the statements is what makes a branch that
suspends inside an enabled task's body count — the body is already spliced in by then.

Three seams keep it consistent with what was already there. `Program::splice` offsets a
`Fork`'s branch targets like any other jump, so a `fork` inside a task works. `settle` skips
a block that `is_forking`, for the same reason it skips one part way through a `wait` — it
has not finished the run it is on. And `Simulator::cancel_scope` is **block oriented**: a
`disable` naming a scope with a running `fork` in it has to cancel every branch and the
block parked at the join, so it collects the *blocks* with a thread inside the scope, drops
every cursor of each — queued, waiting, and the fork records, which are the only thing
holding a parent at a join — and re-queues one cursor at the scope's end. A block has one
activation at a time, which is what makes "every cursor of that block" the right set.

`join_any` and `join_none` are not implemented, and `block_between` closing on a
word-boundary `keyword` rather than a bare `tag` is what keeps them out: without it,
`fork … join_any` would read as a plain `join` with a stray `_any` after it and hand the
design semantics it did not ask for. They are a parse error instead. A `disable` written
inside a branch naming a scope the `fork` itself sits in is `Unsupported` by name
(`Program::check_fork_disables`, a post-pass because the enclosing block's range is not
recorded until the block around the `fork` has finished compiling): the jump a local
`disable` compiles to would stop the one thread that ran it and leave the join waiting for
an arrival that can never come. A branch that genuinely never arrives holds *its own* join
for ever, which is what the LRM says, and holds nothing else — the other branches still run
and time still moves.

**An intra-assignment timing control holds its right hand side in the store.**
`a = #5 b;`, `a = @(posedge clk) b;` and `a = repeat (3) @(ev) b;` all evaluate `b` *now*
and write it when the control expires, which is what tells them from `#5 a = b;`. The held
value cannot live in the resume point — that is only a program counter — so
`Instruction::Hold` puts it in `StateStore::holds` under a hidden `$hold$<index>` slot and
`Instruction::WriteHeld` takes it back out. It is deliberately not a signal: nothing in the
design can name one, so journalling it would only manufacture edges. The `repeat` form is
the ordinary `repeat` loop with the wait as its whole body, so a count of zero writes
immediately.

A **non-blocking** assignment with one of these is not one of them: `a <= #5 b;`
schedules its write and lets the block carry straight on, where everything above suspends
the block. `Instruction::ScheduleWrite` is that shape, and `Simulator::scheduled` holds the
write until its time.

**A scheduled write lands at the *end* of the timestamp it is due at, not at the top of
it**, which is the one thing that tells it from a delayed `assign`'s transaction. It is a
*non-blocking* write, so it belongs in the same place in a timestep as an ordinary `<=` —
`Simulator::land_due_writes` therefore sits beside `commit_updates`, after every block that
runs at that instant, where `land_due_drives` stays at the top because a continuous driver
really has changed the net by then. Measured: iverilog 12.0 prints `00` for **both**
`$display`s at time 2 of

```verilog
initial begin a = 0; a <= #2 8'haa; #2; $display("%h", a); end
initial begin #2; $display("%h", a); end
```

and `aa` only at time 3 (corpus `patch1268`). Landing it first instead lets a block read a
value that has not been written yet. It goes ahead of the timestamp's own non-blocking
updates because it was scheduled earlier, which is the order IEEE 1364-2005 §11.4 puts two
updates in.

**`@*` in front of a statement is not the same list as `@*` in front of a block.** It is
sensitive to what *that statement* reads, so `@* a = c;` is `@(c)` even inside an
`always @*` that is sensitive to `b` and `c` (corpus `nested_impl_event1`).
`Program::emit_event_wait` therefore resolves it into the explicit list it stands for while
the statement is still in hand, and everything past that point sees one kind of event
control instead of two. An `@*` with no statement to read — `a = @* b;` — is a named error.

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

**A driver list is keyed by the net, and a word of an array of nets *is* a net.**
`wire [1:0] foo [0:1];` with two strength-bearing drivers on `foo[0]` and two more on
`foo[1]` has four drivers and two driver lists, so `resolve_contributions` groups by the
name **and** `word_address` — `Some(index)` for a `ResolvedTarget::Word`, `None` for
everything else. Pooling the words into one list resolves `foo[0]` against `foo[1]`, and
skipping the word targets entirely leaves the design asking the *signal* map for a name
only the memory map has, which is `UnknownSignal` naming the array (corpus `pr1703346`).
Writing each word as it came is the third wrong answer and the quietest: iverilog gives
`0x` and `1z` for two drivers that disagree under `highz` halves, where the last write
gives `00` and `11`.

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

**So is a net more than one continuous assignment drives, and that one is not an
improvement but a *termination* rule.** Two plain `assign`s written onto one net used to
be two ordinary writes, and two writes of different values do not settle: each pass undoes
the one before it until `propagate` runs out of passes, so the design fails with
`NoConvergence` rather than with an answer — `assign blend = foo; assign blend = bar;` is
corpus `pr1701921` and six more. `elaborate::resolve_multiply_driven_nets` counts the
assignments by target name once the whole hierarchy has been walked and marks any name
with two, after which `resolve_bit` gives what iverilog gives: the bits the drivers agree
on pass through, the ones they do not are `x`. The count has to be taken at the **end**
rather than per push, because one driver may be an instance's `Binding::Driving` and the
other the parent's own `assign`.

**`assign #10 a = b;` is a real delay, and the trick is that it changes *which* value the
driver asserts, not whether it asserts one.** A delayed assignment is still a continuous
driver on the same `propagate` fixpoint as every other one — it just contributes the value
its right hand side had `#n` ago rather than the value it has now. That is
`runner::DelayedDrive`: `applied`, what the assignment is driving at this instant, and
`pending`, the value in flight with the time it lands. `propagate` evaluates the right hand
side as it always did, puts it in flight if it differs from where the driver is *headed*
(`destination` — the pending value if there is one, otherwise the applied one), and then
contributes `applied` exactly as an undelayed assignment contributes its expression. So
nothing about strength resolution, three-state buses or the change journal had to learn
about delay at all.

The delay is **inertial**, not transport, which is the one rule that had to be measured
rather than assumed (iverilog 12.0): a new value *replaces* what is in flight instead of
queueing behind it, so a pulse shorter than the delay never reaches the net. `assign #10`
against a five-unit pulse produces no transition whatsoever.

**A `delay3` keeps all three of its delays, and which one applies is decided by the value
being driven *to*.** `#(rise, fall, turn_off)` is `parsers::delay::GateDelay`: a bit going
to `1` takes `rise`, to `0` takes `fall`, to `z` takes `turn_off`, and to `x` takes the
*shortest* of the three, since `x` is "it could already be any of these". An omitted
`turn_off` is the shorter of `rise` and `fall` rather than no delay at all. Keeping only
the first — which is what the parser used to do — gives every falling edge the rise delay,
which is a wrong answer wearing a working simulator's clothes.

For a value more than one bit wide the transaction is **one** transaction at the
**longest** of the delays the changed bits ask for, with the unchanged bits asking for
nothing and no intermediate value in between. That was measured rather than reasoned:
`assign #(6, 2) o = i;` on a `[3:0]` carries `1100 -> 0011` — two bits falling, two
rising — in 6, and `1111 -> 1100` in 2. `GateDelay::ticks_between` is the one place the
rule lives, and both the assignment and the gate loop read it, so the two cannot disagree.

Two seams make it work. `Simulator::next_time` is the time wheel: it is the earlier of the
`EventQueue`'s next cursor and the earliest pending transaction, so a timestamp at which
*only* a net changes still gets a turn. `Simulator::land_due_drives` runs at the top of
that timestamp, before any block resumes, so a block scheduled for the same instant reads
the net as it is at that instant. Both cost nothing for a design with no delayed
assignment: `delays` is an empty `Vec` and neither loop has anything to iterate.

**A delayed net reads `x` before its first transaction, not `z`.** Something *is* driving
it — it simply has not said what yet — which is why `setup` runs one `propagate` pass ahead
of time zero when the design has any delayed assignment, and why `applied: None` renders as
`Register::unknown` rather than contributing nothing. Getting this wrong shows up
immediately: `$display` at time zero prints `z` where iverilog prints `x`.

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
terminal that is neither is `SimulationError::GateArrayTerminal` rather than a silent
misconnection. A part select brings its own bounds (`tran t [1:0] (a, c[1:0]);`, corpus
`pr3296466d`); both the array and the terminal are walked from their least significant end,
so which way round either range was declared cannot matter.

**How a terminal is sliced depends on whether it names storage.** A plain signal or a part
select of one becomes a `BitSelect`, which is both readable and writable and so is what an
*output* terminal needs. Anything else of the right width — `{16'b0, regff}` is the corpus
case (`bufif`, `npmos2`, `rnpmos2`) — names no storage, so a bit of it is not something a
`BitSelect` can name and it is read by a **shift** instead: `terminal >> position`, whose
least significant bit is the one that instance wants. That is all one can ask of an
expression and all an input terminal needs; a design that puts such a terminal in an
*output* position fails exactly where a non-arrayed gate with the same output already
fails, resolving it as a write target, by name.

**A gate delay is simulated, on the machinery a delayed `assign` already had.** `Gate`
carries the `#(rise, fall, turn_off)` it was written with and `Simulator::gate_delays`
holds one `DelayedDrive` per gate beside the one per assignment, so the delay changes
*which* value the gate asserts rather than whether it asserts one — and nothing about
strength resolution, three-state buses or the change journal had to learn about it. A
design whose gates name no delay keeps an empty vector and the loop asks nothing per
pass, the same shape the assignment delays use.

**A bidirectional switch is not a driver at all — it makes its two terminals one node.**
`tran`, `rtran`, `tranif0`, `tranif1`, `rtranif0` and `rtranif1` conduct both ways and have
no output terminal, so they elaborate to a `gates::PassSwitch` rather than to a `Gate`:
`GateKind::is_bidirectional` is the one question `elaborate::push_primitive` asks, and both
terminals go into `resolved_nets` so that every driver of either one reaches
`resolve_contributions` instead of writing the store. A node's value is then what every
driver of every net in it resolves to *together* — the same `resolve_bit`, over a pooled
driver list — so the switch changes which contributions are pooled and **nothing about the
value rule**.

Copying a value from one terminal to the other is the shape that looks right and is wrong,
and it is why this was left undone for so long: once `a` has been copied to `b`, a driver on
`a` letting go leaves `b` holding the stale value, which copies straight back, and the net
never floats again. `runner::bond_nodes` is the node model instead, and two things make it
work. A terminal is **seeded with `z`** before anything is merged, so a net a `tranif` has
just let go of has no live driver rather than the value it held — without the seed, "a bit
no driver reaches keeps what it held" *is* the latch. And the partition is over **bits**
rather than nets, because `tran (a[0], a[1]);` joins two bits of one vector (corpus
`pr3296466a`).

**Connectivity is recomputed every propagation pass, not partitioned once at
elaboration.** A union-find built at elaboration is right for `tran`/`rtran` and silently
wrong for the four `if` forms, whose control is an ordinary expression that moves during the
run — corpus `tran-keeper` gates a switch on the very net the switch is holding up, and
settles because the fixpoint re-asks. `Simulator::switch_bits` is that question, asked once
per pass and only when `pass_switches` is non-empty; a design with no `tran` in it pays one
`Vec::is_empty`.

A **`force` or procedural `assign` on a resolved net is a driver of it**, so it is
contributed into the pool beside the module's own drivers. On the net it names it wins
either way — `exec::held_bits` discards the resolved write over a held bit — but through a
`tran` it has to *contend* with the far side: iverilog 12.0 answers `assign y = a;
tran (x, y); force x = 0;` with `a` at 1 as `x=0 y=x` (corpus `pr2937417b`, `pr2937417c`).
Outside a node the contribution changes nothing, since the bit it lands on is discarded.

Three things about a switch are **not** modelled, and each is measured rather than guessed:

- **A control that is `x` or `z` is taken not to conduct.** iverilog conducts at an
  *ambiguous* strength instead, which gives the far side an `x` while leaving the driven
  side alone: `assign p = a; tranif1 (p, q, en);` with `a` at 1 and `en` unknown is
  `p=1 q=x` there and `p=1 q=z` here. Saying "unknown" needs a strength that is a *range*
  rather than a level, which `resolve_bit` has no shape for.
- **A switch delay changes nothing.** `tranif0 #(100) sw(gnd, net1, gnd);` delays the moment
  the switch opens or closes rather than a value, which is not the `DelayedDrive` machinery
  an `assign` or a gate uses. Corpus `pr3499807` is exactly that and fails honestly.
- **Strength reduction and strength propagation are still absent.** The `r`-prefixed forms
  pass the same values as their non-resistive counterparts, and nothing carries a strength
  *through* a switch: corpus `resolv1` needs a `pmos` to carry a `pullup`'s `pull` strength
  to its output, which — like `%v` printing `Pu1` rather than `St1` — would mean the store
  carrying a strength per bit beside its value. That is what the seven `%v` gold files
  (`tran`, `tranif0`, `tranif1`, `rtran`, `rtranif0`, `rtranif1`, `switch_primitives`) are
  blocked on; they run and mismatch rather than failing to elaborate.

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
creates, the *labels* of the generate blocks nested in it, and the *labels* of the named
`begin : blk` blocks its `initial` and `always` bodies open, because a hierarchical
reference reaches through all four.

That last one is `block_labels`, and it is load-bearing rather than tidy: `elaborate`
declares a named block's variables under `scope.qualified`, which inside a generate block
is the *block's* prefix (`genblk1.a.i`), while a reference to one resolves through
`Scope::resolve`, which without the label sends it outwards to the module (`a.i`) and
finds nothing. The walk stops at the first named block on each path, since that label is
the head segment every name inside it is keyed by — one nested deeper is reached through
it. Corpus `pr2306259` is exactly this.

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

**A module may instantiate itself, and a `generate` condition is what has to stop it.**
IEEE 1364-2005 allows recursive instantiation, which is how a design writes a tree or a
chain of a parameterised depth — `sum #(n/2, width)` beside `sum #(n-n/2, width)` under an
`if (n == 1)` (corpus `pr2728812a`). So "this module is already on the path from the top"
is not the question and was the wrong one to ask: the question is whether the recursion
*terminates*, and two bounds answer it without evaluating the generate conditions twice.
`MAX_INSTANTIATION_DEPTH` counts repeats of one module on the path — deliberately small,
because `walk` recurses on the host's own stack and a generous bound is a stack overflow
rather than an error — and `MAX_INSTANCES` counts the instances a design elaborates to at
all, which is what bounds a recursion that *branches*: one that terminates at depth twenty
and instantiates itself twice is 2²⁰ real instances. Either is
`SimulationError::RecursiveInstantiation` naming the module.

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

**An escaped identifier keeps its backslash unless a simple identifier could have spelled
it**, and that is what keeps it one *segment* of that dotted name space. IEEE 1364 §3.7.1
says the backslash and the terminating whitespace are not part of the name, so `\a ` and
`a` really are one object (corpus `escape3` asserts it of `\cpu3 `, `cpu3`, `top.\cpu3 `
and `\top .cpu3` alike) — but dropping it unconditionally gives `reg \bot.r ;` in the top
module and `reg r;` inside an instance called `bot` the *same* store key, and iverilog 12.0
keeps them apart (corpus `escape4`, `escape4b`). `identifier::is_simple_identifier` is the
question, spelled out beside `simple_identifier` so the two cannot disagree, and a name
that answers yes still collapses — so nothing downstream sees a backslash it did not see
before, and a name like `odd*name$` that has no unescaped spelling at all loses nothing by
keeping one.

**An output bound to a select is the alias run backwards.** `.y(bus[i])` — how a generate
loop wires an instance per bit — cannot be aliased, because the port and `bus[i]` are not
one store entry. The port keeps a signal of its own and a continuous assignment carries
it *out* to the bit, which is `Binding::Driving` against `Binding::Driven`'s inward
direction. An output bound to something that cannot be written at all (a concatenation,
`a + 1`) is still `UndrivablePort`, because there is nowhere for the child's value to go.

**An `inout` bound to a select is *bonded*, which is neither direction of the alias.**
A port that is read as well as written cannot be carried by an assignment, because one
assignment only runs one way — and copying the value across is the shape that looks right
and is wrong for the reason a `tran` is not a copy: once `bus[0]` has been copied into the
port, a driver letting go leaves the far side holding the stale value. So `Binding::Bonded`
gives the port a signal of its own and `Elaborator::bond_port` joins each of its bits to
the matching bit of the connection with a `PassSwitch`, which makes the two **one node** —
their drivers are pooled and resolved together by the machinery `tran` already needed.
Both ends go into `resolved_nets` for the reason a switch's terminals do.

`Elaborator::bit_expressions` is how the bits are enumerated, and it goes through
`resolve_target`, so a name, a select and a concatenation of those (`.T({qh, Q})`) are all
answered by the production that already decides which bits an assignment writes — the two
cannot disagree about which bit of `{qh, Q}` is which. Both sides are walked from their
**least significant** end, so a connection narrower than the port leaves the port's high
bits joined to nothing, which is what an unconnected bit of a net already is. Corpus
`inout2`, `inout3`, `inout4`, `br918c`, `br965`, `pr1444055`, `pr1478121`, `pr2219441`,
`pr3296466b`, `tri2`.

Still not modelled: a generate loop bound that reads a *signal* evaluates it rather than
refusing — an `x` runs zero iterations where iverilog reports a non-constant bound, and
`eval` cannot tell a parameter from a net through the store to say otherwise. `generate`
items written outside `generate`/`endgenerate` and a `defparam` whose path indexes
something other than a generate block are both unsupported.
**A user-defined primitive is a module with a truth table in it.** `primitive mux (q, sel,
a, b); … table … endtable endprimitive` is instantiated exactly the way a module is, so
`parsers/primitive.rs` parses one into a `VerilogModule` whose *single* statement is a
`ModuleStatement::PrimitiveTable`. The header is the same production a module's is — both
spellings, reconciled by the same `reconcile_ports` — and instantiation, the module
library, port binding and the flattening walk then work on it unchanged. Nothing else had
to learn what a UDP is: `elaborate::primitive_table` asking that one question is the whole
of the difference. A `table` is deliberately *not* one of `parse_module_statement`'s
alternatives, so there is no ordering hazard to get wrong — a table is legal only inside a
`primitive`.

A combinational UDP is then a **continuous driver** like a gate: `simulator::udp::Udp`
joins the same `propagate` fixpoint, contributes one bit at strong strength, and marks its
output as a `resolved_net`, so a UDP and a `bufif1` may drive one bus without either one
knowing. **The lookup rule was measured against iverilog 12.0, not read off the LRM**: an
input's `z` is read as an `x` before anything is matched (a table has no `z` symbol), `?`
matches `0`, `1` and `x` alike, `b` matches only the two known levels, and where rows
*disagree* — which the LRM leaves undefined — a `0` row beats a `1` row and both beat an
unmatched combination, whichever order they were written in. That last rule is what makes
a row whose output is `x` say nothing a missing row does not already say, since an
unmatched combination is `x` anyway. There is no cleverness about an unknown input: `x 0`
against `0 0 : 1` and `1 0 : 1` is `x` and not `1`, even though both substitutions agree.

**A `#(...)` on a primitive instantiation is a *delay*, not a parameter override.** The
grammar cannot tell — `#(...)` after a module name is a parameter list and that is what it
reads — so only the module being instantiated says otherwise, and `Elaborator::instantiate`
is where the question is asked (`primitive_delay`). The answer travels to `build_udp` on the
`Scope`, the way a port binding does, and from there it is the same `GateDelay` a gate
carries and the same `DelayedDrive` slot: `Simulator::udp_delays` sits beside `gate_delays`
and the propagation arm is the gate's. `BUFG #(6, 2)` therefore traces exactly what
`buf #(6, 2)` does (corpus `udp_bufg2`, `pr2829776`). A *named* override on a primitive and
a fourth delay are both named errors — a primitive has no parameters and a `delay3` has
three terms, and a delay a design wrote and the simulator dropped is a wrong answer at the
right values.

A **sequential** UDP — one whose output is a `reg`, whose rows carry a current-state field,
and whose input columns may name an edge (`(01)`, `r`, `*`) — parses and is then
`SimulationError::SequentialPrimitive`, naming it. Its rows ask about the *previous* value
of an input, and a continuous driver is handed only the present ones; a driver that quietly
answered from the levels alone would be a wrong answer wearing a working simulator's
clothes. A UDP instance also still needs an instance *name*: `p(Q, D);` — legal, and how a
UDP is often written — is a parse error, because a module instantiation's name is not
optional (corpus `pr298`, `pr3587570`).

**A `specify` block records and does not simulate, and that is the one place a no-op is the
honest reading.** A module path delay (`(A => Z) = (0.1, 0.2);`) changes only *when* a value
arrives, never what the value is, and this simulator settles every continuous driver in zero
time — so a recorded, unsimulated path produces the same values at different edge times
rather than a wrong answer. It is exactly the trade already taken for a gate delay. A design
whose *checks* are about timing does then fail honestly: corpus `specify2` prints `FAILED —
dst changed too fast`, where before it did not parse at all.

Two things inside the block are not inert and are not treated as though they were. A
`specparam` is a **constant the whole module may name**, so `elaborate` declares it beside
the parameters; one whose value is a real number is kept by the parser as the text it was
written as — a path delay has no evaluator behind it — and declared as a `real`. A
**timing check** (`$setup`, `$hold`, `$width`, …) reports a violation, which needs the same
model the paths would; one is recorded and never run, so no violation is invented and none
is claimed to have been checked. Every form is parsed *structurally* — there is no "skip to
the next `;`" fallback, so anything inside a `specify` block the grammar does not recognise
is a parse error rather than something swallowed.

**`time` is a variable and `event` is not.** `time t;` is a 64
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
the same shape as `MemoryAsValue`: the name exists, it simply is not a value.

**A `real` is sixty-four bits plus a flag saying to read them as a double**, which is
exactly the shape signedness already had. `Register` carries `real` beside `signed`, and it
is the only copy: `Register::from_f64` / `to_f64` are the two ends of it, `SignalState`
re-stamps the declared flag on every write, and — like signedness — realness is *not* part
of equality, so a store never journals an edge over how a value is read. `real r;`,
`realtime t;`, `real samples [0:3];`, `parameter real PI = 3.14;`, `function real f;`,
`input real x;` inside a function or a task, and `specparam tRise = 0.9;` all declare one;
`0.5`, `1e3` and `1.5e-3` are the literal.

**A real has no `x` and no `z`**, and that is the one place it is not like every other type
here: `StateStore::declare_real` fills with `0.0` rather than with `Register::unknown`, so
an untouched `real` reads `0.000000` where an untouched `reg` reads `x`. An unwritten word
of an array of reals reads `0.0` too. A four-state value converted to one reads its `x` and
`z` bits as `0`, which is what IEEE 1364 asks for.

**Realness travels *up* from the operands, where signedness travels down.** That asymmetry
is the whole of the model and it is deliberate: signedness has to be decided before an
operand is evaluated because it changes *how* the operand is evaluated, while realness
changes only what is done with the result — so `eval_binary` reads it off the two values it
was handed. One real operand makes the operation real (`7/2.0` is 3.5 where `7/2` is 3) and
nothing pushes back down, which is why `7/2 + 0.5` is 3.5: an integer division and then a
real addition. It is also why `real r; reg [7:0] a, b; r = a * b;` multiplies in *eight*
bits — `ResolvedTarget::width` reports `SELF_DETERMINED` for a real target, because a real
is not a number of bits and has no width to impose.

`expression_is_real` exists for the two places that *do* have to know before evaluating,
and both are about width. A comparison sizes its operands against each other, and a real
has no width to share — `(a + b) != 254.0` for two eight bit `255`s is true because the
addition wraps in eight bits before it is converted, where widening it to sixty-four first
gives 510 (corpus `pr2918095`). And a `?:` evaluates only the arm it takes, so which arm is
real could not be read off a value: `c ? 1 : 2.5` is `1.0`, and `(c ? 1 : 2.5) / 2` is 0.5
where an integer `1` would give 0 (corpus `pr2453002`). An unknown condition has no `x` to
produce, so it gives what the arms agree on and `0.0` when they do not. Both callers ask
`StateStore::any_real` first — `false` is exact, the same shape `any_signed` and
`any_memory` use — so a design with no real in it never walks anything and `bench eval`
does not move.

**A conversion happens where the value meets the target, in `exec::drive_at`**, before the
resize that would destroy it, and while the value still carries its own signedness — `-1`
in eight bits is `-1.0` and not `255.0`. An assignment **rounds half away from zero** (`i =
1.5;` is 2, `i = -1.5;` is -2) where `$rtoi` **truncates** toward zero (`$rtoi(2.7)` is 2),
and `$itor` rounds because its argument is an integer (`$itor(10.5)` is 11.0). The
conversion is made at the *target's* width, so a `reg [64:0]` holds `2**64` (corpus
`pr2913404`), and an infinity or a NaN converts to `x` — the only four-state answer for a
value with no whole number in it. It also happens *before* a `ResolvedTarget::Parts` is
split, because a concatenation is a run of bits however its parts were declared:
`{a, b} = 2.5;` splits the integer 3, where slicing the IEEE-754 encoding would put a piece
of an exponent in each part. `$realtime`, `$realtobits` and `$bitstoreal` round out the
set; `$bits` of a real reports **64**, where iverilog reports 1, because sixty-four is what
`$realtobits` hands back.

**An operator that reads a pattern of bits is refused by name.** `& | ^ ~^`, the shifts,
`===`/`!==`, `~` and the reductions are all `EvalError::RealOperand` naming the operator,
because iverilog rejects each of them at compile time and converting to an integer behind
the design's back is the kind of wrong answer that looks right. `%` is the exception: the
LRM leaves it illegal and iverilog computes `fmod`, which is what corpus
`mixed_type_div_mod` asserts, so that is what it does.

**The real math library is the whole IEEE 1364-2005 set, and it lives in two constants.**
`REAL_MATH_UNARY` (`$sqrt`, `$ln`, `$log10`, `$exp`, `$floor`, `$ceil`, iverilog's `$fabs`,
and the trigonometric and hyperbolic family `$sin`…`$atanh`) and `REAL_MATH_BINARY`
(`$pow`, `$atan2`, `$hypot`) are each its `f64` counterpart with the argument converted on
the way in — `$sqrt(9)` is `3.0`, because an integer operand of a real function is a real.
They are constants rather than a `|` chain in the evaluator because **three** places have
to agree about the list and only one of them computes anything: the arm that evaluates the
call, `SYSTEM_FUNCTIONS`, which decides whether the name means anything at all, and
`expression_is_real`, which has to answer *before* the call is evaluated. Missing from that
third one is what makes `$floor(200000.0*$sin(cc*0.81)+0.5)` size its operands as bit
vectors (corpus `pr2152011`), and it is a wrong answer rather than an error.

**Formatting is C's.** `%f` is six decimals, `%e` a mantissa and a two digit exponent, `%g`
six significant figures with the trailing zeros dropped, `%E`/`%G` the same in capitals, and
`%5.2f` is the precision field — which only these three read. An argument printed with *no*
specifier is C's `%#g`: six significant figures with the trailing zeros **kept**, so
`$display(1.5)` is `1.50000` and `400.0` is `400.000`. A real in a radix is rounded to a
whole number and printed as narrowly as it goes, and decimal renders the *number* while
every other base renders the sixty-four bit two's complement integer — `%0d` of `-0.4` is
`-0` and `%0x` of it is `0` (corpus `br1029a`). `%s` of a real is an error: there are no
characters in an IEEE-754 encoding.

**`%c`, `%v` and `%m` are the three specifiers that are not about a number.** `%c` is the
low eight bits as a character. `%m` is the *hierarchical name of the scope the call sits
in* — `top`, `top.dut`, `top.dut.blk`, `top.dut.load` inside a task — and it takes no
argument at all, so it is answered before one is fetched. It is settled when the design is
elaborated and is a constant from then on, which is why it arrives in two halves: `Program`
stamps the *block* path it is already carrying as it compiles, and `elaborate` puts the
instance path in front through `Program::qualify_scopes` once it knows which instance the
block belongs to. A spliced task body is skipped there for the reason `rename_local` skips
it — it was qualified when the task was compiled, and doing it twice would prefix it twice.

**`%v` is a strength, and only two of them can be told from a value.** A `z` bit is driven
by nothing, which is `HiZ`; every other bit reports `St`, because an ordinary continuous
assignment and a gate both drive at `strong`. A `pullup`, a `tri0`/`tri1` or an
`assign (pull1, strong0)` really is weaker and this prints `St1` where iverilog prints
`Pu1` — `StateStore` keeps a *value* per signal and not a strength, so there is nothing to
read the difference from. Corpus `multi_bit_strength` is exactly that gap, and so are the
seven `%v` gold files of the switch family (`tran`, `tranif0`, `tranif1`, `rtran`,
`rtranif0`, `rtranif1`, `switch_primitives`) — which also want the *ambiguous* strengths
(`67X`, `SuH`, `StL`) the LRM gives a partly-driven node. Closing it means carrying a
strength per bit through `resolve_contributions`.

**A string is a value wherever a number is wanted.** `$display("%d", "A")` is 65:
`TaskArgument::Text` reaches a numeric format as its own bytes, eight bits a character.
A string argument is held as text rather than as an expression because a task has to try
the *format string* reading of one first, and that is the only reason the two ever needed
telling apart.
| File | Role |
| --- | --- |
| `elaborate.rs` | `elaborate` — flattens a module hierarchy into one `StateStore`, one assignment list and one block list, with qualified names and aliased ports; also owns `TimedBlock`, `rename_expression`, `resolve_range` (a declared width against the parameters in scope), the unrolling of a `generate` region and the application of a `defparam`, and the compiling of a `function` into a `FunctionDefinition` and of a `task` into a `TaskDefinition` |
| `eval.rs` | `eval(&Expression, &StateStore) -> Result<Register, EvalError>` — the four-state expression evaluator, plus `eval_sized` for an assignment's right hand side; signedness *and* width (`expression_is_signed` / `expression_width` / `operand_rule` / `widened`), realness (`expression_is_real` / `real_binary` / `real_unary`), the `$name` system functions and the `SYSTEM_FUNCTIONS` table naming them — including the reading half, `$sscanf` / `$fscanf` / `$fgets` / `$fgetc` / `$ungetc` / `$feof` / `$ftell` / `$fseek` / `$rewind` — and `call_function` for the design's own |
| `plusargs.rs` | `test` / `value` — the `+name=value` words the simulation was started with, and the conversions `$value$plusargs` reads them with |
| `scan.rs` | `scan` — the reading half of a format string, over a `Source` that is a string (`Text`) or a file's `Reader`; `Slot`, where one conversion's value goes; `END_OF_FILE` |
| `events.rs` | `edges_between` / `edges_from_changes` / `memory_edges` / `trigger_edges` / `control_fires` / `always_block_fires` / `signals_read` / `narrowed` — edge detection and sensitivity matching, including the bits a *select* in a sensitivity list names |
| `gates.rs` | `Gate` — one elaborated primitive, its terminals split into outputs and inputs; `PassSwitch`, a bidirectional switch, which joins two nets instead of driving one; `gate_output`, the four-state truth tables; and `resolve_bit`, the strength-ordered net resolution |
| `udp.rs` | `Udp` — one elaborated *user-defined* primitive instance, a continuous driver beside the gates |
| `exec.rs` | `execute_statements` / `commit_updates` — the run-to-completion entry point, plus `PendingUpdate` and the shared `drive` / `resolve_target` helpers; also `drive_at`, where drive precedence is enforced, and `install_drive` / `apply_drive` / `release_drive` / `deassign_drive` |
| `program.rs` | `Program::compile` / `resume` — statement trees flattened to jump-threaded instructions, so a block can suspend on a `#delay`, a `wait` or an event control and resume by program counter; also `FunctionDefinition::call`, which runs one of those programs against a frame, `TaskDefinition` / `Program::splice`, which inlines one into another, `Program::compile_block` / `rename_range`, which give a named block's variables their scope, `ScopeRange` / `rename_scopes` / `scope_end_containing`, which are what a `disable` jumps by, and `compile_fork` / `Instruction::Fork` / `JoinBranch`, which lay a time-consuming `fork` out as one thread per branch |
| `runner.rs` | `Simulator` — `new()` / `with_modules()` / `setup()` / `set_input()` / `poke()` / `run()` / `advance()` / `get()` / `add_search_path()` / `set_output_directory()` / `set_timescale()` / `add_plusarg()`, the driver, plus `end_of_timestep()`, the slot the deferred tasks report in, `wake_waiting()` / `EventWatch`, which resume the blocks suspended on the design rather than on the clock, `cancel_scope()`, which is `disable` reaching another block, `DelayedDrive` / `next_time()` / `land_due_drives()`, the inertial delay on a continuous assignment, `ForkJoin` / `branch_arrived()` / `is_forking()`, the join barrier a `fork` suspends on, and `switch_bits()` / `bond_nodes()`, which pool the drivers of every net a `tran` joins |
| `tasks.rs` | `TaskCall` / `TaskContext` / `Output` / `TimeFormat` — system tasks, their format strings (including the `%f`/`%e`/`%g` real conversions, `%c`, `%v` and the `%m` scope name), the buffer they print into — shared with the `StateStore`, so a function body's `$display` lands in it where it ran — the descriptor mask that decides which files a `$f…` task writes to beside it, `$sformat` / `$swrite`, which format into a register instead, the deferred `$strobe` queue and the one armed `$monitor`, the `$readmemh` / `$writememh` memory file format, and the `$dump…` family, which it resolves and hands to the `VcdDump` it owns |
| `state_store.rs` | `StateStore` — signal name → `SignalState` (value, declared range, declared signedness, declared realness and whether it was declared a net), backed by `register::Register`; memory name → `Memory`, in a second map, which is the whole bit-versus-word disambiguation; event name in a third, valueless namespace with the trigger journal `trigger_event` / `take_triggers`; plus the change journal `take_changes` / `clear_changes` drive, the memory journal `take_memory_changes`, the simulated clock `$time` reads, the `$random` stream (`next_random` over `random_from_seed`, IEEE 1364-2005's generator), the `FileTable` `$fopen` opens into together with the directory a relative write path hangs off and the search path a read is resolved through, the plus-args the two `$…plusargs` functions read, the `Reader` a read-mode descriptor holds, the fill queue (`owe_fill` / `take_fills` / `pending_fill`) a scan writes its arguments through, a `$random(seed)` writes its next seed back through, and a function hands its side effects back through, the `Output` handle a `$display` inside a function body prints into, the design's `FunctionDefinition`s, the `frame()` a call runs in, and the installed `Drive`s with the `DriveLevel` precedence rule `exec::held_bits` answers |
| `event_queue.rs` | time-ordered `EventQueue` of `ExecutionCursor`s: `insert` / `pop` / `peek_time` / `retain` / `cursors`, FIFO within one timestamp. A cursor carries the `fork` it is a branch of, if it is one |
| `signals.rs` | `Signal` trait plus `FiniteSignal` / `InfiniteSignal` test stimulus |
| `validator.rs` | `validate_module` / `gather_definitions` |
| `vcd.rs` | `VcdDump` — the value change dump: `add` resolves `$dumpvars` targets into variables, `note_changes` marks the ones the change journal says were written, `flush` writes the header, the opening block and each timestep's section, and `trimmed` / `identifier` are the iverilog-measured vector trimming and identifier alphabet |

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

**An entry's *kind* field carries the plus-args, and they are part of the test.** It is
comma separated after `normal` — `br937 normal,+string=0123456789 ivltests` — and
iverilog's own `perl-lib/RegressionList.pm` splits it the same way, handing every field
that begins with `+` to `vvp` and the rest to the compiler. `Entry::plusargs` keeps them
and `judge_with` hands them to `Simulator::add_plusarg`; a design that reads one and is
given nothing runs its checks against an option it was told it did not have, which is a
wrong answer rather than a missing feature.

**An entry's *kind* field can carry a `-f<file>` command file, and the one option any of
them holds is the default timescale.** `pr1403406a` and `pr1403406b` are the two, and
their files say only `+timescale+1ns/1ps`; `default_timescale` reads it and builds that
entry a `Preprocessor` of its own through `with_default_timescale`. The last one wins,
which is what iverilog does and what its own warning in `pr1403406b`'s gold says it does —
and that warning, a compiler diagnostic with no channel here, is the whole of why
`pr1403406b` is still a gold mismatch.

Two rules make a gold comparison mean something (`gold_lines` / `first_difference`):

- **Trailing whitespace is trimmed per line, leading whitespace is not.** Column alignment
  is exactly what a lot of these `$display` tests check.
- **A trailing newline is not a difference**, which comes free from `str::lines`.

`VCD info: dumpfile … opened for output.` is **compared like any other line.** Seven gold
files carry it, and the harness used to drop it from both sides because visilog had no
dumper. It has one now and prints that line where iverilog does, so dropping it would only
hide a design that stopped before it opened its dump file — removing the rule was worth +1
on its own (`pr1963962`, whose whole gold file is that line).

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

**`TIME_BUDGET` is what a design that never finishes is given**, and nothing else — one
that calls `$finish` stops on its own. So the cost of raising it falls entirely on the
free-running designs, and it is real: the whole corpus takes about 70 seconds at a
hundred thousand ticks against about 30 at ten thousand. It is a hundred thousand because
the corpus writes testbenches that long — `pr528` and `pr528b` clock a `` `timescale 1ps ``
design every five thousand ticks and finish at 50001 — and a design cut off mid-run scores
as a wrong answer rather than as one that was not given time.

The harness runs the corpus through `front_end`, which is `Preprocessor` + `parse_expanded`
rather than `parse_source`, because the corpus files `` `include `` one another by paths
relative to `ivtest/` and `ivtest/ivltests/`. `judge` keeps its bare
`judge(source: &str)` signature so the control tests exercise exactly the corpus
path; `judge_with` is the one that takes the configured preprocessor and the gold text.

**`VISILOG_ONLY=<name>` runs one design and shows what it printed.** The closure report
names the files that got a wrong answer but cannot say *what* they got — printing 1514
designs' output would bury the number the report exists for — so triage used to mean
hand-rolling a throwaway unit test around one design, which is slow and easy to get
subtly wrong (a corpus file with a backtick directive has to go through `Preprocessor`,
not `parse_verilog_source`). `ivtest_probe` is that probe, kept: it runs exactly the
entry named through the same `judge_with` every other entry goes through, and prints the
outcome, the output, and — for a `gold=` entry — the two side by side with the first
differing line marked.

```bash
VISILOG_ONLY=pr2835632b cargo test --test ivtest_corpus ivtest_probe -- --ignored --nocapture
```

**iverilog 12.0 is installed on this machine** (`/usr/bin/iverilog`, `/usr/bin/vvp`), so
a disputed answer is a measurement rather than a guess — write the smallest design that
isolates the question, `iverilog -o x x.v && vvp x`, and record what it printed in the
test's doc comment. Nearly every rule in this file was settled that way, and the ones
that were reasoned from the LRM instead are the ones that turned out wrong.

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
- **A delay value is an *expression*, and it is evaluated where it is waited on.**
  `#tPD`, `#(period / 2)` and `#n` for an ordinary `integer` are all Verilog, and none of
  them has a value at parse time — so `Delay` holds three `Expression`s and
  `Instruction::Delay` carries the whole `Delay` rather than a number. `resume` works it
  out against the store when the block reaches it, which is what makes `n = 3; #n …; n = 7;
  #n …;` wait 3 and then 7. A delay that evaluates to `x` is zero, which is what iverilog
  does with one. `Delay::ticks(&store)` is still the single place a delay *mode* is chosen,
  so `+mindelays`/`+maxdelays` is still a one-function change; it just takes a store now.
- **The unparenthesised form is a number or a name, and nothing more.** A delay prefixes a
  statement with only whitespace between, so a full expression parser would read `#5 a = 1;`
  as `5 a` and `#2 -> ev;` as `2 - >`. `delay_operand` is therefore a constant or a
  hierarchical identifier, and an expression is legal only inside parentheses — which is
  exactly what the LRM says. `#(2:10:17)` is the `min:typ:max` triple, tried first inside
  those parentheses because the single-value branch would match `2` and choke on the `:`.
- **A `#delay` on an `assign` and a `#delay` on a gate are both simulated, through one
  production.** `parse_gate_delay` reads the `delay3` — up to three delays rather than one,
  `#(rise, fall, turn_off)` — into a `GateDelay`, and both an `assign` and a gate schedule
  it. A fourth delay is not a `delay3` and is left unconsumed rather than quietly dropped.
- **System task names are decomposed, not enumerated.** `split_task_name` peels an optional
  `f` prefix (takes a descriptor) and an optional `b`/`h`/`o` suffix (the default radix), so
  `$display`, `$writeh`, `$fdisplayb`, `$strobeh`, `$fmonitor` and `$readmemb` all come from
  one table. The whole words are matched *first*, and there are six: `$finish` and
  `$timeformat` because `finish`'s `f` is not the prefix, `$monitoroff` because its trailing
  `f` is not one either, `$fflush` because its trailing `h` is not a radix, and `$time` and
  `$monitoron` alongside them. The `$dump…` family is matched whole as well — `$dumpflush`
  ends in an `h` that is not a radix. `$readmem` and
  `$readmemo` are consequently names nothing implements — the radix suffix is the file
  format rather than a default, so only `b` and `h` spell a task. `$sformat` and `$swrite`
  go through the same split although they take no descriptor, which is what gives
  `$swriteb` its radix for free.
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
  `program.rs`'s `case_matches` switches on it: `Exact` is **case equality** — `===`, not
  `==` — so an `x` matches an `x` and a `z` matches a `z` while still being told apart from
  each other and from a known bit, which is IEEE 1364-2005 §9.5 and what iverilog 12.0
  does (`case (3'bx11)` takes the `3'bx11` arm; `case (3'bz11)` takes the `3'bz11` arm and
  not the `3'bx11` one; `case (3'bx11)` against a lone `3'bz11` arm takes the default).
  Reading it as `==` instead — an unknown on either side never matching — is what a `case`
  whose arms enumerate `x` and `z` states catches, and it is silent everywhere else:
  corpus `case3.8D` and `always3.1.6D` were exactly that. The four-state `Register`
  comparison already answers it, so the whole of the rule is comparing at the wider of the
  two widths. The wildcard forms instead compare for *identity*
  with the don't-care bits masked out — `Register::matches_ignoring_z` / `_xz`, which read
  the don't-care mask straight off the `unknown` bit plane. A wildcard counts on **either
  side**, so a `z` in the subject is as much a don't-care as one in the label; testing only
  the label half is the easy mistake. `casez` still tells an `x` apart from a `0`.
  The `case` tag is a prefix of both keywords, so `parse_case_keyword` tries it last.
  **A subject and its labels are not yet sized against each other**, which is the same
  mutual context a comparison already gets: they should be widened to the widest of *all*
  of them, read signed only when every one of them is, and compared as reals when any one
  of them is. Today each side is self-determined and the label is truncated to the
  subject's width, so `case (3'sb100)` matches the label `4'sb0100`. Corpus
  `mixed_width_case` and `constfunc13` are that gap, and they fail honestly — they run and
  print `FAILED`. Fixing it means `Instruction::CaseSubject` knowing its arms, since the
  labels are separate `JumpIfMatch` instructions by the time `resume` sees them.
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
  An intra-assignment timing control (`a = #5 b;`, `a = @(ev) b;`) is a
  different thing and stays a field on `ProceduralAssignment`: it reads its
  right hand side *before* it waits.
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
  two instances of one module count separately. An `x` count runs zero iterations, and a
  **real** count is *rounded*, half away from zero, the way a real written into an integer
  is: `repeat (10.4)` runs ten times, `repeat (10.6)` eleven and `repeat (3.5)` four
  (iverilog 12.0). Reading the sixty-four bit encoding as a number instead asks for a count
  in the billions, which is a runaway loop rather than an answer — corpus `br967`
  failed as `NoConvergence` on a design that runs ten times.
  **The index is unique within one program and not between two**, so `elaborate` stamps
  each block's own position in the flat block list on it with `Program::tag_slots` — the
  same treatment the `$hold$` slot of an intra-assignment control gets. Two `initial`
  blocks both start at instruction zero, and sharing one counter makes `repeat (10)`
  twice into five iterations apiece: corpus `pr923` stopped halfway through its output,
  looking for all the world like a time budget. The tag goes in **front** where
  `Program::splice`'s offset goes behind (`$b5$repeat$0` against `$repeat$0$5`), because
  a shared separator at one end would let a task's loop inside block 0 and block 5's own
  loop spell the same name.
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
  and `sized_within` stops a comparison — or a `Shared` operation — of two plain signals
  from measuring anything.
  Undoing any of those costs 5–10% on `bench eval` on its own.
  **A `Shared` operation's mutual sizing measures one operand and reads the other off the
  evaluated left value**, which is what keeps it to a single walk of a single subtree per
  node; measuring both sides instead doubled `bench eval/nested_arithmetic`.
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
  walks the compiled instructions and rejects a `#delay`, a *deferred* system task (one that
  does not print where it stands — `TaskCall::prints_now` is the question), a non-blocking
  assignment, a `force` or procedural `assign`, a `wait` or event control, `$random`, and a
  `disable` naming a scope outside the body — each with a name. A `disable` of a block the
  function *is* inside is fine, because that one is a jump and needs no driver; corpus
  `disblock2` is exactly that. Every one of them is something a frame would silently swallow,
  and a call that quietly did nothing is the hardest kind of wrong answer to find. The same
  walk is what collects the **writes** a call hands back to the design and produces the read
  set a call copies into its frame, so adding a new `Instruction` means teaching `BodyNames`
  about it or a function will stop seeing what it reads.
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
- **A call may be separated from its argument list, and a call and a plain name are one
  parser.** `f1 ( f1 (1) )` and `mux ( INIT, {a1, a0})` are calls, safe for the reason a
  separated `[` is: in an operand position a name followed by a parenthesised list can
  only be a call. Paying for that skip on every operand cost **4% on `bench parse/*`**
  while `fn_call` was its own `alt` alternative ahead of `identifier`, because a name was
  then read twice — once to fail at the `(` and once to keep it. `call_or_name` reads it
  once and comes out 5% *faster* than the arrangement without the widening at all. It
  therefore sits **after** the selects in `operand_no_ws`, since it answers for any name
  at all and would otherwise leave their `[` behind.
- **An unsized literal fills the width it is written against, and only if its leading
  digit is unknown.** `'hx` compared with a 64 bit register is sixty-four `x`s and `'hz`
  sixty-four `z`s — IEEE 1364-2005 3.5.1's rule that a literal extends by its most
  significant digit — while `'h1` zero pads and `-1` sign extends like any other value.
  A **sized** literal is *not* one of these: `4'bx` was extended to its own four bits
  where it was written and is an ordinary value from then on, so `reg [63:0] p; p = 4'bx;`
  is `...000x` (measured against iverilog 12.0). `VerilogConstant::extends_with_unknown`
  is the one place the question is asked, and it asks about the **size** first, which is
  what keeps the digit scan off the path nearly every literal takes.
  It is also the one *leaf* `eval::sized_within` has to answer `true` for: widening it
  before it is evaluated is not the same as padding the value afterwards, so a comparison
  against one measures its operands where `state == 3'b010` measures nothing. Getting that
  half wrong leaves `period !== 'hx` *true* for an untouched 64 bit register (corpus
  `pr673`).

- **A based literal is three tokens.** The size, the base designator and the digits are
  separated by whitespace and comments exactly as `#` is from its delay value, so `5'h 0`
  and `5 'h0` parse. The `'` and its base letter are *one* token — `5 ' h0` is not a
  literal — which is also what the LRM says.
- **A backslash before a newline inside a string literal is a line continuation, and it
  contributes nothing.** IEEE 1364-2005 §3.6; iverilog 12.0 prints `ab` for a literal
  spelled `"a\<newline>b"`. It is `string.rs::line_continuation`, deliberately *not* one of
  `parse_escape_sequence`'s alternatives, because it produces no character rather than one
  — and it is tried before every other piece of a string's body, since they would all claim
  its backslash. Getting it wrong is silent: the backslash falls through to the ordinary
  character arm and keeps the newline with it, so the string holds two characters nothing
  wrote (corpus `string12`). This is the *opposite* rule from a continuation in a
  `` `define `` body, where the newline stays — a `//` comment there would otherwise
  swallow the rest of the macro.
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
  declaration parser and `ModuleStatement::Assignment` wraps one. The strength *and the
  delay* belong to the `assign` rather than to a target, so every target in the list shares
  them, the same way every name in a `reg [4:0] a, b;` shares one width.
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
- **A UDP table row is written without separators, so every symbol is one character.**
  `?? 0` is three fields, not two, which is why a level is a single character and an edge
  has to be bracketed (`(01)`) or one of the five shorthands (`r f p n *`). The row parser
  reads input columns until it reaches a `:`, and the shape of what follows — one field or
  two — is what says whether the row carries a current state. `e` is not a symbol, which is
  what lets `many0(table_row)` stop at `endtable`.
- **A `specify` path terminal is deliberately not a general expression.** `b *> a` would
  otherwise read as `b` multiplied by whatever follows, and what comes out is a wrong parse
  tree rather than an error. `specify.rs::terminal` is a name with an optional bit or part
  select and nothing else. The delay on the right of the `=` *is* an expression, which is
  how `= (tRise, tFall)` names two `specparam`s.
- **A real number is parsed in exactly one place.** `numbers.rs::real_number` reads both of
  IEEE 1364's spellings — fixed point (`0.9`, `0.500`) and exponent (`1e3`, `1.5e-3`) — and
  three productions come to it: `expr.rs::real_literal`, a `specify` path delay and a
  `specparam` value. It is tried *before* the integer grammar, which would otherwise read
  `0.9` as `0` and leave `.9` behind. Digits are required on **both** sides of the `.`, so
  `a[3].b` is still a hierarchical name, and an exponent with no digits after it is not one,
  so `1e` is the constant `1`.
- **An event control has five spellings and one of them is a bare identifier.**
  `@(posedge clk)`, `@(a or b)`, `@(*)`, `@*` and `@ev` all parse to an
  `EventControl`, and the bare form is tried last because it is the loosest. It
  takes an *identifier*, and `keywords::is_reserved_word` is what stops it: without
  that guard `always @* begin … end` reads `begin` as the event it waits on. This is
  the mirror image of the task-enable trap — the same helper, the other way round.
- **An `always` or `initial` body is exactly one statement.** Both go through
  `behavior.rs::statement_body`, the same production a conditional arm uses — a
  `begin`…`end` block, a null statement, or a single statement. Reading a *run* of
  statements instead let an unbracketed body swallow whatever followed the block, and
  every form that is legal both inside a block and at module level is a way for it to do
  so silently: `always @(posedge clk) d <= ~c;` followed by
  `assign {e0, f0, g0, h0} = oo;` compiled to one three-instruction block, turning a
  continuous driver into a procedural `assign` on a net (corpus `initmod`, `pr434`).
- **A named block keeps its node; an unnamed one does not.** `parse_block` returns a
  `Vec<ProceduralStatements>` either way, so `always`/`initial`/`if` bodies are
  unchanged, but a `begin : name` comes back as a single
  `ProceduralStatements::Block` because its name is a scope the simulator has to
  know about. Only a named block may declare variables, which is why `block_item`
  is only tried after a `: name` was read.
- **`Program::rename_range` must skip an inlined task body, and a named block's
  rename goes through it.** A block local called `count` and a design signal called
  `count` are different variables, and a task body spliced inside the block has
  already resolved its own names — renaming it again would re-point what it read.
  That is the same rule `rename_local` follows, over a range instead of the whole
  program.
- **A replication is held, not expanded.** `{N{a, b}}` is `Expression::Replication`
  rather than an eagerly repeated `Concatenation`, because the count is a full expression
  and may name a parameter — and because `{16384{4'b1001}}` appears in the corpus.
  `replication` is tried before the plain concatenation, since `{a` starts both and only
  the brace after the first expression tells them apart. A count of zero contributes no
  bits, which is legal only inside a wider concatenation and is exactly where it lands; a
  non-constant count is a **named** error, and an absurd one is refused by the same
  `MAX_SELECT_WIDTH` guard a nonsense part select uses.
- **An index is a *position*, and a signed one may be negative.** `reg [3:0] v [-7:7];`
  and `reg [base+15:base] big;` for a negative `base` are both ordinary Verilog, so
  `eval::select_index` reads a signed value as the negative number it is rather than as a
  very large unsigned one. Reading `-7` as `18446744073709551609` names no word of any
  memory, which makes a write a silent no-op and a read an `x` — indistinguishable from an
  index that really is out of range. That is the **one** place the rule lives: every
  select comes through it — a bit, a part, an indexed part and a memory word, reading and
  writing alike — so none of them can disagree about which word a design named.
- **A range is what decides a parameter's signedness**, failing a `signed` qualifier: one
  written with a range is unsigned unless it says otherwise, and only a *rangeless*
  parameter keeps the signedness its value arrived with. The trap is that a bare decimal
  is itself signed, so reading the value's own flag makes `parameter [3:0] DAC = 8;` into
  `-8`, and a select through it then names a bit nothing has (corpus `pr542`). Measured
  against iverilog 12.0.
- **An indexed part select is its own node because only its *width* is constant.**
  `a[base +: width]` and `a[base -: width]` are `Expression::IndexedPartSelect`, not a
  desugared `PartSelect`: the base may be any expression, including one that moves during
  the run, which is the whole reason the operator exists. `indexed_select_width` and
  `indexed_select_indices` are shared by the evaluator and by `resolve_target`, so reading
  and writing a select can never disagree about which bits it names. An unknown *base*
  selects `x` when read — that is what a vector indexed by an unknown holds — and a write
  through one is **ignored** — the LRM says so and iverilog leaves the target untouched, as it
  does for a write out of range. `ResolvedTarget::Nowhere` is that write.
  `indexed_part_select` is tried before `part_select`, which would otherwise read the `:`
  of `+:` as its own separator.
- **Every shape of `name[...]` comes out of one parser, and that is what makes a second
  bracket possible.** `expr.rs::select` reads the name, then one `bracketed_select`, then
  asks for another; `mem[i][3:0]`, `mem[i][2]` and `mem[i][b +: 4]` are
  `Expression::WordSelect`, whose `WordSelectKind` is the same three shapes a plain
  select has, held without a name of their own because the name belongs to the word in
  front of them. Four `alt` alternatives instead would re-parse the name and the first
  bracket once per shape, and again to look for the second — that arrangement measured
  **11% slower** on `bench parse/*` than this one, so `select` is the single entry point
  and `operand_no_ws` and `assignment_lhs` both use it. `bracketed_select`'s
  alternatives keep the historical order for the historical reasons — the bit before the
  part so a conditional index (`q[a ? b : c]`) is not split at its `:`, the indexed part
  before the plain one so the `:` of `+:` is not read as a separator — and each carries
  its own brackets so the `alt` gets a second chance at a whole one rather than failing
  inside a `delimited` that has already eaten the `[`. `bit_select` / `part_select` /
  `indexed_part_select` survive as one-shape wrappers over the same parser, because a
  `specify` path terminal takes exactly one select and must not take a word one.
  A first bracket that is *not* a plain index is the whole select, since `mem[3:0][1]`
  is not Verilog.
  **Only a memory has a second dimension**, and nothing in the grammar can tell
  `mem[i][2]` from a second select on a vector, so `a[0][1:0]` for a plain `a` is
  `EvalError::NotAMemory` naming it rather than bits of the wrong thing — a packed
  dimension (`reg [3:0][7:0] v;`) is still not modelled. A word is a bare `Register` with
  no declared range, so the declared indices are mapped through the *memory's* range:
  that is `state_store::bit_position_in`, the one copy of the mapping, which
  `SignalState::bit_position` and `Memory::bit_of` / `with_bit_of` both go through.
  `indexed_select_indices` asks the memory map for its "which end is most significant"
  question only on a miss of the signal map, so an ordinary vector pays nothing.
- **`resolve_target` guards its part-select width, and must keep doing so.** A range like
  `a[1000000:0]` names more bits than any register has; the evaluator always refused it,
  and the *write* path did not, so once enough designs elaborated to reach it one asked
  for a 34 GB allocation and aborted the whole test harness. Both halves now check
  `MAX_SELECT_WIDTH` before collecting indices.
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

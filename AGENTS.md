# visilog — guide for agents

A Verilog parser and (nascent) simulator written in Rust. The parser is built on
[`nom`](https://docs.rs/nom/7) parser combinators; the simulator is early scaffolding
that consumes the parser's AST.

This file is the repo guide for coding agents. `CLAUDE.md` is a symlink to it.

## Commands

```bash
cargo build          # build
cargo test           # run the full suite (all tests are inline unit tests)
cargo fmt            # format — run before every push
cargo fmt --check    # verify formatting without rewriting
```

There is no integration-test directory and no benchmark harness. `cargo test` is the
whole story, and it runs in well under a second — run it after every change.

`src/main.rs` is a stub (`fn main() {}`) that exists only to root the module tree.
There is no CLI yet, so `cargo run` does nothing. Verify work through tests.

## Layout

```
src/
  main.rs              stub binary; declares the module tree
  git_utils.rs         shallow-clones + caches external repos (for corpus testing)
  register.rs          4-state register value type (0/1/x/z) and radix conversions
  parsers/             the Verilog front end — see below
  simulator/           event-driven simulation scaffolding
  verilog/examples/    sample .v files
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
| `behavior.rs` | `initial` / `always` / `begin…end` blocks |
| `statements.rs` | `ModuleStatement` — the union of things legal in a module body |
| `modules.rs` | `module … endmodule`, ports, and module instantiation |
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

Scaffolding, not a working simulator. Most functions are stubs marked `TODO(meawoppl)`.
The intended pipeline is spelled out in the comment block at the top of `runner.rs`:
validate → gather registers → build the expression graph → compute edge statements →
queue initial/always blocks → run the event queue.

| File | Role |
| --- | --- |
| `runner.rs` | `Simulator` struct; `setup()` and `run()` are stubs |
| `state_store.rs` | `StateStore` — name → `Register` map |
| `event_queue.rs` | time-ordered `EventQueue` of `ExecutionCursor`s |
| `signals.rs` | `Signal` trait plus `FiniteSignal` / `InfiniteSignal` test stimulus |
| `validator.rs` | `validate_module` / `gather_definitions` |

## Conventions

**Parser signature.** Everything is a free function `fn(&str) -> IResult<&str, T>`.
Prefer this plain form over returning `impl FnMut` — it keeps parsers usable as function
pointers, which the `helpers.rs` assertions require.

**Whitespace.** Use `ws(inner)` from `simple.rs`, which wraps a parser in
`multispace0` on both sides. Attach whitespace to the *elements* of a list rather than to
the separator — `separated_list0(char(','), ws(item))` handles space on both sides of the
comma, whereas putting `multispace0` on the separator only eats one side and leaves the
next element starting with a space.

**Tests live next to the code** in an inline `#[cfg(test)] mod tests`. Use the helpers:

```rust
use crate::parsers::helpers::{assert_parses, assert_parses_to};

assert_parses_to(verilog_expression, "a + b", expected_ast);
assert_parses(parse_module_declaration, source);   // asserts no leftover input
```

Both assert the parser consumed the *entire* input, which is the failure these parsers
hit most often. A parser that returns `Ok` with unconsumed trailing input is almost
always a bug — assert on the remainder, not just `is_ok()`.

`expr.rs` also has a whitespace-injection fuzz test that splices random spaces into known
expressions using a seeded `StdRng` (seed 42) and re-parses. If you touch whitespace
handling in an expression layer, that test is your tripwire.

## Gotchas

- **`cargo build` emits ~160 warnings**, nearly all `dead_code` — the parser and simulator
  types have no non-test consumer yet because `main.rs` is a stub. This is expected and
  not something to "fix" by deleting code. It does mean a genuine new warning is easy to
  miss; check the warning count or grep for your file specifically.
- **Duplicate definitions exist.** `NetType` is defined in *both* `parsers/nets.rs` and
  `parsers/modules.rs`; `parse_bit_select` / `parse_part_select` are defined in *both*
  `parsers/identifier.rs` and `parsers/assignment.rs`; `Register` is defined in both
  `src/register.rs` and `simulator/state_store.rs`. Check which one is in scope before
  assuming a change took effect.
- **`ModuleStatement::ModuleInstantiation` exists but isn't parsed.** The variant is in
  the enum in `statements.rs` and `parse_module_instantiation_statement` exists in
  `modules.rs`, but `parse_module_statement`'s `alt(...)` does not include it, so module
  instantiations inside a module body will not parse.
- **`src/verilog/examples/*.v` are not referenced by any code.** They're a corpus waiting
  to be wired into tests, not fixtures currently under test. Adding a test that parses
  them is a good way to find real gaps.
- **`git_utils.rs`'s only test is disabled** (its `#[test]` is commented out) because it
  hits the network. Don't re-enable it in CI without gating it.
- **`nom` is pinned to 7.x.** The 8.x API differs substantially; don't upgrade casually.

## Git workflow

- **Never commit directly to `main`.** Always work on a branch.
- Branch names are prefixed with the username and use dashes:
  `meawoppl/short-description`.
- Add files individually (`git add path/to/file`); do not use `git add -A`.
- Run `cargo fmt` before pushing.
- Keep commit titles to 10 words or fewer.
- Don't leave dead code behind, and don't leave comments describing code that was removed.
- Don't add backward-compatibility shims unless they were asked for.
- Open a PR and share the PR link.

## CI

`.github/workflows/rust.yml` runs on pushes to `main` and on PRs targeting `main`:
`Swatinem/rust-cache` for the cargo cache, then `cargo build --verbose` and
`cargo test --verbose`.

CI does not pin a Rust toolchain — it uses whatever the `ubuntu-latest` runner ships.

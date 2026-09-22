//! Four-state evaluation of parsed Verilog expressions.
//!
//! [`eval`] walks an [`Expression`] and produces a [`Register`], the repo's
//! 0/1/x/z bit vector type, reading identifier values out of a [`StateStore`].
//!
//! # Widths
//!
//! Verilog sizes most expressions **context-determined**: the width of an
//! assignment's target flows back *down* into the operands, and the operation
//! is carried out at that width. `reg [15:0] w; reg [7:0] a, b; w = a * b;`
//! widens `a` and `b` to sixteen bits before multiplying, so the full product
//! survives; sizing each operand by itself would produce an eight bit product
//! and then zero pad a plausible wrong number.
//!
//! The context arrives as the `width` argument of [`eval_in_context`], and it
//! is a **lower bound** rather than an exact size: an operand is padded out to
//! it and is otherwise left at its own width. That is exactly Verilog's rule —
//! an expression is evaluated at the larger of its self-determined width and
//! its context — and it means [`SELF_DETERMINED`], a bound of zero, asks for
//! the self-determined answer without a separate code path.
//!
//! Which operands the bound reaches is not uniform, and [`OperandRule`] is
//! where that lives. It reaches the operands of `+ - * / % & | ^ ~^`, of unary
//! `+ - ~`, the two arms of a `?:`, and the *left* operand of a shift or a
//! `**`. It does not reach a shift's right operand, a `?:` condition, an
//! operand of a comparison or a reduction, or anything inside a concatenation:
//! those size themselves, and a wider context only zero pads whatever they
//! produce.
//!
//! # Signedness
//!
//! Signedness *is* modelled. It starts at a declaration — `reg signed [3:0] a`,
//! an `integer`, a literal with the `s` designator or written as a bare decimal
//! — and rides on the [`Register`] a lookup produces, because a register is
//! bits plus how to read them.
//!
//! Verilog's propagation rule is then one sentence: **an operation is signed
//! only if every one of its operands is**. A single unsigned operand makes the
//! whole expression unsigned, which is why `$signed(a) | b` is unsigned even
//! though half of it was cast. The rule has teeth in exactly five places —
//! `/`, `%`, `>>>`, the relational operators, and the widening that happens
//! whenever two operands of different widths meet — and everything else moves
//! the same bits either way. A concatenation, a bit or part select, and the
//! result of a comparison are unsigned no matter what went into them.
//!
//! Signedness and width arrive together, which is what makes
//! `reg [15:0] r; r = -4'd12;` store `16'hfff4`: the literal is zero padded to
//! sixteen bits by its context and *then* negated, rather than negated in four
//! bits and padded afterwards.

use std::fmt;

use crate::parsers::base::RawToken;
use crate::parsers::constants::{VerilogBaseType, VerilogConstant};
use crate::parsers::expr::{Expression, WordSelectKind};
use crate::parsers::identifier::Identifier;
use crate::parsers::operators::{BinaryOperator, UnaryOperator};
use crate::register::{sign_extend_to_i128, Chunk, Register, ONE, REAL_WIDTH, X, Z, ZERO};
use crate::simulator::exec::{
    address_brackets, range_width, resolve_target, word_select_split, ResolvedTarget,
};
use crate::simulator::plusargs;
use crate::simulator::runner::SimulationError;
use crate::simulator::scan::{self, Slot, END_OF_FILE};
use crate::simulator::state_store::{
    random_from_seed, DriveLevel, Memory, NotReadable, SignalState, StateStore, MAX_CALL_DEPTH,
};
use crate::simulator::tasks::ascii;

/// Width given to a literal written without an explicit size (`42`, `'hFF`).
/// Verilog uses the host `integer` width, which is 32 bits.
const UNSIZED_CONSTANT_WIDTH: usize = 32;

/// Widest register that can be turned into a number, which is what arithmetic
/// and relational operators need. Operators that work bit by bit (bitwise,
/// shifts, selects, concatenation, reduction) are not limited in the width of
/// the value they act on, but a shift amount or a select index still has to be
/// read as a number and so is bounded by this.
const MAX_ARITHMETIC_WIDTH: usize = 128;

/// Upper bound on the width a part select may produce, so that a nonsense
/// range such as `a[1000000:0]` reports an error instead of allocating.
pub const MAX_SELECT_WIDTH: usize = 1 << 16;

/// The width context of an expression nothing around it can size: a lower
/// bound of zero, which every register already meets.
pub const SELF_DETERMINED: usize = 0;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EvalError {
    /// An identifier that has no entry in the [`StateStore`].
    UnknownIdentifier(String),
    /// A memory read as though it were a value: a bare `mem`, or a part select
    /// of one. Only a word select (`mem[addr]`) reads a memory, so this is a
    /// name that exists reported as what it is rather than as unknown.
    MemoryAsValue(String),
    /// `a[0][1:0]` where `a` is a plain vector. Only a memory has a second
    /// dimension to select from, and a vector with a packed one is not
    /// modelled — so this names what was asked for rather than selecting
    /// bits of the wrong thing.
    NotAMemory(String),
    /// An array indexed by the wrong number of addresses: `a[i]` or `a[i][j][k]`
    /// where `a` was declared `reg [7:0] a [0:3][0:15];`.
    ///
    /// A word of an array is named by one index per declared dimension, and
    /// anything else names no word at all — a partial address is a whole row,
    /// which is not a value, and one index too many is a select of something
    /// that has already been narrowed to a word. Both are reported by name,
    /// because reading *some* word instead is the quietest possible wrong
    /// answer: it returns a real value that the design never asked for.
    ArrayDimensions {
        name: String,
        declared: usize,
        used: usize,
    },
    /// A named event read as though it were a value. An event has no value at
    /// all — it is triggered by `-> e;` and waited on by `@(e)` — so a name
    /// that exists is reported as what it is rather than as unknown.
    EventAsValue(String),
    /// A call to a function the design does not declare, so there is no body
    /// to run.
    UnsupportedFunctionCall(String),
    /// A call with the wrong number of arguments for the function it names.
    FunctionArity {
        name: String,
        expected: usize,
        found: usize,
    },
    /// A function body that could not be run to a value. The reason is the
    /// simulation error the body raised, which is not an [`EvalError`] — a
    /// function body is a procedural block, and a block can fail in ways an
    /// expression cannot.
    FunctionFailed { name: String, reason: String },
    /// Calls nested deeper than [`MAX_CALL_DEPTH`], which is what a recursive
    /// function that never reaches its base case looks like.
    FunctionCallDepth { name: String, depth: usize },
    /// A literal whose text could not be turned into bits.
    MalformedConstant(String),
    /// `{}` with nothing in it.
    EmptyConcatenation,
    /// A part select bound that did not evaluate to a usable constant.
    NonConstantSelectBound(String),
    /// A `{N{…}}` whose `N` did not evaluate to a usable constant.
    NonConstantReplicationCount(String),
    /// A value too wide to evaluate; see [`MAX_ARITHMETIC_WIDTH`].
    WidthOverflow(usize),
    /// An operator applied to a real value that has no meaning for one: the
    /// bitwise operators, the shifts and the case comparisons all read a
    /// pattern of bits, and the bits of a real are an IEEE-754 encoding rather
    /// than a number. iverilog rejects every one of these at compile time.
    RealOperand(String),
    /// A `$name` used as a function that this simulator does not implement.
    UnknownSystemFunction(String),
    /// A system function called with a number of arguments it does not take.
    SystemFunctionArity {
        name: String,
        expected: String,
        found: usize,
    },
    /// A `$sscanf`, `$fscanf` or `$fgets` that could not be carried out: a
    /// conversion this simulator does not implement, a format asking for more
    /// arguments than it was given, or an argument that cannot be written.
    /// Never a quiet count of zero, which a design reads as "did not match".
    Scan(String),
    /// A `$value$plusargs` whose format string names no conversion, or a
    /// conversion this simulator does not read. Never a quiet `0`, which a
    /// design reads as "that plus-arg was not given".
    PlusArgs(String),
    /// A `$random(seed)` whose seed is not something that can be written.
    /// The seed is an `inout` — a draw advances it — so a constant there is a
    /// design asking for a stream that cannot move, which is reported rather
    /// than turned into the same number over and over.
    RandomSeed(String),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::UnknownIdentifier(name) => {
                write!(f, "no value for identifier `{}`", name)
            }
            EvalError::MemoryAsValue(name) => {
                write!(f, "memory `{}` has no value without a word select", name)
            }
            EvalError::NotAMemory(name) => {
                write!(f, "`{}` is not a memory, so it has no second select", name)
            }
            EvalError::ArrayDimensions {
                name,
                declared,
                used,
            } => write!(
                f,
                "array `{}` has {} dimension{}, so {} index{} names no word of it",
                name,
                declared,
                if *declared == 1 { "" } else { "s" },
                used,
                if *used == 1 { "" } else { "es" },
            ),
            EvalError::EventAsValue(name) => {
                write!(f, "event `{}` has no value; it can only be triggered", name)
            }
            EvalError::UnsupportedFunctionCall(name) => {
                write!(f, "function call `{}` is not supported", name)
            }
            EvalError::FunctionArity {
                name,
                expected,
                found,
            } => write!(
                f,
                "function `{}` takes {} arguments, but was given {}",
                name, expected, found
            ),
            EvalError::FunctionFailed { name, reason } => {
                write!(f, "function `{}` could not be evaluated: {}", name, reason)
            }
            EvalError::FunctionCallDepth { name, depth } => write!(
                f,
                "function `{}` called more than {} deep, which is runaway recursion",
                name, depth
            ),
            EvalError::MalformedConstant(text) => {
                write!(f, "could not interpret constant `{}`", text)
            }
            EvalError::EmptyConcatenation => write!(f, "empty concatenation has no value"),
            EvalError::NonConstantReplicationCount(text) => write!(
                f,
                "replication count `{}` is not a constant this simulator can evaluate",
                text
            ),
            EvalError::NonConstantSelectBound(text) => {
                write!(f, "part select bound `{}` is not a constant", text)
            }
            EvalError::WidthOverflow(width) => {
                write!(f, "{} bit value is too wide to evaluate", width)
            }
            EvalError::RealOperand(operator) => {
                write!(f, "operator `{}` cannot take a real operand", operator)
            }
            EvalError::UnknownSystemFunction(name) => {
                write!(f, "unknown system function `${}`", name)
            }
            EvalError::SystemFunctionArity {
                name,
                expected,
                found,
            } => write!(f, "`${}` takes {}, but was given {}", name, expected, found),
            EvalError::Scan(reason) => write!(f, "cannot read: {}", reason),
            EvalError::PlusArgs(reason) => write!(f, "{}", reason),
            EvalError::RandomSeed(reason) => {
                write!(f, "`$random`'s seed cannot be written back: {}", reason)
            }
        }
    }
}

impl std::error::Error for EvalError {}

/// Evaluates `expr` against the values in `store`, self-determined: nothing
/// outside the expression has a say in how wide it is or how its bits read.
///
/// This is what a `case` subject, a condition, a task argument and every other
/// expression that is not the right hand side of an assignment wants.
pub fn eval(expr: &Expression, store: &StateStore) -> Result<Register, EvalError> {
    eval_in_context(expr, store, true, SELF_DETERMINED)
}

/// Evaluates `expr` as the right hand side of an assignment into a target
/// `width` bits wide, so that the target's width reaches the operands *before*
/// the operators run.
///
/// The width is a lower bound: an expression wider than its target is still
/// evaluated at its own width and truncated when it is written, which is the
/// order Verilog asks for.
pub fn eval_sized(
    expr: &Expression,
    store: &StateStore,
    width: usize,
) -> Result<Register, EvalError> {
    eval_in_context(expr, store, true, width)
}

/// Evaluates `expr` where `signed_context` says whether the expression around
/// it allows a signed reading.
///
/// Verilog decides signedness for a whole expression *before* evaluating it and
/// then pushes the answer back down: `(a >>> 1) | u` is unsigned because `u`
/// is, and being unsigned makes the `>>>` inside it a plain `>>` even though
/// `a` was declared signed. That is why signedness cannot simply be computed
/// bottom-up out of the operand values — it has to arrive from above as well,
/// which is what this parameter carries. [`expression_is_signed`] is the other
/// half: it answers "would this subexpression be signed on its own?" without
/// evaluating it.
///
/// A *self-determined* operand — the operands of a comparison, a shift amount,
/// a concatenation member, a system function argument — is evaluated with a
/// signed context of `true` and a width of [`SELF_DETERMINED`], because nothing
/// above it has a say.
///
/// `width` is the other half of the context: the fewest bits the result may
/// come back in. It is threaded into exactly the operands [`OperandRule`] calls
/// context-determined, and every other arm pads its own answer out to it
/// instead — a comparison still produces one bit, and a concatenation is still
/// as wide as its parts add up to.
fn eval_in_context(
    expr: &Expression,
    store: &StateStore,
    signed_context: bool,
    width: usize,
) -> Result<Register, EvalError> {
    match expr {
        // Only a *leaf* has to be told that its context is unsigned. Every
        // operator below already asks its own operands, and an operand
        // evaluated in an unsigned context comes back unsigned — so by the time
        // an operator decides its result there is nothing left to demote.
        Expression::Constant(constant) => widened_literal(
            eval_constant(constant, signed_context),
            width,
            constant.extends_with_unknown(),
        ),
        // A real is neither widened nor demoted: it is sixty-four bits that
        // are not a number of bits at all, and no context can make it wider or
        // make it read unsigned.
        Expression::RealLiteral(value) => Ok(Register::from_f64(*value)),
        Expression::Identifier(id) => {
            let value = match store.get(&id.name) {
                Some(value) => value.clone(),
                None => return Err(unresolved(&id.name, store)),
            };
            Ok(widened(demoted(value, signed_context), width))
        }
        Expression::Parenthetical(inner) => eval_in_context(inner, store, signed_context, width),
        Expression::Unary(op, operand) => {
            if unary_keeps_signedness(op) {
                // `+ - ~` hand the context straight through and keep whatever
                // width the operand came back at, so there is nothing to pad.
                let context = signed_context && expression_is_signed(operand, store);
                eval_unary(op, &eval_in_context(operand, store, context, width)?)
            } else {
                // A reduction and `!` read a self-determined operand and answer
                // in one unsigned bit; a wider context only zero pads that.
                let operand = eval_in_context(operand, store, true, SELF_DETERMINED)?;
                widened_result(eval_unary(op, &operand), width)
            }
        }
        Expression::Binary(lhs, op, rhs) => {
            let rule = operand_rule(op);
            let (mut left, mut right) = operand_contexts(rule, lhs, rhs, store, signed_context);
            let (mut left_width, mut right_width) = operand_widths(rule, width);
            // An operation with a real operand has no width, so a context
            // cannot reach its operands: `w = (a + b) + 1.0;` adds `a` and `b`
            // at their own width and converts the sum, however wide `w` is. The
            // cheap question is asked first — most designs have no real in them
            // and `any_real` answers for those without walking anything.
            let declared_real = store.any_real() && either_is_real(lhs, rhs, store);
            if declared_real {
                left_width = SELF_DETERMINED;
                right_width = SELF_DETERMINED;
            }
            // `+ - * / % & | ^ ~^` size their two operands against *each other*
            // as well as against the context: the operation happens at the
            // widest of the three, and the context is only a lower bound on
            // that. `c & ~(1'b1 << 0)` for a four bit `c` has to invert four
            // bits rather than one and then pad with zeros.
            let mutually_sized = !declared_real && matches!(rule, OperandRule::Shared);
            if mutually_sized && sized_within(lhs) {
                // Only an operand that is `sized_within` can tell being widened
                // before from being widened after, and an operand comes back at
                // least as wide as it is on its own whatever it was asked for —
                // so measuring an operand *for itself* would change nothing,
                // and one that is not `sized_within` needs no measurement taken
                // for it either. `count + 1` therefore measures nothing at all.
                left_width = width.max(other_operand_width(rhs, store));
            }
            if matches!(rule, OperandRule::Compared) && (sized_within(lhs) || sized_within(rhs)) {
                let (signed, common) = compared_operands(lhs, rhs, store);
                left = signed;
                right = signed;
                left_width = common;
                right_width = common;
            }
            let left_value = eval_in_context(lhs, store, left, left_width)?;
            // The right operand's share of the mutual context is read off the
            // left one *after* it has been evaluated rather than measured
            // before, because by then the answer is free: the left value is
            // already as wide as the widest of the context, itself and the
            // right operand. Measuring it instead would walk the same subtree
            // the evaluator just walked, which is the whole of what a
            // context-determined operation would otherwise cost.
            if mutually_sized && sized_within(rhs) && !left_value.is_real() {
                right_width = right_width.max(left_value.width());
            }
            let value = eval_binary(
                op,
                &left_value,
                &eval_in_context(rhs, store, right, right_width)?,
            );
            // Every context-determined rule hands the width on to an operand
            // wide enough to satisfy it, so the result already meets it. A
            // comparison and a `&&` answer in one bit however wide their
            // operands were, so those are the two with padding left to do.
            match rule {
                OperandRule::Compared | OperandRule::SelfDetermined => widened_result(value, width),
                _ => value,
            }
        }
        Expression::Conditional(condition, when_true, when_false) => {
            // One real arm makes the whole conditional real, and that has to be
            // decided *before* the condition picks one — the arm that is not
            // taken is never evaluated, so its type could not be read off a
            // value. `c ? 1 : 2.5` is `1.0`, and dividing it by 2 gives 0.5
            // where an integer `1` would give 0.
            //
            // A design with no real in it pays one load and one branch for the
            // question, and everything the answer needs is out of line: the
            // walk, the arms' signedness and the conversion are all inside
            // `real_conditional`.
            if store.any_real() && either_is_real(when_true, when_false, store) {
                return real_conditional(condition, when_true, when_false, store, signed_context);
            }
            // Only the taken branch is evaluated. When the condition is `x` both
            // branches are needed, and the result merges them bit by bit: bits
            // that agree survive, bits that disagree become `x`.
            //
            // Both arms carry the conditional's own signedness, so a signed arm
            // beside an unsigned one is read unsigned even when it is the one
            // taken. The condition itself is self-determined.
            let arms = signed_context
                && expression_is_signed(when_true, store)
                && expression_is_signed(when_false, store);
            match truth(&eval(condition, store)?) {
                Some(true) => eval_in_context(when_true, store, arms, width),
                Some(false) => eval_in_context(when_false, store, arms, width),
                None => {
                    let (when_true, when_false) = (
                        eval_in_context(when_true, store, arms, width)?,
                        eval_in_context(when_false, store, arms, width)?,
                    );
                    Ok(merge(&when_true, &when_false).with_signedness(arms))
                }
            }
        }
        Expression::StringLiteral(text) => Ok(widened(string_bits(text), width)),
        Expression::Concatenation(parts) => {
            if parts.is_empty() {
                return Err(EvalError::EmptyConcatenation);
            }
            let mut values = Vec::with_capacity(parts.len());
            for part in parts {
                values.push(eval(part, store)?);
            }
            // A concatenation sizes itself out of its parts, and a context
            // cannot reach into them: `c = { a**b };` is the four bit power
            // even when `c` is sixteen bits wide.
            Ok(widened(Register::concatenated(&values), width))
        }
        Expression::Replication(count, parts) => {
            if parts.is_empty() {
                return Err(EvalError::EmptyConcatenation);
            }
            let times = replication_count(count, store).ok_or_else(|| {
                EvalError::NonConstantReplicationCount(count.to_contracted_string())
            })?;
            let mut once = Vec::with_capacity(parts.len());
            for part in parts {
                once.push(eval(part, store)?);
            }
            let inner = Register::concatenated(&once);
            // `{0{x}}` is zero bits, which is legal only inside a wider
            // concatenation — and that is exactly where it lands, contributing
            // nothing. The guard is on the *product*, so a large count over a
            // narrow part is caught before it allocates.
            if inner.width().saturating_mul(times) > MAX_SELECT_WIDTH {
                return Err(EvalError::WidthOverflow(inner.width() * times));
            }
            let repeated = vec![inner; times];
            Ok(widened(Register::concatenated(&repeated), width))
        }
        Expression::BitSelect(id, index) => {
            // An index that is unknown, or too far from zero to be a bit
            // number, selects `x`.
            let index = select_index(&eval(index, store)?)?;
            let value = match store.get_signal(&id.name) {
                // `a[3]` where `a` is a vector: one bit of it.
                Some(signal) => match index {
                    Some(index) => logic_bit(signal.bit(index)),
                    None => Register::unknown(1),
                },
                // `m[3]` where `m` is a memory: one whole word of it. The two
                // are the same syntax, and the only thing that tells them apart
                // is which map the declaration put the name in.
                None => match store.memory(&id.name) {
                    Some(memory) => {
                        // One index names a word only of a one-dimensional
                        // array; `a[i]` of `reg [7:0] a [0:3][0:15];` is a
                        // whole row, which is not a value.
                        if memory.dimensions() != 1 {
                            return Err(EvalError::ArrayDimensions {
                                name: id.name.clone(),
                                declared: memory.dimensions(),
                                used: 1,
                            });
                        }
                        demoted(
                            memory.word(index.as_ref().map(std::slice::from_ref)),
                            signed_context,
                        )
                    }
                    None => return Err(EvalError::UnknownIdentifier(id.name.clone())),
                },
            };
            Ok(widened(value, width))
        }
        Expression::PartSelect(id, first, second) => {
            let Some(signal) = store.get_signal(&id.name) else {
                return Err(unresolved(&id.name, store));
            };
            let first = select_bound(first, store)?;
            let second = select_bound(second, store)?;
            let selected = (first - second).unsigned_abs() as usize + 1;
            if selected > MAX_SELECT_WIDTH {
                return Err(EvalError::WidthOverflow(selected));
            }
            // The result runs from the first bound to the second, so a select
            // out of an ascending vector (`a[0:3]`) comes back in source order.
            let indices: Vec<i64> = if first >= second {
                (second..=first).rev().collect()
            } else {
                (first..=second).collect()
            };
            let bits: Vec<u8> = indices.into_iter().map(|i| signal.bit(i)).collect();
            Ok(widened(Register::from_bits(bits), width))
        }
        Expression::IndexedPartSelect {
            id,
            base,
            width: selected,
            upward,
        } => {
            let Some(signal) = store.get_signal(&id.name) else {
                return Err(unresolved(&id.name, store));
            };
            let span = indexed_select_width(selected, store)?;
            // The base is the operand that is allowed to move, so an unknown
            // one is not an error the way a bad bound is — it selects `x`,
            // which is what a vector indexed by an unknown holds.
            let Some(indices) = indexed_select_indices(&id.name, base, span, *upward, store)?
            else {
                return Ok(widened(Register::unknown(span), width));
            };
            let bits: Vec<u8> = indices.into_iter().map(|i| signal.bit(i)).collect();
            Ok(widened(Register::from_bits(bits), width))
        }
        // `mem[i][3:0]` — the word first, then the select inside it. A word is
        // a bare `Register`, so the declared indices are mapped through the
        // *memory's* range rather than a signal's.
        Expression::WordSelect {
            id,
            indices: brackets,
            select,
        } => {
            let Some(memory) = store.memory(&id.name) else {
                return Err(EvalError::NotAMemory(id.name.clone()));
            };
            let declared = memory.dimensions();
            let Some(split) = word_select_split(brackets.len(), select, declared) else {
                return Err(EvalError::ArrayDimensions {
                    name: id.name.clone(),
                    declared,
                    used: address_brackets(brackets.len(), select),
                });
            };
            // An index that is unknown, or too far from zero to be an address,
            // names no word — which reads `x`, exactly as an out-of-range one
            // does.
            let mut address = Vec::with_capacity(declared);
            let mut known = true;
            for bracket in &brackets[..split.leading] {
                match select_index(&eval(bracket, store)?)? {
                    Some(index) => address.push(index),
                    None => known = false,
                }
            }
            if !split.selects_within {
                let WordSelectKind::Bit(last) = select else {
                    unreachable!("`word_select_split` only takes a plain index as an address")
                };
                match select_index(&eval(last, store)?)? {
                    Some(index) => address.push(index),
                    None => known = false,
                }
                let word = memory.word(known.then_some(address.as_slice()));
                return Ok(widened(demoted(word, signed_context), width));
            }
            let word = memory.word(known.then_some(address.as_slice()));
            let value = match select {
                WordSelectKind::Bit(bit) => {
                    match numeric(&eval(bit, store)?)?.and_then(|v| i64::try_from(v).ok()) {
                        Some(bit) => logic_bit(memory.bit_of(&word, bit)),
                        None => Register::unknown(1),
                    }
                }
                WordSelectKind::Part(first, second) => {
                    let first = select_bound(first, store)?;
                    let second = select_bound(second, store)?;
                    let selected = (first - second).unsigned_abs() as usize + 1;
                    if selected > MAX_SELECT_WIDTH {
                        return Err(EvalError::WidthOverflow(selected));
                    }
                    let indices: Vec<i64> = if first >= second {
                        (second..=first).rev().collect()
                    } else {
                        (first..=second).collect()
                    };
                    let bits: Vec<u8> = indices
                        .into_iter()
                        .map(|i| memory.bit_of(&word, i))
                        .collect();
                    Register::from_bits(bits)
                }
                WordSelectKind::Indexed {
                    base,
                    width: selected,
                    upward,
                } => {
                    let span = indexed_select_width(selected, store)?;
                    match indexed_select_indices(&id.name, base, span, *upward, store)? {
                        Some(indices) => {
                            let bits: Vec<u8> = indices
                                .into_iter()
                                .map(|i| memory.bit_of(&word, i))
                                .collect();
                            Register::from_bits(bits)
                        }
                        // An unknown base selects `x`, exactly as it does out
                        // of a vector.
                        None => Register::unknown(span),
                    }
                }
            };
            Ok(widened(value, width))
        }
        // A call is as wide as its function was declared, and a context can
        // only pad that — it cannot reach the arguments, which the function's
        // own declaration sizes.
        Expression::FunctionCall(id, arguments) => widened_result(
            call_function(id, arguments, store).map(|value| demoted(value, signed_context)),
            width,
        ),
        // `$signed(...)` is the one call that produces a signed value out of
        // nothing, so it is a leaf for this purpose too.
        Expression::SystemFunctionCall(name, arguments) => widened_result(
            eval_system_function(name, arguments, store)
                .map(|value| demoted(value, signed_context)),
            width,
        ),
    }
}

/// The array a word select names a *whole word* of, or `None` when its last
/// bracket selects bits inside the word, or it names no word at all.
///
/// `a[i][j]` of `reg [7:0] a [0:3][0:15];` is a whole word, and so is as wide,
/// as signed and as real as the array's element; `mem[i][j]` of a
/// one-dimensional array is one bit of a word and is none of those. The
/// walks that size and sign an expression before evaluating it ask this so
/// they answer the same question [`eval`] does.
fn whole_word<'a>(
    name: &str,
    indices: &[Expression],
    select: &WordSelectKind,
    store: &'a StateStore,
) -> Option<&'a Memory> {
    let memory = store.memory(name)?;
    let split = word_select_split(indices.len(), select, memory.dimensions())?;
    (!split.selects_within).then_some(memory)
}

/// The error for a name that produced no value: a memory used where a value was
/// wanted is reported as the memory it is, and anything else is simply unknown.
fn unresolved(name: &str, store: &StateStore) -> EvalError {
    if store.memory(name).is_some() {
        EvalError::MemoryAsValue(name.to_string())
    } else if store.is_event(name) {
        EvalError::EventAsValue(name.to_string())
    } else {
        EvalError::UnknownIdentifier(name.to_string())
    }
}

/// Evaluates a call to a function the design declares.
///
/// The arguments are evaluated here, in the *caller's* store, because that is
/// where the expressions that produced them were written; the body then runs
/// against a frame of its own. Nothing it writes reaches the design, which is
/// what lets a call happen at all from an evaluator holding a shared reference.
fn call_function(
    id: &Identifier,
    arguments: &[Expression],
    store: &StateStore,
) -> Result<Register, EvalError> {
    let definition = store
        .function(&id.name)
        .ok_or_else(|| EvalError::UnsupportedFunctionCall(id.name.clone()))?;
    if arguments.len() != definition.arity() {
        return Err(EvalError::FunctionArity {
            name: id.name.clone(),
            expected: definition.arity(),
            found: arguments.len(),
        });
    }

    // An argument is *assigned* to the input it lands in, so it is sized by
    // that input the way a right hand side is sized by its target: `test(ltl +
    // 7'd1)` for an eight bit input adds in eight bits and keeps the carry,
    // where adding in seven wraps to zero (corpus `pr2913438b`). Nothing around
    // the call has a say, and a real input has no width to impose.
    let mut values = Vec::with_capacity(arguments.len());
    for (argument, input) in arguments.iter().zip(&definition.arguments) {
        let width = if input.real {
            SELF_DETERMINED
        } else {
            range_width(input.range)
        };
        values.push(eval_sized(argument, store, width)?);
    }

    let _depth = store
        .enter_call()
        .ok_or_else(|| EvalError::FunctionCallDepth {
            name: id.name.clone(),
            depth: MAX_CALL_DEPTH,
        })?;
    definition
        .call(&values, store)
        .map_err(|error| match error {
            // A body that failed while *evaluating* something reports what it hit,
            // rather than a wrapper per frame saying the same thing again: a chain
            // of nested calls would otherwise name every one of them.
            SimulationError::Eval(inner) => inner,
            other => EvalError::FunctionFailed {
                name: id.name.clone(),
                reason: other.to_string(),
            },
        })
}

/// `value` padded out to a context that asked for at least `width` bits.
///
/// A signed value replicates its sign bit and an unsigned one is zero padded,
/// the same widening two operands of different widths get when they meet. A
/// value already that wide is handed straight back, so a [`SELF_DETERMINED`]
/// context costs one comparison. The signedness is restamped because
/// [`Register::coerced`] builds a fresh register, and a fresh register is
/// unsigned.
#[inline(always)]
fn widened(value: Register, width: usize) -> Register {
    // A [`SELF_DETERMINED`] context takes this branch every time, so the
    // padding itself is kept out of line and out of the hot path.
    if value.width() >= width {
        return value;
    }
    pad(&value, width)
}

/// [`widened`] for a value that has still to be unwrapped.
///
/// A [`SELF_DETERMINED`] context hands the result straight back rather than
/// taking the value out and putting it back, which is what the overwhelmingly
/// common case costs.
#[inline(always)]
fn widened_result(value: Result<Register, EvalError>, width: usize) -> Result<Register, EvalError> {
    if width == SELF_DETERMINED {
        return value;
    }
    Ok(widened(value?, width))
}

/// [`widened_result`] for a literal, which is the one value whose *own*
/// spelling says how it extends.
///
/// An **unsized** literal is "at least 32 bits" and takes the width of whatever
/// it is written against, so `'hx` in a 64 bit context is sixty-four `x`s and
/// `'hz` is sixty-four `z`s — IEEE 1364-2005 3.5.1's rule that a literal whose
/// most significant digit is `x` or `z` extends with it. Zero padding instead
/// makes `period !== 'hx` *true* for an untouched 64 bit register, which is
/// corpus `pr673`. A **sized** literal has already been extended to its own
/// width by [`constant_bits`] and is an ordinary value from then on: `4'bx`
/// written into a 64 bit register is `…000x`, measured against iverilog 12.0.
#[inline(always)]
fn widened_literal(
    value: Result<Register, EvalError>,
    width: usize,
    extends_with_unknown: bool,
) -> Result<Register, EvalError> {
    if width == SELF_DETERMINED || !extends_with_unknown {
        return widened_result(value, width);
    }
    let value = value?;
    if value.width() >= width {
        return Ok(value);
    }
    Ok(value.extend_msb(width).with_signedness(value.is_signed()))
}

#[cold]
fn pad(value: &Register, width: usize) -> Register {
    // A real is not a number of bits, so a context asking for more of them has
    // nothing to say to it: padding one would turn the IEEE-754 encoding into
    // an integer that happens to hold the same bits (corpus `pr2913404`).
    if value.is_real() {
        return value.clone();
    }
    value.coerced(width).with_signedness(value.is_signed())
}

/// `c ? a : b` where one of the arms is a real, so the whole conditional is.
///
/// The arms are self-determined — a real has no width for a context to reach —
/// and the arm that is taken is converted, so `c ? 1 : 2.5` is `1.0` and
/// dividing it by 2 gives 0.5 where an integer `1` would give 0. An unknown
/// condition has no `x` to produce, so it gives what the two arms agree on and
/// `0.0` when they do not, which is what iverilog produces (corpus
/// `pr2453002`).
#[cold]
fn real_conditional(
    condition: &Expression,
    when_true: &Expression,
    when_false: &Expression,
    store: &StateStore,
    signed_context: bool,
) -> Result<Register, EvalError> {
    // Both arms carry the conditional's own signedness, exactly as they do
    // when it is not real. Working it out here rather than at the call keeps
    // the two walks off the ordinary conditional's path.
    let arms = signed_context
        && expression_is_signed(when_true, store)
        && expression_is_signed(when_false, store);
    let arm = |expr: &Expression| -> Result<Register, EvalError> {
        let value = eval_in_context(expr, store, arms, SELF_DETERMINED)?;
        Ok(if value.is_real() {
            value
        } else {
            Register::from_f64(value.to_f64())
        })
    };
    match truth(&eval(condition, store)?) {
        Some(true) => arm(when_true),
        Some(false) => arm(when_false),
        None => {
            let (a, b) = (arm(when_true)?.to_f64(), arm(when_false)?.to_f64());
            Ok(Register::from_f64(if a == b { a } else { 0.0 }))
        }
    }
}

/// `value` as an unsigned one unless the context allows it to stay signed.
#[inline(always)]
fn demoted(value: Register, signed_context: bool) -> Register {
    if signed_context {
        value
    } else {
        value.with_signedness(false)
    }
}

// ---------------------------------------------------------------------------
// Signedness and width
// ---------------------------------------------------------------------------

/// Where a binary operator's operands take their signedness and their width
/// from.
///
/// The distinction is not about the operator's arithmetic — it is about which
/// operands are *context-determined*, meaning the expression around them can
/// make them unsigned and can make them wider, and which decide for
/// themselves. Both halves of the context follow the same split, which is why
/// there is one rule rather than two.
#[derive(Clone, Copy)]
enum OperandRule {
    /// `+ - * / % & | ^ ^~`: both operands are context-determined, and the
    /// result is signed only when both of them are.
    Shared,
    /// `**`: the base is context-determined, the exponent is self-determined —
    /// its width cannot change the result, but its sign can, since a negative
    /// exponent has its own rule.
    BaseAndExponent,
    /// `<< >> <<< >>>`: the value being shifted is context-determined and alone
    /// decides the result; the shift amount is self-determined.
    ShiftedValue,
    /// `< <= > >= == != === !==`: the one-bit result is unsigned however the
    /// operands compared, so nothing *outside* the comparison reaches them —
    /// but the two operands are context-determined **with respect to each
    /// other**. They are sized to the wider of the two and read signed only
    /// when both of them are, which is why `a/b` compared against a sixteen
    /// bit net divides in sixteen bits rather than eight.
    Compared,
    /// `&& ||`: each operand is collapsed to a truth value on its own, so
    /// neither the expression around them nor the other operand has any say.
    SelfDetermined,
}

fn operand_rule(op: &BinaryOperator) -> OperandRule {
    match op {
        BinaryOperator::Addition
        | BinaryOperator::Subtraction
        | BinaryOperator::Multiplication
        | BinaryOperator::Division
        | BinaryOperator::Modulus
        | BinaryOperator::BitwiseAnd
        | BinaryOperator::BitwiseOr
        | BinaryOperator::BitwiseInclusiveOr
        | BinaryOperator::BitwiseXOr
        | BinaryOperator::BitwiseXNor => OperandRule::Shared,
        BinaryOperator::Power => OperandRule::BaseAndExponent,
        BinaryOperator::ShiftLeft
        | BinaryOperator::ShiftRight
        | BinaryOperator::ArithmeticShiftLeft
        | BinaryOperator::ArithmeticShiftRight => OperandRule::ShiftedValue,
        BinaryOperator::LessThan
        | BinaryOperator::LessThanOrEqual
        | BinaryOperator::GreaterThan
        | BinaryOperator::GreaterThanOrEqual
        | BinaryOperator::LogicalEquality
        | BinaryOperator::LogicalInequality
        | BinaryOperator::CaseEquality
        | BinaryOperator::CaseInequality => OperandRule::Compared,
        BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => OperandRule::SelfDetermined,
    }
}

/// The signed context each operand of `op` is evaluated in.
fn operand_contexts(
    rule: OperandRule,
    lhs: &Expression,
    rhs: &Expression,
    store: &StateStore,
    signed_context: bool,
) -> (bool, bool) {
    match rule {
        OperandRule::Shared => {
            let signed = signed_context
                && expression_is_signed(lhs, store)
                && expression_is_signed(rhs, store);
            (signed, signed)
        }
        OperandRule::BaseAndExponent => {
            let signed = signed_context
                && expression_is_signed(lhs, store)
                && expression_is_signed(rhs, store);
            (signed, true)
        }
        OperandRule::ShiftedValue => (signed_context && expression_is_signed(lhs, store), true),
        // A comparison is unsigned whatever it compared, so the context above
        // it has nothing to say. Its two operands do decide together — see
        // [`compared_operands`] — but only an operand that reads its own
        // signedness *inside* an operation can tell, and for the rest
        // `align_numeric` reaches the same answer from the values.
        OperandRule::Compared | OperandRule::SelfDetermined => (true, true),
    }
}

/// The width context each operand of `op` is evaluated in, given the `width`
/// the whole operation was asked for.
///
/// The bound reaches every context-determined operand unchanged, because the
/// operation is carried out at the width its operands come back at: widening
/// them *is* how the operation is widened. Everything else gets
/// [`SELF_DETERMINED`] — a shift amount and the operands of a comparison size
/// themselves, and no context can make them wider.
fn operand_widths(rule: OperandRule, width: usize) -> (usize, usize) {
    match rule {
        OperandRule::Shared => (width, width),
        // `**` takes its left operand's width, so that is the one the context
        // reaches; an exponent cannot change the result's width.
        OperandRule::BaseAndExponent | OperandRule::ShiftedValue => (width, SELF_DETERMINED),
        // Both operands of a comparison are sized to the wider of the two,
        // which is the one place a width has to be worked out rather than
        // handed down. `&&` and `||` read each side on its own.
        OperandRule::Compared | OperandRule::SelfDetermined => (SELF_DETERMINED, SELF_DETERMINED),
    }
}

/// Whether a unary operator hands its operand's signedness on. `+ - ~` do; a
/// reduction and `!` produce one unsigned bit.
fn unary_keeps_signedness(op: &UnaryOperator) -> bool {
    matches!(
        op,
        UnaryOperator::Positive | UnaryOperator::Negative | UnaryOperator::BitwiseNegation
    )
}

/// Whether `expr` would be signed if nothing around it had a say.
///
/// This walks the expression without evaluating it, which is what lets
/// [`eval_in_context`] decide an operation's signedness *before* it evaluates
/// the operands — the order Verilog requires. It reads the same sources the
/// evaluator does, so the two cannot disagree about a leaf: a literal's own
/// designator, a signal's declaration, and [`system_function_is_signed`].
fn expression_is_signed(expr: &Expression, store: &StateStore) -> bool {
    match expr {
        Expression::Constant(constant) => constant.is_signed(),
        // A whole word of a multi-dimensional array is as signed as the array
        // was declared. A select out of a word is a run of bits, and a run of
        // bits is unsigned however the memory was declared — the same rule a
        // part select of a signed `reg` follows.
        Expression::WordSelect {
            id,
            indices,
            select,
        } => whole_word(&id.name, indices, select, store).is_some_and(Memory::is_signed),
        // A real has a sign, and saying otherwise would make the *integer*
        // beside it unsigned: `2.5 > -1` has to read that `-1` as -1 rather
        // than as four billion before converting it.
        Expression::RealLiteral(_) => true,
        // The store's hint first: looking a name up costs a hash of it, and in
        // a design that declares nothing signed the answer is already known.
        Expression::Identifier(id) => {
            store.any_signed()
                && store
                    .get_signal(&id.name)
                    .is_some_and(|signal| signal.is_signed())
        }
        Expression::Parenthetical(inner) => expression_is_signed(inner, store),
        Expression::Unary(op, operand) => {
            unary_keeps_signedness(op) && expression_is_signed(operand, store)
        }
        Expression::Binary(lhs, op, rhs) => match operand_rule(op) {
            OperandRule::Shared | OperandRule::BaseAndExponent => {
                expression_is_signed(lhs, store) && expression_is_signed(rhs, store)
            }
            OperandRule::ShiftedValue => expression_is_signed(lhs, store),
            OperandRule::Compared | OperandRule::SelfDetermined => false,
        },
        Expression::Conditional(_, when_true, when_false) => {
            expression_is_signed(when_true, store) && expression_is_signed(when_false, store)
        }
        // A concatenation and a select are bit vectors, not numbers: unsigned
        // however signed the things that went into them were.
        Expression::StringLiteral(_)
        | Expression::Concatenation(_)
        | Expression::Replication(_, _)
        | Expression::BitSelect(_, _)
        | Expression::PartSelect(_, _, _)
        | Expression::IndexedPartSelect { .. } => false,
        // A function is as signed as it was declared to be, which is a
        // property of the declaration rather than of what it returns.
        Expression::FunctionCall(id, _) => store
            .function(&id.name)
            .is_some_and(|definition| definition.result.signed),
        Expression::SystemFunctionCall(name, _) => system_function_is_signed(name),
    }
}

/// Whether `expr` produces a *real*.
///
/// This is [`expression_is_signed`]'s counterpart, and it exists for the one
/// thing realness has to be known *before* an operand is evaluated: an
/// operation with a real operand has no width, so nothing may widen the
/// integer beside it. `(a + b) != 254.0` for two eight bit `255`s is true,
/// because the addition is carried out in eight bits and wraps before it is
/// converted — widening it to the real's sixty-four first gives 510 and a
/// wrong answer (corpus `pr2918095`).
///
/// Realness otherwise travels *up* from the values, which is the whole
/// difference between it and signedness: an operand is evaluated the same way
/// whether or not the operator turns out to be real, so `7/2 + 0.5` is 3.5 —
/// an integer division and then a real addition.
///
/// Every caller asks [`StateStore::any_real`] first, so a design with no real
/// in it never walks anything.
fn expression_is_real(expr: &Expression, store: &StateStore) -> bool {
    match expr {
        Expression::RealLiteral(_) => true,
        // A whole word of an array of reals is a real, as `r[i][j]` of
        // `real r [3:0][1:0];` is. A real has no bits to select from, so a
        // select out of one is not itself real.
        Expression::WordSelect {
            id,
            indices,
            select,
        } => whole_word(&id.name, indices, select, store).is_some_and(Memory::is_real),
        // The store's hint first, the way [`expression_is_signed`] asks it:
        // `declare_real` and `declare_real_memory` are the only two things that
        // set the flag, so `false` means no *declaration* is real and the name
        // need not be hashed. A real **literal** is not covered by it, which is
        // why the flag cannot stand in for this walk at the callers.
        Expression::Identifier(id) => {
            store.any_real()
                && store
                    .get_signal(&id.name)
                    .is_some_and(|signal| signal.is_real())
        }
        // A word of an array of reals is one, and the array is the only place
        // the declaration is recorded.
        Expression::BitSelect(id, _) => {
            store.any_real()
                && store
                    .memory(&id.name)
                    .is_some_and(|memory| memory.is_real())
        }
        Expression::Parenthetical(inner) => expression_is_real(inner, store),
        // `~` and the reductions are refused for a real rather than made real,
        // so every unary operator that survives one hands it on.
        Expression::Unary(_, operand) => expression_is_real(operand, store),
        // One real operand makes the operation real, which is not the rule
        // signedness follows — there it takes *both*.
        Expression::Binary(lhs, _, rhs) => {
            expression_is_real(lhs, store) || expression_is_real(rhs, store)
        }
        Expression::Conditional(_, when_true, when_false) => {
            expression_is_real(when_true, store) || expression_is_real(when_false, store)
        }
        Expression::FunctionCall(id, _) => store
            .function(&id.name)
            .is_some_and(|definition| definition.result.real),
        // The whole real math library answers here beside the three casts,
        // because a comparison sizes its operands against each other and a real
        // has no width to share: `$sin(x) != 0.5` has to know before it
        // evaluates either side.
        Expression::SystemFunctionCall(name, _) => {
            matches!(name.as_str(), "realtime" | "itor" | "bitstoreal")
                || REAL_MATH_UNARY.contains(&name.as_str())
                || REAL_MATH_BINARY.contains(&name.as_str())
        }
        Expression::Constant(_)
        | Expression::StringLiteral(_)
        | Expression::Concatenation(_)
        | Expression::Replication(_, _)
        | Expression::PartSelect(_, _, _)
        | Expression::IndexedPartSelect { .. } => false,
    }
}

/// Whether either side of an operation is a real, which is what says the
/// operation has no width to hand down.
///
/// Kept out of line, and every caller asks [`StateStore::any_real`] *first*:
/// a design with no real in it never walks anything, and pays a load and a
/// branch where the question is put.
#[cold]
fn either_is_real(lhs: &Expression, rhs: &Expression, store: &StateStore) -> bool {
    expression_is_real(lhs, store) || expression_is_real(rhs, store)
}

/// The signedness and the width a comparison's two operands share.
///
/// A comparison is self-determined as far as the expression *around* it is
/// concerned — its answer is one unsigned bit however wide its operands were —
/// but the two operands are context-determined with respect to **each other**:
/// they are sized to the wider of the two, and read signed only when both of
/// them are. `assign wide = a / b;` compared against `a / b` therefore divides
/// in the net's width on one side and in the operands' width on the other, and
/// a signed operand beside an unsigned one is zero padded rather than sign
/// extended.
fn compared_operands(lhs: &Expression, rhs: &Expression, store: &StateStore) -> (bool, usize) {
    let signed = expression_is_signed(lhs, store) && expression_is_signed(rhs, store);
    // A real has no width to share: the integer beside it is worked out at its
    // own width and converted afterwards.
    if store.any_real() && either_is_real(lhs, rhs, store) {
        return (signed, SELF_DETERMINED);
    }
    let width = expression_width(lhs, store).max(expression_width(rhs, store));
    (signed, width)
}

/// How wide an operand makes the operand *beside* it, which is what a
/// context-determined binary operator sizes its two operands against each other
/// with.
///
/// It is [`expression_width`] with one exception: a **real** operand has no
/// width to share, so it reports [`SELF_DETERMINED`] and leaves the integer
/// beside it at its own size. `-180 + bits * (360.0/63.0)` has to add at the
/// integer's width and convert, where widening it to the real's sixty-four
/// bits reads `-180` as a twenty-digit unsigned number (corpus `pr1574175`).
///
/// The question is asked here rather than through [`StateStore::any_real`]
/// because that flag answers for *declarations*, and this expression has a real
/// in it with nothing declared.
///
/// The measurement comes first and the realness walk only runs when it answered
/// [`REAL_WIDTH`], which is what keeps it off the hot path: a real literal, a
/// real signal, a real function's result and any operation over one all measure
/// sixty-four bits, and an operand that measures fewer than that cannot widen
/// the one beside it past a width it would have reached anyway.
fn other_operand_width(expr: &Expression, store: &StateStore) -> usize {
    let measured = expression_width(expr, store);
    if measured == REAL_WIDTH && expression_is_real(expr, store) {
        SELF_DETERMINED
    } else {
        measured
    }
}

/// Whether widening `expr` *before* it is evaluated can give different bits
/// from widening the value it produces afterwards.
///
/// Only an expression that carries out an operation at its own width can tell:
/// `~a` inverts four bits and then pads with zeros, or pads to eight and
/// inverts those. A literal, a signal, a select, a concatenation, a call, a
/// reduction and a nested comparison all produce their bits once and are padded
/// the same way whichever end it happens at — and [`align_numeric`] and
/// [`equal_values`] already pad them. That is what lets a comparison of two
/// plain signals skip measuring anything, which is most comparisons a design
/// writes.
fn sized_within(expr: &Expression) -> bool {
    match expr {
        Expression::Parenthetical(inner) => sized_within(inner),
        // A reduction and `!` answer in one unsigned bit; `+ - ~` work at the
        // width they are given.
        Expression::Unary(op, _) => unary_keeps_signedness(op),
        Expression::Binary(_, op, _) => !matches!(
            operand_rule(op),
            OperandRule::Compared | OperandRule::SelfDetermined
        ),
        Expression::Conditional(_, _, _) => true,
        // An unsized `'hx` fills a wider context with `x`s, where padding the
        // thirty-two bits it produced on its own fills with `0`s — so it is
        // the one *leaf* whose value depends on which end the widening
        // happens at, and a comparison against one has to measure. That is
        // corpus `pr673`: `period !== 'hx` for an untouched 64 bit register.
        Expression::Constant(constant) => constant.extends_with_unknown(),
        _ => false,
    }
}

/// How wide `expr` is when nothing around it has a say — its *self-determined*
/// width.
///
/// [`eval_in_context`] hands a width down rather than working one out, so this
/// is needed in exactly one place: a comparison, whose two operands size each
/// other and so have to be measured before either is evaluated. It reads the
/// same sources the evaluator does — a literal's own size, a signal's
/// declaration, a function's declared result — so the two cannot disagree.
///
/// A name the store does not have, and a part select whose bounds are not
/// constants, report one bit rather than an error: evaluating the same
/// expression is about to fail and say why, and a width guessed here never
/// reaches the answer.
pub(crate) fn expression_width(expr: &Expression, store: &StateStore) -> usize {
    match expr {
        Expression::Constant(constant) => constant.size().unwrap_or(UNSIZED_CONSTANT_WIDTH),
        // A whole word of a multi-dimensional array is as wide as the array's
        // element: `$bits(a[0][1])` of `reg [7:0] a [0:2][3:1];` is 8
        // (iverilog 12.0). Otherwise it is as wide as the last bracket names,
        // which is the same question the three plain selects answer.
        Expression::WordSelect {
            id,
            indices,
            select,
        } => match (whole_word(&id.name, indices, select, store), select) {
            (Some(memory), _) => memory.width(),
            (None, WordSelectKind::Bit(_)) => 1,
            (None, WordSelectKind::Part(first, second)) => {
                match (select_bound(first, store), select_bound(second, store)) {
                    (Ok(first), Ok(second)) => (first - second).unsigned_abs() as usize + 1,
                    _ => 1,
                }
            }
            (None, WordSelectKind::Indexed { width, .. }) => {
                indexed_select_width(width, store).unwrap_or(1)
            }
        },
        Expression::RealLiteral(_) => REAL_WIDTH,
        Expression::Identifier(id) => store
            .get_signal(&id.name)
            .map_or(1, |signal| signal.width()),
        Expression::Parenthetical(inner) => expression_width(inner, store),
        // `+ - ~` are as wide as what they act on; a reduction and `!` answer
        // in one bit.
        Expression::Unary(op, operand) => {
            if unary_keeps_signedness(op) {
                expression_width(operand, store)
            } else {
                1
            }
        }
        Expression::Binary(lhs, op, rhs) => match operand_rule(op) {
            OperandRule::Shared => expression_width(lhs, store).max(expression_width(rhs, store)),
            OperandRule::BaseAndExponent | OperandRule::ShiftedValue => {
                expression_width(lhs, store)
            }
            OperandRule::Compared | OperandRule::SelfDetermined => 1,
        },
        Expression::Conditional(_, when_true, when_false) => {
            expression_width(when_true, store).max(expression_width(when_false, store))
        }
        Expression::StringLiteral(text) => string_width(text),
        Expression::Concatenation(parts) => {
            parts.iter().map(|part| expression_width(part, store)).sum()
        }
        // The count multiplies the width of the inner concatenation. A count
        // that is not a constant reports the inner width alone rather than an
        // error, for the reason the doc comment gives: evaluating the same
        // expression is about to fail and say so.
        Expression::Replication(count, parts) => {
            let inner: usize = parts.iter().map(|part| expression_width(part, store)).sum();
            replication_count(count, store).map_or(inner, |times| inner * times)
        }
        Expression::BitSelect(_, _) => 1,
        // The width is constant by construction, so this is exact whenever the
        // expression is legal at all.
        Expression::IndexedPartSelect { width, .. } => {
            indexed_select_width(width, store).unwrap_or(1)
        }
        Expression::PartSelect(_, first, second) => {
            match (select_bound(first, store), select_bound(second, store)) {
                (Ok(first), Ok(second)) => (first - second).unsigned_abs() as usize + 1,
                _ => 1,
            }
        }
        Expression::FunctionCall(id, _) => store
            .function(&id.name)
            .map_or(1, |definition| range_width(definition.result.range)),
        Expression::SystemFunctionCall(name, arguments) => match name.as_str() {
            "time" => TIME_WIDTH,
            "realtime" | "itor" | "bitstoreal" | "realtobits" => REAL_WIDTH,
            // A cast changes no bit and no width.
            "signed" | "unsigned" => arguments.first().map_or(SYSTEM_FUNCTION_WIDTH, |argument| {
                expression_width(argument, store)
            }),
            _ => SYSTEM_FUNCTION_WIDTH,
        },
    }
}

/// Whether `$name(...)` hands back a signed value.
///
/// Everything that returns Verilog's `integer` does — including `$random`,
/// which is why half the numbers it draws are negative. `$time` is the
/// exception: `time` is a 64 bit *unsigned* type.
fn system_function_is_signed(name: &str) -> bool {
    match name {
        // `$realtobits` hands back a bit pattern rather than a number, so
        // there is no sign in it to read.
        // A descriptor is a bit mask, not a number to do arithmetic on.
        // `$stime` is `$time` truncated to thirty-two bits, and `time` is an
        // unsigned type — which is also the width `%d` gives it: iverilog 12.0
        // prints `$display($stime)` in ten columns where an `integer` takes
        // eleven (corpus `pr2842621`).
        "unsigned" | "time" | "stime" | "realtobits" | "fopen" => false,
        _ => true,
    }
}

// ---------------------------------------------------------------------------
// System functions
// ---------------------------------------------------------------------------

/// Width of what a system function that returns a number hands back: Verilog's
/// `integer`, which is 32 bits.
const SYSTEM_FUNCTION_WIDTH: usize = 32;

/// Width `$time` reports in. Verilog's time unit is a 64 bit quantity;
/// `$stime` is the same value truncated to an `integer`.
const TIME_WIDTH: usize = 64;

/// How wide a *type keyword* is, for `$bits(integer)`.
///
/// `$bits` takes a data type as well as a value, and a type is the one argument
/// the store has nothing to say about — which is why this is asked only once a
/// lookup has already come back `UnknownIdentifier`: anything a design declares
/// answers for itself. A type keyword is reserved, so no plain identifier can be
/// spelled one; the name is taken a segment at a time all the same, since a
/// reference written inside an instance arrives qualified.
///
/// The three are what iverilog 12.0 accepts — `$bits(reg)` is 1, `$bits(integer)`
/// 32 and `$bits(time)` 64, while `$bits(real)` and `$bits(realtime)` are
/// "Invalid data type for $bits()" and `$bits(wire)` is a syntax error. Answering
/// for a type it refuses would be inventing a number.
fn type_width(name: &str) -> Option<usize> {
    match name.rsplit('.').next()? {
        "reg" => Some(1),
        "integer" => Some(SYSTEM_FUNCTION_WIDTH),
        "time" => Some(TIME_WIDTH),
        _ => None,
    }
}

/// Every `$name` [`eval_system_function`] implements.
///
/// Callers that have to decide whether a `$name` is meaningful *before* running
/// it — [`TaskCall::compile`](crate::simulator::tasks::TaskCall::compile) — ask
/// here, so an unrecognised name is rejected in one place. A name listed but
/// not matched below still errors rather than evaluating to anything.
pub const SYSTEM_FUNCTIONS: [&str; 47] = [
    "time",
    "stime",
    "realtime",
    "signed",
    "unsigned",
    "random",
    "bits",
    "clog2",
    "rtoi",
    "itor",
    "realtobits",
    "bitstoreal",
    "fopen",
    // The plus-args. Both names carry a `$` in the middle, which
    // `expr.rs::system_name` already reads as part of one token.
    "test$plusargs",
    "value$plusargs",
    // The real math library — see [`REAL_MATH_UNARY`] and
    // [`REAL_MATH_BINARY`], which are what the evaluator matches on.
    "sqrt",
    "ln",
    "log10",
    "exp",
    "floor",
    "ceil",
    "fabs",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "sinh",
    "cosh",
    "tanh",
    "asinh",
    "acosh",
    "atanh",
    "pow",
    "atan2",
    "hypot",
    // The reading half of file I/O. Every one of these is a system *function*
    // that writes through its argument list — see
    // [`StateStore::owe_fill`](crate::simulator::state_store::StateStore::owe_fill).
    "sscanf",
    "fscanf",
    "fgets",
    "fgetc",
    "ungetc",
    "feof",
    "ftell",
    "fseek",
    "rewind",
    // The one system function that is a question about the *design* rather
    // than about a value: how many drivers reach a bit of a net. It writes
    // through its arguments the way a scan does.
    "countdrivers",
];

/// The one-argument members of IEEE 1364-2005's real math library, plus
/// iverilog's `$fabs`.
///
/// Every one takes and returns a real, and an integer argument converts on the
/// way in — `$sqrt(9)` is `3.0`, because an integer operand of a real function
/// is a real. The list is a constant rather than a `|` chain in the evaluator
/// because three places have to agree about it: the arm that computes the
/// value, [`SYSTEM_FUNCTIONS`], which decides whether the name means anything
/// at all, and [`expression_is_real`], which has to say the call is real
/// *before* it is evaluated.
const REAL_MATH_UNARY: [&str; 19] = [
    "sqrt", "ln", "log10", "exp", "floor", "ceil", "fabs", "sin", "cos", "tan", "asin", "acos",
    "atan", "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
];

/// The two-argument members of the same library. `$atanh` is unary and lives
/// above; `$atan2` is the two-argument arc tangent and lives here.
const REAL_MATH_BINARY: [&str; 3] = ["pow", "atan2", "hypot"];

/// Gives every `$time`, `$stime` and `$realtime` in `expression` the number of
/// clock ticks in one unit of the module it was written in, unless it has one
/// already.
///
/// `eval` is handed a `&StateStore` and nothing else, so the unit a call
/// reports in has to travel *with the call*: it is written on as a hidden
/// argument, a call to [`TICKS_PER_UNIT`] — a name no design can spell, so a
/// `$time(a)` the design wrote is still the arity error it always was.
/// "Unless it has one" is what lets elaboration stamp a module once its walk
/// is over — a child instance's walk ends first, so the parent never reaches
/// the child's calls.
pub fn stamp_system_time(expression: &mut Expression, ticks_per_unit: u64) {
    let nested = |all: &mut [Expression]| {
        for inner in all {
            stamp_system_time(inner, ticks_per_unit);
        }
    };
    match expression {
        Expression::SystemFunctionCall(name, arguments) => {
            if arguments.is_empty() && matches!(name.as_str(), "time" | "stime" | "realtime") {
                let unit = VerilogConstant::from_int(ticks_per_unit.min(i64::MAX as u64) as i64);
                arguments.push(Expression::SystemFunctionCall(
                    TICKS_PER_UNIT.to_string(),
                    vec![Expression::Constant(unit)],
                ));
            } else {
                nested(arguments);
            }
        }
        Expression::Constant(_)
        | Expression::RealLiteral(_)
        | Expression::Identifier(_)
        | Expression::StringLiteral(_) => {}
        Expression::Unary(_, inner) | Expression::Parenthetical(inner) => {
            stamp_system_time(inner, ticks_per_unit)
        }
        Expression::Binary(left, _, right) => {
            stamp_system_time(left, ticks_per_unit);
            stamp_system_time(right, ticks_per_unit);
        }
        Expression::Conditional(condition, yes, no) => {
            stamp_system_time(condition, ticks_per_unit);
            stamp_system_time(yes, ticks_per_unit);
            stamp_system_time(no, ticks_per_unit);
        }
        Expression::Concatenation(parts) => nested(parts),
        Expression::Replication(count, parts) => {
            stamp_system_time(count, ticks_per_unit);
            nested(parts);
        }
        Expression::IndexedPartSelect { base, width, .. } => {
            stamp_system_time(base, ticks_per_unit);
            stamp_system_time(width, ticks_per_unit);
        }
        Expression::FunctionCall(_, arguments) => nested(arguments),
        Expression::BitSelect(_, index) => stamp_system_time(index, ticks_per_unit),
        Expression::PartSelect(_, first, second) => {
            stamp_system_time(first, ticks_per_unit);
            stamp_system_time(second, ticks_per_unit);
        }
        Expression::WordSelect {
            indices, select, ..
        } => {
            nested(indices);
            for inner in select.expressions_mut() {
                stamp_system_time(inner, ticks_per_unit);
            }
        }
    }
}

/// The name of the hidden argument [`stamp_system_time`] writes on. A system
/// function's name is carried without its `$`, and the grammar reads a `$name`
/// as `$` followed by an identifier, which cannot itself begin with one — so
/// nothing a design writes produces this.
const TICKS_PER_UNIT: &str = "$ticks_per_unit";

/// The clock ticks in one unit of the module a `$time`-family call sits in:
/// the argument [`stamp_system_time`] wrote on, or one tick for a call nothing
/// stamped.
fn ticks_per_unit(
    name: &str,
    arguments: &[Expression],
    store: &StateStore,
) -> Result<u128, EvalError> {
    match arguments {
        [] => Ok(1),
        [Expression::SystemFunctionCall(hidden, unit)] if hidden == TICKS_PER_UNIT => {
            let unit = unit.first().map(|unit| eval(unit, store)).transpose()?;
            Ok(unit.and_then(|unit| unit.to_u128()).unwrap_or(1).max(1))
        }
        _ => Err(EvalError::SystemFunctionArity {
            name: name.to_string(),
            expected: "no arguments".to_string(),
            found: arguments.len(),
        }),
    }
}

/// Evaluates `$name(...)`, the simulator's own functions.
///
/// A name this simulator does not implement is an error that repeats the name,
/// never a zero: a design that quietly evaluated `$foo` to `0` would look
/// exactly like one that worked.
fn eval_system_function(
    name: &str,
    arguments: &[Expression],
    store: &StateStore,
) -> Result<Register, EvalError> {
    // Every arm below produces bits; how they are read is one decision, taken
    // once here, so that the table [`expression_is_signed`] consults cannot
    // drift from what the evaluator actually hands back.
    let value = eval_system_function_bits(name, arguments, store)?;
    Ok(value.with_signedness(system_function_is_signed(name)))
}

fn eval_system_function_bits(
    name: &str,
    arguments: &[Expression],
    store: &StateStore,
) -> Result<Register, EvalError> {
    let arity = |expected: &str, allowed: &[usize]| -> Result<(), EvalError> {
        if allowed.contains(&arguments.len()) {
            return Ok(());
        }
        Err(EvalError::SystemFunctionArity {
            name: name.to_string(),
            expected: expected.to_string(),
            found: arguments.len(),
        })
    };

    match name {
        // The store carries the timestamp the surrounding block is running at.
        // `$stime` is the same clock as an `integer`, which is what a design
        // that prints a timestamp with `%0d` usually wants.
        //
        // The clock counts ticks of the design's finest precision, and a call
        // reports in the unit of the module it was written in — which
        // elaboration stamps on as a hidden argument, see
        // [`stamp_system_time`]. An integer time is **rounded**, half up:
        // measured against iverilog 12.0 in a `1ns` module beside a `1ps` one,
        // 1499ps is `1`, 1500ps is `2` and 2500ps is `3`.
        "time" | "stime" => {
            let ticks_per_unit = ticks_per_unit(name, arguments, store)?;
            let width = if name == "time" {
                TIME_WIDTH
            } else {
                SYSTEM_FUNCTION_WIDTH
            };
            let ticks = store.time().unsigned_abs() as u128;
            Ok(Register::from_u128(
                (ticks + ticks_per_unit / 2) / ticks_per_unit,
                width,
            ))
        }
        // The same clock as a real, which is exact where `$time` rounds: the
        // 1499ps above is `1.499`.
        "realtime" => {
            let ticks_per_unit = ticks_per_unit(name, arguments, store)?;
            Ok(Register::from_f64(
                store.time() as f64 / ticks_per_unit as f64,
            ))
        }
        // The two conversions, and the difference between them is the whole
        // point: `$rtoi` **truncates** toward zero where an assignment to an
        // `integer` rounds, so `$rtoi(2.7)` is 2 and `i = 2.7;` is 3.
        "rtoi" => {
            arity("exactly one argument", &[1])?;
            let value = eval(&arguments[0], store)?;
            Ok(Register::integer_from_f64(
                value.to_f64().trunc(),
                SYSTEM_FUNCTION_WIDTH,
            ))
        }
        "itor" => {
            arity("exactly one argument", &[1])?;
            // `$itor` takes an *integer*, so a real argument is converted to
            // one before it is converted back: `$itor(10.5)` is `11.0` and
            // `$itor(1.0/0.0)` is `0.0`, since an infinity is no integer at
            // all (corpus `itor_rtoi`).
            let value = eval(&arguments[0], store)?.to_f64();
            Ok(Register::from_f64(if value.is_finite() {
                value.round()
            } else {
                0.0
            }))
        }
        // `$fopen` is a system *function* — it hands a descriptor back — which
        // is why the file table lives on the [`StateStore`] beside the
        // `$random` stream rather than on the `TaskContext`: this is all the
        // evaluator is given. One argument opens a multi-channel descriptor, a
        // one-hot bit allocated from bit 1 up because bit 0 is standard output;
        // two opens a file descriptor in a C `fopen` mode. A file that cannot
        // be opened is **0** rather than an error, because 0 is what the design
        // itself tests for.
        "fopen" => {
            arity("a file name and an optional mode", &[1, 2])?;
            let name = file_name(&arguments[0], store)?;
            let descriptor = match arguments.get(1) {
                None => store.open_channel(&name),
                Some(mode) => store.open_descriptor(&name, &file_name(mode, store)?),
            };
            Ok(Register::from_u128(
                descriptor as u128,
                SYSTEM_FUNCTION_WIDTH,
            ))
        }
        // The reading half. `$sscanf` and `$fscanf` differ in one thing —
        // where the characters come from — so everything past that point is
        // the one engine in [`scan`].
        // The two plus-arg readers. `$test$plusargs` asks whether an option was
        // given; `$value$plusargs` also *writes* the value into its second
        // argument, so it goes through the same fill queue `$sscanf` does —
        // `eval` is handed a `&StateStore` and cannot write one itself.
        //
        // A plus-arg nothing matches is `0` with the target left alone, which
        // is not a failure: it is the answer `if (!$value$plusargs(…))` exists
        // to read, and visilog has no command line, so a design run through the
        // library sees an empty list unless its caller said otherwise.
        "test$plusargs" => {
            arity("exactly one option name", &[1])?;
            let Some(prefix) = known_text(&arguments[0], store)? else {
                return Ok(Register::from_u128(0, SYSTEM_FUNCTION_WIDTH));
            };
            Ok(signed_result(i64::from(plusargs::test(
                store.plusargs(),
                &prefix,
            ))))
        }
        "value$plusargs" => {
            arity("a format string and a target to fill", &[2])?;
            let slot = scan_slots(&arguments[1..], store)?
                .pop()
                .expect("one argument gives one slot");
            // A format with an unknown bit in it names no option, the same way
            // one does for `$sscanf`.
            let Some(format) = known_text(&arguments[0], store)? else {
                return Ok(signed_result(0));
            };
            let real = slot.target.is_real(store);
            let found = plusargs::value(store.plusargs(), &format, slot.width, real)
                .map_err(EvalError::PlusArgs)?;
            match found {
                Some(value) => {
                    store.owe_fill(slot.target, value);
                    Ok(signed_result(1))
                }
                None => Ok(signed_result(0)),
            }
        }
        // `$countdrivers(net, [forced, countD, count0, count1, countX])` —
        // how many continuous drivers reached one *bit* of a net, and what
        // each of them was driving. The answer is `1` for a bit more than one
        // driver reaches and `0` otherwise; everything else comes back through
        // the arguments, which is why this is one of the system functions that
        // writes the design.
        //
        // A driver that is contributing `z` is not counted at all, so an
        // undriven net answers `0` rather than the number of `assign`
        // statements naming it (corpus `countdrivers2`, measured against
        // iverilog 12.0).
        "countdrivers" => {
            if arguments.is_empty() || arguments.len() > 6 {
                return Err(EvalError::SystemFunctionArity {
                    name: name.to_string(),
                    expected: "a net, and up to five variables to fill".to_string(),
                    found: arguments.len(),
                });
            }
            let (signal, position) = counted_bit(&arguments[0], store)?;
            let tally = signal
                .driver_counts()
                .and_then(|counts| counts.get(position).copied())
                .unwrap_or_default();
            let forced = u32::from(forced_bit(&arguments[0], store)?);
            let answers = [forced, tally.total(), tally.zero, tally.one, tally.unknown];
            for (argument, answer) in arguments[1..].iter().zip(answers) {
                let target = resolve_target(store, argument)
                    .map_err(|error| EvalError::Scan(error.to_string()))?;
                store.owe_fill(target, signed_result(i64::from(answer)));
            }
            Ok(signed_result(i64::from(u32::from(tally.total() > 1))))
        }
        "sscanf" | "fscanf" => {
            if arguments.len() < 2 {
                return Err(EvalError::SystemFunctionArity {
                    name: name.to_string(),
                    expected: "a source, a format string, and the arguments to fill".to_string(),
                    found: arguments.len(),
                });
            }
            // The targets are resolved before anything is read, so an
            // argument that cannot be written into is reported instead of
            // being discovered half way through a scan that has already
            // consumed the input.
            let slots = scan_slots(&arguments[2..], store)?;
            // A format string with an unknown bit in it is no format at all:
            // `$sscanf(s, 'bx, a)` is end of file, which is what corpus
            // `scanf4` asserts.
            let Some(format) = known_text(&arguments[1], store)? else {
                return Ok(scan_count(END_OF_FILE));
            };
            let (count, fills) = if name == "sscanf" {
                let Some(text) = known_text(&arguments[0], store)? else {
                    return Ok(scan_count(END_OF_FILE));
                };
                scan::scan(&mut scan::Text::new(&text), &format, &slots).map_err(EvalError::Scan)?
            } else {
                let descriptor = descriptor(&arguments[0], store)?;
                match store.with_reader(descriptor, |reader| scan::scan(reader, &format, &slots)) {
                    Ok(scanned) => scanned.map_err(EvalError::Scan)?,
                    // A descriptor that names no readable file is end of file,
                    // which is the number iverilog hands back for all three of
                    // a closed descriptor, a write-only one and a channel.
                    Err(NotReadable::EndOfFile) => (END_OF_FILE, Vec::new()),
                    Err(NotReadable::Update) => return Err(update_mode("$fscanf")),
                }
            };
            for (target, value) in fills {
                store.owe_fill(target, value);
            }
            Ok(scan_count(count))
        }
        // `$fgets(target, fd)` — one line, up to as many characters as the
        // target holds, the newline kept. The answer is how many characters
        // were read; `0` is end of file, and the target is then left alone.
        "fgets" => {
            arity("a target and a file descriptor", &[2])?;
            let slot = scan_slots(&arguments[..1], store)?
                .pop()
                .expect("one argument gives one slot");
            let descriptor = descriptor(&arguments[1], store)?;
            let line = match store.with_reader(descriptor, |reader| {
                read_line(reader, slot.width.max(8) / 8)
            }) {
                Ok(line) => line,
                Err(NotReadable::EndOfFile) => Vec::new(),
                Err(NotReadable::Update) => return Err(update_mode("$fgets")),
            };
            if !line.is_empty() {
                store.owe_fill(slot.target, string_bits_of(&line));
            }
            Ok(Register::from_u128(
                line.len() as u128,
                SYSTEM_FUNCTION_WIDTH,
            ))
        }
        // `$fgetc` — one character, or `-1` at end of file.
        "fgetc" => {
            arity("exactly one file descriptor", &[1])?;
            let descriptor = descriptor(&arguments[0], store)?;
            let byte = match store.with_reader(descriptor, |reader| reader.take()) {
                Ok(byte) => byte,
                Err(NotReadable::EndOfFile) => None,
                Err(NotReadable::Update) => return Err(update_mode("$fgetc")),
            };
            Ok(signed_result(byte.map_or(END_OF_FILE, i64::from)))
        }
        // `$ungetc` — puts one character back, so the next read finds it. `0`
        // is success and `-1` is failure, which is C's convention rather than
        // the character itself.
        "ungetc" => {
            arity("a character and a file descriptor", &[2])?;
            let byte = eval(&arguments[0], store)?.to_u128().map(|code| code as u8);
            let descriptor = descriptor(&arguments[1], store)?;
            let done = match byte {
                Some(byte) => store
                    .with_reader(descriptor, |reader| reader.unread(byte))
                    .unwrap_or(false),
                None => false,
            };
            Ok(signed_result(if done { 0 } else { END_OF_FILE }))
        }
        // `$feof` — whether a read has run off the end. Sticky, like C's, so
        // reaching the last byte is not yet end of file.
        "feof" => {
            arity("exactly one file descriptor", &[1])?;
            let descriptor = descriptor(&arguments[0], store)?;
            let done = store
                .with_reader(descriptor, |reader| reader.at_eof())
                .unwrap_or(true);
            Ok(Register::from_u128(u128::from(done), SYSTEM_FUNCTION_WIDTH))
        }
        // `$ftell` — the byte offset of the next character, or `-1`.
        "ftell" => {
            arity("exactly one file descriptor", &[1])?;
            let descriptor = descriptor(&arguments[0], store)?;
            let position = store
                .with_reader(descriptor, |reader| reader.position())
                .ok()
                .flatten();
            Ok(signed_result(position.unwrap_or(END_OF_FILE)))
        }
        // `$fseek(fd, offset, operation)` and `$rewind(fd)` — `0` on success
        // and `-1` on failure, C's convention again.
        "fseek" | "rewind" => {
            let expected = if name == "rewind" {
                arity("exactly one file descriptor", &[1])?;
                Some(std::io::SeekFrom::Start(0))
            } else {
                arity("a file descriptor, an offset and an operation", &[3])?;
                let offset = whole_number(&arguments[1], store)?;
                let operation = whole_number(&arguments[2], store)?;
                offset
                    .zip(operation)
                    .and_then(|(offset, operation)| scan::seek_from(operation, offset))
            };
            let descriptor = descriptor(&arguments[0], store)?;
            let done = match expected {
                Some(to) => store
                    .with_reader(descriptor, |reader| reader.seek(to))
                    .unwrap_or(false),
                None => false,
            };
            Ok(signed_result(if done { 0 } else { END_OF_FILE }))
        }
        // The real math library. Each is its `f64` counterpart, with the
        // argument converted on the way in — `$sqrt(9)` is `3.0`, because an
        // integer operand of a real function is a real. Checked against
        // iverilog 12.0 rather than assumed.
        _ if REAL_MATH_UNARY.contains(&name) => {
            arity("exactly one argument", &[1])?;
            let value = eval(&arguments[0], store)?.to_f64();
            let result = match name {
                "sqrt" => value.sqrt(),
                "ln" => value.ln(),
                "log10" => value.log10(),
                "exp" => value.exp(),
                "floor" => value.floor(),
                "ceil" => value.ceil(),
                "fabs" => value.abs(),
                "sin" => value.sin(),
                "cos" => value.cos(),
                "tan" => value.tan(),
                "asin" => value.asin(),
                "acos" => value.acos(),
                "atan" => value.atan(),
                "sinh" => value.sinh(),
                "cosh" => value.cosh(),
                "tanh" => value.tanh(),
                "asinh" => value.asinh(),
                "acosh" => value.acosh(),
                "atanh" => value.atanh(),
                _ => unreachable!("the arm matched one of these names"),
            };
            Ok(Register::from_f64(result))
        }
        _ if REAL_MATH_BINARY.contains(&name) => {
            arity("exactly two arguments", &[2])?;
            let left = eval(&arguments[0], store)?.to_f64();
            let right = eval(&arguments[1], store)?.to_f64();
            let result = match name {
                "pow" => left.powf(right),
                // C's `atan2(y, x)`, and the argument order is the one place
                // this family can be wrong without looking wrong: the *first*
                // argument is the numerator.
                "atan2" => left.atan2(right),
                "hypot" => left.hypot(right),
                _ => unreachable!("the arm matched one of these names"),
            };
            Ok(Register::from_f64(result))
        }
        // The IEEE-754 encoding, and back. They are a pair of casts over the
        // same sixty-four bits: `$realtobits(1.5)` is `64'h3ff8000000000000`
        // and `$bitstoreal` of that is `1.5` again.
        "realtobits" => {
            arity("exactly one argument", &[1])?;
            let value = eval(&arguments[0], store)?.to_f64();
            Ok(Register::from_f64(value).with_realness(false))
        }
        "bitstoreal" => {
            arity("exactly one argument", &[1])?;
            let bits = eval(&arguments[0], store)?.coerced(REAL_WIDTH);
            // An unknown bit has no place in a double, so it reads as `0` the
            // same way it does anywhere else a four-state value becomes one.
            Ok(Register::from_f64(f64::from_bits(
                bits.chunk(0).ones() as u64
            )))
        }
        // A cast that changes no bit and no width: it says only how the bits
        // that are already there are to be read. Everything it changes happens
        // in the operator that receives the result — `$signed(4'b1000)` widens
        // as -8, compares as -8, and `>>>` on it is arithmetic.
        "signed" | "unsigned" => {
            arity("exactly one argument", &[1])?;
            eval(&arguments[0], store)
        }
        // `$random` and `$random(seed)`.
        //
        // The two forms differ only in where the stream's state is kept. A
        // bare `$random` draws from the store's, and a seeded one draws from
        // the design's own variable: the seed is an `inout`, so the draw reads
        // it *and* writes the next one back through it, which is what makes
        // `for (…) r = $random(s);` a sequence rather than one number over and
        // over. The write-back goes through the queue `$sscanf` uses, so it
        // lands before the next statement can read the seed — and a *second*
        // draw in the same statement reads it out of that queue rather than
        // out of the signal, which is what makes `{$random(s), $random(s)}`
        // two numbers (corpus `concat3`). That is the same
        // [`StateStore::pending_fill`] a second call to a function with a side
        // effect already went through.
        "random" => {
            arity("no arguments, or a seed", &[0, 1])?;
            let Some(argument) = arguments.first() else {
                return Ok(signed_result(i64::from(store.next_random())));
            };
            let target = resolve_target(store, argument)
                .map_err(|error| EvalError::RandomSeed(error.to_string()))?;
            let owed = match &target {
                ResolvedTarget::Whole(name) => store.pending_fill(name),
                _ => None,
            };
            let seed = match owed {
                Some(value) => value,
                None => eval(argument, store)?,
            };
            // An unknown bit of the seed reads as `0`, which is what iverilog
            // takes from a four-state value asked for as an integer.
            let seed = seed.coerced(SYSTEM_FUNCTION_WIDTH);
            let (value, next) = random_from_seed(seed.chunk(0).ones() as u32 as i32);
            store.owe_fill(target, signed_result(i64::from(next)));
            Ok(signed_result(i64::from(value)))
        }
        // The width of the operand, which every value here knows about itself
        // — or of a *type*, which nothing in the store does.
        "bits" => {
            arity("exactly one argument", &[1])?;
            let width = match eval(&arguments[0], store) {
                Ok(value) => value.width(),
                Err(EvalError::UnknownIdentifier(name)) => match type_width(&name) {
                    Some(width) => width,
                    None => return Err(EvalError::UnknownIdentifier(name)),
                },
                Err(error) => return Err(error),
            };
            Ok(Register::from_u128(width as u128, SYSTEM_FUNCTION_WIDTH))
        }
        // `$clog2(n)` is how many bits it takes to count `n` things: the
        // ceiling of log2, and `0` for `0` and `1`.
        "clog2" => {
            arity("exactly one argument", &[1])?;
            match numeric(&eval(&arguments[0], store)?)? {
                Some(value) => Ok(Register::from_u128(
                    clog2(value) as u128,
                    SYSTEM_FUNCTION_WIDTH,
                )),
                // Unknown in, unknown out.
                None => Ok(Register::unknown(SYSTEM_FUNCTION_WIDTH)),
            }
        }
        other => Err(EvalError::UnknownSystemFunction(other.to_string())),
    }
}

/// The number of bits an unsigned count of `value` distinct values needs.
fn clog2(value: u128) -> u32 {
    if value <= 1 {
        return 0;
    }
    128 - (value - 1).leading_zeros()
}

/// How many times a `{N{…}}` repeats its parts, or `None` when `N` is not a
/// usable constant.
///
/// `None` rather than an error so that [`expression_width`] can fall back
/// without one; the evaluator turns the same `None` into a named error, which
/// is where a design that wrote something unusable finds out.
fn replication_count(expr: &Expression, store: &StateStore) -> Option<usize> {
    numeric(&eval(expr, store).ok()?)
        .ok()?
        .and_then(|value| usize::try_from(value).ok())
}

/// How many bits a `[base +: width]` names. The width — unlike the base — must
/// be a constant, which is what makes the select a fixed size however the base
/// moves.
pub fn indexed_select_width(expr: &Expression, store: &StateStore) -> Result<usize, EvalError> {
    let span = select_bound(expr, store)?;
    let span = usize::try_from(span)
        .map_err(|_| EvalError::NonConstantSelectBound(expr.to_contracted_string()))?;
    if span == 0 || span > MAX_SELECT_WIDTH {
        return Err(EvalError::WidthOverflow(span));
    }
    Ok(span)
}

/// The declared bit indices a `[base +: span]` or `[base -: span]` names, most
/// significant first — the order [`Register::from_bits`] and
/// `ResolvedTarget::Bits` both take.
///
/// `None` means the base did not evaluate to a number, which for this operator
/// is a legal outcome rather than an error: the base is the operand allowed to
/// move at run time, so an unknown one selects `x`.
///
/// `name` is the vector being selected from, because *which end is most
/// significant* is the vector's own declaration to say: `reg [-1:14]` is
/// written from its low index down, so a select out of it runs the other way
/// round from one out of a `reg [14:-1]`. That is the same rule a plain
/// `PartSelect` follows from the order its two bounds were written in — this
/// operator has only one bound, so it reads the declaration instead.
pub fn indexed_select_indices(
    name: &str,
    base: &Expression,
    span: usize,
    upward: bool,
    store: &StateStore,
) -> Result<Option<Vec<i64>>, EvalError> {
    let Some(base) = select_index(&eval(base, store)?)? else {
        return Ok(None);
    };
    let span = span as i64;
    // `+:` runs from the base upwards and `-:` from the base downwards; which
    // end of that run is the *most significant* is the declaration's to say.
    let (high, low) = if upward {
        (base + span - 1, base)
    } else {
        (base, base - span + 1)
    };
    // A memory is asked second, and only on a miss: `mem[a][b +: 4]` selects
    // out of the *word*, whose range is the memory's declaration.
    let ascending = store
        .get_signal(name)
        .map(|signal| signal.range())
        .or_else(|| store.memory(name).map(|memory| memory.range()))
        .is_some_and(|(msb, lsb)| msb < lsb);
    Ok(Some(if ascending {
        (low..=high).collect()
    } else {
        (low..=high).rev().collect()
    }))
}

/// How wide a string literal is: eight bits per character, most significant
/// character first.
///
/// An empty string is one NUL byte rather than nothing — `$bits("")` is 8, as
/// iverilog agrees — so it still has a value to be compared against.
fn string_width(text: &str) -> usize {
    8 * text.len().max(1)
}

/// A string literal as bits. Unsigned: it is a vector of bytes, not a number
/// anyone declared a sign for.
/// The text an expression spells, which is how a file name reaches `$fopen`.
///
/// A literal is the obvious case, but `$fopen({"work/", name})` is the one that
/// matters: a design that builds a path out of a parameter and a `reg` hands
/// over a bit vector, and eight bits at a time it is the same characters.
fn file_name(expression: &Expression, store: &StateStore) -> Result<String, EvalError> {
    Ok(ascii(&eval(expression, store)?))
}

/// The text an expression spells, or `None` if any bit of it is unknown.
///
/// A scan's format string and a `$sscanf` source both come through here, and
/// the `None` is what makes `$sscanf(s, 'bx, a)` end of file: there is nothing
/// to read *by*, which is a different thing from reading and not matching.
fn known_text(expression: &Expression, store: &StateStore) -> Result<Option<String>, EvalError> {
    let value = eval(expression, store)?;
    if value.has_unknown() {
        return Ok(None);
    }
    Ok(Some(ascii(&value)))
}

/// A descriptor argument: a `$fopen` answer, which has to be fully known.
///
/// An unknown one is an error rather than a plausible number, exactly as it is
/// for the writing half — a design cannot have got a descriptor with an `x` in
/// it from anywhere this simulator handed one out.
fn descriptor(expression: &Expression, store: &StateStore) -> Result<u32, EvalError> {
    let value = eval(expression, store)?;
    value
        .to_u128()
        .map(|descriptor| descriptor as u32)
        .ok_or_else(|| {
            EvalError::Scan(format!(
                "file descriptor `{}` is not a known value",
                expression.to_contracted_string()
            ))
        })
}

/// An integer argument that has to be a whole known number — a `$fseek` offset
/// or operation. `None` is an unknown one, which fails the seek rather than
/// seeking somewhere arbitrary.
fn whole_number(expression: &Expression, store: &StateStore) -> Result<Option<i64>, EvalError> {
    Ok(eval(expression, store)?
        .to_i128()
        .and_then(|value| i64::try_from(value).ok()))
}

/// The signal and bit position `$countdrivers` was asked about.
///
/// A net named whole reports its **least significant** bit, which is the same
/// bit a scalar gate terminal names; a bit select names the bit its index maps
/// to. The name goes through [`StateStore::unalias`], because a testbench
/// reaching into an instance writes the *port's* name (`pad1.pad`) and
/// flattening left the port and what it was bound to as one entry under the
/// parent's (corpus `countdrivers4`).
fn counted_bit<'a>(
    argument: &Expression,
    store: &'a StateStore,
) -> Result<(&'a SignalState, usize), EvalError> {
    let (name, index) = match argument {
        Expression::Identifier(id) => (id.name.as_str(), None),
        Expression::BitSelect(id, index) => {
            (id.name.as_str(), Some(select_index(&eval(index, store)?)?))
        }
        _ => {
            return Err(EvalError::Scan(
                "`$countdrivers` takes a net or one bit of one".to_string(),
            ))
        }
    };
    let name = store.unalias(name);
    let signal = store
        .get_signal(name)
        .ok_or_else(|| EvalError::UnknownIdentifier(name.to_string()))?;
    let position = match index {
        Some(Some(index)) => signal.bit_position(index),
        // An index that is not a known number names no bit, and neither does
        // one outside the declared range — which is what an out-of-range read
        // already is.
        Some(None) => None,
        None => signal.width().checked_sub(1),
    };
    let position = position.ok_or_else(|| {
        EvalError::Scan(format!(
            "`$countdrivers` names a bit `{}` does not have",
            name
        ))
    })?;
    Ok((signal, position))
}

/// Whether a `force` is holding the bit `$countdrivers` was asked about.
///
/// A design that forces nothing costs one length compare, the same shape
/// `exec::held_bits` uses to answer the same question about a write.
fn forced_bit(argument: &Expression, store: &StateStore) -> Result<bool, EvalError> {
    if !store.has_drives() {
        return Ok(false);
    }
    let (signal, position) = counted_bit(argument, store)?;
    let index = bit_index_at(signal.range(), position);
    let wanted = store.unalias(name_of(argument));
    let drives = store.drives();
    for drive in drives.iter() {
        if drive.level() != DriveLevel::Force || !drive.covers(wanted) {
            continue;
        }
        let Ok(target) = resolve_target(store, drive.target()) else {
            continue;
        };
        let covered = match &target {
            ResolvedTarget::Whole(_) => true,
            ResolvedTarget::Bits { name, indices } => name == wanted && indices.contains(&index),
            _ => false,
        };
        if covered {
            return Ok(true);
        }
    }
    Ok(false)
}

/// The declared index a position in a signal's range stands for.
fn bit_index_at(range: (i64, i64), position: usize) -> i64 {
    let (most, least) = range;
    if most >= least {
        most - position as i64
    } else {
        most + position as i64
    }
}

/// The name a `$countdrivers` argument is about.
fn name_of(argument: &Expression) -> &str {
    match argument {
        Expression::Identifier(id) | Expression::BitSelect(id, _) => id.name.as_str(),
        _ => "",
    }
}

/// Turns each argument of a scan into the place its conversion writes.
fn scan_slots(arguments: &[Expression], store: &StateStore) -> Result<Vec<Slot>, EvalError> {
    arguments
        .iter()
        .map(|argument| {
            let target = resolve_target(store, argument)
                .map_err(|error| EvalError::Scan(error.to_string()))?;
            let width = target.width(store);
            Ok(Slot { target, width })
        })
        .collect()
}

/// What a scan hands back: an `integer`, so `-1` reads as `-1`.
fn scan_count(count: i64) -> Register {
    signed_result(count)
}

fn signed_result(value: i64) -> Register {
    Register::from_u128(value as i128 as u128, SYSTEM_FUNCTION_WIDTH).with_signedness(true)
}

fn update_mode(task: &str) -> EvalError {
    EvalError::Scan(format!(
        "`{}` on a file opened in an update mode (`r+`, `w+`, `a+`), which this simulator does not read",
        task
    ))
}

/// One line from a file: up to `bytes` characters, stopping after a newline.
/// An empty answer is end of file, and is what leaves `$fgets`'s target alone.
fn read_line(reader: &mut crate::simulator::state_store::Reader, bytes: usize) -> Vec<u8> {
    let mut line = Vec::new();
    while line.len() < bytes {
        let Some(byte) = reader.take() else { break };
        line.push(byte);
        if byte == b'\n' {
            break;
        }
    }
    line
}

/// A run of bytes as a bit vector, eight bits a character, most significant
/// character first — the same shape a string literal has.
fn string_bits_of(bytes: &[u8]) -> Register {
    Register::from_bits(
        bytes
            .iter()
            .flat_map(|byte| (0..8).rev().map(move |offset| (byte >> offset) & 1))
            .collect::<Vec<u8>>(),
    )
}

pub(crate) fn string_bits(text: &str) -> Register {
    let mut bits = Vec::with_capacity(string_width(text));
    if text.is_empty() {
        return Register::from_u128(0, 8);
    }
    for byte in text.bytes() {
        for offset in (0..8).rev() {
            bits.push((byte >> offset) & 1);
        }
    }
    Register::from_bits(bits)
}

fn select_bound(expr: &Expression, store: &StateStore) -> Result<i64, EvalError> {
    let value = select_index(&eval(expr, store)?)?
        .ok_or_else(|| EvalError::NonConstantSelectBound(expr.to_contracted_string()))?;
    Ok(value)
}

/// A value read as a **position** in a vector or a memory, rather than as a
/// quantity: `None` when it is not a known number, or is too far from zero to
/// be an index of anything.
///
/// A position may be negative — `reg [3:0] value [-7:7];` and
/// `reg [base+15:base] big;` for a negative `base` are both ordinary Verilog —
/// so a *signed* value has to be read as the negative number it is. Reading
/// `-7` as `18446744073709551609` names no word of any memory, which turns a
/// write into a silent no-op and a read into `x` (corpus `negative_genvar`,
/// `signed_net_display`).
///
/// This is the one place that rule lives. Every select — a bit, a part, an
/// indexed part, and a memory word, reading and writing alike — comes through
/// it, so none of them can disagree about which word a design named.
pub fn select_index(value: &Register) -> Result<Option<i64>, EvalError> {
    let Some(bits) = numeric(value)? else {
        return Ok(None);
    };
    Ok(if value.is_signed() {
        i64::try_from(sign_extend_to_i128(bits, value.width())).ok()
    } else {
        i64::try_from(bits).ok()
    })
}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

fn eval_constant(constant: &VerilogConstant, signed_context: bool) -> Result<Register, EvalError> {
    let bits = constant_bits(constant.size(), constant.base_type(), constant.digits())?;
    Ok(bits.with_signedness(signed_context && constant.is_signed()))
}

/// Converts the pieces of a literal — its optional size, its base and its
/// digits as written — into bits. An absent size means
/// [`UNSIZED_CONSTANT_WIDTH`]; a size narrower than the digits truncates,
/// keeping the least significant bits. `_` separators are ignored.
/// Rebuilds the `<size>'<base><digits>` text of a literal. Used only to give
/// [`EvalError::MalformedConstant`] something legible to name — the digits
/// alone are empty for `4'b`, and misleading for `0'b1`, where the width is
/// what is wrong.
fn literal_text(size: Option<usize>, base: &VerilogBaseType, digits: &str) -> String {
    format!(
        "{}'{}{}",
        size.map(|size| size.to_string()).unwrap_or_default(),
        match base {
            VerilogBaseType::Binary => 'b',
            VerilogBaseType::Decimal => 'd',
            VerilogBaseType::Octal => 'o',
            VerilogBaseType::Hexadecimal => 'h',
        },
        digits
    )
}

fn constant_bits(
    size: Option<usize>,
    base: &VerilogBaseType,
    digits: &str,
) -> Result<Register, EvalError> {
    let malformed = || EvalError::MalformedConstant(literal_text(size, base, digits));

    let digits: String = digits.chars().filter(|c| *c != '_').collect();
    if digits.is_empty() {
        return Err(malformed());
    }

    // A written literal never carries a sign — the grammar reads `-1` as a
    // unary minus over `1`. `VerilogConstant::from_int` is the one producer
    // that can hand one over, which is how a genvar counting down reaches
    // `-1` (corpus `negative_genvar`, `br_gh567`), so the two's complement is
    // worked out here rather than teaching each base about a sign it cannot
    // otherwise see.
    if let Some(magnitude) = digits.strip_prefix('-') {
        let width = size.unwrap_or(UNSIZED_CONSTANT_WIDTH);
        let value = decimal_bits(magnitude)
            .map_err(|_| malformed())?
            .extend_msb(width)
            .to_u128()
            .ok_or_else(malformed)?;
        return Ok(Register::from_u128(value.wrapping_neg(), width).with_signedness(true));
    }

    // These helpers only see the digits, so restate their complaint in terms
    // of the whole literal. Both only ever report MalformedConstant.
    let bits = match base {
        VerilogBaseType::Binary => based_bits(&digits, 1),
        VerilogBaseType::Octal => based_bits(&digits, 3),
        VerilogBaseType::Hexadecimal => based_bits(&digits, 4),
        VerilogBaseType::Decimal => decimal_bits(&digits),
    }
    .map_err(|_| malformed())?;

    let width = size.unwrap_or(UNSIZED_CONSTANT_WIDTH);
    if width == 0 {
        return Err(malformed());
    }

    Ok(bits.extend_msb(width))
}

/// Splits a `<size>'<base><digits>` literal and hands the pieces to
/// [`constant_bits`].
fn constant_register(token: &str) -> Result<Register, EvalError> {
    let malformed = || EvalError::MalformedConstant(token.to_string());

    let (size_text, rest) = token.split_once('\'').ok_or_else(malformed)?;
    let mut rest = rest.chars();
    let base = match rest.next().ok_or_else(malformed)?.to_ascii_lowercase() {
        'b' => VerilogBaseType::Binary,
        'o' => VerilogBaseType::Octal,
        'h' => VerilogBaseType::Hexadecimal,
        'd' => VerilogBaseType::Decimal,
        _ => return Err(malformed()),
    };

    let size = if size_text.is_empty() {
        None
    } else {
        Some(size_text.parse::<usize>().map_err(|_| malformed())?)
    };

    constant_bits(size, &base, rest.as_str())
}

/// Expands binary / octal / hex digits, `bits_per_digit` bits each. An `x` or
/// `z` digit expands to that many `x` or `z` bits.
fn based_bits(digits: &str, bits_per_digit: usize) -> Result<Register, EvalError> {
    let radix = 1u32 << bits_per_digit;
    let mut bits = Vec::with_capacity(digits.len() * bits_per_digit);
    for digit in digits.chars() {
        match digit.to_ascii_lowercase() {
            'x' => bits.extend(std::iter::repeat(X).take(bits_per_digit)),
            'z' | '?' => bits.extend(std::iter::repeat(Z).take(bits_per_digit)),
            other => {
                let value = other
                    .to_digit(radix)
                    .ok_or_else(|| EvalError::MalformedConstant(digits.to_string()))?;
                bits.extend((0..bits_per_digit).rev().map(|i| ((value >> i) & 1) as u8));
            }
        }
    }
    Ok(Register::from_bits(bits))
}

/// Decimal digits, rendered in the fewest bits that hold the value.
///
/// A decimal has **no unknown digit**, so the whole value is unknown or none of
/// it is: `7'dx` is one `x` bit, which the size then extends into `xxxxxxx` the
/// way a leading digit extends in any other base. That is IEEE 1364-2005 §3.5.1
/// and it is why `2'dx` is two bits rather than seven — written into a
/// `reg [6:0]` it is `00000xx` unsigned and `xxxxxxx` signed, exactly as a known
/// two bit value would be (corpus `pr1792734`, measured against iverilog 12.0).
/// `_` separators are already gone by the time the digits arrive here, so `7'dx_`
/// is the same literal.
fn decimal_bits(digits: &str) -> Result<Register, EvalError> {
    if digits.len() == 1 {
        match digits.as_bytes()[0].to_ascii_lowercase() {
            b'x' => return Ok(Register::from_bits(vec![X])),
            b'z' | b'?' => return Ok(Register::from_bits(vec![Z])),
            _ => {}
        }
    }
    let value = digits
        .parse::<u128>()
        .map_err(|_| EvalError::MalformedConstant(digits.to_string()))?;
    let width = (128 - value.leading_zeros() as usize).max(1);
    Ok(Register::from_u128(value, width))
}

// ---------------------------------------------------------------------------
// Unary operators
// ---------------------------------------------------------------------------

fn eval_unary(op: &UnaryOperator, operand: &Register) -> Result<Register, EvalError> {
    if operand.is_real() {
        return real_unary(op, operand);
    }
    match op {
        // `+a` is a no-op on the bits, and leaves the operand's signedness
        // alone with them.
        UnaryOperator::Positive => Ok(operand.clone()),
        // Two's complement in the operand's own width. Any unknown bit makes
        // the whole result unknown, because a carry can reach every bit.
        // Negating an *unsigned* value is still unsigned — `-4'd12` is `4'd4`,
        // and only a wider context could make it -12.
        UnaryOperator::Negative => {
            let width = operand.width().max(1);
            let signed = operand.is_signed();
            match numeric(operand)? {
                Some(value) => Ok(Register::from_u128(
                    value.wrapping_neg() & width_mask(width),
                    width,
                )
                .with_signedness(signed)),
                None => Ok(Register::unknown(width).with_signedness(signed)),
            }
        }
        // Bit for bit, width preserving. `z` inverts to `x`, matching Verilog:
        // an undriven bit is not a known 0 or 1.
        UnaryOperator::BitwiseNegation => Ok(operand
            .map_chunks(|bits| Chunk {
                value: bits.zeros(),
                unknown: bits.unknown,
            })
            .with_signedness(operand.is_signed())),
        // One bit: true when the operand is all zero.
        UnaryOperator::LogicalNegation => Ok(logic_bit(match truth(operand) {
            Some(true) => ZERO,
            Some(false) => ONE,
            None => X,
        })),
        UnaryOperator::ReductionAnd => Ok(logic_bit(reduce_and(operand))),
        UnaryOperator::ReductionNand => Ok(logic_bit(invert(reduce_and(operand)))),
        UnaryOperator::ReductionOr => Ok(logic_bit(reduce_or(operand))),
        UnaryOperator::ReductionNor => Ok(logic_bit(invert(reduce_or(operand)))),
        UnaryOperator::ReductionXor => Ok(logic_bit(reduce_xor(operand))),
        UnaryOperator::ReductionXnor => Ok(logic_bit(invert(reduce_xor(operand)))),
    }
}

/// `&a`: a single 0 forces 0 even when other bits are unknown.
fn reduce_and(operand: &Register) -> u8 {
    if operand.has_zero() {
        ZERO
    } else if operand.has_unknown() {
        X
    } else {
        ONE
    }
}

/// `|a`: a single 1 forces 1 even when other bits are unknown.
fn reduce_or(operand: &Register) -> u8 {
    if operand.has_one() {
        ONE
    } else if operand.has_unknown() {
        X
    } else {
        ZERO
    }
}

/// `^a`: parity. Unlike and/or there is no dominant value, so any unknown bit
/// makes the whole result unknown.
fn reduce_xor(operand: &Register) -> u8 {
    if operand.has_unknown() {
        return X;
    }
    if operand.count_ones() % 2 == 0 {
        ZERO
    } else {
        ONE
    }
}

// ---------------------------------------------------------------------------
// Binary operators
// ---------------------------------------------------------------------------

fn eval_binary(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Result<Register, EvalError> {
    // One operand being real makes the whole operation real: `7/2.0` is 3.5
    // where `7/2` is 3. It is read off the operands *after* they are evaluated
    // rather than decided before, because — unlike signedness — realness
    // changes nothing about how an operand is evaluated, only what is done
    // with it. `7/2 + 0.5` is 3.5 for exactly that reason: the division is an
    // integer one and only the addition is real.
    if lhs.is_real() || rhs.is_real() {
        return real_binary(op, lhs, rhs);
    }
    match op {
        BinaryOperator::Addition
        | BinaryOperator::Subtraction
        | BinaryOperator::Multiplication
        | BinaryOperator::Division
        | BinaryOperator::Modulus => arithmetic(op, lhs, rhs),
        BinaryOperator::Power => power(lhs, rhs),

        BinaryOperator::BitwiseAnd
        | BinaryOperator::BitwiseOr
        | BinaryOperator::BitwiseInclusiveOr
        | BinaryOperator::BitwiseXOr
        | BinaryOperator::BitwiseXNor => Ok(bitwise(op, lhs, rhs)),

        BinaryOperator::ShiftLeft
        | BinaryOperator::ArithmeticShiftLeft
        | BinaryOperator::ShiftRight
        | BinaryOperator::ArithmeticShiftRight => shift(op, lhs, rhs),

        BinaryOperator::LessThan
        | BinaryOperator::LessThanOrEqual
        | BinaryOperator::GreaterThan
        | BinaryOperator::GreaterThanOrEqual => relational(op, lhs, rhs),

        BinaryOperator::LogicalEquality | BinaryOperator::LogicalInequality => {
            Ok(logical_equality(op, lhs, rhs))
        }
        BinaryOperator::CaseEquality | BinaryOperator::CaseInequality => {
            Ok(case_equality(op, lhs, rhs))
        }

        BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => Ok(logical(op, lhs, rhs)),
    }
}

/// A binary operation with a real operand, carried out in `f64`.
///
/// The other operand is *converted*, whatever it was: an integer beside a real
/// is read as the number its own signedness says it is and then turned into a
/// double, so `7/2.0` divides 7.0 by 2.0. There is no unknown to propagate —
/// a double has no `x` — and a division by zero is an infinity rather than the
/// `x` an integer division by zero gives, which is what iverilog prints.
///
/// The operators that are **not** here are the ones that read a pattern of
/// bits: `& | ^ ~^`, the shifts and `===`/`!==`. iverilog rejects each of them
/// at compile time and so does this, by name, rather than converting to an
/// integer behind the design's back.
#[cold]
fn real_binary(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Result<Register, EvalError> {
    let (a, b) = (lhs.to_f64(), rhs.to_f64());
    let real = |value: f64| Ok(Register::from_f64(value));
    let boolean = |value: bool| Ok(logic_bit(if value { ONE } else { ZERO }));
    match op {
        BinaryOperator::Addition => real(a + b),
        BinaryOperator::Subtraction => real(a - b),
        BinaryOperator::Multiplication => real(a * b),
        BinaryOperator::Division => real(a / b),
        // `%` on a real is `fmod`, not a remainder of the rounded values:
        // `1 % 2.0` is 1.0 and `2.5 % 2` is 0.5. IEEE 1364-2005 leaves it
        // illegal; iverilog computes it, and corpus `mixed_type_div_mod`
        // asserts the answers above.
        BinaryOperator::Modulus => real(a % b),
        BinaryOperator::Power => real(a.powf(b)),
        BinaryOperator::LessThan => boolean(a < b),
        BinaryOperator::LessThanOrEqual => boolean(a <= b),
        BinaryOperator::GreaterThan => boolean(a > b),
        BinaryOperator::GreaterThanOrEqual => boolean(a >= b),
        BinaryOperator::LogicalEquality => boolean(a == b),
        BinaryOperator::LogicalInequality => boolean(a != b),
        BinaryOperator::LogicalAnd => boolean(a != 0.0 && b != 0.0),
        BinaryOperator::LogicalOr => boolean(a != 0.0 || b != 0.0),
        other => Err(EvalError::RealOperand(other.raw_token())),
    }
}

/// A unary operation on a real. `+` and `-` are arithmetic and `!` is a truth
/// value; `~` and the reductions read bits and are refused by name, exactly as
/// they are in [`real_binary`].
#[cold]
fn real_unary(op: &UnaryOperator, operand: &Register) -> Result<Register, EvalError> {
    let value = operand.to_f64();
    match op {
        UnaryOperator::Positive => Ok(Register::from_f64(value)),
        UnaryOperator::Negative => Ok(Register::from_f64(-value)),
        UnaryOperator::LogicalNegation => Ok(logic_bit(if value == 0.0 { ONE } else { ZERO })),
        other => Err(EvalError::RealOperand(other.raw_token())),
    }
}

/// `+ - * / %`. The result is as wide as the wider operand and wraps at that
/// width. Any unknown bit in either operand makes the entire result `x`, since
/// carries let one unknown bit reach any output bit. Division or modulus by
/// zero is `x`, as in Verilog.
fn arithmetic(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Result<Register, EvalError> {
    let (width, signed, values) = align_numeric(lhs, rhs)?;
    let Some((a, b)) = values else {
        return Ok(Register::unknown(width).with_signedness(signed));
    };

    let mask = width_mask(width);
    // Both operands read as `width` bits by now, so the two's complement value
    // of each is a reinterpretation of the bits rather than another widening.
    let two_s_complement = || (sign_extend_to_i128(a, width), sign_extend_to_i128(b, width));
    let value = match op {
        // Add, subtract and multiply produce the same bits either way round,
        // which is the whole point of two's complement.
        BinaryOperator::Addition => a.wrapping_add(b) & mask,
        BinaryOperator::Subtraction => a.wrapping_sub(b) & mask,
        BinaryOperator::Multiplication => a.wrapping_mul(b) & mask,
        BinaryOperator::Division | BinaryOperator::Modulus if b == 0 => {
            return Ok(Register::unknown(width).with_signedness(signed));
        }
        // Division and modulus do not. Rust truncates a quotient toward zero
        // and gives a remainder the sign of the dividend, which is exactly what
        // Verilog asks for. `wrapping_` is for the one overflowing case,
        // `-2**(n-1) / -1`, which Verilog wraps back to itself.
        BinaryOperator::Division if signed => {
            let (a, b) = two_s_complement();
            (a.wrapping_div(b) as u128) & mask
        }
        BinaryOperator::Modulus if signed => {
            let (a, b) = two_s_complement();
            (a.wrapping_rem(b) as u128) & mask
        }
        BinaryOperator::Division => (a / b) & mask,
        BinaryOperator::Modulus => (a % b) & mask,
        other => unreachable!("{} is not an arithmetic operator", other),
    };
    Ok(Register::from_u128(value, width).with_signedness(signed))
}

/// `**` takes the width of its left operand, per IEEE 1364 table 5-22.
///
/// A *negative* exponent cannot produce a fraction in integer arithmetic, so
/// Verilog collapses it to one of a handful of answers (IEEE 1364 table 5-6):
/// `1 ** -n` is 1, `-1 ** -n` alternates, `0 ** -n` is `x` and everything else
/// is 0.
fn power(lhs: &Register, rhs: &Register) -> Result<Register, EvalError> {
    let width = lhs.width().max(1);
    let signed = lhs.is_signed() && rhs.is_signed();
    let (Some(base), Some(exponent)) = (numeric(lhs)?, numeric(rhs)?) else {
        return Ok(Register::unknown(width).with_signedness(signed));
    };

    let mask = width_mask(width);
    let exponent_value = sign_extend_to_i128(exponent, rhs.width());
    if rhs.is_signed() && exponent_value < 0 {
        let base = if lhs.is_signed() {
            sign_extend_to_i128(base, lhs.width())
        } else {
            base as i128
        };
        let value: i128 = match base {
            0 => return Ok(Register::unknown(width).with_signedness(signed)),
            1 => 1,
            -1 if exponent_value % 2 == 0 => 1,
            -1 => -1,
            _ => 0,
        };
        return Ok(Register::from_u128((value as u128) & mask, width).with_signedness(signed));
    }
    let mut result = 1u128 & mask;
    let mut base = base & mask;
    let mut exponent = exponent;
    while exponent > 0 {
        if exponent & 1 == 1 {
            result = result.wrapping_mul(base) & mask;
        }
        base = base.wrapping_mul(base) & mask;
        exponent >>= 1;
    }
    Ok(Register::from_u128(result, width).with_signedness(signed))
}

/// `& | ^ ^~` applied bit by bit. The narrower operand is widened to the width
/// of the wider one — sign extended when both operands are signed, zero
/// extended otherwise.
fn bitwise(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Register {
    let width = lhs.width().max(rhs.width()).max(1);
    let signed = lhs.is_signed() && rhs.is_signed();
    // `zip_chunks` already reads a narrower operand's missing bits as zeros, so
    // only a signed operation has anything to widen.
    let extended;
    let (lhs, rhs) = if signed && lhs.width() != rhs.width() {
        extended = (lhs.sign_extended(width), rhs.sign_extended(width));
        (&extended.0, &extended.1)
    } else {
        (lhs, rhs)
    };
    lhs.zip_chunks(rhs, width, |a, b| bitwise_chunk(op, a, b))
        .with_signedness(signed)
}

/// The truth tables of IEEE 1364 table 5-1, a chunk of bits at a time. `z`
/// behaves exactly like `x`: a bit that is not driven is not a known value.
///
/// Each table is the same statement made of whole words: for `&`, a result bit
/// is `1` where both operands are a known `1`, and unknown where either operand
/// is unknown and neither is the dominant `0`. Bits past an operand's width
/// read as a known `0`, which is the zero extension the narrower operand gets.
fn bitwise_chunk(op: &BinaryOperator, a: Chunk, b: Chunk) -> Chunk {
    match op {
        BinaryOperator::BitwiseAnd => Chunk {
            value: a.ones() & b.ones(),
            unknown: (a.unknown | b.unknown) & !(a.zeros() | b.zeros()),
        },
        BinaryOperator::BitwiseOr | BinaryOperator::BitwiseInclusiveOr => Chunk {
            value: a.ones() | b.ones(),
            unknown: (a.unknown | b.unknown) & !(a.ones() | b.ones()),
        },
        BinaryOperator::BitwiseXOr => {
            let unknown = a.unknown | b.unknown;
            Chunk {
                value: (a.value ^ b.value) & !unknown,
                unknown,
            }
        }
        BinaryOperator::BitwiseXNor => {
            let unknown = a.unknown | b.unknown;
            Chunk {
                value: !(a.value ^ b.value) & !unknown,
                unknown,
            }
        }
        other => unreachable!("{} is not a bitwise operator", other),
    }
}

/// Shifts move bits rather than numbers, so `x` and `z` bits survive being
/// shifted. The result keeps the left operand's width — and its signedness,
/// since the right operand only says how far — and an unknown shift amount
/// makes the whole result `x`.
///
/// `>>>` on a *signed* left operand replicates the sign bit into the vacated
/// positions instead of filling them with `0`, which is the one thing that
/// tells it apart from `>>`. On an unsigned operand the two are the same
/// operation, and `<<<` is always `<<`.
fn shift(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Result<Register, EvalError> {
    let width = lhs.width().max(1);
    let signed = lhs.is_signed();
    let Some(amount) = numeric(rhs)? else {
        return Ok(Register::unknown(width).with_signedness(signed));
    };
    let amount = amount.min(lhs.width() as u128) as usize;

    let shifted = match op {
        BinaryOperator::ShiftLeft | BinaryOperator::ArithmeticShiftLeft => lhs.shifted_left(amount),
        BinaryOperator::ArithmeticShiftRight if signed => lhs.shifted_right_signed(amount),
        _ => lhs.shifted_right(amount),
    };
    Ok(shifted.with_signedness(signed))
}

/// `< <= > >=` produce one bit — an unsigned one, whatever they compared. The
/// comparison itself is two's complement when *both* operands are signed and a
/// magnitude comparison otherwise, so `-1 < 0` is true between two signed
/// operands and false the moment either side is unsigned. An unknown bit in
/// either operand makes the answer `x`.
fn relational(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Result<Register, EvalError> {
    let (width, signed, values) = align_numeric(lhs, rhs)?;
    let Some((a, b)) = values else {
        return Ok(Register::unknown(1));
    };
    let ordering = if signed {
        sign_extend_to_i128(a, width).cmp(&sign_extend_to_i128(b, width))
    } else {
        a.cmp(&b)
    };
    let result = match op {
        BinaryOperator::LessThan => ordering.is_lt(),
        BinaryOperator::LessThanOrEqual => ordering.is_le(),
        BinaryOperator::GreaterThan => ordering.is_gt(),
        BinaryOperator::GreaterThanOrEqual => ordering.is_ge(),
        other => unreachable!("{} is not a relational operator", other),
    };
    Ok(logic_bit(if result { ONE } else { ZERO }))
}

/// `==` and `!=` produce one bit. The narrower operand is widened first, sign
/// extended when both sides are signed — which is what makes
/// `-1 == 32'hffffffff` true and `$signed(4'b1111) == 4'd15` false.
///
/// **An unknown bit only makes the answer unknown when it could still change
/// it.** A *known* pair of bits that disagrees settles the question whatever
/// the unknown ones hold — the two values cannot be equal — so
/// `4'bxxx0 != 4'b0001` is `1` and `4'bxxx0 == 4'b0001` is `0`, where
/// `4'bxxx1 != 4'b0001` is `x`. That is what iverilog 12.0 answers; the LRM's
/// flat "`x` if either operand contains an `x` or `z`" is the coarser reading,
/// and taking it turns a determinable comparison into an `x` that then
/// poisons everything built on it.
fn logical_equality(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Register {
    if lhs.has_unknown() || rhs.has_unknown() {
        if !known_bits_differ(lhs, rhs) {
            return Register::unknown(1);
        }
        let differ = matches!(op, BinaryOperator::LogicalInequality);
        return logic_bit(if differ { ONE } else { ZERO });
    }
    let matched = matches!(op, BinaryOperator::LogicalEquality) == equal_values(lhs, rhs);
    logic_bit(if matched { ONE } else { ZERO })
}

/// Whether any pair of *known* bits disagrees, once the narrower operand has
/// been widened the way [`equal_values`] widens it.
///
/// Only [`logical_equality`] asks, and only when one side already has an
/// unknown bit in it, so the walk never touches the ordinary comparison.
#[cold]
fn known_bits_differ(lhs: &Register, rhs: &Register) -> bool {
    let width = lhs.width().max(rhs.width());
    let widen = |value: &Register| {
        if lhs.is_signed() && rhs.is_signed() {
            value.sign_extended(width)
        } else {
            value.resize(width)
        }
    };
    let (left, right) = (widen(lhs), widen(rhs));
    (0..width).any(
        |index| match (left.bit_from_lsb(index), right.bit_from_lsb(index)) {
            (Some(a), Some(b)) => matches!((a, b), (ZERO, ONE) | (ONE, ZERO)),
            _ => false,
        },
    )
}

/// `===` and `!==` compare all four states exactly and are never `x`. The
/// narrower operand is widened the same way [`logical_equality`] widens it, so
/// `4'b0001 === 1'b1` holds.
fn case_equality(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Register {
    let matched = matches!(op, BinaryOperator::CaseEquality) == equal_values(lhs, rhs);
    logic_bit(if matched { ONE } else { ZERO })
}

/// `&&` and `||` collapse each side to a single truth value and produce one
/// bit. A dominant operand decides the result even when the other is unknown:
/// `0 && x` is 0 and `1 || x` is 1.
fn logical(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Register {
    let (a, b) = (truth(lhs), truth(rhs));
    let value = match op {
        BinaryOperator::LogicalAnd => match (a, b) {
            (Some(false), _) | (_, Some(false)) => ZERO,
            (Some(true), Some(true)) => ONE,
            _ => X,
        },
        BinaryOperator::LogicalOr => match (a, b) {
            (Some(true), _) | (_, Some(true)) => ONE,
            (Some(false), Some(false)) => ZERO,
            _ => X,
        },
        other => unreachable!("{} is not a logical operator", other),
    };
    logic_bit(value)
}

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

/// `0` and `1` swap; `x` and `z` invert to `x`.
fn invert(bit: u8) -> u8 {
    match bit {
        ZERO => ONE,
        ONE => ZERO,
        _ => X,
    }
}

fn logic_bit(bit: u8) -> Register {
    Register::filled(1, bit)
}

/// A register used as a condition: any `1` bit is true, all-zero is false, and
/// anything else (only unknown bits and zeros) is unknown.
fn truth(register: &Register) -> Option<bool> {
    if register.has_one() {
        // Every real with a bit set is true except `-0.0`, whose sign bit is
        // the one place the bits answer differently from the number. Asking
        // here rather than first is what keeps the question off a value that
        // is plainly false or unknown.
        if register.is_real() {
            return Some(register.to_f64() != 0.0);
        }
        Some(true)
    } else if register.has_unknown() {
        None
    } else {
        Some(false)
    }
}

/// Both operands of a binary operator as numbers at a common width, with that
/// width and whether the operation is signed. `None` means an operand had an
/// unknown bit, so there is no number to work with.
///
/// Two rules meet here, and they are the whole of Verilog's signedness
/// propagation. The operation is signed only when **both** operands are — one
/// unsigned operand makes the result unsigned — and the widening that brings
/// the narrower operand up replicates its sign bit exactly when the operation
/// is signed. An unsigned operation therefore reads a signed operand as the
/// plain bit pattern it is, which is what `$signed(a) | b` does.
///
/// The widening is arithmetic rather than a pair of new registers: extending a
/// value is a shift and a mask, and building two registers to throw away is
/// work the arithmetic and relational operators do not need.
type AlignedOperands = (usize, bool, Option<(u128, u128)>);

fn align_numeric(lhs: &Register, rhs: &Register) -> Result<AlignedOperands, EvalError> {
    let width = lhs.width().max(rhs.width()).max(1);
    let signed = lhs.is_signed() && rhs.is_signed();
    let (Some(a), Some(b)) = (numeric(lhs)?, numeric(rhs)?) else {
        return Ok((width, signed, None));
    };
    if !signed {
        // Zero extension is what the number already is.
        return Ok((width, signed, Some((a, b))));
    }
    let mask = width_mask(width);
    let extend = |value: u128, from: usize| (sign_extend_to_i128(value, from) as u128) & mask;
    Ok((
        width,
        signed,
        Some((extend(a, lhs.width()), extend(b, rhs.width()))),
    ))
}

/// Whether two values are the same bits once the narrower one is widened — sign
/// extended when both sides are signed, zero extended otherwise.
fn equal_values(lhs: &Register, rhs: &Register) -> bool {
    if lhs.width() == rhs.width() {
        return lhs == rhs;
    }
    let width = lhs.width().max(rhs.width());
    if lhs.is_signed() && rhs.is_signed() {
        lhs.sign_extended(width) == rhs.sign_extended(width)
    } else {
        lhs.resize(width) == rhs.resize(width)
    }
}

/// The unsigned value of `register`, or `None` when it has unknown bits.
fn numeric(register: &Register) -> Result<Option<u128>, EvalError> {
    if register.width() > MAX_ARITHMETIC_WIDTH {
        return Err(EvalError::WidthOverflow(register.width()));
    }
    Ok(register.to_u128())
}

fn width_mask(width: usize) -> u128 {
    if width >= 128 {
        u128::MAX
    } else {
        (1u128 << width) - 1
    }
}

/// Bit-wise merge of two candidate results: bits that agree survive, bits that
/// disagree become `x`. Used when a conditional's condition is unknown.
fn merge(lhs: &Register, rhs: &Register) -> Register {
    let width = lhs.width().max(rhs.width()).max(1);
    lhs.zip_chunks(rhs, width, |a, b| {
        // Bits where both planes agree keep their value; the rest become `x`.
        let same = !(a.value ^ b.value) & !(a.unknown ^ b.unknown);
        Chunk {
            value: a.value & same,
            unknown: (a.unknown & same) | !same,
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::expr::verilog_expression;

    fn parse(source: &str) -> Expression {
        let (remaining, expr) = verilog_expression(source).expect("expression should parse");
        assert!(
            remaining.trim().is_empty(),
            "unparsed remainder {:?} for {:?}",
            remaining,
            source
        );
        expr
    }

    /// Evaluates against a store and returns the result as a binary string.
    fn bits_in(source: &str, store: &StateStore) -> String {
        eval(&parse(source), store)
            .unwrap_or_else(|e| panic!("{} failed to evaluate: {}", source, e))
            .to_binary()
    }

    fn bits(source: &str) -> String {
        bits_in(source, &StateStore::new())
    }

    /// Evaluates against a store and returns the numeric value.
    fn value_in(source: &str, store: &StateStore) -> u128 {
        eval(&parse(source), store)
            .unwrap_or_else(|e| panic!("{} failed to evaluate: {}", source, e))
            .to_u128()
            .unwrap_or_else(|| panic!("{} evaluated to a non-numeric value", source))
    }

    fn value(source: &str) -> u128 {
        value_in(source, &StateStore::new())
    }

    fn error(source: &str, store: &StateStore) -> EvalError {
        eval(&parse(source), store).expect_err("expected an evaluation error")
    }

    /// `reg [7:0] a = 8'b1010_0110; reg [3:0] b = 4'b0011;`
    fn sample_store() -> StateStore {
        let mut store = StateStore::new();
        store.set_ranged("a", Register::from_binary("10100110"), (7, 0));
        store.set_ranged("b", Register::from_binary("0011"), (3, 0));
        store
    }

    /// A context-determined operator sizes its two operands against *each
    /// other* as well as against the context it was given, so an operation
    /// nested inside one is carried out at the widest of the three rather than
    /// at its own width and padded afterwards.
    ///
    /// Every number here was measured against iverilog 12.0:
    ///
    /// ```verilog
    /// reg [3:0] c, b;  c = 4'b1111; b = 4'b1111;
    /// $display("%b", (c & ~(1'b1 << 9'h00)) & b);   // 1110   (corpus pr2985542)
    ///
    /// reg [15:0] a, bb, answer;  a = 16'h8000; bb = 16'h8000;
    /// answer = (a + bb + 0) >> 1;                   // 16'h8000 (corpus pr1570635b)
    ///
    /// reg [7:0] x, y;  x = 8'hA6; y = 8'h3C;
    /// $display("%0d", ((x + y) * 2) - (x & y));     // 416, $bits is 32
    /// ```
    ///
    /// The last is `benches/simulation.rs`'s `eval/nested_arithmetic`: the
    /// unsized `2` makes the whole expression thirty-two bits, so the benchmark
    /// went from eight bit arithmetic that answered 160 to thirty-two bit
    /// arithmetic that answers 416.
    #[test]
    fn test_an_operation_is_sized_against_the_operand_beside_it() {
        let mut store = StateStore::new();
        store.set_ranged("c", Register::from_binary("1111"), (3, 0));
        store.set_ranged("b", Register::from_binary("1111"), (3, 0));
        // `~` has to invert four bits, not one bit padded with zeros.
        assert_eq!(bits_in("(c & ~(1'b1 << 9'h00)) & b", &store), "1110");
        assert_eq!(value_in("|((c & ~(1'b1 << 9'h00)) & b)", &store), 1);

        // The unsized `0` is thirty-two bits, so the addition does not wrap at
        // the sixteen bit target's width — `eval_sized` is a lower bound.
        let mut wide = StateStore::new();
        wide.set_ranged("a", Register::from_u128(0x8000, 16), (15, 0));
        wide.set_ranged("d", Register::from_u128(0x8000, 16), (15, 0));
        let answer = eval_sized(&parse("(a + d + 0) >> 1"), &wide, 16).expect("should evaluate");
        assert_eq!(answer.to_u128(), Some(0x8000));

        // An eight bit pair beside an unsized literal: the whole expression is
        // thirty-two bits and the product does not wrap.
        let mut bench = StateStore::new();
        bench.set_ranged("x", Register::from_u128(0xA6, 8), (7, 0));
        bench.set_ranged("y", Register::from_u128(0x3C, 8), (7, 0));
        assert_eq!(value_in("((x + y) * 2) - (x & y)", &bench), 416);
    }

    /// A **real** operand has no width to share, so nothing beside it is
    /// widened to the real's sixty-four bits — and the question cannot be
    /// answered from `StateStore::any_real`, which reports *declarations*.
    ///
    /// Corpus `pr1574175` is the case, and iverilog 12.0 prints
    /// `Both of these should be the same (3):   3,   3` for:
    ///
    /// ```verilog
    /// integer correct, incorrect;  reg [5:0] bits;  bits = 32;
    /// incorrect = -180 + bits*(360.0/63.0);
    /// correct   = bits*(360.0/63.0) - 180;
    /// ```
    /// Both sides are real sums, and they agree only once the integer written
    /// into them wraps at thirty-two bits: `bits` is unsigned, so the whole
    /// addition is, and `-180` is `2**32 - 180`. That is what makes this a
    /// sharp test — widening the integer to the real's sixty-four bits instead
    /// gives `2**64 - 180`, which no longer lands on 3 when it is written.
    #[test]
    fn test_an_undeclared_real_still_shares_no_width() {
        let mut store = StateStore::new();
        store.set_ranged("bits", Register::from_u128(32, 6), (5, 0));

        let at_32_bits = 4294967296.0 - 180.0 + 360.0 / 63.0 * 32.0;
        let sum = eval_sized(&parse("-180 + bits * (360.0 / 63.0)"), &store, 32)
            .expect("should evaluate");
        assert!(
            (sum.to_f64() - at_32_bits).abs() < 1e-6,
            "expected {}, got {}",
            at_32_bits,
            sum.to_f64()
        );

        // The other order needs no wrap at all, and the two agree once each is
        // written into a thirty-two bit `integer`.
        let difference =
            eval_sized(&parse("bits * (360.0 / 63.0) - 180"), &store, 32).expect("should evaluate");
        assert!((difference.to_f64() - (360.0 / 63.0 * 32.0 - 180.0)).abs() < 1e-6);
        assert_eq!(sum.to_f64().round() as u64 as u32, 3);
        assert_eq!(difference.to_f64().round() as u32, 3);
    }

    // -- reals -------------------------------------------------------------

    /// Evaluates against a store and returns the result as a double.
    fn real_in(source: &str, store: &StateStore) -> f64 {
        eval(&parse(source), store)
            .unwrap_or_else(|e| panic!("{} failed to evaluate: {}", source, e))
            .to_f64()
    }

    fn real(source: &str) -> f64 {
        real_in(source, &StateStore::new())
    }

    /// One real operand makes the whole operation real — and, unlike
    /// signedness, that is decided from the operands rather than pushed down
    /// into them: `7/2 + 0.5` divides in integers first. Every value here was
    /// measured from iverilog 12.0.
    #[test]
    fn test_one_real_operand_makes_the_operation_real() {
        assert_eq!(real("7 / 2.0"), 3.5);
        assert_eq!(real("7 / 2"), 3.0);
        assert_eq!(real("7 / 2 + 0.5"), 3.5);
        assert_eq!(real("2.0 ** 3"), 8.0);
        // `%` on a real is `fmod`; IEEE 1364 leaves it illegal and iverilog
        // computes it (corpus `mixed_type_div_mod`).
        assert_eq!(real("1 % 2.0"), 1.0);
        assert_eq!(real("1.0 / 0.0"), f64::INFINITY);
    }

    /// A real is signed, and that is what keeps the *integer* beside it signed:
    /// `-1 / 1.0e-6` is negative only if the `-1` is read as one (corpus
    /// `pr2818823`).
    #[test]
    fn test_a_real_keeps_the_integer_beside_it_signed() {
        assert_eq!(real("-1 / 1.0e-6"), -1000000.0);
        assert_eq!(real("-1 * (1.0 / 0.0)"), f64::NEG_INFINITY);
    }

    /// A comparison sizes its operands against each other — but a real has no
    /// width to share, so the integer beside it is worked out at its own width
    /// and converted afterwards. Two eight bit `255`s add to 254, and widening
    /// them to the real's sixty-four first would give 510 (corpus `pr2918095`).
    #[test]
    fn test_a_real_gives_a_comparison_no_width_to_share() {
        let mut store = StateStore::new();
        store.declare_real("r");
        store.set_ranged("a", Register::from_binary("11111111"), (7, 0));
        store.set_ranged("b", Register::from_binary("11111111"), (7, 0));
        assert_eq!(value_in("(a + b) == 254.0", &store), 1);
        assert_eq!(value_in("(a * b) == 1.0", &store), 1);
    }

    /// One real arm makes a conditional real, and that has to be decided before
    /// the condition picks an arm. An unknown condition has no `x` to produce,
    /// so it gives what the arms agree on and `0.0` when they disagree —
    /// iverilog's answer, and what corpus `pr2453002` checks.
    #[test]
    fn test_a_conditional_is_real_when_either_arm_is() {
        let mut store = StateStore::new();
        store.declare_real("r");
        assert_eq!(real_in("1 ? 1 : 2.5", &store), 1.0);
        assert_eq!(real_in("(1 ? 1 : 2.5) / 2", &store), 0.5);
        assert_eq!(real_in("1'bx ? 6 : 6.0", &store), 6.0);
        assert_eq!(real_in("1'bx ? 6.0 : 7.0", &store), 0.0);
    }

    /// The conversions, and the difference between them: an assignment rounds
    /// half away from zero, `$rtoi` truncates toward zero, and `$itor` rounds
    /// because its argument is an integer (corpus `itor_rtoi`).
    #[test]
    fn test_the_real_conversion_functions() {
        assert_eq!(value("$rtoi(2.7)"), 2);
        assert_eq!(real("$itor(10.5)"), 11.0);
        assert_eq!(real("$itor(1.0 / 0.0)"), 0.0);
        assert_eq!(real("$bitstoreal(64'h3ff8000000000000)"), 1.5);
        assert_eq!(
            bits("$realtobits(1.5)"),
            Register::from_hex("3ff8000000000000").to_binary()
        );
    }

    // -- constants ---------------------------------------------------------

    #[test]
    fn test_binary_constant() {
        assert_eq!(bits("4'b1010"), "1010");
        assert_eq!(bits("8'b1010"), "00001010");
        assert_eq!(bits("1'b1"), "1");
    }

    #[test]
    fn test_hex_constant() {
        assert_eq!(bits("8'hFF"), "11111111");
        assert_eq!(bits("8'hac"), "10101100");
        assert_eq!(value("16'hEAC2"), 0xEAC2);
    }

    #[test]
    fn test_octal_constant() {
        assert_eq!(bits("6'o54"), "101100");
        assert_eq!(value("9'o732"), 0o732);
    }

    #[test]
    fn test_decimal_constant() {
        assert_eq!(bits("4'd6"), "0110");
        assert_eq!(value("8'd172"), 172);
        assert_eq!(bits("4'd0"), "0000");
    }

    #[test]
    fn test_unsized_constant_is_32_bits() {
        let register = eval(&parse("42"), &StateStore::new()).unwrap();
        assert_eq!(register.width(), UNSIZED_CONSTANT_WIDTH);
        assert_eq!(register.to_u128(), Some(42));
    }

    /// An unsized literal whose leading digit is `x` or `z` fills whatever
    /// width it is written against, where a *sized* one is the ordinary value
    /// its own width made it and zero pads. Measured against iverilog 12.0:
    ///
    /// ```text
    /// reg [63:0] p;
    /// p = 'hx;   $display("%b", p);  // 64 x's
    /// p = 'hz;   $display("%h", p);  // zzzzzzzzzzzzzzzz
    /// p = 'hx1;  $display("%h", p);  // xxxxxxxxxxxxxxx1
    /// p = 4'bx;  $display("%h", p);  // 000000000000000x
    /// ```
    #[test]
    fn test_unsized_unknown_literal_fills_its_context() {
        let store = StateStore::new();
        let sized = |source: &str, width: usize| {
            eval_sized(&parse(source), &store, width)
                .unwrap_or_else(|e| panic!("{}: {}", source, e))
                .to_binary()
        };
        assert_eq!(sized("'hx", 64), "x".repeat(64));
        assert_eq!(sized("'hz", 64), "z".repeat(64));
        assert_eq!(sized("'hx1", 64), format!("{}0001", "x".repeat(60)));
        // A sized literal already said how wide it is; a context wider than
        // that pads it with zeros like any other value.
        assert_eq!(sized("4'bx", 64), format!("{}xxxx", "0".repeat(60)));
        // A leading digit that is not `x` or `z` pads with zeros, and a signed
        // decimal still sign extends.
        assert_eq!(sized("'h1", 64), format!("{}1", "0".repeat(63)));
        assert_eq!(sized("-1", 64), "1".repeat(64));
    }

    /// A comparison sizes its operands against each other, and an unsized
    /// `'hx` is the one *leaf* that can tell which end the widening happened
    /// at: `period !== 'hx` for an untouched 64 bit register is false, not
    /// true. This is corpus `pr673` / issue #157.
    #[test]
    fn test_unsized_unknown_literal_compares_at_the_wider_width() {
        let mut store = StateStore::new();
        store.declare_signed("period", (63, 0), false);
        assert_eq!(bits_in("period === 'hx", &store), "1");
        assert_eq!(bits_in("period !== 'hx", &store), "0");
        // A sized one is a different value and still compares as one.
        assert_eq!(bits_in("period === 32'hx", &store), "0");
    }

    #[test]
    fn test_constant_truncates_to_declared_size() {
        // 0xFF does not fit in four bits; the low bits survive.
        assert_eq!(bits("4'hFF"), "1111");
        assert_eq!(bits("2'b1011"), "11");
    }

    #[test]
    fn test_constant_with_unknown_digits() {
        assert_eq!(constant_register("4'bx1").unwrap().to_binary(), "xxx1");
        assert_eq!(constant_register("2'hz").unwrap().to_binary(), "zz");
        assert_eq!(constant_register("8'hx0").unwrap().to_binary(), "xxxx0000");
    }

    /// The same x/z literals, but reached through the real parser rather than
    /// [`constant_register`] directly — this is the seam between the constant
    /// grammar and the evaluator, so it is worth pinning end to end.
    #[test]
    fn test_unknown_digit_constants_round_trip_through_parser() {
        assert_eq!(bits("4'bx1"), "xxx1");
        assert_eq!(bits("2'hz"), "zz");
        assert_eq!(bits("8'hx0"), "xxxx0000");
        assert_eq!(bits("4'bzzzz"), "zzzz");
    }

    /// The two ways into a literal — evaluating a parsed
    /// `Expression::Constant` and calling [`constant_register`] on the
    /// equivalent token — share one conversion and must agree exactly.
    #[test]
    fn test_parsed_and_token_constant_paths_agree() {
        let store = StateStore::new();
        for (source, token) in [
            ("4'b1010", "4'b1010"),
            ("8'b1010", "8'b1010"),
            ("1'b1", "1'b1"),
            ("8'hFF", "8'hFF"),
            ("8'hac", "8'hac"),
            ("6'o54", "6'o54"),
            ("4'd6", "4'd6"),
            ("32'hFACE_47B2", "32'hFACE_47B2"),
            // A declared size narrower than the digits truncates.
            ("4'hFF", "4'hFF"),
            ("2'b1011", "2'b1011"),
            // Unknown and high impedance digits.
            ("4'bx1", "4'bx1"),
            ("2'hz", "2'hz"),
            ("8'hx0", "8'hx0"),
            ("4'b1?0z", "4'b1?0z"),
            // Unsized literals take UNSIZED_CONSTANT_WIDTH.
            ("'b1010", "'b1010"),
            ("'hFF", "'hFF"),
            ("42", "'d42"),
            ("0", "'d0"),
        ] {
            let parsed = eval(&parse(source), &store)
                .unwrap_or_else(|e| panic!("{} failed to evaluate: {}", source, e));
            let converted = constant_register(token)
                .unwrap_or_else(|e| panic!("{} failed to convert: {}", token, e));
            assert_eq!(parsed, converted, "{} and {} disagree", source, token);
        }
    }

    /// A decimal literal's value may be a single `x`, `z` or `?` standing for
    /// the whole of it, which the size then extends — there is no such thing as
    /// an unknown decimal *digit*, so `7'd1x` stays malformed.
    ///
    /// Measured against iverilog 12.0, which prints for corpus `pr1792734`:
    ///
    /// ```text
    ///  7'dx: xxxxxxx,  7'dz: zzzzzzz,  7'd?: zzzzzzz
    ///  2'dx: 00000xx,  2'dz: 00000zz,  2'd?: 00000zz
    /// ```
    ///
    /// — the second line being `2'dx` written into a `reg [6:0]`, so two bits
    /// zero extended rather than seven unknown ones.
    #[test]
    fn test_decimal_unknown_constant() {
        let bits = |token: &str| {
            constant_register(token)
                .unwrap_or_else(|e| panic!("{} failed to convert: {}", token, e))
                .to_binary()
        };
        assert_eq!(bits("7'dx"), "xxxxxxx");
        assert_eq!(bits("7'dz"), "zzzzzzz");
        assert_eq!(bits("7'd?"), "zzzzzzz");
        assert_eq!(bits("2'dx"), "xx");
        // The separators are not part of the value, so this is the same literal.
        assert_eq!(bits("7'dx_"), "xxxxxxx");
        // An unknown decimal *digit* is still malformed.
        assert!(matches!(
            constant_register("7'd1x"),
            Err(EvalError::MalformedConstant(_))
        ));
    }

    #[test]
    fn test_malformed_constant() {
        assert!(matches!(
            constant_register("4'q1"),
            Err(EvalError::MalformedConstant(_))
        ));
        assert!(matches!(
            constant_register("nonsense"),
            Err(EvalError::MalformedConstant(_))
        ));
        assert!(matches!(
            constant_register("4'b"),
            Err(EvalError::MalformedConstant(_))
        ));
        assert!(matches!(
            constant_register("0'b1"),
            Err(EvalError::MalformedConstant(_))
        ));
    }

    /// The rejected literal has to be nameable in the message. Reporting only
    /// the digits leaves `4'b` with an empty payload and blames the digits of
    /// `0'b1`, where the width is the actual problem.
    #[test]
    fn test_malformed_constant_names_the_literal() {
        for token in ["4'b", "0'b1", "4'b1234"] {
            let message = constant_register(token).unwrap_err().to_string();
            assert!(
                message.contains(token),
                "{:?} should name {:?}",
                message,
                token
            );
        }
    }

    // -- identifiers -------------------------------------------------------

    #[test]
    fn test_identifier_lookup() {
        let store = sample_store();
        assert_eq!(bits_in("a", &store), "10100110");
        assert_eq!(bits_in("b", &store), "0011");
    }

    #[test]
    fn test_unknown_identifier() {
        let store = sample_store();
        assert_eq!(
            error("missing", &store),
            EvalError::UnknownIdentifier("missing".to_string())
        );
    }

    // -- arithmetic --------------------------------------------------------

    #[test]
    fn test_addition_and_subtraction() {
        assert_eq!(bits("4'd6 + 4'd3"), "1001");
        assert_eq!(bits("4'd6 - 4'd3"), "0011");
        // Subtraction wraps in the result width.
        assert_eq!(bits("4'd3 - 4'd6"), "1101");
        // Addition wraps rather than widening.
        assert_eq!(bits("4'd15 + 4'd1"), "0000");
    }

    #[test]
    fn test_multiplication_division_modulus() {
        assert_eq!(value("8'd12 * 8'd3"), 36);
        assert_eq!(value("8'd13 / 8'd3"), 4);
        assert_eq!(value("8'd13 % 8'd3"), 1);
    }

    #[test]
    fn test_division_by_zero_is_unknown() {
        assert_eq!(bits("4'd8 / 4'd0"), "xxxx");
        assert_eq!(bits("4'd8 % 4'd0"), "xxxx");
    }

    #[test]
    fn test_power_takes_left_operand_width() {
        assert_eq!(bits("8'd2 ** 8'd5"), "00100000");
        // Overflow wraps at the left operand's width.
        assert_eq!(bits("4'd2 ** 4'd4"), "0000");
        assert_eq!(bits("4'd7 ** 4'd0"), "0001");
    }

    #[test]
    fn test_arithmetic_widens_to_the_wider_operand() {
        assert_eq!(bits("8'd1 + 4'd1"), "00000010");
    }

    #[test]
    fn test_unary_plus_and_minus() {
        assert_eq!(bits("+4'd5"), "0101");
        assert_eq!(bits("-4'd1"), "1111");
        assert_eq!(bits("-4'd5"), "1011");
    }

    // -- bitwise -----------------------------------------------------------

    #[test]
    fn test_bitwise_operators() {
        assert_eq!(bits("4'b1100 & 4'b1010"), "1000");
        assert_eq!(bits("4'b1100 | 4'b1010"), "1110");
        assert_eq!(bits("4'b1100 ^ 4'b1010"), "0110");
        assert_eq!(bits("4'b1100 ^~ 4'b1010"), "1001");
        assert_eq!(bits("4'b1100 ~^ 4'b1010"), "1001");
    }

    #[test]
    fn test_bitwise_negation() {
        assert_eq!(bits("~4'b1010"), "0101");
    }

    #[test]
    fn test_bitwise_zero_extends_the_narrower_operand() {
        assert_eq!(bits("8'b11110000 | 4'b1111"), "11111111");
        assert_eq!(bits("8'b11110000 & 4'b1111"), "00000000");
    }

    // -- shifts ------------------------------------------------------------

    #[test]
    fn test_shifts() {
        assert_eq!(bits("4'b0011 << 4'd2"), "1100");
        assert_eq!(bits("4'b1100 >> 4'd2"), "0011");
        // Bits shifted past the end are dropped, width is preserved.
        assert_eq!(bits("4'b1111 << 4'd8"), "0000");
        assert_eq!(bits("4'b1111 >> 4'd8"), "0000");
    }

    #[test]
    fn test_arithmetic_shifts_match_logical_shifts() {
        // Unsigned only: <<< and >>> behave like << and >>.
        assert_eq!(bits("4'b0011 <<< 4'd1"), "0110");
        assert_eq!(bits("4'b1100 >>> 4'd1"), "0110");
    }

    // -- comparison --------------------------------------------------------

    #[test]
    fn test_relational_operators() {
        assert_eq!(bits("4'd3 < 4'd6"), "1");
        assert_eq!(bits("4'd6 < 4'd3"), "0");
        assert_eq!(bits("4'd6 <= 4'd6"), "1");
        assert_eq!(bits("4'd6 > 4'd3"), "1");
        assert_eq!(bits("4'd3 >= 4'd6"), "0");
    }

    #[test]
    fn test_logical_equality() {
        assert_eq!(bits("4'd6 == 4'd6"), "1");
        assert_eq!(bits("4'd6 != 4'd6"), "0");
        // Widths are padded before comparing.
        assert_eq!(bits("8'd1 == 4'd1"), "1");
    }

    /// An unknown bit makes `==` unknown only when it could still change the
    /// answer. A pair of *known* bits that disagrees settles the question
    /// whatever the unknown ones hold, so a comparison that is determinable
    /// is answered rather than poisoned.
    ///
    /// iverilog 12.0, for the six lines below in order:
    ///
    /// ```text
    /// a 0
    /// b 1
    /// c x
    /// d x
    /// e 1
    /// f 1
    /// ```
    ///
    /// The relational operators keep the coarse rule — `4'b1xxx > 4'b0111` is
    /// `x` there even though it is determinable — so this is deliberately not
    /// applied to them.
    #[test]
    fn test_a_known_bit_settles_an_equality() {
        assert_eq!(bits("4'bxxx0 == 4'b0001"), "0");
        assert_eq!(bits("4'bxxx0 != 4'b0001"), "1");
        // Every known pair agrees, so the unknown ones still decide.
        assert_eq!(bits("4'bxxx1 == 4'b0001"), "x");
        assert_eq!(bits("4'bxxx1 != 4'b0001"), "x");
        // A `z` is as unknown as an `x`, and the order of the operands does
        // not matter.
        assert_eq!(bits("4'bzzz0 != 4'b0001"), "1");
        assert_eq!(bits("4'b0001 == 4'bxxx0"), "0");
        // Both sides may be unknown.
        assert_eq!(bits("4'bxxx0 != 4'bxxx1"), "1");
        // The narrower operand is widened first: `3'bx01` is `0x01`, whose
        // known bits all agree with `0001`.
        assert_eq!(bits("3'bx01 == 4'b0001"), "x");
        // A relational operator is untouched.
        assert_eq!(bits("4'b1xxx > 4'b0111"), "x");
    }

    #[test]
    fn test_logical_and_or_not() {
        assert_eq!(bits("4'd2 && 4'd1"), "1");
        assert_eq!(bits("4'd0 && 4'd1"), "0");
        assert_eq!(bits("4'd0 || 4'd4"), "1");
        assert_eq!(bits("4'd0 || 4'd0"), "0");
        assert_eq!(bits("!4'd0"), "1");
        assert_eq!(bits("!4'd7"), "0");
    }

    // -- reduction ---------------------------------------------------------

    #[test]
    fn test_reduction_operators() {
        assert_eq!(bits("&4'b1111"), "1");
        assert_eq!(bits("&4'b1101"), "0");
        assert_eq!(bits("~&4'b1101"), "1");
        assert_eq!(bits("|4'b0010"), "1");
        assert_eq!(bits("|4'b0000"), "0");
        assert_eq!(bits("~|4'b0000"), "1");
        assert_eq!(bits("^4'b1011"), "1");
        assert_eq!(bits("^4'b1001"), "0");
        assert_eq!(bits("~^4'b1001"), "1");
        assert_eq!(bits("^~4'b1011"), "0");
    }

    #[test]
    fn test_reduction_xor_over_a_vector() {
        // The shape used by verilog/examples/parity_calculator.v.
        let mut store = StateStore::new();
        store.set_ranged("data", Register::from_binary("10110100"), (7, 0));
        assert_eq!(bits_in("^data", &store), "0");

        store.set_ranged("data", Register::from_binary("10110101"), (7, 0));
        assert_eq!(bits_in("^data", &store), "1");
    }

    // -- selects -----------------------------------------------------------

    #[test]
    fn test_bit_select() {
        let store = sample_store();
        // a = 8'b1010_0110
        assert_eq!(bits_in("a[7]", &store), "1");
        assert_eq!(bits_in("a[6]", &store), "0");
        assert_eq!(bits_in("a[1]", &store), "1");
        assert_eq!(bits_in("a[0]", &store), "0");
    }

    #[test]
    fn test_bit_select_out_of_range_is_unknown() {
        let store = sample_store();
        assert_eq!(bits_in("a[8]", &store), "x");
        assert_eq!(bits_in("b[9]", &store), "x");
    }

    #[test]
    fn test_bit_select_respects_declared_range() {
        let mut store = StateStore::new();
        store.set_ranged("hi", Register::from_binary("1000"), (7, 4));
        assert_eq!(bits_in("hi[7]", &store), "1");
        assert_eq!(bits_in("hi[4]", &store), "0");
        assert_eq!(bits_in("hi[3]", &store), "x");

        let mut ascending = StateStore::new();
        ascending.set_ranged("up", Register::from_binary("1000"), (0, 3));
        assert_eq!(bits_in("up[0]", &ascending), "1");
        assert_eq!(bits_in("up[3]", &ascending), "0");
    }

    #[test]
    fn test_bit_select_with_computed_index() {
        let store = sample_store();
        assert_eq!(bits_in("a[4'd3 + 4'd4]", &store), "1");
    }

    #[test]
    fn test_bit_select_on_unknown_identifier() {
        assert_eq!(
            error("nope[0]", &StateStore::new()),
            EvalError::UnknownIdentifier("nope".to_string())
        );
    }

    #[test]
    fn test_part_select() {
        let store = sample_store();
        // a = 8'b1010_0110
        assert_eq!(bits_in("a[7:4]", &store), "1010");
        assert_eq!(bits_in("a[3:0]", &store), "0110");
        assert_eq!(bits_in("a[5:2]", &store), "1001");
    }

    #[test]
    fn test_part_select_out_of_range_pads_with_unknown() {
        let store = sample_store();
        assert_eq!(bits_in("a[9:6]", &store), "xx10");
    }

    #[test]
    fn test_part_select_ascending_range() {
        let mut store = StateStore::new();
        store.set_ranged("up", Register::from_binary("1100"), (0, 3));
        assert_eq!(bits_in("up[0:1]", &store), "11");
        assert_eq!(bits_in("up[2:3]", &store), "00");
    }

    #[test]
    fn test_part_select_bound_must_be_known() {
        let mut store = sample_store();
        store.set("i", Register::from_binary("xx"));
        assert!(matches!(
            error("a[i:0]", &store),
            EvalError::NonConstantSelectBound(_)
        ));
    }

    // -- structure ---------------------------------------------------------

    #[test]
    fn test_concatenation() {
        assert_eq!(bits("{4'b1010, 4'b0011}"), "10100011");
        assert_eq!(bits("{1'b1, 2'b00, 1'b1}"), "1001");
    }

    #[test]
    fn test_concatenation_of_selects() {
        let store = sample_store();
        assert_eq!(bits_in("{a[7:4], b}", &store), "10100011");
    }

    #[test]
    fn test_parenthetical_and_precedence() {
        assert_eq!(value("(8'd2 + 8'd3) * 8'd4"), 20);
        assert_eq!(value("8'd2 + 8'd3 * 8'd4"), 14);
    }

    #[test]
    fn test_conditional() {
        assert_eq!(bits("1'b1 ? 4'b1010 : 4'b0101"), "1010");
        assert_eq!(bits("1'b0 ? 4'b1010 : 4'b0101"), "0101");
        assert_eq!(bits("4'd0 ? 4'b1010 : 4'b0101"), "0101");
    }

    #[test]
    fn test_conditional_with_unknown_condition_merges_branches() {
        let mut store = StateStore::new();
        store.set("c", Register::from_binary("x"));
        // Bits that agree survive, bits that differ become x.
        assert_eq!(bits_in("c ? 4'b1010 : 4'b0101", &store), "xxxx");
        assert_eq!(bits_in("c ? 4'b1100 : 4'b1010", &store), "1xx0");
    }

    #[test]
    fn test_conditional_only_evaluates_the_taken_branch() {
        // The untaken branch references an undefined identifier and is ignored.
        assert_eq!(bits("1'b1 ? 4'b1111 : missing"), "1111");
    }

    #[test]
    fn test_function_call_is_unsupported() {
        assert_eq!(
            error("parity(1'b1)", &StateStore::new()),
            EvalError::UnsupportedFunctionCall("parity".to_string())
        );
    }

    // -- four state propagation -------------------------------------------

    #[test]
    fn test_arithmetic_with_unknown_operand_is_all_unknown() {
        let mut store = StateStore::new();
        store.set("u", Register::from_binary("00x1"));
        assert_eq!(bits_in("u + 4'd1", &store), "xxxx");
        assert_eq!(bits_in("4'd1 - u", &store), "xxxx");
        assert_eq!(bits_in("u * 4'd2", &store), "xxxx");
        assert_eq!(bits_in("-u", &store), "xxxx");
    }

    #[test]
    fn test_high_impedance_behaves_like_unknown_in_arithmetic() {
        let mut store = StateStore::new();
        store.set("hz", Register::from_binary("00z1"));
        assert_eq!(bits_in("hz + 4'd1", &store), "xxxx");
    }

    #[test]
    fn test_bitwise_unknown_propagation() {
        let mut store = StateStore::new();
        store.set("u", Register::from_binary("xxxx"));
        // A dominant 0 for AND and a dominant 1 for OR win over x.
        assert_eq!(bits_in("u & 4'b0011", &store), "00xx");
        assert_eq!(bits_in("u | 4'b1100", &store), "11xx");
        // XOR has no dominant value.
        assert_eq!(bits_in("u ^ 4'b1100", &store), "xxxx");
        assert_eq!(bits_in("~u", &store), "xxxx");
    }

    #[test]
    fn test_bitwise_negation_of_high_impedance_is_unknown() {
        let mut store = StateStore::new();
        store.set("hz", Register::from_binary("z0"));
        assert_eq!(bits_in("~hz", &store), "x1");
    }

    #[test]
    fn test_reduction_unknown_propagation() {
        let mut store = StateStore::new();
        store.set("u", Register::from_binary("1x1"));
        assert_eq!(bits_in("&u", &store), "x");
        assert_eq!(bits_in("|u", &store), "1");
        assert_eq!(bits_in("^u", &store), "x");

        store.set("u", Register::from_binary("0x1"));
        assert_eq!(bits_in("&u", &store), "0");
        assert_eq!(bits_in("|u", &store), "1");

        store.set("u", Register::from_binary("0x0"));
        assert_eq!(bits_in("|u", &store), "x");
    }

    #[test]
    fn test_comparison_unknown_propagation() {
        let mut store = StateStore::new();
        store.set("u", Register::from_binary("1x"));
        assert_eq!(bits_in("u < 2'b11", &store), "x");
        assert_eq!(bits_in("u == 2'b11", &store), "x");
        assert_eq!(bits_in("u != 2'b11", &store), "x");
    }

    #[test]
    fn test_case_equality_compares_unknown_bits_exactly() {
        let mut store = StateStore::new();
        store.set("u", Register::from_binary("1x"));
        store.set("v", Register::from_binary("1x"));
        store.set("w", Register::from_binary("1z"));

        assert_eq!(bits_in("u === v", &store), "1");
        assert_eq!(bits_in("u !== v", &store), "0");
        // x and z are distinct states for ===.
        assert_eq!(bits_in("u === w", &store), "0");
        assert_eq!(bits_in("u !== w", &store), "1");
        assert_eq!(bits_in("u === 2'b11", &store), "0");
    }

    #[test]
    fn test_logical_operators_unknown_propagation() {
        let mut store = StateStore::new();
        store.set("u", Register::from_binary("xx"));
        assert_eq!(bits_in("u && 2'b00", &store), "0");
        assert_eq!(bits_in("u && 2'b01", &store), "x");
        assert_eq!(bits_in("u || 2'b01", &store), "1");
        assert_eq!(bits_in("u || 2'b00", &store), "x");
        assert_eq!(bits_in("!u", &store), "x");
    }

    #[test]
    fn test_shift_preserves_unknown_bits() {
        let mut store = StateStore::new();
        store.set("u", Register::from_binary("01x1"));
        assert_eq!(bits_in("u << 4'd1", &store), "1x10");
        assert_eq!(bits_in("u >> 4'd1", &store), "001x");
        // An unknown shift amount poisons the whole result.
        store.set("n", Register::from_binary("xx"));
        assert_eq!(bits_in("u << n", &store), "xxxx");
    }

    #[test]
    fn test_select_of_unknown_bits() {
        let mut store = StateStore::new();
        store.set_ranged("u", Register::from_binary("1x0z"), (3, 0));
        assert_eq!(bits_in("u[2]", &store), "x");
        assert_eq!(bits_in("u[0]", &store), "z");
        assert_eq!(bits_in("u[3:0]", &store), "1x0z");
    }

    #[test]
    fn test_bit_select_with_unknown_index() {
        let mut store = sample_store();
        store.set("i", Register::from_binary("xx"));
        assert_eq!(bits_in("a[i]", &store), "x");
    }

    // -- limits ------------------------------------------------------------

    #[test]
    fn test_arithmetic_width_overflow() {
        let mut store = StateStore::new();
        store.set("wide", Register::zeros(200));
        assert_eq!(error("wide + 4'd1", &store), EvalError::WidthOverflow(200));
        // Bitwise operators are not restricted by the arithmetic width limit.
        assert_eq!(bits_in("wide | wide", &store).len(), 200);
    }

    #[test]
    fn test_part_select_width_overflow() {
        let store = sample_store();
        assert!(matches!(
            error("a[32'd1000000:0]", &store),
            EvalError::WidthOverflow(_)
        ));
    }

    #[test]
    fn test_eval_error_display() {
        assert_eq!(
            EvalError::UnknownIdentifier("q".to_string()).to_string(),
            "no value for identifier `q`"
        );
        assert_eq!(
            EvalError::EmptyConcatenation.to_string(),
            "empty concatenation has no value"
        );
    }

    // -----------------------------------------------------------------------
    // System functions
    // -----------------------------------------------------------------------

    #[test]
    fn test_time_reads_the_store_clock() {
        let mut store = StateStore::new();
        assert_eq!(value_in("$time", &store), 0);
        store.set_time(37);
        assert_eq!(value_in("$time", &store), 37);
        // A `$time` in the middle of an expression is an operand like any other.
        assert_eq!(value_in("$time > 5", &store), 1);
        assert_eq!(value_in("$time + 1", &store), 38);
    }

    #[test]
    fn test_signed_and_unsigned_preserve_the_bits_and_the_width() {
        let store = sample_store();
        // Both casts change how the bits are read and nothing else: same bits,
        // same width. What they change shows up in the operator that receives
        // the result, which is what the signedness tests below assert.
        assert_eq!(bits_in("$signed(a)", &store), "10100110");
        assert_eq!(bits_in("$unsigned(a)", &store), "10100110");
        assert_eq!(bits_in("$signed(b)", &store), "0011");
        assert_eq!(value_in("$signed(b) + 1", &store), 4);
    }

    #[test]
    fn test_bits_reports_the_width_of_its_operand() {
        let store = sample_store();
        assert_eq!(value_in("$bits(a)", &store), 8);
        assert_eq!(value_in("$bits(b)", &store), 4);
        assert_eq!(value_in("$bits({a, b})", &store), 12);
        assert_eq!(value_in("$bits(4'b1010)", &store), 4);
    }

    /// `$bits` takes a data *type* as well as a value, which is how a design
    /// asks whether a port really was declared an `integer` — corpus
    /// `module_nonansi_integer1`, `task_nonansi_time1` and their four siblings
    /// all check `$bits(x) == $bits(integer)`.
    ///
    /// iverilog 12.0 accepts exactly three spellings:
    ///
    /// ```text
    /// $bits(reg)      1
    /// $bits(integer)  32
    /// $bits(time)     64
    /// $bits(real)     bt.v:7: error: Invalid data type for $bits().
    /// $bits(wire)     b3.v:2: syntax error
    /// ```
    #[test]
    fn test_bits_reports_the_width_of_a_type() {
        let store = sample_store();
        assert_eq!(value_in("$bits(reg)", &store), 1);
        assert_eq!(value_in("$bits(integer)", &store), 32);
        assert_eq!(value_in("$bits(time)", &store), 64);
        // A type iverilog refuses stays a name nothing declares, rather than a
        // width invented for it.
        assert!(matches!(
            eval(&parse("$bits(real)"), &store),
            Err(EvalError::UnknownIdentifier(_))
        ));
        // And anything the design *does* declare answers for itself, since the
        // type table is only asked once the lookup has already missed.
        assert_eq!(value_in("$bits(a)", &store), 8);
    }

    #[test]
    fn test_clog2_counts_the_bits_a_count_needs() {
        for (source, expected) in [
            ("$clog2(0)", 0),
            ("$clog2(1)", 0),
            ("$clog2(2)", 1),
            ("$clog2(3)", 2),
            ("$clog2(4)", 2),
            ("$clog2(5)", 3),
            ("$clog2(255)", 8),
            ("$clog2(256)", 8),
            ("$clog2(257)", 9),
        ] {
            assert_eq!(value(source), expected, "{}", source);
        }
        // Unknown in, unknown out — never a plausible-looking zero.
        let mut store = StateStore::new();
        store.set_ranged("u", Register::from_binary("10x1"), (3, 0));
        assert!(eval(&parse("$clog2(u)"), &store).unwrap().has_unknown());
    }

    /// The unseeded stream is IEEE 1364-2005's, which is what makes it the
    /// same stream iverilog draws. Measured: `integer i; initial repeat (4)
    /// begin i = $random; $display("%0d", i); end` under iverilog 12.0 prints
    /// `303379748`, `-1064739199`, `-2071669239`, `-1309649309`.
    #[test]
    fn test_random_draws_the_standard_sequence() {
        let store = StateStore::new();
        let drawn: Vec<i128> = (0..4)
            .map(|_| {
                eval(&parse("$random"), &store)
                    .expect("$random")
                    .to_i128()
                    .expect("a number")
            })
            .collect();
        assert_eq!(
            drawn,
            vec![303379748, -1064739199, -2071669239, -1309649309]
        );
        // Every store starts the stream from the same seed, so two runs of the
        // same design draw the same numbers in the same order.
        let again = StateStore::new();
        assert_eq!(
            eval(&parse("$random"), &again).expect("$random").to_i128(),
            Some(303379748)
        );
        // `$random` is Verilog's 32 bit integer.
        assert_eq!(bits_in("$random", &StateStore::new()).len(), 32);
    }

    /// A seed is an `inout`: the draw reads the design's variable and writes
    /// the next seed back through it, which is what makes a loop over
    /// `$random(s)` a sequence. Measured: corpus `pr995` prints
    /// `seed=00010dce result=80010e00` for a seed of `1`.
    #[test]
    fn test_random_with_a_seed_writes_the_next_one_back() {
        let mut store = StateStore::new();
        store.declare_signed("seed", (31, 0), true);
        store.set_ranged("seed", Register::from_u128(1, 32), (31, 0));

        let drawn = eval(&parse("$random(seed)"), &store).expect("$random(seed)");
        assert_eq!(drawn.to_u128(), Some(0x8001_0e00));

        // The new seed is queued rather than written, the way a `$sscanf`
        // argument is: `program::resume` drains it at the next instruction.
        let fills = store.take_fills();
        assert_eq!(fills.len(), 1);
        assert_eq!(fills[0].1.to_u128(), Some(0x0001_0dce));
    }

    /// Two draws in one expression advance the seed between them, because the
    /// second reads the write the first one owes. Corpus `concat3` is exactly
    /// this: `{$random(seed), $random(seed), …}` has to give four different
    /// numbers, and iverilog 12.0 gives them in MSB -> LSB order.
    #[test]
    fn test_two_seeded_draws_in_one_expression_advance() {
        let mut store = StateStore::new();
        store.declare_signed("seed", (31, 0), true);
        store.set_ranged("seed", Register::from_u128(1, 32), (31, 0));

        let first = eval(&parse("$random(seed)"), &store).expect("first");
        let second = eval(&parse("$random(seed)"), &store).expect("second");
        assert_eq!(first.to_u128(), Some(0x8001_0e00));
        assert_eq!(second.to_u128(), Some(0x9c59_8438));
    }

    /// A seed that cannot be written is a named error rather than a stream
    /// that never moves — `$random(7)` would otherwise draw one number for
    /// ever and look exactly like a design whose stimulus had stopped.
    #[test]
    fn test_random_needs_a_writable_seed() {
        let store = StateStore::new();
        assert!(matches!(
            error("$random(7)", &store),
            EvalError::RandomSeed(_)
        ));
    }

    #[test]
    fn test_every_listed_system_function_evaluates() {
        // [`SYSTEM_FUNCTIONS`] is what `TaskCall::compile` trusts when it
        // decides a `$name` is meaningful, so a name listed there but missing
        // from the evaluator would be accepted and then fail late.
        let store = sample_store();
        for name in SYSTEM_FUNCTIONS {
            let source = match name {
                "time" | "stime" | "realtime" | "random" => format!("${}", name),
                // The empty path names no file, so this exercises `$fopen`
                // without leaving one behind.
                "fopen" => "$fopen(\"\")".to_string(),
                // The two-argument members of the real math library.
                _ if REAL_MATH_BINARY.contains(&name) => format!("${}(a, a)", name),
                // The reading half, each against a descriptor nothing has
                // open, which reads as end of file and touches no file.
                "sscanf" => "$sscanf(\"1\", \"%d\", a)".to_string(),
                "fscanf" => "$fscanf(32'h8000_0009, \"%d\", a)".to_string(),
                "fgets" | "ungetc" => format!("${}(a, 32'h8000_0009)", name),
                "fseek" => "$fseek(32'h8000_0009, 0, 0)".to_string(),
                // The plus-args, against an empty list: `$test` is `0` and
                // `$value` is `0` with `a` left alone, which is the answer a
                // design reads rather than a failure.
                "value$plusargs" => "$value$plusargs(\"opt=%d\", a)".to_string(),
                other => format!("${}(a)", other),
            };
            eval(&parse(&source), &store)
                .unwrap_or_else(|error| panic!("{} failed to evaluate: {}", source, error));
        }
    }

    #[test]
    fn test_stime_is_the_clock_as_an_integer() {
        let mut store = StateStore::new();
        store.set_time(1234);
        assert_eq!(value_in("$stime", &store), 1234);
        assert_eq!(bits_in("$stime", &store).len(), 32);
        assert_eq!(bits_in("$time", &store).len(), 64);
        // Both are *time* values, and `time` is an unsigned type — which is
        // also the field `%d` gives them: iverilog 12.0 prints
        // `$display($stime)` in ten columns where an `integer` takes eleven
        // (corpus `pr2842621`).
        assert!(!eval(&parse("$stime"), &store).unwrap().is_signed());
        assert!(!eval(&parse("$time"), &store).unwrap().is_signed());
    }

    #[test]
    fn test_an_unknown_system_function_is_an_error_that_names_it() {
        let store = StateStore::new();
        assert_eq!(
            error("$nosuchthing(1)", &store).to_string(),
            "unknown system function `$nosuchthing`"
        );
        assert_eq!(
            error("$foo", &store),
            EvalError::UnknownSystemFunction("foo".to_string())
        );
    }

    #[test]
    fn test_a_system_function_checks_its_argument_count() {
        let store = sample_store();
        assert_eq!(
            error("$signed(a, b)", &store).to_string(),
            "`$signed` takes exactly one argument, but was given 2"
        );
        assert_eq!(
            error("$time(a)", &store).to_string(),
            "`$time` takes no arguments, but was given 1"
        );
    }

    // -- signedness --------------------------------------------------------

    /// `reg signed [3:0] s = 4'b1000;` — that is -8 — beside the same bits
    /// declared unsigned, and a small unsigned constant to mix in.
    fn signed_store() -> StateStore {
        let mut store = StateStore::new();
        store.set_ranged(
            "s",
            Register::from_binary("1000").with_signedness(true),
            (3, 0),
        );
        store.set_ranged("u", Register::from_binary("1000"), (3, 0));
        store.set_ranged("one", Register::from_binary("0001"), (3, 0));
        store
    }

    /// The two's complement value of an expression, which is what a signed
    /// result means.
    fn signed_value_in(source: &str, store: &StateStore) -> i128 {
        eval(&parse(source), store)
            .unwrap_or_else(|e| panic!("{} failed to evaluate: {}", source, e))
            .to_i128()
            .unwrap_or_else(|| panic!("{} evaluated to a non-numeric value", source))
    }

    fn signed_value(source: &str) -> i128 {
        signed_value_in(source, &StateStore::new())
    }

    /// A decimal written without a base is signed; anything with a base is
    /// unsigned unless it says `s`.
    #[test]
    fn test_which_literals_are_signed() {
        assert!(eval(&parse("42"), &StateStore::new()).unwrap().is_signed());
        assert!(!eval(&parse("4'd2"), &StateStore::new())
            .unwrap()
            .is_signed());
        assert!(eval(&parse("4'sd2"), &StateStore::new())
            .unwrap()
            .is_signed());
        assert!(eval(&parse("4'sb1000"), &StateStore::new())
            .unwrap()
            .is_signed());
        // The `s` changes no bit, only how they read.
        assert_eq!(bits("4'sb1000"), "1000");
        // It is the *base designator* rather than the size that makes a
        // decimal unsigned: `'d1` is not signed where `1` is. Measured against
        // iverilog 12.0 — `parameter tp = 'd1; $display("%d", tp);` prints in
        // ten columns and `parameter tp = 1;` in eleven (corpus `pr812`), and
        // `('d1 - 'd2) < 0` is false where `(1 - 2) < 0` is true.
        assert!(!eval(&parse("'d1"), &StateStore::new()).unwrap().is_signed());
        assert!(eval(&parse("'sd1"), &StateStore::new())
            .unwrap()
            .is_signed());
        assert_eq!(bits("('d1 - 'd2) < 0"), "0");
        assert_eq!(bits("(1 - 2) < 0"), "1");
    }

    /// `-1 < 0` is true between signed operands and false the moment either
    /// side is unsigned, because the comparison is then a magnitude one.
    #[test]
    fn test_relational_comparison_follows_signedness() {
        let store = signed_store();
        assert_eq!(bits_in("s < 0", &store), "1");
        assert_eq!(bits_in("u < 0", &store), "0");

        // Parenthesised because a unary expression does not yet consume the
        // whitespace after it, so `-1 < 0` stops the parser at the `<`.
        assert_eq!(bits("(-1) < 0"), "1");
        assert_eq!(bits("$unsigned(-1) < 0"), "0");

        // -8 is less than 1, but 8 is not.
        assert_eq!(bits_in("s < one", &store), "0");
        assert_eq!(bits_in("$signed(s) < $signed(one)", &store), "1");
    }

    /// `>>>` replicates the sign bit of a signed value; `>>` never does, and on
    /// an unsigned value the two are the same shift.
    #[test]
    fn test_arithmetic_right_shift_replicates_the_sign_bit() {
        let store = signed_store();
        assert_eq!(bits_in("s >>> 1", &store), "1100");
        assert_eq!(bits_in("s >> 1", &store), "0100");
        assert_eq!(bits_in("u >>> 1", &store), "0100");
        assert_eq!(bits_in("$signed(u) >>> 2", &store), "1110");
        // Shifting further than the width leaves the sign bit everywhere.
        assert_eq!(bits_in("s >>> 9", &store), "1111");
        // `<<<` is `<<` whatever the signedness.
        assert_eq!(bits_in("s <<< 1", &store), "0000");
    }

    /// A signed quotient truncates toward zero and a signed remainder takes the
    /// sign of the dividend — which is what Verilog asks for and what an
    /// unsigned division of the same bits does not do.
    #[test]
    fn test_signed_division_truncates_toward_zero() {
        assert_eq!(signed_value("(-7) / 2"), -3);
        assert_eq!(signed_value("7 / (-2)"), -3);
        assert_eq!(signed_value("(-7) % 2"), -1);
        assert_eq!(signed_value("7 % (-2)"), 1);
        assert_eq!(signed_value("(-8) / 2"), -4);

        // The same bits divided as magnitudes are a different answer entirely.
        assert_eq!(value("$unsigned(-7) / 2"), 0x7fff_fffc);
    }

    /// One unsigned operand makes the whole expression unsigned, and that
    /// reaches *inside* it: the `>>>` of an unsigned expression is a plain
    /// `>>` even over an operand that was declared signed.
    #[test]
    fn test_a_mixed_expression_is_unsigned_throughout() {
        let store = signed_store();
        assert_eq!(bits_in("(s >>> 1) | 4'sb0001", &store), "1101");
        assert_eq!(bits_in("(s >>> 1) | one", &store), "0101");
        assert!(!eval(&parse("s + one"), &store).unwrap().is_signed());
        assert!(eval(&parse("s + 4'sd1"), &store).unwrap().is_signed());

        // A concatenation and a select are bit vectors, so they are unsigned
        // however signed the thing they were built from was.
        assert!(!eval(&parse("{s}"), &store).unwrap().is_signed());
        assert!(!eval(&parse("s[3]"), &store).unwrap().is_signed());
    }

    /// The narrower operand of a signed operation is sign extended rather than
    /// zero extended, which is the whole of why `-1 == 32'hffffffff` holds.
    #[test]
    fn test_widening_extends_the_sign_of_a_signed_operand() {
        let store = signed_store();
        // -8 + 0 in 32 bits is still -8, but 8 zero extended is 8.
        assert_eq!(signed_value_in("s + 0", &store), -8);
        assert_eq!(value_in("u + 32'd0", &store), 8);
        assert_eq!(bits("(-1) == 32'hffffffff"), "1");
        assert_eq!(bits_in("$signed(s) == (-8)", &store), "1");
        assert_eq!(bits_in("s == 4'd8", &store), "1");
    }

    /// `$signed` and `$unsigned` are casts, not identities: they decide how the
    /// operator that receives the value reads it.
    #[test]
    fn test_signed_and_unsigned_are_real_casts() {
        let store = signed_store();
        assert!(eval(&parse("$signed(u)"), &store).unwrap().is_signed());
        assert!(!eval(&parse("$unsigned(s)"), &store).unwrap().is_signed());
        assert_eq!(bits_in("$signed(u) < 0", &store), "1");
        assert_eq!(bits_in("$unsigned(s) < 0", &store), "0");
    }

    /// An `x` or `z` sign bit replicates too, so a partly undriven signed value
    /// does not read as a positive number when it is widened.
    #[test]
    fn test_sign_extension_carries_an_unknown_sign_bit() {
        let mut store = StateStore::new();
        store.set_ranged(
            "floating",
            Register::from_binary("z010").with_signedness(true),
            (3, 0),
        );
        assert_eq!(
            eval(&parse("floating"), &store)
                .unwrap()
                .sign_extended(6)
                .to_binary(),
            "zzz010"
        );
    }

    /// `{N{…}}` repeats the inner concatenation `N` times. Every expectation
    /// here is what `iverilog` 12.0 prints for the same expression.
    #[test]
    fn test_replication() {
        assert_eq!(bits("{2{2'b01}}"), "0101");
        assert_eq!(bits("{{4{1'b1}}, 4'b0000}"), "11110000");
        assert_eq!(bits("{3{1'bx}}"), "xxx");
        // Sign extension is the idiom this operator exists for.
        let store = sample_store();
        assert_eq!(bits_in("{{2{a[7]}}, a[3:0]}", &store), "110110");
    }

    /// A count of zero produces no bits at all. That is legal only inside a
    /// wider concatenation, which is exactly where it can appear.
    #[test]
    fn test_zero_replication_contributes_nothing() {
        assert_eq!(bits("{{0{1'b1}}, 3'b101}"), "101");
    }

    /// A replication is a bit vector, so it is unsigned however signed its
    /// parts were — and it is as wide as the count times the parts.
    #[test]
    fn test_replication_is_unsigned_and_sized_by_its_count() {
        let store = StateStore::new();
        let expression = parse("{3{2'b10}}");
        assert!(!expression_is_signed(&expression, &store));
        assert_eq!(expression_width(&expression, &store), 6);
    }

    /// A count that is not a constant is reported by name rather than guessed
    /// at: a replication silently sized wrong would be a plausible-looking
    /// pattern of bits.
    #[test]
    fn test_non_constant_replication_count_is_named() {
        let error = eval(&parse("{missing{1'b1}}"), &StateStore::new())
            .expect_err("an unknown count cannot be evaluated");
        assert!(
            matches!(error, EvalError::NonConstantReplicationCount(_)),
            "expected a named count error, got {:?}",
            error
        );
    }

    /// A count large enough to allocate absurdly is refused, the same way a
    /// nonsense part select is.
    #[test]
    fn test_absurd_replication_count_is_refused() {
        let error = eval(&parse("{1000000{8'hFF}}"), &StateStore::new())
            .expect_err("a million bytes is not a value to build");
        assert!(
            matches!(error, EvalError::WidthOverflow(_)),
            "expected a width overflow, got {:?}",
            error
        );
    }

    /// `a[base +: width]` and `a[base -: width]` — the indexed part selects.
    /// Every expectation is what `iverilog` 12.0 prints for the same select on
    /// `a = 16'b1010_1100_0011_0101`.
    #[test]
    fn test_indexed_part_select() {
        let mut store = StateStore::new();
        store.set_ranged("a", Register::from_binary("1010110000110101"), (15, 0));
        assert_eq!(bits_in("a[0 +: 4]", &store), "0101");
        assert_eq!(bits_in("a[4 +: 4]", &store), "0011");
        assert_eq!(bits_in("a[15 -: 4]", &store), "1010");
        assert_eq!(bits_in("a[7 -: 8]", &store), "00110101");
    }

    /// The base may be any expression, including one that moves at run time —
    /// that is the whole reason the operator exists. Only the width has to be
    /// constant.
    #[test]
    fn test_indexed_part_select_base_may_be_computed() {
        let mut store = StateStore::new();
        store.set_ranged("a", Register::from_binary("1010110000110101"), (15, 0));
        store.set_ranged("n", Register::from_u128(1, 4), (3, 0));
        assert_eq!(bits_in("a[n * 4 +: 4]", &store), "0011");
    }

    /// An unknown base selects `x`, the way a vector indexed by an unknown
    /// does — it is not the error a bad *width* is.
    #[test]
    fn test_indexed_part_select_with_unknown_base_is_unknown() {
        let mut store = StateStore::new();
        store.set_ranged("a", Register::from_binary("1010110000110101"), (15, 0));
        store.set_ranged("n", Register::from_binary("xx"), (1, 0));
        assert_eq!(bits_in("a[n +: 4]", &store), "xxxx");
    }

    /// The width is constant by construction, so it is known without
    /// evaluating anything.
    #[test]
    fn test_indexed_part_select_width() {
        let store = StateStore::new();
        assert_eq!(expression_width(&parse("a[0 +: 4]"), &store), 4);
        assert_eq!(expression_width(&parse("a[15 -: 8]"), &store), 8);
    }

    /// A string used as a *value* is an unsigned bit vector of eight bits per
    /// character, most significant character first. Every expectation here is
    /// what `iverilog` 12.0 prints for the same expression.
    #[test]
    fn test_string_literals_are_bit_vectors() {
        assert_eq!(value("\"FOO\""), 0x46_4f_4f);
        assert_eq!(value("\"A\""), 0x41);
        // Concatenation composes them the obvious way.
        assert_eq!(value("{\"ab\", \"cd\"}"), 0x61_62_63_64);
        // And they compare as the numbers those bytes make.
        assert_eq!(bits("\"A\" < \"B\""), "1");
    }

    /// Eight bits per character, and an empty string is one NUL byte rather
    /// than nothing — `$bits("")` is 8, which iverilog agrees with.
    #[test]
    fn test_string_literal_widths() {
        let store = StateStore::new();
        assert_eq!(expression_width(&parse("\"test\""), &store), 32);
        assert_eq!(expression_width(&parse("\"A\""), &store), 8);
        assert_eq!(expression_width(&parse("\"\""), &store), 8);
        assert_eq!(value("\"\""), 0);
    }

    /// A string is a vector of bytes, not a number anyone declared a sign for.
    #[test]
    fn test_string_literals_are_unsigned() {
        assert!(!expression_is_signed(
            &parse("\"\\377\""),
            &StateStore::new()
        ));
    }

    /// The real math library. Every expectation is what `iverilog` 12.0 prints
    /// through `%f` for the same call.
    #[test]
    fn test_real_math_functions() {
        let store = StateStore::new();
        let called = |source: &str| {
            eval(&parse(source), &store)
                .expect("should evaluate")
                .to_f64()
        };

        assert!((called("$sqrt(2.0)") - 1.414214).abs() < 1e-6);
        assert!((called("$ln(2.718281828)") - 1.0).abs() < 1e-6);
        assert!((called("$log10(100.0)") - 2.0).abs() < 1e-9);
        assert!((called("$exp(1.0)") - 2.718282).abs() < 1e-6);
        assert!((called("$pow(2.0, 10.0)") - 1024.0).abs() < 1e-9);
        assert!((called("$floor(2.7)") - 2.0).abs() < 1e-9);
        assert!((called("$ceil(2.1)") - 3.0).abs() < 1e-9);
    }

    /// The trigonometric and hyperbolic half of the same library. Every
    /// expectation is the line `iverilog` 12.0 prints for `$display("%f", …)`
    /// of the same call, so the argument order of `$atan2` — numerator first,
    /// which is the one thing here that can be wrong without looking wrong — is
    /// measured rather than assumed.
    #[test]
    fn test_trigonometric_system_functions() {
        let store = StateStore::new();
        let called = |source: &str| {
            eval(&parse(source), &store)
                .expect("should evaluate")
                .to_f64()
        };

        assert!((called("$sin(0.81)") - 0.724287).abs() < 1e-6);
        assert!((called("$cos(0.81)") - 0.689498).abs() < 1e-6);
        assert!((called("$tan(0.81)") - 1.050455).abs() < 1e-6);
        assert!((called("$asin(0.5)") - 0.523599).abs() < 1e-6);
        assert!((called("$acos(0.5)") - 1.047198).abs() < 1e-6);
        assert!((called("$atan(1.0)") - 0.785398).abs() < 1e-6);
        assert!((called("$atan2(1.0, 2.0)") - 0.463648).abs() < 1e-6);
        assert!((called("$sinh(1.0)") - 1.175201).abs() < 1e-6);
        assert!((called("$cosh(1.0)") - 1.543081).abs() < 1e-6);
        assert!((called("$tanh(1.0)") - 0.761594).abs() < 1e-6);
        assert!((called("$asinh(1.0)") - 0.881374).abs() < 1e-6);
        assert!((called("$acosh(2.0)") - 1.316958).abs() < 1e-6);
        assert!((called("$atanh(0.5)") - 0.549306).abs() < 1e-6);
        assert!((called("$hypot(3.0, 4.0)") - 5.0).abs() < 1e-9);
    }

    /// A call to the real math library is real *before* it is evaluated, which
    /// is what a comparison needs: it sizes its two operands against each
    /// other, and a real has no width to share. Corpus `pr2152011` is
    /// `$floor(200000.0*$sin(cc*0.81)+0.5)`, where a `$sin` read as a bit
    /// vector would take the integer `0` into the multiply.
    #[test]
    fn test_the_real_math_library_reads_as_real() {
        let store = StateStore::new();
        for name in REAL_MATH_UNARY.iter().chain(REAL_MATH_BINARY.iter()) {
            let source = if REAL_MATH_BINARY.contains(name) {
                format!("${}(1.0, 1.0)", name)
            } else {
                format!("${}(1.0)", name)
            };
            let call = parse(&source);
            assert!(
                expression_is_real(&call, &store),
                "{} should read as real without evaluating",
                source
            );
            assert!(
                eval(&call, &store).expect("should evaluate").is_real(),
                "{} should evaluate to a real",
                source
            );
            assert!(
                SYSTEM_FUNCTIONS.contains(name),
                "${} is implemented but not listed as a system function",
                name
            );
        }
    }

    /// An integer argument to a real function converts on the way in, so
    /// `$sqrt(9)` is `3.0` rather than an integer square root.
    #[test]
    fn test_real_math_converts_an_integer_argument() {
        let store = StateStore::new();
        let value = eval(&parse("$sqrt(9)"), &store).expect("should evaluate");
        assert!(value.is_real());
        assert!((value.to_f64() - 3.0).abs() < 1e-9);
    }
}

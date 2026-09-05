//! Four-state evaluation of parsed Verilog expressions.
//!
//! [`eval`] walks an [`Expression`] and produces a [`Register`], the repo's
//! 0/1/x/z bit vector type, reading identifier values out of a [`StateStore`].
//!
//! # Deliberate simplifications
//!
//! Real Verilog sizes an expression using *context-determined* widths: the
//! width of an assignment's target flows back down into the operands. Nothing
//! here knows about an assignment target, so every operand is
//! **self-determined**: the width of a sub-expression depends only on that
//! sub-expression. The individual rules are documented on the helpers below.
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
//! Sizing is still self-determined, so the one thing the operators here cannot
//! do is let an assignment's target widen a signed operand *before* the
//! operation: `reg [15:0] r; r = -4'd12;` still negates in four bits.

use std::fmt;

use crate::parsers::constants::{VerilogBaseType, VerilogConstant};
use crate::parsers::expr::Expression;
use crate::parsers::identifier::Identifier;
use crate::parsers::operators::{BinaryOperator, UnaryOperator};
use crate::register::{sign_extend_to_i128, Chunk, Register, ONE, X, Z, ZERO};
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::{StateStore, MAX_CALL_DEPTH};

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
const MAX_SELECT_WIDTH: usize = 1 << 16;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EvalError {
    /// An identifier that has no entry in the [`StateStore`].
    UnknownIdentifier(String),
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
    /// A value too wide to evaluate; see [`MAX_ARITHMETIC_WIDTH`].
    WidthOverflow(usize),
    /// A `$name` used as a function that this simulator does not implement.
    UnknownSystemFunction(String),
    /// A system function called with a number of arguments it does not take.
    SystemFunctionArity {
        name: String,
        expected: String,
        found: usize,
    },
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::UnknownIdentifier(name) => {
                write!(f, "no value for identifier `{}`", name)
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
            EvalError::NonConstantSelectBound(text) => {
                write!(f, "part select bound `{}` is not a constant", text)
            }
            EvalError::WidthOverflow(width) => {
                write!(f, "{} bit value is too wide to evaluate", width)
            }
            EvalError::UnknownSystemFunction(name) => {
                write!(f, "unknown system function `${}`", name)
            }
            EvalError::SystemFunctionArity {
                name,
                expected,
                found,
            } => write!(f, "`${}` takes {}, but was given {}", name, expected, found),
        }
    }
}

impl std::error::Error for EvalError {}

/// Evaluates `expr` against the values in `store`.
///
/// The top of an expression is self-determined — nothing outside it can change
/// how it reads its own bits — so this is [`eval_in_context`] with a signed
/// context.
pub fn eval(expr: &Expression, store: &StateStore) -> Result<Register, EvalError> {
    eval_in_context(expr, store, true)
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
/// signed context of `true`, because nothing above it has a say.
fn eval_in_context(
    expr: &Expression,
    store: &StateStore,
    signed_context: bool,
) -> Result<Register, EvalError> {
    match expr {
        // Only a *leaf* has to be told that its context is unsigned. Every
        // operator below already asks its own operands, and an operand
        // evaluated in an unsigned context comes back unsigned — so by the time
        // an operator decides its result there is nothing left to demote.
        Expression::Constant(constant) => eval_constant(constant, signed_context),
        Expression::Identifier(id) => {
            let value = store
                .get(&id.name)
                .cloned()
                .ok_or_else(|| EvalError::UnknownIdentifier(id.name.clone()))?;
            Ok(demoted(value, signed_context))
        }
        Expression::Parenthetical(inner) => eval_in_context(inner, store, signed_context),
        Expression::Unary(op, operand) => {
            let context = if unary_keeps_signedness(op) {
                signed_context && expression_is_signed(operand, store)
            } else {
                true
            };
            eval_unary(op, &eval_in_context(operand, store, context)?)
        }
        Expression::Binary(lhs, op, rhs) => {
            let (left, right) = operand_contexts(op, lhs, rhs, store, signed_context);
            eval_binary(
                op,
                &eval_in_context(lhs, store, left)?,
                &eval_in_context(rhs, store, right)?,
            )
        }
        Expression::Conditional(condition, when_true, when_false) => {
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
                Some(true) => eval_in_context(when_true, store, arms),
                Some(false) => eval_in_context(when_false, store, arms),
                None => {
                    let (when_true, when_false) = (
                        eval_in_context(when_true, store, arms)?,
                        eval_in_context(when_false, store, arms)?,
                    );
                    Ok(merge(&when_true, &when_false).with_signedness(arms))
                }
            }
        }
        Expression::Concatenation(parts) => {
            if parts.is_empty() {
                return Err(EvalError::EmptyConcatenation);
            }
            let mut values = Vec::with_capacity(parts.len());
            for part in parts {
                values.push(eval(part, store)?);
            }
            Ok(Register::concatenated(&values))
        }
        Expression::BitSelect(id, index) => {
            let signal = store
                .get_signal(&id.name)
                .ok_or_else(|| EvalError::UnknownIdentifier(id.name.clone()))?;
            // An index that is unknown, or too large to be a bit number, selects `x`.
            match numeric(&eval(index, store)?)?.and_then(|value| i64::try_from(value).ok()) {
                Some(index) => Ok(logic_bit(signal.bit(index))),
                None => Ok(Register::unknown(1)),
            }
        }
        Expression::PartSelect(id, first, second) => {
            let signal = store
                .get_signal(&id.name)
                .ok_or_else(|| EvalError::UnknownIdentifier(id.name.clone()))?;
            let first = select_bound(first, store)?;
            let second = select_bound(second, store)?;
            let width = (first - second).unsigned_abs() as usize + 1;
            if width > MAX_SELECT_WIDTH {
                return Err(EvalError::WidthOverflow(width));
            }
            // The result runs from the first bound to the second, so a select
            // out of an ascending vector (`a[0:3]`) comes back in source order.
            let indices: Vec<i64> = if first >= second {
                (second..=first).rev().collect()
            } else {
                (first..=second).collect()
            };
            let bits: Vec<u8> = indices.into_iter().map(|i| signal.bit(i)).collect();
            Ok(Register::from_bits(bits))
        }
        Expression::FunctionCall(id, arguments) => Ok(demoted(
            call_function(id, arguments, store)?,
            signed_context,
        )),
        // `$signed(...)` is the one call that produces a signed value out of
        // nothing, so it is a leaf for this purpose too.
        Expression::SystemFunctionCall(name, arguments) => Ok(demoted(
            eval_system_function(name, arguments, store)?,
            signed_context,
        )),
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

    // An argument is self-determined: the function's own declaration says how
    // wide the variable it lands in is, and nothing around the call has a say.
    let mut values = Vec::with_capacity(arguments.len());
    for argument in arguments {
        values.push(eval(argument, store)?);
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

/// `value` as an unsigned one unless the context allows it to stay signed.
fn demoted(value: Register, signed_context: bool) -> Register {
    if signed_context {
        value
    } else {
        value.with_signedness(false)
    }
}

// ---------------------------------------------------------------------------
// Signedness
// ---------------------------------------------------------------------------

/// Where a binary operator's operands take their signedness from.
///
/// The distinction is not about the operator's arithmetic — it is about which
/// operands are *context-determined*, meaning the expression around them can
/// make them unsigned, and which decide for themselves.
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
    /// `< <= > >= == != === !== && ||`: both operands are self-determined and
    /// the one-bit result is unsigned however they compared.
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
        _ => OperandRule::SelfDetermined,
    }
}

/// The signed context each operand of `op` is evaluated in.
fn operand_contexts(
    op: &BinaryOperator,
    lhs: &Expression,
    rhs: &Expression,
    store: &StateStore,
    signed_context: bool,
) -> (bool, bool) {
    match operand_rule(op) {
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
        OperandRule::SelfDetermined => (true, true),
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
            OperandRule::SelfDetermined => false,
        },
        Expression::Conditional(_, when_true, when_false) => {
            expression_is_signed(when_true, store) && expression_is_signed(when_false, store)
        }
        // A concatenation and a select are bit vectors, not numbers: unsigned
        // however signed the things that went into them were.
        Expression::Concatenation(_)
        | Expression::BitSelect(_, _)
        | Expression::PartSelect(_, _, _) => false,
        // A function is as signed as it was declared to be, which is a
        // property of the declaration rather than of what it returns.
        Expression::FunctionCall(id, _) => store
            .function(&id.name)
            .is_some_and(|definition| definition.result.signed),
        Expression::SystemFunctionCall(name, _) => system_function_is_signed(name),
    }
}

/// Whether `$name(...)` hands back a signed value.
///
/// Everything that returns Verilog's `integer` does — including `$random`,
/// which is why half the numbers it draws are negative. `$time` is the
/// exception: `time` is a 64 bit *unsigned* type.
fn system_function_is_signed(name: &str) -> bool {
    match name {
        "unsigned" | "time" => false,
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

/// Every `$name` [`eval_system_function`] implements.
///
/// Callers that have to decide whether a `$name` is meaningful *before* running
/// it — [`TaskCall::compile`](crate::simulator::tasks::TaskCall::compile) — ask
/// here, so an unrecognised name is rejected in one place. A name listed but
/// not matched below still errors rather than evaluating to anything.
pub const SYSTEM_FUNCTIONS: [&str; 7] = [
    "time", "stime", "signed", "unsigned", "random", "bits", "clog2",
];

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
        "time" | "stime" => {
            arity("no arguments", &[0])?;
            let width = if name == "time" {
                TIME_WIDTH
            } else {
                SYSTEM_FUNCTION_WIDTH
            };
            Ok(Register::from_u128(
                store.time().unsigned_abs() as u128,
                width,
            ))
        }
        // A cast that changes no bit and no width: it says only how the bits
        // that are already there are to be read. Everything it changes happens
        // in the operator that receives the result — `$signed(4'b1000)` widens
        // as -8, compares as -8, and `>>>` on it is arithmetic.
        "signed" | "unsigned" => {
            arity("exactly one argument", &[1])?;
            eval(&arguments[0], store)
        }
        // `$random` and `$random(seed)`. The seed restarts the stream; see
        // [`StateStore::seed_random`].
        "random" => {
            arity("no arguments, or a seed", &[0, 1])?;
            if let Some(seed) = arguments.first() {
                match numeric(&eval(seed, store)?)? {
                    Some(value) => store.seed_random(value as u64),
                    // An unknown seed leaves the stream where it is; there is
                    // no number to restart it from.
                    None => {}
                }
            }
            Ok(Register::from_u128(
                store.next_random() as u128,
                SYSTEM_FUNCTION_WIDTH,
            ))
        }
        // The width of the operand, which every value here knows about itself.
        "bits" => {
            arity("exactly one argument", &[1])?;
            let width = eval(&arguments[0], store)?.width();
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

fn select_bound(expr: &Expression, store: &StateStore) -> Result<i64, EvalError> {
    let value = numeric(&eval(expr, store)?)?
        .and_then(|value| i64::try_from(value).ok())
        .ok_or_else(|| EvalError::NonConstantSelectBound(expr.to_contracted_string()))?;
    Ok(value)
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
fn decimal_bits(digits: &str) -> Result<Register, EvalError> {
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

/// `==` and `!=` produce one bit, and are `x` if either operand contains an
/// unknown bit. The narrower operand is widened first, sign extended when both
/// sides are signed — which is what makes `-1 == 32'hffffffff` true and
/// `$signed(4'b1111) == 4'd15` false.
fn logical_equality(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Register {
    if lhs.has_unknown() || rhs.has_unknown() {
        return Register::unknown(1);
    }
    let matched = matches!(op, BinaryOperator::LogicalEquality) == equal_values(lhs, rhs);
    logic_bit(if matched { ONE } else { ZERO })
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

    #[test]
    fn test_random_is_reproducible_from_the_default_seed() {
        // Every store starts the stream from the same seed, so two runs of the
        // same design draw the same numbers in the same order.
        let draw = || {
            let store = StateStore::new();
            (0..4)
                .map(|_| value_in("$random", &store))
                .collect::<Vec<_>>()
        };
        let first = draw();
        assert_eq!(first, draw());
        // Within one run the numbers advance rather than repeating.
        assert!(first.windows(2).all(|pair| pair[0] != pair[1]));
        // `$random` is Verilog's 32 bit integer.
        assert_eq!(bits_in("$random", &StateStore::new()).len(), 32);
    }

    #[test]
    fn test_random_with_a_seed_restarts_the_stream() {
        let store = StateStore::new();
        let seeded: Vec<u128> = (0..3).map(|_| value_in("$random(7)", &store)).collect();
        // Re-seeding with the same value gives the same number back, which is
        // what makes a seeded design's stimulus repeatable.
        assert_eq!(seeded[0], seeded[1]);
        assert_eq!(seeded[1], seeded[2]);

        let store = StateStore::new();
        assert_eq!(value_in("$random(7)", &store), seeded[0]);
        assert_ne!(value_in("$random(9)", &store), seeded[0]);
    }

    #[test]
    fn test_every_listed_system_function_evaluates() {
        // [`SYSTEM_FUNCTIONS`] is what `TaskCall::compile` trusts when it
        // decides a `$name` is meaningful, so a name listed there but missing
        // from the evaluator would be accepted and then fail late.
        let store = sample_store();
        for name in SYSTEM_FUNCTIONS {
            let source = match name {
                "time" | "stime" | "random" => format!("${}", name),
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
}

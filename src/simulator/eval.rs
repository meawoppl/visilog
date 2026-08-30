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
//! Everything is treated as **unsigned**. The AST carries no signedness, so
//! `>>>` behaves like `>>`, `<<<` behaves like `<<`, and relational operators
//! compare magnitudes rather than two's complement values.

use std::fmt;

use crate::parsers::constants::{VerilogBaseType, VerilogConstant};
use crate::parsers::expr::Expression;
use crate::parsers::operators::{BinaryOperator, UnaryOperator};
use crate::register::{Register, ONE, X, Z, ZERO};
use crate::simulator::state_store::StateStore;

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
    /// Function calls are parsed but not evaluated.
    UnsupportedFunctionCall(String),
    /// A literal whose text could not be turned into bits.
    MalformedConstant(String),
    /// `{}` with nothing in it.
    EmptyConcatenation,
    /// A part select bound that did not evaluate to a usable constant.
    NonConstantSelectBound(String),
    /// A value too wide to evaluate; see [`MAX_ARITHMETIC_WIDTH`].
    WidthOverflow(usize),
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
        }
    }
}

impl std::error::Error for EvalError {}

/// Evaluates `expr` against the values in `store`.
pub fn eval(expr: &Expression, store: &StateStore) -> Result<Register, EvalError> {
    match expr {
        Expression::Constant(constant) => eval_constant(constant),
        Expression::Identifier(id) => store
            .get(&id.name)
            .cloned()
            .ok_or_else(|| EvalError::UnknownIdentifier(id.name.clone())),
        Expression::Parenthetical(inner) => eval(inner, store),
        Expression::Unary(op, operand) => eval_unary(op, &eval(operand, store)?),
        Expression::Binary(lhs, op, rhs) => eval_binary(op, &eval(lhs, store)?, &eval(rhs, store)?),
        Expression::Conditional(condition, when_true, when_false) => {
            // Only the taken branch is evaluated. When the condition is `x` both
            // branches are needed, and the result merges them bit by bit: bits
            // that agree survive, bits that disagree become `x`.
            match truth(&eval(condition, store)?) {
                Some(true) => eval(when_true, store),
                Some(false) => eval(when_false, store),
                None => Ok(merge(&eval(when_true, store)?, &eval(when_false, store)?)),
            }
        }
        Expression::Concatenation(parts) => {
            if parts.is_empty() {
                return Err(EvalError::EmptyConcatenation);
            }
            let mut bits = Vec::new();
            for part in parts {
                bits.extend_from_slice(eval(part, store)?.get_raw());
            }
            Ok(Register::from_bits(bits))
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
            Ok(Register::from_bits(
                indices.into_iter().map(|i| signal.bit(i)).collect(),
            ))
        }
        Expression::FunctionCall(id, _) => Err(EvalError::UnsupportedFunctionCall(id.name.clone())),
    }
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

fn eval_constant(constant: &VerilogConstant) -> Result<Register, EvalError> {
    constant_bits(constant.size(), constant.base_type(), constant.digits())
}

/// Converts the pieces of a literal — its optional size, its base and its
/// digits as written — into bits. An absent size means
/// [`UNSIZED_CONSTANT_WIDTH`]; a size narrower than the digits truncates,
/// keeping the least significant bits. `_` separators are ignored.
fn constant_bits(
    size: Option<usize>,
    base: &VerilogBaseType,
    digits: &str,
) -> Result<Register, EvalError> {
    let malformed = || EvalError::MalformedConstant(digits.to_string());

    let digits: String = digits.chars().filter(|c| *c != '_').collect();
    if digits.is_empty() {
        return Err(malformed());
    }

    let bits = match base {
        VerilogBaseType::Binary => based_bits(&digits, 1)?,
        VerilogBaseType::Octal => based_bits(&digits, 3)?,
        VerilogBaseType::Hexadecimal => based_bits(&digits, 4)?,
        VerilogBaseType::Decimal => decimal_bits(&digits)?,
    };

    let width = size.unwrap_or(UNSIZED_CONSTANT_WIDTH);
    if width == 0 {
        return Err(malformed());
    }

    Ok(Register::from_bits(bits).extend_msb(width))
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
fn based_bits(digits: &str, bits_per_digit: usize) -> Result<Vec<u8>, EvalError> {
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
    Ok(bits)
}

/// Decimal digits, rendered in the fewest bits that hold the value.
fn decimal_bits(digits: &str) -> Result<Vec<u8>, EvalError> {
    let value = digits
        .parse::<u128>()
        .map_err(|_| EvalError::MalformedConstant(digits.to_string()))?;
    let width = (128 - value.leading_zeros() as usize).max(1);
    Ok(Register::from_u128(value, width).get_raw().clone())
}

// ---------------------------------------------------------------------------
// Unary operators
// ---------------------------------------------------------------------------

fn eval_unary(op: &UnaryOperator, operand: &Register) -> Result<Register, EvalError> {
    match op {
        // `+a` is a no-op on an unsigned value.
        UnaryOperator::Positive => Ok(operand.clone()),
        // Two's complement in the operand's own width. Any unknown bit makes
        // the whole result unknown, because a carry can reach every bit.
        UnaryOperator::Negative => {
            let width = operand.width().max(1);
            match numeric(operand)? {
                Some(value) => Ok(Register::from_u128(
                    value.wrapping_neg() & width_mask(width),
                    width,
                )),
                None => Ok(Register::unknown(width)),
            }
        }
        // Bit for bit, width preserving. `z` inverts to `x`, matching Verilog:
        // an undriven bit is not a known 0 or 1.
        UnaryOperator::BitwiseNegation => Ok(Register::from_bits(
            operand.get_raw().iter().map(|&bit| invert(bit)).collect(),
        )),
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
    if operand.get_raw().iter().any(|&bit| bit == ZERO) {
        ZERO
    } else if operand.has_unknown() {
        X
    } else {
        ONE
    }
}

/// `|a`: a single 1 forces 1 even when other bits are unknown.
fn reduce_or(operand: &Register) -> u8 {
    if operand.get_raw().iter().any(|&bit| bit == ONE) {
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
    let ones = operand.get_raw().iter().filter(|&&bit| bit == ONE).count();
    if ones % 2 == 0 {
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
    let width = lhs.width().max(rhs.width()).max(1);
    let (Some(a), Some(b)) = (numeric(lhs)?, numeric(rhs)?) else {
        return Ok(Register::unknown(width));
    };

    let mask = width_mask(width);
    let value = match op {
        BinaryOperator::Addition => a.wrapping_add(b) & mask,
        BinaryOperator::Subtraction => a.wrapping_sub(b) & mask,
        BinaryOperator::Multiplication => a.wrapping_mul(b) & mask,
        BinaryOperator::Division | BinaryOperator::Modulus if b == 0 => {
            return Ok(Register::unknown(width));
        }
        BinaryOperator::Division => (a / b) & mask,
        BinaryOperator::Modulus => (a % b) & mask,
        other => unreachable!("{} is not an arithmetic operator", other),
    };
    Ok(Register::from_u128(value, width))
}

/// `**` takes the width of its left operand, per IEEE 1364 table 5-22.
fn power(lhs: &Register, rhs: &Register) -> Result<Register, EvalError> {
    let width = lhs.width().max(1);
    let (Some(base), Some(exponent)) = (numeric(lhs)?, numeric(rhs)?) else {
        return Ok(Register::unknown(width));
    };

    let mask = width_mask(width);
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
    Ok(Register::from_u128(result, width))
}

/// `& | ^ ^~` applied bit by bit. The narrower operand is zero-extended to the
/// width of the wider one.
fn bitwise(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Register {
    let width = lhs.width().max(rhs.width()).max(1);
    let lhs = lhs.resize(width);
    let rhs = rhs.resize(width);
    let bits = lhs
        .get_raw()
        .iter()
        .zip(rhs.get_raw().iter())
        .map(|(&a, &b)| bitwise_bit(op, a, b))
        .collect();
    Register::from_bits(bits)
}

/// The truth tables of IEEE 1364 table 5-1. `z` behaves exactly like `x`: a
/// bit that is not driven is not a known value.
fn bitwise_bit(op: &BinaryOperator, a: u8, b: u8) -> u8 {
    match op {
        BinaryOperator::BitwiseAnd => {
            if a == ZERO || b == ZERO {
                ZERO
            } else if is_unknown(a) || is_unknown(b) {
                X
            } else {
                ONE
            }
        }
        BinaryOperator::BitwiseOr | BinaryOperator::BitwiseInclusiveOr => {
            if a == ONE || b == ONE {
                ONE
            } else if is_unknown(a) || is_unknown(b) {
                X
            } else {
                ZERO
            }
        }
        BinaryOperator::BitwiseXOr => {
            if is_unknown(a) || is_unknown(b) {
                X
            } else if a == b {
                ZERO
            } else {
                ONE
            }
        }
        BinaryOperator::BitwiseXNor => invert(bitwise_bit(&BinaryOperator::BitwiseXOr, a, b)),
        other => unreachable!("{} is not a bitwise operator", other),
    }
}

/// Shifts move bits rather than numbers, so `x` and `z` bits survive being
/// shifted. The result keeps the left operand's width and vacated positions
/// fill with `0`. An unknown shift amount makes the whole result `x`.
///
/// `>>>` and `<<<` are treated as `>>` and `<<`: nothing in the AST records
/// signedness, so there is no sign bit to replicate.
fn shift(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Result<Register, EvalError> {
    let width = lhs.width().max(1);
    let Some(amount) = numeric(rhs)? else {
        return Ok(Register::unknown(width));
    };
    let amount = amount.min(lhs.width() as u128) as usize;

    let left = matches!(
        op,
        BinaryOperator::ShiftLeft | BinaryOperator::ArithmeticShiftLeft
    );
    let mut bits = Vec::with_capacity(lhs.width());
    if left {
        bits.extend_from_slice(&lhs.get_raw()[amount..]);
        bits.extend(std::iter::repeat(ZERO).take(amount));
    } else {
        bits.extend(std::iter::repeat(ZERO).take(amount));
        bits.extend_from_slice(&lhs.get_raw()[..lhs.width() - amount]);
    }
    Ok(Register::from_bits(bits))
}

/// `< <= > >=` produce one bit. Comparison is unsigned; an unknown bit in
/// either operand makes the answer `x`.
fn relational(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Result<Register, EvalError> {
    let (Some(a), Some(b)) = (numeric(lhs)?, numeric(rhs)?) else {
        return Ok(Register::unknown(1));
    };
    let result = match op {
        BinaryOperator::LessThan => a < b,
        BinaryOperator::LessThanOrEqual => a <= b,
        BinaryOperator::GreaterThan => a > b,
        BinaryOperator::GreaterThanOrEqual => a >= b,
        other => unreachable!("{} is not a relational operator", other),
    };
    Ok(logic_bit(if result { ONE } else { ZERO }))
}

/// `==` and `!=` produce one bit, and are `x` if either operand contains an
/// unknown bit.
fn logical_equality(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Register {
    if lhs.has_unknown() || rhs.has_unknown() {
        return Register::unknown(1);
    }
    let width = lhs.width().max(rhs.width());
    let equal = lhs.resize(width) == rhs.resize(width);
    let matched = matches!(op, BinaryOperator::LogicalEquality) == equal;
    logic_bit(if matched { ONE } else { ZERO })
}

/// `===` and `!==` compare all four states exactly and are never `x`. The
/// narrower operand is zero-extended, so `4'b0001 === 1'b1` holds.
fn case_equality(op: &BinaryOperator, lhs: &Register, rhs: &Register) -> Register {
    let width = lhs.width().max(rhs.width());
    let equal = lhs.resize(width) == rhs.resize(width);
    let matched = matches!(op, BinaryOperator::CaseEquality) == equal;
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

fn is_unknown(bit: u8) -> bool {
    bit == X || bit == Z
}

/// `0` and `1` swap; `x` and `z` invert to `x`.
fn invert(bit: u8) -> u8 {
    match bit {
        ZERO => ONE,
        ONE => ZERO,
        _ => X,
    }
}

fn logic_bit(bit: u8) -> Register {
    Register::from_bits(vec![bit])
}

/// A register used as a condition: any `1` bit is true, all-zero is false, and
/// anything else (only unknown bits and zeros) is unknown.
fn truth(register: &Register) -> Option<bool> {
    if register.get_raw().iter().any(|&bit| bit == ONE) {
        Some(true)
    } else if register.has_unknown() {
        None
    } else {
        Some(false)
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
    let lhs = lhs.resize(width);
    let rhs = rhs.resize(width);
    let bits = lhs
        .get_raw()
        .iter()
        .zip(rhs.get_raw().iter())
        .map(|(&a, &b)| if a == b { a } else { X })
        .collect();
    Register::from_bits(bits)
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
}

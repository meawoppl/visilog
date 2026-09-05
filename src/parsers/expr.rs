use super::{
    base::RawToken,
    constants::{verilog_const, VerilogConstant},
    identifier::{identifier, Identifier},
    operators::{unary_operator, BinaryOperator, UnaryOperator},
    simple::ws,
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, char},
    combinator::{map, map_res, opt, recognize},
    multi::{fold_many0, many1, separated_list0, separated_list1},
    sequence::{pair, preceded, tuple},
    IResult,
};
use nom::{combinator::peek, sequence::delimited};

#[derive(PartialEq, Clone)]
pub enum Expression {
    Constant(VerilogConstant),
    Identifier(Identifier),
    Unary(UnaryOperator, Box<Expression>),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>), // condition ? true_expr : false_expr
    Parenthetical(Box<Expression>),
    Concatenation(Vec<Expression>),
    FunctionCall(Identifier, Vec<Expression>),
    /// `$time`, `$random`, `$signed(a)` — a call to one of the simulator's own
    /// functions, named without its `$`.
    ///
    /// Deliberately *not* [`Expression::FunctionCall`]: that one names a
    /// function the design declares, and resolves against the design. A
    /// `$name` resolves against the simulator's built-in table instead, and
    /// can never be an identifier the design wrote. An argument list is
    /// optional, so `$time` and `$random` carry an empty one.
    SystemFunctionCall(String, Vec<Expression>),
    BitSelect(Identifier, Box<Expression>),
    PartSelect(Identifier, Box<Expression>, Box<Expression>),
}

impl Expression {
    pub fn to_contracted_string(&self) -> String {
        match self {
            Expression::Constant(c) => format!("{}", c),
            Expression::Identifier(id) => format!("{}", id.name),
            Expression::Unary(op, expr) => {
                format!("{}{}", op.raw_token(), expr.to_contracted_string())
            }
            Expression::Binary(lhs, op, rhs) => format!(
                "{} {} {}",
                lhs.to_contracted_string(),
                op.raw_token(),
                rhs.to_contracted_string()
            ),
            Expression::Conditional(cond, true_expr, false_expr) => format!(
                "{} ? {} : {}",
                cond.to_contracted_string(),
                true_expr.to_contracted_string(),
                false_expr.to_contracted_string()
            ),
            Expression::Parenthetical(expr) => format!("({})", expr.to_contracted_string()),
            Expression::Concatenation(exprs) => format!(
                "{{{}}}",
                exprs
                    .iter()
                    .map(|e| e.to_contracted_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expression::FunctionCall(id, args) => format!(
                "{}({})",
                id.name,
                args.iter()
                    .map(|e| e.to_contracted_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expression::SystemFunctionCall(name, args) if args.is_empty() => {
                format!("${}", name)
            }
            Expression::SystemFunctionCall(name, args) => format!(
                "${}({})",
                name,
                args.iter()
                    .map(|e| e.to_contracted_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expression::BitSelect(ident, index) => {
                format!("{}[{}]", ident.name, index.to_contracted_string())
            }
            Expression::PartSelect(ident, start, end) => format!(
                "{}[{}:{}]",
                ident.name,
                start.to_contracted_string(),
                end.to_contracted_string()
            ),
        }
    }

    pub fn to_ast_string(&self, indent: usize) -> String {
        let indent_str = "  ".repeat(indent);
        match self {
            Expression::Constant(c) => format!("{}Constant({})", indent_str, c),
            Expression::Identifier(id) => format!("{}Identifier(\"{}\")", indent_str, id.name),
            Expression::Unary(op, expr) => format!(
                "{}Unary{}(\n{},\n{})",
                indent_str,
                op,
                expr.to_ast_string(indent + 1),
                indent_str,
            ),
            Expression::Binary(lhs, op, rhs) => format!(
                "{}Binary{}(\n{}{},\n{}{}\n{})",
                indent_str,
                op.to_string(),
                indent_str,
                lhs.to_ast_string(indent + 1),
                indent_str,
                rhs.to_ast_string(indent + 1),
                indent_str,
            ),
            Expression::Conditional(cond, true_expr, false_expr) => format!(
                "{}Conditional(\n{}{},\n{}{},\n{}{})",
                indent_str,
                indent_str,
                cond.to_ast_string(indent + 1),
                indent_str,
                true_expr.to_ast_string(indent + 1),
                indent_str,
                false_expr.to_ast_string(indent + 1)
            ),
            Expression::Parenthetical(expr) => format!(
                "{}Parenthetical(\n{})",
                indent_str,
                expr.to_ast_string(indent + 1)
            ),
            Expression::Concatenation(exprs) => format!(
                "{}Concatenation(\n{})",
                indent_str,
                exprs
                    .iter()
                    .map(|e| e.to_ast_string(indent + 1))
                    .collect::<Vec<_>>()
                    .join(",\n")
            ),
            Expression::FunctionCall(id, args) => format!(
                "{}FunctionCall({},\n{})",
                indent_str,
                id.name,
                args.iter()
                    .map(|e| e.to_ast_string(indent + 1))
                    .collect::<Vec<_>>()
                    .join(",\n")
            ),
            Expression::SystemFunctionCall(name, args) => format!(
                "{}SystemFunctionCall(${},\n{})",
                indent_str,
                name,
                args.iter()
                    .map(|e| e.to_ast_string(indent + 1))
                    .collect::<Vec<_>>()
                    .join(",\n")
            ),
            Expression::BitSelect(ident, index) => format!(
                "{}BitSelect(\n{}{},\n{}{})",
                indent_str,
                ident.name,
                indent_str,
                index.to_ast_string(indent + 1),
                indent_str
            ),
            Expression::PartSelect(ident, start, end) => format!(
                "{}PartSelect(\n{}{},\n{}{},\n{}{})",
                indent_str,
                ident.name,
                indent_str,
                start.to_ast_string(indent + 1),
                indent_str,
                end.to_ast_string(indent + 1),
                indent_str
            ),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_contracted_string())
    }
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            self.to_contracted_string(),
            self.to_ast_string(0)
        )
    }
}

fn parenthetical(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(tag("("), ws(verilog_expression), tag(")")),
        |expr| Expression::Parenthetical(Box::new(expr)),
    )(input)
}

// TODO(meawoppl) - support the multiplication concatentation operator roughly here
fn concatenation(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            tag("{"),
            separated_list1(tag(","), ws(verilog_expression)),
            tag("}"),
        ),
        |exprs| Expression::Concatenation(exprs),
    )(input)
}

/// `$` followed by a name, without the `$`.
///
/// The `$` is a token of its own and is deliberately not folded into
/// [`identifier`]: `$foo` names the simulator, never a signal, so letting it
/// parse as an identifier would make it legal everywhere a signal is. Shared
/// with the statement form in
/// [`behavior`](crate::parsers::behavior::parse_system_task), so there is one
/// definition of what a `$name` looks like.
pub fn system_name(input: &str) -> IResult<&str, String> {
    map(
        preceded(
            char('$'),
            recognize(pair(
                alt((alpha1, tag("_"))),
                take_while(|c: char| c.is_alphanumeric() || c == '_' || c == '$'),
            )),
        ),
        |name: &str| name.to_string(),
    )(input)
}

/// `$time`, `$random`, `$signed(a)` — a system function used as an operand.
///
/// The argument list is optional because the commonest system functions —
/// `$time` and `$random` — are written bare. An empty list and an absent one
/// are the same thing.
fn system_function_call(input: &str) -> IResult<&str, Expression> {
    let (input, name) = system_name(input)?;
    let (input, args) = opt(delimited(
        tag("("),
        separated_list0(tag(","), ws(verilog_expression)),
        tag(")"),
    ))(input)?;
    Ok((
        input,
        Expression::SystemFunctionCall(name, args.unwrap_or_default()),
    ))
}

fn fn_call(input: &str) -> IResult<&str, Expression> {
    let (input, id) = identifier(input)?;
    let (input, args) = delimited(
        tag("("),
        separated_list1(tag(","), ws(verilog_expression)),
        tag(")"),
    )(input)?;

    Ok((input, Expression::FunctionCall(id, args)))
}

fn operand(input: &str) -> IResult<&str, Expression> {
    // NOTE(meawoppl)
    // fn_call has to go before identifier, as function names
    // are valid identifiers
    ws(operand_no_ws)(input)
}

// According gemini (dubious source) the following convention is necessary:
// 1. Reduction Operators:  When using unary reduction operators (e.g., &a, |b, ^c),
//    whitespace is not allowed between the operator and the operand.
// 2. Ambiguity with &:  The & symbol can be both a bitwise AND operator
//    and a unary reduction AND operator. To avoid ambiguity, whitespace
//    is sometimes necessary:
//      a & b (bitwise AND)
//      a & &b (reduction AND of b, then bitwise AND with a)

fn operand_no_ws(input: &str) -> IResult<&str, Expression> {
    // NOTE(meawoppl)
    // fn_call has to go before identifier, as function names
    // are valid identifiers
    alt((
        // A `$name` can start nothing else, so it is unambiguous first.
        system_function_call,
        fn_call,
        bit_select,
        part_select,
        map(identifier, Expression::Identifier),
        map(verilog_const, Expression::Constant),
        parenthetical,
        concatenation,
    ))(input)
}

/// `a[i]` — a single-bit select. The index is a full expression, so
/// `a[n]`, `a[n+1]` and `a[c ? x : y]` are all accepted.
pub fn bit_select(input: &str) -> IResult<&str, Expression> {
    let (input, expr) = identifier(input)?;
    let (input, index) = delimited(tag("["), ws(verilog_expression), tag("]"))(input)?;
    Ok((input, Expression::BitSelect(expr, Box::new(index))))
}

/// `a[msb:lsb]` — a range select. Both bounds are full expressions.
pub fn part_select(input: &str) -> IResult<&str, Expression> {
    let (input, ident) = identifier(input)?;
    let (input, (start, end)) = delimited(
        tag("["),
        pair(
            ws(verilog_expression),
            preceded(tag(":"), ws(verilog_expression)),
        ),
        tag("]"),
    )(input)?;
    Ok((
        input,
        Expression::PartSelect(ident, Box::new(start), Box::new(end)),
    ))
}

// Alright, this is following table 5-4 in the IEEE 1364-2005 standard
// Use use the fold_many0() combinator to parse the expression
// by composing layers of order of operation here. There are 14 layers as a result
// They are enumerated below with a brief description of the operation(s)

// Layer 1: Unary operators
fn unary_operator_layer(input: &str) -> IResult<&str, Expression> {
    alt((
        // `ws` wraps the unary *term*, exactly as `operand` wraps a bare one,
        // so the whitespace on either side of `-a` belongs to the term and the
        // operator that follows it gets a chance to match. The junction between
        // the operator and what it applies to stays `operand_no_ws`, and that
        // is load-bearing: allow whitespace there and `a && b` reads as
        // `a & (&b)`, because the first `&` matches the bitwise layer and the
        // second is then a reduction operator applied to ` b`.
        ws(map_res(
            tuple((many1(unary_operator), operand_no_ws)),
            |(ops, exp)| {
                let mut result = exp;

                // These apply right to left somewhat confusingly
                for op in ops.iter().rev() {
                    result = Expression::Unary(op.clone(), Box::new(result));
                }
                Ok::<_, nom::Err<(&str, nom::error::ErrorKind)>>(result)
            },
        )),
        operand,
    ))(input)
}

// Layer 2: Exponentiation Operator
fn exp_layer(input: &str) -> IResult<&str, Expression> {
    let (input, init) = unary_operator_layer(input)?;

    fold_many0(
        pair(tag("**"), unary_operator_layer),
        move || init.clone(),
        |acc, (_, val): (&str, Expression)| {
            Expression::Binary(Box::new(acc), BinaryOperator::Power, Box::new(val))
        },
    )(input)
}

// Layer 3: Multiplication, Division, Modulus Operators
fn mul_div_layer(input: &str) -> IResult<&str, Expression> {
    let (input, init) = exp_layer(input)?;

    fold_many0(
        pair(alt((tag("*"), tag("/"), tag("%"))), exp_layer),
        move || init.clone(),
        |acc, (op, val): (&str, Expression)| {
            let token = match op {
                "*" => BinaryOperator::Multiplication,
                "/" => BinaryOperator::Division,
                "%" => BinaryOperator::Modulus,
                _ => unreachable!(),
            };
            Expression::Binary(Box::new(acc), token, Box::new(val))
        },
    )(input)
}
// Layer 4: Addition, Subtraction Operators
fn add_sub_layer(input: &str) -> IResult<&str, Expression> {
    let (input, init) = mul_div_layer(input)?;

    fold_many0(
        pair(alt((tag("+"), tag("-"))), mul_div_layer),
        move || init.clone(),
        |acc, (op, val): (&str, Expression)| {
            let token = match op {
                "+" => BinaryOperator::Addition,
                "-" => BinaryOperator::Subtraction,
                _ => unreachable!(),
            };
            Expression::Binary(Box::new(acc), token, Box::new(val))
        },
    )(input)
}
// Layer 5: Shift Operators
fn shift_layer(input: &str) -> IResult<&str, Expression> {
    let (input, init) = add_sub_layer(input)?;

    fold_many0(
        pair(
            alt((tag("<<<"), tag(">>>"), tag("<<"), tag(">>"))),
            add_sub_layer,
        ),
        move || init.clone(),
        |acc, (op, val): (&str, Expression)| {
            let token = match op {
                "<<<" => BinaryOperator::ArithmeticShiftLeft,
                ">>>" => BinaryOperator::ArithmeticShiftRight,
                "<<" => BinaryOperator::ShiftLeft,
                ">>" => BinaryOperator::ShiftRight,
                _ => unreachable!(),
            };
            Expression::Binary(Box::new(acc), token, Box::new(val))
        },
    )(input)
}
// Layer 6: Relational Operators
fn relational_layer(input: &str) -> IResult<&str, Expression> {
    let (input, init) = shift_layer(input)?;

    fold_many0(
        pair(alt((tag("<="), tag("<"), tag(">="), tag(">"))), shift_layer),
        move || init.clone(),
        |acc, (op, val): (&str, Expression)| {
            let token = match op {
                "<" => BinaryOperator::LessThan,
                "<=" => BinaryOperator::LessThanOrEqual,
                ">" => BinaryOperator::GreaterThan,
                ">=" => BinaryOperator::GreaterThanOrEqual,
                _ => unreachable!(),
            };
            Expression::Binary(Box::new(acc), token, Box::new(val))
        },
    )(input)
}

// Layer 7: Equality Operators
fn equality_layer(input: &str) -> IResult<&str, Expression> {
    let (input, init) = relational_layer(input)?;

    fold_many0(
        pair(
            alt((tag("!=="), tag("==="), tag("=="), tag("!="))),
            relational_layer,
        ),
        move || init.clone(),
        |acc, (op, val): (&str, Expression)| {
            let token = match op {
                "===" => BinaryOperator::CaseEquality,
                "!==" => BinaryOperator::CaseInequality,
                "==" => BinaryOperator::LogicalEquality,
                "!=" => BinaryOperator::LogicalInequality,
                _ => unreachable!(),
            };
            Expression::Binary(Box::new(acc), token, Box::new(val))
        },
    )(input)
}

// Layer 8: Bitwise AND Operator
fn bitwise_and_layer(input: &str) -> IResult<&str, Expression> {
    let (input, init) = equality_layer(input)?;

    fold_many0(
        pair(tag("&"), equality_layer),
        move || init.clone(),
        |acc, (_, val): (&str, Expression)| {
            Expression::Binary(Box::new(acc), BinaryOperator::BitwiseAnd, Box::new(val))
        },
    )(input)
}
// Layer 9: Bitwise XOR/XNOR Operators
fn bitwise_xor_xnor_layer(input: &str) -> IResult<&str, Expression> {
    let (input, init) = bitwise_and_layer(input)?;

    fold_many0(
        pair(alt((tag("^~"), tag("^"), tag("~^"))), bitwise_and_layer),
        move || init.clone(),
        |acc, (op, val): (&str, Expression)| {
            let token = match op {
                "^" => BinaryOperator::BitwiseXOr,
                "~^" => BinaryOperator::BitwiseXNor,
                "^~" => BinaryOperator::BitwiseXNor,
                _ => unreachable!(),
            };
            Expression::Binary(Box::new(acc), token, Box::new(val))
        },
    )(input)
}
// Layer 10: Bitwise OR Operator
fn bitwise_or_layer(input: &str) -> IResult<&str, Expression> {
    let (input, init) = bitwise_xor_xnor_layer(input)?;

    fold_many0(
        pair(tag("|"), bitwise_xor_xnor_layer),
        move || init.clone(),
        |acc, (_, val): (&str, Expression)| {
            Expression::Binary(Box::new(acc), BinaryOperator::BitwiseOr, Box::new(val))
        },
    )(input)
}
// Layer 11: Logical AND Operator
fn logical_and_layer(input: &str) -> IResult<&str, Expression> {
    let (input, init) = bitwise_or_layer(input)?;

    fold_many0(
        pair(tag("&&"), bitwise_or_layer),
        move || init.clone(),
        |acc, (_, val): (&str, Expression)| {
            Expression::Binary(Box::new(acc), BinaryOperator::LogicalAnd, Box::new(val))
        },
    )(input)
}
// Layer 12: Logical OR Operator
fn logical_or_layer(input: &str) -> IResult<&str, Expression> {
    let (input, init) = logical_and_layer(input)?;

    fold_many0(
        pair(tag("||"), logical_and_layer),
        move || init.clone(),
        |acc, (_, val): (&str, Expression)| {
            Expression::Binary(Box::new(acc), BinaryOperator::LogicalOr, Box::new(val))
        },
    )(input)
}

// Layer 13: Conditional Operator
fn conditional_layer(input: &str) -> IResult<&str, Expression> {
    // Slightly hacky way to avoid left recursion
    // parse an expression (must lead a conditional)
    // The ensures the recursive call will always have less tokens
    // than the previous one...
    let (remaining, init) = logical_or_layer(input)?;

    // Escape path if this is an expression that doesn't have a conditional
    if remaining.len() == 0 {
        return Ok((remaining, init));
    }

    // If we made it here, we -may- have a conditional
    let is_ternary = peek(ws(tag("?")))(remaining).is_ok();

    // Bail if the next char isn't a ternary operator
    if !is_ternary {
        return Ok((remaining, init));
    }

    // Now can we can recursive sub-descend the remaining bits
    let (after_ternary, tf_pair) = map_res(
        tuple((
            ws(tag("?")),
            verilog_expression,
            ws(tag(":")),
            verilog_expression,
        )),
        move |(_, true_expr, _, false_expr)| {
            Ok::<_, nom::Err<nom::error::Error<&str>>>((true_expr, false_expr))
        },
    )(remaining)?;

    if after_ternary.len() < remaining.len() {
        return Ok((
            after_ternary,
            Expression::Conditional(Box::new(init), Box::new(tf_pair.0), Box::new(tf_pair.1)),
        ));
    } else {
        return Ok((remaining, init));
    }
}

// Layer 14: Concatenation Operators

pub fn verilog_expression(input: &str) -> IResult<&str, Expression> {
    conditional_layer(input)
}

#[cfg(test)]
mod tests {
    use rand::{RngCore, SeedableRng};

    use crate::parsers::helpers::{assert_parses, assert_parses_to};

    use super::*;

    #[test]
    fn test_constant_expression() {
        let expressions = vec![
            "32'd12",
            "8'hFF",
            "16'b1010_1010",
            "123",
            "'h1234",
            "'b101",
            "'o123",
            "'x123",
            "0",
            "1",
            "2",
        ];

        for to_parse in expressions {
            let expr = assert_parses(verilog_expression, to_parse);
            match expr {
                Expression::Constant(_) => {}
                _ => panic!("Expected a constant expression"),
            }
        }
    }

    #[test]
    fn test_unary_ops() {
        let expressions = vec![
            (
                "--2",
                Expression::Unary(
                    UnaryOperator::Negative,
                    Box::new(Expression::Unary(
                        UnaryOperator::Negative,
                        Box::new(Expression::Constant(VerilogConstant::from_int(2))),
                    )),
                ),
            ),
            (
                "++2",
                Expression::Unary(
                    UnaryOperator::Positive,
                    Box::new(Expression::Unary(
                        UnaryOperator::Positive,
                        Box::new(Expression::Constant(VerilogConstant::from_int(2))),
                    )),
                ),
            ),
            (
                "+-2",
                Expression::Unary(
                    UnaryOperator::Positive,
                    Box::new(Expression::Unary(
                        UnaryOperator::Negative,
                        Box::new(Expression::Constant(VerilogConstant::from_int(2))),
                    )),
                ),
            ),
        ];

        for (expr, expected) in expressions {
            assert_parses_to(verilog_expression, expr, expected);
        }
    }

    #[test]
    fn test_negation_addition_identifiers() {
        let expressions = vec![(
            "-x+y",
            Expression::Binary(
                Box::new(Expression::Unary(
                    UnaryOperator::Negative,
                    Box::new(Expression::Identifier(Identifier::new("x".to_string()))),
                )),
                BinaryOperator::Addition,
                Box::new(Expression::Identifier(Identifier::new("y".to_string()))),
            ),
        )];

        for (expr, expected) in expressions {
            assert_parses_to(verilog_expression, expr, expected);
        }
    }

    /// A unary term has to consume the whitespace *after* it, or the operator
    /// that follows never gets a chance to match: `-1 < 0` used to leave
    /// `" < 0"` behind while `-1<0` parsed. A module body is parsed with
    /// `many0`, so the leftover presented as the whole module failing at
    /// `endmodule` rather than as an expression problem.
    #[test]
    fn test_unary_term_consumes_the_whitespace_after_it() {
        let a = || Expression::Identifier(Identifier::new("a".to_string()));
        let b = || Expression::Identifier(Identifier::new("b".to_string()));
        let unary = |op: UnaryOperator, e: Expression| Expression::Unary(op, Box::new(e));
        let binary = |l: Expression, op: BinaryOperator, r: Expression| {
            Expression::Binary(Box::new(l), op, Box::new(r))
        };

        let less_than = binary(
            unary(
                UnaryOperator::Negative,
                Expression::Constant(VerilogConstant::from_int(1)),
            ),
            BinaryOperator::LessThan,
            Expression::Constant(VerilogConstant::from_int(0)),
        );
        assert_parses_to(verilog_expression, "-1<0", less_than.clone());
        assert_parses_to(verilog_expression, "-1 < 0", less_than);

        let and = binary(
            unary(UnaryOperator::BitwiseNegation, a()),
            BinaryOperator::BitwiseAnd,
            b(),
        );
        assert_parses_to(verilog_expression, "~a&b", and.clone());
        assert_parses_to(verilog_expression, "~a & b", and);

        assert_parses_to(
            verilog_expression,
            "-a + b",
            binary(
                unary(UnaryOperator::Negative, a()),
                BinaryOperator::Addition,
                b(),
            ),
        );
    }

    /// Skipping the whitespace *around* a unary term must not let any into the
    /// junction between the operator and what it applies to. That junction is
    /// what tells `a && b` from `a & (&b)`: the bitwise layer takes the first
    /// `&`, and the second one is only a reduction operator if it touches its
    /// operand.
    #[test]
    fn test_a_spaced_reduction_operator_does_not_shadow_the_logical_operators() {
        let a = || Expression::Identifier(Identifier::new("a".to_string()));
        let b = || Expression::Identifier(Identifier::new("b".to_string()));
        for (input, op) in [
            ("a && b", BinaryOperator::LogicalAnd),
            ("a || b", BinaryOperator::LogicalOr),
        ] {
            assert_parses_to(
                verilog_expression,
                input,
                Expression::Binary(Box::new(a()), op, Box::new(b())),
            );
        }
        // Whitespace between a unary operator and its operand is still not an
        // expression at all, rather than a reduction of the operand.
        assert!(!matches!(verilog_expression("a & & b"), Ok(("", _))));
    }

    #[test]
    fn test_add_subtract_constants_identifiers() {
        let expressions = vec![
            (
                "1+2",
                Expression::Binary(
                    Box::new(Expression::Constant(VerilogConstant::from_int(1))),
                    BinaryOperator::Addition,
                    Box::new(Expression::Constant(VerilogConstant::from_int(2))),
                ),
            ),
            (
                "a-b",
                Expression::Binary(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    BinaryOperator::Subtraction,
                    Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                ),
            ),
            (
                "(a+b)",
                Expression::Parenthetical(Box::new(Expression::Binary(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    BinaryOperator::Addition,
                    Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                ))),
            ),
            (
                "{a,b}",
                Expression::Concatenation(vec![
                    Expression::Identifier(Identifier::new("a".to_string())),
                    Expression::Identifier(Identifier::new("b".to_string())),
                ]),
            ),
            (
                "a?b:c",
                Expression::Conditional(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                    Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                ),
            ),
        ];

        for (expr, expected) in expressions {
            assert_parses_to(verilog_expression, expr, expected);
        }
    }

    #[test]
    fn test_logical_and_logical_or() {
        let expressions = vec![
            (
                "1 && 2",
                Expression::Binary(
                    Box::new(Expression::Constant(VerilogConstant::from_int(1))),
                    BinaryOperator::LogicalAnd,
                    Box::new(Expression::Constant(VerilogConstant::from_int(2))),
                ),
            ),
            (
                "1 || 2",
                Expression::Binary(
                    Box::new(Expression::Constant(VerilogConstant::from_int(1))),
                    BinaryOperator::LogicalOr,
                    Box::new(Expression::Constant(VerilogConstant::from_int(2))),
                ),
            ),
        ];

        for (expr, expected) in expressions {
            assert_parses_to(verilog_expression, expr, expected);
        }
    }

    #[test]
    fn test_add_subtract_constants_identifiers_whitespace_and_comments() {
        let expressions = vec![
            (
                "1+2",
                Expression::Binary(
                    Box::new(Expression::Constant(VerilogConstant::from_int(1))),
                    BinaryOperator::Addition,
                    Box::new(Expression::Constant(VerilogConstant::from_int(2))),
                ),
            ),
            (
                "a-b",
                Expression::Binary(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    BinaryOperator::Subtraction,
                    Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                ),
            ),
        ];

        // Every token in these expressions is one character wide, so every
        // insertion point is a token boundary and any filler is legal there.
        // A `//` comment carries its own newline: without one it would swallow
        // the rest of the expression rather than separate two tokens.
        let fillers = [
            " ",
            "\t",
            "\n",
            "/* c */",
            "/*\n c\n */",
            "// c\n",
            "(* a *)",
            "(* a = 3 * 4 *)",
        ];

        for (expr, expected) in expressions {
            let mut rng = rand::rngs::StdRng::seed_from_u64(42);
            for _ in 0..10 {
                // Each piece is inserted whole, so a later insertion can never
                // land in the middle of an earlier comment and unbalance it.
                let mut pieces: Vec<&str> = expr.split("").filter(|p| !p.is_empty()).collect();
                for _ in 0..pieces.len() {
                    let pos = rng.next_u32() as usize % pieces.len();
                    let filler = fillers[rng.next_u32() as usize % fillers.len()];
                    pieces.insert(pos, filler);
                }
                let expr_with_ws: String = pieces.concat();
                let result_with_ws = verilog_expression(&expr_with_ws);
                assert!(
                    result_with_ws.is_ok(),
                    "Failed to parse expression with whitespace: {}",
                    expr_with_ws
                );
                let (_, parsed_expr_with_ws) = result_with_ws.unwrap();
                assert_eq!(
                    parsed_expr_with_ws, expected,
                    "Parsed expression with whitespace did not match expected: {}",
                    expr_with_ws
                );
            }
        }
    }

    /// The same injection, over a unary term followed by a binary operator —
    /// the shape that regressed in issue #126, and the one the corpus above
    /// cannot express.
    ///
    /// A unary operator and its operand are **one** token here rather than
    /// two: whitespace inside `-a` is not legal Verilog for the reduction
    /// operators (`a & &b`), so the boundaries a filler may land on are the
    /// ones *between* terms.
    #[test]
    fn test_unary_then_binary_with_injected_whitespace_and_comments() {
        let unary = |op: UnaryOperator, name: &str| {
            Expression::Unary(
                op,
                Box::new(Expression::Identifier(Identifier::new(name.to_string()))),
            )
        };
        let b = || Expression::Identifier(Identifier::new("b".to_string()));
        let expressions = vec![
            (
                vec!["-a", "+", "b"],
                Expression::Binary(
                    Box::new(unary(UnaryOperator::Negative, "a")),
                    BinaryOperator::Addition,
                    Box::new(b()),
                ),
            ),
            (
                vec!["~a", "&", "b"],
                Expression::Binary(
                    Box::new(unary(UnaryOperator::BitwiseNegation, "a")),
                    BinaryOperator::BitwiseAnd,
                    Box::new(b()),
                ),
            ),
        ];

        let fillers = [
            " ",
            "\t",
            "\n",
            "/* c */",
            "/*\n c\n */",
            "// c\n",
            "(* a *)",
            "(* a = 3 * 4 *)",
        ];

        for (tokens, expected) in expressions {
            let mut rng = rand::rngs::StdRng::seed_from_u64(42);
            for _ in 0..10 {
                let mut pieces = tokens.clone();
                for _ in 0..pieces.len() {
                    let pos = rng.next_u32() as usize % pieces.len();
                    let filler = fillers[rng.next_u32() as usize % fillers.len()];
                    pieces.insert(pos, filler);
                }
                let expr_with_ws: String = pieces.concat();
                assert_parses_to(verilog_expression, &expr_with_ws, expected.clone());
            }
        }
    }

    /// An attribute is metadata with no simulation semantics, so it is skipped
    /// like a comment and may sit between the operands of an expression.
    #[test]
    fn test_attributes_between_operands_and_operators() {
        let expected = Expression::Binary(
            Box::new(Expression::Identifier(Identifier::new("inp_a".to_string()))),
            BinaryOperator::Addition,
            Box::new(Expression::Identifier(Identifier::new("inp_b".to_string()))),
        );
        for input in [
            "inp_a + (* ripple_adder *) inp_b",
            "inp_a (* ripple_adder *) + inp_b",
            "(* ripple_adder *) inp_a + inp_b",
            "inp_a + inp_b (* ripple_adder *)",
            "inp_a +(* a = \"x\", b = 3 * 4 *)inp_b",
        ] {
            assert_parses_to(verilog_expression, input, expected.clone());
        }
    }

    /// The regression risk of skipping `(*`: an ordinary parenthesised operand
    /// and the multiplication operator must be untouched. `*` is not a unary
    /// operator in Verilog, so `(*` can only ever start an attribute.
    #[test]
    fn test_parenthesised_expressions_and_multiplication_still_parse() {
        let a = || Expression::Identifier(Identifier::new("a".to_string()));
        let b = || Expression::Identifier(Identifier::new("b".to_string()));
        let paren = |e: Expression| Expression::Parenthetical(Box::new(e));
        let product = |l: Expression, r: Expression| {
            Expression::Binary(Box::new(l), BinaryOperator::Multiplication, Box::new(r))
        };
        for input in ["(a) * (b)", "(a)*(b)", "( a ) * ( b )"] {
            assert_parses_to(verilog_expression, input, product(paren(a()), paren(b())));
        }
        assert_parses_to(verilog_expression, "a*(b)", product(a(), paren(b())));
        assert_parses_to(verilog_expression, "(a)*b", product(paren(a()), b()));
        assert_parses_to(
            verilog_expression,
            "(a) ** (b)",
            Expression::Binary(
                Box::new(paren(a())),
                BinaryOperator::Power,
                Box::new(paren(b())),
            ),
        );
        assert_parses(verilog_expression, "(2**3)*(4%5)");
        // An unterminated attribute is not a licence to eat the rest of the
        // input: it is simply not an expression.
        assert!(verilog_expression("(* a").is_err());
    }

    /// A comment separates two tokens exactly like whitespace does, so one can
    /// sit between any operand and any operator.
    #[test]
    fn test_comments_between_operands_and_operators() {
        let expected = Expression::Binary(
            Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
            BinaryOperator::Addition,
            Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
        );
        for input in [
            "a /* c */ + b",
            "a +/* c */b",
            "/* c */ a + b",
            "a + b /* c */",
            "a // c\n + b",
            "a + // c\n b",
            "a /*\n * spanning lines\n */ + b",
        ] {
            assert_parses_to(verilog_expression, input, expected.clone());
        }
    }

    #[test]
    fn test_parse_expression_ws_removal() {
        let expressions = vec![
            "a",
            "a + b",
            "a - b",
            "a * b",
            "a ** b",
            "a / b",
            "a && b",
            "a <<< b",
            "a >>> b",
            "a == b",
            "a != b",
            "a === b",
            "a !== b",
            "a << b",
            "a >> b",
            "a < b",
            "a <= b",
            "a > b",
            "a >= b",
            "a || b",
            "a ** b",
            "a ~^ b",
            "a ^ b",
            "a ^~ b",
            "a % b",
            "!a",
            "a ? b : c",
            "(a + b)",
            "{a, b, c}",
            "identifier",
            "a + (b * c)",
            "a ? (b + c) : (d - e)",
            "{a, {b, c}, d}",
        ];

        for expr in expressions {
            let result = verilog_expression(expr);
            println!("{:?}", result);

            assert!(result.is_ok(), "Failed to parse expression: {}", expr);

            // Remove whitespace and test that parse still works
            let expr_no_ws = expr.replace(" ", "");
            let result = verilog_expression(&expr_no_ws);
            assert!(result.is_ok(), "Failed to parse expression: {}", expr);
        }
    }

    #[test]
    fn test_function_call() {
        let expressions = vec![
            (
                "blahblah(a, b, c)",
                Expression::FunctionCall(
                    Identifier::new("blahblah".to_string()),
                    vec![
                        Expression::Identifier(Identifier::new("a".to_string())),
                        Expression::Identifier(Identifier::new("b".to_string())),
                        Expression::Identifier(Identifier::new("c".to_string())),
                    ],
                ),
            ),
            (
                "foo(1, 2, 3)",
                Expression::FunctionCall(
                    Identifier::new("foo".to_string()),
                    vec![
                        Expression::Constant(VerilogConstant::from_int(1)),
                        Expression::Constant(VerilogConstant::from_int(2)),
                        Expression::Constant(VerilogConstant::from_int(3)),
                    ],
                ),
            ),
        ];

        for (expr, expected) in expressions {
            assert_parses_to(verilog_expression, expr, expected);
        }
    }

    #[test]
    fn test_parenthetical_statements() {
        let expressions = vec![
            (
                "(a + b)",
                Expression::Parenthetical(Box::new(Expression::Binary(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    BinaryOperator::Addition,
                    Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                ))),
            ),
            (
                "(1 + 2)",
                Expression::Parenthetical(Box::new(Expression::Binary(
                    Box::new(Expression::Constant(VerilogConstant::from_int(1))),
                    BinaryOperator::Addition,
                    Box::new(Expression::Constant(VerilogConstant::from_int(2))),
                ))),
            ),
        ];

        for (expr, expected) in expressions {
            assert_parses_to(verilog_expression, expr, expected);
        }
    }

    #[test]
    fn test_nested_ternary_with_parentheses() {
        let expressions = vec![
            (
                "a ? (b ? c : d) : e",
                Expression::Conditional(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    Box::new(Expression::Parenthetical(Box::new(
                        Expression::Conditional(
                            Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                            Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                            Box::new(Expression::Identifier(Identifier::new("d".to_string()))),
                        ),
                    ))),
                    Box::new(Expression::Identifier(Identifier::new("e".to_string()))),
                ),
            ),
            (
                "a ? b : (c ? d : e)",
                Expression::Conditional(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                    Box::new(Expression::Parenthetical(Box::new(
                        Expression::Conditional(
                            Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                            Box::new(Expression::Identifier(Identifier::new("d".to_string()))),
                            Box::new(Expression::Identifier(Identifier::new("e".to_string()))),
                        ),
                    ))),
                ),
            ),
        ];

        for (expr, expected) in expressions {
            assert_parses_to(verilog_expression, expr, expected);
        }
    }

    #[test]
    fn test_concatenation_operator() {
        let expressions = vec![
            (
                "{a, b, c}",
                Expression::Concatenation(vec![
                    Expression::Identifier(Identifier::new("a".to_string())),
                    Expression::Identifier(Identifier::new("b".to_string())),
                    Expression::Identifier(Identifier::new("c".to_string())),
                ]),
            ),
            (
                "{1, 2, 3}",
                Expression::Concatenation(vec![
                    Expression::Constant(VerilogConstant::from_int(1)),
                    Expression::Constant(VerilogConstant::from_int(2)),
                    Expression::Constant(VerilogConstant::from_int(3)),
                ]),
            ),
        ];

        for (expr, expected) in expressions {
            assert_parses_to(verilog_expression, expr, expected);
        }
    }

    #[test]
    fn test_additional_cases() {
        let expressions = vec![
            (
                "a + b * c",
                Expression::Binary(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    BinaryOperator::Addition,
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                        BinaryOperator::Multiplication,
                        Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                    )),
                ),
            ),
            (
                "a * b + c",
                Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                        BinaryOperator::Multiplication,
                        Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                    )),
                    BinaryOperator::Addition,
                    Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                ),
            ),
            (
                "a && b || c",
                Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                        BinaryOperator::LogicalAnd,
                        Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                    )),
                    BinaryOperator::LogicalOr,
                    Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                ),
            ),
            (
                "a || b && c",
                Expression::Binary(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    BinaryOperator::LogicalOr,
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                        BinaryOperator::LogicalAnd,
                        Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                    )),
                ),
            ),
            (
                "a == b != c",
                Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                        BinaryOperator::LogicalEquality,
                        Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                    )),
                    BinaryOperator::LogicalInequality,
                    Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                ),
            ),
            (
                "a + b * c - d / e",
                Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                        BinaryOperator::Addition,
                        Box::new(Expression::Binary(
                            Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                            BinaryOperator::Multiplication,
                            Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                        )),
                    )),
                    BinaryOperator::Subtraction,
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("d".to_string()))),
                        BinaryOperator::Division,
                        Box::new(Expression::Identifier(Identifier::new("e".to_string()))),
                    )),
                ),
            ),
            (
                "a ? b : c ? d : e",
                Expression::Conditional(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                    Box::new(Expression::Conditional(
                        Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                        Box::new(Expression::Identifier(Identifier::new("d".to_string()))),
                        Box::new(Expression::Identifier(Identifier::new("e".to_string()))),
                    )),
                ),
            ),
            (
                "a ? b ? c : d : e",
                Expression::Conditional(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    Box::new(Expression::Conditional(
                        Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                        Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                        Box::new(Expression::Identifier(Identifier::new("d".to_string()))),
                    )),
                    Box::new(Expression::Identifier(Identifier::new("e".to_string()))),
                ),
            ),
            (
                "a + b * c ? d : e",
                Expression::Conditional(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                        BinaryOperator::Addition,
                        Box::new(Expression::Binary(
                            Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                            BinaryOperator::Multiplication,
                            Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                        )),
                    )),
                    Box::new(Expression::Identifier(Identifier::new("d".to_string()))),
                    Box::new(Expression::Identifier(Identifier::new("e".to_string()))),
                ),
            ),
            (
                "a + b * c ? d : e + f",
                Expression::Conditional(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                        BinaryOperator::Addition,
                        Box::new(Expression::Binary(
                            Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                            BinaryOperator::Multiplication,
                            Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                        )),
                    )),
                    Box::new(Expression::Identifier(Identifier::new("d".to_string()))),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("e".to_string()))),
                        BinaryOperator::Addition,
                        Box::new(Expression::Identifier(Identifier::new("f".to_string()))),
                    )),
                ),
            ),
            (
                "a[3]",
                Expression::BitSelect(
                    Identifier::new("a".to_string()),
                    Box::new(Expression::Constant(VerilogConstant::from_int(3))),
                ),
            ),
            (
                "a[3:0]",
                Expression::PartSelect(
                    Identifier::new("a".to_string()),
                    Box::new(Expression::Constant(VerilogConstant::from_int(3))),
                    Box::new(Expression::Constant(VerilogConstant::from_int(0))),
                ),
            ),
            (
                "a[b+c:2+4]",
                Expression::PartSelect(
                    Identifier::new("a".to_string()),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                        BinaryOperator::Addition,
                        Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                    )),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Constant(VerilogConstant::from_int(2))),
                        BinaryOperator::Addition,
                        Box::new(Expression::Constant(VerilogConstant::from_int(4))),
                    )),
                ),
            ),
            (
                "a + b * c ? d + e : f",
                Expression::Conditional(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                        BinaryOperator::Addition,
                        Box::new(Expression::Binary(
                            Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                            BinaryOperator::Multiplication,
                            Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                        )),
                    )),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("d".to_string()))),
                        BinaryOperator::Addition,
                        Box::new(Expression::Identifier(Identifier::new("e".to_string()))),
                    )),
                    Box::new(Expression::Identifier(Identifier::new("f".to_string()))),
                ),
            ),
            (
                "~a[2:3]",
                Expression::Unary(
                    UnaryOperator::BitwiseNegation,
                    Box::new(Expression::PartSelect(
                        Identifier::new("a".to_string()),
                        Box::new(Expression::Constant(VerilogConstant::from_int(2))),
                        Box::new(Expression::Constant(VerilogConstant::from_int(3))),
                    )),
                ),
            ),
            (
                "a[3:4] && b[4:5]",
                Expression::Binary(
                    Box::new(Expression::PartSelect(
                        Identifier::new("a".to_string()),
                        Box::new(Expression::Constant(VerilogConstant::from_int(3))),
                        Box::new(Expression::Constant(VerilogConstant::from_int(4))),
                    )),
                    BinaryOperator::LogicalAnd,
                    Box::new(Expression::PartSelect(
                        Identifier::new("b".to_string()),
                        Box::new(Expression::Constant(VerilogConstant::from_int(4))),
                        Box::new(Expression::Constant(VerilogConstant::from_int(5))),
                    )),
                ),
            ),
        ];

        for (expr, expected) in expressions {
            assert_parses_to(verilog_expression, expr, expected);
        }
    }

    #[test]
    fn test_more_cases() {
        let expressions = vec![
            (
                "a + b * c - d / e",
                Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                        BinaryOperator::Addition,
                        Box::new(Expression::Binary(
                            Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                            BinaryOperator::Multiplication,
                            Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                        )),
                    )),
                    BinaryOperator::Subtraction,
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("d".to_string()))),
                        BinaryOperator::Division,
                        Box::new(Expression::Identifier(Identifier::new("e".to_string()))),
                    )),
                ),
            ),
            (
                "a << b >> c",
                Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                        BinaryOperator::ShiftLeft,
                        Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                    )),
                    BinaryOperator::ShiftRight,
                    Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                ),
            ),
            (
                "a < b > c",
                Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                        BinaryOperator::LessThan,
                        Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                    )),
                    BinaryOperator::GreaterThan,
                    Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                ),
            ),
            (
                "a <= b >= c",
                Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                        BinaryOperator::LessThanOrEqual,
                        Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                    )),
                    BinaryOperator::GreaterThanOrEqual,
                    Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                ),
            ),
            (
                "a & b | c ^ d",
                Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                        BinaryOperator::BitwiseAnd,
                        Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                    )),
                    BinaryOperator::BitwiseOr,
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                        BinaryOperator::BitwiseXOr,
                        Box::new(Expression::Identifier(Identifier::new("d".to_string()))),
                    )),
                ),
            ),
        ];

        for (expr, expected) in expressions {
            let result = verilog_expression(expr);
            assert!(result.is_ok(), "Failed to parse expression: {}", expr);
            let (_, parsed_expr) = result.unwrap();
            assert_eq!(
                parsed_expr, expected,
                "Parsed expression did not match expected: {}",
                expr
            );
        }
    }

    #[test]
    fn test_edge_cases() {
        let expressions = vec![
            (
                "a ? b : c ? d : e",
                Expression::Conditional(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                    Box::new(Expression::Conditional(
                        Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                        Box::new(Expression::Identifier(Identifier::new("d".to_string()))),
                        Box::new(Expression::Identifier(Identifier::new("e".to_string()))),
                    )),
                ),
            ),
            (
                "a ? b ? c : d : e",
                Expression::Conditional(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    Box::new(Expression::Conditional(
                        Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                        Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                        Box::new(Expression::Identifier(Identifier::new("d".to_string()))),
                    )),
                    Box::new(Expression::Identifier(Identifier::new("e".to_string()))),
                ),
            ),
            (
                "a ? b : c ? d : e ? f : g",
                Expression::Conditional(
                    Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                    Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
                    Box::new(Expression::Conditional(
                        Box::new(Expression::Identifier(Identifier::new("c".to_string()))),
                        Box::new(Expression::Identifier(Identifier::new("d".to_string()))),
                        Box::new(Expression::Conditional(
                            Box::new(Expression::Identifier(Identifier::new("e".to_string()))),
                            Box::new(Expression::Identifier(Identifier::new("f".to_string()))),
                            Box::new(Expression::Identifier(Identifier::new("g".to_string()))),
                        )),
                    )),
                ),
            ),
        ];

        for (expr, expected) in expressions {
            let result = verilog_expression(expr);
            assert!(result.is_ok(), "Failed to parse expression: {}", expr);
            let (_, parsed_expr) = result.unwrap();
            assert_eq!(
                parsed_expr, expected,
                "Parsed expression did not match expected: {}",
                expr
            );
        }
    }

    fn system_call(name: &str, args: Vec<Expression>) -> Expression {
        Expression::SystemFunctionCall(name.to_string(), args)
    }

    fn ident(name: &str) -> Expression {
        Expression::Identifier(Identifier::new(name.to_string()))
    }

    #[test]
    fn test_a_bare_system_function_is_an_operand() {
        assert_parses_to(verilog_expression, "$time", system_call("time", vec![]));
        assert_parses_to(verilog_expression, "$random", system_call("random", vec![]));
    }

    #[test]
    fn test_a_system_function_takes_arguments() {
        assert_parses_to(
            verilog_expression,
            "$signed(b)",
            system_call("signed", vec![ident("b")]),
        );
        assert_parses_to(
            verilog_expression,
            "$clog2(a + 1)",
            system_call(
                "clog2",
                vec![Expression::Binary(
                    Box::new(ident("a")),
                    BinaryOperator::Addition,
                    Box::new(Expression::Constant(VerilogConstant::from_int(1))),
                )],
            ),
        );
        // An empty list and no list at all are the same thing.
        assert_parses_to(
            verilog_expression,
            "$random()",
            system_call("random", vec![]),
        );
    }

    #[test]
    fn test_a_system_function_composes_like_any_other_operand() {
        assert_parses_to(
            verilog_expression,
            "$time > 5",
            Expression::Binary(
                Box::new(system_call("time", vec![])),
                BinaryOperator::GreaterThan,
                Box::new(Expression::Constant(VerilogConstant::from_int(5))),
            ),
        );
        assert_parses_to(
            verilog_expression,
            "$signed(a) + $signed(b)",
            Expression::Binary(
                Box::new(system_call("signed", vec![ident("a")])),
                BinaryOperator::Addition,
                Box::new(system_call("signed", vec![ident("b")])),
            ),
        );
    }

    #[test]
    fn test_a_system_function_is_not_a_user_function_call() {
        // `$signed(b)` and `signed_thing(b)` take different resolution paths,
        // so they must not share an AST node.
        let system = assert_parses(verilog_expression, "$foo(b)");
        assert_eq!(system, system_call("foo", vec![ident("b")]));
        let user = assert_parses(verilog_expression, "foo(b)");
        assert_eq!(
            user,
            Expression::FunctionCall(Identifier::new("foo".to_string()), vec![ident("b")])
        );
    }

    #[test]
    fn test_a_system_function_prints_its_dollar_back() {
        assert_eq!(
            assert_parses(verilog_expression, "$signed(a) + $time").to_contracted_string(),
            "$signed(a) + $time"
        );
    }
}

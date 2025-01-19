use super::{base::RawToken, constants::{verilog_const, VerilogConstant}, identifier::{self, identifier, Identifier}, operators::{binary_operator, unary_operator, unary_operator_from_string, BinaryOperator, UnaryOperator}, simple::ws};
use nom::{branch::alt, bytes::complete::tag, combinator::{map, map_res}, error::ErrorKind, multi::{fold_many0, many0, many1, separated_list1}, sequence::{pair, preceded, tuple}, IResult};
use nom::Err;
use rand::seq::SliceRandom;
use rand::thread_rng;


#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Constant(VerilogConstant),
    Identifier(Identifier),
    Unary(UnaryOperator, Box<Expression>),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>), // condition ? true_expr : false_expr
    Parenthetical(Box<Expression>),
    Concatenation(Vec<Expression>),
    FunctionCall(Identifier, Vec<Expression>),
}


fn operand(input: &str) -> IResult<&str, Expression> {
    ws(alt((
        map(identifier, Expression::Identifier),
        map(verilog_const, |vc| Expression::Constant(vc),
    ))))(input)
}

// Alright, this is following table 5-4 in the IEEE 1364-2005 standard
// Use use the fold_many0() combinator to parse the expression 
// by composing layers of order of operation here. There are 14 layers as a result
// They are enumerated below with a brief description of the operation(s)

// Layer 1: Unary operators
fn unary_operator_layer(input: &str) -> IResult<&str, Expression> {
    alt((
        map_res(tuple((
            many1(unary_operator),
            verilog_expression,
        )), |(ops, exp)| {
            let mut result = exp;

            // These apply right to left somewhat confusingly
            for op in ops.iter().rev() {
                result = Expression::Unary(op.clone(), Box::new(result));
            }
            Ok::<_, nom::Err<(&str, nom::error::ErrorKind)>>(result)
        }),
        operand,
    ))(input)

}

// Layer 2: Exponentiation Operator
// Layer 3: Multiplication, Division, Modulus Operators
// Layer 4: Addition, Subtraction Operators
fn add_sub_layer(input: &str) -> IResult<&str, Expression> {
    let (input, init) = unary_operator_layer(input)?;

    fold_many0(
        pair(alt((tag("+"), tag("-"))), operand),
        move || {init.clone()},
        |acc, (op, val): (&str, Expression)| {
            let token = match op {
                "+" => BinaryOperator::Addition,
                "-" => BinaryOperator::Subtraction,
                _ => unreachable!(),
            };
            Expression::Binary( Box::new(acc), token, Box::new(val))
        },
    )(input)
}
// Layer 5: Shift Operators
// Layer 6: Relational Operators
// Layer 7: Equality Operators
// Layer 8: Bitwise AND Operator
// Layer 9: Bitwise XOR/XNOR Operators
// Layer 10: Bitwise OR Operator
// Layer 11: Logical AND Operator
// Layer 12: Logical OR Operator
// Layer 13: Conditional Operator
// Layer 14: Concatenation Operators



fn verilog_expression(input: &str) -> IResult<&str, Expression> {
    add_sub_layer(input)
}





#[cfg(test)]
mod tests {
    use rand::{RngCore, SeedableRng};

    use super::*;
    #[test]
    fn test_add_subtract_constants_identifiers() {
        let expressions = vec![
            ("1+2", Expression::Binary(
                Box::new(Expression::Constant(VerilogConstant::from_int(1))),
                BinaryOperator::Addition,
                Box::new(Expression::Constant(VerilogConstant::from_int(2))),
            )),
            ("a-b", Expression::Binary(
                Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                BinaryOperator::Subtraction,
                Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
            )),
        ];

        for (expr, expected) in expressions {
            let result = verilog_expression(expr);
            assert!(result.is_ok(), "Failed to parse expression: {}", expr);
            let (_, parsed_expr) = result.unwrap();
            assert_eq!(parsed_expr, expected, "Parsed expression did not match expected: {}", expr);
        }
    }

    #[test]
    fn test_add_subtract_constants_identifiers_whitespace() {
        let expressions = vec![
            ("1+2", Expression::Binary(
                Box::new(Expression::Constant(VerilogConstant::from_int(1))),
                BinaryOperator::Addition,
                Box::new(Expression::Constant(VerilogConstant::from_int(2))),
            )),
            ("a-b", Expression::Binary(
                Box::new(Expression::Identifier(Identifier::new("a".to_string()))),
                BinaryOperator::Subtraction,
                Box::new(Expression::Identifier(Identifier::new("b".to_string()))),
            )),
        ];

        for (expr, expected) in expressions {
            let mut rng = rand::rngs::StdRng::seed_from_u64(42);
            for _ in 0..10 {
                let mut chars: Vec<char> = expr.chars().collect();
                for _ in 0..(chars.len() / 2) {
                    
                    let pos = rng.next_u32() as usize % chars.len();
                    chars.insert(pos, ' ');
                }
                let expr_with_ws: String = chars.into_iter().collect();
                let result_with_ws = verilog_expression(&expr_with_ws);
                assert!(result_with_ws.is_ok(), "Failed to parse expression with whitespace: {}", expr_with_ws);
                let (_, parsed_expr_with_ws) = result_with_ws.unwrap();
                assert_eq!(parsed_expr_with_ws, expected, "Parsed expression with whitespace did not match expected: {}", expr_with_ws);
            }

        }
    }



    #[test]
    fn test_parse_expression() {
        let expressions = vec![
            "a",
            "a + b",
            "a - b",
            "a * b",
            "a / b",
            "a && b",
            "a || b",
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
            assert!(result.is_ok(), "Failed to parse expression: {}", expr);

            // Remove whitespace and test that parse still works
            let expr_no_ws = expr.replace(" ", "");
            let result = verilog_expression(&expr_no_ws);
            assert!(result.is_ok(), "Failed to parse expression: {}", expr);
        }        
    }
}

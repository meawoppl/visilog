trait Precedence {
    fn precedence(&self) -> u8;
}

// NOTE
//  7  -->   ! ~                      highest precedence (unary operators)
//  6  -->   * / %
//  5  -->   + -                     (binary add/subtract)
//  4  -->   << >> <<< >>>
//  3  -->   < <= > >=
//  2  -->   == != === !==
//  1  -->   & ^~ | && ||
//  0  -->   ?: (ternary operator)    lowest precedence

#[derive(Clone, PartialEq, Debug)]
pub enum UnaryOperator {
    // Arithmetic Operators
    Positive,
    Negative,
    LogicalNegation,
    BitwiseNegation,

    // Reduction Operators
    ReductionAnd,
    ReductionNand,
    ReductionNor,
    ReductionOr,
    ReductionXor,
    ReductionXnor,
}

pub fn unary_operator_from_string(input: &str) -> Option<UnaryOperator> {
    match input {
        "+" => Some(UnaryOperator::Positive),
        "-" => Some(UnaryOperator::Negative),
        "~" => Some(UnaryOperator::BitwiseNegation),
        "!" => Some(UnaryOperator::LogicalNegation),
        //
        "&" => Some(UnaryOperator::ReductionAnd),
        "~&" => Some(UnaryOperator::ReductionNand),
        "|" => Some(UnaryOperator::ReductionOr),
        "~|" => Some(UnaryOperator::ReductionNor),
        "^" => Some(UnaryOperator::ReductionXor),
        "~^" | "^~" => Some(UnaryOperator::ReductionXnor),
        _ => None,
    }
}

pub fn unary_operator(input: &str) -> IResult<&str, UnaryOperator> {
    // NOTE(meawoppl) - the alt() call only supports 21 arguments, so we need to split it up
    alt((
        map(tag("+"), |_| UnaryOperator::Positive),
        map(tag("-"), |_| UnaryOperator::Negative),
        map(tag("~"), |_| UnaryOperator::BitwiseNegation),
        map(tag("!"), |_| UnaryOperator::LogicalNegation),
        //
        map(tag("&"), |_| UnaryOperator::ReductionAnd),
        map(tag("~&"), |_| UnaryOperator::ReductionNand),
        map(tag("|"), |_| UnaryOperator::ReductionOr),
        map(tag("~|"), |_| UnaryOperator::ReductionNor),
        map(tag("^"), |_| UnaryOperator::ReductionXor),
        map(tag("~^"), |_| UnaryOperator::ReductionXnor),
        map(tag("^~"), |_| UnaryOperator::ReductionXnor),
    ))(input)
}

impl Precedence for UnaryOperator {
    fn precedence(&self) -> u8 {
        return 7;
    }
}

impl RawToken for UnaryOperator {
    fn raw_token(&self) -> String {
        match self {
            UnaryOperator::Positive => "+".to_string(),
            UnaryOperator::Negative => "-".to_string(),
            UnaryOperator::BitwiseNegation => "~".to_string(),
            UnaryOperator::LogicalNegation => "!".to_string(),
            UnaryOperator::ReductionAnd => "&".to_string(),
            UnaryOperator::ReductionNand => "~&".to_string(),
            UnaryOperator::ReductionOr => "|".to_string(),
            UnaryOperator::ReductionNor => "~|".to_string(),
            UnaryOperator::ReductionXor => "^".to_string(),
            UnaryOperator::ReductionXnor => "^~".to_string(),
        }
    }
}

pub const ALL_UNARY_OPERATORS: &[&str] =
    &["+", "-", "!", "~", "&", "~&", "|", "~|", "^", "~^", "^~"];

#[derive(Clone, PartialEq, Debug)]
pub enum BinaryOperator {
    Addition,
    ArithmeticShiftLeft,
    ArithmeticShiftRight,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXNor,
    BitwiseXOr,
    BitwiseInclusiveOr,
    CaseEquality,
    CaseInequality,
    Division,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    LogicalAnd,
    LogicalEquality,
    LogicalInequality,
    LogicalOr,
    Modulus,
    Multiplication,
    Power,
    ShiftLeft,
    ShiftRight,
    Subtraction,
}

impl Precedence for BinaryOperator {
    fn precedence(&self) -> u8 {
        // NOTE(meawoppl) - Empty comment lines are to keep the precedence values aligned
        match self {
            BinaryOperator::Division => 6,
            BinaryOperator::Modulus => 6,
            BinaryOperator::Power => 6,
            BinaryOperator::Multiplication => 6,
            //
            BinaryOperator::Addition => 5,
            BinaryOperator::Subtraction => 5,
            //
            BinaryOperator::ArithmeticShiftLeft => 4,
            BinaryOperator::ArithmeticShiftRight => 4,
            BinaryOperator::ShiftLeft => 4,
            BinaryOperator::ShiftRight => 4,
            //
            BinaryOperator::GreaterThan => 3,
            BinaryOperator::GreaterThanOrEqual => 3,
            BinaryOperator::LessThan => 3,
            BinaryOperator::LessThanOrEqual => 3,
            //
            BinaryOperator::CaseEquality => 2,
            BinaryOperator::CaseInequality => 2,
            BinaryOperator::LogicalEquality => 2,
            BinaryOperator::LogicalInequality => 2,
            //
            BinaryOperator::BitwiseAnd => 1,
            BinaryOperator::BitwiseXNor => 1,
            BinaryOperator::BitwiseXOr => 1,
            BinaryOperator::BitwiseOr => 1,
            BinaryOperator::BitwiseInclusiveOr => 1,
            BinaryOperator::LogicalAnd => 1,
            BinaryOperator::LogicalOr => 1,
        }
    }
}

impl RawToken for BinaryOperator {
    fn raw_token(&self) -> String {
        match self {
            BinaryOperator::Addition => "+".to_string(),
            BinaryOperator::Subtraction => "-".to_string(),
            BinaryOperator::Multiplication => "*".to_string(),
            BinaryOperator::Division => "/".to_string(),
            BinaryOperator::Modulus => "%".to_string(),
            BinaryOperator::Power => "**".to_string(),
            BinaryOperator::GreaterThan => ">".to_string(),
            BinaryOperator::GreaterThanOrEqual => ">=".to_string(),
            BinaryOperator::LessThan => "<".to_string(),
            BinaryOperator::LessThanOrEqual => "<=".to_string(),
            BinaryOperator::LogicalAnd => "&&".to_string(),
            BinaryOperator::LogicalOr => "||".to_string(),
            BinaryOperator::LogicalEquality => "==".to_string(),
            BinaryOperator::LogicalInequality => "!=".to_string(),
            BinaryOperator::CaseEquality => "===".to_string(),
            BinaryOperator::CaseInequality => "!==".to_string(),
            BinaryOperator::BitwiseAnd => "&".to_string(),
            BinaryOperator::BitwiseInclusiveOr => "|".to_string(),
            BinaryOperator::BitwiseOr => "|".to_string(),
            BinaryOperator::BitwiseXOr => "^".to_string(),
            BinaryOperator::BitwiseXNor => "^~".to_string(),
            BinaryOperator::ShiftLeft => "<<".to_string(),
            BinaryOperator::ShiftRight => ">>".to_string(),
            BinaryOperator::ArithmeticShiftLeft => "<<<".to_string(),
            BinaryOperator::ArithmeticShiftRight => ">>>".to_string(),
        }
    }
}
pub fn binary_expression_from_string(input: &str) -> Option<BinaryOperator> {
    match input {
        "+" => Some(BinaryOperator::Addition),
        "-" => Some(BinaryOperator::Subtraction),
        "*" => Some(BinaryOperator::Multiplication),
        "/" => Some(BinaryOperator::Division),
        "%" => Some(BinaryOperator::Modulus),
        ">" => Some(BinaryOperator::GreaterThan),
        ">=" => Some(BinaryOperator::GreaterThanOrEqual),
        "<" => Some(BinaryOperator::LessThan),
        "<=" => Some(BinaryOperator::LessThanOrEqual),
        "&&" => Some(BinaryOperator::LogicalAnd),
        "||" => Some(BinaryOperator::LogicalOr),
        "==" => Some(BinaryOperator::LogicalEquality),
        "!=" => Some(BinaryOperator::LogicalInequality),
        "===" => Some(BinaryOperator::CaseEquality),
        "!==" => Some(BinaryOperator::CaseInequality),
        "&" => Some(BinaryOperator::BitwiseAnd),
        "|" => Some(BinaryOperator::BitwiseInclusiveOr),
        "^" => Some(BinaryOperator::BitwiseXOr),
        "^~" | "~^" => Some(BinaryOperator::BitwiseXNor),
        "<<" => Some(BinaryOperator::ShiftLeft),
        ">>" => Some(BinaryOperator::ShiftRight),
        "<<<" => Some(BinaryOperator::ArithmeticShiftLeft),
        ">>>" => Some(BinaryOperator::ArithmeticShiftRight),
        _ => None,
    }
}

pub const ALL_BINARY_OPERATORS: &[&str] = &[
    "+", "-", "*", "/", "%", ">", ">=", "<", "<=", "&&", "||", "==", "!=", "===", "!==", "&", "|",
    "^", "^~", "~^", "&", "|", "^", "~^", "^~", "<<", ">>", "<<<", ">>>",
];

use nom::{branch::alt, bytes::complete::tag, combinator::map, IResult};

use super::base::RawToken;

pub fn binary_operator(input: &str) -> IResult<&str, BinaryOperator> {
    // NOTE(meawoppl) - the alt() call only supports 21 arguments, so we need to split it up
    // NOTE(meawoppl) - the order of the operators in alt makes sure it does not partially parse
    let a1 = alt((
        map(tag("+"), |_| BinaryOperator::Addition),
        map(tag("-"), |_| BinaryOperator::Subtraction),
        map(tag("*"), |_| BinaryOperator::Multiplication),
        map(tag("/"), |_| BinaryOperator::Division),
        map(tag("%"), |_| BinaryOperator::Modulus),
        map(tag("&&"), |_| BinaryOperator::LogicalAnd),
        map(tag("||"), |_| BinaryOperator::LogicalOr),
        map(tag("&"), |_| BinaryOperator::BitwiseAnd),
        map(tag("|"), |_| BinaryOperator::BitwiseInclusiveOr),
    ));

    let a2 = alt((
        map(tag("==="), |_| BinaryOperator::CaseEquality),
        map(tag("=="), |_| BinaryOperator::LogicalEquality),
        map(tag("!=="), |_| BinaryOperator::CaseInequality),
        map(tag("!="), |_| BinaryOperator::LogicalInequality),
        map(tag("^~"), |_| BinaryOperator::BitwiseXNor),
        map(tag("~^"), |_| BinaryOperator::BitwiseXNor),
        map(tag("^"), |_| BinaryOperator::BitwiseXOr),
        map(tag("<<<"), |_| BinaryOperator::ArithmeticShiftLeft),
        map(tag(">>>"), |_| BinaryOperator::ArithmeticShiftRight),
        map(tag("<<"), |_| BinaryOperator::ShiftLeft),
        map(tag(">>"), |_| BinaryOperator::ShiftRight),
        map(tag(">="), |_| BinaryOperator::GreaterThanOrEqual),
        map(tag(">"), |_| BinaryOperator::GreaterThan),
        map(tag("<="), |_| BinaryOperator::LessThanOrEqual),
        map(tag("<"), |_| BinaryOperator::LessThan),
    ));

    alt((a1, a2))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binary_expression_from_string() {
        assert_eq!(
            binary_expression_from_string("+"),
            Some(BinaryOperator::Addition)
        );
        assert_eq!(
            binary_expression_from_string("-"),
            Some(BinaryOperator::Subtraction)
        );
        assert_eq!(
            binary_expression_from_string("*"),
            Some(BinaryOperator::Multiplication)
        );
        assert_eq!(
            binary_expression_from_string("/"),
            Some(BinaryOperator::Division)
        );
        assert_eq!(
            binary_expression_from_string("%"),
            Some(BinaryOperator::Modulus)
        );
        assert_eq!(
            binary_expression_from_string("**"),
            Some(BinaryOperator::Power)
        );
        assert_eq!(
            binary_expression_from_string(">"),
            Some(BinaryOperator::GreaterThan)
        );
        assert_eq!(
            binary_expression_from_string(">="),
            Some(BinaryOperator::GreaterThanOrEqual)
        );
        assert_eq!(
            binary_expression_from_string("<"),
            Some(BinaryOperator::LessThan)
        );
        assert_eq!(
            binary_expression_from_string("<="),
            Some(BinaryOperator::LessThanOrEqual)
        );
        assert_eq!(
            binary_expression_from_string("&&"),
            Some(BinaryOperator::LogicalAnd)
        );
        assert_eq!(
            binary_expression_from_string("||"),
            Some(BinaryOperator::LogicalOr)
        );
        assert_eq!(
            binary_expression_from_string("=="),
            Some(BinaryOperator::LogicalEquality)
        );
        assert_eq!(
            binary_expression_from_string("!="),
            Some(BinaryOperator::LogicalInequality)
        );
        assert_eq!(
            binary_expression_from_string("==="),
            Some(BinaryOperator::CaseEquality)
        );
        assert_eq!(
            binary_expression_from_string("!=="),
            Some(BinaryOperator::CaseInequality)
        );
        assert_eq!(
            binary_expression_from_string("&"),
            Some(BinaryOperator::BitwiseAnd)
        );
        assert_eq!(
            binary_expression_from_string("|"),
            Some(BinaryOperator::BitwiseInclusiveOr)
        );
        assert_eq!(
            binary_expression_from_string("^"),
            Some(BinaryOperator::BitwiseXOr)
        );
        assert_eq!(
            binary_expression_from_string("^~"),
            Some(BinaryOperator::BitwiseXNor)
        );
        assert_eq!(
            binary_expression_from_string("~^"),
            Some(BinaryOperator::BitwiseXNor)
        );
        assert_eq!(
            binary_expression_from_string("<<"),
            Some(BinaryOperator::ShiftLeft)
        );
        assert_eq!(
            binary_expression_from_string(">>"),
            Some(BinaryOperator::ShiftRight)
        );
        assert_eq!(
            binary_expression_from_string("<<<"),
            Some(BinaryOperator::ArithmeticShiftLeft)
        );
        assert_eq!(
            binary_expression_from_string(">>>"),
            Some(BinaryOperator::ArithmeticShiftRight)
        );
        assert_eq!(binary_expression_from_string("nonexistent"), None);
    }

    #[test]
    fn test_reduction_operator_from_string() {
        assert_eq!(
            unary_operator_from_string("&"),
            Some(UnaryOperator::ReductionAnd)
        );
        assert_eq!(
            unary_operator_from_string("~&"),
            Some(UnaryOperator::ReductionNand)
        );
        assert_eq!(
            unary_operator_from_string("|"),
            Some(UnaryOperator::ReductionOr)
        );
        assert_eq!(
            unary_operator_from_string("~|"),
            Some(UnaryOperator::ReductionNor)
        );
        assert_eq!(
            unary_operator_from_string("^"),
            Some(UnaryOperator::ReductionXor)
        );
        assert_eq!(
            unary_operator_from_string("~^"),
            Some(UnaryOperator::ReductionXnor)
        );
        assert_eq!(
            unary_operator_from_string("^~"),
            Some(UnaryOperator::ReductionXnor)
        );
        assert_eq!(unary_operator_from_string("nonexistent"), None);
    }

    #[test]
    fn test_all_binary_expression_from_string() {
        for expr in ALL_BINARY_OPERATORS {
            assert!(
                binary_expression_from_string(expr).is_some(),
                "Binary expression {} failed to parse",
                expr
            );
        }
        assert_eq!(binary_expression_from_string("nonexistent"), None);
    }

    #[test]
    fn test_unary_operator_from_string() {
        assert_eq!(
            unary_operator_from_string("+"),
            Some(UnaryOperator::Positive)
        );
        assert_eq!(
            unary_operator_from_string("-"),
            Some(UnaryOperator::Negative)
        );
        assert_eq!(
            unary_operator_from_string("~"),
            Some(UnaryOperator::BitwiseNegation)
        );
        assert_eq!(
            unary_operator_from_string("!"),
            Some(UnaryOperator::LogicalNegation)
        );
        assert_eq!(unary_operator_from_string("nonexistent"), None);
    }

    #[test]
    fn test_all_unary_operator_from_string() {
        for op in ALL_UNARY_OPERATORS {
            assert!(
                unary_operator_from_string(op).is_some(),
                "Unary operator {} failed to parse",
                op
            );
        }
        assert_eq!(unary_operator_from_string("nonexistent"), None);
    }

    #[test]
    fn test_parse_binary_operator() {
        assert_eq!(binary_operator("+"), Ok(("", BinaryOperator::Addition)));
        assert_eq!(binary_operator("-"), Ok(("", BinaryOperator::Subtraction)));
        assert_eq!(
            binary_operator("*"),
            Ok(("", BinaryOperator::Multiplication))
        );
        assert_eq!(binary_operator("/"), Ok(("", BinaryOperator::Division)));
        assert_eq!(binary_operator("%"), Ok(("", BinaryOperator::Modulus)));
        assert_eq!(
            binary_operator(">="),
            Ok(("", BinaryOperator::GreaterThanOrEqual))
        );
        assert_eq!(binary_operator(">"), Ok(("", BinaryOperator::GreaterThan)));
        assert_eq!(
            binary_operator("<="),
            Ok(("", BinaryOperator::LessThanOrEqual))
        );
        assert_eq!(binary_operator("<"), Ok(("", BinaryOperator::LessThan)));
        assert_eq!(binary_operator("&&"), Ok(("", BinaryOperator::LogicalAnd)));
        assert_eq!(binary_operator("||"), Ok(("", BinaryOperator::LogicalOr)));
        assert_eq!(
            binary_operator("=="),
            Ok(("", BinaryOperator::LogicalEquality))
        );
        assert_eq!(
            binary_operator("!="),
            Ok(("", BinaryOperator::LogicalInequality))
        );
        assert_eq!(
            binary_operator("==="),
            Ok(("", BinaryOperator::CaseEquality))
        );
        assert_eq!(
            binary_operator("!=="),
            Ok(("", BinaryOperator::CaseInequality))
        );
        assert_eq!(binary_operator("&"), Ok(("", BinaryOperator::BitwiseAnd)));
        assert_eq!(
            binary_operator("|"),
            Ok(("", BinaryOperator::BitwiseInclusiveOr))
        );
        assert_eq!(binary_operator("^"), Ok(("", BinaryOperator::BitwiseXOr)));
        assert_eq!(binary_operator("^~"), Ok(("", BinaryOperator::BitwiseXNor)));
        assert_eq!(binary_operator("~^"), Ok(("", BinaryOperator::BitwiseXNor)));
        assert_eq!(binary_operator("<<"), Ok(("", BinaryOperator::ShiftLeft)));
        assert_eq!(binary_operator(">>"), Ok(("", BinaryOperator::ShiftRight)));
        assert_eq!(
            binary_operator("<<<"),
            Ok(("", BinaryOperator::ArithmeticShiftLeft))
        );
        assert_eq!(
            binary_operator(">>>"),
            Ok(("", BinaryOperator::ArithmeticShiftRight))
        );
        assert!(binary_operator("nonexistent").is_err());
    }
}

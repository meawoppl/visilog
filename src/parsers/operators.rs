#[derive(Clone, PartialEq, Debug)]
pub enum UnaryOperator {
    Positive,
    Negative,
    LogicalNegation,
    BitwiseNegation,
}

pub fn unary_operator_from_string(input: &str) -> Option<UnaryOperator> {
    match input {
        "+" => Some(UnaryOperator::Positive),
        "-" => Some(UnaryOperator::Negative),
        "~" => Some(UnaryOperator::BitwiseNegation),
        "!" => Some(UnaryOperator::LogicalNegation),
        _ => None,
    }
}

pub const ALL_UNARY_OPERATORS: &[&str] = &["+", "-", "!", "~"];

#[derive(Clone, PartialEq, Debug)]
pub enum BinaryOperator {
    Concatenation,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    LogicalNegation,
    LogicalAnd,
    LogicalOr,
    LogicalEquality,
    LogicalInequality,
    CaseEquality,
    CaseInequality,
    BitwiseNegation,
    BitwiseAnd,
    BitwiseInclusiveOr,
    BitwiseExclusiveOr,
    BitwiseEquivalence,
    ShiftLeft,
    ShiftRight,
    ArithmeticShiftLeft,
    ArithmeticShiftRight,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ReductionOperator {
    ReductionAnd,
    ReductionNand,
    ReductionOr,
    ReductionNor,
    ReductionXor,
    ReductionXnor,
}

pub fn reduction_operator_from_string(input: &str) -> Option<ReductionOperator> {
    match input {
        "&" => Some(ReductionOperator::ReductionAnd),
        "~&" => Some(ReductionOperator::ReductionNand),
        "|" => Some(ReductionOperator::ReductionOr),
        "~|" => Some(ReductionOperator::ReductionNor),
        "^" => Some(ReductionOperator::ReductionXor),
        "~^" | "^~" => Some(ReductionOperator::ReductionXnor),
        _ => None,
    }
}

pub fn binary_expression_from_string(input: &str) -> Option<BinaryOperator> {
    match input {
        "{}" => Some(BinaryOperator::Concatenation),
        "+" => Some(BinaryOperator::Addition),
        "-" => Some(BinaryOperator::Subtraction),
        "*" => Some(BinaryOperator::Multiplication),
        "/" => Some(BinaryOperator::Division),
        "%" => Some(BinaryOperator::Modulus),
        ">" => Some(BinaryOperator::GreaterThan),
        ">=" => Some(BinaryOperator::GreaterThanOrEqual),
        "<" => Some(BinaryOperator::LessThan),
        "<=" => Some(BinaryOperator::LessThanOrEqual),
        "!" => Some(BinaryOperator::LogicalNegation),
        "&&" => Some(BinaryOperator::LogicalAnd),
        "||" => Some(BinaryOperator::LogicalOr),
        "==" => Some(BinaryOperator::LogicalEquality),
        "!=" => Some(BinaryOperator::LogicalInequality),
        "===" => Some(BinaryOperator::CaseEquality),
        "!==" => Some(BinaryOperator::CaseInequality),
        "~" => Some(BinaryOperator::BitwiseNegation),
        "&" => Some(BinaryOperator::BitwiseAnd),
        "|" => Some(BinaryOperator::BitwiseInclusiveOr),
        "^" => Some(BinaryOperator::BitwiseExclusiveOr),
        "^~" | "~^" => Some(BinaryOperator::BitwiseEquivalence),
        "<<" => Some(BinaryOperator::ShiftLeft),
        ">>" => Some(BinaryOperator::ShiftRight),
        "<<<" => Some(BinaryOperator::ArithmeticShiftLeft),
        ">>>" => Some(BinaryOperator::ArithmeticShiftRight),
        _ => None,
    }
}

pub const ALL_BINARY_EXPRESSIONS: &[&str] = &[
    "{}", "+", "-", "*", "/", "%", ">", ">=", "<", "<=", "!", "&&", "||", "==", "!=", "===", "!==",
    "~", "&", "|", "^", "^~", "~^", "&", "|", "^", "~^", "^~", "<<", ">>", "<<<", ">>>",
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binary_expression_from_string() {
        assert_eq!(
            binary_expression_from_string("{}"),
            Some(BinaryOperator::Concatenation)
        );
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
            binary_expression_from_string("!"),
            Some(BinaryOperator::LogicalNegation)
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
            binary_expression_from_string("~"),
            Some(BinaryOperator::BitwiseNegation)
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
            Some(BinaryOperator::BitwiseExclusiveOr)
        );
        assert_eq!(
            binary_expression_from_string("^~"),
            Some(BinaryOperator::BitwiseEquivalence)
        );
        assert_eq!(
            binary_expression_from_string("~^"),
            Some(BinaryOperator::BitwiseEquivalence)
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
            reduction_operator_from_string("&"),
            Some(ReductionOperator::ReductionAnd)
        );
        assert_eq!(
            reduction_operator_from_string("~&"),
            Some(ReductionOperator::ReductionNand)
        );
        assert_eq!(
            reduction_operator_from_string("|"),
            Some(ReductionOperator::ReductionOr)
        );
        assert_eq!(
            reduction_operator_from_string("~|"),
            Some(ReductionOperator::ReductionNor)
        );
        assert_eq!(
            reduction_operator_from_string("^"),
            Some(ReductionOperator::ReductionXor)
        );
        assert_eq!(
            reduction_operator_from_string("~^"),
            Some(ReductionOperator::ReductionXnor)
        );
        assert_eq!(
            reduction_operator_from_string("^~"),
            Some(ReductionOperator::ReductionXnor)
        );
        assert_eq!(reduction_operator_from_string("nonexistent"), None);
    }

    #[test]
    fn test_all_binary_expression_from_string() {
        for expr in ALL_BINARY_EXPRESSIONS {
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
        assert_eq!(unary_operator_from_string("+"), Some(UnaryOperator::Positive));
        assert_eq!(unary_operator_from_string("-"), Some(UnaryOperator::Negative));
        assert_eq!(unary_operator_from_string("~"), Some(UnaryOperator::BitwiseNegation));
        assert_eq!(unary_operator_from_string("!"), Some(UnaryOperator::LogicalNegation));
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
}

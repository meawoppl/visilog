#[derive(Clone, PartialEq, Debug)]
pub enum VerilogBinaryExpression {
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

pub fn binary_expression_from_string(input: &str) -> Option<VerilogBinaryExpression> {
    match input {
        "{}" => Some(VerilogBinaryExpression::Concatenation),
        "+" => Some(VerilogBinaryExpression::Addition),
        "-" => Some(VerilogBinaryExpression::Subtraction),
        "*" => Some(VerilogBinaryExpression::Multiplication),
        "/" => Some(VerilogBinaryExpression::Division),
        "%" => Some(VerilogBinaryExpression::Modulus),
        ">" => Some(VerilogBinaryExpression::GreaterThan),
        ">=" => Some(VerilogBinaryExpression::GreaterThanOrEqual),
        "<" => Some(VerilogBinaryExpression::LessThan),
        "<=" => Some(VerilogBinaryExpression::LessThanOrEqual),
        "!" => Some(VerilogBinaryExpression::LogicalNegation),
        "&&" => Some(VerilogBinaryExpression::LogicalAnd),
        "||" => Some(VerilogBinaryExpression::LogicalOr),
        "==" => Some(VerilogBinaryExpression::LogicalEquality),
        "!=" => Some(VerilogBinaryExpression::LogicalInequality),
        "===" => Some(VerilogBinaryExpression::CaseEquality),
        "!==" => Some(VerilogBinaryExpression::CaseInequality),
        "~" => Some(VerilogBinaryExpression::BitwiseNegation),
        "&" => Some(VerilogBinaryExpression::BitwiseAnd),
        "|" => Some(VerilogBinaryExpression::BitwiseInclusiveOr),
        "^" => Some(VerilogBinaryExpression::BitwiseExclusiveOr),
        "^~" | "~^" => Some(VerilogBinaryExpression::BitwiseEquivalence),
        "<<" => Some(VerilogBinaryExpression::ShiftLeft),
        ">>" => Some(VerilogBinaryExpression::ShiftRight),
        "<<<" => Some(VerilogBinaryExpression::ArithmeticShiftLeft),
        ">>>" => Some(VerilogBinaryExpression::ArithmeticShiftRight),
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
            Some(VerilogBinaryExpression::Concatenation)
        );
        assert_eq!(
            binary_expression_from_string("+"),
            Some(VerilogBinaryExpression::Addition)
        );
        assert_eq!(
            binary_expression_from_string("-"),
            Some(VerilogBinaryExpression::Subtraction)
        );
        assert_eq!(
            binary_expression_from_string("*"),
            Some(VerilogBinaryExpression::Multiplication)
        );
        assert_eq!(
            binary_expression_from_string("/"),
            Some(VerilogBinaryExpression::Division)
        );
        assert_eq!(
            binary_expression_from_string("%"),
            Some(VerilogBinaryExpression::Modulus)
        );
        assert_eq!(
            binary_expression_from_string(">"),
            Some(VerilogBinaryExpression::GreaterThan)
        );
        assert_eq!(
            binary_expression_from_string(">="),
            Some(VerilogBinaryExpression::GreaterThanOrEqual)
        );
        assert_eq!(
            binary_expression_from_string("<"),
            Some(VerilogBinaryExpression::LessThan)
        );
        assert_eq!(
            binary_expression_from_string("<="),
            Some(VerilogBinaryExpression::LessThanOrEqual)
        );
        assert_eq!(
            binary_expression_from_string("!"),
            Some(VerilogBinaryExpression::LogicalNegation)
        );
        assert_eq!(
            binary_expression_from_string("&&"),
            Some(VerilogBinaryExpression::LogicalAnd)
        );
        assert_eq!(
            binary_expression_from_string("||"),
            Some(VerilogBinaryExpression::LogicalOr)
        );
        assert_eq!(
            binary_expression_from_string("=="),
            Some(VerilogBinaryExpression::LogicalEquality)
        );
        assert_eq!(
            binary_expression_from_string("!="),
            Some(VerilogBinaryExpression::LogicalInequality)
        );
        assert_eq!(
            binary_expression_from_string("==="),
            Some(VerilogBinaryExpression::CaseEquality)
        );
        assert_eq!(
            binary_expression_from_string("!=="),
            Some(VerilogBinaryExpression::CaseInequality)
        );
        assert_eq!(
            binary_expression_from_string("~"),
            Some(VerilogBinaryExpression::BitwiseNegation)
        );
        assert_eq!(
            binary_expression_from_string("&"),
            Some(VerilogBinaryExpression::BitwiseAnd)
        );
        assert_eq!(
            binary_expression_from_string("|"),
            Some(VerilogBinaryExpression::BitwiseInclusiveOr)
        );
        assert_eq!(
            binary_expression_from_string("^"),
            Some(VerilogBinaryExpression::BitwiseExclusiveOr)
        );
        assert_eq!(
            binary_expression_from_string("^~"),
            Some(VerilogBinaryExpression::BitwiseEquivalence)
        );
        assert_eq!(
            binary_expression_from_string("~^"),
            Some(VerilogBinaryExpression::BitwiseEquivalence)
        );
        assert_eq!(
            binary_expression_from_string("<<"),
            Some(VerilogBinaryExpression::ShiftLeft)
        );
        assert_eq!(
            binary_expression_from_string(">>"),
            Some(VerilogBinaryExpression::ShiftRight)
        );
        assert_eq!(
            binary_expression_from_string("<<<"),
            Some(VerilogBinaryExpression::ArithmeticShiftLeft)
        );
        assert_eq!(
            binary_expression_from_string(">>>"),
            Some(VerilogBinaryExpression::ArithmeticShiftRight)
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
}

#[derive(Clone, PartialEq)]
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
    ReductionAnd,
    ReductionNand,
    ReductionOr,
    ReductionNor,
    ReductionXor,
    ReductionXnor,
    ShiftLeft,
    ShiftRight,
    ArithmeticShiftLeft,
    ArithmeticShiftRight,
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
        "&" => Some(VerilogBinaryExpression::ReductionAnd),
        "~&" => Some(VerilogBinaryExpression::ReductionNand),
        "|" => Some(VerilogBinaryExpression::ReductionOr),
        "~|" => Some(VerilogBinaryExpression::ReductionNor),
        "^" => Some(VerilogBinaryExpression::ReductionXor),
        "~^" | "^~" => Some(VerilogBinaryExpression::ReductionXnor),
        "<<" => Some(VerilogBinaryExpression::ShiftLeft),
        ">>" => Some(VerilogBinaryExpression::ShiftRight),
        "<<<" => Some(VerilogBinaryExpression::ArithmeticShiftLeft),
        ">>>" => Some(VerilogBinaryExpression::ArithmeticShiftRight),
        _ => None,
    }
}

pub const ALL_BINARY_EXPRESSIONS: &[&str] = &[
    "{}",
    "+",
    "-",
    "*",
    "/",
    "%",
    ">",
    ">=",
    "<",
    "<=",
    "!",
    "&&",
    "||",
    "==",
    "!=",
    "===",
    "!==",
    "~",
    "&",
    "|",
    "^",
    "^~",
    "~^",
    "&",
    "~&",
    "|",
    "~|",
    "^",
    "~^",
    "^~",
    "<<",
    ">>",
    "<<<",
    ">>>",
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binary_expression_from_string() {
        assert_eq!(binary_expression_from_string("{}"), Some(VerilogBinaryExpression::Concatenation));
        assert_eq!(binary_expression_from_string("+"), Some(VerilogBinaryExpression::Addition));
        assert_eq!(binary_expression_from_string("-"), Some(VerilogBinaryExpression::Subtraction));
        assert_eq!(binary_expression_from_string("*"), Some(VerilogBinaryExpression::Multiplication));
        assert_eq!(binary_expression_from_string("/"), Some(VerilogBinaryExpression::Division));
        assert_eq!(binary_expression_from_string("%"), Some(VerilogBinaryExpression::Modulus));
        assert_eq!(binary_expression_from_string(">"), Some(VerilogBinaryExpression::GreaterThan));
        assert_eq!(binary_expression_from_string(">="), Some(VerilogBinaryExpression::GreaterThanOrEqual));
        assert_eq!(binary_expression_from_string("<"), Some(VerilogBinaryExpression::LessThan));
        assert_eq!(binary_expression_from_string("<="), Some(VerilogBinaryExpression::LessThanOrEqual));
        assert_eq!(binary_expression_from_string("!"), Some(VerilogBinaryExpression::LogicalNegation));
        assert_eq!(binary_expression_from_string("&&"), Some(VerilogBinaryExpression::LogicalAnd));
        assert_eq!(binary_expression_from_string("||"), Some(VerilogBinaryExpression::LogicalOr));
        assert_eq!(binary_expression_from_string("=="), Some(VerilogBinaryExpression::LogicalEquality));
        assert_eq!(binary_expression_from_string("!="), Some(VerilogBinaryExpression::LogicalInequality));
        assert_eq!(binary_expression_from_string("==="), Some(VerilogBinaryExpression::CaseEquality));
        assert_eq!(binary_expression_from_string("!=="), Some(VerilogBinaryExpression::CaseInequality));
        assert_eq!(binary_expression_from_string("~"), Some(VerilogBinaryExpression::BitwiseNegation));
        assert_eq!(binary_expression_from_string("&"), Some(VerilogBinaryExpression::BitwiseAnd));
        assert_eq!(binary_expression_from_string("|"), Some(VerilogBinaryExpression::BitwiseInclusiveOr));
        assert_eq!(binary_expression_from_string("^"), Some(VerilogBinaryExpression::BitwiseExclusiveOr));
        assert_eq!(binary_expression_from_string("^~"), Some(VerilogBinaryExpression::BitwiseEquivalence));
        assert_eq!(binary_expression_from_string("~^"), Some(VerilogBinaryExpression::BitwiseEquivalence));
        assert_eq!(binary_expression_from_string("&"), Some(VerilogBinaryExpression::ReductionAnd));
        assert_eq!(binary_expression_from_string("~&"), Some(VerilogBinaryExpression::ReductionNand));
        assert_eq!(binary_expression_from_string("|"), Some(VerilogBinaryExpression::ReductionOr));
        assert_eq!(binary_expression_from_string("~|"), Some(VerilogBinaryExpression::ReductionNor));
        assert_eq!(binary_expression_from_string("^"), Some(VerilogBinaryExpression::ReductionXor));
        assert_eq!(binary_expression_from_string("~^"), Some(VerilogBinaryExpression::ReductionXnor));
        assert_eq!(binary_expression_from_string("^~"), Some(VerilogBinaryExpression::ReductionXnor));
        assert_eq!(binary_expression_from_string("<<"), Some(VerilogBinaryExpression::ShiftLeft));
        assert_eq!(binary_expression_from_string(">>"), Some(VerilogBinaryExpression::ShiftRight));
        assert_eq!(binary_expression_from_string("<<<"), Some(VerilogBinaryExpression::ArithmeticShiftLeft));
        assert_eq!(binary_expression_from_string(">>>"), Some(VerilogBinaryExpression::ArithmeticShiftRight));
        assert_eq!(binary_expression_from_string("nonexistent"), None);
    }
}

use super::{identifier::Identifier, operators::{BinaryOperator, UnaryOperator}};



#[derive(Debug, PartialEq)]
pub enum Expression {
    Unparsed(String),
    // Literal(Literal),
    Identifier(Identifier),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>), // condition ? true_expr : false_expr
    Parenthesized(Box<Expression>),
}


pub struct ExpressionElements {
    elements: Vec<Expression>,
}
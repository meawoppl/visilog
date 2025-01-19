use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1},
    combinator::{map, map_res},
    error::VerboseError,
    sequence::{delimited, pair},
    IResult,
    multi::fold_many0,
}; // Import fold_many0

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Token {
    Number(i32),
    Plus,
    Minus,
    Star,
    Slash,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Number(i32),
    Binary(Box<Expression>, Token, Box<Expression>),
}

fn number(input: &str) -> IResult<&str, i32> {
    map_res(digit1, |s: &str| s.parse::<i32>())(input)
}

fn factor(input: &str) -> IResult<&str, Expression> {
    map(number, Expression::Number)(input)
}

fn term(input: &str) -> IResult<&str, Expression> {
    let (input, init) = factor(input)?;

    fold_many0(
        pair(alt((tag("*"), tag("/"))), factor),
        move || { init.clone()},
        |acc, (op, val): (&str, Expression)| {
            let token = match op {
                "*" => Token::Star,
                "/" => Token::Slash,
                _ => unreachable!(), // Should not happen with the current parser
            };
            Expression::Binary(Box::new(acc), token, Box::new(val))
        },
    )(input)
}

fn expression(input: &str) -> IResult<&str, Expression> {
    let (input, init) = term(input)?;

    fold_many0(
        pair(alt((tag("+"), tag("-"))), term),
        move || {init.clone()},
        |acc, (op, val): (&str, Expression)| {
            let token = match op {
                "+" => Token::Plus,
                "-" => Token::Minus,
                _ => unreachable!(), // Should not happen with the current parser
            };
            Expression::Binary(Box::new(acc), token, Box::new(val))
        },
    )(input)
}




#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_expression() {
        let input = "10 + 2 * 5 - 3 / 2";
        let (_, ast) = expression(input).unwrap();
        println!("{:?}", ast);
    }

    #[test]
    fn test_single_number() {
        let input = "42";
        let (_, ast) = expression(input).unwrap();
        assert_eq!(ast, Expression::Number(42));
    }

    #[test]
    fn test_addition() {
        let input = "1+1";
        let (_, ast) = expression(input).unwrap();
        assert_eq!(
            ast,
            Expression::Binary(
                Box::new(Expression::Number(1)),
                Token::Plus,
                Box::new(Expression::Number(1))
            )
        );
    }

    #[test]
    fn test_repeated_addition() {
        let input = "1+1+1+1+1";
        let (_, ast) = expression(input).unwrap();
        assert_eq!(
            ast,
            Expression::Binary(
                Box::new(Expression::Number(1)),
                Token::Plus,
                Box::new(Expression::Number(1))
            )
        );
    }


    #[test]
    fn test_subtraction() {
        let input = "5-3";
        let (_, ast) = expression(input).unwrap();
        assert_eq!(
            ast,
            Expression::Binary(
                Box::new(Expression::Number(5)),
                Token::Minus,
                Box::new(Expression::Number(3))
            )
        );
    }

    #[test]
    fn test_multiplication() {
        let input = "4*2";
        let (_, ast) = expression(input).unwrap();
        assert_eq!(
            ast,
            Expression::Binary(
                Box::new(Expression::Number(4)),
                Token::Star,
                Box::new(Expression::Number(2))
            )
        );
    }

    #[test]
    fn test_division() {
        let input = "8/4";
        let (_, ast) = expression(input).unwrap();
        assert_eq!(
            ast,
            Expression::Binary(
                Box::new(Expression::Number(8)),
                Token::Slash,
                Box::new(Expression::Number(4))
            )
        );
    }

    #[test]
    fn test_complex_expression() {
        let input = "3+5*2-8/4";
        let (_, ast) = expression(input).unwrap();
        
        
        
        println!("{:?}", ast);
    }
}
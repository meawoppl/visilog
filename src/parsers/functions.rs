use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, char, multispace0},
    combinator::{map, opt, recognize},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration {
    pub range_or_type: Option<RangeOrType>,
    pub identifier: String,
    pub items: Vec<FunctionItemDeclaration>,
    pub statement: String,
}

#[derive(Debug, PartialEq)]
pub enum RangeOrType {
    Range(i64, i64),
    Integer,
}

#[derive(Debug, PartialEq)]
pub enum FunctionItemDeclaration {
    InputDeclaration(String),
    ParameterDeclaration(String),
    RegDeclaration(String),
    IntegerDeclaration(String),
}

fn parse_identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    ))(input)
}

fn parse_range_or_type(input: &str) -> IResult<&str, RangeOrType> {
    alt((
        map(
            delimited(char('['), pair(parse_integer, preceded(char(':'), parse_integer)), char(']')),
            |(n, m)| RangeOrType::Range(n, m),
        ),
        map(tag("integer"), |_| RangeOrType::Integer),
    ))(input)
}

fn parse_integer(input: &str) -> IResult<&str, i64> {
    map(take_while(|c: char| c.is_digit(10)), |s: &str| s.parse::<i64>().unwrap())(input)
}

fn parse_function_item_declaration(input: &str) -> IResult<&str, FunctionItemDeclaration> {
    alt((
        map(preceded(tag("input"), parse_identifier), |id| FunctionItemDeclaration::InputDeclaration(id.to_string())),
        map(preceded(tag("parameter"), parse_identifier), |id| FunctionItemDeclaration::ParameterDeclaration(id.to_string())),
        map(preceded(tag("reg"), parse_identifier), |id| FunctionItemDeclaration::RegDeclaration(id.to_string())),
        map(preceded(tag("integer"), parse_identifier), |id| FunctionItemDeclaration::IntegerDeclaration(id.to_string())),
    ))(input)
}

fn parse_function_declaration(input: &str) -> IResult<&str, FunctionDeclaration> {
    let (input, _) = tag("function")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, range_or_type) = opt(parse_range_or_type)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, identifier) = parse_identifier(input)?;
    let (input, _) = char(';')(input)?;
    let (input, items) = separated_list0(multispace0, parse_function_item_declaration)(input)?;
    let (input, statement) = take_while(|c| c != 'e')(input)?;
    let (input, _) = tag("endfunction")(input)?;
    Ok((
        input,
        FunctionDeclaration {
            range_or_type,
            identifier: identifier.to_string(),
            items,
            statement: statement.to_string(),
        },
    ))
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    pub identifier: String,
    pub arguments: Vec<String>,
}

fn parse_function_call(input: &str) -> IResult<&str, FunctionCall> {
    let (input, identifier) = parse_identifier(input)?;
    let (input, arguments) = opt(delimited(
        char('('),
        separated_list0(char(','), parse_identifier),
        char(')'),
    ))(input)?;
    Ok((
        input,
        FunctionCall {
            identifier: identifier.to_string(),
            arguments: arguments.unwrap_or_default().into_iter().map(|s| s.to_string()).collect(),
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_function_declaration() {
        let input = r#"
            function integer my_function;
                input a;
                reg b;
                integer c;
                my_function = a + b + c;
            endfunction
        "#;
        let result = parse_function_declaration(input);
        assert!(result.is_ok());
        let (_, function) = result.unwrap();
        assert_eq!(function.identifier, "my_function");
        assert_eq!(function.range_or_type, Some(RangeOrType::Integer));
        assert_eq!(function.items.len(), 3);
        assert_eq!(function.items[0], FunctionItemDeclaration::InputDeclaration("a".to_string()));
        assert_eq!(function.items[1], FunctionItemDeclaration::RegDeclaration("b".to_string()));
        assert_eq!(function.items[2], FunctionItemDeclaration::IntegerDeclaration("c".to_string()));
        assert_eq!(function.statement.trim(), "my_function = a + b + c;");
    }

    #[test]
    fn test_parse_function_call() {
        let input = "my_function(a, b, c)";
        let result = parse_function_call(input);
        assert!(result.is_ok());
        let (_, function_call) = result.unwrap();
        assert_eq!(function_call.identifier, "my_function");
        assert_eq!(function_call.arguments, vec!["a".to_string(), "b".to_string(), "c".to_string()]);
    }
}

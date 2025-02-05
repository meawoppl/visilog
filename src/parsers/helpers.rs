use nom::Err;
use nom::IResult;

pub fn assert_parses_to<'a, O>(
    parser: fn(&'a str) -> IResult<&'a str, O>,
    input: &'a str,
    expected: O,
) -> O
where
    O: std::fmt::Debug + PartialEq,
{
    let result = assert_parses(parser, input);
    assert_eq!(result, expected);
    result
}

pub fn assert_parses<'a, O>(parser: fn(&'a str) -> IResult<&'a str, O>, input: &'a str) -> O
where
    O: std::fmt::Debug + PartialEq,
{
    let res = parser(input);
    match res {
        Ok((residual, result)) => {
            assert_eq!(
                residual, "",
                "Failed to parse entire expression: {} (leftovers: {})",
                input, residual
            );

            result
        }
        Err(err) => match err {
            Err::Error(_) | Err::Failure(_) => {
                // Unpack Verbose Errors nicely
                let verbose_error = nom::error::VerboseError {
                    errors: vec![(input, nom::error::VerboseErrorKind::Context("context"))],
                };
                panic!(
                    "Parsing Failure: {}",
                    nom::error::convert_error(input, verbose_error)
                );
            }
            _ => {
                panic!("Failed to parse expression: {}", input);
            }
        },
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use nom::character::complete::digit1;
    fn parse_digit(input: &str) -> IResult<&str, &str> {
        digit1(input)
    }

    #[test]
    fn test_assert_parses_to_success() {
        assert_parses_to(digit1, "123", "123");
    }

    #[test]
    #[should_panic(expected = "0: at line 1")]
    fn test_assert_parses_to_failure() {
        assert_parses_to(parse_digit, "abc", "123");
    }

    #[test]
    #[should_panic(expected = "Failed to parse entire expression")]
    fn test_assert_parses_to_unexpected_residual() {
        assert_parses_to(parse_digit, "123abc", "123");
    }

    #[test]
    #[should_panic(expected = "assertion `left == right` failed\n  left: \"123\"\n right: \"456\"")]
    fn test_assert_parses_to_unexpected_result() {
        assert_parses_to(parse_digit, "123", "456");
    }
}

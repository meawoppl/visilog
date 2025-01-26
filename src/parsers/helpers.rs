use nom::IResult;
use nom::character::complete::char;

pub fn assert_parses_completely<T: std::fmt::Debug + PartialEq>(
    parser: impl Fn(&str) -> IResult<&str, T>,
    input: &str,
    expected: T,
) {
    let (remaining_input, result) = parser(input).expect("Parsing failed");
    assert_eq!(result, expected);
    assert!(remaining_input.is_empty(), "Input not fully consumed: {}", remaining_input);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn trivial_parser(input: &str) -> IResult<&str, char> {
        char('a')(input)
    }

    #[test]
    fn test_assert_parses_completely_success() {
        assert_parses_completely(trivial_parser, "a", 'a');
    }

    #[test]
    #[should_panic(expected = "Parsing failed")]
    fn test_assert_parses_completely_failure() {
        assert_parses_completely(trivial_parser, "b", 'a');
    }

    #[test]
    #[should_panic(expected = "Input not fully consumed: a")]
    fn test_assert_parses_completely_remaining_input() {
        assert_parses_completely(trivial_parser, "aa", 'a');
    }
}

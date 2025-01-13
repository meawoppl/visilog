use nom::{branch::alt, character::complete::one_of, combinator::map, sequence::{preceded, tuple}, IResult};

use super::numbers::{decimal, hexadecimal};
use nom::character::complete::char;

#[derive(Debug, PartialEq)]
enum VerilogBaseType {
    Binary,
    Decimal,
    Octal,
    Hexadecimal,
}

fn const_type_char(input: &str) -> IResult<&str, VerilogBaseType> {
    map(one_of("bdohxBDOHX"), |c| {
        match c.to_ascii_lowercase() {
            'b' => VerilogBaseType::Binary,
            'd' => VerilogBaseType::Decimal,
            'o' => VerilogBaseType::Octal,
            'h' => VerilogBaseType::Hexadecimal,
            'x' => VerilogBaseType::Hexadecimal,
            _ => unreachable!(), // Should never happen due to one_of
        }
    })(input)
}

fn sized_const(input: &str) -> IResult<&str, (&str, VerilogBaseType, &str)> {
    tuple((
        decimal,
        preceded(char('\''), const_type_char),
        alt((hexadecimal, decimal)),
    ))(input)
}

#[cfg(test)]
mod tests {
    use nom::Parser;

    use super::*;

    #[test]
    fn test_sized_bits() {
        assert_eq!(
            sized_const("3'b010"),
            Ok(("", ("3", VerilogBaseType::Binary, "010")))
        );
        assert_eq!(
            sized_const("3'd2"),
            Ok(("", ("3", VerilogBaseType::Decimal, "2")))
        );
        assert_eq!(
            sized_const("8'h70"),
            Ok(("", ("8", VerilogBaseType::Hexadecimal, "70")))
        );
        assert_eq!(
            sized_const("9'h1FA"),
            Ok(("", ("9", VerilogBaseType::Hexadecimal, "1FA")))
        );
        assert_eq!(
            sized_const("32'hFACE_47B2"),
            Ok(("", ("32", VerilogBaseType::Hexadecimal, "FACE_47B2")))
        );
        assert_eq!(
            sized_const("8'D234"),
            Ok(("", ("8", VerilogBaseType::Decimal, "234")))
        );
    }
}

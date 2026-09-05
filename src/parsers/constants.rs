use core::fmt;

use nom::{
    branch::alt,
    character::complete::one_of,
    combinator::{map, map_res},
    sequence::{preceded, tuple},
    IResult,
};

use super::base::RawToken;
use super::numbers::{based_digits, decimal};
use nom::character::complete::char;

#[derive(Clone, Debug, PartialEq)]
pub enum VerilogBaseType {
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

#[derive(Debug, Clone, PartialEq)]
pub struct VerilogConstant {
    size: Option<usize>,
    base_type: VerilogBaseType,
    value: String,
    /// The `s` of `4'sd12` — the literal is a two's complement number.
    signed: bool,
}

/// The optional `s` that makes a based literal signed: `4'sd12`, `8'SH0F`.
fn const_signedness(input: &str) -> IResult<&str, bool> {
    map(nom::combinator::opt(one_of("sS")), |s| s.is_some())(input)
}

impl fmt::Display for VerilogConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.size.is_none() && self.base_type == VerilogBaseType::Decimal {
            return write!(f, "{}", self.value);
        }

        write!(
            f,
            "{}'{}{}{}",
            match self.size {
                Some(size) => size.to_string(),
                None => "".to_string(),
            },
            if self.signed { "s" } else { "" },
            match self.base_type {
                VerilogBaseType::Binary => "b",
                VerilogBaseType::Decimal => "d",
                VerilogBaseType::Octal => "o",
                VerilogBaseType::Hexadecimal => "h",
            },
            self.value
        )
    }
}

impl VerilogConstant {
    fn new(size: Option<usize>, base_type: VerilogBaseType, value: String) -> Self {
        VerilogConstant {
            size,
            base_type,
            value,
            signed: false,
        }
    }

    /// The same literal, read as two's complement — the `s` of `4'sd12`.
    fn with_signedness(mut self, signed: bool) -> Self {
        self.signed = signed;
        self
    }

    pub fn from_int(value: i64) -> Self {
        VerilogConstant {
            size: None,
            base_type: VerilogBaseType::Decimal,
            value: value.to_string(),
            signed: false,
        }
    }

    /// Whether the literal is a two's complement number.
    ///
    /// Two spellings make one: the `s` designator of `4'sd12`, and a decimal
    /// written with neither a size nor a base — `42` is signed where `4'd42`
    /// is not. The LRM draws that second line at the *base* rather than at the
    /// size, so `'d42` is unsigned to it and signed here; nothing on this type
    /// records whether a base was written, and an unsized based decimal is
    /// rare enough not to be worth a field that could not be compared.
    pub fn is_signed(&self) -> bool {
        self.signed || (self.size.is_none() && self.base_type == VerilogBaseType::Decimal)
    }

    /// The declared bit width, e.g. the `8` of `8'hFF`. `None` when the
    /// literal was written without one (`42`, `'hFF`).
    pub fn size(&self) -> Option<usize> {
        self.size
    }

    /// The radix the digits are written in, e.g. hexadecimal for `8'hFF`.
    pub fn base_type(&self) -> &VerilogBaseType {
        &self.base_type
    }

    /// The digits as written, e.g. the `FACE_47B2` of `32'hFACE_47B2`. Case,
    /// `_` separators and `x`/`z`/`?` are all preserved verbatim.
    pub fn digits(&self) -> &str {
        &self.value
    }
}

impl RawToken for VerilogConstant {
    fn raw_token(&self) -> String {
        format!(
            "{}'{}{}{}",
            match self.size {
                Some(size) => size.to_string(),
                None => "".to_string(),
            },
            if self.signed { "s" } else { "" },
            match self.base_type {
                VerilogBaseType::Binary => "b",
                VerilogBaseType::Decimal => "d",
                VerilogBaseType::Octal => "o",
                VerilogBaseType::Hexadecimal => "h",
            },
            self.value,
        )
    }
}

fn integer_constant(input: &str) -> IResult<&str, VerilogConstant> {
    map_res(decimal, |content| {
        let cnst = VerilogConstant::new(None, VerilogBaseType::Decimal, content.to_string());
        Ok::<_, nom::Err<nom::error::Error<&str>>>(cnst)
    })(input)
}

fn unsized_const(input: &str) -> IResult<&str, VerilogConstant> {
    let parsed = tuple((
        preceded(char('\''), tuple((const_signedness, const_type_char))),
        based_digits,
    ));

    map_res(parsed, |((signed, base), content)| {
        let cnst = VerilogConstant::new(None, base, content.to_string()).with_signedness(signed);
        Ok::<_, nom::Err<nom::error::Error<&str>>>(cnst)
    })(input)
}

fn sized_const(input: &str) -> IResult<&str, VerilogConstant> {
    let parsed = tuple((
        decimal,
        preceded(char('\''), tuple((const_signedness, const_type_char))),
        based_digits,
    ));

    map_res(parsed, |(size_str, (signed, base), content)| {
        let size = size_str.parse::<usize>().unwrap();
        let cnst =
            VerilogConstant::new(Some(size), base, content.to_string()).with_signedness(signed);
        Ok::<_, nom::Err<nom::error::Error<&str>>>(cnst)
    })(input)
}

pub fn verilog_const(input: &str) -> IResult<&str, VerilogConstant> {
    alt((sized_const, unsized_const, integer_constant))(input)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_sized_bits() {
        assert_eq!(
            sized_const("3'b010"),
            Ok((
                "",
                VerilogConstant::new(Some(3), VerilogBaseType::Binary, "010".to_string())
            ))
        );
        assert_eq!(
            sized_const("3'd2"),
            Ok((
                "",
                VerilogConstant::new(Some(3), VerilogBaseType::Decimal, "2".to_string())
            ))
        );
        assert_eq!(
            sized_const("8'h70"),
            Ok((
                "",
                VerilogConstant::new(Some(8), VerilogBaseType::Hexadecimal, "70".to_string())
            ))
        );
        assert_eq!(
            sized_const("9'h1FA"),
            Ok((
                "",
                VerilogConstant::new(Some(9), VerilogBaseType::Hexadecimal, "1FA".to_string())
            ))
        );
        assert_eq!(
            sized_const("32'hFACE_47B2"),
            Ok((
                "",
                VerilogConstant::new(
                    Some(32),
                    VerilogBaseType::Hexadecimal,
                    "FACE_47B2".to_string()
                )
            ))
        );
        assert_eq!(
            sized_const("8'D234"),
            Ok((
                "",
                VerilogConstant::new(Some(8), VerilogBaseType::Decimal, "234".to_string())
            ))
        );
        assert_eq!(
            sized_const("4'o77"),
            Ok((
                "",
                VerilogConstant::new(Some(4), VerilogBaseType::Octal, "77".to_string())
            ))
        );
        assert_eq!(
            sized_const("16'hABCD"),
            Ok((
                "",
                VerilogConstant::new(Some(16), VerilogBaseType::Hexadecimal, "ABCD".to_string())
            ))
        );
        assert_eq!(
            sized_const("16'hABCD"),
            Ok((
                "",
                VerilogConstant::new(Some(16), VerilogBaseType::Hexadecimal, "ABCD".to_string())
            ))
        );
        assert_eq!(
            sized_const("4'b1010"),
            Ok((
                "",
                VerilogConstant::new(Some(4), VerilogBaseType::Binary, "1010".to_string())
            ))
        );
        assert_eq!(
            sized_const("12'd4095"),
            Ok((
                "",
                VerilogConstant::new(Some(12), VerilogBaseType::Decimal, "4095".to_string())
            ))
        );
    }

    #[test]
    fn test_integer_constants() {
        assert_eq!(
            integer_constant("123"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "123".to_string())
            ))
        );
        assert_eq!(
            integer_constant("0"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "0".to_string())
            ))
        );
        assert_eq!(
            integer_constant("456789"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "456789".to_string())
            ))
        );
        assert_eq!(
            integer_constant("42"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "42".to_string())
            ))
        );
        assert_eq!(
            integer_constant("987654321"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "987654321".to_string())
            ))
        );
        assert_eq!(
            integer_constant("987654321"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "987654321".to_string())
            ))
        );
        assert_eq!(
            integer_constant("42"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "42".to_string())
            ))
        );
    }

    #[test]
    fn test_unsized_constants() {
        assert_eq!(
            unsized_const("'b1010"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Binary, "1010".to_string())
            ))
        );
        assert_eq!(
            unsized_const("'d42"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "42".to_string())
            ))
        );
        assert_eq!(
            unsized_const("'h1A3F"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "1A3F".to_string())
            ))
        );
        assert_eq!(
            unsized_const("'o77"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Octal, "77".to_string())
            ))
        );
        assert_eq!(
            unsized_const("'HFF"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "FF".to_string())
            ))
        );
        assert_eq!(
            unsized_const("'b1101"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Binary, "1101".to_string())
            ))
        );
        assert_eq!(
            unsized_const("'d1234"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "1234".to_string())
            ))
        );
    }

    #[test]
    fn test_verilog_const() {
        assert_eq!(
            verilog_const("3'b010"),
            Ok((
                "",
                VerilogConstant::new(Some(3), VerilogBaseType::Binary, "010".to_string())
            ))
        );
        assert_eq!(
            verilog_const("3'd2"),
            Ok((
                "",
                VerilogConstant::new(Some(3), VerilogBaseType::Decimal, "2".to_string())
            ))
        );
        assert_eq!(
            verilog_const("8'h70"),
            Ok((
                "",
                VerilogConstant::new(Some(8), VerilogBaseType::Hexadecimal, "70".to_string())
            ))
        );
        assert_eq!(
            verilog_const("9'h1FA"),
            Ok((
                "",
                VerilogConstant::new(Some(9), VerilogBaseType::Hexadecimal, "1FA".to_string())
            ))
        );
        assert_eq!(
            verilog_const("32'hFACE_47B2"),
            Ok((
                "",
                VerilogConstant::new(
                    Some(32),
                    VerilogBaseType::Hexadecimal,
                    "FACE_47B2".to_string()
                )
            ))
        );
        assert_eq!(
            verilog_const("8'D234"),
            Ok((
                "",
                VerilogConstant::new(Some(8), VerilogBaseType::Decimal, "234".to_string())
            ))
        );
        assert_eq!(
            verilog_const("123"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "123".to_string())
            ))
        );
        assert_eq!(
            verilog_const("0"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "0".to_string())
            ))
        );
        assert_eq!(
            verilog_const("456789"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "456789".to_string())
            ))
        );
        assert_eq!(
            verilog_const("'b1010"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Binary, "1010".to_string())
            ))
        );
        assert_eq!(
            verilog_const("'d42"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "42".to_string())
            ))
        );
        assert_eq!(
            verilog_const("'h1A3F"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "1A3F".to_string())
            ))
        );
        assert_eq!(
            verilog_const("'o77"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Octal, "77".to_string())
            ))
        );
        assert_eq!(
            verilog_const("'HFF"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "FF".to_string())
            ))
        );
        assert_eq!(
            verilog_const("4'o77"),
            Ok((
                "",
                VerilogConstant::new(Some(4), VerilogBaseType::Octal, "77".to_string())
            ))
        );
        assert_eq!(
            verilog_const("16'hABCD"),
            Ok((
                "",
                VerilogConstant::new(Some(16), VerilogBaseType::Hexadecimal, "ABCD".to_string())
            ))
        );
        assert_eq!(
            unsized_const("'b1101"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Binary, "1101".to_string())
            ))
        );
        assert_eq!(
            unsized_const("'d100"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "100".to_string())
            ))
        );
        assert_eq!(
            unsized_const("'hABC"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "ABC".to_string())
            ))
        );
    }

    #[test]
    fn test_four_state_constants() {
        assert_eq!(
            verilog_const("4'bzzzz"),
            Ok((
                "",
                VerilogConstant::new(Some(4), VerilogBaseType::Binary, "zzzz".to_string())
            ))
        );
        assert_eq!(
            verilog_const("8'hXX"),
            Ok((
                "",
                VerilogConstant::new(Some(8), VerilogBaseType::Hexadecimal, "XX".to_string())
            ))
        );
        assert_eq!(
            verilog_const("'b1?0z"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Binary, "1?0z".to_string())
            ))
        );
    }
}

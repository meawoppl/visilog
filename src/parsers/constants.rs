use nom::{
    branch::alt,
    character::complete::one_of,
    combinator::{map, map_res},
    sequence::{preceded, tuple},
    IResult,
};

use super::numbers::{decimal, hexadecimal};
use super::base::RawToken;
use nom::character::complete::char;


#[derive(Clone, Debug, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct VerilogConstant {
    size: Option<usize>,
    base_type: VerilogBaseType,
    value: String,
}

impl VerilogConstant {
    fn new(size: Option<usize>, base_type: VerilogBaseType, value: String) -> Self {
        VerilogConstant {
            size,
            base_type,
            value,
        }
    }
    pub fn from_int(value: i64) -> Self {
        VerilogConstant {
            size: None,
            base_type: VerilogBaseType::Decimal,
            value: value.to_string(),
        }
    }
}

impl RawToken for VerilogConstant {
    fn raw_token(&self) -> String {
        format!("{}'{}{}",
             match self.size {
                Some(size) => size.to_string(),
                None => "".to_string(),
             },
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
        preceded(char('\''), const_type_char),
        alt((hexadecimal, decimal)),
    ));

    map_res(parsed, |(base, content)| {
        let cnst = VerilogConstant::new(None, base, content.to_string());
        Ok::<_, nom::Err<nom::error::Error<&str>>>(cnst)
    })(input)
}

fn sized_const(input: &str) -> IResult<&str, VerilogConstant> {
    let parsed = tuple((
        decimal,
        preceded(char('\''), const_type_char),
        alt((hexadecimal, decimal)),
    ));

    map_res(parsed, |(size_str, base, content)| {
        let size = size_str.parse::<usize>().unwrap();
        let cnst = VerilogConstant::new(Some(size), base, content.to_string());
        Ok::<_, nom::Err<nom::error::Error<&str>>>(cnst)
    })(input)
}

pub fn verilog_const(input: &str) -> IResult<&str, VerilogConstant> {
    alt((sized_const, unsized_const, integer_constant))(input)
}

#[cfg(test)]
mod tests {
    use nom::Parser;

    use super::*;

    #[test]
    fn test_sized_bits() {
        assert_eq!(
            sized_const("3'b010"),
            Ok(("", VerilogConstant::new(Some(3), VerilogBaseType::Binary, "010".to_string())))
        );
        assert_eq!(
            sized_const("3'd2"),
            Ok(("", VerilogConstant::new(Some(3), VerilogBaseType::Decimal, "2".to_string())))
        );
        assert_eq!(
            sized_const("8'h70"),
            Ok(("", VerilogConstant::new(Some(8), VerilogBaseType::Hexadecimal, "70".to_string())))
        );
        assert_eq!(
            sized_const("9'h1FA"),
            Ok(("", VerilogConstant::new(Some(9), VerilogBaseType::Hexadecimal, "1FA".to_string())))
        );
        assert_eq!(
            sized_const("32'hFACE_47B2"),
            Ok(("", VerilogConstant::new(Some(32), VerilogBaseType::Hexadecimal, "FACE_47B2".to_string())))
        );
        assert_eq!(
            sized_const("8'D234"),
            Ok(("", VerilogConstant::new(Some(8), VerilogBaseType::Decimal, "234".to_string())))
        );
    }


    #[test]
    fn test_integer_constants() {
        assert_eq!(
            integer_constant("123"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Decimal, "123".to_string())))
        );
        assert_eq!(
            integer_constant("0"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Decimal, "0".to_string())))
        );
        assert_eq!(
            integer_constant("456789"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Decimal, "456789".to_string())))
        );
    }

    #[test]
    fn test_unsized_constants() {
        assert_eq!(
            unsized_const("'b1010"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Binary, "1010".to_string())))
        );
        assert_eq!(
            unsized_const("'d42"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Decimal, "42".to_string())))
        );
        assert_eq!(
            unsized_const("'h1A3F"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "1A3F".to_string())))
        );
        assert_eq!(
            unsized_const("'o77"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Octal, "77".to_string())))
        );
        assert_eq!(
            unsized_const("'HFF"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "FF".to_string())))
        );
    }

    #[test]
    fn test_verilog_const() {
        assert_eq!(
            verilog_const("3'b010"),
            Ok(("", VerilogConstant::new(Some(3), VerilogBaseType::Binary, "010".to_string())))
        );
        assert_eq!(
            verilog_const("3'd2"),
            Ok(("", VerilogConstant::new(Some(3), VerilogBaseType::Decimal, "2".to_string())))
        );
        assert_eq!(
            verilog_const("8'h70"),
            Ok(("", VerilogConstant::new(Some(8), VerilogBaseType::Hexadecimal, "70".to_string())))
        );
        assert_eq!(
            verilog_const("9'h1FA"),
            Ok(("", VerilogConstant::new(Some(9), VerilogBaseType::Hexadecimal, "1FA".to_string())))
        );
        assert_eq!(
            verilog_const("32'hFACE_47B2"),
            Ok(("", VerilogConstant::new(Some(32), VerilogBaseType::Hexadecimal, "FACE_47B2".to_string())))
        );
        assert_eq!(
            verilog_const("8'D234"),
            Ok(("", VerilogConstant::new(Some(8), VerilogBaseType::Decimal, "234".to_string())))
        );
        assert_eq!(
            verilog_const("123"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Decimal, "123".to_string())))
        );
        assert_eq!(
            verilog_const("0"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Decimal, "0".to_string())))
        );
        assert_eq!(
            verilog_const("456789"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Decimal, "456789".to_string())))
        );
        assert_eq!(
            verilog_const("'b1010"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Binary, "1010".to_string())))
        );
        assert_eq!(
            verilog_const("'d42"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Decimal, "42".to_string())))
        );
        assert_eq!(
            verilog_const("'h1A3F"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "1A3F".to_string())))
        );
        assert_eq!(
            verilog_const("'o77"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Octal, "77".to_string())))
        );
        assert_eq!(
            verilog_const("'HFF"),
            Ok(("", VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "FF".to_string())))
        );
    }

}

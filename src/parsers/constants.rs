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
use super::simple::ws_and_comments;
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
    /// Whether a base designator was written at all: `'d1` against `1`.
    ///
    /// It is the *base* rather than the size that decides whether a decimal
    /// is signed without an `s` — `1` is and `'d1` is not — and nothing else
    /// on this type can tell those two apart, since both arrive as an unsized
    /// decimal. A **sized** literal always has one, which is why
    /// [`VerilogConstant::new`] reads it off the size and only an unsized one
    /// has to say so.
    based: bool,
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
            based: size.is_some(),
            size,
            base_type,
            value,
            signed: false,
        }
    }

    /// The same literal, written with a base designator — `'d1` rather than
    /// `1`. See [`VerilogConstant::based`].
    fn with_base(mut self) -> Self {
        self.based = true;
        self
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
            based: false,
            // A negative value has to read as one: a genvar counting down
            // reaches `-1`, and an unsigned `-1` is `4294967295`, which makes
            // `i >= 0` true for ever and the loop never end.
            signed: value < 0,
        }
    }

    /// Whether the literal is a two's complement number.
    ///
    /// Two spellings make one: the `s` designator of `4'sd12`, and a decimal
    /// written with **no base designator at all** — `42` is signed where
    /// `'d42` and `4'd42` are not. The line is drawn at the *base* rather than
    /// at the size, which is what IEEE 1364-2005 asks for and what iverilog
    /// 12.0 does: `parameter tp = 'd1; $display("%d", tp);` prints in ten
    /// columns, the width of an unsigned `integer`, where `parameter tp = 1;`
    /// prints in eleven (corpus `pr812`).
    pub fn is_signed(&self) -> bool {
        self.signed
            || (!self.based && self.size.is_none() && self.base_type == VerilogBaseType::Decimal)
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

    /// Whether widening this literal fills with `x`/`z` rather than with `0`.
    ///
    /// An **unsized** literal takes the width of whatever it is written
    /// against, and IEEE 1364-2005 3.5.1 extends one whose most significant
    /// digit is `x` or `z` with that digit — so `'hx` against a 64 bit
    /// register is sixty-four `x`s. A **sized** literal was already extended
    /// to its own width where it was written and is an ordinary value from
    /// then on, which is why the size is asked about first: it is the cheap
    /// half of the question and answers for nearly every literal a design
    /// contains.
    pub fn extends_with_unknown(&self) -> bool {
        self.size.is_none()
            && matches!(
                self.value
                    .chars()
                    .find(|c| *c != '_')
                    .map(|c| c.to_ascii_lowercase()),
                Some('x') | Some('z') | Some('?')
            )
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

/// The base designator of a based literal: the `'h` of `8'hFF`, with the
/// optional `s` of `4'sd12` in front of the base letter.
///
/// The `'` and the letter are one token — the LRM lets whitespace separate a
/// literal's size from its base and its base from its digits, but not the `'`
/// from what it introduces.
fn base_designator(input: &str) -> IResult<&str, (bool, VerilogBaseType)> {
    preceded(char('\''), tuple((const_signedness, const_type_char)))(input)
}

fn unsized_const(input: &str) -> IResult<&str, VerilogConstant> {
    let parsed = tuple((base_designator, preceded(ws_and_comments, based_digits)));

    map_res(parsed, |((signed, base), content)| {
        let cnst = VerilogConstant::new(None, base, content.to_string())
            .with_base()
            .with_signedness(signed);
        Ok::<_, nom::Err<nom::error::Error<&str>>>(cnst)
    })(input)
}

/// `8'hFF`, `4'sd12`, `5'h 0`, `5 'h0`.
///
/// The size, the base designator and the digits are three tokens, so
/// whitespace and comments between them are skipped exactly as they are
/// between a `#` and its delay.
fn sized_const(input: &str) -> IResult<&str, VerilogConstant> {
    let parsed = tuple((
        decimal,
        preceded(ws_and_comments, base_designator),
        preceded(ws_and_comments, based_digits),
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
                    .with_base()
            ))
        );
        assert_eq!(
            sized_const("3'd2"),
            Ok((
                "",
                VerilogConstant::new(Some(3), VerilogBaseType::Decimal, "2".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            sized_const("8'h70"),
            Ok((
                "",
                VerilogConstant::new(Some(8), VerilogBaseType::Hexadecimal, "70".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            sized_const("9'h1FA"),
            Ok((
                "",
                VerilogConstant::new(Some(9), VerilogBaseType::Hexadecimal, "1FA".to_string())
                    .with_base()
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
                .with_base()
            ))
        );
        assert_eq!(
            sized_const("8'D234"),
            Ok((
                "",
                VerilogConstant::new(Some(8), VerilogBaseType::Decimal, "234".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            sized_const("4'o77"),
            Ok((
                "",
                VerilogConstant::new(Some(4), VerilogBaseType::Octal, "77".to_string()).with_base()
            ))
        );
        assert_eq!(
            sized_const("16'hABCD"),
            Ok((
                "",
                VerilogConstant::new(Some(16), VerilogBaseType::Hexadecimal, "ABCD".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            sized_const("16'hABCD"),
            Ok((
                "",
                VerilogConstant::new(Some(16), VerilogBaseType::Hexadecimal, "ABCD".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            sized_const("4'b1010"),
            Ok((
                "",
                VerilogConstant::new(Some(4), VerilogBaseType::Binary, "1010".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            sized_const("12'd4095"),
            Ok((
                "",
                VerilogConstant::new(Some(12), VerilogBaseType::Decimal, "4095".to_string())
                    .with_base()
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
                VerilogConstant::new(None, VerilogBaseType::Binary, "1010".to_string()).with_base()
            ))
        );
        assert_eq!(
            unsized_const("'d42"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "42".to_string()).with_base()
            ))
        );
        assert_eq!(
            unsized_const("'h1A3F"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "1A3F".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            unsized_const("'o77"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Octal, "77".to_string()).with_base()
            ))
        );
        assert_eq!(
            unsized_const("'HFF"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "FF".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            unsized_const("'b1101"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Binary, "1101".to_string()).with_base()
            ))
        );
        assert_eq!(
            unsized_const("'d1234"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "1234".to_string())
                    .with_base()
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
                    .with_base()
            ))
        );
        assert_eq!(
            verilog_const("3'd2"),
            Ok((
                "",
                VerilogConstant::new(Some(3), VerilogBaseType::Decimal, "2".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            verilog_const("8'h70"),
            Ok((
                "",
                VerilogConstant::new(Some(8), VerilogBaseType::Hexadecimal, "70".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            verilog_const("9'h1FA"),
            Ok((
                "",
                VerilogConstant::new(Some(9), VerilogBaseType::Hexadecimal, "1FA".to_string())
                    .with_base()
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
                .with_base()
            ))
        );
        assert_eq!(
            verilog_const("8'D234"),
            Ok((
                "",
                VerilogConstant::new(Some(8), VerilogBaseType::Decimal, "234".to_string())
                    .with_base()
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
                VerilogConstant::new(None, VerilogBaseType::Binary, "1010".to_string()).with_base()
            ))
        );
        assert_eq!(
            verilog_const("'d42"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "42".to_string()).with_base()
            ))
        );
        assert_eq!(
            verilog_const("'h1A3F"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "1A3F".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            verilog_const("'o77"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Octal, "77".to_string()).with_base()
            ))
        );
        assert_eq!(
            verilog_const("'HFF"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "FF".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            verilog_const("4'o77"),
            Ok((
                "",
                VerilogConstant::new(Some(4), VerilogBaseType::Octal, "77".to_string()).with_base()
            ))
        );
        assert_eq!(
            verilog_const("16'hABCD"),
            Ok((
                "",
                VerilogConstant::new(Some(16), VerilogBaseType::Hexadecimal, "ABCD".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            unsized_const("'b1101"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Binary, "1101".to_string()).with_base()
            ))
        );
        assert_eq!(
            unsized_const("'d100"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "100".to_string()).with_base()
            ))
        );
        assert_eq!(
            unsized_const("'hABC"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Hexadecimal, "ABC".to_string())
                    .with_base()
            ))
        );
    }

    /// The size, the base designator and the digits are three tokens, so
    /// whitespace and comments may separate them. The `\'` and its base letter
    /// are one token and may not be split.
    #[test]
    fn test_a_based_literal_tolerates_whitespace_between_its_tokens() {
        let expected = VerilogConstant::new(Some(5), VerilogBaseType::Hexadecimal, "0".to_string());
        for spelling in [
            "5'h0",
            "5'h 0",
            "5 'h0",
            "5 'h 0",
            "5\n'h\n0",
            "5/* wide */'h/* zero */0",
        ] {
            assert_eq!(
                verilog_const(spelling),
                Ok(("", expected.clone())),
                "{}",
                spelling
            );
        }

        // The signed designator rides with the base letter, not with the size.
        assert_eq!(
            verilog_const("4 'sd 12"),
            Ok((
                "",
                VerilogConstant::new(Some(4), VerilogBaseType::Decimal, "12".to_string())
                    .with_base()
                    .with_signedness(true)
            ))
        );

        // An unsized literal gets the same treatment on the one boundary it has.
        assert_eq!(
            verilog_const("'b 1010"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Binary, "1010".to_string()).with_base()
            ))
        );

        // A `\'` split from its base letter is not a literal, and neither is a
        // size on its own.
        assert!(unsized_const("' h0").is_err());
        assert!(sized_const("5 ' h0").is_err());
        assert_eq!(
            verilog_const("5 ;"),
            Ok((
                " ;",
                VerilogConstant::new(None, VerilogBaseType::Decimal, "5".to_string())
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
                    .with_base()
            ))
        );
        assert_eq!(
            verilog_const("8'hXX"),
            Ok((
                "",
                VerilogConstant::new(Some(8), VerilogBaseType::Hexadecimal, "XX".to_string())
                    .with_base()
            ))
        );
        assert_eq!(
            verilog_const("'b1?0z"),
            Ok((
                "",
                VerilogConstant::new(None, VerilogBaseType::Binary, "1?0z".to_string()).with_base()
            ))
        );
    }

    /// Only an *unsized* literal whose most significant digit is `x`, `z` or
    /// `?` fills a wider context with that digit; everything else pads with
    /// zeros. A sized literal is answered by the size alone, which is what
    /// keeps the digit scan off the path nearly every literal takes.
    #[test]
    fn test_which_literals_extend_with_unknown() {
        let extends = |source: &str| verilog_const(source).unwrap().1.extends_with_unknown();
        assert!(extends("'hx"));
        assert!(extends("'hz"));
        assert!(extends("'bx1"));
        assert!(extends("'b?"));
        assert!(extends("'h_x1"));
        assert!(!extends("4'bx"));
        assert!(!extends("'h1x"));
        assert!(!extends("'h1"));
        assert!(!extends("42"));
    }
}

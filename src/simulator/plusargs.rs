//! The command line plus-args, and the conversions `$value$plusargs` reads
//! them with.
//!
//! A plus-arg is a `+name=value` word a simulator is started with, and the two
//! system functions that read them are the only way a design learns anything
//! about its own invocation. visilog has no CLI yet, so the list is whatever a
//! caller handed [`Simulator::add_plusarg`](crate::simulator::runner::Simulator::add_plusarg)
//! — for a corpus run, the `+` fields of the entry's own line in
//! `regress-vlg.list`, which is exactly where iverilog's test driver gets them.
//!
//! **A design that finds no plus-arg is not a design that failed.** `$test`
//! answers `0` and `$value` answers `0` and leaves its target alone, which is
//! what a design checks for, so an empty list is an ordinary answer rather than
//! an error.
//!
//! Every rule below was measured against iverilog 12.0 rather than read off the
//! LRM, and the conversions are deliberately **not** `scan.rs`'s: a value that
//! does not convert is `x` and the call still answers `1`, where a `$sscanf`
//! stops and answers how far it got.

use crate::register::{Register, ONE, X, Z, ZERO};

/// Whether any plus-arg *starts with* `prefix` — `$test$plusargs`.
///
/// Prefix, not equality: `+option=1` answers `$test$plusargs("opt")`, and the
/// empty string answers `1` whenever there is any plus-arg at all (both
/// measured against iverilog 12.0). Matching is case sensitive.
pub fn test(plusargs: &[String], prefix: &str) -> bool {
    plusargs.iter().any(|arg| arg.starts_with(prefix))
}

/// What one conversion in a `$value$plusargs` format string asks for.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Conversion {
    /// `%d` — an optional sign and decimal digits, or a lone `x` or `z`.
    Decimal,
    /// `%b`, `%o`, `%h`/`%x` — digits of the base, `x` and `z` among them.
    Based(usize),
    /// `%e`, `%f`, `%g` — a C floating point number.
    Real,
    /// `%s` — the text's own bytes.
    Text,
}

/// The answer `$value$plusargs` gives: no match at all, or the value to write.
///
/// The two are different in a way a design acts on. `None` is `0` with the
/// target *untouched*, which is how `if (!$value$plusargs(…))` reports a
/// missing option; `Some` is `1`, even when the text made no sense — that is
/// `x`, and it is still an answer.
pub type Match = Option<Register>;

/// `$value$plusargs(format, target)`.
///
/// `format` is a prefix and one conversion — `"option=%h"`. The first plus-arg
/// starting with that prefix wins, and everything after the prefix is the value
/// text. `width` is the target's, which is what decides how much of a value
/// survives: `+neg=-1_00` read as `%h` into a `reg [7:0]` is `8'h00`.
///
/// A format with no `%` in it is an error naming it, which is what iverilog
/// does — there is nothing for such a call to have meant.
pub fn value(plusargs: &[String], format: &str, width: usize, real: bool) -> Result<Match, String> {
    let (prefix, conversion) = split_format(format)?;
    let Some(text) = plusargs
        .iter()
        .find_map(|arg| arg.strip_prefix(prefix))
        .map(str::to_string)
    else {
        return Ok(None);
    };
    // A real target has no width of its own, so the integer conversions are
    // carried out in sixty-four bits and converted where the value meets the
    // target, exactly as an ordinary assignment to a `real` is.
    let width = if real || width == 0 { 64 } else { width };
    Ok(Some(convert(conversion, &text, width)))
}

/// Splits `"option=%h"` into the prefix to match and the conversion to read.
///
/// The conversion letter is case insensitive (`%H` is `%h`), and anything after
/// it is ignored — everything past the prefix is the value, so a format has
/// nowhere to put a trailing literal.
fn split_format(format: &str) -> Result<(&str, Conversion), String> {
    let at = format.find('%').ok_or_else(|| {
        format!(
            "`$value$plusargs` was given the format `{}`, which has no `%` conversion in it",
            format
        )
    })?;
    let letter = format[at + 1..].chars().next().ok_or_else(|| {
        format!(
            "`$value$plusargs` was given the format `{}`, whose `%` names no conversion",
            format
        )
    })?;
    let conversion = match letter.to_ascii_lowercase() {
        'd' => Some(Conversion::Decimal),
        'b' => Some(Conversion::Based(1)),
        'o' => Some(Conversion::Based(3)),
        'h' | 'x' => Some(Conversion::Based(4)),
        'e' | 'f' | 'g' => Some(Conversion::Real),
        's' => Some(Conversion::Text),
        _ => None,
    };
    let conversion = conversion.ok_or_else(|| {
        format!(
            "`$value$plusargs` was given the format `{}`, and `%{}` is not one it reads",
            format, letter
        )
    })?;
    Ok((&format[..at], conversion))
}

/// The value text, read as `conversion` asks and fitted to `width`.
///
/// Text that does not convert is `x` at the target's width rather than an
/// error, because the call still answers `1` — iverilog prints a warning naming
/// the offending text and carries on, and the warning is the only part of that
/// visilog cannot reproduce (it names the source line, which nothing here
/// records). An **empty** value converts to `0` and draws no warning, which is
/// deliberate on iverilog's side and is why it is not folded in with the
/// invalid case.
fn convert(conversion: Conversion, text: &str, width: usize) -> Register {
    match conversion {
        Conversion::Text => text_bits(text, width),
        // A real has no `x`, so text that does not convert is `0.0`.
        Conversion::Real => Register::from_f64(real_value(text).unwrap_or(0.0)),
        Conversion::Decimal => decimal(text, width).unwrap_or_else(|| Register::unknown(width)),
        Conversion::Based(bits) => based(text, bits, width).unwrap_or_else(|| {
            // A lone `-` or a digit of the wrong base is unknown, the same way
            // a decimal that will not parse is.
            Register::unknown(width)
        }),
    }
}

/// `%d`: an optional sign then decimal digits, `_` anywhere among them, or a
/// lone `x` or `z` standing for the whole value.
///
/// Unlike `scan.rs`'s `%d`, `?` is **not** one of those (iverilog warns and
/// answers `x`), and an underscore may lead — `+dec=123456789_` and
/// `+neg=-1_00` are both corpus plus-args.
fn decimal(text: &str, width: usize) -> Option<Register> {
    let digits: String = text.chars().filter(|c| *c != '_').collect();
    if digits.is_empty() {
        return Some(Register::zeros(width));
    }
    match digits.as_str() {
        "x" | "X" => return Some(Register::unknown(width)),
        "z" | "Z" => return Some(Register::high_impedance(width)),
        _ => {}
    }
    let (negative, body) = match digits.strip_prefix('-') {
        Some(body) => (true, body),
        None => (false, digits.strip_prefix('+').unwrap_or(&digits)),
    };
    if body.is_empty() || !body.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    // A value too wide for the arithmetic is refused rather than wrapped: the
    // answer would be a plausible number that is not the one the plus-arg said.
    let magnitude = body.parse::<u128>().ok()?;
    Some(signed_bits(magnitude, negative, width))
}

/// `%b` / `%o` / `%h` / `%x`: the digits of the base with `x` and `z` among
/// them, `_` ignored, and an optional leading sign that negates the whole.
///
/// A sign and an unknown digit together have no meaning — there is nothing to
/// negate — so that combination is refused and reads as `x`.
fn based(text: &str, bits: usize, width: usize) -> Option<Register> {
    let digits: String = text.chars().filter(|c| *c != '_').collect();
    if digits.is_empty() {
        return Some(Register::zeros(width));
    }
    let (negative, body) = match digits.strip_prefix('-') {
        Some(body) => (true, body),
        None => (false, digits.strip_prefix('+').unwrap_or(&digits)),
    };
    if body.is_empty() {
        return None;
    }
    let radix = 1u32 << bits;
    let mut codes: Vec<u8> = Vec::with_capacity(body.len() * bits);
    let mut magnitude: u128 = 0;
    let mut unknown = false;
    for digit in body.chars() {
        let fill = match digit {
            'x' | 'X' => Some(X),
            'z' | 'Z' => Some(Z),
            _ => None,
        };
        match fill {
            Some(code) => {
                unknown = true;
                codes.extend(std::iter::repeat_n(code, bits));
            }
            None => {
                let value = digit.to_digit(radix)?;
                magnitude = magnitude
                    .checked_mul(u128::from(radix))?
                    .checked_add(u128::from(value))?;
                codes.extend((0..bits).rev().map(
                    |at| {
                        if (value >> at) & 1 == 1 {
                            ONE
                        } else {
                            ZERO
                        }
                    },
                ));
            }
        }
    }
    if negative {
        return (!unknown).then(|| signed_bits(magnitude, true, width));
    }
    Some(Register::from_bits(codes).resize(width))
}

/// `%e` / `%f` / `%g`: C's floating point number, with whatever follows it
/// ignored — `+warn_real=9.825units` is `9.825` (iverilog warns and takes it).
fn real_value(text: &str) -> Option<f64> {
    let bytes = text.as_bytes();
    // The longest prefix that parses wins, which is how a trailing unit is
    // dropped without a grammar of its own. The whole string is tried first,
    // so `inf` and `-23.456e+3` cost one parse.
    (1..=bytes.len())
        .rev()
        .find_map(|end| text.get(..end).and_then(|head| head.parse::<f64>().ok()))
}

/// `%s`: the text's own bytes, eight bits a character, at the low end of the
/// target. A target too narrow keeps the **last** characters, which is the rule
/// every other string assignment here follows.
fn text_bits(text: &str, width: usize) -> Register {
    let codes: Vec<u8> = text
        .bytes()
        .flat_map(|byte| (0..8).rev().map(move |at| (byte >> at) & 1))
        .collect();
    if codes.is_empty() {
        return Register::zeros(width);
    }
    Register::from_bits(codes).resize(width)
}

/// A magnitude and a sign, fitted to `width`.
///
/// The negation happens in the full 128 bits and the result is *sign extended*
/// rather than zero padded, so a target wider than the arithmetic still reads
/// the negative number: `+neg=-1_00` into a `reg [71:0]` is
/// `72'hfffffffffffffff9c`.
fn signed_bits(magnitude: u128, negative: bool, width: usize) -> Register {
    let value = if negative {
        magnitude.wrapping_neg()
    } else {
        magnitude
    };
    Register::from_u128(value, 128)
        .with_signedness(negative)
        .coerced(width)
        .with_signedness(negative)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn args() -> Vec<String> {
        [
            "option=0123456789abcdef",
            "hex=123456789abcdef_x_z",
            "oct=1234567_x_z",
            "bin=101_x_z",
            "dec=123456789_",
            "neg=-1_00",
            "bad_num=not_a_num",
            "real=12.3456789",
            "neg_real=-23.456e+3",
            "real_inf=Inf",
            "warn_real=9.825units",
            "empty=",
        ]
        .iter()
        .map(|arg| arg.to_string())
        .collect()
    }

    fn read(format: &str, width: usize) -> Register {
        value(&args(), format, width, false)
            .expect("the format has a conversion")
            .expect("the plus-arg is there")
    }

    /// `$test$plusargs` is a *prefix* match over the whole plus-arg, so `+opt`
    /// answers both `opt` and nothing longer than itself. iverilog 12.0 with
    /// `+option=…` answers 1 for `"opt"`, 1 for `"option"` and 0 for
    /// `"options"`, and answers 1 for `""` because some plus-arg exists.
    #[test]
    fn test_plusargs_matches_a_prefix() {
        assert!(test(&args(), "opt"));
        assert!(test(&args(), "option"));
        assert!(!test(&args(), "options"));
        assert!(!test(&args(), "nothing"));
        assert!(test(&args(), ""));
        assert!(!test(&[], ""));
    }

    /// A plus-arg nothing matches is `None` — `0`, with the target left alone —
    /// which is what a design tests for. It is deliberately not the same answer
    /// as a value that would not convert.
    #[test]
    fn test_a_missing_plusarg_leaves_the_target_alone() {
        assert_eq!(
            value(&args(), "missing=%h", 32, false).expect("the format is fine"),
            None
        );
    }

    /// The based conversions, each against the line iverilog 12.0 prints for
    /// `$display("%h", v)` after the same call on a `reg [71:0]`. `x` and `z`
    /// are digits of the base and `_` is a separator anywhere, including
    /// leading.
    #[test]
    fn test_based_conversions_keep_unknown_digits() {
        // `123456789abcdef_x_z` is seventeen hex digits, so sixty-eight bits
        // zero extended to seventy-two.
        let value = read("hex=%h", 72).to_binary();
        assert_eq!(value.len(), 72);
        assert_eq!(&value[..4], "0000");
        assert!(value.ends_with("11101111xxxxzzzz"), "{}", value);
        // `1234567_x_z` is nine octal digits, so twenty-seven bits.
        assert!(read("oct=%o", 72)
            .to_binary()
            .ends_with("001010011100101110111xxxzzz"));
        assert!(read("bin=%b", 72).to_binary().ends_with("000101xz"));
        // `%x` is `%h`, and the conversion letter's case does not matter.
        assert_eq!(read("hex=%x", 72), read("hex=%H", 72));
    }

    /// A sign negates the whole value in whatever base it was written, and the
    /// answer is *sign extended* to the target. Measured against iverilog 12.0
    /// with `+neg=-1_00`: `%d` is -100, `%b` is -4, `%o` is -64, and `%h` into
    /// an eight bit target is `8'h00` because -256 has no low bits.
    #[test]
    fn test_a_sign_negates_in_the_base_it_was_written() {
        assert_eq!(read("neg=%d", 72).to_i128(), Some(-100));
        assert_eq!(read("neg=%b", 72).to_i128(), Some(-4));
        assert_eq!(read("neg=%o", 72).to_i128(), Some(-64));
        assert_eq!(read("neg=%h", 8).to_binary(), "00000000");
    }

    /// Text that does not convert is `x` and the call still answers `1`, which
    /// is why this is not `scan.rs`: a scan stops and reports how far it got,
    /// where a plus-arg is a value the design was *given* and reading it is not
    /// optional. An **empty** value is `0` instead, and iverilog draws no
    /// warning for that one.
    #[test]
    fn test_an_unreadable_value_is_unknown_and_an_empty_one_is_zero() {
        assert!(read("bad_num=%d", 8).to_binary().chars().all(|c| c == 'x'));
        assert!(read("bad_num=%h", 8).to_binary().chars().all(|c| c == 'x'));
        assert_eq!(read("empty=%d", 8).to_binary(), "00000000");
        assert_eq!(read("empty=%s", 8).to_binary(), "00000000");
        // A real has no `x` to be, so it is `0.0`.
        assert_eq!(read("bad_num=%f", 0).to_f64(), 0.0);
    }

    /// `%d` reads a lone `x` or `z` as the whole value, at the target's width.
    /// `?` is **not** one of them here, where `$sscanf`'s `%d` takes it —
    /// iverilog warns and answers `x` for `+dq=?`.
    #[test]
    fn test_a_lone_unknown_is_the_whole_decimal_value() {
        let args = ["dx=x".to_string(), "dz=z_".to_string(), "dq=?".to_string()];
        let read = |format: &str| {
            value(&args, format, 8, false)
                .expect("the format is fine")
                .expect("the plus-arg is there")
                .to_binary()
        };
        assert_eq!(read("dx=%d"), "xxxxxxxx");
        assert_eq!(read("dz=%d"), "zzzzzzzz");
        assert_eq!(read("dq=%d"), "xxxxxxxx");
    }

    /// The real conversions are C's, and a trailing unit is dropped rather than
    /// failing the whole read — iverilog prints
    /// `Extra character(s) "units" found` and takes the 9.825.
    #[test]
    fn test_real_conversions_take_the_longest_number_they_find() {
        assert!((read("real=%f", 0).to_f64() - 12.3456789).abs() < 1e-9);
        assert!((read("neg_real=%f", 0).to_f64() + 23456.0).abs() < 1e-9);
        assert!(read("real_inf=%f", 0).to_f64().is_infinite());
        assert!((read("warn_real=%f", 0).to_f64() - 9.825).abs() < 1e-9);
        // `%e` and `%g` read the same number `%f` does.
        assert_eq!(read("real=%e", 0).to_f64(), read("real=%g", 0).to_f64());
    }

    /// `%s` is the value's own bytes at the low end of the target, so a target
    /// too narrow keeps the **last** characters. iverilog 12.0 with
    /// `+str=hello_world` into a `reg [79:0]` prints `ello_world`.
    #[test]
    fn test_a_string_lands_at_the_low_end_of_its_target() {
        let args = ["str=hello_world".to_string()];
        let value = value(&args, "str=%s", 80, false)
            .expect("the format is fine")
            .expect("the plus-arg is there");
        let text: String = (0..10)
            .rev()
            .map(|byte| {
                let bits: String = (0..8)
                    .rev()
                    .map(|bit| match value.bit_from_lsb(byte * 8 + bit) {
                        Some(1) => '1',
                        _ => '0',
                    })
                    .collect();
                u8::from_str_radix(&bits, 2).expect("eight bits") as char
            })
            .collect();
        assert_eq!(text, "ello_world");
    }

    /// The first plus-arg that matches wins, the way iverilog's own scan of the
    /// list does: `+dup=11 +dup=22` reads 11.
    #[test]
    fn test_the_first_matching_plusarg_wins() {
        let args = ["dup=11".to_string(), "dup=22".to_string()];
        let read = value(&args, "dup=%d", 8, false)
            .expect("the format is fine")
            .expect("the plus-arg is there");
        assert_eq!(read.to_i128(), Some(11));
    }

    /// A format with no conversion in it is an error naming it, which is what
    /// iverilog reports — there is nothing such a call could have meant, and
    /// answering `0` would look exactly like a plus-arg that was not given.
    #[test]
    fn test_a_format_with_no_conversion_is_a_named_error() {
        let error = value(&args(), "option", 8, false).expect_err("no conversion");
        assert!(error.contains("option"), "{}", error);
        let error = value(&args(), "option=%q", 8, false).expect_err("no such conversion");
        assert!(error.contains("%q"), "{}", error);
    }
}

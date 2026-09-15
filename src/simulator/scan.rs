//! The reading half of a format string: `$sscanf` and `$fscanf`.
//!
//! A scan is the mirror of [`tasks`](crate::simulator::tasks)'s `render`, and
//! it is a different shape for one reason: it **writes through its argument
//! list**. `$sscanf(s, "%d %d", a, b)` hands back how many values it converted
//! and puts the values themselves in `a` and `b`, so it is a system *function*
//! with side effects on the store — see
//! [`StateStore::owe_fill`](crate::simulator::state_store::StateStore::owe_fill)
//! for how those reach the design.
//!
//! Everything here was measured against iverilog 12.0 rather than read off the
//! LRM, because the two disagree in the corners that the corpus tests:
//!
//! * **The return value is `-1` only when there was nothing at all to read.**
//!   `$sscanf("", "%d", a)` is `-1` and `$sscanf("  ", "%d", a)` is `0` — the
//!   check is made once, before the format is walked, so anything that got as
//!   far as skipping whitespace reports a conversion count instead. Telling
//!   those two apart is the whole point: `0` means "the input did not match"
//!   and `-1` means "there is no more input", and a design loops on the
//!   difference.
//! * **A failing conversion stops the scan** and the count so far is the
//!   answer. Values converted before it are written; the rest of the arguments
//!   are left exactly as they were.
//! * **Whitespace in the format matches a run of whitespace or none**, and any
//!   other character in the format must match exactly — with no whitespace
//!   skipped in front of it, so `","` does not match `"  ,"`.
//! * **Every conversion but `%c` skips leading whitespace.**
//! * `x`, `X`, `z`, `Z` and `?` are digits: for `%b`, `%o` and `%h` they are
//!   unknown digits *within* a run, and for `%d` one of them standing alone is
//!   the whole value. `_` separates digits but may not lead them.
//! * A **width** (`%5s`, `%4d`) counts the characters the conversion itself
//!   consumes, not the whitespace skipped in front of it, and a `*` suppresses
//!   the conversion: it still has to succeed, and it still consumes its input,
//!   but it takes no argument and does not count.

use std::io::SeekFrom;

use crate::register::{Register, ONE, X, Z};
use crate::simulator::exec::ResolvedTarget;
use crate::simulator::state_store::Reader;

/// Where one conversion's value goes.
///
/// The width is the target's, resolved before the scan starts, because two
/// conversions need it to decide what to *read*: `%d` of an unknown produces a
/// vector of `x` as wide as the thing it fills.
#[derive(Clone, Debug)]
pub struct Slot {
    pub target: ResolvedTarget,
    pub width: usize,
}

/// The characters a scan reads, one at a time and one of look-ahead.
///
/// One byte of push-back is all a scan ever needs — every conversion stops at
/// the first character it does not want — and it is also exactly what `$ungetc`
/// promises, so the file side of this is the same slot both use.
pub trait Source {
    /// The next byte, left where it is. `None` is end of input.
    fn peek(&mut self) -> Option<u8>;
    /// Consumes the byte [`peek`](Source::peek) looked at.
    fn bump(&mut self);
}

/// A `$sscanf` source: a string, held as the bytes still to be read.
pub struct Text<'a> {
    bytes: &'a [u8],
}

impl<'a> Text<'a> {
    pub fn new(text: &'a str) -> Self {
        Text {
            bytes: text.as_bytes(),
        }
    }
}

impl Source for Text<'_> {
    fn peek(&mut self) -> Option<u8> {
        self.bytes.first().copied()
    }

    fn bump(&mut self) {
        self.bytes = &self.bytes[1..];
    }
}

impl Source for Reader {
    fn peek(&mut self) -> Option<u8> {
        Reader::peek(self)
    }

    fn bump(&mut self) {
        Reader::bump(self)
    }
}

/// What a completed scan returns: `-1` when there was nothing to read at all,
/// otherwise how many arguments were filled in.
pub const END_OF_FILE: i64 = -1;

/// Runs `format` against `input`, filling `slots` in order.
///
/// The writes are handed back rather than made, because the caller is
/// [`eval`](crate::simulator::eval::eval) and it holds a shared reference to
/// the store. An `Err` is a conversion this simulator does not implement or a
/// format that asks for more arguments than it was given — never a silently
/// wrong count, because a `0` is a legitimate answer that a design acts on.
pub fn scan(
    input: &mut dyn Source,
    format: &str,
    slots: &[Slot],
) -> Result<(i64, Vec<(ResolvedTarget, Register)>), String> {
    let mut fills = Vec::new();
    // Asked once, before anything is consumed: "there was nothing to read" is
    // the only thing that reports end of file, and every later failure reports
    // the count instead.
    if input.peek().is_none() {
        return Ok((END_OF_FILE, fills));
    }

    let mut converted = 0i64;
    let mut next_slot = 0usize;
    let mut characters = format.chars().peekable();
    while let Some(character) = characters.next() {
        if character.is_ascii_whitespace() {
            skip_whitespace(input);
            continue;
        }
        if character != '%' {
            if !literal(input, character) {
                return Ok((converted, fills));
            }
            continue;
        }

        let suppress = characters.peek() == Some(&'*');
        if suppress {
            characters.next();
        }
        let mut digits = String::new();
        while characters.peek().is_some_and(char::is_ascii_digit) {
            digits.push(characters.next().expect("peeked digit must exist"));
        }
        let width: Option<usize> = match digits.parse() {
            Ok(width) => Some(width),
            Err(_) => None,
        };
        let specifier = characters
            .next()
            .ok_or_else(|| "a trailing `%` with no conversion after it".to_string())?;

        // `%%` matches a literal percent sign and takes no argument, exactly
        // as it produces one on the printing side.
        if specifier == '%' {
            if !literal(input, '%') {
                return Ok((converted, fills));
            }
            continue;
        }

        // A suppressed conversion is carried out and thrown away, so it needs
        // a width to read into but no target. Sixty-four bits is wide enough
        // for anything that would then be discarded.
        let slot = if suppress {
            None
        } else {
            let slot = slots
                .get(next_slot)
                .ok_or_else(|| format!("`%{}` has no argument left to read into", specifier))?;
            next_slot += 1;
            Some(slot)
        };
        let bits = slot.map_or(64, |slot| slot.width.max(1));

        match convert(input, specifier, width, bits)? {
            None => return Ok((converted, fills)),
            Some(value) => {
                if let Some(slot) = slot {
                    fills.push((slot.target.clone(), value));
                    converted += 1;
                }
            }
        }
    }
    Ok((converted, fills))
}

/// Matches one ordinary character of the format. No whitespace is skipped in
/// front of it — that is C's rule and iverilog's, so `","` against `"  ,12"`
/// does not match.
fn literal(input: &mut dyn Source, character: char) -> bool {
    if input.peek() != Some(character as u8) {
        return false;
    }
    input.bump();
    true
}

fn skip_whitespace(input: &mut dyn Source) {
    while input.peek().is_some_and(|byte| byte.is_ascii_whitespace()) {
        input.bump();
    }
}

/// Carries out one conversion. `Ok(None)` is a conversion that did not match,
/// which stops the scan; `Err` is one this simulator cannot carry out at all.
fn convert(
    input: &mut dyn Source,
    specifier: char,
    width: Option<usize>,
    bits: usize,
) -> Result<Option<Register>, String> {
    // Every conversion but `%c` skips whitespace in front of itself, and the
    // characters it skips do not count against an explicit width.
    if !specifier.eq_ignore_ascii_case(&'c') {
        skip_whitespace(input);
    }
    match specifier.to_ascii_lowercase() {
        'b' => Ok(based(input, width, 1)),
        'o' => Ok(based(input, width, 3)),
        'h' | 'x' => Ok(based(input, width, 4)),
        'd' => Ok(decimal(input, width, bits)),
        'c' => Ok(character(input)),
        's' => Ok(string(input, width)),
        'f' | 'e' | 'g' => Ok(real(input, width)),
        // Each of these is a name rather than a silent zero. `%t` needs the
        // `$timeformat` precision, which lives on the `TaskContext` that `eval`
        // is not handed, and the timescale this simulator does not model;
        // `%u` and `%z` are raw binary, which the text-shaped output buffer
        // cannot carry; `%m` is the scope of the call, which an expression
        // does not know.
        't' => Err("`%t` is not a conversion this simulator can scan".to_string()),
        'u' | 'z' => Err(format!(
            "`%{}` reads raw binary, which this simulator does not scan",
            specifier
        )),
        'm' => Err("`%m` is not a conversion this simulator can scan".to_string()),
        _ => Err(format!(
            "`%{}` is not a conversion this simulator understands",
            specifier
        )),
    }
}

/// How many characters a conversion may still consume.
fn allowance(width: Option<usize>, taken: usize) -> bool {
    width.is_none_or(|width| taken < width)
}

/// `%b`, `%o` and `%h` — a run of digits in a base that is a power of two, so
/// each digit is a fixed number of bits and an unknown one makes all of them
/// unknown. `_` separates digits and may not lead them, which is what makes
/// `"_01"` no match at all rather than the value `1`.
fn based(input: &mut dyn Source, width: Option<usize>, per_digit: usize) -> Option<Register> {
    let mut codes: Vec<u8> = Vec::new();
    let mut taken = 0usize;
    while let Some(byte) = input.peek() {
        if !allowance(width, taken) {
            break;
        }
        // A separator between digits is skipped; one before the first is not a
        // digit at all, so the run has not started and the conversion fails.
        if byte == b'_' {
            if codes.is_empty() {
                break;
            }
            input.bump();
            taken += 1;
            continue;
        }
        let Some(digit) = digit_codes(byte, per_digit) else {
            break;
        };
        codes.extend_from_slice(&digit);
        input.bump();
        taken += 1;
    }
    (!codes.is_empty()).then(|| Register::from_bits(codes))
}

/// One digit of a power-of-two base as `per_digit` bit codes, most significant
/// first. `x`, `z` and `?` are digits in their own right: an unknown digit is
/// unknown in every one of its bits.
fn digit_codes(byte: u8, per_digit: usize) -> Option<Vec<u8>> {
    let unknown = match byte {
        b'x' | b'X' | b'?' => Some(X),
        b'z' | b'Z' => Some(Z),
        _ => None,
    };
    if let Some(code) = unknown {
        return Some(vec![code; per_digit]);
    }
    let value = match byte {
        b'0'..=b'9' => u32::from(byte - b'0'),
        b'a'..=b'f' => u32::from(byte - b'a') + 10,
        b'A'..=b'F' => u32::from(byte - b'A') + 10,
        _ => return None,
    };
    if value >= 1 << per_digit {
        return None;
    }
    Some(
        (0..per_digit)
            .rev()
            .map(|offset| if value >> offset & 1 == 1 { ONE } else { 0 })
            .collect(),
    )
}

/// `%d` — an optional sign then either a run of decimal digits or a single
/// unknown character standing for the whole value. A sign with nothing usable
/// after it is no match, which is what makes `"-q"` fail rather than read `0`.
fn decimal(input: &mut dyn Source, width: Option<usize>, bits: usize) -> Option<Register> {
    let mut taken = 0usize;
    let negative = match input.peek() {
        Some(sign @ (b'+' | b'-')) if allowance(width, taken) => {
            input.bump();
            taken += 1;
            sign == b'-'
        }
        _ => false,
    };

    // `x`, `z` and `?` are the whole value here rather than one digit of it:
    // `$sscanf("x", "%d", a)` fills `a` with `x` across its declared width.
    if allowance(width, taken) {
        match input.peek() {
            Some(b'x' | b'X' | b'?') => {
                input.bump();
                return Some(Register::unknown(bits));
            }
            Some(b'z' | b'Z') => {
                input.bump();
                return Some(Register::high_impedance(bits));
            }
            _ => {}
        }
    }

    let mut value: i128 = 0;
    let mut digits = 0usize;
    while let Some(byte) = input.peek() {
        if !allowance(width, taken) {
            break;
        }
        if byte == b'_' {
            if digits == 0 {
                break;
            }
            input.bump();
            taken += 1;
            continue;
        }
        if !byte.is_ascii_digit() {
            break;
        }
        value = value
            .saturating_mul(10)
            .saturating_add(i128::from(byte - b'0'));
        input.bump();
        taken += 1;
        digits += 1;
    }
    if digits == 0 {
        return None;
    }
    if negative {
        value = -value;
    }
    // Signed, and a hundred and twenty-eight bits wide: the target's own width
    // then truncates or sign extends it where the write lands, which is the
    // same path every other assignment takes.
    Some(Register::from_u128(value as u128, 128).with_signedness(true))
}

/// `%c` — exactly one character, and the one conversion that does *not* skip
/// whitespace in front of itself.
fn character(input: &mut dyn Source) -> Option<Register> {
    let byte = input.peek()?;
    input.bump();
    Some(Register::from_u128(u128::from(byte), 8))
}

/// `%s` — a run of non-whitespace, as one eight bit character per byte. The
/// target's width does the rest: a wider one is zero extended, so the text
/// lands at its least significant end exactly as `$sformat` puts it there.
fn string(input: &mut dyn Source, width: Option<usize>) -> Option<Register> {
    let mut bytes: Vec<u8> = Vec::new();
    while let Some(byte) = input.peek() {
        if byte.is_ascii_whitespace() || !allowance(width, bytes.len()) {
            break;
        }
        bytes.push(byte);
        input.bump();
    }
    if bytes.is_empty() {
        return None;
    }
    let codes = bytes
        .iter()
        .flat_map(|byte| (0..8).rev().map(move |offset| (byte >> offset) & 1))
        .collect::<Vec<u8>>();
    Some(Register::from_bits(codes))
}

/// `%f`, `%e` and `%g` — a C floating point number. All three read the same
/// thing, which is what C says and what iverilog does.
///
/// Two departures from `strtod` are iverilog's and are what the corpus tests:
/// a trailing decimal point is allowed (`"2."` is 2.0), and an `e` that is not
/// followed by a signed run of digits fails the *whole* conversion rather than
/// backing up to the mantissa — `"2.ea"` is no match at all.
fn real(input: &mut dyn Source, width: Option<usize>) -> Option<Register> {
    let mut text = String::new();

    if matches!(input.peek(), Some(b'+' | b'-')) && allowance(width, text.len()) {
        take(input, &mut text);
    }
    let mut mantissa = take_digits(input, width, &mut text);
    if input.peek() == Some(b'.') && allowance(width, text.len()) {
        take(input, &mut text);
        mantissa += take_digits(input, width, &mut text);
    }
    if mantissa == 0 {
        return None;
    }
    if matches!(input.peek(), Some(b'e' | b'E')) && allowance(width, text.len()) {
        take(input, &mut text);
        if matches!(input.peek(), Some(b'+' | b'-')) && allowance(width, text.len()) {
            take(input, &mut text);
        }
        if take_digits(input, width, &mut text) == 0 {
            return None;
        }
    }
    // Rust's own parser takes every spelling that gets here — `"2."`, `".2"`,
    // `"2.e1"` and a leading `+` alike — and answers `"1e5000"` with an
    // infinity, which is what C's `strtod` reports and what corpus `scanf4`
    // asserts.
    text.parse::<f64>().ok().map(Register::from_f64)
}

/// Moves one character from the input onto the text being gathered.
fn take(input: &mut dyn Source, text: &mut String) {
    if let Some(byte) = input.peek() {
        text.push(char::from(byte));
        input.bump();
    }
}

/// Moves a run of decimal digits across, and reports how many.
fn take_digits(input: &mut dyn Source, width: Option<usize>, text: &mut String) -> usize {
    let mut digits = 0;
    while input.peek().is_some_and(|byte| byte.is_ascii_digit()) && allowance(width, text.len()) {
        take(input, text);
        digits += 1;
    }
    digits
}

/// Where a `$fseek` counts its offset from: the three C `SEEK_*` constants, in
/// the order the LRM gives them.
pub fn seek_from(operation: i64, offset: i64) -> Option<SeekFrom> {
    match operation {
        0 => Some(SeekFrom::Start(u64::try_from(offset).ok()?)),
        1 => Some(SeekFrom::Current(offset)),
        2 => Some(SeekFrom::End(offset)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn slots(widths: &[usize]) -> Vec<Slot> {
        widths
            .iter()
            .enumerate()
            .map(|(index, width)| Slot {
                target: ResolvedTarget::Whole(format!("s{}", index)),
                width: *width,
            })
            .collect()
    }

    fn run(text: &str, format: &str, widths: &[usize]) -> (i64, Vec<Register>) {
        let slots = slots(widths);
        let (count, fills) = scan(&mut Text::new(text), format, &slots).expect("scan");
        (count, fills.into_iter().map(|(_, value)| value).collect())
    }

    #[test]
    fn test_decimal_and_radix_conversions() {
        let (count, values) = run("123 hex beaf bin 01xz", "%d hex %h bin %b", &[32, 32, 32]);
        assert_eq!(count, 3);
        assert_eq!(values[0].to_i128(), Some(123));
        assert_eq!(values[1].to_binary(), "1011111010101111");
        assert_eq!(values[2].to_binary(), "01xz");
    }

    #[test]
    fn test_whitespace_matches_a_run_or_none() {
        assert_eq!(run("12    34", "%d %d", &[32, 32]).0, 2);
        assert_eq!(run("12,34", "%d,%d", &[32, 32]).0, 2);
        assert_eq!(run("   12", "%d", &[32]).0, 1);
    }

    #[test]
    fn test_a_literal_must_match_exactly_and_skips_nothing() {
        // The `,` in the format does not skip the spaces in front of it.
        assert_eq!(run("  ,12", ",%d", &[32]).0, 0);
        assert_eq!(run("12;34", "%d,%d", &[32, 32]).0, 1);
    }

    #[test]
    fn test_a_partial_match_stops_and_reports_what_it_converted() {
        // `%s` takes the `a`, the format's space skips the trailing one, and
        // `%c` finds nothing — so one conversion, and the second argument is
        // never written.
        let (count, values) = run("a ", "%s %c", &[64, 32]);
        assert_eq!(count, 1);
        assert_eq!(values.len(), 1);
    }

    #[test]
    fn test_nothing_to_read_is_end_of_file_and_no_match_is_zero() {
        assert_eq!(run("", "%d", &[32]).0, END_OF_FILE);
        assert_eq!(run("", "GOOD %s", &[64]).0, END_OF_FILE);
        // Whitespace is something, so this got as far as failing to convert.
        assert_eq!(run("  ", "%d", &[32]).0, 0);
        assert_eq!(run("ab", "%d", &[32]).0, 0);
    }

    #[test]
    fn test_unknown_digits() {
        let (count, values) = run("01_?xzXZ", "%b", &[64]);
        assert_eq!(count, 1);
        assert_eq!(values[0].to_binary(), "01xxzxz");
        // A leading separator is not a digit, so the run never starts.
        assert_eq!(run("_0110", "%b", &[64]).0, 0);
        // For `%d` a lone unknown is the whole value, at the target's width.
        let (count, values) = run("z", "%d", &[8]);
        assert_eq!(count, 1);
        assert_eq!(values[0].to_binary(), "zzzzzzzz");
    }

    #[test]
    fn test_signs_need_a_digit_after_them() {
        assert_eq!(
            run("-01234_56789", "%d", &[64]).1[0].to_i128(),
            Some(-123456789)
        );
        assert_eq!(run("-q", "%d", &[64]).0, 0);
        assert_eq!(run("+q", "%f", &[64]).0, 0);
    }

    #[test]
    fn test_real_conversions() {
        assert_eq!(run("8.125", "%f", &[64]).1[0].to_f64(), 8.125);
        assert_eq!(run("1e1", "%f", &[64]).1[0].to_f64(), 10.0);
        assert_eq!(run("2.", "%e", &[64]).1[0].to_f64(), 2.0);
        assert_eq!(run(".2", "%g", &[64]).1[0].to_f64(), 0.2);
        assert_eq!(run("2.e-1", "%f", &[64]).1[0].to_f64(), 0.2);
        assert!(run("1e5000", "%f", &[64]).1[0].to_f64().is_infinite());
        // An exponent with nothing after it fails the whole conversion.
        assert_eq!(run("2.ea", "%f", &[64]).0, 0);
    }

    #[test]
    fn test_widths_and_suppression() {
        let (count, values) = run("helloworld", "%5s %s", &[64, 64]);
        assert_eq!(count, 2);
        assert_eq!(values.len(), 2);
        let (count, values) = run("2345 x 6789", "%d %*d %d", &[64, 64]);
        assert_eq!(count, 2);
        assert_eq!(values[0].to_i128(), Some(2345));
        assert_eq!(values[1].to_i128(), Some(6789));
    }

    #[test]
    fn test_a_percent_is_a_literal_percent() {
        assert_eq!(run("test% str", "test%% %s", &[64]).0, 1);
    }

    #[test]
    fn test_an_unimplemented_conversion_is_named() {
        let error = scan(&mut Text::new("1"), "%t", &slots(&[64])).expect_err("named error");
        assert!(error.contains("%t"), "{}", error);
        let error = scan(&mut Text::new("1"), "%q", &slots(&[64])).expect_err("named error");
        assert!(error.contains("%q"), "{}", error);
    }

    #[test]
    fn test_running_out_of_arguments_is_named() {
        let error = scan(&mut Text::new("1 2"), "%d %d", &slots(&[32])).expect_err("named error");
        assert!(error.contains("no argument left"), "{}", error);
    }
}

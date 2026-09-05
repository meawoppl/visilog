use std::fmt;
use std::ops::{Deref, DerefMut};

/// Logic `0`.
pub const ZERO: u8 = 0;
/// Logic `1`.
pub const ONE: u8 = 1;
/// Unknown / uninitialized (`x`).
pub const X: u8 = 2;
/// High impedance (`z`).
pub const Z: u8 = 3;

/// Bits carried by one chunk of a bit plane.
const CHUNK_BITS: usize = 128;

/// One `CHUNK_BITS` wide slice of a register's two bit planes, least
/// significant bit first.
///
/// A four-state bit is a `(value, unknown)` pair: `0` is `(0, 0)`, `1` is
/// `(1, 0)`, `x` is `(0, 1)` and `z` is `(1, 1)` — the same numbering as
/// [`ZERO`], [`ONE`], [`X`] and [`Z`], with `unknown` as the high bit. Holding
/// the two planes apart is what lets a whole word of bits be combined with a
/// couple of machine instructions instead of a loop and a match per bit.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Chunk {
    /// Set for every bit that is `1` or `z`.
    pub value: u128,
    /// Set for every bit that is `x` or `z`.
    pub unknown: u128,
}

impl Chunk {
    /// All bits logic `0`.
    pub const EMPTY: Chunk = Chunk {
        value: 0,
        unknown: 0,
    };

    /// The bits that are a known `1`.
    #[inline]
    pub fn ones(&self) -> u128 {
        self.value & !self.unknown
    }

    /// The bits that are a known `0`. Positions past the register's width read
    /// as known zeros here, so a caller that cares must mask the result.
    #[inline]
    pub fn zeros(&self) -> u128 {
        !self.value & !self.unknown
    }

    /// The bits that are `z`.
    #[inline]
    pub fn high_impedance(&self) -> u128 {
        self.value & self.unknown
    }
}

/// Storage for a register's bit planes. Anything up to [`CHUNK_BITS`] wide —
/// which is every register a typical design carries — lives inline, so the
/// common case neither allocates nor chases a pointer.
///
/// The choice is a function of the width alone, and bits at or above the width
/// are always zero in both planes, which keeps the derived `PartialEq` and
/// `Hash` exact.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Planes {
    Inline(Chunk),
    Spilled {
        value: Vec<u128>,
        unknown: Vec<u128>,
    },
}

/// A four-state (`0`/`1`/`x`/`z`) bit vector.
///
/// Bits are addressed most-significant first in the public API — the left-most
/// bit as written in Verilog source is bit `0` of [`Register::get_raw`] and the
/// first character of [`Register::to_binary`], and the least significant bit is
/// the last. Internally they are packed least-significant first into the two
/// planes of a [`Chunk`].
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Register {
    width: usize,
    planes: Planes,
}

/// The mask of the bits chunk `index` actually carries for a register of
/// `width` bits.
#[inline]
fn chunk_mask(width: usize, index: usize) -> u128 {
    let used = width.saturating_sub(index * CHUNK_BITS);
    if used >= CHUNK_BITS {
        u128::MAX
    } else {
        (1u128 << used) - 1
    }
}

/// Ors `bits` into `words` starting at bit `position`, spilling into the next
/// word when the placement straddles a chunk boundary.
fn place(words: &mut [u128], position: usize, bits: u128) {
    if bits == 0 {
        return;
    }
    let index = position / CHUNK_BITS;
    let shift = position % CHUNK_BITS;
    if index < words.len() {
        words[index] |= bits << shift;
    }
    if shift != 0 && index + 1 < words.len() {
        words[index + 1] |= bits >> (CHUNK_BITS - shift);
    }
}

impl Register {
    pub fn new(width: usize, values: Vec<u8>) -> Self {
        assert!(values.len() == width);
        assert!(
            values.iter().all(|&v| v <= Z),
            "Register bits must be one of 0, 1, x (2) or z (3)"
        );
        Register::pack(&values)
    }

    /// Builds a register directly from a most-significant-first bit vector.
    pub fn from_bits(values: impl Into<Vec<u8>>) -> Self {
        let values = values.into();
        Register::new(values.len(), values)
    }

    /// A register of `width` bits all set to `value`.
    pub fn filled(width: usize, value: u8) -> Self {
        assert!(
            value <= Z,
            "Register bits must be one of 0, 1, x (2) or z (3)"
        );
        let chunk = Chunk {
            value: if value & 1 != 0 { u128::MAX } else { 0 },
            unknown: if value & 2 != 0 { u128::MAX } else { 0 },
        };
        Register::from_chunks(width, |_| chunk)
    }

    /// A register of `width` bits all set to `0`.
    pub fn zeros(width: usize) -> Self {
        Register::filled(width, ZERO)
    }

    /// A register of `width` bits all set to `1`.
    pub fn ones(width: usize) -> Self {
        Register::filled(width, ONE)
    }

    /// A register of `width` bits all set to `x`.
    pub fn unknown(width: usize) -> Self {
        Register::filled(width, X)
    }

    /// A register of `width` bits all set to `z`.
    pub fn high_impedance(width: usize) -> Self {
        Register::filled(width, Z)
    }

    pub fn width(&self) -> usize {
        self.width
    }

    // -- bit planes --------------------------------------------------------

    /// How many chunks the register's bit planes span.
    #[inline]
    pub fn chunk_count(&self) -> usize {
        self.width.div_ceil(CHUNK_BITS)
    }

    /// Chunk `index` of the bit planes. Reading past the end yields
    /// [`Chunk::EMPTY`], which is the zero extension of the value.
    #[inline]
    pub fn chunk(&self, index: usize) -> Chunk {
        match &self.planes {
            Planes::Inline(chunk) => {
                if index == 0 {
                    *chunk
                } else {
                    Chunk::EMPTY
                }
            }
            Planes::Spilled { value, unknown } => Chunk {
                value: value.get(index).copied().unwrap_or(0),
                unknown: unknown.get(index).copied().unwrap_or(0),
            },
        }
    }

    /// Builds a `width` bit register from a function over its chunks. Bits past
    /// `width` are masked away, so the caller may leave junk in them.
    pub fn from_chunks(width: usize, mut chunk: impl FnMut(usize) -> Chunk) -> Self {
        if width <= CHUNK_BITS {
            let mask = chunk_mask(width, 0);
            let packed = if width == 0 { Chunk::EMPTY } else { chunk(0) };
            return Register {
                width,
                planes: Planes::Inline(Chunk {
                    value: packed.value & mask,
                    unknown: packed.unknown & mask,
                }),
            };
        }
        let count = width.div_ceil(CHUNK_BITS);
        let mut value = Vec::with_capacity(count);
        let mut unknown = Vec::with_capacity(count);
        for index in 0..count {
            let packed = chunk(index);
            let mask = chunk_mask(width, index);
            value.push(packed.value & mask);
            unknown.push(packed.unknown & mask);
        }
        Register {
            width,
            planes: Planes::Spilled { value, unknown },
        }
    }

    /// Rewrites every chunk, keeping the width.
    pub fn map_chunks(&self, mut f: impl FnMut(Chunk) -> Chunk) -> Self {
        Register::from_chunks(self.width, |index| f(self.chunk(index)))
    }

    /// Combines two registers chunk by chunk into a `width` bit result. Either
    /// operand may be narrower than `width`; the missing bits read as `0`,
    /// which is the zero extension Verilog applies to the narrower operand.
    pub fn zip_chunks(
        &self,
        other: &Self,
        width: usize,
        mut f: impl FnMut(Chunk, Chunk) -> Chunk,
    ) -> Self {
        Register::from_chunks(width, |index| f(self.chunk(index), other.chunk(index)))
    }

    /// The code of the bit at `index` counted from the least significant end.
    /// The caller guarantees `index < width`.
    #[inline]
    fn code_at(&self, index: usize) -> u8 {
        let chunk = self.chunk(index / CHUNK_BITS);
        let shift = index % CHUNK_BITS;
        (((chunk.value >> shift) & 1) | (((chunk.unknown >> shift) & 1) << 1)) as u8
    }

    /// Packs a most-significant-first bit vector into the two planes.
    fn pack(values: &[u8]) -> Self {
        let width = values.len();
        Register::from_chunks(width, |index| {
            let base = index * CHUNK_BITS;
            let mut chunk = Chunk::EMPTY;
            for offset in 0..CHUNK_BITS.min(width - base) {
                let code = values[width - 1 - base - offset];
                chunk.value |= ((code & 1) as u128) << offset;
                chunk.unknown |= (((code >> 1) & 1) as u128) << offset;
            }
            chunk
        })
    }

    /// The bits as one byte each, most significant first.
    fn bit_codes(&self) -> Vec<u8> {
        (0..self.width).rev().map(|i| self.code_at(i)).collect()
    }

    // -- queries -----------------------------------------------------------

    /// The bit at `index` counted from the least significant end.
    pub fn bit_from_lsb(&self, index: usize) -> Option<u8> {
        if index >= self.width {
            return None;
        }
        Some(self.code_at(index))
    }

    /// A copy with the bit `index` places from the least significant end set to
    /// `code` (one of `0`, `1`, `X`, `Z`). An `index` past the width is ignored,
    /// matching what Verilog does with an out-of-range select on the left of an
    /// assignment.
    ///
    /// This exists so a single-bit write does not have to expand the register
    /// into a byte per bit and rebuild it — the hot path in
    /// `simulator::state_store` writes one bit at a time.
    pub fn with_bit(&self, index: usize, code: u8) -> Self {
        assert!(
            code <= Z,
            "Register bits must be one of 0, 1, x (2) or z (3)"
        );
        if index >= self.width {
            return self.clone();
        }
        let (target, offset) = (index / CHUNK_BITS, index % CHUNK_BITS);
        let bit = 1u128 << offset;
        Register::from_chunks(self.width, |chunk_index| {
            let chunk = self.chunk(chunk_index);
            if chunk_index != target {
                return chunk;
            }
            let set = |plane: u128, on: bool| if on { plane | bit } else { plane & !bit };
            Chunk {
                value: set(chunk.value, code & 1 != 0),
                unknown: set(chunk.unknown, code & 2 != 0),
            }
        })
    }

    /// True when any bit is `x` or `z`.
    pub fn has_unknown(&self) -> bool {
        match &self.planes {
            Planes::Inline(chunk) => chunk.unknown != 0,
            Planes::Spilled { unknown, .. } => unknown.iter().any(|&word| word != 0),
        }
    }

    /// True when any bit is a known `1`.
    pub fn has_one(&self) -> bool {
        (0..self.chunk_count()).any(|index| self.chunk(index).ones() != 0)
    }

    /// True when any bit is a known `0`.
    pub fn has_zero(&self) -> bool {
        (0..self.chunk_count())
            .any(|index| self.chunk(index).zeros() & chunk_mask(self.width, index) != 0)
    }

    // -- wildcard comparison -----------------------------------------------

    /// `casez` matching: a `z` bit — which is also what a `?` digit writes — on
    /// *either* side stands for any value. Every other position has to be
    /// identical, so an `x` matches only another `x`.
    pub fn matches_ignoring_z(&self, other: &Register) -> bool {
        self.matches_ignoring(other, Chunk::high_impedance)
    }

    /// `casex` matching: an `x` or a `z` on either side stands for any value.
    pub fn matches_ignoring_xz(&self, other: &Register) -> bool {
        self.matches_ignoring(other, |chunk| chunk.unknown)
    }

    /// Whether the two registers are identical at every bit position that
    /// neither side marks a don't-care, zero-extending the narrower one.
    ///
    /// `dont_care` picks the don't-care bits out of one side's chunk, so a
    /// whole word is compared at a time rather than a bit at a time.
    fn matches_ignoring(&self, other: &Register, dont_care: impl Fn(&Chunk) -> u128) -> bool {
        let width = self.width.max(other.width);
        (0..width.div_ceil(CHUNK_BITS)).all(|index| {
            let mine = self.chunk(index);
            let theirs = other.chunk(index);
            let compared = chunk_mask(width, index) & !(dont_care(&mine) | dont_care(&theirs));
            (mine.value ^ theirs.value) & compared == 0
                && (mine.unknown ^ theirs.unknown) & compared == 0
        })
    }

    /// How many bits are a known `1`.
    pub fn count_ones(&self) -> u32 {
        (0..self.chunk_count())
            .map(|index| self.chunk(index).ones().count_ones())
            .sum()
    }

    // -- reshaping ---------------------------------------------------------

    /// Zero-extends (or truncates, keeping the least significant bits) to `width`.
    ///
    /// Verilog extends an `x`/`z` filled literal with `x`/`z`, but a value that is
    /// merely being widened for an operation is extended with `0`. This is the
    /// latter case; [`Register::extend_msb`] implements the former.
    pub fn resize(&self, width: usize) -> Self {
        self.resize_with(width, ZERO)
    }

    /// Like [`Register::resize`] but pads with `fill` instead of `0`.
    pub fn resize_with(&self, width: usize, fill: u8) -> Self {
        let current = self.width;
        if width == current {
            return self.clone();
        }
        if width < current {
            return Register::from_chunks(width, |index| self.chunk(index));
        }
        let padding = Chunk {
            value: if fill & 1 != 0 { u128::MAX } else { 0 },
            unknown: if fill & 2 != 0 { u128::MAX } else { 0 },
        };
        Register::from_chunks(width, |index| {
            let base = index * CHUNK_BITS;
            if base >= current {
                return padding;
            }
            let kept = current - base;
            if kept >= CHUNK_BITS {
                return self.chunk(index);
            }
            // The register's own bits stop at `kept`, so everything above it in
            // this chunk is padding.
            let mask = (1u128 << kept) - 1;
            let chunk = self.chunk(index);
            Chunk {
                value: chunk.value | (padding.value & !mask),
                unknown: chunk.unknown | (padding.unknown & !mask),
            }
        })
    }

    /// Widens to `width` following Verilog's literal-extension rule: a value whose
    /// most significant bit is `x` or `z` is extended with that bit, everything else
    /// is extended with `0`. Truncation keeps the least significant bits.
    pub fn extend_msb(&self, width: usize) -> Self {
        let fill = match self.width.checked_sub(1).map(|msb| self.code_at(msb)) {
            Some(X) => X,
            Some(Z) => Z,
            _ => ZERO,
        };
        self.resize_with(width, fill)
    }

    /// Moves every bit `amount` places toward the most significant end, keeping
    /// the width and filling the vacated positions with `0`.
    pub fn shifted_left(&self, amount: usize) -> Self {
        if amount == 0 {
            return self.clone();
        }
        if amount >= self.width {
            return Register::zeros(self.width);
        }
        let words = amount / CHUNK_BITS;
        let bits = amount % CHUNK_BITS;
        Register::from_chunks(self.width, |index| {
            let Some(source) = index.checked_sub(words) else {
                return Chunk::EMPTY;
            };
            let high = self.chunk(source);
            if bits == 0 {
                return high;
            }
            let low = source
                .checked_sub(1)
                .map_or(Chunk::EMPTY, |index| self.chunk(index));
            Chunk {
                value: (high.value << bits) | (low.value >> (CHUNK_BITS - bits)),
                unknown: (high.unknown << bits) | (low.unknown >> (CHUNK_BITS - bits)),
            }
        })
    }

    /// Moves every bit `amount` places toward the least significant end, keeping
    /// the width and filling the vacated positions with `0`.
    pub fn shifted_right(&self, amount: usize) -> Self {
        if amount == 0 {
            return self.clone();
        }
        if amount >= self.width {
            return Register::zeros(self.width);
        }
        let words = amount / CHUNK_BITS;
        let bits = amount % CHUNK_BITS;
        Register::from_chunks(self.width, |index| {
            let low = self.chunk(index + words);
            if bits == 0 {
                return low;
            }
            let high = self.chunk(index + words + 1);
            Chunk {
                value: (low.value >> bits) | (high.value << (CHUNK_BITS - bits)),
                unknown: (low.unknown >> bits) | (high.unknown << (CHUNK_BITS - bits)),
            }
        })
    }

    /// Joins registers left to right, `parts[0]` supplying the most significant
    /// bits.
    pub fn concatenated(parts: &[Register]) -> Self {
        let width: usize = parts.iter().map(|part| part.width).sum();
        let count = width.div_ceil(CHUNK_BITS);
        let mut value = vec![0u128; count];
        let mut unknown = vec![0u128; count];
        let mut offset = 0;
        for part in parts.iter().rev() {
            for index in 0..part.chunk_count() {
                let chunk = part.chunk(index);
                let position = offset + index * CHUNK_BITS;
                place(&mut value, position, chunk.value);
                place(&mut unknown, position, chunk.unknown);
            }
            offset += part.width;
        }
        Register::from_chunks(width, |index| Chunk {
            value: value[index],
            unknown: unknown[index],
        })
    }

    // -- radix conversions -------------------------------------------------

    /// The unsigned numeric value, or `None` if any bit is `x`/`z` or the register
    /// is wider than 128 bits.
    pub fn to_u128(&self) -> Option<u128> {
        if self.width > 128 || self.has_unknown() {
            return None;
        }
        Some(self.chunk(0).value)
    }

    /// The low `width` bits of `value`, most significant first.
    pub fn from_u128(value: u128, width: usize) -> Self {
        Register::from_chunks(width, |index| {
            if index == 0 {
                Chunk { value, unknown: 0 }
            } else {
                Chunk::EMPTY
            }
        })
    }

    pub fn to_binary(&self) -> String {
        (0..self.width)
            .rev()
            .map(|index| match self.code_at(index) {
                ZERO => '0',
                ONE => '1',
                X => 'x',
                _ => 'z',
            })
            .collect()
    }

    pub fn to_hex(&self) -> Option<String> {
        let mut hex_string = String::new();
        for chunk in self.bit_codes().chunks(4) {
            let mut hex_value = 0;
            for (i, &v) in chunk.iter().enumerate() {
                hex_value |= match v {
                    ZERO => 0,
                    ONE => 1 << (3 - i),
                    _ => return None,
                };
            }
            hex_string.push_str(&format!("{:X}", hex_value));
        }
        Some(hex_string)
    }

    pub fn to_decimal(&self) -> Option<String> {
        let mut decimal_value = 0;
        for v in self.bit_codes() {
            decimal_value = decimal_value * 2
                + match v {
                    ZERO => 0,
                    ONE => 1,
                    _ => return None,
                };
        }
        Some(decimal_value.to_string())
    }

    pub fn to_octal(&self) -> Option<String> {
        let mut octal_string = String::new();
        for chunk in self.bit_codes().chunks(3) {
            let mut octal_value = 0;
            for (i, &v) in chunk.iter().enumerate() {
                octal_value |= match v {
                    ZERO => 0,
                    ONE => 1 << (2 - i),
                    _ => return None,
                };
            }
            octal_string.push_str(&format!("{:o}", octal_value));
        }
        Some(octal_string)
    }

    pub fn from_binary(input: &str) -> Self {
        let values: Vec<u8> = input
            .chars()
            .map(|c| match c {
                '0' => ZERO,
                '1' => ONE,
                'x' => X,
                'z' => Z,
                _ => panic!("Invalid character in binary input"),
            })
            .collect();
        Register::pack(&values)
    }

    pub fn from_hex(input: &str) -> Self {
        let values: Vec<u8> = input
            .chars()
            .flat_map(|c| {
                let hex_value = c.to_digit(16).expect("Invalid character in hex input");
                (0..4).rev().map(move |i| ((hex_value >> i) & 1) as u8)
            })
            .collect();
        Register::pack(&values)
    }

    pub fn from_decimal(input: &str) -> Self {
        let decimal_value = input.parse::<u64>().expect("Invalid decimal input");
        let values: Vec<u8> = format!("{:b}", decimal_value)
            .chars()
            .map(|c| match c {
                '0' => ZERO,
                '1' => ONE,
                _ => panic!("Invalid character in decimal input"),
            })
            .collect();
        Register::pack(&values)
    }

    pub fn from_octal(input: &str) -> Self {
        let values: Vec<u8> = input
            .chars()
            .flat_map(|c| {
                let octal_value = c.to_digit(8).expect("Invalid character in octal input");
                (0..3).rev().map(move |i| ((octal_value >> i) & 1) as u8)
            })
            .collect();
        Register::pack(&values)
    }

    /// The register expanded to one byte per bit, most significant first.
    pub fn get_raw(&self) -> RawBits {
        RawBits(self.bit_codes())
    }
}

/// The one-byte-per-bit expansion of a [`Register`], most significant first.
///
/// A register no longer stores its bits this way, so this is a freshly built
/// vector rather than a borrow of one; it behaves as a `Vec<u8>` and compares
/// equal to one.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RawBits(Vec<u8>);

impl Deref for RawBits {
    type Target = Vec<u8>;

    fn deref(&self) -> &Vec<u8> {
        &self.0
    }
}

impl DerefMut for RawBits {
    fn deref_mut(&mut self) -> &mut Vec<u8> {
        &mut self.0
    }
}

impl From<RawBits> for Vec<u8> {
    fn from(bits: RawBits) -> Vec<u8> {
        bits.0
    }
}

impl PartialEq<&Vec<u8>> for RawBits {
    fn eq(&self, other: &&Vec<u8>) -> bool {
        self.0 == **other
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_binary())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_register_helpers() {
        assert_eq!(Register::zeros(3).to_binary(), "000");
        assert_eq!(Register::ones(3).to_binary(), "111");
        assert_eq!(Register::unknown(2).to_binary(), "xx");
        assert_eq!(Register::high_impedance(2).to_binary(), "zz");
        assert_eq!(Register::zeros(5).width(), 5);
        assert!(!Register::zeros(5).has_unknown());
        assert!(Register::from_binary("00z0").has_unknown());
        assert_eq!(format!("{}", Register::from_binary("01xz")), "01xz");
    }

    #[test]
    fn test_register_bit_from_lsb() {
        let reg = Register::from_binary("1000");
        assert_eq!(reg.bit_from_lsb(0), Some(0));
        assert_eq!(reg.bit_from_lsb(3), Some(1));
        assert_eq!(reg.bit_from_lsb(4), None);
    }

    #[test]
    fn test_register_resize() {
        let reg = Register::from_binary("101");
        assert_eq!(reg.resize(5).to_binary(), "00101");
        assert_eq!(reg.resize(2).to_binary(), "01");
        assert_eq!(reg.resize(3).to_binary(), "101");
        assert_eq!(reg.resize_with(5, X).to_binary(), "xx101");
        assert_eq!(
            Register::from_binary("x1").extend_msb(4).to_binary(),
            "xxx1"
        );
        assert_eq!(
            Register::from_binary("z1").extend_msb(4).to_binary(),
            "zzz1"
        );
        assert_eq!(
            Register::from_binary("11").extend_msb(4).to_binary(),
            "0011"
        );
    }

    #[test]
    fn test_matches_ignoring_z_reads_a_z_on_either_side_as_a_wildcard() {
        let label = Register::from_binary("0z");
        assert!(Register::from_binary("00").matches_ignoring_z(&label));
        assert!(Register::from_binary("01").matches_ignoring_z(&label));
        assert!(!Register::from_binary("10").matches_ignoring_z(&label));

        // The wildcard reads the same way from the subject side.
        let subject = Register::from_binary("z1");
        assert!(subject.matches_ignoring_z(&Register::from_binary("01")));
        assert!(subject.matches_ignoring_z(&Register::from_binary("11")));
        assert!(!subject.matches_ignoring_z(&Register::from_binary("10")));

        // An `x` is not a don't-care here, so it matches only another `x`.
        assert!(!Register::from_binary("x1").matches_ignoring_z(&Register::from_binary("01")));
        assert!(Register::from_binary("x1").matches_ignoring_z(&Register::from_binary("x1")));
    }

    #[test]
    fn test_matches_ignoring_xz_reads_both_unknown_codes_as_wildcards() {
        let label = Register::from_binary("1x");
        assert!(Register::from_binary("10").matches_ignoring_xz(&label));
        assert!(Register::from_binary("11").matches_ignoring_xz(&label));
        assert!(!Register::from_binary("00").matches_ignoring_xz(&label));

        let subject = Register::from_binary("x1");
        assert!(subject.matches_ignoring_xz(&Register::from_binary("01")));
        assert!(subject.matches_ignoring_xz(&Register::from_binary("11")));
        assert!(!subject.matches_ignoring_xz(&Register::from_binary("10")));
    }

    #[test]
    fn test_wildcard_matching_zero_extends_the_narrower_side() {
        // The label is two bits and the subject four, so the bits above the
        // label have to be zero: a wildcard only covers its own position.
        let label = Register::from_binary("0z");
        assert!(Register::from_binary("0000").matches_ignoring_z(&label));
        assert!(!Register::from_binary("0100").matches_ignoring_z(&label));
    }

    #[test]
    fn test_wildcard_matching_spans_more_than_one_chunk() {
        let high = "1".to_string() + &"0".repeat(199);
        let subject = Register::from_binary(&(high.clone() + "11"));

        assert!(subject.matches_ignoring_z(&Register::from_binary(&(high + "zz"))));
        // A mismatch above the first chunk is still caught.
        let elsewhere = "0".repeat(200) + "zz";
        assert!(!subject.matches_ignoring_z(&Register::from_binary(&elsewhere)));
    }

    #[test]
    fn test_register_u128_roundtrip() {
        assert_eq!(Register::from_u128(6, 4).to_binary(), "0110");
        assert_eq!(Register::from_u128(0xAC, 8).to_u128(), Some(0xAC));
        assert_eq!(Register::from_binary("1010").to_u128(), Some(10));
        assert_eq!(Register::from_binary("10x0").to_u128(), None);
        assert_eq!(Register::from_binary("10z0").to_u128(), None);
        // Truncation keeps the least significant bits.
        assert_eq!(Register::from_u128(0xFF, 4).to_binary(), "1111");
    }

    #[test]
    fn test_register_to_binary() {
        let reg = Register::new(4, vec![0, 1, 2, 3]);
        assert_eq!(reg.to_binary(), "01xz");

        let reg = Register::new(8, vec![0, 1, 0, 1, 1, 0, 1, 0]);
        assert_eq!(reg.to_binary(), "01011010");

        let reg = Register::new(2, vec![2, 3]);
        assert_eq!(reg.to_binary(), "xz");
    }

    #[test]
    fn test_register_to_hex() {
        let reg = Register::new(4, vec![0, 0, 1, 1]);
        assert_eq!(reg.to_hex(), Some("3".to_string()));

        let reg = Register::new(8, vec![1, 0, 1, 0, 1, 1, 0, 0]);
        assert_eq!(reg.to_hex(), Some("AC".to_string()));

        let reg = Register::new(16, vec![1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0]);
        assert_eq!(reg.to_hex(), Some("EAC2".to_string()));
    }

    #[test]
    fn test_register_to_decimal() {
        let reg = Register::new(4, vec![0, 1, 1, 0]);
        assert_eq!(reg.to_decimal(), Some("6".to_string()));

        let reg = Register::new(8, vec![1, 0, 1, 0, 1, 1, 0, 0]);
        assert_eq!(reg.to_decimal(), Some("172".to_string()));

        let reg = Register::new(12, vec![1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0]);
        assert_eq!(reg.to_decimal(), Some("3762".to_string()));
    }

    #[test]
    fn test_register_to_octal() {
        let reg = Register::new(3, vec![0, 1, 1]);
        assert_eq!(reg.to_octal(), Some("3".to_string()));

        let reg = Register::new(6, vec![1, 0, 1, 1, 0, 0]);
        assert_eq!(reg.to_octal(), Some("54".to_string()));

        let reg = Register::new(9, vec![1, 1, 1, 0, 1, 0, 1, 1, 0]);
        assert_eq!(reg.to_octal(), Some("726".to_string()));
    }

    #[test]
    fn test_register_from_binary() {
        let reg = Register::from_binary("01xz");
        assert_eq!(reg.get_raw(), &vec![0, 1, 2, 3]);

        let reg = Register::from_binary("01011010");
        assert_eq!(reg.get_raw(), &vec![0, 1, 0, 1, 1, 0, 1, 0]);

        let reg = Register::from_binary("xz");
        assert_eq!(reg.get_raw(), &vec![2, 3]);
    }

    #[test]
    fn test_register_from_hex() {
        let reg = Register::from_hex("3");
        assert_eq!(reg.get_raw(), &vec![0, 0, 1, 1]);

        let reg = Register::from_hex("AC");
        assert_eq!(reg.get_raw(), &vec![1, 0, 1, 0, 1, 1, 0, 0]);

        let reg = Register::from_hex("EAC2");
        assert_eq!(
            reg.get_raw(),
            &vec![1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0]
        );
    }

    #[test]
    fn test_register_from_decimal() {
        let reg = Register::from_decimal("6");
        assert_eq!(reg.get_raw(), &vec![1, 1, 0]);

        let reg = Register::from_decimal("172");
        assert_eq!(reg.get_raw(), &vec![1, 0, 1, 0, 1, 1, 0, 0]);

        let reg = Register::from_decimal("3754");
        assert_eq!(reg.get_raw(), &vec![1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0]);
    }

    #[test]
    fn test_register_from_octal() {
        let reg = Register::from_octal("3");
        assert_eq!(reg.get_raw(), &vec![0, 1, 1]);

        let reg = Register::from_octal("54");
        assert_eq!(reg.get_raw(), &vec![1, 0, 1, 1, 0, 0]);

        let reg = Register::from_octal("732");
        assert_eq!(reg.get_raw(), &vec![1, 1, 1, 0, 1, 1, 0, 1, 0]);
    }

    #[test]
    fn test_register_to_hex_with_xz() {
        let reg = Register::new(4, vec![0, 1, 2, 3]);
        assert_eq!(reg.to_hex(), None);

        let reg = Register::new(8, vec![0, 1, 2, 3, 0, 1, 2, 3]);
        assert_eq!(reg.to_hex(), None);

        let reg = Register::new(12, vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3]);
        assert_eq!(reg.to_hex(), None);
    }

    #[test]
    fn test_register_to_decimal_with_xz() {
        let reg = Register::new(4, vec![0, 1, 2, 3]);
        assert_eq!(reg.to_decimal(), None);

        let reg = Register::new(8, vec![0, 1, 2, 3, 0, 1, 2, 3]);
        assert_eq!(reg.to_decimal(), None);

        let reg = Register::new(12, vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3]);
        assert_eq!(reg.to_decimal(), None);
    }

    #[test]
    fn test_register_parsers() {
        let reg_bin = Register::from_binary("01xz");
        assert_eq!(reg_bin.get_raw(), &vec![0, 1, 2, 3]);

        let reg_hex = Register::from_hex("3");
        assert_eq!(reg_hex.get_raw(), &vec![0, 0, 1, 1]);

        let reg_dec = Register::from_decimal("6");
        assert_eq!(reg_dec.get_raw(), &vec![1, 1, 0]);

        let reg_oct = Register::from_octal("3");
        assert_eq!(reg_oct.get_raw(), &vec![0, 1, 1]);

        let reg_bin = Register::from_binary("01011010");
        assert_eq!(reg_bin.get_raw(), &vec![0, 1, 0, 1, 1, 0, 1, 0]);

        let reg_hex = Register::from_hex("AC");
        assert_eq!(reg_hex.get_raw(), &vec![1, 0, 1, 0, 1, 1, 0, 0]);

        let reg_dec = Register::from_decimal("172");
        assert_eq!(reg_dec.get_raw(), &vec![1, 0, 1, 0, 1, 1, 0, 0]);

        let reg_oct = Register::from_octal("54");
        assert_eq!(reg_oct.get_raw(), &vec![1, 0, 1, 1, 0, 0]);

        let reg_bin = Register::from_binary("xz");
        assert_eq!(reg_bin.get_raw(), &vec![2, 3]);

        let reg_hex = Register::from_hex("EAC2");
        assert_eq!(
            reg_hex.get_raw(),
            &vec![1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0]
        );

        // 1110 1010 11000010
        let reg_dec = Register::from_decimal("3754");
        assert_eq!(reg_dec.get_raw(), &vec![1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0]);

        let reg_oct = Register::from_octal("732");
        assert_eq!(reg_oct.get_raw(), &vec![1, 1, 1, 0, 1, 1, 0, 1, 0]);
    }
}

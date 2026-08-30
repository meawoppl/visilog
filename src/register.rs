use std::fmt;

/// Logic `0`.
pub const ZERO: u8 = 0;
/// Logic `1`.
pub const ONE: u8 = 1;
/// Unknown / uninitialized (`x`).
pub const X: u8 = 2;
/// High impedance (`z`).
pub const Z: u8 = 3;

/// A four-state (`0`/`1`/`x`/`z`) bit vector.
///
/// Bits are stored most-significant first: `values[0]` is the left-most bit as
/// written in Verilog source, `values[width - 1]` is the least significant bit.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Register {
    values: Vec<u8>,
}

impl Register {
    pub fn new(width: usize, values: Vec<u8>) -> Self {
        assert!(values.len() == width);
        assert!(
            values.iter().all(|&v| v <= Z),
            "Register bits must be one of 0, 1, x (2) or z (3)"
        );
        Register { values }
    }

    /// Builds a register directly from a most-significant-first bit vector.
    pub fn from_bits(values: Vec<u8>) -> Self {
        Register::new(values.len(), values)
    }

    /// A register of `width` bits all set to `value`.
    pub fn filled(width: usize, value: u8) -> Self {
        Register::new(width, vec![value; width])
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
        self.values.len()
    }

    /// The bit at `index` counted from the least significant end.
    pub fn bit_from_lsb(&self, index: usize) -> Option<u8> {
        if index >= self.values.len() {
            return None;
        }
        Some(self.values[self.values.len() - 1 - index])
    }

    /// True when any bit is `x` or `z`.
    pub fn has_unknown(&self) -> bool {
        self.values.iter().any(|&v| v == X || v == Z)
    }

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
        let current = self.values.len();
        if width == current {
            return self.clone();
        }
        if width < current {
            return Register::from_bits(self.values[current - width..].to_vec());
        }
        let mut values = vec![fill; width - current];
        values.extend_from_slice(&self.values);
        Register::from_bits(values)
    }

    /// Widens to `width` following Verilog's literal-extension rule: a value whose
    /// most significant bit is `x` or `z` is extended with that bit, everything else
    /// is extended with `0`. Truncation keeps the least significant bits.
    pub fn extend_msb(&self, width: usize) -> Self {
        let fill = match self.values.first() {
            Some(&X) => X,
            Some(&Z) => Z,
            _ => ZERO,
        };
        self.resize_with(width, fill)
    }

    /// The unsigned numeric value, or `None` if any bit is `x`/`z` or the register
    /// is wider than 128 bits.
    pub fn to_u128(&self) -> Option<u128> {
        if self.values.len() > 128 || self.has_unknown() {
            return None;
        }
        let mut value: u128 = 0;
        for &v in &self.values {
            value = (value << 1) | v as u128;
        }
        Some(value)
    }

    /// The low `width` bits of `value`, most significant first.
    pub fn from_u128(value: u128, width: usize) -> Self {
        let values = (0..width)
            .rev()
            .map(|i| {
                if i >= 128 {
                    ZERO
                } else {
                    ((value >> i) & 1) as u8
                }
            })
            .collect();
        Register::from_bits(values)
    }

    pub fn to_binary(&self) -> String {
        self.values
            .iter()
            .map(|&v| match v {
                0 => '0',
                1 => '1',
                2 => 'x',
                3 => 'z',
                _ => panic!("Invalid value"),
            })
            .collect()
    }

    pub fn to_hex(&self) -> Option<String> {
        let mut hex_string = String::new();
        for chunk in self.values.chunks(4) {
            let mut hex_value = 0;
            for (i, &v) in chunk.iter().enumerate() {
                hex_value |= match v {
                    0 => 0,
                    1 => 1 << (3 - i),
                    2 | 3 => return None,
                    _ => panic!("Invalid value"),
                };
            }
            hex_string.push_str(&format!("{:X}", hex_value));
        }
        Some(hex_string)
    }

    pub fn to_decimal(&self) -> Option<String> {
        let mut decimal_value = 0;
        for &v in &self.values {
            decimal_value = decimal_value * 2
                + match v {
                    0 => 0,
                    1 => 1,
                    2 | 3 => return None,
                    _ => panic!("Invalid value"),
                };
        }
        Some(decimal_value.to_string())
    }

    pub fn to_octal(&self) -> Option<String> {
        let mut octal_string = String::new();
        for chunk in self.values.chunks(3) {
            let mut octal_value = 0;
            for (i, &v) in chunk.iter().enumerate() {
                octal_value |= match v {
                    0 => 0,
                    1 => 1 << (2 - i),
                    2 | 3 => return None,
                    _ => panic!("Invalid value"),
                };
            }
            octal_string.push_str(&format!("{:o}", octal_value));
        }
        Some(octal_string)
    }

    pub fn from_binary(input: &str) -> Self {
        let values = input
            .chars()
            .map(|c| match c {
                '0' => 0,
                '1' => 1,
                'x' => 2,
                'z' => 3,
                _ => panic!("Invalid character in binary input"),
            })
            .collect();
        Register { values }
    }

    pub fn from_hex(input: &str) -> Self {
        let values = input
            .chars()
            .flat_map(|c| {
                let hex_value = c.to_digit(16).expect("Invalid character in hex input");
                (0..4).rev().map(move |i| ((hex_value >> i) & 1) as u8)
            })
            .collect();
        Register { values }
    }

    pub fn from_decimal(input: &str) -> Self {
        let decimal_value = input.parse::<u64>().expect("Invalid decimal input");
        let values = format!("{:b}", decimal_value)
            .chars()
            .map(|c| match c {
                '0' => 0,
                '1' => 1,
                _ => panic!("Invalid character in decimal input"),
            })
            .collect();
        Register { values }
    }

    pub fn from_octal(input: &str) -> Self {
        let values = input
            .chars()
            .flat_map(|c| {
                let octal_value = c.to_digit(8).expect("Invalid character in octal input");
                (0..3).rev().map(move |i| ((octal_value >> i) & 1) as u8)
            })
            .collect();
        Register { values }
    }

    /// Returns a reference to the raw values of the register.
    pub fn get_raw(&self) -> &Vec<u8> {
        &self.values
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

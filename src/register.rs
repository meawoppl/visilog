pub struct Register {
    values: Vec<u8>,
}

impl Register {
    pub fn new(width: usize, values: Vec<u8>) -> Self {
        assert!(values.len() == width);
        Register { values }
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

#[cfg(test)]
mod tests {
    use super::*;

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

        let reg = Register::new(12, vec![1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0]);
        assert_eq!(reg.to_hex(), Some("EAC2".to_string()));
    }

    #[test]
    fn test_register_to_decimal() {
        let reg = Register::new(4, vec![0, 1, 1, 0]);
        assert_eq!(reg.to_decimal(), Some("6".to_string()));

        let reg = Register::new(8, vec![1, 0, 1, 0, 1, 1, 0, 0]);
        assert_eq!(reg.to_decimal(), Some("172".to_string()));

        let reg = Register::new(12, vec![1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0]);
        assert_eq!(reg.to_decimal(), Some("3754".to_string()));
    }

    #[test]
    fn test_register_to_octal() {
        let reg = Register::new(3, vec![0, 1, 1]);
        assert_eq!(reg.to_octal(), Some("3".to_string()));

        let reg = Register::new(6, vec![1, 0, 1, 1, 0, 0]);
        assert_eq!(reg.to_octal(), Some("54".to_string()));

        let reg = Register::new(9, vec![1, 1, 1, 0, 1, 0, 1, 1, 0]);
        assert_eq!(reg.to_octal(), Some("732".to_string()));
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
        assert_eq!(reg.get_raw(), &vec![1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0]);
    }

    #[test]
    fn test_register_from_decimal() {
        let reg = Register::from_decimal("6");
        assert_eq!(reg.get_raw(), &vec![1, 1, 0]);

        let reg = Register::from_decimal("172");
        assert_eq!(reg.get_raw(), &vec![1, 0, 1, 0, 1, 1, 0, 0]);

        let reg = Register::from_decimal("3754");
        assert_eq!(reg.get_raw(), &vec![1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0]);
    }

    #[test]
    fn test_register_from_octal() {
        let reg = Register::from_octal("3");
        assert_eq!(reg.get_raw(), &vec![0, 1, 1]);

        let reg = Register::from_octal("54");
        assert_eq!(reg.get_raw(), &vec![1, 0, 1, 1, 0, 0]);

        let reg = Register::from_octal("732");
        assert_eq!(reg.get_raw(), &vec![1, 1, 1, 0, 1, 0, 1, 1, 0]);
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
        assert_eq!(reg_hex.get_raw(), &vec![1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0]);

        let reg_dec = Register::from_decimal("3754");
        assert_eq!(reg_dec.get_raw(), &vec![1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0]);

        let reg_oct = Register::from_octal("732");
        assert_eq!(reg_oct.get_raw(), &vec![1, 1, 1, 0, 1, 0, 1, 1, 0]);
    }
}

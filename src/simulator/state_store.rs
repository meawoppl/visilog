use std::collections::HashMap;
use std::fmt;

use crate::register::{Register, X};

/// A single named signal: its current four-state value plus the `(msb, lsb)`
/// range it was declared with.
///
/// The range matters for bit and part selects: `reg [7:0] a` and `reg [0:7] a`
/// hold the same bits but `a[0]` names opposite ends of the vector.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SignalState {
    register: Register,
    range: (i64, i64),
}

impl SignalState {
    /// Wraps a value with the implicit range `(width - 1, 0)`.
    pub fn new(register: Register) -> Self {
        let range = (register.width() as i64 - 1, 0);
        SignalState { register, range }
    }

    /// Wraps a value with an explicit `(msb, lsb)` range.
    ///
    /// The range's bit count must match the register's width.
    pub fn with_range(register: Register, range: (i64, i64)) -> Self {
        assert_eq!(
            range_width(range),
            register.width(),
            "declared range {:?} does not match register width {}",
            range,
            register.width()
        );
        SignalState { register, range }
    }

    pub fn register(&self) -> &Register {
        &self.register
    }

    pub fn range(&self) -> (i64, i64) {
        self.range
    }

    pub fn width(&self) -> usize {
        self.register.width()
    }

    /// Translates a Verilog bit index into an offset into the register's
    /// most-significant-first bit vector, or `None` if the index is outside the
    /// declared range.
    pub fn bit_position(&self, index: i64) -> Option<usize> {
        let (msb, lsb) = self.range;
        let offset = if msb >= lsb {
            if index > msb || index < lsb {
                return None;
            }
            msb - index
        } else {
            if index < msb || index > lsb {
                return None;
            }
            index - msb
        };
        Some(offset as usize)
    }

    /// The value of a single declared bit. Reading outside the declared range
    /// yields `x`, which is what Verilog does for an out-of-bounds select.
    pub fn bit(&self, index: i64) -> u8 {
        match self.bit_position(index) {
            Some(offset) => self.register.get_raw()[offset],
            None => X,
        }
    }

    /// Writes a single declared bit and reports whether the stored value moved.
    /// Writing outside the declared range is discarded, which is what Verilog
    /// does with an out-of-bounds select on the left of an assignment.
    pub fn set_bit(&mut self, index: i64, value: u8) -> bool {
        let Some(offset) = self.bit_position(index) else {
            return false;
        };
        let mut bits = self.register.get_raw().clone();
        if bits[offset] == value {
            return false;
        }
        bits[offset] = value;
        self.register = Register::from_bits(bits);
        true
    }
}

fn range_width(range: (i64, i64)) -> usize {
    ((range.0 - range.1).abs() + 1) as usize
}

/// Name to value map for every signal in a simulation.
#[derive(Clone, Debug, Default)]
pub struct StateStore {
    name_to_signal: HashMap<String, SignalState>,
}

impl StateStore {
    pub fn new() -> Self {
        StateStore {
            name_to_signal: HashMap::new(),
        }
    }

    /// Declares a signal over `(msb, lsb)`, initialized to all `x` the way an
    /// unassigned Verilog `reg` starts out.
    pub fn declare(&mut self, name: impl Into<String>, range: (i64, i64)) {
        let register = Register::unknown(range_width(range));
        self.name_to_signal
            .insert(name.into(), SignalState::with_range(register, range));
    }

    /// Sets a signal's value. A previously declared range is preserved when the
    /// widths still agree; otherwise the signal is (re)declared as `(width - 1, 0)`.
    pub fn set(&mut self, name: impl Into<String>, register: Register) {
        let name = name.into();
        let range = self
            .name_to_signal
            .get(&name)
            .map(|signal| signal.range())
            .filter(|&range| range_width(range) == register.width());
        let signal = match range {
            Some(range) => SignalState::with_range(register, range),
            None => SignalState::new(register),
        };
        self.name_to_signal.insert(name, signal);
    }

    /// Sets a signal's value and declared range in one step.
    pub fn set_ranged(&mut self, name: impl Into<String>, register: Register, range: (i64, i64)) {
        self.name_to_signal
            .insert(name.into(), SignalState::with_range(register, range));
    }

    pub fn get(&self, name: &str) -> Option<&Register> {
        self.name_to_signal.get(name).map(|s| s.register())
    }

    pub fn get_signal(&self, name: &str) -> Option<&SignalState> {
        self.name_to_signal.get(name)
    }

    pub fn get_signal_mut(&mut self, name: &str) -> Option<&mut SignalState> {
        self.name_to_signal.get_mut(name)
    }

    pub fn contains(&self, name: &str) -> bool {
        self.name_to_signal.contains_key(name)
    }

    pub fn len(&self) -> usize {
        self.name_to_signal.len()
    }

    pub fn is_empty(&self) -> bool {
        self.name_to_signal.is_empty()
    }

    /// Every signal name, sorted.
    pub fn names(&self) -> Vec<&str> {
        let mut names: Vec<&str> = self.name_to_signal.keys().map(|k| k.as_str()).collect();
        names.sort();
        names
    }
}

impl fmt::Display for StateStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for name in self.names() {
            writeln!(f, "{}: {}", name, self.name_to_signal[name].register())?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state_store_display_is_sorted_by_name() {
        let mut state_store = StateStore::new();
        state_store.set("reg2", Register::from_binary("010"));
        state_store.set("reg1", Register::from_binary("101"));

        assert_eq!(format!("{}", state_store), "reg1: 101\nreg2: 010\n");
    }

    #[test]
    fn test_set_and_get() {
        let mut state_store = StateStore::new();
        state_store.set("reg1", Register::from_binary("111"));

        assert!(state_store.contains("reg1"));
        assert_eq!(state_store.get("reg1"), Some(&Register::from_binary("111")));
        assert_eq!(state_store.get("nope"), None);
        assert_eq!(state_store.len(), 1);
        assert!(!state_store.is_empty());
    }

    #[test]
    fn test_set_multiple() {
        let mut state_store = StateStore::new();
        state_store.set("reg1", Register::from_binary("101"));
        state_store.set("reg2", Register::from_binary("010"));

        assert_eq!(state_store.names(), vec!["reg1", "reg2"]);
        assert_eq!(state_store.get("reg1").unwrap().to_binary(), "101");
        assert_eq!(state_store.get("reg2").unwrap().to_binary(), "010");
    }

    #[test]
    fn test_set_overwrite() {
        let mut state_store = StateStore::new();
        state_store.set("reg1", Register::from_binary("101"));
        state_store.set("reg1", Register::from_binary("000"));

        assert_eq!(state_store.len(), 1);
        assert_eq!(state_store.get("reg1").unwrap().to_binary(), "000");
    }

    #[test]
    fn test_declare_starts_unknown() {
        let mut state_store = StateStore::new();
        state_store.declare("bus", (7, 0));

        assert_eq!(state_store.get("bus").unwrap().to_binary(), "xxxxxxxx");
        assert_eq!(state_store.get_signal("bus").unwrap().range(), (7, 0));
    }

    #[test]
    fn test_set_preserves_declared_range() {
        let mut state_store = StateStore::new();
        state_store.declare("bus", (11, 4));
        state_store.set("bus", Register::from_binary("00001111"));

        assert_eq!(state_store.get_signal("bus").unwrap().range(), (11, 4));
    }

    #[test]
    fn test_set_redeclares_on_width_change() {
        let mut state_store = StateStore::new();
        state_store.declare("bus", (11, 4));
        state_store.set("bus", Register::from_binary("101"));

        assert_eq!(state_store.get_signal("bus").unwrap().range(), (2, 0));
    }

    #[test]
    fn test_signal_bit_positions_descending() {
        let signal = SignalState::with_range(Register::from_binary("1000"), (7, 4));

        assert_eq!(signal.bit_position(7), Some(0));
        assert_eq!(signal.bit_position(4), Some(3));
        assert_eq!(signal.bit_position(3), None);
        assert_eq!(signal.bit_position(8), None);
        assert_eq!(signal.bit(7), 1);
        assert_eq!(signal.bit(6), 0);
        // Out of range reads are x.
        assert_eq!(signal.bit(9), X);
    }

    #[test]
    fn test_signal_bit_positions_ascending() {
        let signal = SignalState::with_range(Register::from_binary("1000"), (0, 3));

        assert_eq!(signal.bit_position(0), Some(0));
        assert_eq!(signal.bit_position(3), Some(3));
        assert_eq!(signal.bit_position(4), None);
        assert_eq!(signal.bit(0), 1);
        assert_eq!(signal.bit(1), 0);
    }

    #[test]
    fn test_signal_set_bit() {
        let mut signal = SignalState::with_range(Register::unknown(4), (7, 4));

        assert!(signal.set_bit(7, 1));
        assert!(signal.set_bit(4, 0));
        assert_eq!(signal.register().to_binary(), "1xx0");
        // Rewriting the same value is not a change.
        assert!(!signal.set_bit(7, 1));
        // Out of range writes are discarded.
        assert!(!signal.set_bit(3, 1));
        assert_eq!(signal.register().to_binary(), "1xx0");
    }

    #[test]
    fn test_signal_new_uses_implicit_range() {
        let signal = SignalState::new(Register::from_binary("1010"));

        assert_eq!(signal.range(), (3, 0));
        assert_eq!(signal.width(), 4);
        assert_eq!(signal.bit(0), 0);
        assert_eq!(signal.bit(3), 1);
    }
}

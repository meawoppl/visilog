use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

use rand::rngs::StdRng;
use rand::{RngCore, SeedableRng};

use crate::register::{Register, X};

/// What the `$random` stream starts from.
///
/// IEEE 1364 leaves an unseeded `$random` implementation defined, so the choice
/// is ours; a fixed constant is the one that makes a design's output the same
/// on every run, which is what a self-checking test that prints random stimulus
/// needs. A simulation gets a fresh [`StateStore`], so the stream restarts at
/// this seed every time a design is set up.
const DEFAULT_RANDOM_SEED: u64 = 0;

/// The stream `$random` draws from.
///
/// [`eval`](crate::simulator::eval::eval) is handed a `&StateStore` and nothing
/// else, so the one system function that is not a pure function of its
/// arguments has to advance its state through a shared reference — hence the
/// [`RefCell`]. Cloning a store clones the stream's position with it, so a
/// snapshot replays the same numbers.
#[derive(Clone, Debug)]
pub struct RandomStream(RefCell<StdRng>);

impl Default for RandomStream {
    fn default() -> Self {
        RandomStream(RefCell::new(StdRng::seed_from_u64(DEFAULT_RANDOM_SEED)))
    }
}

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
            // `bit_position` counts from the most significant end, the way the
            // bits are written in Verilog source; `Register` indexes from the
            // least significant end.
            Some(offset) => self
                .register
                .bit_from_lsb(self.width() - 1 - offset)
                .unwrap_or(X),
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
        let from_lsb = self.width() - 1 - offset;
        if self.register.bit_from_lsb(from_lsb) == Some(value) {
            return false;
        }
        self.register = self.register.with_bit(from_lsb, value);
        true
    }
}

fn range_width(range: (i64, i64)) -> usize {
    ((range.0 - range.1).abs() + 1) as usize
}

/// Name to value map for every signal in a simulation, together with a journal
/// of everything written since the last marker.
///
/// The journal is what makes edge detection affordable. A scheduler that has to
/// discover which signals moved by diffing two whole snapshots pays for every
/// signal in the design on every delta cycle, when the set of signals that
/// could possibly have moved is exactly the set something wrote. Writes record
/// the value they displaced; [`take_changes`](StateStore::take_changes) hands
/// that list over and starts a fresh one.
/// The store also carries the simulation context an expression can read but no
/// signal holds — the current time and the `$random` stream — because a
/// `&StateStore` is all [`eval`](crate::simulator::eval::eval) is given.
#[derive(Clone, Debug, Default)]
pub struct StateStore {
    name_to_signal: HashMap<String, SignalState>,
    /// For every signal written since the last marker, the value it held at
    /// that marker. `None` records a name that did not exist yet, which makes
    /// the write a declaration rather than a change.
    journal: HashMap<String, Option<Register>>,
    /// What `$time` reads. The driver moves it as simulated time moves.
    time: i64,
    random: RandomStream,
}

impl StateStore {
    pub fn new() -> Self {
        StateStore::default()
    }

    /// The simulated time `$time` reports.
    pub fn time(&self) -> i64 {
        self.time
    }

    /// Tells the store what time it is. The driver does this whenever
    /// simulated time moves, so `$time` reads the timestamp the expression
    /// around it is evaluated at.
    pub fn set_time(&mut self, time: i64) {
        self.time = time;
    }

    /// The next number in the `$random` stream, as Verilog's 32 bit integer.
    pub fn next_random(&self) -> u32 {
        self.random.0.borrow_mut().next_u32()
    }

    /// Restarts the `$random` stream from `seed`, which is what `$random(seed)`
    /// does. Verilog's seed argument is an `inout` the simulator writes back
    /// through; nothing here writes back, so a design that re-seeds from a
    /// variable it never changes draws the same number every time.
    pub fn seed_random(&self, seed: u64) {
        *self.random.0.borrow_mut() = StdRng::seed_from_u64(seed);
    }

    /// Notes the value `name` holds right now, so that a write about to land on
    /// it can be reported as a transition.
    ///
    /// Only the first write since the marker is recorded: a later one would
    /// overwrite the value the round actually started from, and it is that
    /// value an edge has to be measured against.
    fn record(&mut self, name: &str) {
        if self.journal.contains_key(name) {
            return;
        }
        let previous = self
            .name_to_signal
            .get(name)
            .map(|signal| signal.register().clone());
        self.journal.insert(name.to_string(), previous);
    }

    /// The name and pre-write value of every signal written since the last
    /// call, sorted by name, clearing the journal so the next round is measured
    /// from here.
    ///
    /// A name that did not exist at the last call is left out: it was declared
    /// rather than changed, and declaring a signal is not a simulation event.
    /// Writes that put back the value already there are still reported — the
    /// journal records what was displaced, not whether it differed — so the
    /// caller compares.
    pub fn take_changes(&mut self) -> Vec<(String, Register)> {
        let mut changes = Vec::with_capacity(self.journal.len());
        for (name, previous) in self.journal.drain() {
            if let Some(previous) = previous {
                changes.push((name, previous));
            }
        }
        changes.sort_by(|left, right| left.0.cmp(&right.0));
        changes
    }

    /// Forgets every recorded change, making now the point later changes are
    /// measured against.
    pub fn clear_changes(&mut self) {
        self.journal.clear();
    }

    /// Declares a signal over `(msb, lsb)`, initialized to all `x` the way an
    /// unassigned Verilog `reg` starts out.
    pub fn declare(&mut self, name: impl Into<String>, range: (i64, i64)) {
        let name = name.into();
        self.record(&name);
        let register = Register::unknown(range_width(range));
        self.name_to_signal
            .insert(name, SignalState::with_range(register, range));
    }

    /// Sets a signal's value. A previously declared range is preserved when the
    /// widths still agree; otherwise the signal is (re)declared as `(width - 1, 0)`.
    pub fn set(&mut self, name: impl Into<String>, register: Register) {
        let name = name.into();
        self.record(&name);
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
        let name = name.into();
        self.record(&name);
        self.name_to_signal
            .insert(name, SignalState::with_range(register, range));
    }

    pub fn get(&self, name: &str) -> Option<&Register> {
        self.name_to_signal.get(name).map(|s| s.register())
    }

    pub fn get_signal(&self, name: &str) -> Option<&SignalState> {
        self.name_to_signal.get(name)
    }

    /// A signal for in-place modification. What it holds now is journalled
    /// first, since the caller is free to move it.
    pub fn get_signal_mut(&mut self, name: &str) -> Option<&mut SignalState> {
        self.record(name);
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

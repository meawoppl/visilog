use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

use rand::rngs::StdRng;
use rand::{RngCore, SeedableRng};

use crate::parsers::expr::Expression;
use crate::register::{Register, X};
use crate::simulator::program::FunctionDefinition;

/// What the `$random` stream starts from.
///
/// IEEE 1364 leaves an unseeded `$random` implementation defined, so the choice
/// is ours; a fixed constant is the one that makes a design's output the same
/// on every run, which is what a self-checking test that prints random stimulus
/// needs. A simulation gets a fresh [`StateStore`], so the stream restarts at
/// this seed every time a design is set up.
const DEFAULT_RANDOM_SEED: u64 = 0;

/// How deep calls to a design's own functions may nest.
///
/// Every call gets a frame of its own, so a recursive function *works* — but it
/// works on the host's stack, and one that never reaches its base case would
/// take the process down with it. This is the bound that makes runaway
/// recursion a named error instead.
///
/// It is deliberately well under what the stack holds: an unoptimised build
/// runs out somewhere between 96 and 128 nested calls, and a call whose
/// expressions nest deeply costs more than the plain ones that measurement used.
pub const MAX_CALL_DEPTH: usize = 64;

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
///
/// Signedness lives here too, because in Verilog it is a property of the
/// *declaration* — `reg signed [3:0] a` — and not of the bits. It is carried on
/// the stored value itself rather than in a field beside it, so there is one
/// copy of it and an expression that reads the signal reads it along with the
/// bits; what makes it a *declared* property is that the store re-stamps it on
/// every write, and a value cannot bring its own.
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

    /// The same signal, declared signed or unsigned.
    pub fn with_signedness(mut self, signed: bool) -> Self {
        self.register = self.register.with_signedness(signed);
        self
    }

    /// Whether the signal was declared `signed`.
    pub fn is_signed(&self) -> bool {
        self.register.is_signed()
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
        // `with_bit` builds a fresh register, which is an unsigned one until it
        // is told otherwise; the declaration outlives a single bit write.
        let signed = self.register.is_signed();
        self.register = self
            .register
            .with_bit(from_lsb, value)
            .with_signedness(signed);
        true
    }
}

fn range_width(range: (i64, i64)) -> usize {
    ((range.0 - range.1).abs() + 1) as usize
}

/// A memory: `reg [7:0] mem [0:255];` — an array of words, each one a register
/// of its own.
///
/// A memory is kept in a map of its own rather than as a wider [`SignalState`],
/// and that separation is the whole disambiguation between a *bit* select and a
/// *word* select. `a[3]` and `m[3]` are the same syntax; which one is meant
/// depends only on how the name was declared, and the declaration reaches
/// [`eval`](crate::simulator::eval::eval) as *which map the name landed in*.
/// A name is a signal or a memory, never both.
///
/// A word that has never been written reads `x`, exactly like an undriven
/// register, and an address outside the declared range reads `x` and swallows a
/// write — the same thing [`SignalState::bit`] and [`SignalState::set_bit`] do
/// with an out-of-range bit.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Memory {
    words: Vec<Register>,
    /// The declared address range: `(0, 255)` for `mem [0:255]`, `(15, 8)` for
    /// `mem [15:8]`.
    addresses: (i64, i64),
    /// The `(msb, lsb)` range of one word.
    range: (i64, i64),
}

impl Memory {
    /// A memory of `addresses` words, each `range` wide and every bit `x`.
    pub fn new(addresses: (i64, i64), range: (i64, i64), signed: bool) -> Self {
        let word = Register::unknown(range_width(range)).with_signedness(signed);
        Memory {
            words: vec![word; range_width(addresses)],
            addresses,
            range,
        }
    }

    /// How many words the memory holds.
    pub fn depth(&self) -> usize {
        self.words.len()
    }

    /// The declared address range.
    pub fn addresses(&self) -> (i64, i64) {
        self.addresses
    }

    /// The `(msb, lsb)` range of one word.
    pub fn range(&self) -> (i64, i64) {
        self.range
    }

    /// How wide one word is.
    pub fn width(&self) -> usize {
        range_width(self.range)
    }

    /// Whether the declaration carried a `signed` qualifier.
    pub fn is_signed(&self) -> bool {
        self.words[0].is_signed()
    }

    /// Translates a declared address into an offset into `words`, counting from
    /// the address written first — so `mem [0:255]` and `mem [15:8]` both run in
    /// the order their declarations read. `None` is an address outside the
    /// declared range.
    pub fn word_position(&self, address: i64) -> Option<usize> {
        let (first, last) = self.addresses;
        let offset = if first <= last {
            if address < first || address > last {
                return None;
            }
            address - first
        } else {
            if address > first || address < last {
                return None;
            }
            first - address
        };
        Some(offset as usize)
    }

    /// The word at `address`. An address that is unknown — `None`, which is what
    /// an `x` index evaluates to — or outside the declared range reads `x`.
    pub fn word(&self, address: Option<i64>) -> Register {
        match address.and_then(|address| self.word_position(address)) {
            Some(offset) => self.words[offset].clone(),
            None => Register::unknown(self.width()).with_signedness(self.is_signed()),
        }
    }

    /// Writes a word, resized to the memory's own width, and reports whether the
    /// stored value moved. A write outside the declared range is discarded.
    pub fn set_word(&mut self, address: i64, value: &Register) -> bool {
        let Some(offset) = self.word_position(address) else {
            return false;
        };
        // Signedness is the *declaration's*, so it is re-stamped on every write
        // exactly as `SignalState` does it: a value cannot bring its own.
        let signed = self.words[offset].is_signed();
        let value = value.coerced(self.width()).with_signedness(signed);
        if self.words[offset] == value {
            return false;
        }
        self.words[offset] = value;
        true
    }
}

/// How strongly something drives a signal.
///
/// Verilog gives a variable more than one potential source, and says which one
/// wins: a `force` beats a procedural continuous `assign`, which beats an
/// ordinary procedural write. The order of these variants *is* that rule —
/// [`StateStore::permits_write`] compares them — so keep them written weakest
/// first.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum DriveLevel {
    /// An ordinary write: a blocking or non-blocking assignment, a module-level
    /// continuous assignment, a testbench driving an input. Everything that is
    /// not one of the two below.
    Procedural,
    /// A procedural continuous assignment — `assign v = e;` inside a block.
    Assign,
    /// A `force`.
    Force,
}

/// A continuous drive a procedural block installed with `assign` or `force`.
///
/// It is *continuous*: the value is not written once when the statement runs,
/// it is re-evaluated whenever anything the design does could have moved one of
/// its operands. The drive therefore has to outlive the statement that
/// installed it, and it lives here rather than on the `Simulator` because the
/// only thing a running procedural block is handed is a [`StateStore`].
#[derive(Clone, Debug, PartialEq)]
pub struct Drive {
    /// The signal the target names, which is the key the precedence rule is
    /// answered by.
    name: String,
    /// The left hand side, kept unresolved so that the drive re-resolves it the
    /// way a module-level continuous assignment does — a variable index in
    /// `force m[i] = e;` follows `i`.
    target: Expression,
    value: Expression,
    level: DriveLevel,
    /// What a `force` displaced when it was installed. A `release` with no
    /// procedural `assign` underneath it puts this back: writes made while the
    /// force was in place were discarded, so this is still the signal's last
    /// procedural value. `None` for an `assign`, which a `deassign` does not
    /// undo.
    displaced: Option<Register>,
}

impl Drive {
    pub fn new(
        name: impl Into<String>,
        target: Expression,
        value: Expression,
        level: DriveLevel,
        displaced: Option<Register>,
    ) -> Self {
        Drive {
            name: name.into(),
            target,
            value,
            level,
            displaced,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn target(&self) -> &Expression {
        &self.target
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }

    pub fn level(&self) -> DriveLevel {
        self.level
    }

    /// The value this drive displaced, consumed by the `release` that undoes it.
    pub fn into_displaced(self) -> Option<Register> {
        self.displaced
    }
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
    /// The memories the design declares, keyed by qualified name.
    ///
    /// Deliberately a second map rather than a field on [`SignalState`]: a
    /// memory needs `n` words where a signal needs one, and the lookup that
    /// every bit select goes through is the hot path. A name is in one map or
    /// the other, so an ordinary select still costs one hash and only a miss
    /// looks here.
    name_to_memory: HashMap<String, Memory>,
    /// For every memory written since the last marker, the first word the round
    /// displaced and the last word written into it. See
    /// [`set_word`](StateStore::set_word).
    ///
    /// A list rather than a map: a design has a handful of memories at most, so
    /// a linear scan beats hashing a name, and taking an empty one costs
    /// nothing — which matters because every delta cycle asks.
    memory_journal: Vec<(String, Register, Register)>,
    /// For every signal written since the last marker, the value it held at
    /// that marker. `None` records a name that did not exist yet, which makes
    /// the write a declaration rather than a change.
    journal: HashMap<String, Option<Register>>,
    /// What `$time` reads. The driver moves it as simulated time moves.
    time: i64,
    random: RandomStream,
    /// The functions the design declares, keyed by qualified name.
    ///
    /// They live here because [`eval`](crate::simulator::eval::eval) is handed
    /// a `&StateStore` and nothing else, and a call has to find its definition.
    /// The table is shared rather than copied so that a call's frame — itself a
    /// store — can call a function in turn without cloning every compiled body.
    functions: Rc<HashMap<String, FunctionDefinition>>,
    /// How many function calls are on the stack above this store.
    call_depth: Cell<usize>,
    /// Whether any signal here was declared signed.
    ///
    /// A hint, not a fact to reason from: it is set when a signed signal is
    /// declared and never cleared. It exists because `expression_is_signed`
    /// asks about every identifier it walks past, and hashing a name to answer
    /// "no" for a design that declares nothing signed is most of what that walk
    /// would otherwise cost.
    any_signed: bool,
    /// Whether the design declares any memory at all, on the same terms as
    /// `any_signed`. `resolve_target` has to ask "is this name a memory?" of
    /// every bit-select write, and for a design with no memories this answers
    /// it without hashing the name.
    any_memory: bool,
    /// The named events the design declares: `event done;`.
    ///
    /// An event has no value, so it is deliberately not a signal — a third
    /// namespace beside the signals and the memories rather than a zero-width
    /// entry in either. That is what makes reading one a named error instead
    /// of a plausible pattern of bits.
    events: HashSet<String>,
    /// Every event triggered since the last marker, in trigger order.
    ///
    /// This is the whole of an event's state. A trigger is momentary: it is
    /// journalled here, turned into an edge by the settle loop, and gone. A
    /// list rather than a set because a design has a handful of events at
    /// most, and taking an empty one costs nothing.
    triggers: Vec<String>,
    /// The `force`s and procedural `assign`s currently installed, in the order
    /// they were installed.
    ///
    /// A `Vec` and not a map: a design has a handful of these at most and most
    /// have none, so the check every write makes is "is this list empty?" —
    /// a length compare — rather than a hash. Behind an [`Rc`] so that the
    /// driver can hold the list while writing through `&mut StateStore`,
    /// which is what lets these join the continuous-assignment fixpoint;
    /// installing one goes through [`Rc::make_mut`], the same way the function
    /// table does.
    drives: Rc<Vec<Drive>>,
}

impl StateStore {
    pub fn new() -> Self {
        StateStore::default()
    }

    /// Whether any signal in the store was declared signed. `false` is exact —
    /// nothing here is signed — while `true` only means something once was.
    pub fn any_signed(&self) -> bool {
        self.any_signed
    }

    /// A store for the body of a function called against this one: the same
    /// function table and the same clock, but no signals — a call's variables
    /// are its own, and nothing it writes is allowed to reach the design.
    ///
    /// The call depth comes across so that recursion is bounded by the whole
    /// chain of calls rather than restarting at every frame. The `$random`
    /// stream deliberately does not: a frame is thrown away, so advancing a
    /// stream in one would be lost, which is why a function body that draws
    /// from it is rejected when it is elaborated.
    pub fn frame(&self) -> StateStore {
        StateStore {
            name_to_signal: HashMap::new(),
            name_to_memory: HashMap::new(),
            memory_journal: Vec::new(),
            journal: HashMap::new(),
            time: self.time,
            random: RandomStream::default(),
            functions: Rc::clone(&self.functions),
            call_depth: Cell::new(self.call_depth.get()),
            any_signed: false,
            any_memory: false,
            events: HashSet::new(),
            triggers: Vec::new(),
            // A frame holds only the call's own variables, and a function body
            // may not install a drive — nothing here can be forced.
            drives: Rc::new(Vec::new()),
        }
    }

    /// Whether an ordinary write to `name` lands, or is swallowed by something
    /// driving the signal harder.
    ///
    /// This is the whole precedence rule, and it is asked of **every** write —
    /// so the case it is tuned for is the one where the design forces nothing,
    /// which it answers with a length compare and no hashing at all.
    #[inline]
    pub fn permits_write(&self, name: &str, level: DriveLevel) -> bool {
        self.drives.is_empty() || self.strongest_drive(name) <= level
    }

    /// The strongest drive installed on `name`, or
    /// [`DriveLevel::Procedural`] — what an ordinary write is — when nothing
    /// drives it.
    fn strongest_drive(&self, name: &str) -> DriveLevel {
        self.drives
            .iter()
            .filter(|drive| drive.name == name)
            .map(|drive| drive.level)
            .max()
            .unwrap_or(DriveLevel::Procedural)
    }

    /// Whether anything at all is forced or procedurally assigned. `false` is
    /// exact, and is what keeps the drives off a design that uses none.
    pub fn has_drives(&self) -> bool {
        !self.drives.is_empty()
    }

    /// How many drives are installed, which is how many more rounds the
    /// continuous-assignment fixpoint may need.
    pub fn drive_count(&self) -> usize {
        self.drives.len()
    }

    /// The installed drives, as a handle the caller may hold while writing
    /// through the store — which is exactly what re-evaluating them needs.
    pub fn drives(&self) -> Rc<Vec<Drive>> {
        Rc::clone(&self.drives)
    }

    /// The drive of `level` installed on `name`, if there is one.
    pub fn drive(&self, name: &str, level: DriveLevel) -> Option<&Drive> {
        self.drives
            .iter()
            .find(|drive| drive.name == name && drive.level == level)
    }

    /// Installs a drive, replacing any of the same strength on the same signal.
    ///
    /// Re-`force`ing an already forced signal keeps what the *first* force
    /// displaced: that is the value a `release` has to put back, and the
    /// intervening one never reached the signal.
    pub fn install_drive(&mut self, drive: Drive) {
        let drives = Rc::make_mut(&mut self.drives);
        match drives
            .iter_mut()
            .find(|existing| existing.name == drive.name && existing.level == drive.level)
        {
            Some(existing) => {
                existing.target = drive.target;
                existing.value = drive.value;
            }
            None => drives.push(drive),
        }
    }

    /// Takes the drive of `level` off `name`, handing it back so that a
    /// `release` can read what it displaced.
    pub fn remove_drive(&mut self, name: &str, level: DriveLevel) -> Option<Drive> {
        let position = self
            .drives
            .iter()
            .position(|drive| drive.name == name && drive.level == level)?;
        Some(Rc::make_mut(&mut self.drives).remove(position))
    }

    /// Records a function the design declared, under its qualified name.
    pub fn declare_function(&mut self, name: impl Into<String>, definition: FunctionDefinition) {
        Rc::make_mut(&mut self.functions).insert(name.into(), definition);
    }

    /// The definition a call resolves against, if the design declares one.
    pub fn function(&self, name: &str) -> Option<&FunctionDefinition> {
        self.functions.get(name)
    }

    /// Counts one more call onto the stack, or reports that the chain of calls
    /// has gone too deep to be anything but runaway recursion.
    ///
    /// The count comes back down when the guard is dropped, so an error on the
    /// way out of a call unwinds it exactly as a value does.
    pub fn enter_call(&self) -> Option<CallGuard<'_>> {
        if self.call_depth.get() >= MAX_CALL_DEPTH {
            return None;
        }
        self.call_depth.set(self.call_depth.get() + 1);
        Some(CallGuard(self))
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
        self.memory_journal.clear();
        self.triggers.clear();
    }

    /// Records a named event: `event done;`.
    ///
    /// It goes in a namespace of its own rather than into the signal map,
    /// because an event has no value to hold and reading one has to be an
    /// error rather than a number.
    pub fn declare_event(&mut self, name: impl Into<String>) {
        self.events.insert(name.into());
    }

    /// Whether `name` was declared as an event.
    pub fn is_event(&self, name: &str) -> bool {
        !self.events.is_empty() && self.events.contains(name)
    }

    /// Whether the design declares any event at all. `false` is exact, and it
    /// is what keeps the settle loop from asking anything else of a design
    /// that has none.
    pub fn any_event(&self) -> bool {
        !self.events.is_empty()
    }

    /// Fires a named event, reporting whether the name was one.
    ///
    /// Nothing is stored but the fact that it happened: the settle loop takes
    /// the trigger, turns it into an edge, and the event is over. Triggering
    /// the same event twice before a round takes them wakes a block once,
    /// which is what two triggers inside one time step mean.
    pub fn trigger_event(&mut self, name: &str) -> bool {
        if !self.is_event(name) {
            return false;
        }
        if !self.triggers.iter().any(|fired| fired == name) {
            self.triggers.push(name.to_string());
        }
        true
    }

    /// Every event triggered since the last call, clearing the journal so the
    /// next round is measured from here. This is what makes a trigger wake a
    /// block exactly once: the round that takes it is the only round that can
    /// see it.
    pub fn take_triggers(&mut self) -> Vec<String> {
        std::mem::take(&mut self.triggers)
    }

    /// Declares a signal over `(msb, lsb)`, initialized to all `x` the way an
    /// unassigned Verilog `reg` starts out.
    pub fn declare(&mut self, name: impl Into<String>, range: (i64, i64)) {
        self.declare_signed(name, range, false);
    }

    /// [`declare`](StateStore::declare) for a signal whose declaration carried
    /// a `signed` qualifier.
    pub fn declare_signed(&mut self, name: impl Into<String>, range: (i64, i64), signed: bool) {
        self.declare_filled(name, range, signed, Register::unknown);
    }

    /// Declares a *net* — a `wire`, `tri` or a port backed by one — which
    /// starts at `z` rather than `x`.
    ///
    /// The difference is not cosmetic: a variable with no assignment holds `x`
    /// because nothing has said what it is, while a net with no driver holds
    /// `z` because nothing is driving it, and `z` is what a reader sees. So an
    /// undriven bit of `out` reads `z` where an untouched `reg` reads `x` —
    /// which is what iverilog prints, and what a three-state bus depends on.
    pub fn declare_net(&mut self, name: impl Into<String>, range: (i64, i64), signed: bool) {
        self.declare_filled(name, range, signed, Register::high_impedance);
    }

    fn declare_filled(
        &mut self,
        name: impl Into<String>,
        range: (i64, i64),
        signed: bool,
        fill: fn(usize) -> Register,
    ) {
        let name = name.into();
        self.record(&name);
        self.any_signed |= signed;
        let register = fill(range_width(range));
        self.name_to_signal.insert(
            name,
            SignalState::with_range(register, range).with_signedness(signed),
        );
    }

    /// Declares a memory of `addresses` words, each over `range`, every bit
    /// `x` the way an unassigned Verilog `reg` starts out.
    ///
    /// The name goes into the memory map instead of the signal map, and that is
    /// the only record anything downstream has of the declaration having had an
    /// address dimension — it is what makes `mem[3]` a word select and `a[3]` a
    /// bit select.
    pub fn declare_memory(
        &mut self,
        name: impl Into<String>,
        addresses: (i64, i64),
        range: (i64, i64),
        signed: bool,
    ) {
        self.any_signed |= signed;
        self.any_memory = true;
        self.name_to_memory
            .insert(name.into(), Memory::new(addresses, range, signed));
    }

    /// Whether the design declares any memory. `false` is exact.
    pub fn any_memory(&self) -> bool {
        self.any_memory
    }

    /// The memory `name` declares, if it is a memory rather than a signal.
    pub fn memory(&self, name: &str) -> Option<&Memory> {
        self.name_to_memory.get(name)
    }

    /// Writes one word of a memory, reporting whether the stored value moved —
    /// or `None` when `name` is not a memory at all.
    ///
    /// The write is journalled, because a block may be sensitive to a memory
    /// (`always @(vco_tap[index])` wakes when `index` is a memory word that
    /// moves). It is journalled *per name* rather than per word: the pair kept
    /// is the first word displaced this round and the last word written, which
    /// over-approximates in exactly the direction `event_fires` already does —
    /// a block may be woken more often than it should, never less.
    pub fn set_word(&mut self, name: &str, address: i64, value: &Register) -> Option<bool> {
        let memory = self.name_to_memory.get_mut(name)?;
        let before = memory.word(Some(address));
        if !memory.set_word(address, value) {
            return Some(false);
        }
        let after = memory.word(Some(address));
        match self
            .memory_journal
            .iter_mut()
            .find(|(written, _, _)| written == name)
        {
            Some((_, _, latest)) => *latest = after,
            None => self.memory_journal.push((name.to_string(), before, after)),
        }
        Some(true)
    }

    /// Every memory written since the last call, as `(name, before, after)`,
    /// clearing the journal so the next round is measured from here.
    pub fn take_memory_changes(&mut self) -> Vec<(String, Register, Register)> {
        let mut changes = std::mem::take(&mut self.memory_journal);
        changes.sort_by(|left, right| left.0.cmp(&right.0));
        changes
    }

    /// The signedness a write to `name` has to keep: the one the signal was
    /// declared with, since a value cannot change a declaration. A name that
    /// does not exist yet is being declared by this very write, so it takes the
    /// signedness of the value instead.
    fn declared_signedness(&self, name: &str, register: &Register) -> bool {
        self.name_to_signal
            .get(name)
            .map(|signal| signal.is_signed())
            .unwrap_or_else(|| register.is_signed())
    }

    /// Sets a signal's value. A previously declared range is preserved when the
    /// widths still agree; otherwise the signal is (re)declared as `(width - 1, 0)`.
    pub fn set(&mut self, name: impl Into<String>, register: Register) {
        let name = name.into();
        self.record(&name);
        let signed = self.declared_signedness(&name, &register);
        self.any_signed |= signed;
        let range = self
            .name_to_signal
            .get(&name)
            .map(|signal| signal.range())
            .filter(|&range| range_width(range) == register.width());
        let signal = match range {
            Some(range) => SignalState::with_range(register, range),
            None => SignalState::new(register),
        };
        self.name_to_signal
            .insert(name, signal.with_signedness(signed));
    }

    /// Sets a signal's value and declared range in one step.
    pub fn set_ranged(&mut self, name: impl Into<String>, register: Register, range: (i64, i64)) {
        let name = name.into();
        self.record(&name);
        let signed = self.declared_signedness(&name, &register);
        self.any_signed |= signed;
        self.name_to_signal.insert(
            name,
            SignalState::with_range(register, range).with_signedness(signed),
        );
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

/// One call's place on the stack, which it gives back when it is dropped.
pub struct CallGuard<'a>(&'a StateStore);

impl Drop for CallGuard<'_> {
    fn drop(&mut self) {
        self.0.call_depth.set(self.0.call_depth.get() - 1);
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
    fn test_memory_holds_one_register_per_declared_address() {
        let mut store = StateStore::new();
        store.declare_memory("mem", (0, 255), (7, 0), false);

        let memory = store.memory("mem").expect("mem should be a memory");
        assert_eq!(memory.depth(), 256);
        assert_eq!(memory.width(), 8);
        assert_eq!(memory.addresses(), (0, 255));
        assert_eq!(memory.range(), (7, 0));
        // A name is a signal or a memory, never both — which is exactly what
        // tells a word select from a bit select.
        assert!(store.get_signal("mem").is_none());
        assert!(store.any_memory());
    }

    #[test]
    fn test_memory_words_start_unknown_and_are_independent() {
        let mut store = StateStore::new();
        store.declare_memory("mem", (0, 3), (7, 0), false);

        assert_eq!(
            store.memory("mem").unwrap().word(Some(0)).to_binary(),
            "xxxxxxxx"
        );

        store.set_word("mem", 1, &Register::from_binary("00000001"));
        store.set_word("mem", 2, &Register::from_binary("00000010"));

        let memory = store.memory("mem").unwrap();
        assert_eq!(memory.word(Some(0)).to_binary(), "xxxxxxxx");
        assert_eq!(memory.word(Some(1)).to_binary(), "00000001");
        assert_eq!(memory.word(Some(2)).to_binary(), "00000010");
        assert_eq!(memory.word(Some(3)).to_binary(), "xxxxxxxx");
    }

    #[test]
    fn test_memory_addresses_run_ascending_or_descending() {
        let ascending = Memory::new((0, 3), (7, 0), false);
        assert_eq!(ascending.word_position(0), Some(0));
        assert_eq!(ascending.word_position(3), Some(3));
        assert_eq!(ascending.word_position(4), None);
        assert_eq!(ascending.word_position(-1), None);

        // `reg [7:0] m [15:8];` addresses 15 down to 8, and nothing else.
        let descending = Memory::new((15, 8), (7, 0), false);
        assert_eq!(descending.word_position(15), Some(0));
        assert_eq!(descending.word_position(8), Some(7));
        assert_eq!(descending.word_position(7), None);
        assert_eq!(descending.word_position(16), None);
    }

    #[test]
    fn test_memory_out_of_range_reads_x_and_discards_a_write() {
        let mut store = StateStore::new();
        store.declare_memory("mem", (0, 3), (3, 0), false);

        assert_eq!(
            store.set_word("mem", 9, &Register::from_binary("1111")),
            Some(false)
        );
        let memory = store.memory("mem").unwrap();
        assert_eq!(memory.word(Some(9)).to_binary(), "xxxx");
        // An index that did not evaluate to a number reads `x` as well.
        assert_eq!(memory.word(None).to_binary(), "xxxx");
        assert!(memory.words.iter().all(|word| word.to_binary() == "xxxx"));
    }

    #[test]
    fn test_memory_write_is_resized_and_keeps_the_declared_signedness() {
        let mut store = StateStore::new();
        store.declare_memory("mem", (0, 1), (31, 0), true);

        store.set_word("mem", 0, &Register::from_binary("1010"));
        let word = store.memory("mem").unwrap().word(Some(0));
        assert_eq!(word.width(), 32);
        assert!(word.is_signed(), "a value may not change a declaration");
    }

    #[test]
    fn test_memory_writes_are_journalled_so_a_block_can_wake_on_them() {
        let mut store = StateStore::new();
        store.declare_memory("mem", (0, 3), (3, 0), false);
        store.clear_changes();

        // Rewriting the same value is not a change and is not journalled.
        store.set_word("mem", 0, &Register::from_binary("0001"));
        store.set_word("mem", 0, &Register::from_binary("0001"));
        store.set_word("mem", 1, &Register::from_binary("0010"));

        // One entry per *name*, not per word: the pair is the first word the
        // round displaced and the last word written.
        let changes = store.take_memory_changes();
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].0, "mem");
        assert_eq!(changes[0].1.to_binary(), "xxxx");
        assert_eq!(changes[0].2.to_binary(), "0010");
        assert!(store.take_memory_changes().is_empty());
    }

    #[test]
    fn test_writing_a_word_of_something_that_is_not_a_memory_is_reported() {
        let mut store = StateStore::new();
        store.declare("plain", (7, 0));

        assert_eq!(
            store.set_word("plain", 0, &Register::from_binary("1")),
            None
        );
        assert_eq!(
            store.set_word("absent", 0, &Register::from_binary("1")),
            None
        );
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

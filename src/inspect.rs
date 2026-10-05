//! Inspecting and stepping a simulation from an interactive client.
//!
//! A [`Session`] wraps a set-up [`Simulator`] and gives a waveform viewer, a
//! debugger or a structural browser the four things it needs, none of them
//! tied to any one front end:
//!
//! - **Enumeration.** [`Session::signals`] lists every variable, net, real,
//!   memory and named event the design declares, by a stable ID, with its
//!   scope, kind, width, declared range and signedness.
//! - **Values.** [`Session::value`] and [`Session::word`] read the four-state
//!   value of one of them now.
//! - **Change batches.** [`Session::subscribe`] names the IDs a client is
//!   drawing, and [`Session::take_changes`] hands back what they did since the
//!   last call — bounded, with a count of what was dropped rather than an
//!   unbounded buffer.
//! - **Run control.** [`Session::run`] and [`Session::step_timestep`] advance
//!   the design under limits, a cancellation flag and breakpoints, and say
//!   exactly why they stopped.
//!
//! **An ID is the full hierarchical name, top module included**, dot-joined:
//! `tb.dut.count`. That is exactly what a VCD `$scope` path makes of a
//! variable, and the key [`Waveform::traces`](crate::waveform::Waveform) reads
//! it back under — so an ID from here names the same trace in a dump the
//! design wrote. The flat store key is the ID without its leading top segment
//! (`dut.count`). A port aliased onto its parent's signal has no store entry
//! of its own but is listed under its own ID, with [`SignalInfo::storage`]
//! naming the ID of the signal it shares. A name with a segment starting `$` —
//! a `repeat` counter, an intra-assignment hold, an automatic task's
//! activation slot — is the simulator's bookkeeping and is not listed, which
//! is the rule the waveform dump follows too.
//!
//! **Everything here happens at a settled timestamp boundary.** A session
//! advances one timestamp at a time, and a timestamp ends only once the design
//! has stopped moving at that instant: every delta cycle has run, every
//! non-blocking update has landed and the continuous assignments have settled
//! — the moment `$strobe`, `$monitor` and the waveform dump report at. That is
//! the only moment a change is recorded, a breakpoint is evaluated, a value is
//! read between runs, or a run stops. So `a = 1; a = 0;` within one timestamp
//! is not a change, a condition that holds only part way through a timestamp
//! never fires, and pausing a run — by a step budget, a breakpoint or the
//! cancellation flag — leaves the design exactly where an uninterrupted run
//! would have been at that instant. Resuming then gives the same output, the
//! same values and the same waveform as never having paused.

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};

use serde::{Deserialize, Serialize};

use crate::parsers::expr::{verilog_expression, Expression};
use crate::register::Register;
use crate::run::{error_diagnostic, error_stop, load, stop_code, LoadError, RunConfig, StopReason};
use crate::simulator::elaborate::rename_expression;
use crate::simulator::eval::{eval, truth};
use crate::simulator::runner::{SimulationError, Simulator};
use crate::simulator::state_store::StateStore;

/// How many changes [`Session::take_changes`] holds before it starts counting
/// drops instead, unless [`Session::set_change_capacity`] says otherwise.
pub const DEFAULT_CHANGE_CAPACITY: usize = 65_536;

/// What a listed name is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SignalKind {
    /// A `wire` and the rest of the net types: undriven, it reads `z`.
    Net,
    /// A `reg`, `integer`, `time` or parameter: unwritten, it reads `x`.
    Variable,
    /// A `real` or `realtime`: sixty-four bits read as a double.
    Real,
    /// An array of words, read one word at a time with [`Session::word`].
    Memory,
    /// A named event, which has no value at all.
    Event,
}

/// One name the design declares.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SignalInfo {
    /// The stable ID: the full hierarchical name, top module first.
    pub id: String,
    /// The ID of the scope it is declared in — an instance, a generate block,
    /// a named block or a task. The top module's own names have the top as
    /// their scope.
    pub scope: String,
    /// The last segment of the ID.
    pub name: String,
    pub kind: SignalKind,
    /// Bits in the value, or in one word of a memory. Zero for an event.
    pub width: usize,
    /// The declared `(msb, lsb)`, or of one word of a memory. `None` for an
    /// event.
    pub range: Option<(i64, i64)>,
    pub signed: bool,
    /// A memory's address ranges, outermost first. Empty for anything else.
    pub dimensions: Vec<(i64, i64)>,
    /// The ID of the storage this name reads: its own ID, or — for a port
    /// aliased onto its parent's signal — that signal's.
    pub storage: String,
}

/// A four-state value as a client sees it.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
pub enum Value {
    /// Most significant bit first, one of `0`, `1`, `x`, `z` per bit.
    Bits(String),
    Real(f64),
}

impl Value {
    pub fn from_register(register: &Register) -> Value {
        if register.is_real() {
            Value::Real(register.to_f64())
        } else {
            Value::Bits(register.to_binary())
        }
    }

    /// The value as a register `width` bits wide: bits are zero extended or
    /// truncated, and a real is its sixty-four bit encoding.
    fn to_register(&self, width: usize) -> Result<Register, InspectError> {
        match self {
            Value::Real(real) => Ok(Register::from_f64(*real)),
            Value::Bits(bits) => {
                if bits.is_empty() || !bits.chars().all(|c| matches!(c, '0' | '1' | 'x' | 'z')) {
                    return Err(InspectError::BadValue(bits.clone()));
                }
                Ok(Register::from_binary(bits).resize(width))
            }
        }
    }
}

/// One subscribed ID taking a new value at a settled timestamp.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Change {
    pub id: String,
    /// Simulated time, in ticks of the simulation clock.
    pub time: i64,
    pub value: Value,
}

/// What [`Session::take_changes`] hands back.
#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize)]
pub struct ChangeBatch {
    /// In time order, and by ID within one timestamp.
    pub changes: Vec<Change>,
    /// How many changes were not kept because the batch was full. When this
    /// is not zero the batch is incomplete, and [`Session::value`] is the way
    /// to catch up: it always answers with the value now.
    pub dropped: u64,
}

/// What a breakpoint waits for. Every condition is judged at a settled
/// timestamp boundary against the one before it, and fires on the boundary
/// where it *becomes* true — so a run resumed from a hit does not stop again
/// at once on the same condition.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Condition {
    /// The ID's value differs from what it was at the previous boundary.
    Changes(String),
    /// The ID's value is exactly this — `x` and `z` bits included — having
    /// not been at the previous boundary. Bits are zero extended or truncated
    /// to the signal's width.
    Equals(String, Value),
    /// A Verilog expression is true — has a `1` bit, or is a non-zero real —
    /// having not been at the previous boundary. An expression that is `x`
    /// is not true. Names may be written as IDs (`tb.dut.count`) or as the
    /// flat store key (`dut.count`).
    Expression(String),
}

/// Names one breakpoint, for [`Session::remove_breakpoint`] and in
/// [`Stop::hits`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct BreakpointId(pub usize);

/// Bounds on one call to [`Session::run`]. Both are optional; a run with
/// neither goes until the design finishes, runs out of events, hits a
/// breakpoint or is cancelled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Limits {
    /// Do not run a timestamp later than this, in clock ticks. The run stops
    /// with [`StopReason::TimeLimit`] before it would.
    pub until: Option<i64>,
    /// Run at most this many timestamps in this call, then stop with
    /// [`StopReason::StepLimit`].
    pub steps: Option<u64>,
}

/// Why and where a run stopped.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Stop {
    pub reason: StopReason,
    /// Simulated time at the stop, in clock ticks.
    pub time: i64,
    /// Timestamps this call ran.
    pub steps: u64,
    /// The breakpoints that fired on the last timestamp, when `reason` is
    /// [`StopReason::Breakpoint`].
    pub hits: Vec<BreakpointId>,
    /// What went wrong, when the run stopped on an error.
    pub error: Option<String>,
}

/// Why a question put to a [`Session`] could not be answered.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InspectError {
    /// No listed name has this ID.
    UnknownSignal(String),
    /// The ID names something with no single value: a memory (read it a word
    /// at a time) or an event.
    NotAValue { id: String, kind: SignalKind },
    /// [`Session::word`] was asked of something that is not a memory, or with
    /// the wrong number of indices.
    NotAWord(String),
    /// A value handed in was not `0`/`1`/`x`/`z` bits.
    BadValue(String),
    /// A breakpoint expression did not parse or does not evaluate.
    Expression(String),
}

impl fmt::Display for InspectError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InspectError::UnknownSignal(id) => write!(f, "no signal has the ID `{}`", id),
            InspectError::NotAValue { id, kind } => {
                write!(f, "`{}` is a {:?} and has no single value", id, kind)
            }
            InspectError::NotAWord(detail) => write!(f, "{}", detail),
            InspectError::BadValue(bits) => write!(f, "`{}` is not a four-state bit string", bits),
            InspectError::Expression(detail) => write!(f, "{}", detail),
        }
    }
}

impl std::error::Error for InspectError {}

/// Where a listed name's value lives in the store.
#[derive(Debug, Clone)]
enum Storage {
    Signal(String),
    Memory(String),
    Event,
}

/// A subscribed store entry: the IDs a client asked for that read it, and
/// the value last reported for them.
struct Subscription {
    ids: BTreeSet<String>,
    last: Register,
}

/// A breakpoint and what it saw at the previous boundary.
enum Breakpoint {
    Changes {
        key: String,
        last: Register,
    },
    Equals {
        key: String,
        value: Register,
        held: bool,
    },
    Expression {
        expression: Expression,
        held: bool,
    },
}

impl Breakpoint {
    /// Looks at the design as it stands and reports whether this boundary is
    /// the one the condition fires on.
    fn check(&mut self, store: &StateStore) -> Result<bool, String> {
        match self {
            Breakpoint::Changes { key, last } => {
                let now = store.get(key).cloned().ok_or_else(|| key.clone())?;
                let fired = now != *last;
                *last = now;
                Ok(fired)
            }
            Breakpoint::Equals { key, value, held } => {
                let now = store.get(key).is_some_and(|now| now == value);
                let fired = now && !*held;
                *held = now;
                Ok(fired)
            }
            Breakpoint::Expression { expression, held } => {
                let now = holds(expression, store)?;
                let fired = now && !*held;
                *held = now;
                Ok(fired)
            }
        }
    }
}

fn holds(expression: &Expression, store: &StateStore) -> Result<bool, String> {
    eval(expression, store)
        .map(|value| truth(&value) == Some(true))
        .map_err(|error| error.to_string())
}

/// A design being run and inspected by an interactive client.
pub struct Session {
    simulator: Simulator,
    top: String,
    /// Sorted by ID.
    signals: Vec<SignalInfo>,
    /// ID → where its value lives.
    storage: HashMap<String, Storage>,
    /// Whether time zero has run. Until it has, the design is as elaboration
    /// left it: nothing has executed.
    started: bool,
    steps: u64,
    /// The error a run stopped on. A simulator that raised one is not in a
    /// state worth running further, so every later run reports it again.
    failure: Option<(StopReason, String)>,
    /// Store key → subscription.
    subscriptions: BTreeMap<String, Subscription>,
    changes: Vec<Change>,
    dropped: u64,
    capacity: usize,
    breakpoints: Vec<Option<Breakpoint>>,
}

impl Session {
    /// Loads, elaborates and sets up the design `config` describes, ready to
    /// run from time zero. `config`'s time and step limits are not used: a
    /// session's limits are given to each [`Session::run`].
    pub fn new(config: &RunConfig) -> Result<Session, LoadError> {
        let loaded = load(config)?;
        Session::from_simulator(loaded.simulator).map_err(|error| {
            let stop = error_stop(&error, StopReason::Elaboration);
            LoadError {
                stop,
                diagnostic: error_diagnostic(stop_code(stop), error.to_string(), None),
                sources: loaded.sources,
            }
        })
    }

    /// Sets up a simulator that has not been, and wraps it.
    pub fn from_simulator(mut simulator: Simulator) -> Result<Session, SimulationError> {
        simulator.setup()?;
        let top = simulator.top().to_string();
        let (signals, storage) = enumerate(&top, simulator.store(), simulator.aliases());
        Ok(Session {
            simulator,
            top,
            signals,
            storage,
            started: false,
            steps: 0,
            failure: None,
            subscriptions: BTreeMap::new(),
            changes: Vec::new(),
            dropped: 0,
            capacity: DEFAULT_CHANGE_CAPACITY,
            breakpoints: Vec::new(),
        })
    }

    /// The simulator underneath, for what this module does not wrap —
    /// [`Simulator::output`], [`Simulator::ticks_per_unit`] and the rest.
    pub fn simulator(&self) -> &Simulator {
        &self.simulator
    }

    /// The name of the top module, which is every ID's first segment.
    pub fn top(&self) -> &str {
        &self.top
    }

    /// Simulated time, in ticks of the simulation clock.
    pub fn now(&self) -> i64 {
        self.simulator.now()
    }

    /// Timestamps run so far, time zero included.
    pub fn steps(&self) -> u64 {
        self.steps
    }

    /// Whether the design has called `$finish`.
    pub fn finished(&self) -> bool {
        self.simulator.finished()
    }

    /// Everything the design declares, sorted by ID. The list is fixed at
    /// elaboration.
    pub fn signals(&self) -> &[SignalInfo] {
        &self.signals
    }

    /// One listed name.
    pub fn signal(&self, id: &str) -> Option<&SignalInfo> {
        self.signals
            .binary_search_by(|info| info.id.as_str().cmp(id))
            .ok()
            .map(|index| &self.signals[index])
    }

    /// Every scope some listed name sits in, and every scope above those,
    /// sorted: the tree a structural browser draws.
    pub fn scopes(&self) -> Vec<String> {
        let mut scopes = BTreeSet::new();
        for info in &self.signals {
            let mut scope = info.scope.as_str();
            loop {
                scopes.insert(scope.to_string());
                match scope.rsplit_once('.') {
                    Some((parent, _)) => scope = parent,
                    None => break,
                }
            }
        }
        scopes.into_iter().collect()
    }

    /// The value `id` holds now.
    pub fn value(&self, id: &str) -> Result<Value, InspectError> {
        let key = self.signal_key(id)?;
        let register = self
            .simulator
            .store()
            .get(key)
            .ok_or_else(|| InspectError::UnknownSignal(id.to_string()))?;
        Ok(Value::from_register(register))
    }

    /// One word of the memory `id`, at `address` — one index per dimension.
    /// An address outside the declared range reads `x`, as it does in the
    /// design.
    pub fn word(&self, id: &str, address: &[i64]) -> Result<Value, InspectError> {
        let Some(Storage::Memory(key)) = self.storage.get(id) else {
            return Err(match self.storage.get(id) {
                None => InspectError::UnknownSignal(id.to_string()),
                Some(_) => InspectError::NotAWord(format!("`{}` is not a memory", id)),
            });
        };
        let memory = self
            .simulator
            .store()
            .memory(key)
            .ok_or_else(|| InspectError::UnknownSignal(id.to_string()))?;
        if address.len() != memory.dimensions() {
            return Err(InspectError::NotAWord(format!(
                "`{}` has {} dimensions and was given {} indices",
                id,
                memory.dimensions(),
                address.len()
            )));
        }
        Ok(Value::from_register(&memory.word(Some(address))))
    }

    /// Starts reporting changes of each ID in [`Session::take_changes`].
    /// Nothing is reported for the value an ID holds when it is subscribed;
    /// [`Session::value`] is the starting point. Only a net, a variable or a
    /// real can be subscribed. On an error nothing is subscribed.
    pub fn subscribe<'a>(
        &mut self,
        ids: impl IntoIterator<Item = &'a str>,
    ) -> Result<(), InspectError> {
        let mut keys = Vec::new();
        for id in ids {
            keys.push((id.to_string(), self.signal_key(id)?.to_string()));
        }
        for (id, key) in keys {
            let current = self.simulator.store().get(&key).cloned();
            let Some(current) = current else { continue };
            self.subscriptions
                .entry(key)
                .or_insert_with(|| Subscription {
                    ids: BTreeSet::new(),
                    last: current,
                })
                .ids
                .insert(id);
        }
        self.simulator.tap_writes(!self.subscriptions.is_empty());
        Ok(())
    }

    /// Stops reporting changes of `id`. Unsubscribing the last ID stops the
    /// simulator recording writes at all.
    pub fn unsubscribe(&mut self, id: &str) {
        self.subscriptions.retain(|_, subscription| {
            subscription.ids.remove(id);
            !subscription.ids.is_empty()
        });
        self.simulator.tap_writes(!self.subscriptions.is_empty());
    }

    /// How many changes a batch may hold before later ones are dropped and
    /// counted instead.
    pub fn set_change_capacity(&mut self, capacity: usize) {
        self.capacity = capacity;
    }

    /// The changes to subscribed IDs since the last call, and how many were
    /// dropped because the batch was full.
    pub fn take_changes(&mut self) -> ChangeBatch {
        ChangeBatch {
            changes: std::mem::take(&mut self.changes),
            dropped: std::mem::take(&mut self.dropped),
        }
    }

    /// Adds a breakpoint. It starts from the design as it stands, so a
    /// condition already true now fires only once it has stopped being true
    /// and become true again.
    pub fn add_breakpoint(&mut self, condition: Condition) -> Result<BreakpointId, InspectError> {
        let store = self.simulator.store();
        let breakpoint = match condition {
            Condition::Changes(id) => {
                let key = self.signal_key(&id)?.to_string();
                let last = store
                    .get(&key)
                    .cloned()
                    .expect("a listed signal has a value");
                Breakpoint::Changes { key, last }
            }
            Condition::Equals(id, value) => {
                let key = self.signal_key(&id)?.to_string();
                let current = store.get(&key).expect("a listed signal has a value");
                let value = value.to_register(current.width())?;
                let held = *current == value;
                Breakpoint::Equals { key, value, held }
            }
            Condition::Expression(text) => {
                let expression = self.parse_expression(&text)?;
                let held = holds(&expression, store).map_err(|error| {
                    InspectError::Expression(format!("`{}` does not evaluate: {}", text, error))
                })?;
                Breakpoint::Expression { expression, held }
            }
        };
        self.breakpoints.push(Some(breakpoint));
        Ok(BreakpointId(self.breakpoints.len() - 1))
    }

    /// Removes a breakpoint, reporting whether there was one.
    pub fn remove_breakpoint(&mut self, id: BreakpointId) -> bool {
        self.breakpoints
            .get_mut(id.0)
            .and_then(Option::take)
            .is_some()
    }

    /// Runs exactly one timestamp — time zero, the first time — unless the
    /// design has already stopped.
    pub fn step_timestep(&mut self) -> Stop {
        self.run(
            Limits {
                until: None,
                steps: Some(1),
            },
            None,
        )
    }

    /// Runs timestamp by timestamp until something stops it, checking in this
    /// order before each one: the design has finished, `cancel` is set,
    /// nothing is left scheduled, the next timestamp is past
    /// [`Limits::until`], or this call has used [`Limits::steps`]. After each
    /// timestamp the subscribed changes are recorded and the breakpoints are
    /// evaluated; any that fire stop the run there.
    ///
    /// Every stop is at a settled timestamp boundary, so a run may be resumed
    /// by calling this again and ends exactly where one uninterrupted run
    /// would. `cancel` is only read, so another thread may set it.
    pub fn run(&mut self, limits: Limits, cancel: Option<&AtomicBool>) -> Stop {
        let mut steps = 0;
        loop {
            if let Some((reason, error)) = &self.failure {
                return self.stop(*reason, steps, Vec::new(), Some(error.clone()));
            }
            if self.simulator.finished() {
                return self.stop(StopReason::Finished, steps, Vec::new(), None);
            }
            if cancel.is_some_and(|flag| flag.load(Ordering::Relaxed)) {
                return self.stop(StopReason::Cancelled, steps, Vec::new(), None);
            }
            let next = if self.started {
                self.simulator.next_time()
            } else {
                Some(0)
            };
            let Some(next) = next else {
                return self.stop(StopReason::Quiescent, steps, Vec::new(), None);
            };
            if limits.until.is_some_and(|until| next > until) {
                return self.stop(StopReason::TimeLimit, steps, Vec::new(), None);
            }
            if limits.steps.is_some_and(|limit| steps >= limit) {
                return self.stop(StopReason::StepLimit, steps, Vec::new(), None);
            }

            let advanced = self.simulator.advance(next - self.simulator.now());
            self.started = true;
            self.steps += 1;
            steps += 1;
            if let Err(error) = advanced {
                let reason = error_stop(&error, StopReason::Runtime);
                self.failure = Some((reason, error.to_string()));
                continue;
            }
            self.record_changes();
            match self.check_breakpoints() {
                Ok(hits) if hits.is_empty() => {}
                Ok(hits) => return self.stop(StopReason::Breakpoint, steps, hits, None),
                Err(error) => {
                    return self.stop(StopReason::Runtime, steps, Vec::new(), Some(error))
                }
            }
        }
    }

    fn stop(
        &self,
        reason: StopReason,
        steps: u64,
        hits: Vec<BreakpointId>,
        error: Option<String>,
    ) -> Stop {
        Stop {
            reason,
            time: self.simulator.now(),
            steps,
            hits,
            error,
        }
    }

    /// The store key of a listed name that holds a single value.
    fn signal_key(&self, id: &str) -> Result<&str, InspectError> {
        match self.storage.get(id) {
            Some(Storage::Signal(key)) => Ok(key),
            Some(_) => Err(InspectError::NotAValue {
                id: id.to_string(),
                kind: self.signal(id).map_or(SignalKind::Event, |info| info.kind),
            }),
            None => Err(InspectError::UnknownSignal(id.to_string())),
        }
    }

    /// Records the timestamp just run: every subscribed entry the design
    /// wrote, whose value really differs from the one last reported. The
    /// names come from the simulator's write tap, so this costs the entries
    /// written rather than the entries subscribed.
    fn record_changes(&mut self) {
        if self.subscriptions.is_empty() {
            return;
        }
        let written = self.simulator.take_written();
        let time = self.simulator.now();
        let store = self.simulator.store();
        let mut moved: Vec<Change> = Vec::new();
        for key in written {
            let Some(subscription) = self.subscriptions.get_mut(&key) else {
                continue;
            };
            let Some(now) = store.get(&key) else {
                continue;
            };
            if *now == subscription.last {
                continue;
            }
            subscription.last = now.clone();
            let value = Value::from_register(now);
            for id in &subscription.ids {
                moved.push(Change {
                    id: id.clone(),
                    time,
                    value: value.clone(),
                });
            }
        }
        moved.sort_by(|a, b| a.id.cmp(&b.id));
        for change in moved {
            if self.changes.len() < self.capacity {
                self.changes.push(change);
            } else {
                self.dropped += 1;
            }
        }
    }

    /// Evaluates every breakpoint against the timestamp just run. All of them
    /// are evaluated even once one fires, so each one's memory of the
    /// previous boundary stays current.
    fn check_breakpoints(&mut self) -> Result<Vec<BreakpointId>, String> {
        let store = self.simulator.store();
        let mut hits = Vec::new();
        let mut failure = None;
        for (index, breakpoint) in self.breakpoints.iter_mut().enumerate() {
            let Some(breakpoint) = breakpoint else {
                continue;
            };
            match breakpoint.check(store) {
                Ok(true) => hits.push(BreakpointId(index)),
                Ok(false) => {}
                Err(error) => {
                    failure.get_or_insert(format!("breakpoint {}: {}", index, error));
                }
            }
        }
        match failure {
            Some(error) => Err(error),
            None => Ok(hits),
        }
    }

    /// Parses a breakpoint expression and points every name in it at the
    /// store entry it reads.
    fn parse_expression(&self, text: &str) -> Result<Expression, InspectError> {
        let mut expression = match verilog_expression(text.trim()) {
            Ok(("", expression)) => expression,
            Ok((rest, _)) => {
                return Err(InspectError::Expression(format!(
                    "`{}` has `{}` left over after the expression",
                    text, rest
                )))
            }
            Err(error) => {
                return Err(InspectError::Expression(format!(
                    "`{}` does not parse: {}",
                    text, error
                )))
            }
        };
        let store = self.simulator.store();
        let aliases = self.simulator.aliases();
        let prefix = format!("{}.", self.top);
        let resolve = |name: &str| -> String {
            let known = |key: &str| {
                store.contains(key) || store.memory(key).is_some() || store.is_event(key)
            };
            let flat = if known(name) {
                name
            } else {
                name.strip_prefix(&prefix).unwrap_or(name)
            };
            match aliases.get(flat) {
                Some(entry) if !known(flat) => entry.clone(),
                _ => flat.to_string(),
            }
        };
        rename_expression(&mut expression, &resolve);
        Ok(expression)
    }
}

/// Whether a flat store key is the simulator's own bookkeeping rather than a
/// name the design declared.
fn hidden(key: &str) -> bool {
    key.starts_with('$') || key.contains(".$")
}

/// The ID of a flat store key.
fn id_of(top: &str, key: &str) -> String {
    format!("{}.{}", top, key)
}

/// Lists everything the store holds under an ID, plus every aliased port.
fn enumerate(
    top: &str,
    store: &StateStore,
    aliases: &HashMap<String, String>,
) -> (Vec<SignalInfo>, HashMap<String, Storage>) {
    let mut signals = Vec::new();
    let mut storage = HashMap::new();
    let info = |key: &str, kind, width, range, signed, dimensions, storage: String| {
        let id = id_of(top, key);
        let (scope, name) = id.rsplit_once('.').expect("an ID has a top segment");
        SignalInfo {
            scope: scope.to_string(),
            name: name.to_string(),
            id: id.clone(),
            kind,
            width,
            range,
            signed,
            dimensions,
            storage,
        }
    };
    let signal = |key: &str, id: String| {
        let state = store.get_signal(key)?;
        let kind = if state.is_real() {
            SignalKind::Real
        } else if state.is_net() {
            SignalKind::Net
        } else {
            SignalKind::Variable
        };
        Some((
            kind,
            state.width(),
            Some(state.range()),
            state.is_signed(),
            id,
        ))
    };

    for key in store.names().into_iter().filter(|key| !hidden(key)) {
        if let Some((kind, width, range, signed, storage_id)) = signal(key, id_of(top, key)) {
            signals.push(info(
                key,
                kind,
                width,
                range,
                signed,
                Vec::new(),
                storage_id,
            ));
            storage.insert(id_of(top, key), Storage::Signal(key.to_string()));
        }
    }
    for (alias, entry) in aliases {
        if hidden(alias) || store.contains(alias) {
            continue;
        }
        if let Some((kind, width, range, signed, storage_id)) = signal(entry, id_of(top, entry)) {
            signals.push(info(
                alias,
                kind,
                width,
                range,
                signed,
                Vec::new(),
                storage_id,
            ));
            storage.insert(id_of(top, alias), Storage::Signal(entry.clone()));
        }
    }
    for key in store.memory_names().into_iter().filter(|key| !hidden(key)) {
        let memory = store.memory(key).expect("a listed memory exists");
        signals.push(info(
            key,
            SignalKind::Memory,
            memory.width(),
            Some(memory.range()),
            memory.is_signed(),
            memory.addresses().to_vec(),
            id_of(top, key),
        ));
        storage.insert(id_of(top, key), Storage::Memory(key.to_string()));
    }
    for key in store.event_names().into_iter().filter(|key| !hidden(key)) {
        signals.push(info(
            key,
            SignalKind::Event,
            0,
            None,
            false,
            Vec::new(),
            id_of(top, key),
        ));
        storage.insert(id_of(top, key), Storage::Event);
    }
    signals.sort_by(|a, b| a.id.cmp(&b.id));
    (signals, storage)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::source::{parse_source, root_module};
    use crate::waveform::Waveform;
    use std::path::PathBuf;

    fn simulator(source: &str) -> Simulator {
        let parsed = parse_source(source).expect("the design parses");
        let top = root_module(&parsed.modules).expect("the design has a root");
        Simulator::with_modules(parsed.modules, top)
    }

    fn session(source: &str) -> Session {
        Session::from_simulator(simulator(source)).expect("the design sets up")
    }

    fn bits(text: &str) -> Value {
        Value::Bits(text.to_string())
    }

    /// A scratch directory of its own for a test that writes a dump.
    fn scratch(name: &str) -> PathBuf {
        let dir =
            std::env::temp_dir().join(format!("visilog-inspect-{}-{}", std::process::id(), name));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("the scratch directory is made");
        dir
    }

    const HIERARCHY: &str = "
        module child(input clk, output reg [3:0] q);
          always @(posedge clk) q <= q + 1;
        endmodule
        module tb;
          parameter WIDTH = 4;
          reg clk = 0;
          wire [3:0] count;
          real r;
          reg signed [7:0] s;
          reg [7:0] mem [0:3];
          event ev;
          integer i;
          child dut(.clk(clk), .q(count));
          initial begin : blk
            reg [1:0] tmp;
            repeat (2) #1 clk = ~clk;
          end
        endmodule
    ";

    #[test]
    fn test_signals_are_listed_by_hierarchical_id() {
        let session = session(HIERARCHY);
        let signal = |id: &str| {
            session
                .signal(id)
                .unwrap_or_else(|| panic!("`{}` is listed", id))
                .clone()
        };

        let clk = signal("tb.clk");
        assert_eq!(clk.kind, SignalKind::Variable);
        assert_eq!((clk.scope.as_str(), clk.name.as_str()), ("tb", "clk"));
        assert_eq!(clk.storage, "tb.clk");

        let s = signal("tb.s");
        assert!(s.signed);
        assert_eq!((s.width, s.range), (8, Some((7, 0))));

        assert_eq!(signal("tb.r").kind, SignalKind::Real);
        assert_eq!(signal("tb.ev").kind, SignalKind::Event);
        assert_eq!(signal("tb.ev").range, None);
        let i = signal("tb.i");
        assert!(i.signed && i.width == 32);
        assert_eq!(signal("tb.WIDTH").kind, SignalKind::Variable);

        let mem = signal("tb.mem");
        assert_eq!(mem.kind, SignalKind::Memory);
        assert_eq!((mem.width, mem.dimensions.clone()), (8, vec![(0, 3)]));

        let tmp = signal("tb.blk.tmp");
        assert_eq!((tmp.scope.as_str(), tmp.width), ("tb.blk", 2));

        // A port bound to a plain identifier is its parent's signal under a
        // second name: listed under its own ID, reading the parent's storage.
        assert_eq!(signal("tb.dut.clk").storage, "tb.clk");
        assert_eq!(signal("tb.dut.q").storage, "tb.count");
        assert_eq!(signal("tb.dut.q").scope, "tb.dut");

        // The `repeat` counter is the simulator's, not the design's.
        assert!(session.signals().iter().all(|info| !info.id.contains('$')));
        assert!(session
            .signals()
            .windows(2)
            .all(|pair| pair[0].id < pair[1].id));
        assert_eq!(session.scopes(), vec!["tb", "tb.blk", "tb.dut"]);
    }

    #[test]
    fn test_values_read_through_ids_and_refuse_what_has_none() {
        let mut session = session(HIERARCHY);
        assert_eq!(session.value("tb.clk"), Ok(bits("0")));
        assert_eq!(session.value("tb.s"), Ok(bits("xxxxxxxx")));
        assert_eq!(session.value("tb.r"), Ok(Value::Real(0.0)));
        assert_eq!(session.word("tb.mem", &[2]), Ok(bits("xxxxxxxx")));
        assert!(matches!(
            session.value("tb.mem"),
            Err(InspectError::NotAValue {
                kind: SignalKind::Memory,
                ..
            })
        ));
        assert!(matches!(
            session.word("tb.mem", &[1, 2]),
            Err(InspectError::NotAWord(_))
        ));
        assert!(matches!(
            session.subscribe(["tb.clk", "tb.ev"]),
            Err(InspectError::NotAValue { .. })
        ));
        assert_eq!(
            session.value("clk"),
            Err(InspectError::UnknownSignal("clk".into()))
        );

        session.run(Limits::default(), None);
        // The alias reads what its parent holds: two posedges of `clk`.
        assert_eq!(session.value("tb.dut.q"), session.value("tb.count"));
    }

    const CLOCKED: &str = "
        module tb;
          reg clk = 0;
          reg [3:0] count = 0;
          always #5 clk = ~clk;
          always @(posedge clk) count <= count + 1;
          initial #32 $finish;
        endmodule
    ";

    #[test]
    fn test_stepping_stops_at_each_timestamp_with_its_edges() {
        let mut session = session(CLOCKED);
        session.subscribe(["tb.clk", "tb.count"]).unwrap();
        let mut times = Vec::new();
        loop {
            let stop = session.step_timestep();
            assert_eq!(stop.steps, 1);
            times.push(stop.time);
            // The timestamp that runs `$finish` says so itself.
            if stop.reason == StopReason::Finished {
                break;
            }
            assert_eq!(stop.reason, StopReason::StepLimit);
        }
        assert_eq!(times, vec![0, 5, 10, 15, 20, 25, 30, 32]);
        assert_eq!(session.steps(), 8);

        let batch = session.take_changes();
        assert_eq!(batch.dropped, 0);
        let seen: Vec<(i64, &str, Value)> = batch
            .changes
            .iter()
            .map(|change| (change.time, change.id.as_str(), change.value.clone()))
            .collect();
        assert_eq!(
            seen,
            vec![
                (5, "tb.clk", bits("1")),
                (5, "tb.count", bits("0001")),
                (10, "tb.clk", bits("0")),
                (15, "tb.clk", bits("1")),
                (15, "tb.count", bits("0010")),
                (20, "tb.clk", bits("0")),
                (25, "tb.clk", bits("1")),
                (25, "tb.count", bits("0011")),
                (30, "tb.clk", bits("0")),
            ]
        );
        assert_eq!(session.take_changes(), ChangeBatch::default());
        // A finished design stays finished.
        assert_eq!(
            session.run(Limits::default(), None).reason,
            StopReason::Finished
        );
    }

    /// Every write inside one timestamp is invisible except the value it
    /// settles on: `a` pulsing to `0` and back, and `b` given `1` then `0` by
    /// two non-blocking updates, are no change at all.
    const SAME_TIMESTAMP: &str = "
        module tb;
          reg a = 0;
          reg b = 0;
          initial begin
            #10 a = 1; a = 0; a = 1; b <= 1; b <= 0;
            #10 a = 0; a = 1;
            #10 b <= 1;
          end
        endmodule
    ";

    #[test]
    fn test_changes_within_a_timestamp_report_only_where_they_settle() {
        let mut session = session(SAME_TIMESTAMP);
        session.subscribe(["tb.a", "tb.b"]).unwrap();
        let stop = session.run(Limits::default(), None);
        assert_eq!((stop.reason, stop.time), (StopReason::Quiescent, 30));
        let changes = session.take_changes().changes;
        assert_eq!(
            changes,
            vec![
                Change {
                    id: "tb.a".into(),
                    time: 10,
                    value: bits("1")
                },
                Change {
                    id: "tb.b".into(),
                    time: 30,
                    value: bits("1")
                },
            ]
        );
    }

    #[test]
    fn test_a_breakpoint_never_sees_a_value_held_only_mid_timestamp() {
        let mut session = session(SAME_TIMESTAMP);
        // `a` is 0 again part way through time 20 and never at its end.
        session
            .add_breakpoint(Condition::Expression("tb.a == 0 && b == 0".into()))
            .unwrap();
        let hit = session
            .add_breakpoint(Condition::Changes("tb.b".into()))
            .unwrap();
        let stop = session.run(Limits::default(), None);
        assert_eq!(
            (stop.reason, stop.time, stop.hits),
            (StopReason::Breakpoint, 30, vec![hit])
        );
        assert_eq!(session.value("tb.b"), Ok(bits("1")));
        assert_eq!(
            session.run(Limits::default(), None).reason,
            StopReason::Quiescent
        );
    }

    #[test]
    fn test_breakpoints_fire_on_becoming_true_and_can_be_removed() {
        let mut session = session(CLOCKED);
        let three = session
            .add_breakpoint(Condition::Equals("tb.count".into(), bits("11")))
            .unwrap();
        let odd = session
            .add_breakpoint(Condition::Expression("count[0]".into()))
            .unwrap();
        let first = session.run(Limits::default(), None);
        assert_eq!((first.reason, first.time), (StopReason::Breakpoint, 5));
        assert_eq!(first.hits, vec![odd]);
        // `count[0]` stays 1 until 15, so resuming does not stop again at 10.
        assert!(session.remove_breakpoint(odd));
        assert!(!session.remove_breakpoint(odd));
        let second = session.run(Limits::default(), None);
        assert_eq!((second.time, second.hits), (25, vec![three]));
        assert_eq!(session.value("tb.count"), Ok(bits("0011")));
        assert!(matches!(
            session.add_breakpoint(Condition::Expression("count +".into())),
            Err(InspectError::Expression(_))
        ));
        assert!(matches!(
            session.add_breakpoint(Condition::Expression("nothing == 1".into())),
            Err(InspectError::Expression(_))
        ));
    }

    #[test]
    fn test_four_state_transitions_are_reported_with_x_and_z() {
        let mut session = session(
            "
            module tb;
              reg r;
              reg en;
              wire w;
              assign w = en ? r : 1'bz;
              initial begin
                #1 en = 0;
                #1 en = 1;
                #1 r = 0;
                #1 r = 1;
                #1 en = 1'bx;
              end
            endmodule
            ",
        );
        // A driven net holds `x` before its drivers have said anything.
        assert_eq!(session.value("tb.w"), Ok(bits("x")));
        session.subscribe(["tb.w"]).unwrap();
        session.run(Limits::default(), None);
        let seen: Vec<(i64, Value)> = session
            .take_changes()
            .changes
            .into_iter()
            .map(|change| (change.time, change.value))
            .collect();
        assert_eq!(
            seen,
            vec![
                (1, bits("z")),
                (2, bits("x")),
                (3, bits("0")),
                (4, bits("1")),
                (5, bits("x")),
            ]
        );
    }

    #[test]
    fn test_a_full_batch_counts_what_it_drops() {
        let mut session = session(CLOCKED);
        session.subscribe(["tb.clk"]).unwrap();
        session.set_change_capacity(2);
        session.run(Limits::default(), None);
        let batch = session.take_changes();
        assert_eq!(batch.changes.len(), 2);
        assert_eq!(batch.dropped, 4);
        // Dropping loses the report, never the value.
        assert_eq!(session.value("tb.clk"), Ok(bits("0")));
    }

    #[test]
    fn test_limits_stop_before_the_timestamp_they_forbid() {
        let mut session = session(CLOCKED);
        let stop = session.run(
            Limits {
                until: Some(12),
                steps: None,
            },
            None,
        );
        assert_eq!(
            (stop.reason, stop.time, stop.steps),
            (StopReason::TimeLimit, 10, 3)
        );
        let stop = session.run(
            Limits {
                until: None,
                steps: Some(2),
            },
            None,
        );
        assert_eq!(
            (stop.reason, stop.time, stop.steps),
            (StopReason::StepLimit, 20, 2)
        );
    }

    const FREE_RUNNING: &str = "
        module tb;
          reg clk = 0;
          always #5 clk = ~clk;
        endmodule
    ";

    #[test]
    fn test_cancelling_before_a_run_runs_nothing() {
        let mut session = session(FREE_RUNNING);
        let cancel = AtomicBool::new(true);
        let stop = session.run(Limits::default(), Some(&cancel));
        assert_eq!(
            (stop.reason, stop.time, stop.steps),
            (StopReason::Cancelled, 0, 0)
        );
        assert_eq!(session.steps(), 0);
    }

    #[test]
    fn test_cancelling_from_another_thread_stops_at_a_settled_timestamp() {
        let mut session = session(FREE_RUNNING);
        let cancel = AtomicBool::new(false);
        let stop = std::thread::scope(|scope| {
            scope.spawn(|| {
                std::thread::sleep(std::time::Duration::from_millis(20));
                cancel.store(true, Ordering::Relaxed);
            });
            // The step budget is only a guard against a flag nobody reads.
            session.run(
                Limits {
                    until: None,
                    steps: Some(50_000_000),
                },
                Some(&cancel),
            )
        });
        assert_eq!(stop.reason, StopReason::Cancelled);
        assert!(stop.steps > 0);
        // Stopped between timestamps, so the clock agrees with the time.
        let expected = if (stop.time / 5) % 2 == 1 { "1" } else { "0" };
        assert_eq!(stop.time % 5, 0);
        assert_eq!(session.value("tb.clk"), Ok(bits(expected)));

        cancel.store(false, Ordering::Relaxed);
        let resumed = session.run(
            Limits {
                until: None,
                steps: Some(3),
            },
            Some(&cancel),
        );
        assert_eq!(
            (resumed.reason, resumed.time),
            (StopReason::StepLimit, stop.time + 15)
        );
    }

    /// A design that prints, dumps, and moves at several timestamps.
    const DUMPED: &str = "
        module counter(input clk, output reg [3:0] q);
          initial q = 0;
          always @(posedge clk) q <= q + 1;
        endmodule
        module tb;
          reg clk = 0;
          wire [3:0] count;
          real phase;
          counter dut(.clk(clk), .q(count));
          always #5 clk = ~clk;
          always @(negedge clk) begin : side
            reg [7:0] seen;
            seen = {count, count};
            phase = phase + 0.5;
          end
          initial begin
            $dumpfile(\"dump.vcd\");
            $dumpvars(0, tb);
            #47 $display(\"count=%0d phase=%f\", count, phase);
            $finish;
          end
        endmodule
    ";

    /// Everything a run leaves behind that a pause must not change.
    fn observe(session: &Session, dir: &std::path::Path) -> (String, Vec<(String, Value)>, String) {
        let values = session
            .signals()
            .iter()
            .filter_map(|info| Some((info.id.clone(), session.value(&info.id).ok()?)))
            .collect();
        let dump = std::fs::read_to_string(dir.join("dump.vcd")).expect("the dump was written");
        // `$date` is the one line that differs between two runs.
        let body = dump
            .split_once("$enddefinitions")
            .expect("the dump has a header")
            .1
            .to_string();
        (session.simulator().output().text(), values, body)
    }

    fn dumped_session(dir: &std::path::Path) -> Session {
        let mut simulator = simulator(DUMPED);
        simulator.set_output_directory(dir.to_path_buf());
        let mut session = Session::from_simulator(simulator).expect("the design sets up");
        let ids: Vec<String> = session
            .signals()
            .iter()
            .filter(|info| !matches!(info.kind, SignalKind::Memory | SignalKind::Event))
            .map(|info| info.id.clone())
            .collect();
        session.subscribe(ids.iter().map(String::as_str)).unwrap();
        session
    }

    #[test]
    fn test_pausing_and_resuming_gives_the_uninterrupted_run() {
        let whole_dir = scratch("whole");
        let mut whole = dumped_session(&whole_dir);
        let stop = whole.run(Limits::default(), None);
        assert_eq!((stop.reason, stop.time), (StopReason::Finished, 47));
        let whole_changes = whole.take_changes();
        let whole_seen = observe(&whole, &whole_dir);
        assert!(whole_seen.0.contains("count=5"), "{}", whole_seen.0);

        let paused_dir = scratch("paused");
        let mut paused = dumped_session(&paused_dir);
        paused
            .add_breakpoint(Condition::Changes("tb.dut.q".into()))
            .unwrap();
        let cancel = AtomicBool::new(false);
        let mut changes = Vec::new();
        let mut round = 0;
        loop {
            round += 1;
            // Take turns at every way a run can stop short of the end.
            cancel.store(round % 4 == 0, Ordering::Relaxed);
            let limits = Limits {
                until: (round % 3 == 0).then(|| paused.now() + 7),
                steps: (round % 2 == 0).then_some(2),
            };
            let stop = paused.run(limits, Some(&cancel));
            changes.extend(paused.take_changes().changes);
            if stop.reason == StopReason::Finished {
                break;
            }
            assert!(round < 1000, "the paused run never finished");
        }
        assert!(round > 10, "only {} pauses", round);
        assert_eq!(changes, whole_changes.changes);
        assert_eq!(observe(&paused, &paused_dir), whole_seen);
    }

    #[test]
    fn test_signal_ids_name_the_traces_in_the_dump() {
        let dir = scratch("ids");
        let mut session = dumped_session(&dir);
        session.run(Limits::default(), None);
        let dump = std::fs::read_to_string(dir.join("dump.vcd")).unwrap();
        let waveform = Waveform::parse(&dump).expect("the dump reads back");
        let mut compared = 0;
        for info in session.signals() {
            if matches!(info.kind, SignalKind::Memory | SignalKind::Event) {
                continue;
            }
            let trace = waveform
                .traces
                .get(&info.id)
                .unwrap_or_else(|| panic!("`{}` is not in the dump", info.id));
            // A dump declares a real one wide, where its value is sixty-four
            // bits.
            if info.kind != SignalKind::Real {
                assert_eq!(trace.width, info.width, "{}", info.id);
            }
            if let Value::Bits(now) = session.value(&info.id).unwrap() {
                assert_eq!(
                    trace.value_at(waveform.end),
                    Some(now.as_str()),
                    "{}",
                    info.id
                );
                compared += 1;
            }
        }
        // clk, count, the two ports of dut and the named block's local.
        assert_eq!(compared, 5);
    }

    #[test]
    fn test_a_session_loads_from_a_run_config() {
        let dir = scratch("config");
        let source = dir.join("tb.v");
        std::fs::write(&source, CLOCKED).unwrap();
        let config = RunConfig {
            sources: vec![source],
            ..RunConfig::default()
        };
        let mut session = Session::new(&config).expect("the design loads");
        assert_eq!(session.top(), "tb");
        assert_eq!(
            session.run(Limits::default(), None).reason,
            StopReason::Finished
        );

        let missing = RunConfig {
            sources: vec![dir.join("absent.v")],
            ..RunConfig::default()
        };
        assert_eq!(
            Session::new(&missing).err().map(|error| error.stop),
            Some(StopReason::Io)
        );
    }
}

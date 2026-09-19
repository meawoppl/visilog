//! The simulation driver.
//!
//! [`Simulator`] takes a parsed [`VerilogModule`], declares every port, net,
//! register and parameter into a [`StateStore`], and then settles the module's
//! continuous assignments to a fixpoint:
//!
//! ```text
//! let mut simulator = Simulator::new(module);
//! simulator.setup()?;
//! simulator.set_input("a", Register::from_u128(3, 4))?;
//! simulator.run()?;
//! simulator.get("sum")?;
//! ```
//!
//! Sequential logic runs through [`Simulator::poke`], which drives an input and
//! then settles: any `always` block sensitive to the resulting edges executes,
//! its non-blocking updates commit, and continuous assignments re-propagate.
//! That repeats until nothing changes, which is Verilog's delta-cycle model.
//!
//! [`Simulator::advance`] moves simulated time forward, which is what gives
//! `#delay` meaning: a block that hits a delay suspends and re-queues itself
//! for a later timestamp. That also lets a design clock itself — an
//! `always begin #50 clk = ~clk; end` needs no external stimulus, just time.
//!
//! Module hierarchy is flattened at elaboration time by
//! [`crate::simulator::elaborate`]: a child's signals join the same
//! [`StateStore`] under qualified names (`dut.count`), and a port bound to a
//! plain identifier becomes the very same store entry as the parent signal it
//! was connected to. Hand the simulator more than one module with
//! [`Simulator::with_modules`].

use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::fmt;
use std::path::PathBuf;

use crate::parsers::expr::Expression;
use crate::parsers::{
    assignment::ContinuousAssignment, behavior::EventControl, gates::DriveStrength,
    modules::VerilogModule, preprocessor::Timescale,
};
use crate::register::Register;
use crate::simulator::elaborate::{elaborate, BlockKind, PulledNet, TimedBlock};
use crate::simulator::eval::{eval, eval_sized, EvalError};
use crate::simulator::event_queue::{EventQueue, ExecutionCursor};
use crate::simulator::events::{self, SignalEdge};
use crate::simulator::exec::{
    apply_drive, commit_updates, drive_resolved, resolve_target, PendingUpdate, ResolvedTarget,
};
use crate::simulator::gates::{resolve_strength, Driven, Gate, PassSwitch, Strength};
use crate::simulator::program::{self, Resume, WaitReason, FORK_TIMING_UNSUPPORTED};
use crate::simulator::state_store::DriverTally;
use crate::simulator::state_store::{bit_position_in, StateStore};
use crate::simulator::tasks::{Output, TaskContext};
use crate::simulator::udp::Udp;
use std::rc::Rc;

/// Ceiling on delta cycles within a single settle. A design that keeps
/// producing edges past this is oscillating, not converging.
const MAX_DELTA_CYCLES: usize = 100;

/// Ceiling on block resumptions within a single timestamp. A free-running
/// `always` block with no delay in it restarts forever without time moving;
/// this turns that into an error rather than a hang.
const MAX_RESUMPTIONS_PER_TIME: usize = 10_000;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SimulationError {
    /// A name with no entry in the [`StateStore`].
    UnknownSignal(String),
    /// [`Simulator::set_input`] was handed a name that is not an input port.
    NotAnInput(String),
    /// A module construct this milestone cannot execute.
    Unsupported(&'static str),
    /// A `$name(…)` call the simulator cannot carry out: an unrecognised task,
    /// a format specifier it does not know, or a specifier with no argument.
    /// It always names what it rejected — a design that silently printed
    /// nothing would look exactly like one that passed.
    SystemTask(String),
    /// An instantiation, or a top-level name, that no supplied module defines.
    UnknownModule(String),
    /// A named port connection for a port the instantiated module does not have.
    UnknownPort { module: String, port: String },
    /// A `#(...)` override for a parameter the instantiated module does not
    /// declare.
    UnknownParameter { module: String, parameter: String },
    /// More positional arguments than the module has ports (or parameters).
    TooManyArguments {
        module: String,
        what: &'static str,
        expected: usize,
        found: usize,
    },
    /// An output port connected to something that is not a plain signal. The
    /// child drives it, and there is no way to push a value back out through an
    /// arbitrary expression.
    UndrivablePort {
        instance: String,
        port: String,
        connection: String,
    },
    /// A module that instantiates itself, directly or around a cycle. No amount
    /// of flattening terminates on that.
    RecursiveInstantiation(String),
    /// A task enable naming a task the module does not declare.
    UnknownTask(String),
    /// A task enable with the wrong number of arguments. A task's arguments are
    /// how its results get out, so a missing one is never harmless.
    TaskArity {
        name: String,
        expected: usize,
        found: usize,
    },
    /// A task that enables itself, directly or around a cycle. A body is
    /// inlined where it is enabled, and inlining a cycle does not terminate.
    RecursiveTask(String),
    /// An `assign` whose left hand side is not something that can be driven.
    UnsupportedTarget(String),
    /// A gate primitive instantiated with a terminal count its type cannot
    /// take: `and (out);` has nothing to read, `bufif1 (out, in);` has no
    /// control.
    GateTerminals { gate: &'static str, found: usize },
    /// A connection to an arrayed module instance that is neither as wide as
    /// one element's port — shared by every element — nor exactly `count`
    /// times that, one slice per element.
    ArrayConnectionWidth {
        instance: String,
        port: String,
        port_width: usize,
        count: usize,
        found: usize,
    },
    /// A terminal of an *arrayed* gate instance that is neither one bit wide —
    /// shared by every instance — nor one bit per instance.
    GateArrayTerminal {
        gate: &'static str,
        expected: usize,
        found: usize,
    },
    /// [`Simulator::setup`] has not run yet.
    NotSetUp,
    /// The continuous assignments never stopped changing, which means the
    /// module contains a combinational loop.
    NoConvergence { passes: usize },
    /// The expression evaluator rejected an assignment's right hand side.
    Eval(EvalError),
    /// A declared range whose bounds are not constant where they are written:
    /// `reg [n-1:0] q;` for an `n` that is not a parameter, or a bound that
    /// evaluates to `x`. A width the simulator guessed at would be silently
    /// wrong for the whole run, so the bound is named instead.
    UnresolvedRange { bound: String, why: String },
    /// A `generate` control expression that is not constant where it is
    /// written: a loop bound, an `if` condition or a `case` subject. It
    /// decides how much of the design exists, so a value the elaborator picked
    /// for itself would be the wrong design rather than a wrong number.
    UnresolvedGenerate { expression: String, why: String },
    /// A `generate` loop that never stopped. Every iteration is a real copy of
    /// the body, so this is an allocation nothing survives rather than a hang.
    GenerateLoopBound { limit: usize },
    /// A `generate` loop whose step assigns a variable other than the one its
    /// initialiser named, which the LRM does not allow and which would
    /// otherwise loop for ever.
    GenerateLoopVariable { init: String, step: String },
    /// A `defparam` naming a parameter no instance in the design declares. An
    /// override that quietly did not happen leaves a design running at a width
    /// it was told not to use.
    UnappliedDefparam(String),
    /// A `disable` naming a scope the design has nowhere: no named block and
    /// no task spells it, anywhere. Disabling a scope that exists but is not
    /// running is a legitimate no-op — disabling one that does not exist is a
    /// design that thinks it cancelled something.
    UnknownScope(String),
}

impl fmt::Display for SimulationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SimulationError::UnknownSignal(name) => write!(f, "no signal named `{}`", name),
            SimulationError::NotAnInput(name) => write!(f, "`{}` is not an input port", name),
            SimulationError::Unsupported(what) => {
                write!(f, "{} is not supported by the simulator", what)
            }
            SimulationError::SystemTask(problem) => write!(f, "{}", problem),
            SimulationError::UnknownModule(name) => write!(f, "no module named `{}`", name),
            SimulationError::UnknownTask(name) => write!(f, "no task named `{}`", name),
            SimulationError::TaskArity {
                name,
                expected,
                found,
            } => write!(
                f,
                "task `{}` takes {} arguments, but was enabled with {}",
                name, expected, found
            ),
            SimulationError::RecursiveTask(name) => {
                write!(f, "task `{}` enables itself", name)
            }
            SimulationError::UnknownPort { module, port } => {
                write!(f, "module `{}` has no port `{}`", module, port)
            }
            SimulationError::UnknownParameter { module, parameter } => {
                write!(f, "module `{}` has no parameter `{}`", module, parameter)
            }
            SimulationError::TooManyArguments {
                module,
                what,
                expected,
                found,
            } => write!(
                f,
                "module `{}` has {} {}, but {} were supplied",
                module, expected, what, found
            ),
            SimulationError::UndrivablePort {
                instance,
                port,
                connection,
            } => write!(
                f,
                "output port `{}` of instance `{}` is connected to `{}`, which cannot be driven",
                port, instance, connection
            ),
            SimulationError::RecursiveInstantiation(name) => {
                write!(f, "module `{}` instantiates itself", name)
            }
            SimulationError::UnsupportedTarget(text) => {
                write!(f, "cannot drive `{}`", text)
            }
            SimulationError::UnresolvedRange { bound, why } => write!(
                f,
                "range bound `{}` is not a constant: {}",
                bound, why
            ),
            SimulationError::UnresolvedGenerate { expression, why } => write!(
                f,
                "generate expression `{}` is not a constant: {}",
                expression, why
            ),
            SimulationError::GenerateLoopBound { limit } => write!(
                f,
                "a generate loop ran past {} iterations without its condition going false",
                limit
            ),
            SimulationError::GenerateLoopVariable { init, step } => write!(
                f,
                "a generate loop starts `{}` and steps `{}`",
                init, step
            ),
            SimulationError::UnappliedDefparam(path) => {
                write!(f, "`defparam {}` names no parameter in the design", path)
            }
            SimulationError::UnknownScope(scope) => {
                write!(f, "`disable {}` names no block or task in the design", scope)
            }
            SimulationError::GateTerminals { gate, found } => write!(
                f,
                "gate `{}` cannot be instantiated with {} terminals",
                gate, found
            ),
            SimulationError::ArrayConnectionWidth {
                instance,
                port,
                port_width,
                count,
                found,
            } => write!(
                f,
                "port `{}` of instance array `{}` is {} bits wide, so a connection to \
                 it must be {} bits (shared) or {} bits (one slice for each of {}); \
                 this one is {}",
                port,
                instance,
                port_width,
                port_width,
                port_width * count,
                count,
                found
            ),
            SimulationError::GateArrayTerminal {
                gate,
                expected,
                found,
            } => write!(
                f,
                "an array of {} gates needs a terminal of 1 or {} bits, but one is {} bits wide",
                gate, expected, found
            ),
            SimulationError::NotSetUp => write!(f, "the simulator has not been set up"),
            SimulationError::NoConvergence { passes } => write!(
                f,
                "continuous assignments did not settle in {} passes; the module has a combinational loop",
                passes
            ),
            SimulationError::Eval(error) => write!(f, "{}", error),
        }
    }
}

impl std::error::Error for SimulationError {}

impl From<EvalError> for SimulationError {
    fn from(error: EvalError) -> Self {
        SimulationError::Eval(error)
    }
}

/// A block suspended on something the clock does not decide.
struct Waiting {
    cursor: ExecutionCursor,
    /// What it is waiting for. `None` is a `wait (c)`, whose condition is a
    /// value the block reads for itself when it is re-entered.
    watch: Option<EventWatch>,
}

/// An event control a block is suspended on, with what the signals it names
/// held when the block started waiting.
///
/// The snapshot is what gives the moment of arming any meaning. A settle round
/// sees everything the timestep moved, including what the block itself wrote
/// before it reached the wait — so `clk = 0; @(negedge clk) …` would be woken
/// by its own write. Measuring against what was there when the wait was armed
/// asks the question the design asked: has it moved *since*?
struct EventWatch {
    control: EventControl,
    /// One entry per signal the control names, holding what it had at the last
    /// look. `None` is a name the store has no value for, which is what a
    /// named event is — an event has no value, and is matched by its trigger
    /// instead.
    snapshot: Vec<(String, Option<Register>)>,
}

impl EventWatch {
    fn arm(control: EventControl, state: &StateStore) -> EventWatch {
        let snapshot = events::control_signals(&control)
            .into_iter()
            .map(|name| {
                let value = state.get(&name).cloned();
                (name, value)
            })
            .collect();
        EventWatch { control, snapshot }
    }

    /// The edges the watched signals have taken since the last look, which is
    /// also where the next look is measured from.
    fn edges_since(&mut self, state: &StateStore) -> Vec<SignalEdge> {
        let mut edges = Vec::new();
        for (name, before) in &mut self.snapshot {
            let current = state.get(name).cloned();
            if current == *before {
                continue;
            }
            if let (Some(was), Some(is)) = (before.as_ref(), current.as_ref()) {
                edges.push(SignalEdge::new(name.clone(), was.clone(), is.clone()));
            }
            *before = current;
        }
        edges
    }
}

/// The starting value of every *non-plain* entry in a sensitivity list, which
/// is what the first edge of that entry is measured from.
///
/// Taken where the design is elaborated and nothing has run yet, so a
/// `posedge (clock & b)` over two untouched registers starts at `x` — and
/// `clock` reaching `0` while `b` reaches `1111` is then the *negedge* of the
/// expression it really is, rather than the posedge the rising operand looks
/// like. A block whose entries are all plain gets an empty vector and pays
/// nothing for the rest of the run.
fn snapshot_event_values(control: &EventControl, state: &StateStore) -> Vec<Option<Register>> {
    let EventControl::Events(events) = control else {
        return Vec::new();
    };
    if events
        .iter()
        .all(|event| events::is_plain_event_expression(&event.expression))
    {
        return Vec::new();
    }
    events
        .iter()
        .map(|event| {
            if events::is_plain_event_expression(&event.expression) {
                None
            } else {
                eval(&event.expression, state).ok()
            }
        })
        .collect()
}

/// What one continuous assignment carrying a `#delay` is driving, and what it
/// is about to drive.
///
/// A delayed `assign` is still a continuous driver — it re-asserts its value on
/// every propagation pass exactly as an undelayed one does. The only difference
/// is *which* value: not the one its right hand side has now, but the one it
/// had `#n` ago.
#[derive(Clone, Debug, Default)]
struct DelayedDrive {
    /// What the assignment is driving at this instant. `None` until the first
    /// transaction lands, which is what makes the net read `x` rather than the
    /// `z` of a net nothing drives — something *is* driving it, it simply has
    /// not said what yet.
    applied: Option<Register>,
    /// The value in flight and the time it lands.
    pending: Option<(i64, Register)>,
}

impl DelayedDrive {
    /// What the assignment will be driving once everything in flight has
    /// landed, which is what a new value is compared against.
    fn destination(&self) -> Option<&Register> {
        self.pending
            .as_ref()
            .map(|(_, value)| value)
            .or(self.applied.as_ref())
    }
}

/// One `fork`…`join` that is currently running.
///
/// The branches are ordinary [`ExecutionCursor`]s carrying this fork's index;
/// what the record adds is the two things a branch cannot know for itself —
/// where the block picks up afterwards, and how many siblings have still to
/// arrive.
#[derive(Clone, Copy, Debug)]
struct ForkJoin {
    /// Where the block that wrote the `fork` carries on, and — through its own
    /// `fork` field — which outer join *it* is a branch of. That chain is what
    /// makes a `fork` nested inside a branch work with no second mechanism.
    parent: ExecutionCursor,
    /// The `Instruction::Fork` this record was opened at.
    ///
    /// A labelled `fork`'s scope runs from that instruction up to — but not
    /// including — the join, so asking "is this fork inside the scope?" of the
    /// parent cursor, which sits *at* the join, answers `no` for the fork's
    /// own label. The site answers `yes`, which is what tells a `disable` of
    /// the label from a `disable` of something inside one branch.
    site: usize,
    /// Branches that have not reached their `join` yet.
    outstanding: usize,
}

/// A parsed design, elaborated into signals and runnable blocks.
pub struct Simulator {
    /// Every module the design may draw on. Only the top one is walked
    /// directly; the rest are reached through instantiations.
    modules: Vec<VerilogModule>,
    /// The name of the module to elaborate. Resolved in `setup` rather than in
    /// the constructor so that every elaboration error surfaces from one place.
    top: String,
    state: StateStore,
    assignments: Vec<ContinuousAssignment>,
    /// One slot per entry in `assignments`, `None` for an assignment that named
    /// no delay. Empty for a design that names none anywhere, which is what
    /// keeps the question off the propagation hot path.
    delays: Vec<Option<DelayedDrive>>,
    /// The same, one slot per gate. A gate is a continuous driver like a
    /// delayed `assign`, so it wants the identical machinery — an empty vector
    /// for a design whose gates name no delay.
    gate_delays: Vec<Option<DelayedDrive>>,
    /// The same again, one slot per user-defined primitive. A UDP is a
    /// continuous driver like a gate, so `BUFG #5 bg(out, in);` wants the
    /// identical machinery — and an empty vector for a design whose primitives
    /// name no delay.
    udp_delays: Vec<Option<DelayedDrive>>,
    /// Writes an `a <= #5 b;` scheduled but has not yet made. Each holds the
    /// value its right hand side had when the statement ran, so nothing about
    /// it is re-read when it lands. Empty for a design that writes none.
    scheduled: Vec<(i64, PendingUpdate)>,
    /// The cursors [`Simulator::advance`] has taken off the queue for the round
    /// it is running and has not resumed yet.
    ///
    /// It is a field rather than a local because a `disable` has to be able to
    /// reach it: a block due at this timestamp is off the queue from the moment
    /// the round is collected, so [`Simulator::cancel_scope`] looking only at
    /// the queue would find nothing and let it run anyway (corpus `sdw_dsbl`).
    round: VecDeque<ExecutionCursor>,
    /// Whether the continuous assignments have been settled once, before the
    /// first block ran. See [`Simulator::advance`].
    settled_once: bool,
    /// The design's gate primitives, which are continuous drivers and settle
    /// in the same fixpoint the assignments do.
    gates: Vec<Gate>,
    /// The design's user-defined primitives, continuous drivers beside the
    /// gates and settled in the same fixpoint.
    udps: Vec<Udp>,
    /// The design's bidirectional pass switches. One is not a driver — it joins
    /// two nets into a node whose drivers resolve together — so it is held
    /// apart from the gates. Empty for a design with no `tran` in it, which is
    /// what keeps the question off the propagation hot path.
    pass_switches: Vec<PassSwitch>,
    /// The nets a gate drives, which are resolved between all their continuous
    /// drivers rather than written by whichever one ran last. Empty for a
    /// design with no gates, which is what keeps the question off the hot path.
    resolved_nets: HashSet<String>,
    /// Nets that drive themselves — `supply0`/`supply1` and `tri0`/`tri1`.
    pulled_nets: Vec<PulledNet>,
    blocks: Vec<TimedBlock>,
    /// The blocks suspended on something other than the clock: a `wait` on a
    /// value, or an event control waiting for an edge.
    ///
    /// They are not on the [`EventQueue`], because nothing schedules them —
    /// what wakes them is the design moving, which is exactly what a settle
    /// round measures. Empty for a design that waits on nothing, which is what
    /// keeps the question off that round's hot path.
    waiting: Vec<Waiting>,
    /// Which blocks ran in the previous settle round, and which have run in
    /// this one — one flag per block, swapped at the top of every round.
    ///
    /// This is what tells an edge a block made *itself* from one the rest of
    /// the design made. A block is sensitive only while it is parked at its
    /// event control, so the writes it made on its last run through cannot
    /// wake it; `TimedBlock::writes` says which names those are and this says
    /// whether the block was running when they moved. Two vectors rather than
    /// one so the swap is free and a round costs a `fill` of bytes.
    ran_before: Vec<bool>,
    ran_now: Vec<bool>,
    /// What each block's *non-plain* sensitivity entries last evaluated to —
    /// one slot per entry in the block's list, `None` for an entry that names
    /// a signal directly and is matched against the change journal instead.
    ///
    /// `always @(posedge clock & b)` has an edge of its own, which is not the
    /// edge of either operand: `clock` going `x -> 0` beside `b` going
    /// `x -> 1111` is a *negedge* of `clock & b` although the `b` operand rose.
    /// Keeping the evaluated value is what lets the edge be asked of the
    /// expression. A block whose entries are all plain — every design that
    /// writes only `posedge clk` — keeps an empty vector, which is what keeps
    /// the extra evaluation off the settle round's hot path.
    event_values: Vec<Vec<Option<Register>>>,
    /// Whether any block has an expression sensitivity entry at all, so that a
    /// design that has none — nearly every design — asks one `bool` per settle
    /// round rather than reaching into [`Simulator::event_values`] per block.
    expression_events: bool,
    /// The `fork`…`join`s currently running, indexed by the number a branch
    /// cursor carries. A retired slot is `None` and is handed out again, so a
    /// `fork` inside a loop does not grow this without bound. Empty for a
    /// design whose forks all finish in zero time, which are compiled as plain
    /// blocks and never reach here.
    forks: Vec<Option<ForkJoin>>,
    /// Qualified names of ports that were aliased onto a parent signal, so they
    /// can still be read back even though they hold no state of their own.
    aliases: HashMap<String, String>,
    queue: EventQueue,
    now: i64,
    inputs: Vec<String>,
    is_setup: bool,
    /// Where system tasks print, what `$time` reads, and whether the design has
    /// called `$finish`.
    tasks: TaskContext,
    /// Where a relative `$fopen` or `$writememh` path is written.
    ///
    /// It is kept here rather than only on the `StateStore` because
    /// [`Simulator::setup`] builds a fresh store out of the elaboration, and a
    /// directory the caller chose outlives any one elaboration — the same
    /// reason the `$readmemh` search path is kept here.
    output_directory: Option<PathBuf>,
    /// Where a relative `$readmemh` or `$fopen(name, "r")` path is looked for,
    /// after the process working directory. Kept here and handed to each fresh
    /// store for the same reason `output_directory` is.
    search_paths: Vec<PathBuf>,
    /// The `+name=value` words the design was started with, without their `+`.
    /// Kept here and handed to each fresh store for the same reason
    /// `search_paths` is: [`Simulator::setup`] builds a new `StateStore` and
    /// what the caller configured outlives any one elaboration.
    plusargs: Vec<String>,
    /// The `` `timescale `` the source declared, which a waveform dump states
    /// in its header. A `Simulator` is built from parsed *modules* and a
    /// timescale is a property of the *file*, so — like the search path and the
    /// output directory — only the caller that read the file knows it.
    timescale: Option<Timescale>,
}

impl Simulator {
    /// A design of exactly one module, which is therefore the top.
    pub fn new(module: VerilogModule) -> Self {
        let top = module.identifier.name.clone();
        Self::with_modules(vec![module], top)
    }

    /// A design of several modules, elaborated from `top` downwards.
    ///
    /// A module named by an instantiation is looked up here, so the order of
    /// `modules` does not matter. A `top` that names nothing is reported by
    /// [`Simulator::setup`] as [`SimulationError::UnknownModule`].
    pub fn with_modules(modules: Vec<VerilogModule>, top: impl Into<String>) -> Self {
        Self {
            modules,
            top: top.into(),
            state: StateStore::new(),
            assignments: Vec::new(),
            delays: Vec::new(),
            gate_delays: Vec::new(),
            udp_delays: Vec::new(),
            scheduled: Vec::new(),
            round: VecDeque::new(),
            settled_once: false,
            gates: Vec::new(),
            udps: Vec::new(),
            pass_switches: Vec::new(),
            resolved_nets: HashSet::new(),
            pulled_nets: Vec::new(),
            blocks: Vec::new(),
            waiting: Vec::new(),
            ran_before: Vec::new(),
            ran_now: Vec::new(),
            event_values: Vec::new(),
            expression_events: false,
            forks: Vec::new(),
            aliases: HashMap::new(),
            queue: EventQueue::new(),
            now: 0,
            inputs: Vec::new(),
            is_setup: false,
            tasks: TaskContext::new(),
            output_directory: None,
            search_paths: Vec::new(),
            plusargs: Vec::new(),
            timescale: None,
        }
    }

    /// Elaborates the design: declares every signal the top module and
    /// everything it instantiates names, and collects their continuous
    /// assignments and procedural blocks.
    ///
    /// Signals start out all `x`, the way an undriven Verilog net does;
    /// parameters are folded to their value immediately. A child's signals join
    /// the same flat store under qualified names, so `dut.count` is read back
    /// exactly like a local one.
    pub fn setup(&mut self) -> Result<(), SimulationError> {
        self.state = StateStore::new();
        self.assignments.clear();
        self.delays.clear();
        self.gate_delays.clear();
        self.udp_delays.clear();
        self.gates.clear();
        self.udps.clear();
        self.pass_switches.clear();
        self.resolved_nets.clear();
        self.scheduled.clear();
        self.round.clear();
        self.settled_once = false;
        self.pulled_nets.clear();
        self.blocks.clear();
        self.waiting.clear();
        self.forks.clear();
        self.aliases.clear();
        self.queue = EventQueue::new();
        self.now = 0;
        self.inputs.clear();
        self.is_setup = false;
        self.tasks.reset();
        // What the design is called and what one tick of it is: the two things
        // a waveform header states that no task argument carries.
        self.tasks.describe_design(self.top.clone(), self.timescale);

        let top = self
            .modules
            .iter()
            .position(|module| module.identifier.name == self.top)
            .ok_or_else(|| SimulationError::UnknownModule(self.top.clone()))?;
        let elaborated = elaborate(&self.modules, top)?;
        self.state = elaborated.state;
        // A `$display` inside a function body prints through the store, which
        // is all `eval` is handed. Linking the two *after* elaboration is what
        // makes a constant function's output disappear: the store the
        // parameters were evaluated against had a buffer of its own, and
        // iverilog drops that output too.
        self.state.print_into(self.tasks.output().clone());
        if let Some(directory) = &self.output_directory {
            self.state.set_output_directory(directory.clone());
        }
        self.state.set_search_paths(self.search_paths.clone());
        self.state.set_plusargs(self.plusargs.clone());
        self.assignments = elaborated.assignments;
        // A design that names no delay on any `assign` keeps an empty vector,
        // so the propagation loop asks nothing per pass.
        self.delays = if self.assignments.iter().any(|a| a.delay().is_some()) {
            self.assignments
                .iter()
                .map(|a| a.delay().map(|_| DelayedDrive::default()))
                .collect()
        } else {
            Vec::new()
        };
        self.gates = elaborated.gates;
        self.gate_delays = if self.gates.iter().any(|gate| gate.delay.is_some()) {
            self.gates
                .iter()
                .map(|gate| gate.delay.as_ref().map(|_| DelayedDrive::default()))
                .collect()
        } else {
            Vec::new()
        };
        self.udps = elaborated.udps;
        self.udp_delays = if self.udps.iter().any(|udp| udp.delay.is_some()) {
            self.udps
                .iter()
                .map(|udp| udp.delay.as_ref().map(|_| DelayedDrive::default()))
                .collect()
        } else {
            Vec::new()
        };
        self.pass_switches = elaborated.pass_switches;
        self.resolved_nets = elaborated.resolved_nets;
        self.pulled_nets = elaborated.pulled_nets;
        self.blocks = elaborated.blocks;
        self.ran_before = vec![false; self.blocks.len()];
        self.ran_now = vec![false; self.blocks.len()];
        self.event_values = self
            .blocks
            .iter()
            .map(|block| snapshot_event_values(&block.control, &self.state))
            .collect();
        self.expression_events = self.event_values.iter().any(|values| !values.is_empty());
        self.inputs = elaborated.inputs;
        self.aliases = elaborated.aliases;
        // An aliased port is a *name* the design has and the flat store does
        // not, so a waveform that left them out would show an instance with no
        // ports on it. The dump is the only thing that wants the table by
        // value, and it is built once per elaboration.
        self.tasks.name_aliases(self.aliases.clone());
        // A testbench reaching into an instance by a port's own name — `$countdrivers
        // (pad1.pad, …)` — has to find the entry that port was aliased onto, and `eval`
        // is handed the store and nothing else.
        self.state.name_aliases(Rc::new(self.aliases.clone()));
        // `$countdrivers` reports what reached a net, which means the net has to be
        // *resolved* between its drivers rather than simply written — and that is
        // decided here, once, not when the call runs. So a design that asks anywhere
        // resolves every net a continuous driver names, singly driven ones included:
        // resolving one strong driver gives exactly what writing it gave, and the
        // driver list it builds on the way is the answer. A design that never asks
        // pays one walk of its compiled blocks at setup and nothing per pass.
        if self
            .blocks
            .iter()
            .any(|block| block.program.calls_system_function("countdrivers"))
        {
            self.state.count_drivers();
            self.resolve_every_driven_net();
        }
        // `$printtimescale` is a question about the hierarchy, which
        // flattening has just thrown away — so the instance list comes over
        // here, together with every module's own scale, because a module the
        // design never instantiated still has one and still answers.
        self.tasks.describe_scopes(
            elaborated.instances,
            self.modules
                .iter()
                .map(|module| (module.identifier.name.clone(), module.timescale))
                .collect(),
        );

        self.is_setup = true;

        // Everything that starts on its own starts at time zero: `initial`
        // blocks, which run once, and free-running `always` blocks, which have
        // no event to wait for. Edge-triggered blocks are not queued — they are
        // woken by `settle`.
        for id in 0..self.blocks.len() {
            if self.blocks[id].kind == BlockKind::Initial || self.blocks[id].free_running {
                self.queue.insert(0, ExecutionCursor::new(id, 0));
            }
        }

        // Declaring every signal journalled it, and a declaration is not a
        // change anything should wake on. Clearing here leaves the journal
        // holding only what the drivers go on to do to the nets — which *is*
        // an event, and the first `settle` has to be able to see it.
        self.state.clear_changes();

        // A delayed `assign` is driving from the first instant, and what it is
        // driving before its first transaction lands is `x` — not the `z` of a
        // net nothing drives. One pass puts that on the net and puts the first
        // transaction in flight, both of which a block running at time zero can
        // already see. It is deliberately conditional: settling unconditionally
        // here would make a module that never converges fail at setup rather
        // than when someone actually asks it to run.
        if !self.delays.is_empty() || !self.gate_delays.is_empty() || !self.udp_delays.is_empty() {
            self.propagate()?;
        }

        // Drain time zero. This is a no-op for a module with no procedural
        // blocks, for the same reason.
        self.advance(0)?;

        Ok(())
    }

    /// Drives an input port. The value is resized to the port's declared width,
    /// keeping the least significant bits.
    pub fn set_input(&mut self, name: &str, value: Register) -> Result<(), SimulationError> {
        if !self.is_setup {
            return Err(SimulationError::NotSetUp);
        }
        if !self.inputs.iter().any(|input| input == name) {
            return Err(SimulationError::NotAnInput(name.to_string()));
        }
        let signal = self
            .state
            .get_signal(name)
            .ok_or_else(|| SimulationError::UnknownSignal(name.to_string()))?;
        let (width, range) = (signal.width(), signal.range());
        self.state.set_ranged(name, value.resize(width), range);
        Ok(())
    }

    /// The current value of any signal: an output port, an input, an internal
    /// wire, or anything inside an instance at its qualified name (`dut.count`).
    ///
    /// A child port that was aliased onto a parent signal has no store entry of
    /// its own, so its qualified spelling is looked up through the alias table
    /// and reads back the signal it shares.
    pub fn get(&self, name: &str) -> Result<&Register, SimulationError> {
        if let Some(register) = self.state.get(name) {
            return Ok(register);
        }
        self.aliases
            .get(name)
            .and_then(|canonical| self.state.get(canonical))
            .ok_or_else(|| SimulationError::UnknownSignal(name.to_string()))
    }

    /// Settles the continuous assignments.
    ///
    /// Continuous assignments form a dataflow graph but are stored in source
    /// order, so one pass over them can leave a consumer holding the stale value
    /// of a producer written later in the file. Passes are repeated until a full
    /// pass changes nothing, and the number of passes taken is returned.
    ///
    /// A pure dataflow graph of `n` assignments needs at most `n` passes to
    /// settle — each pass finalizes at least the next assignment in dependency
    /// order — plus one more to observe that nothing moved. The limit here is
    /// looser than that so a feedback path that latches onto a stable value
    /// still has room, but it is finite: a module that genuinely oscillates
    /// reports [`SimulationError::NoConvergence`] instead of hanging.
    pub fn run(&mut self) -> Result<usize, SimulationError> {
        if !self.is_setup {
            return Err(SimulationError::NotSetUp);
        }
        self.propagate()
    }

    /// Drives an input and settles the whole design: continuous assignments
    /// re-propagate, every `always` block sensitive to the resulting edges
    /// executes, and its non-blocking updates commit. Returns the delta cycles
    /// taken.
    ///
    /// This is the entry point for sequential logic — clocking a design means
    /// poking its clock, since it is the *edge* that wakes an `always` block.
    pub fn poke(&mut self, name: &str, value: Register) -> Result<usize, SimulationError> {
        if self.finished() {
            return Ok(0);
        }
        // The marker goes down before the input is written, so writing it is
        // itself an edge. Without that, nothing an edge-triggered block waits
        // on ever appears to move and the design never wakes.
        self.state.clear_changes();
        self.set_input(name, value)?;
        self.propagate()?;
        let deltas = self.settle()?;
        self.end_of_timestep()?;
        Ok(deltas)
    }

    /// One full clock pulse: low-to-high, then high-to-low. Edge-triggered
    /// logic acts on the rising half.
    pub fn tick(&mut self, clock: &str) -> Result<(), SimulationError> {
        self.poke(clock, Register::from_u128(1, 1))?;
        self.poke(clock, Register::from_u128(0, 1))?;
        Ok(())
    }

    /// Repeatedly wakes `always` blocks until the design stops changing.
    ///
    /// The store journals what is written to it, so the changes it has recorded
    /// since the caller's marker are the set of edges that may wake a block;
    /// running those blocks can move more signals, which is itself a new set of
    /// edges. Verilog calls each of these rounds a delta cycle, and they repeat
    /// until a round produces no edges at all.
    ///
    /// Taking the changes also resets the marker, which is what keeps the
    /// rounds separate: round N+1 reacts to exactly what round N moved, never
    /// to an accumulation across rounds.
    ///
    /// A design that never stops producing edges — a bare `always` block that
    /// keeps toggling, say — reports [`SimulationError::NoConvergence`] rather
    /// than hanging.
    fn settle(&mut self) -> Result<usize, SimulationError> {
        for delta in 1..=MAX_DELTA_CYCLES {
            // The blocks that ran in the previous round are what this round
            // measures "was that edge one of mine?" against, so the two lists
            // change places here and the new one starts empty.
            std::mem::swap(&mut self.ran_before, &mut self.ran_now);
            self.ran_now.fill(false);
            // Taking the changes here, before the blocks run, is what makes the
            // next round's edges exactly what this round moves.
            let changes = self.state.take_changes();
            // The waveform dump measures a timestep from the same journal the
            // edges come out of, so it costs the names *written* rather than
            // the names in the design. A design that dumps nothing asks one
            // question per round and does nothing else.
            if self.tasks.is_dumping() {
                self.tasks
                    .note_changes(changes.iter().map(|(name, _)| name.as_str()));
            }
            let mut edges = events::edges_from_changes(changes, &self.state);
            // A memory keeps a journal of its own, since one displaced
            // `Register` per name cannot say which word moved. A design that
            // declares no memory skips it on a flag rather than on a lookup.
            if self.state.any_memory() {
                let memory_changes = self.state.take_memory_changes();
                if self.tasks.is_dumping() {
                    self.tasks
                        .note_changes(memory_changes.iter().map(|(name, _, _)| name.as_str()));
                }
                edges.extend(events::memory_edges(memory_changes));
            }
            // A named event has no value, so it cannot appear in either
            // journal above: a trigger is recorded as the bare fact that it
            // happened, and taking it here is what makes it wake a block
            // exactly once. They are kept apart from the rest because a
            // suspended block measures a *signal* against what it held when it
            // started waiting, and a trigger has nothing to measure.
            let triggers = if self.state.any_event() {
                events::trigger_edges(self.state.take_triggers())
            } else {
                Vec::new()
            };
            edges.extend(triggers.iter().cloned());
            if edges.is_empty() {
                return Ok(delta - 1);
            }

            let mut pending = Vec::new();
            // Hoisted out of the loop: a design with no `posedge (a & b)` in
            // it pays one `bool` for the whole round.
            let expression_events = self.expression_events;
            for id in 0..self.blocks.len() {
                // A free-running `always` waits on nothing, so `always_block_fires`
                // would report it as firing on every edge. It is driven by time,
                // not by edges, so it is skipped here.
                if self.blocks[id].kind != BlockKind::Always || self.blocks[id].free_running {
                    continue;
                }
                // A block part way through a `wait` — or part way through a
                // `fork`, waiting at its `join` — has not finished the run it
                // is on, and an `always` block does not start again until it
                // does. Starting a second copy of it here would give the design
                // two writers of everything the block assigns.
                if self
                    .waiting
                    .iter()
                    .any(|waiting| waiting.cursor.block == id)
                    || self.is_forking(id)
                {
                    // A sensitivity entry that is an *expression* still has to
                    // be re-measured, or the round that does give the block a
                    // turn would compare against a value several rounds old and
                    // find an edge that never happened.
                    if expression_events {
                        self.refresh_event_values(id);
                    }
                    continue;
                }
                // An edge the block made *itself* on its last run through
                // does not wake it. A block is sensitive only while it is
                // parked at its event control, so a write it made on its way
                // to the end happened while it was not listening, and by the
                // time it comes back the event is in the past. Corpus
                // `event_list3`, whose block assigns a signal its own
                // sensitivity list names and runs twice without this.
                let filtered;
                let offered = if self.ran_before[id] && !self.blocks[id].writes.is_empty() {
                    filtered = edges
                        .iter()
                        .filter(|edge| !self.blocks[id].writes.contains(&edge.name))
                        .cloned()
                        .collect::<Vec<_>>();
                    &filtered[..]
                } else {
                    &edges[..]
                };
                let fires = if expression_events {
                    self.block_fires(id, offered)
                } else {
                    self.blocks[id].fires(offered, &self.state)
                };
                if fires {
                    let (updates, _) = self.resume_block(ExecutionCursor::new(id, 0))?;
                    pending.extend(updates);
                }
                if self.finished() {
                    break;
                }
            }

            pending.extend(self.wake_waiting(&triggers)?);

            commit_updates(pending, &mut self.state)?;
            self.propagate()?;
        }

        Err(SimulationError::NoConvergence {
            passes: MAX_DELTA_CYCLES,
        })
    }

    /// Whether the edges this round woke block `id`.
    ///
    /// A block all of whose sensitivity entries name a signal directly takes
    /// the path it always took — the whole question is answered against the
    /// change journal and nothing is evaluated. A block with an *expression*
    /// entry goes through [`events::events_fire`] instead, which measures that
    /// entry's edge from what it last evaluated to and records what it
    /// evaluates to now.
    fn block_fires(&mut self, id: usize, edges: &[SignalEdge]) -> bool {
        if self.event_values[id].is_empty() {
            return self.blocks[id].fires(edges, &self.state);
        }
        // Taken out and put back so the block and its values can be borrowed
        // at once; it is a `Vec` move and the design that pays for it is one
        // that wrote `posedge (a & b)`.
        let mut values = std::mem::take(&mut self.event_values[id]);
        let fired = match &self.blocks[id].control {
            EventControl::Events(events) => {
                events::events_fire(events, edges, &self.state, &mut values)
            }
            _ => false,
        };
        self.event_values[id] = values;
        fired
    }

    /// Re-measures a block's *expression* sensitivity entries without asking
    /// whether they fire, for the rounds the block is not offered a turn at
    /// all. Nothing to do for a block whose entries all name a signal.
    fn refresh_event_values(&mut self, id: usize) {
        if self.event_values[id].is_empty() {
            return;
        }
        self.block_fires(id, &[]);
    }

    /// Resumes every waiting block whose wait is now satisfied.
    ///
    /// The two reasons a block waits are answered differently on purpose. A
    /// condition is a value that is still there, so the block is simply
    /// re-entered and its own `wait` instruction decides. An edge is not: it
    /// is measured against what the watched signals held when the block
    /// started waiting, which is what keeps a block from being woken by a
    /// write it made itself before it reached the wait. `triggers` are the
    /// named events fired this round, which have no value to measure and so
    /// are offered to every waiter as they are.
    ///
    /// A block that is still not satisfied goes back on the list, and writes
    /// nothing, so it cannot keep the settle loop from converging.
    fn wake_waiting(
        &mut self,
        triggers: &[SignalEdge],
    ) -> Result<Vec<PendingUpdate>, SimulationError> {
        let mut pending = Vec::new();
        if self.waiting.is_empty() {
            return Ok(pending);
        }

        let implicit = BTreeSet::new();
        let mut still_waiting = Vec::new();
        let mut woken = Vec::new();
        for mut waiting in std::mem::take(&mut self.waiting) {
            let wake = match &mut waiting.watch {
                None => true,
                Some(watch) => {
                    let mut edges = watch.edges_since(&self.state);
                    edges.extend(triggers.iter().cloned());
                    events::control_fires(&watch.control, &edges, &implicit, &self.state)
                }
            };
            if wake {
                woken.push(waiting.cursor);
            } else {
                still_waiting.push(waiting);
            }
        }
        self.waiting = still_waiting;

        for cursor in woken {
            let (updates, _) = self.resume_block(cursor)?;
            pending.extend(updates);
            if self.finished() {
                break;
            }
        }
        Ok(pending)
    }

    /// Runs whatever the timestep just finished deferred to its end.
    ///
    /// `$strobe` and `$monitor` both report *after* everything else in a
    /// timestep has run, so they need a slot that only exists once the design
    /// has stopped moving. That is exactly what a settled `settle` is, and the
    /// two places a timestep can end here are the same two places `settle` is
    /// called from: the end of one timestamp's work in
    /// [`Simulator::advance`], and the end of a [`Simulator::poke`], which
    /// settles the design at the time it is already at.
    ///
    /// A design that uses neither task pays [`TaskContext::has_deferred`] —
    /// a load and a branch — per timestep.
    fn end_of_timestep(&mut self) -> Result<(), SimulationError> {
        if !self.tasks.has_deferred() {
            return Ok(());
        }
        self.tasks.flush(&self.state)?;
        // A design that has stopped owes its waveform a final `#<time>`: that
        // is how a viewer knows how long it ran after its last transition.
        if self.tasks.finished() {
            self.tasks.close_dump(&self.state, self.now);
        }
        Ok(())
    }

    /// The current simulated time.
    pub fn now(&self) -> i64 {
        self.now
    }

    /// Adds a directory to look in for a relative `$readmemh` / `$readmemb`
    /// file, or one a design opens with `$fopen(name, "r")`, after the process
    /// working directory.
    ///
    /// A `Simulator` is built from parsed modules and never learns which file
    /// they came from, so it cannot resolve a data path "next to the design" on
    /// its own. A caller that does know — a test harness walking a corpus, say
    /// — says so here. The search path outlives [`Simulator::setup`], which is
    /// why it is configured on the simulator rather than reset with the rest of
    /// the task state.
    pub fn add_search_path(&mut self, directory: impl Into<PathBuf>) {
        self.search_paths.push(directory.into());
        self.state.set_search_paths(self.search_paths.clone());
    }

    /// Adds one `+name=value` word to what `$test$plusargs` and
    /// `$value$plusargs` read, with or without its leading `+`.
    ///
    /// A plus-arg is the one thing a design learns about its own invocation,
    /// and visilog has no command line to learn it from — so, like the search
    /// path, it is something only the caller knows. A design given none is told
    /// so rather than failing: `$test$plusargs` answers `0` and
    /// `$value$plusargs` answers `0` and leaves its target alone, which is
    /// exactly what a design checks for.
    pub fn add_plusarg(&mut self, plusarg: impl AsRef<str>) {
        let plusarg = plusarg.as_ref();
        self.plusargs
            .push(plusarg.strip_prefix('+').unwrap_or(plusarg).to_string());
        self.state.set_plusargs(self.plusargs.clone());
    }

    /// Where a relative `$fopen` or `$writemem…` path is written, which
    /// defaults to the process working directory.
    ///
    /// This is [`Simulator::add_search_path`] the other way round, and it
    /// exists for the same reason: a `Simulator` is built from parsed modules
    /// and never learns which file they came from, so where a design's output
    /// belongs is something only the caller knows.
    pub fn set_output_directory(&mut self, directory: impl Into<PathBuf>) {
        self.output_directory = Some(directory.into());
    }

    /// The `` `timescale `` the front end recorded, which is what a waveform
    /// dump's `$timescale` states.
    ///
    /// The clock counts *ticks* and nothing rescales them, so this names the
    /// unit a tick already is — the design's `unit`, not its `precision`. That
    /// is exactly the reading `$timeformat` takes of the same directive, and it
    /// is the one that makes `#5` in a `` `timescale 1ns `` design five
    /// nanoseconds in the waveform. A design that declared none dumps in `1s`,
    /// which is what iverilog writes for one.
    pub fn set_timescale(&mut self, timescale: Option<Timescale>) {
        self.timescale = timescale;
    }

    /// Everything the design has printed with `$display` and `$write`.
    ///
    /// System task output is buffered rather than written to stdout, which is
    /// what makes a self-checking design testable: whether it printed `PASSED`
    /// is a plain assertion. A caller that wants it on a terminal prints this.
    pub fn output(&self) -> &Output {
        self.tasks.output()
    }

    /// Whether the design has called `$finish`. A finished simulation runs no
    /// more blocks and its time no longer moves.
    pub fn finished(&self) -> bool {
        self.tasks.finished()
    }

    /// Runs simulated time forward by `duration` time units, executing every
    /// scheduled block resumption along the way.
    ///
    /// This is what makes `#delay` mean something. A block that hits a delay
    /// suspends and re-queues itself for a later timestamp; advancing time is
    /// what brings it back. It is also what drives a self-clocking design —
    /// `always begin #50 clk = ~clk; end` needs no external stimulus at all,
    /// just time.
    pub fn advance(&mut self, duration: i64) -> Result<(), SimulationError> {
        if !self.is_setup {
            return Err(SimulationError::NotSetUp);
        }
        // `$finish` ends the simulation: no block runs again and time stops
        // where it stopped.
        if self.finished() {
            return Ok(());
        }

        // The continuous assignments settle **before** the first block runs:
        // `assign y = 3;` read from an `initial` block reads 3, not the `z` of
        // a net nothing drives, which is what iverilog prints.
        //
        // Here rather than in `setup` because a right hand side is only worth
        // evaluating once the inputs have been written — settling at setup
        // evaluates every `assign` against undriven inputs, which for a design
        // whose assignment calls a recursive function means recursing on `z`
        // until the call-depth guard stops it, at every setup.
        //
        // Whatever this pass reports is deliberately **dropped**: a module
        // that never converges should fail when someone runs it, and the same
        // error is raised again by the first real propagation below.
        if !self.settled_once {
            self.settled_once = true;
            let _ = self.propagate();
            // Before simulation starts a *driven* net holds `x`, so its
            // drivers' first values are events only when they differ from `x`.
            // A primitive whose `initial` is 0 wakes `always @(q)`; one with no
            // `initial`, whose output is simply `x`, does not — iverilog puts
            // that `x` on the net "before the start of simulation". Corpus
            // `br_ml20190806a` and `br_ml20190806b` pin the two halves. A net
            // nothing drives is never written, so it still reads `z`.
            self.state.treat_undriven_start_as_unknown();
            // The loop below starts every timestamp by clearing the journal,
            // so without settling here that first change would be dropped
            // unseen. Its result is dropped for the reason the pass above
            // drops its own.
            let _ = self.settle();
        }

        let target = self.now + duration;
        while let Some(time) = self.next_time() {
            if time > target {
                break;
            }
            self.now = time;
            // The store carries the clock `$time` reads, so it moves with
            // `now` — both for the blocks resumed below and for the continuous
            // assignments settled after them.
            self.state.set_time(time);

            // Everything the resumptions below move is an edge for the settle
            // that follows them.
            self.state.clear_changes();
            // A delayed `assign` whose transaction is due now starts driving
            // its new value before anything runs, so a block scheduled for
            // this instant reads the net as it is at this instant.
            self.land_due_drives(time);
            let mut pending = Vec::new();
            let mut resumptions = 0;

            // Everything due at this timestamp runs before time moves on,
            // including anything re-queued for this same instant — but a block
            // re-queued by a `#0` belongs to a *later* round, after the
            // continuous assignments have caught up with what this round
            // wrote. `#0` is the idiom for "let everything settle, then look",
            // so a round that ran and then read a net without re-propagating
            // would read what the net held before the block moved anything.
            //
            // Non-blocking updates deliberately do **not** commit between
            // rounds: they land once, at the end of the timestep, which is
            // what makes `a <= b; b <= a;` across two blocks a swap.
            while self.queue.peek_time() == Some(time) {
                while self.queue.peek_time() == Some(time) {
                    let (_, cursor) = self.queue.pop().expect("peeked time must pop");
                    self.round.push_back(cursor);
                }
                resumptions += self.round.len();
                if resumptions > MAX_RESUMPTIONS_PER_TIME {
                    self.round.clear();
                    return Err(SimulationError::NoConvergence {
                        passes: resumptions,
                    });
                }

                // A `$finish` stops the block that ran it and nothing else in
                // this timestep: iverilog runs everything already scheduled at
                // this instant and reports the end-of-timestep `$monitor`
                // before it stops. Measured — an `always #10` beside an
                // `initial #30 $finish` still runs at 30, and the monitor
                // still prints that step's line.
                //
                // Taken one at a time rather than iterated, because a
                // `disable` run by one of them reaches into this very list:
                // `cancel_scope` drops the cursors of the block it cancelled
                // from it, so a block due at this timestamp that has not run
                // yet does not run at all.
                while let Some(cursor) = self.round.pop_front() {
                    let (updates, _) = self.resume_block(cursor)?;
                    pending.extend(updates);
                }
                // Whatever this reports is dropped, for the same reason the
                // pre-pass in front of the loop drops its result: a design
                // that does not converge should be reported by the settle that
                // ends the timestep, not by an intermediate one that has only
                // seen part of the round's writes.
                let _ = self.propagate();
            }

            // A write an `a <= #2 b;` scheduled for this instant is a
            // *non-blocking* write like any other, so it lands here — after
            // every block that runs at this timestamp — rather than at the top
            // of it. Landing it first instead lets a `#2` in another block
            // read a value that has not been written yet: iverilog 12.0 prints
            // `00` for both `$display`s at time 2 of
            //
            // ```verilog
            // initial begin a = 0; a <= #2 8'haa; #2; $display("%h", a); end
            // initial begin #2; $display("%h", a); end
            // ```
            //
            // and `aa` only at time 3 (corpus `patch1268`). It goes ahead of
            // this timestamp's own non-blocking updates because it was
            // scheduled earlier, which is the order IEEE 1364-2005 §11.4 puts
            // two updates in.
            self.land_due_writes(time)?;
            commit_updates(pending, &mut self.state)?;
            self.propagate()?;
            self.settle()?;
            self.end_of_timestep()?;

            if self.finished() {
                return Ok(());
            }
        }

        self.now = target;
        self.state.set_time(target);
        // Once per call rather than once per timestep: a waveform read after
        // `advance` returns is the whole of the run so far, and a design that
        // dumps nothing pays one branch for it.
        self.tasks.flush_dump_file(&self.state);
        Ok(())
    }

    /// The next instant the design has something to do at: a queued block
    /// resumption, or a delayed `assign` whose new value is due to land.
    ///
    /// A design that names no delay on an `assign` answers out of the queue
    /// alone — `delays` is empty and the iterator ends immediately.
    fn next_time(&self) -> Option<i64> {
        let due = self
            .delays
            .iter()
            .chain(self.gate_delays.iter())
            .chain(self.udp_delays.iter())
            .flatten()
            .filter_map(|drive| drive.pending.as_ref().map(|(time, _)| *time))
            .chain(self.scheduled.iter().map(|(at, _)| *at))
            .min();
        match (self.queue.peek_time(), due) {
            (Some(queued), Some(due)) => Some(queued.min(due)),
            (queued, None) => queued,
            (None, due) => due,
        }
    }

    /// Moves every delayed `assign` transaction due at or before `time` onto
    /// the value it drives.
    ///
    /// Nothing is written here: the assignment is a continuous driver, so the
    /// value reaches the net through the very same
    /// [`propagate`](Simulator::propagate) pass an undelayed one goes through.
    /// Commits every scheduled write due at or before `time`.
    ///
    /// Unlike a delayed `assign`, which keeps driving, one of these is a
    /// **one-shot**: it lands once and is gone, which is what a non-blocking
    /// assignment means.
    fn land_due_writes(&mut self, time: i64) -> Result<bool, SimulationError> {
        if self.scheduled.is_empty() {
            return Ok(false);
        }
        // In scheduled order, so two writes landing at the same instant happen
        // the way the design wrote them.
        let mut due = Vec::new();
        let mut later = Vec::new();
        for (at, update) in self.scheduled.drain(..) {
            if at <= time {
                due.push(update);
            } else {
                later.push((at, update));
            }
        }
        self.scheduled = later;
        let mut changed = false;
        for update in due {
            changed |= drive_resolved(&mut self.state, update.target(), update.value())?;
        }
        Ok(changed)
    }

    /// Takes the scheduled writes out of a block's pending updates and holds
    /// them until their time. What is left is the ordinary non-blocking
    /// updates, which commit at the end of this delta cycle.
    fn hold_scheduled(&mut self, pending: Vec<PendingUpdate>) -> Vec<PendingUpdate> {
        if pending.iter().all(|update| update.at().is_none()) {
            return pending;
        }
        let mut immediate = Vec::with_capacity(pending.len());
        for update in pending {
            match update.at() {
                Some(at) => self.scheduled.push((at, update)),
                None => immediate.push(update),
            }
        }
        immediate
    }

    fn land_due_drives(&mut self, time: i64) {
        for drive in self
            .delays
            .iter_mut()
            .chain(self.gate_delays.iter_mut())
            .chain(self.udp_delays.iter_mut())
            .flatten()
        {
            let Some((at, _)) = &drive.pending else {
                continue;
            };
            if *at > time {
                continue;
            }
            let (_, value) = drive.pending.take().expect("the slot was just matched");
            drive.applied = Some(value);
        }
    }

    /// Resumes one block, queueing its continuation if it hits a delay. Returns
    /// its deferred updates and whether it ran to the end.
    fn resume_block(
        &mut self,
        cursor: ExecutionCursor,
    ) -> Result<(Vec<PendingUpdate>, bool), SimulationError> {
        let id = cursor.block;
        // Whatever this run writes, it writes while the block is *not* parked
        // at its event control — so none of it can wake the block. The flag is
        // what the next settle round measures that against.
        if let Some(ran) = self.ran_now.get_mut(id) {
            *ran = true;
        }
        let mut pc = cursor.pc;
        let mut carried = Vec::new();
        // A `disable` of another block does not suspend the block that wrote
        // it: the driver cancels what it named and control comes straight back
        // here. Looping rather than recursing keeps the store borrow inside
        // `resume` and lets the cancellation have the simulator mutably.
        loop {
            let outcome = program::resume(
                &self.blocks[id].program,
                pc,
                &mut self.state,
                &mut self.tasks,
            )?;
            let Resume::Disabled {
                scope,
                pc: next,
                pending,
            } = outcome
            else {
                let (mut updates, halted) = self.settled_resume(cursor, outcome)?;
                carried.append(&mut updates);
                // A scheduled write leaves the block here and waits on the
                // time wheel instead of committing with this delta cycle's
                // updates. Filtering at the one place updates leave a block
                // means no caller has to know the difference.
                let carried = self.hold_scheduled(carried);
                return Ok((carried, halted));
            };
            carried.extend(pending);
            // A `disable` that named a scope *this* thread is inside cancels
            // this activation too — `cancel_scope` has already re-queued the
            // block at the scope's end, so carrying on from `pc + 1` would
            // give the design two threads of one block. That is the `disable`
            // written inside a `fork` branch naming the scope around the
            // `fork`: the jump a local one compiles to cannot reach the
            // siblings, so it comes here instead.
            if self.cancel_scope(&scope, id)? {
                let carried = self.hold_scheduled(carried);
                return Ok((carried, true));
            }
            pc = next;
        }
    }

    /// Records that one branch of `fork` has reached its `join`, and re-queues
    /// the block that wrote the `fork` once the last of them has.
    ///
    /// Re-queueing at the current time rather than resuming inline is what
    /// makes the join resume at the **maximum** of the branch finish times:
    /// each branch arrives at whatever instant its own delays took it to, and
    /// the one that arrives last is by definition the latest.
    fn branch_arrived(&mut self, fork: usize) {
        let Some(Some(record)) = self.forks.get_mut(fork) else {
            return;
        };
        record.outstanding -= 1;
        if record.outstanding > 0 {
            return;
        }
        let parent = record.parent;
        self.forks[fork] = None;
        self.queue.insert(self.now, parent);
    }

    /// Takes a slot for a new `fork`, reusing one a finished fork gave back.
    fn open_fork(&mut self, record: ForkJoin) -> usize {
        match self.forks.iter().position(Option::is_none) {
            Some(free) => {
                self.forks[free] = Some(record);
                free
            }
            None => {
                self.forks.push(Some(record));
                self.forks.len() - 1
            }
        }
    }

    /// Whether `id` already has a thread of execution in flight, which is what
    /// stops `settle` starting a second copy of an `always` block part way
    /// through a `fork`.
    fn is_forking(&self, id: usize) -> bool {
        self.forks
            .iter()
            .flatten()
            .any(|record| record.parent.block == id)
    }

    /// Cancels every suspended block currently inside `scope`, re-queueing each
    /// one at the instruction its scope ends on.
    ///
    /// That is the whole of what disabling somebody else means: the LRM says
    /// execution continues with the statement following the disabled block, and
    /// a resume point here is a program counter, so "continue after it" is the
    /// scope's `end`. Queueing rather than resuming inline is what puts the
    /// cancelled block's remaining output *after* the block that disabled it,
    /// which is where iverilog puts it.
    ///
    /// A scope the design has nowhere is [`SimulationError::UnknownScope`]. A
    /// scope that exists but is not running anywhere is a no-op, which is what
    /// the LRM asks for — `always #6 disable foo;` cancels the enable of `foo`
    /// that happens to be in flight and says nothing about the times it is not.
    fn cancel_scope(&mut self, scope: &str, running: usize) -> Result<bool, SimulationError> {
        let known = self
            .blocks
            .iter()
            .any(|block| block.program.scopes().iter().any(|it| it.name == scope));
        if !known {
            return Err(SimulationError::UnknownScope(scope.to_string()));
        }

        // Which threads are inside the scope, and where each of them carries
        // on. A block parked at a `join` is on neither the queue nor the
        // waiting list — the fork record is the only thing holding it — so a
        // `disable` naming the scope the fork sits in has to look there as well
        // or the block would simply be lost.
        // `round` is the fourth place a live cursor can be: `advance` takes
        // every cursor due at this timestamp off the queue before it resumes
        // any of them, so a block that is due *now* and has not run yet is on
        // neither the queue nor the waiting list (corpus `sdw_dsbl`, where the
        // `disable` and the block it names are both due at time 15).
        let scheduled = self
            .queue
            .cursors()
            .copied()
            .chain(self.round.iter().copied())
            .chain(self.waiting.iter().map(|waiting| waiting.cursor));
        let mut whole: Vec<(usize, usize)> = Vec::new();
        let mut threads: Vec<(ExecutionCursor, usize)> = Vec::new();
        for cursor in scheduled {
            let Some(end) = self.blocks[cursor.block]
                .program
                .scope_end_containing(scope, cursor.pc)
            else {
                continue;
            };
            if self.is_branch_of_outside_fork(cursor, scope) {
                threads.push((cursor, end));
            } else if !whole.iter().any(|(id, _)| *id == cursor.block) {
                whole.push((cursor.block, end));
            }
        }
        // A `fork` whose *parent* is inside the scope is a fork the scope
        // contains, so the whole activation goes: every branch, and the block
        // held at the join.
        for record in self.forks.iter().flatten() {
            let cursor = record.parent;
            let Some(end) = self.blocks[cursor.block]
                .program
                .scope_end_containing(scope, cursor.pc)
            else {
                continue;
            };
            if !whole.iter().any(|(id, _)| *id == cursor.block) {
                whole.push((cursor.block, end));
            }
        }

        if whole.is_empty() && threads.is_empty() {
            return Ok(false);
        }
        let doomed: Vec<usize> = whole.iter().map(|(block, _)| *block).collect();
        // A block whose whole activation is going takes its branch threads
        // with it, so a thread of one is not re-queued a second time.
        threads.retain(|(cursor, _)| !doomed.contains(&cursor.block));
        let cancelled: Vec<ExecutionCursor> = threads.iter().map(|(cursor, _)| *cursor).collect();
        self.queue
            .retain(|cursor| doomed.contains(&cursor.block) || cancelled.contains(cursor));
        self.round
            .retain(|cursor| !doomed.contains(&cursor.block) && !cancelled.contains(cursor));
        self.waiting.retain(|waiting| {
            !doomed.contains(&waiting.cursor.block) && !cancelled.contains(&waiting.cursor)
        });
        for record in self.forks.iter_mut() {
            if record.is_some_and(|it| doomed.contains(&it.parent.block)) {
                *record = None;
            }
        }

        for (block, end) in whole {
            self.queue
                .insert(self.now, ExecutionCursor::new(block, end));
        }
        // A cancelled branch keeps its `fork`, so it still reaches its
        // `JoinBranch` and the join it belongs to still completes.
        for (cursor, end) in threads {
            self.queue.insert(
                self.now,
                ExecutionCursor {
                    block: cursor.block,
                    pc: end,
                    fork: cursor.fork,
                },
            );
        }
        Ok(doomed.contains(&running))
    }

    /// Whether `cursor` is a `fork` branch whose own `fork` sits *outside*
    /// `scope` — which is what tells a `disable` of a sibling branch's scope
    /// from a `disable` of a scope the whole `fork` is inside.
    ///
    /// In the first case only that one thread stops, and it still has to arrive
    /// at its `join` or the block would wait for an arrival that can never
    /// come. In the second the `fork` itself is being cancelled, so every
    /// branch and the block parked at the join go together.
    fn is_branch_of_outside_fork(&self, cursor: ExecutionCursor, scope: &str) -> bool {
        let Some(fork) = cursor.fork else {
            return false;
        };
        let Some(Some(record)) = self.forks.get(fork) else {
            return false;
        };
        // Asked of the `Fork` instruction rather than of the parent cursor:
        // a labelled `fork`'s own scope ends *at* the join the parent sits on,
        // so the parent is outside it either way and only the site tells
        // `disable fork_label` from a `disable` of something in one branch.
        self.blocks[record.parent.block]
            .program
            .scope_end_containing(scope, record.site)
            .is_none()
    }

    /// What the driver does with a [`Resume`] that is not a
    /// [`Resume::Disabled`]: queue a delay, arm a wait, or restart a
    /// free-running block that ran off its end.
    fn settled_resume(
        &mut self,
        cursor: ExecutionCursor,
        outcome: Resume,
    ) -> Result<(Vec<PendingUpdate>, bool), SimulationError> {
        let id = cursor.block;
        // A `$finish` ends the simulation at the end of *this* timestep, so
        // nothing is scheduled past one: a free-running block that halts does
        // not restart, a `#delay` has no later instant to resume at, and a
        // `wait` will never be offered another edge. Whatever is already
        // queued at this instant still runs, which is where the finishing
        // timestep's `$monitor` line comes from. Without the guard a
        // free-running block whose body ends in `$finish` restarts at the same
        // instant and never stops (corpus `always3.1.6D`).
        let finished = self.finished();
        match outcome {
            // A free-running `always` restarts the moment it finishes, which
            // is how `always begin #50 … end` keeps going forever — and how
            // `always value = @(ev) 5;` waits for the event again after the
            // one it was woken by. A *branch* running off the end of the block
            // is that thread ending, never the block restarting.
            Resume::Halted { pending } => {
                match cursor.fork {
                    Some(fork) => self.branch_arrived(fork),
                    None if self.blocks[id].free_running && !finished => {
                        self.queue.insert(self.now, ExecutionCursor::new(id, 0));
                    }
                    None => {}
                }
                Ok((pending, true))
            }
            Resume::Suspended { pc, delay, pending } => {
                if !finished {
                    self.queue
                        .insert(self.now + delay, ExecutionCursor { pc, ..cursor });
                }
                Ok((pending, false))
            }
            // Nothing schedules this one: it goes on the waiting list and
            // `settle` offers it every round of edges until one satisfies it.
            Resume::Waiting { pc, wait, pending } => {
                if finished {
                    return Ok((pending, false));
                }
                let watch = match wait {
                    WaitReason::Condition => None,
                    WaitReason::Event(control) => Some(EventWatch::arm(control, &self.state)),
                };
                self.waiting.push(Waiting {
                    cursor: ExecutionCursor { pc, ..cursor },
                    watch,
                });
                Ok((pending, false))
            }
            // One thread per branch, each queued at this instant so they all
            // get their turn before time moves. The block itself is held in
            // the fork record — it is on no queue and on no waiting list until
            // the last branch arrives.
            Resume::Forked {
                branches,
                site,
                pc,
                pending,
            } => {
                if finished {
                    return Ok((pending, false));
                }
                let fork = self.open_fork(ForkJoin {
                    parent: ExecutionCursor { pc, ..cursor },
                    site,
                    outstanding: branches.len(),
                });
                for branch in branches {
                    self.queue
                        .insert(self.now, ExecutionCursor::branch(id, branch, fork));
                }
                Ok((pending, false))
            }
            Resume::BranchDone { pending } => {
                match cursor.fork {
                    Some(fork) => self.branch_arrived(fork),
                    // A `JoinBranch` is only ever reached by a thread the
                    // driver started, so a cursor without a fork here means the
                    // compiled layout and the scheduler have parted company.
                    None => return Err(FORK_TIMING_UNSUPPORTED),
                }
                Ok((pending, true))
            }
            // `resume_block` takes this one before it gets here.
            Resume::Disabled { scope, .. } => Err(SimulationError::UnknownScope(scope)),
        }
    }

    /// Re-evaluates every `force` and procedural `assign` the design has
    /// installed, reporting whether any of them moved a signal.
    ///
    /// They are continuous drives, so this is one round of the same fixpoint
    /// the module's own `assign` statements settle in — which is what makes a
    /// forced signal follow its expression when an operand moves, rather than
    /// freeze at the value it had when the `force` ran.
    ///
    /// The drives are held through a handle rather than borrowed out of the
    /// store, because writing them needs the store mutably and the precedence
    /// rule needs the list to still be *in* it: an `assign` underneath a
    /// `force` has to see the force to know its own write goes nowhere.
    fn apply_drives(&mut self) -> Result<bool, SimulationError> {
        if !self.state.has_drives() {
            return Ok(false);
        }
        let drives = self.state.drives();
        let mut changed = false;
        for drive in drives.iter() {
            changed |= apply_drive(&mut self.state, drive)?;
        }
        Ok(changed)
    }

    /// Settles the continuous assignments and gates alike. See
    /// [`Simulator::run`].
    fn propagate(&mut self) -> Result<usize, SimulationError> {
        let limit = 2
            * (self.assignments.len()
                + self.gates.len()
                + self.udps.len()
                + self.pass_switches.len()
                + self.state.drive_count())
            + 4;
        // A continuous assignment is re-evaluated until the design stops
        // moving, so a function called from one prints an unpredictable number
        // of times. That is refused by name, on exactly the terms an
        // outstanding `$sscanf` fill already is — one length compare per pass.
        let printed = self.state.output().len();
        for pass in 1..=limit {
            let mut changed = false;
            let mut contributions: Vec<Contribution> = Vec::new();
            // A `supply` or `tri0`/`tri1` net drives itself, every pass, at its
            // own strength. Seeding it as the first contribution is what makes
            // `tri0 c; assign c = d;` read `0` while `d` is `z` and `1` once
            // `d` is `1` — the same resolution rule as any other contention,
            // with no separate case for a net that has no other driver.
            for pulled in &self.pulled_nets {
                let width = self
                    .state
                    .get_signal(&pulled.name)
                    .map_or(1, |signal| signal.width());
                contributions.push(Contribution {
                    target: ResolvedTarget::Whole(pulled.name.clone()),
                    value: Register::from_bits(vec![pulled.code; width]),
                    counted: true,
                    strength: Driven::Declared(DriveStrength {
                        zero: pulled.strength,
                        one: pulled.strength,
                    }),
                });
            }
            for (index, assignment) in self.assignments.iter().enumerate() {
                // The net being driven sizes the expression driving it, the
                // same way a procedural assignment's target does, so the
                // target is resolved before the right hand side is evaluated.
                let target = resolve_target(&self.state, assignment.lhs())?;
                let width = target.width(&self.state);
                let value = eval_sized(assignment.rhs(), &self.state, width)?;
                // A delay does not stop the assignment being a continuous
                // driver — it only changes which value it drives. The fresh
                // one goes into flight; what comes out here is the one that
                // has already landed.
                let value = match self.delays.get_mut(index).and_then(Option::as_mut) {
                    None => value,
                    Some(_) => {
                        let delay = assignment
                            .delay()
                            .expect("a delay slot belongs to a delayed assignment");
                        // Which of the rise, fall and turn-off delays applies
                        // is decided by the move being made, so it is measured
                        // from what the driver is asserting *now* rather than
                        // from where it is headed.
                        let applied = self.delays[index]
                            .as_ref()
                            .expect("the slot was just matched")
                            .applied
                            .clone();
                        let ticks = delay.ticks_between(applied.as_ref(), &value, &self.state)?;
                        let drive = self.delays[index]
                            .as_mut()
                            .expect("the slot was just matched");
                        // Inertial, not transport: a new value replaces
                        // whatever was in flight rather than queueing behind
                        // it, so a pulse shorter than the delay never reaches
                        // the net at all.
                        if drive.destination() != Some(&value) {
                            if ticks == 0 {
                                drive.applied = Some(value);
                                drive.pending = None;
                            } else {
                                drive.pending = Some((self.now + ticks, value));
                            }
                        }
                        match &drive.applied {
                            Some(applied) => applied.clone(),
                            None => Register::unknown(width),
                        }
                    }
                };
                // A net a gate also drives is resolved rather than written:
                // the assignment is one driver of it, not the only one. An
                // `assign` drives at `strong` unless it says otherwise, and
                // `assign (pull1, pull0) x = y;` saying otherwise is this one
                // value coming off the assignment instead of the constant.
                if self.target_is_resolved(&target) {
                    contributions.push(Contribution {
                        target,
                        value,
                        counted: true,
                        strength: Driven::Declared(
                            assignment.strength().unwrap_or(DriveStrength::STRONG),
                        ),
                    });
                } else {
                    changed |= drive_resolved(&mut self.state, &target, &value)?;
                }
            }
            // A continuous assignment is re-evaluated on every pass, so one
            // whose right hand side reads a file, writes its arguments or
            // calls a function that writes a design signal would do so an
            // unpredictable number of times. That is refused by name rather
            // than applied or dropped.
            if self.state.owes_fills() {
                return Err(SimulationError::Unsupported(
                    "an expression that writes the design — `$sscanf` or a function \
                     with a side effect — in a continuous assignment",
                ));
            }
            if self.state.output().len() != printed {
                return Err(SimulationError::Unsupported(
                    "a `$display` in a function called from a continuous assignment",
                ));
            }
            for (index, gate) in self.gates.iter().enumerate() {
                let (code, driven) = gate.evaluate(&self.state)?;
                // A delay does not stop a gate being a continuous driver — it
                // only changes which value it drives, exactly as it does for
                // an `assign`. The fresh value goes into flight; what comes
                // out here is the one that has already landed.
                let value = match self.gate_delays.get(index).and_then(Option::as_ref) {
                    None => Register::from_bits(vec![code]),
                    Some(drive) => {
                        let value = Register::from_bits(vec![code]);
                        let delay = gate
                            .delay
                            .as_ref()
                            .expect("a delay slot belongs to a delayed gate");
                        let ticks =
                            delay.ticks_between(drive.applied.as_ref(), &value, &self.state)?;
                        let drive = self.gate_delays[index]
                            .as_mut()
                            .expect("the slot was just matched");
                        if drive.destination() != Some(&value) {
                            if ticks == 0 {
                                drive.applied = Some(value);
                                drive.pending = None;
                            } else {
                                drive.pending = Some((self.now + ticks, value));
                            }
                        }
                        match &drive.applied {
                            Some(applied) => applied.clone(),
                            // A delayed gate is driving from the first
                            // instant; what it drives before its first
                            // transaction lands is `x`, not the `z` of a
                            // terminal nothing drives.
                            None => Register::unknown(1),
                        }
                    }
                };
                // A **delayed** gate drives at what it declared, not at the
                // strength its inputs say this instant: the two would be out
                // of step, since the value in hand is the one that landed
                // `#n` ago and the strength would be the one for the value
                // still in flight. A `bufif1` whose enable has just gone away
                // would then let go of the net at once and keep its turn-off
                // delay for nothing (corpus `rise_fall_decay2`). The declared
                // strength beside the landed value is exactly the pairing
                // every delayed gate had before a strength was modelled at
                // all; a delayed *switch* consequently drives at `strong`
                // rather than passing its source's level on.
                let driven = match self.gate_delays.get(index).and_then(Option::as_ref) {
                    Some(_) => Driven::Declared(gate.strength),
                    None => driven,
                };
                for output in &gate.outputs {
                    let target = scalar_output(&self.state, resolve_target(&self.state, output)?);
                    contributions.push(Contribution {
                        target,
                        value: value.clone(),
                        counted: true,
                        strength: driven,
                    });
                }
            }
            // A user-defined primitive drives its output exactly the way a gate
            // does: one bit, at strong strength, resolved against every other
            // driver of that net — and a `#(...)` on its instantiation is a
            // delay, which changes only *which* value it drives, through the
            // same `DelayedDrive` a gate uses.
            for (index, udp) in self.udps.iter().enumerate() {
                let code = udp.evaluate(&self.state)?;
                let value = match self.udp_delays.get(index).and_then(Option::as_ref) {
                    None => Register::from_bits(vec![code]),
                    Some(drive) => {
                        let value = Register::from_bits(vec![code]);
                        let delay = udp
                            .delay
                            .as_ref()
                            .expect("a delay slot belongs to a delayed primitive");
                        let ticks =
                            delay.ticks_between(drive.applied.as_ref(), &value, &self.state)?;
                        let drive = self.udp_delays[index]
                            .as_mut()
                            .expect("the slot was just matched");
                        if drive.destination() != Some(&value) {
                            if ticks == 0 {
                                drive.applied = Some(value);
                                drive.pending = None;
                            } else {
                                drive.pending = Some((self.now + ticks, value));
                            }
                        }
                        match &drive.applied {
                            Some(applied) => applied.clone(),
                            // Driving from the first instant, and what it
                            // drives before its first transaction lands is `x`.
                            None => Register::unknown(1),
                        }
                    }
                };
                let target = scalar_output(&self.state, resolve_target(&self.state, &udp.output)?);
                contributions.push(Contribution {
                    target,
                    value,
                    counted: true,
                    strength: Driven::Declared(DriveStrength::STRONG),
                });
            }
            // A `force` or a procedural `assign` on a *resolved* net is a
            // continuous driver of it like any other, so it joins the pool
            // rather than only writing the net. On the net it names it wins
            // either way — `exec::held_bits` discards the resolved write over a
            // held bit — but through a `tran` it has to *contend* with what the
            // far side is driving: iverilog 12.0 answers `assign y = a;
            // tran (x, y); force x = 0;` with `a` at 1 as `x=0 y=x` (corpus
            // `pr2937417b`).
            // A drive keeps the *name* its target lands on, so a design that
            // forces an ordinary register answers this without resolving
            // anything — the same shape `resolved_nets` already answers with.
            if self.state.has_drives() {
                let drives = self.state.drives();
                for drive in drives.iter() {
                    if !drive.names().iter().any(|name| self.is_resolved(name)) {
                        continue;
                    }
                    let target = resolve_target(&self.state, drive.target())?;
                    if target.is_multiple() {
                        continue;
                    }
                    let width = target.width(&self.state);
                    let value = eval_sized(drive.value(), &self.state, width)?;
                    contributions.push(Contribution {
                        target,
                        value,
                        // A `force` contends but is not a *driver*: iverilog
                        // answers `$countdrivers` over a forced net with the
                        // drivers it had before the force.
                        counted: false,
                        strength: Driven::Declared(DriveStrength::STRONG),
                    });
                }
            }
            // Which nets a `tran` joins is worked out here, every pass, rather
            // than partitioned once at elaboration: a `tranif`'s control moves
            // during the run, and corpus `tran-keeper` gates a switch on the
            // very net it is holding up.
            let switches = self.switch_bits()?;
            changed |= self.resolve_contributions(contributions, &switches)?;
            changed |= self.apply_drives()?;
            if !changed {
                return Ok(pass);
            }
        }
        Err(SimulationError::NoConvergence { passes: limit })
    }

    /// Marks every net a continuous driver names as one to resolve.
    ///
    /// Only a design that calls `$countdrivers` goes through here: resolution
    /// of a single `strong` driver gives what writing it gave, so the values
    /// do not move, and what is gained is the per-bit driver list the answer
    /// is read off. A target that does not resolve to a single signal — a
    /// concatenation — is left alone, since `target_is_resolved` would refuse
    /// it anyway.
    fn resolve_every_driven_net(&mut self) {
        let mut driven: Vec<String> = Vec::new();
        let mut note = |target: &Expression, state: &StateStore| {
            if let Ok(resolved) = resolve_target(state, target) {
                if !resolved.is_multiple() {
                    driven.push(resolved.name().to_string());
                }
            }
        };
        for assignment in &self.assignments {
            note(assignment.lhs(), &self.state);
        }
        for gate in &self.gates {
            for output in &gate.outputs {
                note(output, &self.state);
            }
        }
        for udp in &self.udps {
            note(&udp.output, &self.state);
        }
        self.resolved_nets.extend(driven);
    }

    /// Whether a net has to be resolved between its drivers rather than simply
    /// written.
    ///
    /// A design with no gates in it answers without hashing the name, the same
    /// shape `StateStore::any_signed` and `any_memory` use.
    fn is_resolved(&self, name: &str) -> bool {
        !self.resolved_nets.is_empty() && self.resolved_nets.contains(name)
    }

    /// Whether a *target* has to be resolved between its drivers.
    ///
    /// A concatenation never does: it names several signals, so the one name
    /// the resolution path groups by would be a lie. It goes down the plain
    /// write path, which splits it.
    fn target_is_resolved(&self, target: &ResolvedTarget) -> bool {
        !target.is_multiple() && self.is_resolved(target.name())
    }

    /// Combines one pass's worth of driver contributions and writes the result.
    ///
    /// Every driver of a resolved net contributes a value and a strength, and
    /// each *bit* is settled on its own by [`resolve_bit`]: a bit nothing
    /// reaches keeps what it held, so a driver of `bus[0]` says nothing about
    /// `bus[1]`. The whole net is then written once, which is what keeps a
    /// three-state bus out of the change journal while it is not moving.
    ///
    /// One **word of an array of nets** is a net in its own right, so it is
    /// grouped by its address as well as by its name: `wire [1:0] foo [0:1];`
    /// with two strength-bearing drivers on `foo[0]` and two more on `foo[1]`
    /// has four drivers and two driver *lists*, and they must not be pooled.
    fn resolve_contributions(
        &mut self,
        contributions: Vec<Contribution>,
        switches: &[SwitchBits],
    ) -> Result<bool, SimulationError> {
        if contributions.is_empty() && switches.is_empty() {
            return Ok(false);
        }
        // Grouped by net, in the order the drivers were written, so a design
        // resolves the same way twice.
        let mut nets: Vec<(&str, Option<i64>)> = Vec::new();
        for contribution in &contributions {
            let net = (
                contribution.target.name(),
                contribution.target.word_address(),
            );
            if !nets.contains(&net) {
                nets.push(net);
            }
        }
        // A switch terminal is a net whether or not anything drives it: an
        // undriven one still has to be *written* — with whatever the node it
        // joins resolved to, or with `z` when it joins nothing.
        for switch in switches {
            for end in &switch.ends {
                let net = (end.name.as_str(), None);
                if !nets.contains(&net) {
                    nets.push(net);
                }
            }
        }
        let mut resolving: Vec<NetDrivers> = Vec::with_capacity(nets.len());
        for (name, address) in nets {
            // A memory word's width and held value come from the memory map,
            // and a signal's from the signal map — a name is in one or the
            // other, never both, so the address is what says which to ask.
            let (width, bits) = match address {
                Some(index) => {
                    let memory = self
                        .state
                        .memory(name)
                        .ok_or_else(|| SimulationError::UnknownSignal(name.to_string()))?;
                    (memory.width(), memory.word(Some(index)).get_raw().to_vec())
                }
                None => {
                    let signal = self
                        .state
                        .get_signal(name)
                        .ok_or_else(|| SimulationError::UnknownSignal(name.to_string()))?;
                    (signal.width(), signal.register().get_raw().to_vec())
                }
            };
            // Bits run most significant first, the way a `Register` is written.
            let mut driven: Vec<Vec<BitDriver>> = vec![Vec::new(); width];
            for contribution in &contributions {
                if contribution.target.name() != name
                    || contribution.target.word_address() != address
                {
                    continue;
                }
                match &contribution.target {
                    // A whole net, or a whole word of an array of nets.
                    ResolvedTarget::Whole(_) | ResolvedTarget::Word { .. } => contribute_whole(
                        &mut driven,
                        &contribution.value,
                        contribution.strength,
                        contribution.counted,
                    ),
                    ResolvedTarget::Bits { indices, .. } => {
                        let signal = self
                            .state
                            .get_signal(name)
                            .expect("a bit select's signal was just looked up");
                        let value = contribution.value.coerced(indices.len());
                        for (offset, index) in indices.iter().enumerate() {
                            let Some(position) = signal.bit_position(*index) else {
                                continue;
                            };
                            driven[position].push(BitDriver {
                                strength: contribution.strength.of(value.get_raw()[offset]),
                                counted: contribution.counted,
                            });
                        }
                    }
                    // Bits of a word are grouped by the same address a whole
                    // word is, so this list is that word's — only the mapping
                    // from a declared index to a position is the memory's
                    // rather than a signal's.
                    ResolvedTarget::WordBits { indices, .. } => {
                        let memory = self
                            .state
                            .memory(name)
                            .expect("a word select's memory was just looked up");
                        let value = contribution.value.coerced(indices.len());
                        for (offset, index) in indices.iter().enumerate() {
                            let Some(position) = bit_position_in(memory.range(), *index) else {
                                continue;
                            };
                            driven[position].push(BitDriver {
                                strength: contribution.strength.of(value.get_raw()[offset]),
                                counted: contribution.counted,
                            });
                        }
                    }
                    // An event holds no value to resolve, and a concatenation
                    // never reaches here — `target_is_resolved` reports it
                    // unresolved so it goes down the plain write path.
                    ResolvedTarget::Event(_)
                    | ResolvedTarget::Parts(_)
                    | ResolvedTarget::Nowhere => {}
                }
            }
            resolving.push(NetDrivers {
                name: name.to_string(),
                address,
                bits,
                driven,
            });
        }
        // A design with no `tran` in it pays one `is_empty` for the question.
        if !switches.is_empty() {
            bond_nodes(&mut resolving, switches);
        }
        let mut changed = false;
        for net in resolving {
            let mut bits = net.bits;
            // The strength of every bit, beside the value — which is what `%v`
            // prints and what a value alone cannot say. A bit no driver reaches
            // keeps the strength it was last resolved at, exactly as it keeps
            // its value.
            let mut levels = self.state.strengths_of(&net.name, bits.len());
            for (position, drivers) in net.driven.iter().enumerate() {
                if !drivers.is_empty() {
                    let resolved = resolve_strength(drivers.iter().map(|d| d.strength));
                    bits[position] = resolved.value();
                    levels[position] = resolved;
                }
            }
            let target = match net.address {
                Some(index) => ResolvedTarget::Word {
                    name: net.name,
                    index,
                },
                None => ResolvedTarget::Whole(net.name),
            };
            // A memory word has no strength recorded: a name is in the signal
            // map or the memory map and never both, and only the signal map
            // has somewhere to keep one.
            //
            // A **strength** that moved counts as a change, because a MOS
            // switch passes its source's strength on: `pullup (w); bufif1 (w,
            // 1'b1, g);` leaves `w` at `1` whether `g` is on or not and only
            // the level moves, and the `pmos` downstream of it has to be
            // re-evaluated or it carries the stale one for the rest of the run
            // (corpus `resolv1`).
            if let ResolvedTarget::Whole(name) = &target {
                changed |= self
                    .state
                    .get_signal(name)
                    .is_some_and(|signal| signal.strengths() != Some(levels.as_slice()));
                self.state.set_strengths(name, levels);
                // The driver *tally* is what `$countdrivers` reports, and it
                // is taken after `bond_nodes` has pooled — a bit joined to
                // another by a `tran`, which is what an `inout` port bound to
                // a select is, has the node's drivers and not its own (corpus
                // `countdrivers3`). A design that never asks builds none.
                if self.state.counts_drivers() {
                    let counts = net
                        .driven
                        .iter()
                        .map(|drivers| {
                            let mut tally = DriverTally::default();
                            for driver in drivers.iter().filter(|driver| driver.counted) {
                                tally.count(driver.strength.value());
                            }
                            tally
                        })
                        .collect();
                    self.state.set_driver_counts(name, counts);
                }
            }
            changed |= drive_resolved(&mut self.state, &target, &Register::from_bits(bits))?;
        }
        Ok(changed)
    }

    /// Where each pass switch's terminals land in the flat store this pass, and
    /// whether the switch is conducting.
    ///
    /// Both halves have to be asked afresh every pass: a `tranif`'s control is
    /// an ordinary expression that moves during the run, and a terminal may be
    /// a select whose index does.
    fn switch_bits(&self) -> Result<Vec<SwitchBits>, SimulationError> {
        if self.pass_switches.is_empty() {
            return Ok(Vec::new());
        }
        let mut bits = Vec::with_capacity(self.pass_switches.len());
        for switch in &self.pass_switches {
            let (Some(first), Some(second)) = (
                self.terminal_bit(&switch.terminals[0])?,
                self.terminal_bit(&switch.terminals[1])?,
            ) else {
                // A terminal naming a bit the net does not have joins nothing,
                // which is what an out-of-range driver already contributes.
                continue;
            };
            bits.push(SwitchBits {
                ends: [first, second],
                conducting: switch.conducts(&self.state)?,
            });
        }
        Ok(bits)
    }

    /// The one bit of one net a switch terminal names.
    ///
    /// A whole net names its **least significant** bit, which is the same bit a
    /// scalar connection to any other primitive terminal names — and a wider
    /// terminal on a switch that is not an array is illegal Verilog anyway
    /// (iverilog 12.0 refuses `tran t(p, q);` for two-bit `p` and `q` with
    /// "Expression width 2 does not match width 1 of logic gate array port").
    fn terminal_bit(&self, terminal: &Expression) -> Result<Option<TerminalBit>, SimulationError> {
        let target = resolve_target(&self.state, terminal)?;
        let (ResolvedTarget::Whole(name) | ResolvedTarget::Bits { name, .. }) = &target else {
            return Err(SWITCH_TERMINAL_UNSUPPORTED);
        };
        let signal = self
            .state
            .get_signal(name)
            .ok_or_else(|| SimulationError::UnknownSignal(name.clone()))?;
        let position = match &target {
            ResolvedTarget::Whole(_) => signal.width().checked_sub(1),
            ResolvedTarget::Bits { indices, .. } if indices.len() == 1 => {
                signal.bit_position(indices[0])
            }
            _ => return Err(SWITCH_TERMINAL_UNSUPPORTED),
        };
        Ok(position.map(|position| TerminalBit {
            name: name.clone(),
            position,
        }))
    }
}

/// A terminal a pass switch cannot join: a concatenation, an arithmetic
/// expression, a memory word. Nothing about it is a bit of a net, so there is
/// no node for it to be part of.
const SWITCH_TERMINAL_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a `tran` terminal that is not a net or one bit of one");

/// Where one end of a pass switch lands in the flat store.
struct TerminalBit {
    name: String,
    /// The bit's position in the net's raw bits, most significant first — the
    /// same indexing the per-bit driver lists use.
    position: usize,
}

/// One pass switch measured against the design as it stands this pass.
struct SwitchBits {
    ends: [TerminalBit; 2],
    conducting: bool,
}

/// One net's per-bit driver lists, before they are resolved.
struct NetDrivers {
    name: String,
    address: Option<i64>,
    /// What the net holds now. A bit no driver reaches keeps its value.
    bits: Vec<u8>,
    /// The drivers of each bit, most significant first.
    driven: Vec<Vec<BitDriver>>,
}

/// Pools the driver lists of every bit a conducting pass switch joins.
///
/// A `tran` makes its two terminals one **node**, and a node's value is what
/// every driver of every net in it resolves to *together* — the same
/// [`resolve_bit`] every other contention already goes through. So the whole of
/// the model is that the bits in one node share one driver list, and nothing
/// about the value rule changed. Copying a value from one terminal to the other
/// instead latches: a driver letting go of `a` would leave `b` holding the
/// stale value, which copies straight back, and the net would never float
/// again.
///
/// The partition is by **bit** rather than by net, because `tran (a[0], a[1]);`
/// joins two bits of one vector (corpus `pr3296466a`).
///
/// A terminal is seeded with `z` before anything is merged, and that is what
/// makes a switch *opening* mean something: a net a `tranif` has just let go of
/// has no driver of its own this pass, and without the seed it would keep the
/// value it held. Measured against iverilog 12.0, `assign p = a;
/// tranif1 (p, q, en);` gives `p=1 q=1` while `en` is high and `p=1 q=z` once
/// it goes low.
fn bond_nodes(nets: &mut [NetDrivers], switches: &[SwitchBits]) {
    // Which net each terminal belongs to, worked out while `nets` is only
    // borrowed for reading. A switch whose terminal names no net in the list
    // has nothing to join — `resolve_contributions` puts every terminal in the
    // list, so that is only a net the store does not have.
    let cells: Vec<Option<[(usize, usize); 2]>> = {
        let index: HashMap<&str, usize> = nets
            .iter()
            .enumerate()
            .filter(|(_, net)| net.address.is_none())
            .map(|(position, net)| (net.name.as_str(), position))
            .collect();
        switches
            .iter()
            .map(|switch| {
                let first = *index.get(switch.ends[0].name.as_str())?;
                let second = *index.get(switch.ends[1].name.as_str())?;
                Some([
                    (first, switch.ends[0].position),
                    (second, switch.ends[1].position),
                ])
            })
            .collect()
    };
    for ends in cells.iter().flatten() {
        for (net, position) in ends {
            if let Some(drivers) = nets[*net].driven.get_mut(*position) {
                drivers.push(BitDriver {
                    strength: Strength::HIGHZ,
                    counted: false,
                });
            }
        }
    }
    let mut node = Nodes::default();
    for (switch, ends) in switches.iter().zip(&cells) {
        let Some(ends) = ends else { continue };
        if !switch.conducting {
            continue;
        }
        let first = node.cell(ends[0]);
        let second = node.cell(ends[1]);
        node.join(first, second);
    }
    let mut pooled: HashMap<usize, Vec<BitDriver>> = HashMap::new();
    for id in 0..node.cells.len() {
        let (net, position) = node.cells[id];
        let root = node.root(id);
        let drivers = nets[net].driven.get(position).cloned().unwrap_or_default();
        pooled.entry(root).or_default().extend(drivers);
    }
    for id in 0..node.cells.len() {
        let (net, position) = node.cells[id];
        let root = node.root(id);
        if let (Some(slot), Some(drivers)) = (nets[net].driven.get_mut(position), pooled.get(&root))
        {
            *slot = drivers.clone();
        }
    }
}

/// The union-find that decides which bits are one node, rebuilt every pass.
#[derive(Default)]
struct Nodes {
    /// Each cell's `(net, bit position)`, indexed by its id.
    cells: Vec<(usize, usize)>,
    ids: HashMap<(usize, usize), usize>,
    parent: Vec<usize>,
}

impl Nodes {
    /// The id of a cell, added to the partition the first time it is named.
    fn cell(&mut self, cell: (usize, usize)) -> usize {
        *self.ids.entry(cell).or_insert_with(|| {
            self.cells.push(cell);
            self.parent.push(self.parent.len());
            self.parent.len() - 1
        })
    }

    fn root(&mut self, mut id: usize) -> usize {
        while self.parent[id] != id {
            self.parent[id] = self.parent[self.parent[id]];
            id = self.parent[id];
        }
        id
    }

    fn join(&mut self, first: usize, second: usize) {
        let first = self.root(first);
        let second = self.root(second);
        if first != second {
            self.parent[second] = first;
        }
    }
}

/// Adds one driver's claim on a whole net — or a whole memory word — to the
/// per-bit driver lists.
fn contribute_whole(
    driven: &mut [Vec<BitDriver>],
    value: &Register,
    strength: Driven,
    counted: bool,
) {
    let value = value.coerced(driven.len());
    for (offset, slot) in driven.iter_mut().enumerate() {
        slot.push(BitDriver {
            strength: strength.of(value.get_raw()[offset]),
            counted,
        });
    }
}

/// One continuous driver's claim on a net for one propagation pass.
struct Contribution {
    target: ResolvedTarget,
    value: Register,
    strength: Driven,
    /// Whether `$countdrivers` counts this one. A `force` or a procedural
    /// `assign` is contributed so that it *contends* through a `tran` like any
    /// other driver, but iverilog does not count one as a **driver** of the
    /// net: `force n = 2'bxx;` over `wire [1:0] n = 2'b0x;` still answers one
    /// driver, driving `0` (measured against iverilog 12.0, corpus
    /// `countdrivers2`).
    counted: bool,
}

/// One driver's claim on one *bit*, which is what the resolution is over.
#[derive(Clone, Copy)]
struct BitDriver {
    strength: Strength,
    counted: bool,
}

/// A gate terminal is one bit, so an output connected to a vector drives that
/// vector's least significant bit and leaves the rest of it alone.
fn scalar_output(state: &StateStore, target: ResolvedTarget) -> ResolvedTarget {
    match target {
        ResolvedTarget::Whole(name) => match state.get_signal(&name).map(|signal| signal.range()) {
            Some((_, least)) if state.get_signal(&name).is_some_and(|s| s.width() > 1) => {
                ResolvedTarget::Bits {
                    name,
                    indices: vec![least],
                }
            }
            _ => ResolvedTarget::Whole(name),
        },
        ResolvedTarget::Bits { name, mut indices } if indices.len() > 1 => {
            let least = indices.pop().expect("a select names at least one bit");
            ResolvedTarget::Bits {
                name,
                indices: vec![least],
            }
        }
        other => other,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::fs;
    use std::path::PathBuf;

    use crate::parsers::modules::parse_module_declaration;

    fn simulator_for(source: &str) -> Simulator {
        let (remaining, module) = parse_module_declaration(source).unwrap();
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        let mut simulator = Simulator::new(module);
        simulator.setup().unwrap();
        simulator
    }

    fn one() -> Register {
        Register::from_u128(1, 1)
    }

    fn zero() -> Register {
        Register::from_u128(0, 1)
    }

    fn simulator_for_example(name: &str) -> Simulator {
        let path: PathBuf = [
            env!("CARGO_MANIFEST_DIR"),
            "src",
            "verilog",
            "examples",
            name,
        ]
        .iter()
        .collect();
        let source = fs::read_to_string(&path).expect("unable to read example");
        simulator_for(&source)
    }

    /// A whole design opening a file, writing to it and to standard output at
    /// once, and closing it — which is corpus `fopen1` and `fopen2` in
    /// miniature. The `|1` is what makes the design's own report reach the
    /// output buffer a self-checking test reads.
    #[test]
    fn test_a_design_writes_to_a_file_and_to_the_output_buffer() {
        let directory = std::env::temp_dir().join("visilog-runner-fopen");
        let _ = fs::remove_dir_all(&directory);
        fs::create_dir_all(&directory).expect("scratch directory should be creatable");

        let (remaining, module) = parse_module_declaration(
            r#"
            module writer;
                integer fp;
                initial begin
                    fp = $fopen("out.txt");
                    if (fp != 2) $display("FAILED fp=%0d", fp);
                    $fdisplay(fp, "to the file");
                    $fdisplay(fp | 1, "to both");
                    $fclose(fp);
                    $display("PASSED");
                end
            endmodule
        "#,
        )
        .unwrap();
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        let mut simulator = Simulator::new(module);
        simulator.set_output_directory(&directory);
        simulator.setup().unwrap();
        simulator.advance(10).unwrap();

        assert_eq!(simulator.output().text(), "to both\nPASSED\n");
        let written = fs::read_to_string(directory.join("out.txt")).expect("file should exist");
        assert_eq!(written, "to the file\nto both\n");
    }

    /// Runs a one-module design to completion against a scratch directory,
    /// which is both where it writes and where it looks for what it reads.
    fn run_with_files(name: &str, source: &str, files: &[(&str, &str)]) -> Simulator {
        let directory = std::env::temp_dir().join(format!("visilog-runner-{}", name));
        let _ = fs::remove_dir_all(&directory);
        fs::create_dir_all(&directory).expect("scratch directory should be creatable");
        for (file, text) in files {
            fs::write(directory.join(file), text).expect("scratch file should be writable");
        }
        let (remaining, module) = parse_module_declaration(source).unwrap();
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        let mut simulator = Simulator::new(module);
        simulator.set_output_directory(&directory);
        simulator.add_search_path(&directory);
        simulator.setup().unwrap();
        simulator.advance(10).unwrap();
        simulator
    }

    /// `$sscanf` both answers a count and writes through its arguments, and
    /// the writes have landed by the next statement — corpus `scanf` in
    /// miniature, plus the partial match that stops part way.
    #[test]
    fn test_sscanf_writes_through_its_arguments() {
        let simulator = run_with_files(
            "sscanf",
            r#"
            module top;
                integer cnt;
                reg [31:0] a1, a2, a3;
                initial begin
                    cnt = $sscanf("123 hex beaf bin 01xz", "%d hex %h bin %b", a1, a2, a3);
                    $display("%0d %0d %h %b", cnt, a1, a2, a3[3:0]);
                    a2 = 0;
                    cnt = $sscanf("12;34", "%d,%d", a1, a2);
                    $display("%0d %0d %0d", cnt, a1, a2);
                    cnt = $sscanf("", "%d", a1);
                    $display("%0d", cnt);
                end
            endmodule
        "#,
            &[],
        );
        assert_eq!(
            simulator.output().text(),
            "3 123 0000beaf 01xz\n1 12 0\n-1\n"
        );
    }

    /// A file opened for reading is found through the search path, and reading
    /// it runs out as `-1` rather than as a quiet `0` — corpus `pr2824189`.
    #[test]
    fn test_fscanf_reads_to_end_of_file() {
        let simulator = run_with_files(
            "fscanf",
            r#"
            module top;
                integer fd, n, v;
                initial begin
                    fd = $fopen("data.txt", "r");
                    n = $fscanf(fd, " %d ", v);
                    $display("%0d %0d", n, v);
                    n = $fscanf(fd, " %d ", v);
                    $display("%0d %0d", n, v);
                    n = $fscanf(fd, "%d", v);
                    $display("%0d %0d", n, v);
                    n = $fscanf(fd, "%d", v);
                    $display("%0d %0d", n, v);
                    $fclose(fd);
                end
            endmodule
        "#,
            &[("data.txt", "7\n8\nq\n")],
        );
        // The `q` is not a digit, so the third scan is a real `0` — and it
        // leaves the `q` where it was, so the fourth is `0` again rather than
        // end of file.
        assert_eq!(simulator.output().text(), "1 7\n1 8\n0 8\n0 8\n");
    }

    /// A file the design wrote under a relative name is found again under the
    /// same name when it is opened to read — through the output directory,
    /// with no search path configured at all (corpus `pr1876798`).
    #[test]
    fn test_a_design_reads_back_what_it_wrote() {
        let directory = std::env::temp_dir().join("visilog-runner-roundtrip");
        let _ = fs::remove_dir_all(&directory);
        fs::create_dir_all(directory.join("work")).expect("scratch directory should be creatable");
        let (_, module) = parse_module_declaration(
            r#"
            module top;
                integer f, i, n;
                integer v [0:3];
                initial begin
                    f = $fopen("work/temp.txt", "w");
                    for (i = 0; i < 4; i = i + 1) $fdisplay(f, "%d", i);
                    $fclose(f);
                    f = $fopen("work/temp.txt", "r");
                    for (i = 0; i < 4; i = i + 1) n = $fscanf(f, " %d ", v[i]);
                    $fclose(f);
                    $display("%0d %0d %0d %0d", v[0], v[1], v[2], v[3]);
                end
            endmodule
        "#,
        )
        .unwrap();
        let mut simulator = Simulator::new(module);
        simulator.set_output_directory(&directory);
        simulator.setup().unwrap();
        simulator.advance(10).unwrap();
        assert_eq!(simulator.output().text(), "0 1 2 3\n");
    }

    /// `$fgets`, `$feof`, `$ftell`, `$rewind`, `$fgetc` and `$ungetc` against
    /// one file, with the answers measured against iverilog 12.0.
    #[test]
    fn test_line_and_character_reads() {
        let simulator = run_with_files(
            "fgets",
            r#"
            module top;
                integer fd, r, c;
                reg [8*20:1] s;
                initial begin
                    fd = $fopen("in.txt", "r");
                    r = $fgets(s, fd);
                    $display("%0d <%0s> %0d %0d", r, s[8*20:9], $ftell(fd), $feof(fd));
                    r = $fgets(s, fd);
                    $display("%0d <%0s> %0d %0d", r, s[8*20:9], $ftell(fd), $feof(fd));
                    r = $fgets(s, fd);
                    $display("%0d %0d", r, $feof(fd));
                    r = $rewind(fd);
                    c = $fgetc(fd);
                    r = $ungetc(c, fd);
                    $display("%0d %0d %0d %0d", r, c, $fgetc(fd), $feof(fd));
                    $fclose(fd);
                end
            endmodule
        "#,
            &[("in.txt", "hello 42\nsecond\n")],
        );
        assert_eq!(
            simulator.output().text(),
            "9 <hello 42> 9 0\n7 <second> 16 0\n0 1\n0 104 104 0\n"
        );
    }

    /// Reading a descriptor that names no readable file is end of file — a
    /// channel, a write-only file and a closed one alike, which is what
    /// iverilog answers — while an update mode, which this simulator cannot
    /// read, is a named error rather than a silent `-1`.
    #[test]
    fn test_reading_what_is_not_readable() {
        let simulator = run_with_files(
            "unreadable",
            r#"
            module top;
                integer mcd, fd, n, v;
                initial begin
                    mcd = $fopen("channel.txt");
                    fd = $fopen("written.txt", "w");
                    n = $fscanf(mcd, "%d", v);
                    $display("%0d", n);
                    n = $fscanf(fd, "%d", v);
                    $display("%0d", n);
                    n = $fgets(v, fd);
                    $display("%0d", n);
                end
            endmodule
        "#,
            &[],
        );
        assert_eq!(simulator.output().text(), "-1\n-1\n0\n");

        let (_, module) = parse_module_declaration(
            r#"
            module top;
                integer fd, n, v;
                initial begin
                    fd = $fopen("update.txt", "w+");
                    n = $fscanf(fd, "%d", v);
                end
            endmodule
        "#,
        )
        .unwrap();
        let directory = std::env::temp_dir().join("visilog-runner-update");
        fs::create_dir_all(&directory).expect("scratch directory should be creatable");
        let mut simulator = Simulator::new(module);
        simulator.set_output_directory(&directory);
        // `setup` runs time zero, which is when the `initial` block reads.
        let error = simulator.setup().expect_err("an update mode is refused");
        assert!(error.to_string().contains("update mode"), "{}", error);
    }

    #[test]
    fn test_simple_adder() {
        let mut simulator = simulator_for(
            r#"
            module adder(
                input [7:0] a,
                input [7:0] b,
                output [7:0] c
            );
                assign c = a + b;
            endmodule
        "#,
        );

        simulator
            .set_input("a", Register::from_u128(200, 8))
            .unwrap();
        simulator.set_input("b", Register::from_u128(0, 8)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("c").unwrap().to_u128(), Some(200));

        // The sum wraps at the width of the operands.
        simulator
            .set_input("b", Register::from_u128(100, 8))
            .unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("c").unwrap().to_u128(), Some(44));
    }

    #[test]
    fn test_simple_module_example() {
        let mut simulator = simulator_for_example("simple_module.v");

        // The continuous assignments have settled before anything ran, so
        // `sum` already holds `a + b` — and `a` and `b` are undriven nets, so
        // that is `x`, not the `z` they themselves read. iverilog prints
        // `sum=xxxx a=zzzz` for the same design.
        assert_eq!(simulator.get("sum").unwrap().to_binary(), "xxxx");

        simulator.set_input("a", Register::from_u128(3, 4)).unwrap();
        simulator.set_input("b", Register::from_u128(5, 4)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("sum").unwrap().to_binary(), "1000");

        // [3:0] + [3:0] wraps rather than carrying out.
        simulator
            .set_input("a", Register::from_u128(15, 4))
            .unwrap();
        simulator.set_input("b", Register::from_u128(2, 4)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("sum").unwrap().to_u128(), Some(1));
    }

    #[test]
    fn test_simple_module_propagates_unknown() {
        let mut simulator = simulator_for_example("simple_module.v");

        // A carry can reach any output bit, so a single unknown input bit makes
        // the whole sum unknown.
        simulator
            .set_input("a", Register::from_binary("001x"))
            .unwrap();
        simulator.set_input("b", Register::from_u128(5, 4)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("sum").unwrap().to_binary(), "xxxx");
    }

    #[test]
    fn test_parity_calculator_example() {
        let mut simulator = simulator_for_example("parity_calculator.v");

        simulator
            .set_input("data", Register::from_binary("10110010"))
            .unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("parity").unwrap().to_binary(), "0");

        simulator
            .set_input("data", Register::from_binary("10110011"))
            .unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("parity").unwrap().to_binary(), "1");

        // Parity has no dominant value, so one unknown bit is enough.
        simulator
            .set_input("data", Register::from_binary("1011001x"))
            .unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("parity").unwrap().to_binary(), "x");
    }

    #[test]
    fn test_multi_stage_dataflow_needs_several_passes() {
        // The assignments are written consumer first, so a single pass would
        // leave `c` holding the stale, still unknown value of `b`.
        let mut simulator = simulator_for(
            r#"
            module chain(
                input [3:0] a,
                output [3:0] c
            );
                wire [3:0] b;
                assign c = b + 1;
                assign b = a + 1;
            endmodule
        "#,
        );

        simulator.set_input("a", Register::from_u128(4, 4)).unwrap();
        let passes = simulator.run().unwrap();

        assert_eq!(simulator.get("b").unwrap().to_u128(), Some(5));
        assert_eq!(simulator.get("c").unwrap().to_u128(), Some(6));
        // Two passes to settle, one more to notice nothing moved.
        assert_eq!(passes, 3);
    }

    #[test]
    fn test_combinational_loop_does_not_converge() {
        // A plain `assign a = ~a;` settles: `~x` is `x`. Case inequality gives a
        // definite answer for an unknown operand, so this really does oscillate.
        let mut simulator = simulator_for(
            r#"
            module oscillator(
                output a
            );
                assign a = a !== 1'b1;
            endmodule
        "#,
        );

        assert_eq!(
            simulator.run(),
            Err(SimulationError::NoConvergence { passes: 6 })
        );
    }

    #[test]
    fn test_bit_and_part_select_targets() {
        let mut simulator = simulator_for(
            r#"
            module packer(
                input [1:0] hi,
                input lo,
                output [3:0] out
            );
                assign out[3:2] = hi;
                assign out[0] = lo;
            endmodule
        "#,
        );

        simulator
            .set_input("hi", Register::from_binary("10"))
            .unwrap();
        simulator
            .set_input("lo", Register::from_binary("1"))
            .unwrap();
        simulator.run().unwrap();

        // Bit 1 has no driver, so it reads `z` — `out` is a net, and an
        // undriven net is high-impedance rather than unknown. iverilog prints
        // `10z1` for this module.
        assert_eq!(simulator.get("out").unwrap().to_binary(), "10z1");
    }

    /// An indexed part select works as an assignment target, not just as a
    /// value. `iverilog` prints `0110` for the same writes.
    #[test]
    fn test_indexed_part_select_target() {
        let mut simulator = simulator_for(
            r#"
            module packer(input [1:0] hi, output [3:0] out);
                assign out[1 +: 2] = hi;
                assign out[3 -: 1] = 1'b0;
                assign out[0] = 1'b0;
            endmodule
        "#,
        );

        simulator
            .set_input("hi", Register::from_binary("11"))
            .unwrap();
        simulator.run().unwrap();

        assert_eq!(simulator.get("out").unwrap().to_binary(), "0110");
    }

    /// An index is a **position**, so a negative one names a real word.
    ///
    /// `reg [3:0] value [-7:7];` is an ordinary declaration and `value[-7]` is
    /// its first word. Reading the index unsigned makes `-7` into
    /// `18446744073709551609`, which names no word of anything — so the write
    /// is silently discarded and the read answers `x`. An index that really is
    /// out of range is still ignored, which is what `v[-1]` on a `reg [7:0]`
    /// asks for. iverilog 12.0 prints `-7 -1 7  v=00000000` for this design
    /// (corpus `negative_genvar`, `signed_net_display`).
    #[test]
    fn test_a_negative_index_names_a_word_rather_than_a_huge_one() {
        let mut simulator = simulator_for(
            r#"
            module m();
                reg signed [3:0] value [-7:7];
                reg [7:0] v;
                integer j;
                initial begin
                    for (j = -7; j <= 7; j = j + 1) value[j] = j;
                    $write("%0d %0d %0d", value[-7], value[-1], value[7]);
                    v = 8'b0000_0000;
                    v[-1] = 1'b1;
                    $display("  v=%b", v);
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "-7 -1 7  v=00000000\n");
    }

    /// A vector declared `[base+15:base]` for a negative `base` really does
    /// have negative bit indices, so an indexed part select whose base is a
    /// *signed* value has to read it as the negative number it is. Reading
    /// `-2` as `4294967294` selects nothing at all and answers `xxxx`, where
    /// iverilog 12.0 straddles the bottom of the vector and prints
    /// `011x x001` for this design (corpus `pr2835632b`).
    #[test]
    fn test_an_indexed_part_select_reads_a_signed_base_as_negative() {
        let mut simulator = simulator_for(
            r#"
            module m();
                parameter base = -1;
                reg [base+15:base] big = 16'h0123;
                reg [base:base+15] ltl = 16'h3210;
                integer a;
                initial begin
                    a = base - 1;
                    $displayb(big[a+:4], " ", ltl[a+:4]);
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "011x x001\n");
    }

    /// An undriven **net** reads `z` while an untouched **variable** reads
    /// `x`. The difference is not cosmetic: a variable with no assignment is
    /// A port's default value is applied where a body declaration's is, so a
    /// `reg` in the body naming the same port cannot overwrite it — and a
    /// value given to an output reaches the parent's net through the alias.
    #[test]
    fn test_a_ports_default_value_reaches_the_parent() {
        let modules = crate::parsers::source::parse_verilog_source(
            r#"
            module child (a, b, c);
              output a;
              reg a = 1'b0;
              output reg b = 1'b1;
              output integer c = 7;
            endmodule
            module top;
              wire out1, out2;
              wire [31:0] out3;
              child dut(out1, out2, out3);
              initial #1 $display("%b %b %0d", out1, out2, out3);
            endmodule
        "#,
        )
        .expect("design should parse")
        .1;
        let mut simulator = Simulator::with_modules(modules, "top");
        simulator.setup().expect("design should elaborate");
        simulator.advance(1).expect("advance should succeed");
        assert_eq!(simulator.output().lines(), vec!["0 1 7"]);
    }

    /// An `inout` bound to a **select** is bonded to it, not assigned from it:
    /// the port and the parent's bit become one node, so the connection
    /// carries a value in both directions.
    ///
    /// `t1` drives nothing and still reads the `1` that the parent's own
    /// `assign bus[1] = 1'b1;` puts on its bit, which is what a
    /// `Binding::Driving` assignment could not do — it only runs outwards.
    /// iverilog 12.0 prints `bus=10 q0=0 q1=1`.
    #[test]
    fn test_an_inout_bound_to_a_select_carries_both_ways() {
        let modules = crate::parsers::source::parse_verilog_source(
            r#"
            module tap(inout p, input drive, input value, output q);
              assign p = drive ? value : 1'bz;
              assign q = p;
            endmodule
            module top;
              wire [1:0] bus;
              wire q0, q1;
              reg d0, v0, d1, v1;
              tap t0(bus[0], d0, v0, q0);
              tap t1(bus[1], d1, v1, q1);
              assign bus[1] = 1'b1;
              initial begin
                d0 = 1'b1; v0 = 1'b0; d1 = 1'b0; v1 = 1'b0;
                #1 $display("bus=%b q0=%b q1=%b", bus, q0, q1);
              end
            endmodule
        "#,
        )
        .expect("design should parse")
        .1;
        let mut simulator = Simulator::with_modules(modules, "top");
        simulator.setup().expect("design should elaborate");
        simulator.advance(1).expect("advance should succeed");
        assert_eq!(simulator.output().lines(), vec!["bus=10 q0=0 q1=1"]);
    }

    /// unknown because nothing has said what it is, while a net with no driver
    /// is high-impedance because nothing is driving it.
    ///
    /// iverilog 12.0 prints `out=10z1 standalone=z avariable=x` for the
    /// equivalent design.
    #[test]
    fn test_undriven_nets_read_z_and_variables_read_x() {
        let mut simulator = simulator_for(
            r#"
            module m(input [1:0] hi, output [3:0] out, output reg q);
                wire standalone;
                reg  avariable;
                assign out[3:2] = hi;
                assign out[0] = 1'b1;
            endmodule
        "#,
        );

        simulator
            .set_input("hi", Register::from_binary("10"))
            .unwrap();
        simulator.run().unwrap();

        assert_eq!(simulator.get("out").unwrap().to_binary(), "10z1");
        assert_eq!(simulator.get("standalone").unwrap().to_binary(), "z");
        assert_eq!(simulator.get("avariable").unwrap().to_binary(), "x");
        // `output reg q` is a variable however it is spelled.
        assert_eq!(simulator.get("q").unwrap().to_binary(), "x");
    }

    /// `supply0`/`supply1` sit at their rail and `tri0`/`tri1` are pulled to a
    /// value that any real driver overrides.
    ///
    /// iverilog 12.0 prints `a=0 b=1 g=0 v=1` for the undriven cases, and for
    /// a driven `tri0`: `z` leaves it at `0`, `1` pulls it to `1`, `0` to `0`.
    #[test]
    fn test_supply_and_pulled_nets() {
        let mut simulator = simulator_for(
            r#"
            module m(input d);
                tri0    a;
                tri1    b;
                supply0 g;
                supply1 v;
                tri0    c;
                assign c = d;
            endmodule
        "#,
        );

        simulator
            .set_input("d", Register::from_binary("z"))
            .unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("a").unwrap().to_binary(), "0");
        assert_eq!(simulator.get("b").unwrap().to_binary(), "1");
        assert_eq!(simulator.get("g").unwrap().to_binary(), "0");
        assert_eq!(simulator.get("v").unwrap().to_binary(), "1");
        // Nothing but the pull reaches `c`, so it holds the pull value.
        assert_eq!(simulator.get("c").unwrap().to_binary(), "0");

        // A real driver is `strong`, which outranks `pull` either way.
        simulator.poke("d", one()).unwrap();
        assert_eq!(simulator.get("c").unwrap().to_binary(), "1");
        simulator.poke("d", zero()).unwrap();
        assert_eq!(simulator.get("c").unwrap().to_binary(), "0");
    }

    /// An array of nets is a memory in the store, exactly as a `reg` array is,
    /// and its undriven words read `z` because a net with no driver does.
    ///
    /// iverilog 12.0 prints `arr[1]=10 arr[2]=01 n[0]=zzz` for this design.
    #[test]
    fn test_net_arrays_simulate() {
        let mut simulator = simulator_for(
            r#"
            module m();
                wire [1:0] arr[2:1];
                wire signed [2:0] n [0:3];
                reg  [1:0] d0, d1;
                assign arr[1] = d0;
                assign arr[2] = d1;
                initial begin
                    d0 = 2'b10;
                    d1 = 2'b01;
                    #1 $display("arr[1]=%b arr[2]=%b n[0]=%b", arr[1], arr[2], n[0]);
                end
            endmodule
        "#,
        );

        simulator.advance(5).expect("time should advance");
        assert_eq!(
            simulator.output().text().trim(),
            "arr[1]=10 arr[2]=01 n[0]=zzz"
        );
    }

    /// An empty argument slot renders as exactly one space, which is how a
    /// design separates two values without a format string. Arguments are
    /// otherwise concatenated with no separator at all.
    ///
    /// iverilog 12.0 prints, for the same five calls:
    /// `x y`, ` 3  5`, `A  B`, two spaces, and `end:  3`.
    #[test]
    fn test_empty_task_arguments_are_one_space() {
        let mut simulator = simulator_for(
            r#"
            module m();
                reg [3:0] a, b;
                initial begin
                    a = 4'd3;
                    b = 4'd5;
                    $display("x",,"y");
                    $display(a,,b);
                    $display("A",,,"B");
                    $display(,);
                    $display("end:",, a);
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "x y\n 3  5\nA  B\n  \nend:  3\n");
    }

    /// An empty argument *list* is not one empty argument: `$finish()` takes
    /// none, and printing a space for it would be a lie about what was written.
    #[test]
    fn test_an_empty_argument_list_prints_nothing() {
        let mut simulator = simulator_for(
            r#"
            module m();
                initial begin
                    $display();
                    $display("after");
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "\nafter\n");
    }

    /// A string is legal wherever a value is: as a parameter's value, as a
    /// comparison operand, and inside a concatenation.
    ///
    /// iverilog 12.0 prints `s=464f4f s=FOO NAME=test` and then `eq works`.
    #[test]
    fn test_string_literals_simulate() {
        let mut simulator = simulator_for(
            r#"
            module m();
                parameter NAME = "test";
                reg [39:0] y;
                reg [23:0] s;
                initial begin
                    s = "FOO";
                    y = "hello";
                    $display("s=%h s=%s NAME=%s", s, s, NAME);
                    if (y === "hello") $display("eq works");
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "s=464f4f s=FOO NAME=test\neq works\n"
        );
    }

    /// An ANSI parameter port list declares ordinary parameters, so an
    /// override reaches them and a port width made of one resolves.
    ///
    /// iverilog 12.0 prints `a=1 b=2 c=-3 w=2` then `a=3 b=9 c=-3 w=4`.
    #[test]
    fn test_ansi_parameter_ports() {
        let modules = crate::parsers::source::parse_verilog_source(
            r#"
            module dut #(parameter a = 1, b = 2, parameter signed [7:0] c = -3)
                        (output [a:0] q);
                assign q = 0;
                initial $display("a=%0d b=%0d c=%0d w=%0d", a, b, c, $bits(q));
            endmodule
            module tb;
                wire [1:0] q1;
                wire [3:0] q2;
                dut d1 (q1);
                dut #(.a(3), .b(9)) d2 (q2);
            endmodule
        "#,
        )
        .expect("should parse")
        .1;

        let mut simulator = Simulator::with_modules(modules, "tb");
        simulator.setup().expect("should set up");
        simulator.advance(1).expect("time should advance");

        assert_eq!(
            simulator.output().text(),
            "a=1 b=2 c=-3 w=2\na=3 b=9 c=-3 w=4\n"
        );
    }

    /// A generate `for` or `if` may be written at module level with no
    /// `generate`/`endgenerate` around it, which iverilog accepts and the
    /// corpus uses freely.
    ///
    /// iverilog 12.0 prints `0 1 3 1` for this design.
    #[test]
    fn test_bare_generate_items() {
        let mut simulator = simulator_for(
            r#"
            module tb;
                wire [2:0] idx[7:0];
                genvar g;
                for (g = 0; g < 4; g=g+1)
                    assign idx[g] = g;
                if (1) begin : yes
                    wire w = 1'b1;
                end
                initial begin
                    #1 $display("%0d %0d %0d %b", idx[0], idx[1], idx[3], yes.w);
                end
            endmodule
        "#,
        );

        simulator.advance(2).expect("time should advance");
        assert_eq!(simulator.output().text(), "0 1 3 1\n");
    }

    /// In an ANSI header a port with no direction of its own inherits the one
    /// before it: `input clk, reset` declares two one-bit inputs.
    ///
    /// iverilog 12.0 prints `q=a5` for this design, which only works if
    /// `reset` is a real input rather than something the header dropped.
    #[test]
    fn test_ansi_port_directions_carry_forward() {
        let modules = crate::parsers::source::parse_verilog_source(
            r#"
            module ansireg(input clk, reset, input [7:0] d, output reg [7:0] q);
                always @(posedge clk) q <= reset ? 8'h00 : d;
            endmodule
            module tb;
                reg c, r;
                reg [7:0] d;
                wire [7:0] q;
                ansireg u(c, r, d, q);
                initial begin
                    c = 0; r = 0; d = 8'hA5;
                    #1 c = 1;
                    #1 $display("q=%h", q);
                end
            endmodule
        "#,
        )
        .expect("should parse")
        .1;

        let mut simulator = Simulator::with_modules(modules, "tb");
        simulator.setup().expect("should set up");
        simulator.advance(3).expect("time should advance");

        assert_eq!(simulator.output().text(), "q=a5\n");
    }

    /// `#` and its parameter block are separate tokens, and a named
    /// connection may be left blank: a blank *parameter* takes its default and
    /// a blank *port* is unconnected.
    ///
    /// iverilog 12.0 prints `a=4 b=2 c=5` then `a=1 b=2 c=3` — `b` falls back
    /// to its default in the first instance, and the second overrides nothing.
    #[test]
    fn test_blank_named_connections_and_spaced_parameter_block() {
        let modules = crate::parsers::source::parse_verilog_source(
            r#"
            module dut #(parameter a = 1, b = 2, c = 3) (input x, output y);
                assign y = x;
                initial $display("a=%0d b=%0d c=%0d", a, b, c);
            endmodule
            module tb;
                reg x;
                wire y1, y2;
                dut # (.a(4), .b(), .c(5)) u1 (x, y1);
                dut u2 (.x(x), .y());
                initial begin
                    x = 1;
                    #1 $display("y1=%b", y1);
                end
            endmodule
        "#,
        )
        .expect("should parse")
        .1;

        let mut simulator = Simulator::with_modules(modules, "tb");
        simulator.setup().expect("should set up");
        simulator.advance(2).expect("time should advance");

        assert_eq!(
            simulator.output().text(),
            "a=4 b=2 c=5\na=1 b=2 c=3\ny1=1\n"
        );
    }

    /// `a <= #5 b;` reads `b` **now** and writes `a` five ticks later, and it
    /// does **not** suspend the block — so a `#2` after it measures from the
    /// statement rather than from the write.
    ///
    /// iverilog 12.0 traces `t=0 a=1 b=10`, `t=3 a=1 b=99`, `t=6 a=10 b=99`:
    /// `b` changing to 99 at t=3 does not reach the write that lands at t=6.
    #[test]
    fn test_non_blocking_intra_assignment_delay() {
        let mut simulator = simulator_for(
            r#"
            module m();
                reg [7:0] a, b;
                initial begin
                    a = 8'd1;
                    b = 8'd10;
                    #1 a <= #5 b;
                    #2 b = 8'd99;
                    #20 $display("t=%0d a=%0d b=%0d", $time, a, b);
                end
            endmodule
        "#,
        );

        // The block reached `#2 b = 99` at t=3, so it did not suspend on the
        // scheduled write.
        simulator.advance(3).expect("time should advance");
        assert_eq!(simulator.get("b").unwrap().to_u128(), Some(99));
        // The write has not landed yet.
        assert_eq!(simulator.get("a").unwrap().to_u128(), Some(1));

        simulator.advance(3).expect("time should advance");
        // It lands at t=6, carrying the value `b` held at t=1.
        assert_eq!(simulator.get("a").unwrap().to_u128(), Some(10));

        simulator.advance(30).expect("time should advance");
        assert_eq!(simulator.output().text(), "t=23 a=10 b=99\n");
    }

    /// A write an `a <= #2 b;` scheduled is a *non-blocking* write, so it
    /// lands at the **end** of the timestamp it is due at — after every block
    /// that runs there — rather than at the top of it.
    ///
    /// iverilog 12.0 prints
    ///
    /// ```text
    /// at 2 (same timestamp as the landing): 00
    /// other block at 2: 00
    /// at 3: aa
    /// ```
    ///
    /// for the design below. Landing it at the top of the timestamp instead
    /// gives `aa` on all three lines, which is a block reading a value that
    /// has not been written yet (corpus `patch1268`).
    #[test]
    fn test_a_scheduled_write_lands_after_the_blocks_at_its_timestamp() {
        let mut simulator = simulator_for(
            r#"
            module m();
                reg [7:0] a;
                initial begin
                    a = 8'h00;
                    a <= #2 8'haa;
                    #2;
                    $display("at 2: %h", a);
                    #1;
                    $display("at 3: %h", a);
                end
                initial begin
                    #2;
                    $display("other block at 2: %h", a);
                end
            endmodule
        "#,
        );

        simulator.advance(10).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "at 2: 00\nother block at 2: 00\nat 3: aa\n"
        );
    }

    /// `{a, b, c} = v;` splits the value across the parts, most significant
    /// part first, each taking its own width.
    ///
    /// iverilog 12.0 prints `a=0101 b=10 c=1`, then `a=0111 b=00`, then
    /// `c=1 a=0011` for these three assignments.
    #[test]
    fn test_concatenation_assignment_target() {
        let mut simulator = simulator_for(
            r#"
            module m();
                reg [3:0] a;
                reg [1:0] b;
                reg c;
                reg [7:0] src;
                initial begin
                    src = 8'b1010_1101;
                    {a, b, c} = src[6:0];
                    $display("a=%b b=%b c=%b", a, b, c);
                    {a[1:0], b} = 4'b1100;
                    $display("a=%b b=%b", a, b);
                    {c, a} = 5'b10011;
                    $display("c=%b a=%b", c, a);
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "a=0101 b=10 c=1\na=0111 b=00\nc=1 a=0011\n"
        );
    }

    /// The continuous assignments settle **before** the first block runs, so
    /// an `initial` block reading a net reads what drives it rather than the
    /// `z` of a net nothing has reached yet.
    ///
    /// iverilog 12.0 prints `t0: y=0011 a=0` for this design — at time zero,
    /// before any delay.
    #[test]
    fn test_continuous_assignments_settle_before_time_zero() {
        let mut simulator = simulator_for(
            r#"
            module m();
                wire [3:0] y;
                wire a;
                assign y = 3;
                assign a = 1'b0;
                initial $display("t0: y=%b a=%b", y, a);
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "t0: y=0011 a=0\n");
    }

    /// A concatenation is a run of bits however its parts were declared, so a
    /// real written into one is **converted to an integer first and then
    /// split** — slicing the IEEE-754 encoding would put a piece of an exponent
    /// in each part. It is converted at the concatenation's own width, so
    /// `258.6` wraps in eight bits exactly as `259` would. iverilog 12.0 prints
    /// `a=0 b=3` for both.
    #[test]
    fn test_a_real_written_into_a_concatenation_is_converted_first() {
        let mut simulator = simulator_for(
            r#"
            module m();
                reg [3:0] a, b;
                real r;
                initial begin
                    r = 2.5;
                    {a, b} = r;
                    $display("a=%0d b=%0d", a, b);
                    {a, b} = 258.6;
                    $display("a=%0d b=%0d", a, b);
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "a=0 b=3\na=0 b=3\n");
    }

    /// A mid-block `#0` sees the continuous assignments caught up with what
    /// the block just wrote — that is the whole idiom: "let everything settle,
    /// then look".
    ///
    /// The non-blocking swap in the same design is the guard on the other
    /// side: updates must still land once, at the end of the timestep, so two
    /// blocks writing each other's values exchange them rather than both
    /// taking one. iverilog 12.0 prints `y=00000110` and `p=2 q=1`.
    #[test]
    fn test_zero_delay_settles_without_committing_non_blocking_updates() {
        let mut simulator = simulator_for(
            r#"
            module m();
                reg [7:0] a;
                wire [7:0] y;
                assign y = a + 1;
                reg [7:0] p, q;
                initial begin
                    a = 5;
                    #0;
                    $display("y=%b", y);
                end
                initial begin
                    p = 1;
                    q = 2;
                end
                initial begin
                    #1 p <= q;
                    q <= p;
                    #1 $display("p=%0d q=%0d", p, q);
                end
            endmodule
        "#,
        );

        simulator.advance(3).expect("time should advance");
        assert_eq!(simulator.output().text(), "y=00000110\np=2 q=1\n");
    }

    /// An output port bound to a **concatenation** carries the child's value
    /// out across the parts, most significant first. It became expressible
    /// once a concatenation was a writable target.
    ///
    /// iverilog 12.0 prints `e=0 f=1 g=1 h=0` for `iB = 4'b0101` and
    /// `assign oB = iB + 1`.
    #[test]
    fn test_output_bound_to_a_concatenation() {
        let modules = crate::parsers::source::parse_verilog_source(
            r#"
            module child(input clk, input [3:0] iB, output [3:0] oB);
                assign oB = iB + 1;
            endmodule
            module tb;
                reg clk;
                reg [3:0] i;
                wire e, f, g, h;
                child M1(clk, i, {e, f, g, h});
                initial begin
                    i = 4'b0101;
                    #1 $display("e=%b f=%b g=%b h=%b", e, f, g, h);
                end
            endmodule
        "#,
        )
        .expect("should parse")
        .1;

        let mut simulator = Simulator::with_modules(modules, "tb");
        simulator.setup().expect("should set up");
        simulator.advance(2).expect("time should advance");

        assert_eq!(simulator.output().text(), "e=0 f=1 g=1 h=0\n");
    }

    /// A hierarchical name is **writable**, not just readable: a testbench
    /// reaching into a design writes `d.pass = 1'b1;`, and a bit of one.
    ///
    /// Elaboration has already flattened the store to exactly those dotted
    /// names, so this is a front-end gap rather than a model one. iverilog
    /// 12.0 prints `pass=1 v=0100`.
    #[test]
    fn test_hierarchical_assignment_targets() {
        let modules = crate::parsers::source::parse_verilog_source(
            r#"
            module dut;
                reg pass;
                reg [3:0] v;
                initial begin pass = 1'b0; v = 0; end
            endmodule
            module top;
                dut d();
                initial begin
                    #1 d.pass = 1'b1;
                    d.v[2] = 1'b1;
                    #1 $display("pass=%b v=%b", d.pass, d.v);
                end
            endmodule
        "#,
        )
        .expect("should parse")
        .1;

        let mut simulator = Simulator::with_modules(modules, "top");
        simulator.setup().expect("should set up");
        simulator.advance(3).expect("time should advance");

        assert_eq!(simulator.output().text(), "pass=1 v=0100\n");
    }

    /// A net taking its first driven value at time zero is an **event**, so an
    /// edge-triggered block wakes on it. Here the only thing that ever moves is
    /// a primitive's output settling to its `initial` 0 — `e` and `d` are never
    /// driven — and `always @(q)` has to see that.
    ///
    /// This is corpus `br_ml20190806a`, which iverilog 12.0 passes: `r` is 0 at
    /// `#1`, where it stayed `x` while the first change was dropped unseen.
    #[test]
    fn test_a_nets_first_driven_value_is_an_event() {
        let modules = crate::parsers::source::parse_verilog_source(
            r#"
            primitive latch(q, e, d);
              output q; input e; input d; reg q;
              initial q = 1'b0;
              table
                1 1 : ? : 1 ;
                1 0 : ? : 0 ;
                0 ? : ? : - ;
              endtable
            endprimitive
            module test();
              wire q;
              reg e, d, r;
              latch latch(q, e, d);
              always @(q) r = q;
              initial begin #1; $display("q=%b r=%b", q, r); end
            endmodule
        "#,
        )
        .expect("should parse")
        .1;

        let mut simulator = Simulator::with_modules(modules, "test");
        simulator.setup().expect("should set up");
        simulator.advance(3).expect("time should advance");

        assert_eq!(simulator.output().text(), "q=0 r=0\n");
    }

    /// The other half of the rule: a net whose driver starts at `x` gets that
    /// `x` silently, before simulation starts, so `always @(q)` does **not**
    /// wake. Same latch as above with no `initial`; iverilog 12.0 prints
    /// `x x` (corpus `br_ml20190806b`).
    #[test]
    fn test_a_nets_first_value_of_x_is_not_an_event() {
        let modules = crate::parsers::source::parse_verilog_source(
            r#"
            primitive latch(q, e, d);
              output q; input e; input d; reg q;
              table
                1 1 : ? : 1 ;
                1 0 : ? : 0 ;
                0 ? : ? : - ;
              endtable
            endprimitive
            module test();
              wire q;
              reg e, d, r;
              latch latch(q, e, d);
              always @(q) r = 1;
              initial begin #1; $display("%b %b", q, r); end
            endmodule
        "#,
        )
        .expect("should parse")
        .1;

        let mut simulator = Simulator::with_modules(modules, "test");
        simulator.setup().expect("should set up");
        simulator.advance(3).expect("time should advance");

        assert_eq!(simulator.output().text(), "x x\n");
    }

    /// An array of instances: a connection as wide as the port reaches every
    /// element, and one `count` times as wide is sliced — the array's *right*
    /// index taking the least significant slice. Elements are named `u[0]`,
    /// `p[1]` and reached that way.
    ///
    /// iverilog 12.0 prints `o=0101 y=1101` and `u[0].o=1 p[1].y=11 p[0].a=01`.
    #[test]
    fn test_instance_arrays_slice_their_connections() {
        let modules = crate::parsers::source::parse_verilog_source(
            r#"
            module inv(output o, input i);
              assign o = ~i;
            endmodule
            module pair(output [1:0] y, input [1:0] a, input en);
              assign y = en ? a : 2'b00;
            endmodule
            module tb;
              reg [3:0] i;
              wire [3:0] o;
              reg [3:0] a;
              reg en;
              wire [3:0] y;
              inv u[3:0] (o, i);
              pair p[1:0] (.y(y), .a(a), .en(en));
              initial begin
                i = 4'b1010; a = 4'b1101; en = 1;
                #1 $display("o=%b y=%b", o, y);
                #1 $display("u[0].o=%b p[1].y=%b p[0].a=%b", u[0].o, p[1].y, p[0].a);
              end
            endmodule
        "#,
        )
        .expect("should parse")
        .1;

        let mut simulator = Simulator::with_modules(modules, "tb");
        simulator.setup().expect("should set up");
        simulator.advance(3).expect("time should advance");

        assert_eq!(
            simulator.output().text(),
            "o=0101 y=1101\nu[0].o=1 p[1].y=11 p[0].a=01\n"
        );
    }

    /// A write through an index that is not a known number is **ignored** —
    /// a bit, a memory word and an indexed part select alike. That is what the
    /// LRM specifies, the same as for a write out of range, and iverilog 12.0
    /// prints `v=00 mem0=11 mem1=22 w=0000` for this design.
    #[test]
    fn test_a_write_through_an_unknown_index_is_ignored() {
        let mut simulator = simulator_for(
            r#"
            module m();
                reg [7:0] v;
                reg [7:0] mem [0:3];
                reg [15:0] w;
                reg [2:0] ix;
                reg [3:0] base;
                initial begin
                    v = 8'h00; mem[0] = 8'h11; mem[1] = 8'h22; w = 16'h0000;
                    ix = 3'bx; base = 4'bx;
                    v[ix] = 1'b1;
                    mem[ix] = 8'hff;
                    w[base +: 4] = 4'hf;
                    $display("v=%h mem0=%h mem1=%h w=%h", v, mem[0], mem[1], w);
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "v=00 mem0=11 mem1=22 w=0000\n");
    }

    #[test]
    fn test_parameters_are_visible_to_assignments() {
        let mut simulator = simulator_for(
            r#"
            module offset(
                input [7:0] a,
                output [7:0] b
            );
                localparam BIAS = 8'd10;
                assign b = a + BIAS;
            endmodule
        "#,
        );

        simulator.set_input("a", Register::from_u128(5, 8)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("b").unwrap().to_u128(), Some(15));
    }

    #[test]
    fn test_edge_triggered_flip_flop_captures_on_rising_edge_only() {
        let mut simulator = simulator_for(
            r#"
            module dff(
                input clk,
                input d,
                output reg q
            );
                always @(posedge clk) q <= d;
            endmodule
        "#,
        );

        simulator.poke("d", one()).unwrap();
        // No clock edge yet, so the flop has not captured anything.
        assert_eq!(simulator.get("q").unwrap().to_binary(), "x");

        simulator.poke("clk", one()).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_binary(), "1");

        // A falling edge must not capture: drop `d` and clock low again.
        simulator.poke("d", zero()).unwrap();
        simulator.poke("clk", zero()).unwrap();
        assert_eq!(
            simulator.get("q").unwrap().to_binary(),
            "1",
            "a negedge must not capture in a posedge-triggered flop"
        );

        simulator.poke("clk", one()).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_binary(), "0");
    }

    /// A variable bit-select target — `q[addr] <= 1'b1;` — has to survive
    /// parsing as an expression index and then resolve against the runtime
    /// value of `addr`.
    #[test]
    fn test_variable_bit_select_target_writes_the_addressed_bit() {
        let mut simulator = simulator_for(
            r#"
            module bit_writer(
                input clk,
                input rst,
                input [1:0] addr,
                output reg [3:0] q
            );
                always @(posedge clk) begin
                    if (rst)
                        q <= 4'b0000;
                    else
                        q[addr] <= 1'b1;
                end
            endmodule
        "#,
        );

        simulator.set_input("rst", one()).unwrap();
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("q").unwrap().to_binary(), "0000");

        simulator.set_input("rst", zero()).unwrap();
        simulator
            .set_input("addr", Register::from_u128(2, 2))
            .unwrap();
        simulator.tick("clk").unwrap();
        assert_eq!(
            simulator.get("q").unwrap().to_binary(),
            "0100",
            "addr=2 must set bit 2, not a literal index"
        );

        simulator
            .set_input("addr", Register::from_u128(0, 2))
            .unwrap();
        simulator.tick("clk").unwrap();
        assert_eq!(
            simulator.get("q").unwrap().to_binary(),
            "0101",
            "the index follows addr from cycle to cycle"
        );
    }

    #[test]
    fn test_counter_example_counts_and_resets() {
        let mut simulator = simulator_for_example("counter.v");

        // Asynchronous reset: a posedge on `rst` clears the count with no clock.
        simulator.poke("rst", one()).unwrap();
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(0));

        simulator.poke("rst", zero()).unwrap();
        for expected in 1..=5u128 {
            simulator.tick("clk").unwrap();
            assert_eq!(
                simulator.get("count").unwrap().to_u128(),
                Some(expected),
                "count after {} ticks",
                expected
            );
        }

        // 4 bits, so it wraps rather than reaching 16.
        for _ in 6..=16 {
            simulator.tick("clk").unwrap();
        }
        assert_eq!(
            simulator.get("count").unwrap().to_u128(),
            Some(0),
            "a 4-bit counter wraps at 16"
        );

        // Reset again mid-count.
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(1));
        simulator.poke("rst", one()).unwrap();
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(0));
    }

    #[test]
    fn test_complex_module_example_pipelines_and_drives_tristate() {
        // sum <= temp; temp <= a + b; inside one posedge block, so a value
        // takes two clocks to reach `sum`. `data` is a continuous assign that
        // has to re-propagate after each clock.
        let mut simulator = simulator_for_example("complex_module.v");

        simulator.poke("rst", one()).unwrap();
        assert_eq!(simulator.get("sum").unwrap().to_u128(), Some(0));
        simulator.poke("rst", zero()).unwrap();

        simulator.poke("a", Register::from_u128(5, 4)).unwrap();
        simulator.poke("b", Register::from_u128(6, 4)).unwrap();

        // First clock loads temp; sum still holds the old temp.
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("temp").unwrap().to_u128(), Some(11));
        assert_eq!(simulator.get("sum").unwrap().to_u128(), Some(0));

        // Second clock walks it through to sum, and the assign follows.
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("sum").unwrap().to_u128(), Some(11));
        assert_eq!(
            simulator.get("data").unwrap().to_u128(),
            Some(11),
            "sum > 4'b1000, so data should be driven with sum"
        );
    }

    /// A strength-bearing `assign` is resolved even when it is the net's only
    /// driver and the design has no gate in it at all.
    ///
    /// This is the seam `push_assignment` opens: `highz` is a half that does
    /// not drive, so `(strong1, highz0)` floats every `0` bit, and only
    /// `resolve_bit` knows that. Writing the net directly — which is what a
    /// plain `assign` still does — would put the `0`s straight through.
    /// `iverilog 12.0` prints `1z1z` and `z0z0` for these two.
    #[test]
    fn test_assign_strength_is_resolved_without_any_gate() {
        let mut simulator = simulator_for(
            r#"
            module strengths(input [3:0] v, output [3:0] a, output [3:0] b, output [3:0] c);
                assign (strong1, highz0) a = v;
                assign (highz1, strong0) b = v;
                assign (supply1, supply0) c = v;
            endmodule
        "#,
        );

        simulator.poke("v", Register::from_binary("1010")).unwrap();
        assert_eq!(simulator.get("a").unwrap().to_binary(), "1z1z");
        assert_eq!(simulator.get("b").unwrap().to_binary(), "z0z0");
        assert_eq!(
            simulator.get("c").unwrap().to_binary(),
            "1010",
            "every level but highz drives the plain logic value"
        );

        // Neither polarity claims an `x` or a `z`, so neither moves.
        simulator.poke("v", Register::from_binary("01xz")).unwrap();
        assert_eq!(simulator.get("a").unwrap().to_binary(), "z1xz");
        assert_eq!(simulator.get("b").unwrap().to_binary(), "0zxz");
    }

    /// A `0` the target's width introduced floats exactly as one the
    /// expression produced does, because resolution runs per bit over the
    /// whole net rather than over the right hand side.
    #[test]
    fn test_assign_strength_floats_the_padding_too() {
        let mut simulator = simulator_for(
            r#"
            module padded(input clk, output [3:0] a);
                assign (strong1, highz0) a = 1'b1;
            endmodule
        "#,
        );

        simulator.poke("clk", zero()).unwrap();
        assert_eq!(simulator.get("a").unwrap().to_binary(), "zzz1");
    }

    /// Two continuous assignments driving one net resolve by strength rather
    /// than by whichever ran last: `pull` loses to `strong`. Contention is not
    /// an error — `iverilog` prints `1010` here.
    #[test]
    fn test_contending_assign_strengths_resolve_by_level() {
        let mut simulator = simulator_for(
            r#"
            module contend(input clk, output [3:0] c);
                assign (pull1, pull0)     c = 4'b1111;
                assign (strong1, strong0) c = 4'b1010;
            endmodule
        "#,
        );

        simulator.poke("clk", zero()).unwrap();
        assert_eq!(simulator.get("c").unwrap().to_binary(), "1010");
    }

    /// Drivers tied at the strongest level agree on a bit or that bit is `x`,
    /// and the bits they agree on are untouched — `iverilog` prints `1xx0`.
    #[test]
    fn test_equal_strength_drivers_disagree_to_x_per_bit() {
        let mut simulator = simulator_for(
            r#"
            module tie(input clk, output [3:0] d);
                assign (strong1, strong0) d = 4'b1100;
                assign (strong1, strong0) d = 4'b1010;
            endmodule
        "#,
        );

        simulator.poke("clk", zero()).unwrap();
        assert_eq!(simulator.get("d").unwrap().to_binary(), "1xx0");
    }

    /// A word of an **array of nets** is a net in its own right, so two
    /// strength-bearing drivers of `foo[0]` resolve against each other and say
    /// nothing about `foo[1]`. Corpus `pr1703346` is exactly this and iverilog
    /// 12.0 prints `foo[0] = 01, foo[1] = 10`.
    #[test]
    fn test_an_array_of_nets_resolves_a_word_at_a_time() {
        let mut simulator = simulator_for(
            r#"
            module main;
                wire [1:0] foo [0:1];

                assign (highz0, strong1) foo[0] = 2'b01;
                assign (strong0, highz1) foo[0] = 2'b01;

                assign (highz0, strong1) foo[1] = 2'b10;
                assign (strong0, highz1) foo[1] = 2'b10;

                initial #1 $display("foo[0] = %b, foo[1] = %b", foo[0], foo[1]);
            endmodule
        "#,
        );

        simulator.advance(2).unwrap();
        assert_eq!(simulator.output().text(), "foo[0] = 01, foo[1] = 10\n");
    }

    /// `$countdrivers` counts the continuous drivers that reached one **bit**
    /// of a net, and a driver contributing `z` is not one of them.
    ///
    /// Measured against iverilog 12.0, which is corpus `countdrivers2`:
    /// `wire [1:0] n = 2'bzx;` answers `0` for bit 1 — the `assign` is there
    /// but it is driving nothing — while `wire [1:0] n = 2'b0x;` answers one
    /// driver, driving `0`.
    #[test]
    fn test_countdrivers_ignores_a_driver_that_is_floating() {
        let mut simulator = simulator_for(
            r#"
            module top;
                wire [1:0] floating = 2'bzx;
                wire [1:0] held = 2'b0x;
                reg [15:0] multi, forced, all, zero, one, unknown;
                initial begin
                    #1;
                    multi = $countdrivers(floating[1], forced, all, zero, one, unknown);
                    $display("%0d %0d %0d %0d %0d %0d",
                             multi, forced, all, zero, one, unknown);
                    multi = $countdrivers(held[1], forced, all, zero, one, unknown);
                    $display("%0d %0d %0d %0d %0d %0d",
                             multi, forced, all, zero, one, unknown);
                end
            endmodule
        "#,
        );
        simulator.advance(2).unwrap();
        assert_eq!(
            simulator.output().text(),
            "0 0 0 0 0 0
0 0 1 1 0 0
"
        );
    }

    /// More than one driver makes the answer `1`, and each is counted by what
    /// it is driving.
    ///
    /// iverilog 12.0 over corpus `countdrivers2`'s five-driver net answers
    /// `multi = 1 countD = 6 count0 = 2 count1 = 3 countX = 1` for bit 1 of a
    /// `wire [1:0]` declared `2'bxx` and assigned `0x`, `0x`, `1x`, `1x`, `1x`.
    #[test]
    fn test_countdrivers_counts_each_driver_by_what_it_drives() {
        let mut simulator = simulator_for(
            r#"
            module top;
                wire [1:0] bus = 2'bxx;
                assign bus = 2'b0x;
                assign bus = 2'b0x;
                assign bus = 2'b1x;
                assign bus = 2'b1x;
                assign bus = 2'b1x;
                reg [15:0] multi, forced, all, zero, one, unknown;
                initial begin
                    #1;
                    multi = $countdrivers(bus[1], forced, all, zero, one, unknown);
                    $display("%0d %0d %0d %0d %0d %0d",
                             multi, forced, all, zero, one, unknown);
                end
            endmodule
        "#,
        );
        simulator.advance(2).unwrap();
        assert_eq!(
            simulator.output().text(),
            "1 0 6 2 3 1
"
        );
    }

    /// A `force` is reported *as* a force and is not counted as a driver.
    ///
    /// Measured against iverilog 12.0: `wire [1:0] n = 2'b0x;` then
    /// `force n = 2'bxx;` answers `forced = 1` with the driver tally it had
    /// before the force — one driver, driving `0`. Counting the force as a
    /// driver as well gives `countD = 2`, and clearing the tally when the net
    /// is written gives `countD = 0`; both were real, and the second is why
    /// `StateStore::set` keeps the tally across a write.
    #[test]
    fn test_a_force_is_reported_but_not_counted() {
        let mut simulator = simulator_for(
            r#"
            module top;
                wire [1:0] n = 2'b0x;
                reg [15:0] multi, forced, all, zero, one, unknown;
                initial begin
                    #1;
                    multi = $countdrivers(n[1], forced, all, zero, one, unknown);
                    $display("%0d %0d %0d %0d %0d %0d",
                             multi, forced, all, zero, one, unknown);
                    force n = 2'bxx;
                    multi = $countdrivers(n[1], forced, all, zero, one, unknown);
                    $display("%0d %0d %0d %0d %0d %0d",
                             multi, forced, all, zero, one, unknown);
                end
            endmodule
        "#,
        );
        simulator.advance(2).unwrap();
        assert_eq!(
            simulator.output().text(),
            "0 0 1 1 0 0
0 1 1 1 0 0
"
        );
    }

    /// The same shape with drivers that *disagree*, which is what tells real
    /// per-bit resolution from writing the last value: iverilog 12.0 prints
    /// `foo[0] = 0x, foo[1] = 1z`. Pooling the two words' drivers into one
    /// list, or writing each word as it came, both get this wrong.
    #[test]
    fn test_array_of_net_words_resolve_bit_by_bit() {
        let mut simulator = simulator_for(
            r#"
            module main;
                wire [1:0] foo [0:1];

                assign (highz0, strong1) foo[0] = 2'b01;
                assign (strong0, highz1) foo[0] = 2'b00;

                assign (highz0, strong1) foo[1] = 2'b10;
                assign (strong0, highz1) foo[1] = 2'b11;

                initial #1 $display("foo[0] = %b, foo[1] = %b", foo[0], foo[1]);
            endmodule
        "#,
        );

        simulator.advance(2).unwrap();
        assert_eq!(simulator.output().text(), "foo[0] = 0x, foo[1] = 1z\n");
    }

    /// One `assign` may name several targets, and they share its strength.
    #[test]
    fn test_continuous_assignment_list_drives_every_target() {
        let mut simulator = simulator_for(
            r#"
            module several(input clk, output [3:0] a, output [3:0] b, output [3:0] c);
                assign a = 4'd5, b = 4'd8;
                assign (strong1, highz0) c = 4'b1010;
            endmodule
        "#,
        );

        simulator.poke("clk", zero()).unwrap();
        assert_eq!(simulator.get("a").unwrap().to_u128(), Some(5));
        assert_eq!(simulator.get("b").unwrap().to_u128(), Some(8));
        assert_eq!(simulator.get("c").unwrap().to_binary(), "1z1z");
    }

    /// Two continuous assignments on one net are *resolved* against each
    /// other, not written one after the other.
    ///
    /// Writing them in turn does not settle at all — each pass undoes the one
    /// before it — so a design that does this used to fail as `NoConvergence`
    /// (corpus `pr1701921`). iverilog 12.0 prints `blend=x agree=1` for this
    /// design: drivers that disagree give `x` and drivers that agree pass
    /// their value through.
    #[test]
    fn test_two_assignments_on_one_net_resolve_against_each_other() {
        let mut simulator = simulator_for(
            r#"
            module top();
                reg foo, bar;
                wire blend, agree;
                assign blend = foo;
                assign blend = bar;
                assign agree = bar;
                assign agree = bar;
                initial bar = 1'b1;
            endmodule
        "#,
        );
        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.get("blend").unwrap().to_binary(), "x");
        assert_eq!(simulator.get("agree").unwrap().to_binary(), "1");
    }

    #[test]
    fn test_non_blocking_updates_are_visible_across_blocks() {
        // Two flops in series. With `<=` the second captures the *old* `a`, so
        // a value takes two clocks to walk the pipeline. Were these blocking,
        // `b` would take the new `a` and both would move in one clock.
        let mut simulator = simulator_for(
            r#"
            module pipeline(
                input clk,
                input d,
                output reg a,
                output reg b
            );
                always @(posedge clk) a <= d;
                always @(posedge clk) b <= a;
            endmodule
        "#,
        );

        simulator.poke("d", one()).unwrap();
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("a").unwrap().to_binary(), "1");
        assert_eq!(
            simulator.get("b").unwrap().to_binary(),
            "x",
            "b must capture the pre-clock a, not the new one"
        );

        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("b").unwrap().to_binary(), "1");
    }

    #[test]
    fn test_errors_on_bad_signal_names() {
        let mut simulator = simulator_for(
            r#"
            module buffer(
                input a,
                output b
            );
                assign b = a;
            endmodule
        "#,
        );

        assert_eq!(
            simulator.set_input("b", Register::from_binary("1")),
            Err(SimulationError::NotAnInput("b".to_string()))
        );
        assert_eq!(
            simulator.get("nope"),
            Err(SimulationError::UnknownSignal("nope".to_string()))
        );
    }

    #[test]
    fn test_initial_block_stimulus_lands_at_its_scheduled_times() {
        let mut simulator = simulator_for(
            r#"
            module stimulus(
                output reg a,
                output reg b
            );
                initial begin
                    a = 1'b0;
                    #10 a = 1'b1;
                    #10 b = 1'b1;
                end
            endmodule
        "#,
        );

        // The statements before the first delay have already run at time zero.
        assert_eq!(simulator.now(), 0);
        assert_eq!(simulator.get("a").unwrap().to_binary(), "0");
        assert_eq!(simulator.get("b").unwrap().to_binary(), "x");

        // Stopping short of the delay must not run the next statement.
        simulator.advance(9).unwrap();
        assert_eq!(simulator.now(), 9);
        assert_eq!(simulator.get("a").unwrap().to_binary(), "0");

        simulator.advance(1).unwrap();
        assert_eq!(simulator.now(), 10);
        assert_eq!(simulator.get("a").unwrap().to_binary(), "1");
        assert_eq!(simulator.get("b").unwrap().to_binary(), "x");

        simulator.advance(10).unwrap();
        assert_eq!(simulator.get("b").unwrap().to_binary(), "1");
    }

    #[test]
    fn test_free_running_always_block_generates_a_clock() {
        let mut simulator = simulator_for(
            r#"
            module oscillator(
                output reg clk
            );
                initial clk = 1'b0;
                always begin
                    #50 clk = ~clk;
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.get("clk").unwrap().to_binary(), "0");

        simulator.advance(50).unwrap();
        assert_eq!(simulator.get("clk").unwrap().to_binary(), "1");

        simulator.advance(50).unwrap();
        assert_eq!(simulator.get("clk").unwrap().to_binary(), "0");

        // Several periods in one call.
        simulator.advance(150).unwrap();
        assert_eq!(simulator.now(), 250);
        assert_eq!(simulator.get("clk").unwrap().to_binary(), "1");
    }

    #[test]
    fn test_self_clocking_counter_runs_on_time_alone() {
        // No pokes, no ticks — the design drives itself. The free-running block
        // makes the clock, and the edge-triggered block counts its posedges.
        let mut simulator = simulator_for(
            r#"
            module self_clocked(
                output reg [3:0] count,
                output reg clk
            );
                initial begin
                    clk = 1'b0;
                    count = 4'b0000;
                end
                always begin
                    #10 clk = ~clk;
                end
                always @(posedge clk) count <= count + 1;
            endmodule
        "#,
        );

        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(0));

        // clk rises at 10, 30, 50, 70, 90 — five posedges by time 100.
        simulator.advance(100).unwrap();
        assert_eq!(simulator.now(), 100);
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(5));

        simulator.advance(100).unwrap();
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(10));
    }

    /// Every name in a comma-separated declaration has to reach the store, not
    /// just the first — otherwise driving the second one writes nowhere.
    #[test]
    fn test_comma_declared_registers_all_simulate() {
        let simulator = simulator_for(
            r#"
            module lists(output wire [3:0] sum);
                reg [3:0] a, b;
                integer i, j;

                initial begin
                    a = 4'd3;
                    b = 4'd4;
                    i = 7;
                    j = 9;
                end

                assign sum = a + b;
            endmodule
        "#,
        );

        assert_eq!(simulator.get("a").unwrap().to_u128(), Some(3));
        assert_eq!(simulator.get("b").unwrap().to_u128(), Some(4));
        assert_eq!(simulator.get("sum").unwrap().to_u128(), Some(7));

        // An `integer` is 32 bits wide, and both names exist.
        assert_eq!(simulator.get("i").unwrap().width(), 32);
        assert_eq!(simulator.get("i").unwrap().to_u128(), Some(7));
        assert_eq!(simulator.get("j").unwrap().to_u128(), Some(9));
    }

    #[test]
    fn test_delay_nested_in_a_conditional_schedules_correctly() {
        // The resume point is inside the `if` body, so this only works because
        // blocks compile to a flat program with a program counter.
        let mut simulator = simulator_for(
            r#"
            module gated(
                output reg a,
                output reg done
            );
                initial begin
                    a = 1'b1;
                    if (a) begin
                        #25 done = 1'b1;
                    end
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.get("done").unwrap().to_binary(), "x");
        simulator.advance(24).unwrap();
        assert_eq!(simulator.get("done").unwrap().to_binary(), "x");
        simulator.advance(1).unwrap();
        assert_eq!(simulator.get("done").unwrap().to_binary(), "1");
    }

    #[test]
    fn test_free_running_block_without_a_delay_is_reported_not_hung() {
        // `always begin a = ~a; end` has no delay, so it restarts forever
        // without time advancing. That has to be an error, not a hang.
        let (_, module) = parse_module_declaration(
            r#"
            module spinner(
                output reg a
            );
                initial a = 1'b0;
                always begin
                    a = ~a;
                end
            endmodule
        "#,
        )
        .unwrap();

        let mut simulator = Simulator::new(module);
        assert!(matches!(
            simulator.setup(),
            Err(SimulationError::NoConvergence { .. })
        ));
    }

    #[test]
    fn test_clock_divider_example_resets_and_counts() {
        let mut simulator = simulator_for_example("clock_divider.v");

        // Asynchronous reset clears both the counter and the divided output.
        simulator.poke("rst", one()).unwrap();
        assert_eq!(simulator.get("counter").unwrap().to_u128(), Some(0));
        assert_eq!(simulator.get("divided_clk").unwrap().to_binary(), "0");

        simulator.poke("rst", zero()).unwrap();
        for expected in 1..=5u128 {
            simulator.tick("clk").unwrap();
            assert_eq!(simulator.get("counter").unwrap().to_u128(), Some(expected));
        }

        // The divide threshold is 50_000_000, so the output must not have moved
        // yet. Reaching it by simulation is not practical; the threshold logic
        // itself is covered at a testable scale by the test below.
        assert_eq!(simulator.get("divided_clk").unwrap().to_binary(), "0");

        simulator.poke("rst", one()).unwrap();
        assert_eq!(simulator.get("counter").unwrap().to_u128(), Some(0));
    }

    #[test]
    fn test_divider_pattern_toggles_at_its_threshold() {
        // Same shape as `clock_divider.v` — `counter <= counter + 1;` followed
        // by a nested `if` that also assigns `counter` — but with a threshold a
        // test can actually reach. Both writes are non-blocking, so the later
        // one wins and the counter wraps rather than reaching 4.
        let mut simulator = simulator_for(
            r#"
            module small_divider(
                input clk,
                input rst,
                output reg out
            );
                reg [3:0] counter;
                always @(posedge clk or posedge rst) begin
                    if (rst) begin
                        counter <= 4'b0;
                        out <= 1'b0;
                    end else begin
                        counter <= counter + 1;
                        if (counter == 4'd3) begin
                            counter <= 4'b0;
                            out <= ~out;
                        end
                    end
                end
            endmodule
        "#,
        );

        simulator.poke("rst", one()).unwrap();
        simulator.poke("rst", zero()).unwrap();

        let mut seen = Vec::new();
        for _ in 0..8 {
            simulator.tick("clk").unwrap();
            seen.push((
                simulator.get("counter").unwrap().to_u128().unwrap(),
                simulator.get("out").unwrap().to_binary(),
            ));
        }

        let expected: Vec<(u128, String)> = [1, 2, 3, 0, 1, 2, 3, 0]
            .iter()
            .zip(["0", "0", "0", "1", "1", "1", "1", "0"])
            .map(|(count, out)| (*count as u128, out.to_string()))
            .collect();
        assert_eq!(seen, expected, "counter should wrap at 3 and toggle `out`");
    }

    #[test]
    fn test_spi_controller_example_resets_and_drives_its_outputs() {
        let mut simulator = simulator_for_example("spi_controller.v");

        simulator.poke("rst", one()).unwrap();
        assert_eq!(simulator.get("state").unwrap().to_binary(), "00", "IDLE");
        assert_eq!(simulator.get("data").unwrap().to_binary(), "00000000");
        // assign miso = data[7];
        assert_eq!(simulator.get("miso").unwrap().to_binary(), "0");
        // assign cs = (state == IDLE) ? 1 : 0;
        assert_eq!(simulator.get("cs").unwrap().to_binary(), "1");

        // assign sclk = clk; — a continuous assign straight through.
        simulator.poke("clk", one()).unwrap();
        assert_eq!(simulator.get("sclk").unwrap().to_binary(), "1");
        simulator.poke("clk", zero()).unwrap();
        assert_eq!(simulator.get("sclk").unwrap().to_binary(), "0");

        // The module cannot leave IDLE, and that is faithful rather than a
        // simulator bug: `cs` is an *output* driven from `state`, so IDLE forces
        // `cs` to 1, while the IDLE arm only advances when `cs == 0`. The
        // example has no way to drive `cs` externally.
        simulator.poke("rst", zero()).unwrap();
        for _ in 0..5 {
            simulator.tick("clk").unwrap();
        }
        assert_eq!(
            simulator.get("state").unwrap().to_binary(),
            "00",
            "cs is driven from state, so IDLE is self-latching in this module"
        );
    }

    #[test]
    fn test_every_example_module_simulates() {
        let dir: PathBuf = [env!("CARGO_MANIFEST_DIR"), "src", "verilog", "examples"]
            .iter()
            .collect();
        let mut paths: Vec<PathBuf> = fs::read_dir(dir)
            .expect("unable to read examples")
            .map(|entry| entry.expect("unable to read entry").path())
            .filter(|path| path.is_file())
            .collect();
        paths.sort();
        assert_eq!(paths.len(), 6, "expected six example modules");

        for path in paths {
            let name = path.file_name().unwrap().to_string_lossy().to_string();
            let source = fs::read_to_string(&path).expect("unable to read example");
            let (remaining, module) = parse_module_declaration(&source)
                .unwrap_or_else(|error| panic!("{} should parse: {:?}", name, error));
            assert!(remaining.trim().is_empty(), "{} left {:?}", name, remaining);

            let mut simulator = Simulator::new(module);
            simulator
                .setup()
                .unwrap_or_else(|error| panic!("{} should set up: {}", name, error));

            // Drive whatever stimulus each module happens to have. A module
            // without a given port is fine; anything else is a real failure.
            for (port, value) in [("rst", one()), ("rst", zero()), ("clk", one())] {
                match simulator.poke(port, value) {
                    Ok(_) | Err(SimulationError::NotAnInput(_)) => {}
                    Err(error) => panic!("{} failed driving {}: {}", name, port, error),
                }
            }
            simulator
                .advance(10)
                .unwrap_or_else(|error| panic!("{} should advance time: {}", name, error));
        }
    }

    #[test]
    fn test_methods_require_setup() {
        let (_, module) = parse_module_declaration("module empty(); endmodule").unwrap();
        let mut simulator = Simulator::new(module);

        assert_eq!(simulator.run(), Err(SimulationError::NotSetUp));
        assert_eq!(
            simulator.set_input("a", Register::from_binary("1")),
            Err(SimulationError::NotSetUp)
        );
    }

    /// The unsigned value of a signal, which is what a width test is about.
    /// A signal read as four-state bits, which is the only way to see the `z`
    /// a three-state driver leaves behind.
    fn level(simulator: &Simulator, name: &str) -> String {
        simulator
            .get(name)
            .unwrap_or_else(|_| panic!("no signal `{}`", name))
            .to_binary()
    }

    fn number(simulator: &Simulator, name: &str) -> u128 {
        simulator
            .get(name)
            .unwrap_or_else(|_| panic!("no signal `{}`", name))
            .to_u128()
            .unwrap_or_else(|| panic!("`{}` has unknown bits", name))
    }

    /// The headline of context-determined widths: the target widens the
    /// operands *before* the operator runs, so the whole sixteen bit product of
    /// two eight bit numbers survives. Sizing each operand by itself multiplies
    /// in eight bits — 600 truncates to 88 — and then pads a plausible wrong
    /// number out to sixteen.
    ///
    /// Both assignment flavours go through the same instruction, so the
    /// non-blocking one is here to say the deferred write is sized by the
    /// target it was resolved against rather than by the value it carried.
    #[test]
    fn test_multiplication_widens_to_its_target() {
        let simulator = simulator_for(
            r#"
            module widening_multiply();
                reg [15:0] blocking, deferred;
                reg [7:0] a, b;
                initial begin
                    a = 8'd200;
                    b = 8'd3;
                    blocking = a * b;
                    deferred <= a * b;
                end
            endmodule
        "#,
        );

        assert_eq!(number(&simulator, "blocking"), 600);
        assert_eq!(number(&simulator, "deferred"), 600);
    }

    /// A continuous assignment is an assignment too: the net being driven sizes
    /// the expression driving it, which is the `propagate` half of the same
    /// rule.
    #[test]
    fn test_continuous_assignment_widens_to_its_net() {
        let mut simulator = simulator_for(
            r#"
            module widening_assign();
                reg [7:0] a, b;
                wire [15:0] product;
                assign product = a * b;
                initial begin
                    a = 8'd200;
                    b = 8'd3;
                end
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();

        assert_eq!(number(&simulator, "product"), 600);
    }

    /// A shift's two operands are sized by opposite rules, and this is the pair
    /// that tells them apart.
    ///
    /// The *value* is context-determined, so `8'h81 << 3` into a sixteen bit
    /// target keeps the bits that would have fallen off the top — that is the
    /// corpus `shift_pad` case. The *amount* is self-determined, so the two bit
    /// sum `3 + 2` still wraps to 1 rather than reaching 5 because the target is
    /// wide: shifting by one gives 2, shifting by five would give 32.
    #[test]
    fn test_a_shift_widens_its_value_but_not_its_amount() {
        let simulator = simulator_for(
            r#"
            module shift_widths();
                reg [15:0] shifted, by_sum;
                reg [7:0] a;
                reg [1:0] s, t;
                initial begin
                    a = 8'h81;
                    s = 2'd3;
                    t = 2'd2;
                    shifted = a << s;
                    by_sum = 8'h01 << (s + t);
                end
            endmodule
        "#,
        );

        assert_eq!(number(&simulator, "shifted"), 0x0408);
        assert_eq!(number(&simulator, "by_sum"), 2);
    }

    /// A comparison answers in one bit however wide the target is, and the
    /// target's width does not reach its operands: `9 + 8` still wraps to 1 in
    /// four bits, so the answer is false. An operand widened to the eight bit
    /// target would hold 17 and compare true.
    ///
    /// The two operands do size *each other*, which is a separate rule — both
    /// are four bits here, so there is nothing for it to do.
    #[test]
    fn test_a_comparison_is_one_bit_and_sizes_its_own_operands() {
        let simulator = simulator_for(
            r#"
            module comparison_widths();
                reg [7:0] answer;
                reg [3:0] a;
                initial begin
                    a = 4'd9;
                    answer = (a + 4'd8 > 4'd2);
                end
            endmodule
        "#,
        );

        assert_eq!(number(&simulator, "answer"), 0);
        assert_eq!(simulator.get("answer").unwrap().width(), 8);
    }

    /// A concatenation sizes itself out of its parts and a context cannot reach
    /// into them, so wrapping an expression in `{}` is how a design *asks* for
    /// the self-determined answer. The pair is the point: the same addition
    /// wraps at eight bits inside the braces and survives at sixteen without
    /// them.
    #[test]
    fn test_a_concatenation_is_not_widened_by_its_target() {
        let simulator = simulator_for(
            r#"
            module concatenation_widths();
                reg [15:0] braced, plain;
                reg [7:0] a, b;
                initial begin
                    a = 8'hff;
                    b = 8'h01;
                    braced = {a + b};
                    plain = a + b;
                end
            endmodule
        "#,
        );

        assert_eq!(number(&simulator, "braced"), 0x0000);
        assert_eq!(number(&simulator, "plain"), 0x0100);
    }

    /// Corpus `pr2823711`, which names the rule outright: `**` takes its left
    /// operand's width, widened to the context, so `4'hf ** 6'ha` is a sixteen
    /// bit power when it is assigned to a sixteen bit register and a four bit
    /// one when a concatenation cuts the context off.
    #[test]
    fn test_power_takes_the_width_of_its_context() {
        let simulator = simulator_for(
            r#"
            module power_widths();
                reg [15:0] contextual, braced;
                reg [3:0] a;
                reg [5:0] b;
                initial begin
                    a = 4'hf;
                    b = 6'ha;
                    contextual = a ** b;
                    braced = { a ** b };
                end
            endmodule
        "#,
        );

        assert_eq!(number(&simulator, "contextual"), 0xac61);
        assert_eq!(number(&simulator, "braced"), 0x0001);
    }

    /// Unary `-` is context-determined too, so the literal is padded out to the
    /// target *first* and negated there. Negating in four bits and padding
    /// afterwards would store `16'h0004`.
    #[test]
    fn test_negation_widens_before_it_negates() {
        let simulator = simulator_for(
            r#"
            module negation_width();
                reg [15:0] negated;
                initial negated = -4'd12;
            endmodule
        "#,
        );

        assert_eq!(number(&simulator, "negated"), 0xfff4);
    }

    /// Signedness has to survive the whole path — parser, elaboration,
    /// evaluation and the write back into the store — so this asserts it end to
    /// end on a self-checking design, the way the corpus does.
    #[test]
    fn test_signed_declarations_simulate() {
        let simulator = simulator_for(
            r#"
            module signed_arithmetic();
                reg signed [3:0] a;
                reg [3:0] b;
                reg [7:0] wide;
                reg signed [7:0] swide;
                initial begin
                    a = 4'b1111;
                    b = 4'b1111;
                    // A signed value widens by replicating its sign bit; the
                    // same bits declared unsigned widen with zeros.
                    wide = a;
                    swide = b;
                    if (wide !== 8'hff) $display("FAILED sign extension: %b", wide);
                    else if (swide !== 8'h0f) $display("FAILED zero extension: %b", swide);
                    else if (a >= 0) $display("FAILED comparison");
                    else if (b < 0) $display("FAILED unsigned comparison");
                    // The literal is `4'sb1111`, not `4'b1111`: a comparison
                    // reads both its operands unsigned the moment either one
                    // is, and an unsigned `a` would make the `>>>` a plain
                    // `>>`. Comparing against a *signed* literal is what keeps
                    // this a question about the shift.
                    else if ((a >>> 1) !== 4'sb1111) $display("FAILED arithmetic shift");
                    else if ((b >>> 1) !== 4'b0111) $display("FAILED logical shift");
                    else $display("PASSED");
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.output().text(), "PASSED\n");
    }

    #[test]
    fn test_display_in_an_initial_block_is_readable_afterwards() {
        // Setup drains time zero, so the `initial` block has already run and
        // said what it thinks of itself.
        let simulator = simulator_for(
            r#"
            module self_checking();
                initial $display("PASSED");
            endmodule
        "#,
        );

        assert_eq!(simulator.output().text(), "PASSED\n");
        assert_eq!(simulator.output().lines(), vec!["PASSED"]);
    }

    #[test]
    fn test_display_reports_signal_values_and_the_time() {
        let mut simulator = simulator_for(
            r#"
            module report(
                output reg [7:0] count
            );
                initial begin
                    count = 8'd7;
                    $display("count=%0d %b at %0d", count, count, $time);
                    #20;
                    count = 8'd8;
                    $display("count=%0d %b at %0d", count, count, $time);
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.output().lines(), vec!["count=7 00000111 at 0"]);

        simulator.advance(20).unwrap();
        assert_eq!(
            simulator.output().lines(),
            vec!["count=7 00000111 at 0", "count=8 00001000 at 20"]
        );
    }

    #[test]
    fn test_finish_stops_the_simulation() {
        let mut simulator = simulator_for(
            r#"
            module stopper(
                output reg [3:0] count
            );
                initial begin
                    count = 4'd0;
                    #10;
                    $display("running at %0d", $time);
                    count = 4'd1;
                    #10;
                    $finish;
                    count = 4'd2;
                    $display("never reached");
                end
            endmodule
        "#,
        );

        assert!(!simulator.finished());
        simulator.advance(10).unwrap();
        assert_eq!(simulator.output().lines(), vec!["running at 10"]);

        // `$finish` lands at 20 and takes the rest of the block with it.
        simulator.advance(10).unwrap();
        assert!(simulator.finished());
        assert_eq!(simulator.now(), 20);
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(1));

        // Nothing runs after that, and time no longer moves.
        simulator.advance(1000).unwrap();
        assert_eq!(simulator.now(), 20);
        assert_eq!(simulator.output().lines(), vec!["running at 10"]);
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(1));
    }

    #[test]
    fn test_finish_stops_a_free_running_design() {
        // The clock would toggle forever; `$finish` is what ends it.
        let mut simulator = simulator_for(
            r#"
            module timed(
                output reg clk
            );
                initial begin
                    clk = 1'b0;
                    #25;
                    $finish;
                end
                always begin
                    #10 clk = ~clk;
                end
            endmodule
        "#,
        );

        simulator.advance(1000).unwrap();
        assert!(simulator.finished());
        assert_eq!(simulator.now(), 25);
    }

    /// `$finish` ends the simulation at the end of the timestep it ran in, not
    /// the instant it ran: everything already scheduled at that instant still
    /// runs, and the deferred `$monitor` still reports. Measured against
    /// iverilog 12.0, which prints
    ///
    /// ```text
    /// mon 0 a=0
    /// always at 10 a=9
    /// mon 10 a=9
    /// always at 20 a=10
    /// mon 20 a=10
    /// finish at 30 a=10
    /// always at 30 a=2
    /// mon 30 a=2
    /// ```
    ///
    /// Stopping at the `$finish` instead drops the last two lines, which is
    /// what corpus `pr243` was missing.
    #[test]
    fn test_finish_ends_the_timestep_rather_than_the_instant() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg [3:0] a;
                initial begin
                    $monitor("mon %0t a=%0d", $time, a);
                    a = 0;
                    #10 a = 1;
                    #10 a = 2;
                    #10 $display("finish at %0t a=%0d", $time, a);
                    $finish;
                    $display("after finish");
                end
                always #10 begin
                    a = a + 8;
                    $display("always at %0t a=%0d", $time, a);
                end
                initial #35 $display("never reached");
            endmodule
        "#,
        );

        simulator.advance(1000).unwrap();
        assert_eq!(
            simulator.output().lines(),
            vec![
                "mon 0 a=0",
                "always at 10 a=9",
                "mon 10 a=9",
                "always at 20 a=10",
                "mon 20 a=10",
                "finish at 30 a=10",
                "always at 30 a=2",
                "mon 30 a=2",
            ]
        );
        assert_eq!(simulator.now(), 30);
    }

    #[test]
    fn test_system_task_inside_an_always_block() {
        let mut simulator = simulator_for(
            r#"
            module logger(
                input clk,
                output reg [3:0] count
            );
                initial count = 4'd0;
                always @(posedge clk) begin
                    $display("tick %0d", count);
                    count <= count + 1;
                end
            endmodule
        "#,
        );

        simulator.tick("clk").unwrap();
        simulator.tick("clk").unwrap();

        // The block reads `count` before its non-blocking update lands, so the
        // first tick reports the old value.
        assert_eq!(simulator.output().lines(), vec!["tick 0", "tick 1"]);
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(2));
    }

    #[test]
    fn test_system_task_inside_an_if_arm() {
        // The call sits inside a branch, so it only runs when the jump-threaded
        // program actually reaches it.
        let mut simulator = simulator_for(
            r#"
            module branching(
                input sel,
                output reg done
            );
                always @(sel) begin
                    if (sel) $display("taken");
                    else $display("not taken");
                    done = 1'b1;
                end
            endmodule
        "#,
        );

        simulator.poke("sel", one()).unwrap();
        assert_eq!(simulator.output().lines(), vec!["taken"]);

        simulator.poke("sel", zero()).unwrap();
        assert_eq!(simulator.output().lines(), vec!["taken", "not taken"]);
        assert_eq!(simulator.get("done").unwrap().to_binary(), "1");
    }

    #[test]
    fn test_write_leaves_the_line_open_for_the_next_task() {
        let simulator = simulator_for(
            r#"
            module piecewise();
                initial begin
                    $write("PAS");
                    $write("SED");
                    $display("!");
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.output().text(), "PASSED!\n");
    }

    #[test]
    fn test_an_unknown_system_task_is_reported_by_name() {
        let (_, module) = parse_module_declaration(
            r#"
            module mystery(
                output reg a
            );
                initial begin
                    a = 1'b0;
                    $nosuchtask("x");
                end
            endmodule
        "#,
        )
        .unwrap();

        let mut simulator = Simulator::new(module);
        assert_eq!(
            simulator.setup(),
            Err(SimulationError::SystemTask(
                "unknown system task `$nosuchtask`".to_string()
            ))
        );
    }

    /// Issue #104: a Verilog-1995 module simulates, and the width of its output
    /// comes from the body declaration — the header carries names only.
    #[test]
    fn test_a_non_ansi_module_simulates() {
        let mut simulator = simulator_for(
            r#"
            module widen ( a, q );
                input a;
                output [11:0] q;
                assign q = a ? 12'hfff : 12'h000;
            endmodule
            "#,
        );

        simulator.set_input("a", one()).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(0xfff));
        assert_eq!(simulator.get("q").unwrap().width(), 12);

        simulator.set_input("a", zero()).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(0));
    }

    /// A port that is both `output` and `reg` is one signal, not two: the
    /// direction declaration is the port, the `reg` says what backs it.
    #[test]
    fn test_a_non_ansi_output_reg_is_one_signal() {
        let mut simulator = simulator_for(
            r#"
            module counter95 ( clk, count );
                input clk;
                output [3:0] count;
                reg [3:0] count;
                initial count = 0;
                always @(posedge clk) count <= count + 1;
            endmodule
            "#,
        );

        assert_eq!(simulator.state.names().len(), 2);
        assert_eq!(simulator.get("count").unwrap().width(), 4);
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(0));

        for expected in 1..=3 {
            simulator.poke("clk", one()).unwrap();
            simulator.poke("clk", zero()).unwrap();
            assert_eq!(simulator.get("count").unwrap().to_u128(), Some(expected));
        }
    }

    /// A delay prefixing a system task has to hold the task back, not just the
    /// next assignment: the print lands when the delay expires and not before.
    #[test]
    fn test_a_delayed_system_task_prints_when_the_delay_expires() {
        let mut simulator = simulator_for(
            r#"
            module delayed_task(
                output reg [3:0] count
            );
                initial begin
                    count = 4'd0;
                    #10 $display("first at %0d", $time);
                    #10 $display("second at %0d", $time);
                end
            endmodule
        "#,
        );

        assert!(simulator.output().lines().is_empty());
        simulator.advance(9).unwrap();
        assert!(simulator.output().lines().is_empty());

        simulator.advance(1).unwrap();
        assert_eq!(simulator.output().lines(), vec!["first at 10"]);

        simulator.advance(10).unwrap();
        assert_eq!(
            simulator.output().lines(),
            vec!["first at 10", "second at 20"]
        );
    }

    /// A delay prefixing an `if` holds back the whole conditional — the
    /// condition is not even tested until the delay expires.
    #[test]
    fn test_a_delayed_if_runs_when_the_delay_expires() {
        let mut simulator = simulator_for(
            r#"
            module delayed_if(
                output reg [3:0] q
            );
                initial begin
                    q = 4'd1;
                    #15 if (q == 4'd1) begin
                        q = 4'd2;
                        $display("taken at %0d", $time);
                    end else begin
                        $display("not taken");
                    end
                end
            endmodule
        "#,
        );

        simulator.advance(14).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(1));
        assert!(simulator.output().lines().is_empty());

        simulator.advance(1).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(2));
        assert_eq!(simulator.output().lines(), vec!["taken at 15"]);
    }

    /// A delayed statement nested inside a `case` arm suspends the block and
    /// resumes inside the arm, then carries on past the `case`.
    #[test]
    fn test_a_delayed_statement_inside_a_case_arm_advances_time() {
        let mut simulator = simulator_for(
            r#"
            module delayed_arm(
                output reg [3:0] q
            );
                initial begin
                    q = 4'd0;
                    case (q)
                        4'd0: begin
                            #5 q = 4'd1;
                            #5 $display("arm at %0d", $time);
                        end
                        default: $display("default arm");
                    endcase
                    $display("after at %0d", $time);
                end
            endmodule
        "#,
        );

        simulator.advance(5).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(1));
        assert!(simulator.output().lines().is_empty());

        simulator.advance(5).unwrap();
        assert_eq!(simulator.output().lines(), vec!["arm at 10", "after at 10"]);
    }

    #[test]
    fn test_time_in_an_expression_follows_simulated_time() {
        let mut simulator = simulator_for(
            r#"
            module clocked(output reg [7:0] stamp, output reg late);
                initial begin
                    #10 stamp = $time;
                    late = ($time > 5);
                    #10 stamp = $time + 1;
                end
            endmodule
        "#,
        );

        // Before any time passes `$time` is zero, and the block has not run.
        assert_eq!(simulator.now(), 0);
        simulator.advance(10).unwrap();
        assert_eq!(simulator.get("stamp").unwrap().to_u128(), Some(10));
        assert_eq!(simulator.get("late").unwrap().to_u128(), Some(1));
        simulator.advance(10).unwrap();
        assert_eq!(simulator.get("stamp").unwrap().to_u128(), Some(21));
    }

    #[test]
    fn test_a_design_can_print_and_compare_its_own_time() {
        let mut simulator = simulator_for(
            r#"
            module stamped();
                initial begin
                    #7 $display("t=%0d", $time);
                    if ($time == 7) $display("PASSED");
                end
            endmodule
        "#,
        );
        simulator.advance(10).unwrap();
        assert_eq!(simulator.output().lines(), vec!["t=7", "PASSED"]);
    }

    #[test]
    fn test_random_stimulus_repeats_across_runs() {
        let source = r#"
            module noisy(output reg [7:0] sample);
                initial begin
                    #1 sample = $random;
                    #1 sample = $random;
                end
            endmodule
        "#;

        let draw = || {
            let mut simulator = simulator_for(source);
            let mut samples = Vec::new();
            for _ in 0..2 {
                simulator.advance(1).unwrap();
                samples.push(simulator.get("sample").unwrap().to_u128().unwrap());
            }
            samples
        };

        let first = draw();
        // A design that seeds nothing still simulates the same way twice, which
        // is what lets a self-checking test assert on random stimulus.
        assert_eq!(first, draw());
        assert_ne!(first[0], first[1]);
    }

    #[test]
    fn test_an_unknown_system_function_is_reported_by_name() {
        let (_, module) = parse_module_declaration(
            r#"
            module bogus(output reg [7:0] q);
                initial q = $nosuchfunction(1);
            endmodule
        "#,
        )
        .expect("module should parse");
        let mut simulator = Simulator::new(module);

        // Wherever it lands — elaboration or a later timestep — an unknown
        // `$name` is an error that repeats the name, never a silent zero.
        let message = simulator
            .setup()
            .and_then(|()| simulator.advance(1))
            .expect_err("an unknown system function should fail")
            .to_string();
        assert!(
            message.contains("$nosuchfunction"),
            "unexpected message: {}",
            message
        );
        assert!(simulator.get("q").unwrap().has_unknown());
    }

    // -- the design's own functions ---------------------------------------

    /// A function declared the 1995 way — arguments as `input` declarations
    /// inside the body — is evaluated inside an ordinary expression, and
    /// returns by assigning to its own name.
    #[test]
    fn test_function_evaluates_inside_an_expression() {
        let mut simulator = simulator_for(
            r#"
            module incrementer(input [7:0] a, output [7:0] y);
                function [7:0] do_add;
                    input [7:0] value;
                    do_add = value + 1;
                endfunction

                assign y = do_add(a) + 1;
            endmodule
        "#,
        );

        simulator
            .set_input("a", Register::from_u128(40, 8))
            .unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(42));
    }

    /// Two calls in the *same* expression get their own arguments: a frame per
    /// call is what tells this apart from one set of variables the calls write
    /// over each other.
    #[test]
    fn test_function_arguments_are_bound_per_call() {
        let mut simulator = simulator_for(
            r#"
            module squares(input [7:0] a, input [7:0] b, output [7:0] y);
                function [7:0] square(input [7:0] value);
                    square = value * value;
                endfunction

                assign y = square(a) - square(b);
            endmodule
        "#,
        );

        simulator.set_input("a", Register::from_u128(5, 8)).unwrap();
        simulator.set_input("b", Register::from_u128(3, 8)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(16));

        // The same function, different arguments, a different answer.
        simulator.set_input("a", Register::from_u128(9, 8)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(72));
    }

    /// A body-local variable is the function's own: it is declared into the
    /// frame a call builds, and a loop over it runs to a value.
    #[test]
    fn test_function_with_a_local_variable() {
        let mut simulator = simulator_for(
            r#"
            module popcount(input [3:0] value, output [7:0] ones);
                function [7:0] count_ones;
                    input [3:0] bits;
                    integer i;
                    begin
                        count_ones = 0;
                        for (i = 0; i < 4; i = i + 1)
                            count_ones = count_ones + bits[i];
                    end
                endfunction

                assign ones = count_ones(value);
            endmodule
        "#,
        );

        simulator
            .set_input("value", Register::from_binary("1011"))
            .unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("ones").unwrap().to_u128(), Some(3));
    }

    /// A function may read the design around it, not only its arguments — the
    /// signals its body names are copied into the frame a call runs in.
    #[test]
    fn test_function_reads_a_design_signal() {
        let mut simulator = simulator_for(
            r#"
            module scaled(input [7:0] a, output [7:0] y);
                parameter STEP = 3;

                function [7:0] step_up;
                    input [7:0] value;
                    step_up = value + STEP;
                endfunction

                assign y = step_up(a);
            endmodule
        "#,
        );

        simulator
            .set_input("a", Register::from_u128(10, 8))
            .unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(13));
    }

    /// Recursion works, because a call's variables live in a frame of its own
    /// rather than in the design's store.
    #[test]
    fn test_recursive_function() {
        let mut simulator = simulator_for(
            r#"
            module factorial(input [7:0] n, output [31:0] y);
                function [31:0] fact;
                    input [7:0] value;
                    if (value <= 1)
                        fact = 1;
                    else
                        fact = value * fact(value - 1);
                endfunction

                assign y = fact(n);
            endmodule
        "#,
        );

        simulator.set_input("n", Register::from_u128(5, 8)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(120));
    }

    /// Recursion that never reaches a base case is a named error rather than a
    /// stack overflow.
    #[test]
    fn test_runaway_recursion_is_a_named_error() {
        let mut simulator = simulator_for(
            r#"
            module runaway(input [7:0] n, output [7:0] y);
                function [7:0] forever_deeper;
                    input [7:0] value;
                    forever_deeper = forever_deeper(value + 1);
                endfunction

                assign y = forever_deeper(n);
            endmodule
        "#,
        );

        simulator.set_input("n", Register::from_u128(1, 8)).unwrap();
        let error = simulator.run().expect_err("runaway recursion should fail");
        assert!(
            matches!(
                error,
                SimulationError::Eval(EvalError::FunctionCallDepth { .. })
            ),
            "unexpected error: {:?}",
            error
        );
    }

    /// The same guard, over the deepest frame per call the evaluator has: a
    /// **real** function whose recursive call is an arm of a `?:`, which goes
    /// through `real_conditional` and so adds a frame to every level. The bound
    /// has to fire before the stack does, in a debug build, where the frames
    /// are widest.
    #[test]
    fn test_runaway_recursion_through_a_real_conditional_is_a_named_error() {
        let mut simulator = simulator_for(
            r#"
            module runaway(input [7:0] n, output [7:0] y);
                function real deeper;
                    input real value;
                    deeper = 1 ? deeper(value + 1.0) : 0.0;
                endfunction

                assign y = deeper(n);
            endmodule
        "#,
        );

        simulator.set_input("n", Register::from_u128(1, 8)).unwrap();
        let error = simulator.run().expect_err("runaway recursion should fail");
        assert!(
            matches!(
                error,
                SimulationError::Eval(EvalError::FunctionCallDepth { .. })
            ),
            "unexpected error: {:?}",
            error
        );
    }

    /// A function in an instantiated module belongs to that instance: it is
    /// qualified like every other name, so two instances do not share one.
    #[test]
    fn test_function_inside_an_instance() {
        let child = r#"
            module doubler(input [7:0] a, output [7:0] y);
                function [7:0] twice;
                    input [7:0] value;
                    twice = value + value;
                endfunction

                assign y = twice(a);
            endmodule
        "#;
        let top = r#"
            module top(input [7:0] a, output [7:0] y, output [7:0] z);
                doubler one (.a(a), .y(y));
                doubler two (.a(y), .y(z));
            endmodule
        "#;

        let (_, child) = parse_module_declaration(child).expect("child should parse");
        let (_, top) = parse_module_declaration(top).expect("top should parse");
        let mut simulator = Simulator::with_modules(vec![top, child], "top");
        simulator.setup().unwrap();

        simulator.set_input("a", Register::from_u128(3, 8)).unwrap();
        simulator.run().unwrap();
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(6));
        assert_eq!(simulator.get("z").unwrap().to_u128(), Some(12));
    }

    /// A function may be called from a procedural block as readily as from a
    /// continuous assignment: both go through the same evaluator.
    #[test]
    fn test_function_called_from_a_procedural_block() {
        let mut simulator = simulator_for(
            r#"
            module checker(output reg [7:0] q);
                function [7:0] twice;
                    input [7:0] value;
                    twice = value + value;
                endfunction

                initial begin
                    q = twice(8'd21);
                    if (q == 42)
                        $display("PASSED");
                end
            endmodule
        "#,
        );

        simulator.advance(1).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(42));
        assert_eq!(simulator.output().text().trim(), "PASSED");
    }

    /// What a function body may not do is decided when the design is
    /// elaborated, and each of those is an error naming what it rejected — a
    /// call that quietly did nothing would be far harder to find.
    #[test]
    fn test_a_function_body_that_cannot_be_run_is_rejected() {
        let rejected = [
            ("#5 f = 1;", "a delay inside a function"),
            // `$display` is allowed now — it prints into the buffer the frame
            // shares with the design. A `$strobe` still is not: it reports at
            // the end of a timestep, on a `TaskContext` the call outlives.
            (
                "$strobe(\"hi\");",
                "a deferred system task inside a function",
            ),
            ("f <= 1;", "a non-blocking assignment inside a function"),
            ("f = $random;", "`$random` inside a function"),
        ];

        for (body, expected) in rejected {
            let source = format!(
                r#"
                module rejected(output [7:0] y);
                    reg [7:0] outside;
                    function [7:0] f;
                        input [7:0] value;
                        {}
                    endfunction

                    assign y = f(8'd1);
                endmodule
            "#,
                body
            );
            let (_, module) = parse_module_declaration(&source).expect("module should parse");
            let mut simulator = Simulator::new(module);
            let error = simulator
                .setup()
                .expect_err("the function body should be rejected");
            assert_eq!(
                error.to_string(),
                format!("{} is not supported by the simulator", expected)
            );
        }
    }

    /// A `$display` inside a function body prints, and it prints **before**
    /// whatever the statement that made the call goes on to print.
    ///
    /// The frame carries the design's `Output` handle, so the line lands the
    /// moment it runs rather than at some boundary afterwards. iverilog 12.0
    /// prints exactly this order for the same design:
    ///
    /// ```text
    ///   inside f(1)
    /// outer sees 2
    ///   inside f(2)
    ///   inside f(3)
    /// x = 7
    /// ```
    #[test]
    fn test_a_function_prints_before_the_statement_that_called_it() {
        let mut simulator = simulator_for(
            r#"
            module printer;
                integer x;
                function integer f;
                    input integer a;
                    begin
                        $display("  inside f(%0d)", a);
                        f = a + 1;
                    end
                endfunction

                initial begin
                    $display("outer sees %0d", f(1));
                    x = f(2) + f(3);
                    $display("x = %0d", x);
                end
            endmodule
        "#,
        );
        simulator.advance(10).expect("the design should run");
        assert_eq!(
            simulator.output().lines(),
            vec![
                "  inside f(1)",
                "outer sees 2",
                "  inside f(2)",
                "  inside f(3)",
                "x = 7",
            ]
        );
    }

    /// A function called while the design is still being *elaborated* prints
    /// nothing, because the store the parameters are evaluated against has a
    /// buffer of its own and only `setup` links the design's.
    ///
    /// That is what iverilog does with a constant function: `constfunc13` and
    /// `mixed_width_case` are the same design written the two ways, and their
    /// output is identical — the constant version's thirteen lines come from
    /// its `initial` block rather than from inside the function.
    #[test]
    fn test_a_constant_function_prints_nothing() {
        let mut simulator = simulator_for(
            r#"
            module constant_printer;
                function integer f;
                    input integer a;
                    begin
                        $display("elaborating %0d", a);
                        f = a + 1;
                    end
                endfunction

                localparam RESULT = f(1);

                initial $display("result %0d", RESULT);
            endmodule
        "#,
        );
        simulator.advance(10).expect("the design should run");
        assert_eq!(simulator.output().lines(), vec!["result 2"]);
    }

    /// A continuous assignment is re-evaluated until the design stops moving,
    /// so a function it calls prints an unpredictable number of times. That is
    /// refused by name, on the same terms an outstanding `$sscanf` fill is —
    /// printing twice what iverilog prints once is a wrong answer wearing a
    /// working simulator's clothes (corpus `br948`).
    #[test]
    fn test_a_function_that_prints_cannot_drive_a_continuous_assignment() {
        let (_, module) = parse_module_declaration(
            r#"
            module driven(output y);
                reg a;
                function invert;
                    input value;
                    begin
                        invert = ~value;
                        $display("invert %b", value);
                    end
                endfunction

                assign y = invert(a);
            endmodule
        "#,
        )
        .expect("module should parse");
        let mut simulator = Simulator::new(module);
        simulator.setup().expect("the design should elaborate");
        let error = simulator
            .run()
            .expect_err("the assignment should be refused");
        assert_eq!(
            error.to_string(),
            "a `$display` in a function called from a continuous assignment \
             is not supported by the simulator"
        );
    }

    /// A function may write a design signal, and the write reaches the design.
    ///
    /// The body runs against a frame, so the write cannot land where it is
    /// made; it is queued the way a `$sscanf`'s argument is and carried out at
    /// the next instruction boundary. iverilog prints `1 2` for this design —
    /// the call's own result *and* the signal it left behind.
    #[test]
    fn test_a_function_may_write_a_design_signal() {
        let mut simulator = simulator_for(
            r#"
            module side_effect;
                integer count;
                integer result;
                function integer bump;
                    input integer step;
                    begin
                        count = count + step;
                        bump = count;
                    end
                endfunction

                initial begin
                    count = 0;
                    result = bump(1);
                    $display("%0d %0d", result, count);
                    result = bump(1);
                    $display("%0d %0d", result, count);
                end
            endmodule
        "#,
        );
        simulator.advance(10).expect("the design should run");
        assert_eq!(simulator.output().lines(), vec!["1 1", "2 2"]);
    }

    /// Two calls in *one* expression see each other's writes.
    ///
    /// Nothing drains the queue between them, so the second call seeds its
    /// frame from what the first one owes rather than from a signal it has not
    /// reached yet. That is the whole of what corpus `concat3` measures: with
    /// a stale read both halves of `{ufunc(0), ufunc(0)}` come back the same,
    /// and the design reports the concatenation order as broken.
    #[test]
    fn test_two_calls_in_one_expression_see_each_others_writes() {
        let mut simulator = simulator_for(
            r#"
            module ordered;
                integer count;
                reg [63:0] pair;
                function integer bump;
                    input dummy;
                    begin
                        count = count + 1;
                        bump = count;
                    end
                endfunction

                initial begin
                    count = 0;
                    pair = {bump(0), bump(0)};
                    $display("%0d %0d %0d", pair[63:32], pair[31:0], count);
                end
            endmodule
        "#,
        );
        simulator.advance(10).expect("the design should run");
        assert_eq!(simulator.output().lines(), vec!["1 2 2"]);
    }

    /// A function with a side effect cannot drive a continuous assignment, for
    /// the reason one that prints cannot: the assignment is re-evaluated until
    /// the design settles, so the side effect would happen an unpredictable
    /// number of times (corpus `concat4`).
    #[test]
    fn test_a_function_with_a_side_effect_cannot_drive_a_continuous_assignment() {
        let (_, module) = parse_module_declaration(
            r#"
            module driven(output [31:0] y);
                integer count;
                function integer bump;
                    input dummy;
                    begin
                        count = count + 1;
                        bump = count;
                    end
                endfunction

                assign y = bump(0);
            endmodule
        "#,
        )
        .expect("module should parse");
        let mut simulator = Simulator::new(module);
        simulator.setup().expect("the design should elaborate");
        let error = simulator
            .run()
            .expect_err("the assignment should be refused");
        assert_eq!(
            error.to_string(),
            "an expression that writes the design — `$sscanf` or a function with a \
             side effect — in a continuous assignment is not supported by the simulator"
        );
    }

    /// A parameter's value may be a call, which is why functions are compiled
    /// before the declarations rather than with them.
    #[test]
    fn test_a_parameter_may_be_computed_by_a_function() {
        let mut simulator = simulator_for(
            r#"
            module sized(output [7:0] y);
                function [7:0] doubled;
                    input [7:0] value;
                    doubled = value * 2;
                endfunction

                localparam WIDTH = doubled(8'd4);
                assign y = WIDTH;
            endmodule
        "#,
        );

        simulator.run().unwrap();
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(8));
    }

    /// An `@(*)` block that calls a function is sensitive to what the
    /// *function* reads as well as to what the block itself names — otherwise a
    /// design whose call is its only reader would never wake.
    #[test]
    fn test_implicit_sensitivity_reaches_through_a_call() {
        let mut simulator = simulator_for(
            r#"
            module sensitive(input clk, output reg [7:0] y);
                reg [7:0] offset;

                function [7:0] plus_offset;
                    input [7:0] value;
                    plus_offset = value + offset;
                endfunction

                initial offset = 8'd0;
                always @(posedge clk) offset = offset + 1;
                always @(*) y = plus_offset(8'd10);
            endmodule
        "#,
        );

        simulator.advance(1).unwrap();
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(10));

        // `y` names neither `offset` nor anything that moved but the clock, so
        // it can only follow if the call's own reads are in the block's
        // sensitivity list.
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(11));
    }

    /// A call with the wrong number of arguments names the function and both
    /// counts.
    #[test]
    fn test_function_called_with_the_wrong_number_of_arguments() {
        let (_, module) = parse_module_declaration(
            r#"
            module mismatched(output [7:0] y);
                function [7:0] twice;
                    input [7:0] value;
                    twice = value + value;
                endfunction

                assign y = twice(8'd1, 8'd2);
            endmodule
        "#,
        )
        .expect("module should parse");
        let mut simulator = Simulator::new(module);
        simulator.setup().unwrap();

        let error = simulator.run().expect_err("the arity should be rejected");
        assert!(
            matches!(
                error,
                SimulationError::Eval(EvalError::FunctionArity {
                    expected: 1,
                    found: 2,
                    ..
                })
            ),
            "unexpected error: {:?}",
            error
        );
    }
    /// The substance of the feature: an `output` argument is copied back to
    /// the caller's variable when the task returns, and an `inout` one is
    /// copied both ways. Checked against iverilog, which gives `a = 3`,
    /// `b = 12` at time 5.
    #[test]
    fn test_task_copies_output_and_inout_arguments_back() {
        let mut simulator = simulator_for(
            r#"
            module copier();
                reg [7:0] a, b;
                task tk(input [7:0] x, output [7:0] y, inout [7:0] z);
                    begin
                        y = x + 1;
                        z = z + 2;
                        #5;
                        y = y + 10;
                    end
                endtask

                initial begin
                    a = 8'd1;
                    b = 8'd5;
                    tk(a, b, a);
                    $display("a=%0d b=%0d", a, b);
                end
            endmodule
        "#,
        );

        simulator.advance(10).unwrap();
        assert_eq!(simulator.get("a").unwrap().to_u128(), Some(3));
        assert_eq!(simulator.get("b").unwrap().to_u128(), Some(12));
        assert_eq!(simulator.output().text().trim(), "a=3 b=12");
    }

    /// An `input` argument is copied *in* and never back out, so a task that
    /// writes one leaves the caller's variable alone.
    #[test]
    fn test_task_input_argument_is_not_copied_back() {
        let mut simulator = simulator_for(
            r#"
            module one_way();
                reg [7:0] a;
                task tk(input [7:0] x);
                    x = 8'd99;
                endtask

                initial begin
                    a = 8'd7;
                    tk(a);
                end
            endmodule
        "#,
        );

        simulator.advance(1).unwrap();
        assert_eq!(simulator.get("a").unwrap().to_u128(), Some(7));
    }

    /// A task may consume time, which is the whole reason its body is inlined
    /// into the block that enables it: the resume point is a program counter,
    /// and the body's instructions are in the same list.
    #[test]
    fn test_task_containing_a_delay_suspends_its_caller() {
        let mut simulator = simulator_for(
            r#"
            module waiter(output reg [7:0] q);
                task step;
                    begin
                        #10 q = q + 1;
                        #10 q = q + 1;
                    end
                endtask

                initial begin
                    q = 0;
                    step;
                    $display("done at %0d with q=%0d", $time, q);
                end
            endmodule
        "#,
        );

        simulator.advance(5).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(0));
        simulator.advance(10).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(1));
        simulator.advance(20).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(2));
        assert_eq!(simulator.output().text().trim(), "done at 20 with q=2");
    }

    /// A task's arguments and locals are static storage of its own, so a name
    /// it declares does not touch the design signal spelled the same way — and
    /// two enables of one task share those variables the way the LRM says.
    #[test]
    fn test_task_locals_are_separate_from_the_design() {
        let mut simulator = simulator_for(
            r#"
            module shadowed();
                reg [7:0] tmp, out;
                task add_two(input [7:0] value, output [7:0] result);
                    reg [7:0] tmp;
                    begin
                        tmp = value + 2;
                        result = tmp;
                    end
                endtask

                initial begin
                    tmp = 8'd50;
                    add_two(8'd1, out);
                    $display("tmp=%0d out=%0d", tmp, out);
                    add_two(8'd10, out);
                    $display("tmp=%0d out=%0d", tmp, out);
                end
            endmodule
        "#,
        );

        simulator.advance(1).unwrap();
        assert_eq!(
            simulator.output().text().trim(),
            "tmp=50 out=3\ntmp=50 out=12"
        );
    }

    /// A task enabled by a task is inlined into it, so an enable two deep runs
    /// exactly as one written out by hand does.
    #[test]
    fn test_a_task_may_enable_another_task() {
        let mut simulator = simulator_for(
            r#"
            module nested(output reg [7:0] q);
                task inner(input [7:0] value);
                    q = q + value;
                endtask

                task outer(input [7:0] value);
                    begin
                        inner(value);
                        inner(value);
                    end
                endtask

                initial begin
                    q = 0;
                    outer(8'd3);
                end
            endmodule
        "#,
        );

        simulator.advance(1).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(6));
    }

    /// A task with no arguments is enabled by naming it, with no parentheses
    /// at all — and one declared below the block that enables it is still
    /// found, because Verilog puts no ordering requirement on module items.
    #[test]
    fn test_bare_task_enable_finds_a_task_declared_later() {
        let mut simulator = simulator_for(
            r#"
            module bare();
                initial announce;

                task announce;
                    $display("PASSED");
                endtask
            endmodule
        "#,
        );

        simulator.advance(1).unwrap();
        assert_eq!(simulator.output().text().trim(), "PASSED");
    }

    /// Two instances of one module get a set of task variables each, because
    /// they are qualified exactly as any other signal is.
    #[test]
    fn test_two_instances_do_not_share_task_variables() {
        let source = r#"
            module leaf(output reg [7:0] q);
                parameter SEED = 0;

                task scale(input [7:0] value, output [7:0] result);
                    result = value * 2;
                endtask

                initial scale(SEED, q);
            endmodule

            module top();
                wire [7:0] a, b;
                leaf #(.SEED(3)) one (.q(a));
                leaf #(.SEED(5)) two (.q(b));
            endmodule
        "#;
        let (remaining, modules) = crate::parsers::source::parse_verilog_source(source).unwrap();
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        let mut simulator = Simulator::with_modules(modules, "top");
        simulator.setup().unwrap();
        simulator.advance(1).unwrap();

        assert_eq!(simulator.get("a").unwrap().to_u128(), Some(6));
        assert_eq!(simulator.get("b").unwrap().to_u128(), Some(10));
        assert_eq!(simulator.get("one.scale.value").unwrap().to_u128(), Some(3));
        assert_eq!(simulator.get("two.scale.value").unwrap().to_u128(), Some(5));
    }

    /// A task argument may be sized from a parameter, and a parameter override
    /// in the parent reaches it: a task's widths are resolved at elaboration
    /// like every other declaration's, not folded at parse time.
    #[test]
    fn test_a_task_argument_is_sized_from_a_parameter() {
        let source = r#"
            module leaf(output reg [31:0] q);
                parameter WIDTH = 4;

                task widen;
                    input [WIDTH-1:0] a;
                    output [31:0] result;
                    result = a;
                endtask

                initial widen(8'hFF, q);
            endmodule

            module top();
                wire [31:0] narrow, wide;
                leaf #(.WIDTH(4)) small (.q(narrow));
                leaf #(.WIDTH(8)) large (.q(wide));
            endmodule
        "#;
        let (remaining, modules) = crate::parsers::source::parse_verilog_source(source).unwrap();
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        let mut simulator = Simulator::with_modules(modules, "top");
        simulator.setup().unwrap();
        simulator.advance(1).unwrap();

        // The argument truncates what it is handed to its own declared width,
        // which is the parameter's — four bits in one instance, eight in the
        // other.
        assert_eq!(simulator.get("narrow").unwrap().to_u128(), Some(0xF));
        assert_eq!(simulator.get("wide").unwrap().to_u128(), Some(0xFF));
    }

    /// A bound that names nothing is the same named error a `reg` gets, rather
    /// than a width the simulator picked for itself.
    #[test]
    fn test_an_unresolved_task_argument_width_is_a_named_error() {
        let (_, module) = parse_module_declaration(
            r#"
            module unresolved();
                task t;
                    input [n-1:0] a;
                    b = a;
                endtask
                initial t(1);
            endmodule
        "#,
        )
        .expect("module should parse");

        let error = Simulator::new(module)
            .setup()
            .expect_err("the bound should be rejected");
        assert!(
            matches!(error, SimulationError::UnresolvedRange { ref bound, .. } if bound == "n - 1"),
            "unexpected error: {:?}",
            error
        );
    }

    /// A task's body is not in the statement tree, so an `@(*)` block that
    /// enables one has to take its sensitivity list from the compiled
    /// instructions — otherwise it would never wake on a signal only the task
    /// reads.
    #[test]
    fn test_an_implicit_sensitivity_list_sees_what_a_task_reads() {
        let mut simulator = simulator_for(
            r#"
            module sensitive(input [7:0] a, output reg [7:0] q);
                reg [7:0] gain;
                task scale(output [7:0] result);
                    result = a * gain;
                endtask

                initial gain = 8'd3;
                always @(*) scale(q);
            endmodule
        "#,
        );

        simulator.poke("a", Register::from_u128(2, 8)).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(6));
        simulator.poke("a", Register::from_u128(5, 8)).unwrap();
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(15));
    }

    /// A task enabled inside another task keeps its own names: the outer
    /// task's local `tmp` is not the design's `tmp`, and neither of them is the
    /// inner task's. Renaming a body twice is what would confuse them.
    #[test]
    fn test_a_nested_enable_does_not_capture_the_outer_task_locals() {
        let mut simulator = simulator_for(
            r#"
            module layered();
                reg [7:0] tmp, out;

                task inner(output [7:0] result);
                    result = tmp;
                endtask

                task outer(output [7:0] result);
                    reg [7:0] tmp;
                    begin
                        tmp = 8'd99;
                        inner(result);
                    end
                endtask

                initial begin
                    tmp = 8'd4;
                    outer(out);
                end
            endmodule
        "#,
        );

        simulator.advance(1).unwrap();
        assert_eq!(simulator.get("out").unwrap().to_u128(), Some(4));
        assert_eq!(simulator.get("tmp").unwrap().to_u128(), Some(4));
    }

    /// What a task enable cannot do is an error naming it, never a statement
    /// that quietly did nothing.
    #[test]
    fn test_task_enables_that_cannot_be_run_are_rejected() {
        let rejected = [
            (
                "task tk; input a; tk = a; endtask initial tk(1, 2);",
                "task `tk` takes 1 arguments, but was enabled with 2",
            ),
            ("initial missing(1);", "no task named `missing`"),
            (
                "task tk; input a; tk(a); endtask initial tk(1);",
                "task `tk` enables itself",
            ),
        ];

        for (body, expected) in rejected {
            let source = format!("module rejected();\n{}\nendmodule", body);
            let (_, module) = parse_module_declaration(&source).expect("module should parse");
            let error = Simulator::new(module)
                .setup()
                .expect_err("the enable should be rejected");
            assert_eq!(error.to_string(), expected);
        }
    }

    /// The test that catches "every word is secretly one word": several
    /// addresses are written with different values and all of them are read
    /// back.
    #[test]
    fn test_a_memory_holds_a_different_value_at_every_address() {
        let simulator = simulator_for(
            r#"
            module memory_holds_words();
                reg [7:0] mem [0:3];
                initial begin
                    mem[0] = 8'd10;
                    mem[1] = 8'd20;
                    mem[2] = 8'd30;
                    mem[3] = 8'd40;
                    $display("%0d %0d %0d %0d", mem[0], mem[1], mem[2], mem[3]);
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.output().lines(), vec!["10 20 30 40"]);
    }

    /// `mem[i][3:0]` — a select *inside* one word. The word index and the bit
    /// index are the same syntax, and only the declaration says which is
    /// which, so this is the test that catches the two being swapped.
    #[test]
    fn test_a_select_inside_a_memory_word_reads_and_writes() {
        let simulator = simulator_for(
            r#"
            module word_select();
                reg [7:0] mem [0:3];
                reg [1:0] idx;
                initial begin
                    idx = 1;
                    mem[0] = 8'b1010_0101;
                    mem[1] = 8'b0000_0000;
                    // read: a bit, a part and an indexed part of one word
                    $display("%b %b %b %b", mem[0][7], mem[0][3:0], mem[0][4 +: 4],
                             mem[idx][1:0]);
                    // write: each of the three, leaving the rest of the word
                    mem[1][0] = 1'b1;
                    mem[1][7:6] = 2'b11;
                    mem[1][4 +: 2] = 2'b01;
                    mem[idx][2] = 1'b1;
                    $display("%b %b", mem[1], mem[0]);
                end
            endmodule
        "#,
        );

        assert_eq!(
            simulator.output().lines(),
            vec!["1 0101 1010 00", "11010101 10100101"]
        );
    }

    /// A word select on something that is not a memory is a named error, not a
    /// select of the wrong thing: a vector has no second dimension.
    #[test]
    fn test_a_word_select_of_a_vector_is_reported() {
        let (remaining, module) = parse_module_declaration(
            r#"
            module not_a_memory();
                reg [7:0] vec;
                wire q;
                assign q = vec[0][1:0];
            endmodule
        "#,
        )
        .unwrap();
        assert!(remaining.trim().is_empty());

        let mut simulator = Simulator::new(module);
        simulator.setup().expect("design should elaborate");
        let error = simulator.run().expect_err("a vector has no word");
        assert!(
            format!("{}", error).contains("not a memory"),
            "unexpected error: {}",
            error
        );
    }

    /// A word nothing has written reads `x`, like any undriven register. This
    /// is `meminit` in the ivtest corpus.
    #[test]
    fn test_an_unwritten_memory_word_reads_unknown() {
        let simulator = simulator_for(
            r#"
            module memory_starts_unknown();
                reg [3:0] mem [0:1];
                initial begin
                    if (mem[0] !== 4'bxxxx) $display("FAILED -- mem[0] == %b", mem[0]);
                    else if (mem[1] !== 4'bxxxx) $display("FAILED -- mem[1] == %b", mem[1]);
                    else $display("PASSED");
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.output().lines(), vec!["PASSED"]);
    }

    /// A variable address on both sides, which is the whole point of a memory:
    /// a constant index could have been a signal each.
    #[test]
    fn test_a_memory_is_addressed_by_a_variable() {
        let simulator = simulator_for(
            r#"
            module memory_variable_index();
                reg [7:0] mem [0:7];
                integer i;
                initial begin
                    for (i = 0; i < 8; i = i + 1) mem[i] = i * 3;
                    for (i = 7; i >= 0; i = i - 1) $write("%0d ", mem[i]);
                    $display("");
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.output().lines(), vec!["21 18 15 12 9 6 3 0 "]);
    }

    /// A clocked memory: the address and the data both move, and the word
    /// written under one address must still be there after another is written.
    #[test]
    fn test_a_clocked_memory_keeps_every_word_it_was_given() {
        let mut simulator = simulator_for(
            r#"
            module ram(
                input clk,
                input [2:0] addr,
                input [7:0] data,
                output reg [7:0] q
            );
                reg [7:0] mem [0:7];
                always @(posedge clk) begin
                    mem[addr] <= data;
                    q <= mem[addr];
                end
            endmodule
        "#,
        );

        let write = |simulator: &mut Simulator, address: u128, value: u128| {
            simulator
                .set_input("addr", Register::from_u128(address, 3))
                .unwrap();
            simulator
                .set_input("data", Register::from_u128(value, 8))
                .unwrap();
            simulator.tick("clk").unwrap();
        };
        write(&mut simulator, 2, 0xAA);
        write(&mut simulator, 5, 0x55);

        // Reading back address 2 must still see what was written there, not the
        // later write to address 5.
        simulator
            .set_input("addr", Register::from_u128(2, 3))
            .unwrap();
        simulator
            .set_input("data", Register::from_u128(0, 8))
            .unwrap();
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("q").unwrap().to_binary(), "10101010");
    }

    /// `reg [7:0] m [15:8];` — neither zero based nor ascending.
    #[test]
    fn test_a_memory_with_a_descending_non_zero_based_range_addresses_correctly() {
        let simulator = simulator_for(
            r#"
            module descending_memory();
                reg [7:0] m [15:8];
                initial begin
                    m[15] = 8'd1;
                    m[8] = 8'd2;
                    $display("%0d %0d %b %b", m[15], m[8], m[7], m[16]);
                end
            endmodule
        "#,
        );

        // Both ends of the declared range hold their own value, and an address
        // outside it reads `x`.
        assert_eq!(simulator.output().lines(), vec!["1 2 xxxxxxxx xxxxxxxx"]);
    }

    /// The regression risk: `a[3]` on a plain vector is still a *bit*.
    #[test]
    fn test_a_plain_vector_still_bit_selects() {
        let simulator = simulator_for(
            r#"
            module vector_and_memory();
                reg [7:0] a;
                reg m [0:7];
                initial begin
                    a = 8'b00001000;
                    m[3] = 1'b0;
                    m[7] = 1'b1;
                    // `a[3]` is one bit of a byte; `m[3]` is one word of an array.
                    $display("%b %b %b %b", a[3], a[2], m[3], m[7]);
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.output().lines(), vec!["1 0 0 1"]);
    }

    /// An address outside the declared range reads `x` and swallows a write,
    /// which is what an out-of-range bit select already did.
    #[test]
    fn test_an_out_of_range_memory_address_reads_x_and_discards_a_write() {
        let simulator = simulator_for(
            r#"
            module out_of_range_memory();
                reg [3:0] mem [0:1];
                initial begin
                    mem[0] = 4'd1;
                    mem[9] = 4'd2;
                    $display("%b %b", mem[0], mem[9]);
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.output().lines(), vec!["0001 xxxx"]);
    }

    /// A continuous assignment reading a memory word settles against it, which
    /// is the shape of the corpus file `pr2890322`.
    #[test]
    fn test_a_continuous_assignment_reads_a_memory_word() {
        let mut simulator = simulator_for(
            r#"
            module memory_through_a_wire();
                reg [7:0] mem [0:1];
                wire [7:0] sum = mem[0] + mem[1];
                initial begin
                    mem[0] = 1;
                    mem[1] = 2;
                    #1 if (sum === 3) $display("PASSED");
                       else $display("FAILED %b", sum);
                end
            endmodule
        "#,
        );

        simulator.advance(2).unwrap();
        assert_eq!(simulator.output().lines(), vec!["PASSED"]);
    }

    /// A memory write is an edge, so a block sensitive to what a word feeds
    /// wakes on it. Without this, `pr2011429` in the corpus goes wrong.
    #[test]
    fn test_a_block_wakes_when_a_memory_word_moves() {
        let mut simulator = simulator_for(
            r#"
            module wake_on_memory();
                reg [7:0] bus;
                reg picked;
                integer index [0:0];
                always @(bus[index[0]]) picked = bus[index[0]];
                initial begin
                    bus = 8'b10101010;
                    index[0] = 0;
                    #1 $display("%b", picked);
                    index[0] = 1;
                    #1 $display("%b", picked);
                end
            endmodule
        "#,
        );

        simulator.advance(5).unwrap();
        assert_eq!(simulator.output().lines(), vec!["0", "1"]);
    }

    /// A memory has no value of its own, and saying so by name beats reporting
    /// it as an identifier that does not exist.
    #[test]
    fn test_a_memory_used_as_a_value_is_reported_as_a_memory() {
        let (remaining, module) = parse_module_declaration(
            r#"
            module memory_as_value();
                reg [7:0] mem [0:1];
                wire [7:0] q;
                assign q = mem;
            endmodule
        "#,
        )
        .unwrap();
        assert!(remaining.trim().is_empty());

        let mut simulator = Simulator::new(module);
        simulator.setup().expect("design should elaborate");
        let error = simulator.run().expect_err("mem has no value");
        assert!(
            format!("{}", error).contains("memory `mem`"),
            "unexpected error: {}",
            error
        );
    }
    /// `$strobe` is `$display` moved to the end of the timestep, and the point
    /// of the move is that it sees what the rest of the step went on to do.
    #[test]
    fn test_strobe_reports_the_end_of_the_timestep_not_the_moment_it_ran() {
        let simulator = simulator_for(
            r#"
            module deferred();
                reg [3:0] a;
                initial begin
                    a = 4'd1;
                    $display("display %0d", a);
                    $strobe("strobe %0d", a);
                    a = 4'd7;
                    $display("display %0d", a);
                end
            endmodule
        "#,
        );

        assert_eq!(
            simulator.output().lines(),
            vec!["display 1", "display 7", "strobe 7"],
            "a `$strobe` must print last, and must report the later value"
        );
    }

    /// A `$monitor` prints once when it is armed and then only when one of the
    /// values it printed has moved — a timestep that changes nothing it reads
    /// produces no line at all.
    #[test]
    fn test_monitor_reprints_only_when_an_argument_moves() {
        let mut simulator = simulator_for(
            r#"
            module watched();
                reg [3:0] a;
                initial begin
                    a = 4'd0;
                    $monitor("a=%0d", a);
                    #10 a = 4'd1;
                    #10 a = 4'd1;
                    #10 a = 4'd2;
                end
            endmodule
        "#,
        );
        simulator.advance(40).expect("time should advance");

        // Nothing for time 20, where the assignment wrote the value that was
        // already there.
        assert_eq!(simulator.output().lines(), vec!["a=0", "a=1", "a=2"]);
    }

    #[test]
    fn test_monitoroff_suppresses_and_monitoron_resumes() {
        let mut simulator = simulator_for(
            r#"
            module switched();
                reg [3:0] a;
                initial begin
                    a = 4'd0;
                    $monitor("a=%0d", a);
                    #10 a = 4'd1;
                    #10 $monitoroff;
                    #10 a = 4'd2;
                    #10 $monitoron;
                    #10 a = 4'd3;
                end
            endmodule
        "#,
        );
        simulator.advance(60).expect("time should advance");

        // `a = 2` at time 30 goes unreported; `$monitoron` at time 40 reports
        // it at once, the way the LRM asks, and then time 50 reports the 3.
        assert_eq!(simulator.output().lines(), vec!["a=0", "a=1", "a=2", "a=3"]);
    }

    /// Only one monitor is ever active, so a second `$monitor` takes the place
    /// of the first rather than joining it.
    #[test]
    fn test_a_second_monitor_replaces_the_first() {
        let mut simulator = simulator_for(
            r#"
            module replaced();
                reg [3:0] a, b;
                initial begin
                    a = 4'd0;
                    b = 4'd0;
                    $monitor("a=%0d", a);
                    #10 $monitor("b=%0d", b);
                    #10 a = 4'd5;
                    #10 b = 4'd6;
                end
            endmodule
        "#,
        );
        simulator.advance(40).expect("time should advance");

        // Time 20 moves `a`, which nothing watches any more.
        assert_eq!(simulator.output().lines(), vec!["a=0", "b=0", "b=6"]);
    }

    /// A directory of this test's own, so a data file cannot collide with
    /// another test's or with anything in the repository.
    fn scratch_directory(name: &str) -> PathBuf {
        let directory = std::env::temp_dir().join(format!("visilog-{}", name));
        fs::create_dir_all(&directory).expect("scratch directory should be creatable");
        directory
    }

    /// The plus-args a caller handed the simulator reach a design through
    /// `$test$plusargs` and `$value$plusargs`, and a plus-arg nothing matches
    /// leaves its target **alone** — which is the answer a design reads, not a
    /// failure. Every line is what iverilog 12.0 prints for the same design run
    /// as `vvp … +option=0123456789abcdef +rate=2.5`.
    ///
    /// Each read is its own statement on purpose: a fill lands at the next
    /// instruction boundary, the same way `$sscanf`'s does, so a design that
    /// reads the target in the *same* statement as the call sees the old value
    /// where iverilog sees the new one.
    #[test]
    fn test_plusargs_reach_a_design_and_a_missing_one_changes_nothing() {
        let (remaining, module) = parse_module_declaration(
            r#"
            module reader();
                reg [63:0] wide;
                reg [31:0] norm;
                real rate;
                integer code;
                initial begin
                    $display("%0d %0d", $test$plusargs("opt"), $test$plusargs("nope"));
                    code = $value$plusargs("option=%h", wide);
                    $display("%0d %h", code, wide);
                    code = $value$plusargs("option=%h", norm);
                    $display("%0d %h", code, norm);
                    code = $value$plusargs("missing=%d", norm);
                    $display("%0d %h", code, norm);
                    code = $value$plusargs("rate=%f", rate);
                    $display("%0d %f", code, rate);
                end
            endmodule
        "#,
        )
        .expect("design should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);

        let mut simulator = Simulator::new(module);
        // With and without the `+`, which is how a caller has it either way.
        simulator.add_plusarg("+option=0123456789abcdef");
        simulator.add_plusarg("rate=2.5");
        simulator.setup().expect("design should run");

        assert_eq!(
            simulator.output().lines(),
            vec![
                "1 0",
                "1 0123456789abcdef",
                "1 89abcdef",
                "0 89abcdef",
                "1 2.500000",
            ]
        );
    }

    /// A design handed no plus-args at all is *told* so rather than stopped:
    /// `$test$plusargs` is `0` and `$value$plusargs` is `0` with its target
    /// untouched, which is exactly the branch `if (!$value$plusargs(…))` exists
    /// to take. visilog has no command line, so this is what every caller that
    /// says nothing gets.
    #[test]
    fn test_a_design_with_no_plusargs_is_told_so() {
        let (_, module) = parse_module_declaration(
            r#"
            module reader();
                reg [7:0] opt;
                integer code;
                initial begin
                    opt = 8'h5a;
                    code = $value$plusargs("size=%d", opt);
                    $display("%0d %0d %h", $test$plusargs("size"), code, opt);
                end
            endmodule
        "#,
        )
        .expect("design should parse");

        let mut simulator = Simulator::new(module);
        simulator.setup().expect("design should run");
        assert_eq!(simulator.output().lines(), vec!["0 0 5a"]);
    }

    /// `$printtimescale` reports the scale of the scope it names, and every
    /// line here is what iverilog 12.0 prints for this design (corpus
    /// `pr1701855`, whose gold file this is).
    ///
    /// Three rules are load-bearing. A name is tried **as written** first, so
    /// `othertop` — a module the design never instantiated — answers for
    /// itself rather than shedding its tail and answering for `top`. One that
    /// names nothing on its own is qualified by the instance the call sits in,
    /// so `dut` is `top.dut`. And what is printed is the *whole* name, not the
    /// scope that matched it.
    #[test]
    fn test_printtimescale_reports_the_scope_it_names() {
        let source = "`timescale 1us/1ns\n\
             module top;\n\
               initial begin\n\
                 $printtimescale;\n\
                 $printtimescale(dut);\n\
                 $printtimescale(dut.dut);\n\
                 $printtimescale(othertop);\n\
               end\n\
               lower dut();\n\
             endmodule\n\
             `timescale 10ns/10ps\n\
             module lower; evenlower dut(); endmodule\n\
             `timescale 1ns/10ps\n\
             module evenlower; endmodule\n\
             `timescale 1ms/1us\n\
             module othertop; endmodule\n";
        let parsed = crate::parsers::source::parse_source(source).expect("design should parse");

        let mut simulator = Simulator::with_modules(parsed.modules, "top");
        simulator.setup().expect("design should run");

        assert_eq!(
            simulator.output().lines(),
            vec![
                "Time scale of (top) is 1us / 1ns",
                "Time scale of (top.dut) is 10ns / 10ps",
                "Time scale of (top.dut.dut) is 1ns / 10ps",
                "Time scale of (othertop) is 1ms / 1us",
            ]
        );
    }

    /// A module that declared no `` `timescale `` is at the default, `1s / 1s`;
    /// a named block has none of its own and answers for the instance around
    /// it; and a bit select of a *vector* is written back as the one-bit part
    /// select it stands for, where a word of a memory keeps its single index.
    /// All of it is iverilog 12.0's, from corpus `pr1403406` and `pr1701855b`.
    #[test]
    fn test_printtimescale_defaults_and_renders_a_select_the_way_iverilog_does() {
        let source = "module top;\n\
               reg [1:0] rgval;\n\
               reg rgarr [2:0];\n\
               initial begin : blk\n\
                 $printtimescale;\n\
                 $printtimescale(top.rgval[0]);\n\
                 $printtimescale(top.rgarr[0]);\n\
                 $printtimescale(top.blk);\n\
               end\n\
             endmodule\n";
        let parsed = crate::parsers::source::parse_source(source).expect("design should parse");

        let mut simulator = Simulator::with_modules(parsed.modules, "top");
        simulator.setup().expect("design should run");

        assert_eq!(
            simulator.output().lines(),
            vec![
                "Time scale of (top) is 1s / 1s",
                "Time scale of (top.rgval[0:0]) is 1s / 1s",
                "Time scale of (top.rgarr[0]) is 1s / 1s",
                "Time scale of (top.blk) is 1s / 1s",
            ]
        );
    }

    /// IEEE 1364-2005's load direction, measured against iverilog 12.0 on a
    /// three-word file into memories declared *descending*:
    ///
    /// - with no range the load starts at the **lowest** address and runs
    ///   upward, so `down[0]` is the first word — 1364-2001 would have
    ///   started at `down[7]`;
    /// - with only a start it still runs upward from there;
    /// - with both bounds the design's direction wins: `5, 3` fills
    ///   `both[5]`, `both[4]`, `both[3]`.
    #[test]
    fn test_readmemh_follows_the_1364_2005_load_direction() {
        let directory = scratch_directory("readmem2005");
        fs::write(directory.join("three.hex"), "11 22 33\n").expect("data file should be writable");

        let (remaining, module) = parse_module_declaration(
            r#"
            module loader();
                reg [7:0] down [7:0];
                reg [7:0] from2 [7:0];
                reg [7:0] both [7:0];
                initial begin
                    $readmemh("three.hex", down);
                    $readmemh("three.hex", from2, 2);
                    $readmemh("three.hex", both, 5, 3);
                    $display("%h %h %h %h", down[0], down[1], down[2], down[7]);
                    $display("%h %h %h", from2[2], from2[3], from2[4]);
                    $display("%h %h %h", both[5], both[4], both[3]);
                end
            endmodule
        "#,
        )
        .expect("design should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);

        let mut simulator = Simulator::new(module);
        simulator.add_search_path(directory);
        simulator.setup().expect("design should run");

        assert_eq!(
            simulator.output().lines(),
            vec!["11 22 33 xx", "11 22 33", "11 22 33"]
        );
    }

    /// `$writememh` follows the same 1364-2005 rule as `$readmemh`: defaulted
    /// bounds write the lowest address first, a start with no finish writes
    /// upward, and two explicit bounds write in the direction given. iverilog
    /// 12.0 writes `00 11 22 33`, `11 22 33` and `33 22 11` for these three —
    /// after an address comment this does not emit, which a reader skips.
    #[test]
    fn test_writememh_follows_the_1364_2005_direction() {
        let directory = scratch_directory("writemem2005");
        let (remaining, module) = parse_module_declaration(
            r#"
            module writer();
                reg [7:0] down [3:0];
                integer k;
                initial begin
                    for (k = 0; k < 4; k = k + 1) down[k] = 8'h11 * k;
                    $writememh("defaults.hex", down);
                    $writememh("from1.hex", down, 1);
                    $writememh("both.hex", down, 3, 1);
                end
            endmodule
        "#,
        )
        .expect("design should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);

        let mut simulator = Simulator::new(module);
        simulator.set_output_directory(directory.clone());
        simulator.setup().expect("design should run");

        let written =
            |name: &str| fs::read_to_string(directory.join(name)).expect("file should be written");
        assert_eq!(written("defaults.hex"), "00\n11\n22\n33\n");
        assert_eq!(written("from1.hex"), "11\n22\n33\n");
        assert_eq!(written("both.hex"), "33\n22\n11\n");
    }

    #[test]
    fn test_readmemh_loads_a_memory_including_comments_and_an_address_jump() {
        let directory = scratch_directory("readmemh");
        fs::write(
            directory.join("words.hex"),
            "// the first three words\n0a 0b /* and one more */ 0c\n@4\nff\n",
        )
        .expect("data file should be writable");

        let (remaining, module) = parse_module_declaration(
            r#"
            module loader();
                reg [7:0] mem [0:7];
                initial begin
                    $readmemh("words.hex", mem);
                    $display("%0d %0d %0d %0d", mem[0], mem[1], mem[2], mem[4]);
                    $display("%h %h", mem[3], mem[5]);
                end
            endmodule
        "#,
        )
        .expect("design should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);

        let mut simulator = Simulator::new(module);
        simulator.add_search_path(directory);
        simulator.setup().expect("design should run");

        // The `@4` moved the load address, so word 3 was never written and
        // still reads `x` the way an untouched word does — and so does word 5,
        // which is past the end of the file. (`%h` renders per *digit*, so an
        // eight bit unknown is two unknown nibbles rather than one `x` padded
        // out to the field.)
        assert_eq!(simulator.output().lines(), vec!["10 11 12 255", "xx xx"]);
    }

    /// A data file that is not there is an error naming it — never an empty
    /// memory, which would leave the design reading `x` and looking exactly
    /// like one that simply ran.
    #[test]
    fn test_a_missing_memory_file_is_an_error_that_names_it() {
        let (_, module) = parse_module_declaration(
            r#"
            module missing();
                reg [7:0] mem [0:3];
                initial $readmemh("no-such-file.hex", mem);
            endmodule
        "#,
        )
        .expect("design should parse");

        let mut simulator = Simulator::new(module);
        let error = simulator.setup().expect_err("the file does not exist");
        let message = error.to_string();
        assert!(
            message.contains("no-such-file.hex") && message.contains("looked in"),
            "unexpected error: {}",
            message
        );
    }

    #[test]
    fn test_writememh_writes_what_readmemh_reads_back() {
        let directory = scratch_directory("writememh");
        let written = directory.join("out.hex");
        let source = format!(
            r#"
            module round_trip();
                reg [7:0] source [0:3];
                reg [7:0] copy [0:3];
                initial begin
                    source[0] = 8'h12;
                    source[1] = 8'h34;
                    source[2] = 8'h56;
                    source[3] = 8'h78;
                    $writememh("{}", source);
                    $readmemh("{}", copy);
                    $display("%h %h %h %h", copy[0], copy[1], copy[2], copy[3]);
                end
            endmodule
        "#,
            written.display(),
            written.display()
        );

        let (remaining, module) = parse_module_declaration(&source).expect("design should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        let mut simulator = Simulator::new(module);
        simulator.setup().expect("design should run");

        assert_eq!(simulator.output().lines(), vec!["12 34 56 78"]);
    }

    /// An edge is a property of the least significant bit of the *triggering
    /// expression*, so a sensitivity entry that names one bit of a vector has
    /// to be asked about that bit. `bus` moving `0000 -> 0010` is a `posedge`
    /// of `bus[1]` and no edge of `bus` at all — reading the whole vector's
    /// LSB answers for a different bit entirely. A generate loop writing one
    /// such block per bit is where it shows (corpus `pr1623097`).
    ///
    /// iverilog 12.0 prints `hit=1110` for this design: bit 0 never rises,
    /// bits 1 and 2 do, and bit 3 falls.
    #[test]
    fn test_a_sensitivity_list_can_name_one_bit_of_a_vector() {
        let mut simulator = simulator_for(
            r#"
            module selected();
                reg [3:0] bus;
                reg [3:0] hit;
                initial begin
                    bus = 4'b1000;
                    hit = 4'b0000;
                    #1 bus = 4'b0010;
                    #1 bus = 4'b0110;
                    #1 $display("hit=%b", hit);
                end
                always @(posedge bus[0]) hit[0] = 1'b1;
                always @(posedge bus[1]) hit[1] = 1'b1;
                always @(posedge bus[2]) hit[2] = 1'b1;
                always @(negedge bus[3]) hit[3] = 1'b1;
            endmodule
        "#,
        );
        simulator.advance(5).expect("time should advance");

        assert_eq!(simulator.output().lines(), vec!["hit=1110"]);
    }

    /// A sensitivity entry that is an *expression* has an edge of its own,
    /// which is not the edge of any one operand. At time zero `clock` goes
    /// `x -> 0` while `b` goes `x -> 1111`: `b`'s least significant bit rises,
    /// but `clock & b` goes `xxxx -> 0000`, which is a **negedge**. Matching
    /// the operands instead wakes the block there and puts every later count
    /// one out.
    ///
    /// iverilog 12.0 on this design:
    ///
    /// ```text
    /// 0 clock=0
    /// 10 clock=1
    /// 10 edge count=1
    /// 20 clock=2
    /// 30 clock=3
    /// 30 edge count=2
    /// ```
    #[test]
    fn test_a_posedge_of_an_expression_is_the_expressions_own_edge() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg [3:0] clock, b, count;
                initial begin
                    b = 4'b1111; count = 0;
                    for (clock = 0; clock <= 3; clock = clock + 1) begin
                        $display("%0t clock=%h", $time, clock);
                        #10;
                    end
                end
                always @(posedge clock & b) begin
                    count = count + 1;
                    $display("%0t edge count=%h", $time, count);
                end
            endmodule
        "#,
        );
        simulator.advance(100).expect("time should advance");

        assert_eq!(
            simulator.output().lines(),
            vec![
                "0 clock=0",
                "10 clock=1",
                "10 edge count=1",
                "20 clock=2",
                "30 clock=3",
                "30 edge count=2",
            ]
        );
    }

    /// An `always` block is sensitive only while it is *parked* at its event
    /// control, so a write it makes on its way to the end cannot wake it: the
    /// block was not listening when the event happened, and by the time it
    /// comes back the event is in the past. A block that toggles a signal its
    /// own sensitivity list names therefore runs once per *external* change
    /// rather than for ever.
    ///
    /// iverilog 12.0 prints `runs=2 a=0`: once for the two writes at time
    /// zero, once for the `c` at time ten, and not again for either of its own
    /// two writes to `a`.
    #[test]
    fn test_a_block_is_not_woken_by_its_own_write() {
        let mut simulator = simulator_for(
            r#"
            module feedback();
                reg a, c;
                integer runs;
                initial begin
                    runs = 0;
                    a = 1'b0;
                    c = 1'b0;
                    #10 c = 1'b1;
                    #10 $display("runs=%0d a=%b", runs, a);
                end
                always @(a or c) begin
                    runs = runs + 1;
                    a = ~a;
                end
            endmodule
        "#,
        );
        simulator.advance(30).expect("time should advance");

        assert_eq!(simulator.output().lines(), vec!["runs=2 a=0"]);
    }

    /// `%t` pads to twenty characters until `$timeformat` says otherwise, and
    /// then renders exactly what it was asked for — scaled from the tick this
    /// design counts to the unit it asked for. A design with no `` `timescale ``
    /// is at `1s / 1s`, so a tick is a second and `$timeformat(-9, …)` asks for
    /// it in nanoseconds. iverilog 12.0 prints the same three lines:
    ///
    /// ```text
    /// default[                   7]
    /// set[7000000000.00 ns]
    /// narrow[7000000000.00 ns]
    /// ```
    #[test]
    fn test_timeformat_configures_how_percent_t_renders() {
        let mut simulator = simulator_for(
            r#"
            module clocked();
                initial begin
                    #7 $display("default[%t]", $time);
                    $timeformat(-9, 2, " ns", 10);
                    $display("set[%t]", $time);
                    $display("narrow[%0t]", $time);
                end
            endmodule
        "#,
        );
        simulator.advance(10).expect("time should advance");

        assert_eq!(
            simulator.output().lines(),
            vec![
                "default[                   7]",
                "set[7000000000.00 ns]",
                "narrow[7000000000.00 ns]",
            ]
        );
    }

    /// `%t` scales from the tick the clock counts — the design's
    /// `` `timescale `` unit — to the unit `$timeformat` names, and a design
    /// that never called `$timeformat` prints in the finest *precision* its
    /// directives declared. Measured against iverilog 12.0 for
    /// `` `timescale 1ns/100ps ``, which prints:
    ///
    /// ```text
    /// default[                  50]
    /// us[   0.005000 us]
    /// ps[5000 ps]
    /// ```
    #[test]
    fn test_percent_t_scales_by_the_designs_timescale() {
        let source = r#"
            module scaled();
                initial begin
                    #5 $display("default[%t]", $time);
                    $timeformat(-6, 6, " us", 14);
                    $display("us[%t]", $time);
                    $timeformat(-12, 0, " ps", 0);
                    $display("ps[%0t]", $time);
                end
            endmodule
        "#;
        let (remaining, module) = parse_module_declaration(source).expect("design should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        let mut simulator = Simulator::new(module);
        simulator.set_timescale(Some(
            Timescale::parse("1ns/100ps").expect("a legal timescale"),
        ));
        simulator.setup().expect("design should elaborate");
        simulator.advance(10).expect("time should advance");

        assert_eq!(
            simulator.output().lines(),
            vec![
                "default[                  50]",
                "us[   0.005000 us]",
                "ps[5000 ps]",
            ]
        );
    }

    /// The error a design stops elaborating with, for the constructs the
    /// simulator rejects by name.
    fn setup_error(source: &str) -> SimulationError {
        let (remaining, module) = parse_module_declaration(source).expect("design should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        let mut simulator = Simulator::new(module);
        simulator
            .setup()
            .expect_err("setup should have been rejected")
    }

    /// A `time` is sixty-four bits wide and unsigned, which is what makes it
    /// different from an `integer`: it holds a value no thirty-two bit variable
    /// could, and an array of them indexes like any other memory.
    #[test]
    fn test_time_holds_sixty_four_bits() {
        let mut simulator = simulator_for(
            r#"
            module timed();
                time stamp;
                time marks [0:1];
                initial begin
                    stamp = 64'hDEADBEEFCAFEBABE;
                    marks[1] = 64'h100000000;
                    stamp = marks[1] + 1;
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.get("stamp").expect("declared").width(), 64);
        simulator.advance(1).expect("time should advance");
        assert_eq!(
            simulator.get("stamp").expect("declared").to_u128(),
            Some(0x1_0000_0001)
        );
    }

    /// A `time` variable round-trips a value through a simulation: it records
    /// the moment the statement that wrote it ran.
    #[test]
    fn test_time_records_the_moment_a_block_ran() {
        let mut simulator = simulator_for(
            r#"
            module timed();
                time stamp;
                initial #7 stamp = $time;
            endmodule
        "#,
        );

        simulator.advance(10).expect("time should advance");
        assert_eq!(simulator.get("stamp").expect("declared").to_u128(), Some(7));
    }

    /// A named event wakes a block waiting on it exactly once per trigger.
    ///
    /// The count is the point: a trigger is momentary, so it has to wake the
    /// block on the round that takes it and on no round after — a value left
    /// standing in the store would wake it again on every delta cycle.
    #[test]
    fn test_event_wakes_a_block_once_per_trigger() {
        let mut simulator = simulator_for(
            r#"
            module signalled();
                event tick;
                integer count;
                initial begin
                    count = 0;
                    #1 -> tick;
                    #1 -> tick;
                    #1 -> tick;
                end
                always @(tick) count = count + 1;
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.get("count").expect("declared").to_u128(), Some(1));

        // Time passing without a trigger wakes nothing.
        simulator.advance(0).expect("time should advance");
        assert_eq!(simulator.get("count").expect("declared").to_u128(), Some(1));

        simulator.advance(10).expect("time should advance");
        assert_eq!(simulator.get("count").expect("declared").to_u128(), Some(3));
    }

    /// One trigger wakes every block waiting on the event, and each of them
    /// once.
    #[test]
    fn test_one_trigger_wakes_every_waiting_block() {
        let mut simulator = simulator_for(
            r#"
            module signalled();
                event tick;
                integer here;
                integer there;
                initial begin
                    here = 0;
                    there = 0;
                    #1 -> tick;
                end
                always @(tick) here = here + 1;
                always @(tick) there = there + 10;
            endmodule
        "#,
        );

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("here").expect("declared").to_u128(), Some(1));
        assert_eq!(
            simulator.get("there").expect("declared").to_u128(),
            Some(10)
        );
    }

    /// An event has no value, so reading one is an error naming it rather than
    /// a plausible pattern of bits.
    #[test]
    fn test_event_used_as_a_value_is_a_named_error() {
        let error = setup_error(
            r#"
            module signalled();
                event tick;
                reg [3:0] q;
                initial q = tick;
            endmodule
        "#,
        );

        assert_eq!(
            error.to_string(),
            "event `tick` has no value; it can only be triggered"
        );
    }

    /// A `real` starts at `0.0` rather than at `x`, which is a property of the
    /// type: a double has no unknown to hold. iverilog prints `0.000000` for
    /// an untouched one.
    #[test]
    fn test_an_untouched_real_reads_zero() {
        let mut simulator = simulator_for(
            r#"
            module floating();
                real r;
                realtime t;
                real samples [0:1];
                initial $display("%f %f %f", r, t, samples[1]);
            endmodule
        "#,
        );
        simulator.advance(1).expect("design should run");
        assert_eq!(
            simulator.output().lines(),
            vec!["0.000000 0.000000 0.000000"]
        );
    }

    /// A real round-trips through the store, through an array of reals, and
    /// through the conversions in both directions. Every value here was
    /// measured from iverilog 12.0.
    #[test]
    fn test_reals_round_trip_and_convert() {
        let mut simulator = simulator_for(
            r#"
            module floating();
                real r;
                integer i;
                reg [7:0] a;
                real samples [0:3];
                initial begin
                    r = 1.5 + 0.5;
                    $display("%f", r);
                    i = 2.5;
                    $display("%0d", i);
                    i = -1.5;
                    $display("%0d", i);
                    a = 8'hFF;
                    r = a;
                    $display("%f", r);
                    r = 7 / 2;
                    $display("%f", r);
                    r = 7 / 2.0;
                    $display("%f", r);
                    samples[2] = 0.25;
                    $display("%g %g", samples[2], samples[3]);
                    $display("%0d %f", $rtoi(2.7), $itor(5));
                end
            endmodule
        "#,
        );
        simulator.advance(1).expect("design should run");
        assert_eq!(
            simulator.output().lines(),
            vec![
                "2.000000",
                "3",
                "-2",
                "255.000000",
                "3.000000",
                "3.500000",
                "0.25 0",
                "2 5.000000",
            ]
        );
    }
    /// `%f`, `%e` and `%g` are C's, and an argument with no specifier at all
    /// is C's `%#g` — six significant digits with the trailing zeros kept,
    /// which is why `1.5` prints as `1.50000`. Every line here was measured
    /// from iverilog 12.0.
    #[test]
    fn test_real_format_specifiers() {
        let mut simulator = simulator_for(
            r#"
            module printing();
                initial begin
                    $display("%f", 1.5 + 0.5);
                    $display("%g", 1.5 * 0.5);
                    $display("%e", 1.5 / 0.5);
                    $display("%f", 1e3);
                    $display("%g", 1.5e-3);
                    $display("[%0f]", 2.5);
                    $display("[%5.2f]", 2.5);
                    $display("%f", 3);
                    $display("%0d", 2.6);
                    $display("%h %b %o", 2.6, 2.6, 2.6);
                    $display(1.5, " ", 400.0, " ", 1e10, " ", 0.0015);
                    $display("%f", 1.0 / 0.0);
                end
            endmodule
        "#,
        );
        simulator.advance(1).expect("design should run");
        assert_eq!(
            simulator.output().lines(),
            vec![
                "2.000000",
                "0.75",
                "3.000000e+00",
                "1000.000000",
                "0.0015",
                "[2.500000]",
                "[ 2.50]",
                "3.000000",
                "3",
                "3 11 3",
                "1.50000 400.000 1.00000e+10 0.00150000",
                "inf",
            ]
        );
    }

    /// A `real` reaches every place a value can be declared: a parameter, a
    /// function's result and its argument, and a `specparam`.
    #[test]
    fn test_reals_are_declared_everywhere_a_value_is() {
        let mut simulator = simulator_for(
            r#"
            module scopes();
                parameter real PI = 3.14;
                parameter real WHOLE = 3;
                real r;
                function real half;
                    input real x;
                    half = x / 2.0;
                endfunction
                initial begin
                    r = half(5.0);
                    $display("%f %f %f", PI, WHOLE, r);
                end
            endmodule
        "#,
        );
        simulator.advance(1).expect("design should run");
        assert_eq!(
            simulator.output().lines(),
            vec!["3.140000 3.000000 2.500000"]
        );
    }

    /// An operator that reads a *pattern of bits* has no meaning for a real,
    /// and iverilog rejects each of these at compile time. There is no
    /// compile-time diagnostic channel here, so they are named errors from the
    /// evaluator instead — never a silent conversion to an integer.
    #[test]
    fn test_bitwise_operators_refuse_a_real() {
        for (source, operator) in [
            ("r & 3", "&"),
            ("r | 3", "|"),
            ("r ^ 3", "^"),
            ("r >> 1", ">>"),
            ("r << 1", "<<"),
            ("r === 1.5", "==="),
            ("~r", "~"),
            ("&r", "&"),
        ] {
            let source = format!(
                r#"
                module refusing();
                    real r;
                    reg [7:0] q;
                    initial begin
                        r = 5.5;
                        q = {};
                    end
                endmodule
            "#,
                source
            );
            let (remaining, module) =
                parse_module_declaration(&source).expect("design should parse");
            assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
            let mut simulator = Simulator::new(module);
            let error = match simulator.setup() {
                Err(error) => error,
                Ok(_) => simulator
                    .advance(1)
                    .expect_err("the operator has no meaning for a real"),
            };
            assert_eq!(
                error.to_string(),
                format!("operator `{}` cannot take a real operand", operator),
                "{}",
                source
            );
        }
    }

    /// `%m` is the hierarchical name of the scope the call sits in: the top
    /// module, then the instance path, then the named blocks and the task. It
    /// takes no argument — the scope is settled when the design is elaborated
    /// and is a constant from then on. Every line here was measured from
    /// iverilog 12.0.
    #[test]
    fn test_the_scope_format_names_the_hierarchy() {
        let modules = crate::parsers::source::parse_verilog_source(
            r#"
            module sub;
                initial begin
                    $display("in sub: %m");
                    t;
                end
                task t;
                    $display("in task: %m");
                endtask
            endmodule
            module top;
                sub s();
                initial begin
                    $display("top: %m");
                    begin : blk
                        $display("block: %m");
                    end
                end
            endmodule
        "#,
        )
        .expect("should parse")
        .1;
        let mut simulator = Simulator::with_modules(modules, "top");
        simulator.setup().expect("design should elaborate");
        simulator.advance(1).expect("design should run");
        assert_eq!(
            simulator.output().lines(),
            vec![
                "in sub: top.s",
                "in task: top.s.t",
                "top: top",
                "block: top.blk"
            ]
        );
    }

    /// A procedural `assign` gives a variable a second source, and it is the
    /// one that wins: an ordinary write while it is installed goes nowhere.
    /// `deassign` takes it away and hands the variable back.
    #[test]
    fn test_a_procedural_assign_overrides_writes_until_it_is_deassigned() {
        let mut simulator = simulator_for(
            r#"
            module driven();
                reg v, source;
                initial begin
                    source = 1'b1;
                    v = 1'b0;
                    assign v = source;
                    #5 v = 1'b0;
                    #5 deassign v;
                    #5 v = 1'b0;
                end
            endmodule
        "#,
        );

        // The drive lands the moment the statement runs.
        assert_eq!(simulator.get("v").unwrap().to_binary(), "1");

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("v").unwrap().to_binary(), "1");

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("v").unwrap().to_binary(), "1");

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("v").unwrap().to_binary(), "0");
    }

    /// A procedural `assign` takes a **concatenation**, and it holds every
    /// signal the concatenation names.
    ///
    /// iverilog 12.0 prints `0011`, `0010`, `1001` for this design: the write
    /// of `4'h5` goes nowhere while the drive is installed, and the write of
    /// `4'h9` lands once `deassign` has taken it off. Corpus `assign3.2D` and
    /// `assign3.2E` are the same shape around a clock.
    #[test]
    fn test_a_procedural_assign_holds_every_part_of_a_concatenation() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg a, b, c, d;
                initial begin
                    {a, b, c, d} = 4'h3;
                    $display("%b%b%b%b", a, b, c, d);
                    assign {a, b, c, d} = 4'h2;
                    {a, b, c, d} = 4'h5;
                    $display("%b%b%b%b", a, b, c, d);
                    deassign {a, b, c, d};
                    {a, b, c, d} = 4'h9;
                    $display("%b%b%b%b", a, b, c, d);
                end
            endmodule
        "#,
        );
        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "0011\n0010\n1001\n");
    }

    /// A concatenation is an assignment target inside a `function` body too,
    /// including one over the function's own result variable.
    ///
    /// iverilog 12.0 prints `a5` for this design, which is corpus
    /// `constfunc14`'s `concat1` in miniature. It used to be a named
    /// `UnsupportedTarget` at elaboration, because the body walk asked each
    /// target for the *one* signal it wrote.
    #[test]
    fn test_a_function_body_assigns_through_a_concatenation() {
        let mut simulator = simulator_for(
            r#"
            module main();
                function [7:0] swap(input [7:0] v);
                    reg [3:0] hi, lo;
                    begin
                        {hi, lo} = v;
                        {swap[3:0], swap[7:4]} = {hi, lo};
                    end
                endfunction
                initial $display("%h", swap(8'h5a));
            endmodule
        "#,
        );
        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "a5\n");
    }

    /// `force` is stronger than a procedural `assign`, and `release` hands the
    /// variable back to it rather than to whatever the force left behind.
    #[test]
    fn test_force_overrides_a_procedural_assign_and_release_falls_back_to_it() {
        let mut simulator = simulator_for(
            r#"
            module forced();
                reg v, a, b;
                initial begin
                    a = 1'b0;
                    b = 1'b1;
                    assign v = a;
                    #5 force v = b;
                    #5 release v;
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.get("v").unwrap().to_binary(), "0");

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("v").unwrap().to_binary(), "1");

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("v").unwrap().to_binary(), "0");
    }

    /// A `release` puts nothing back. A **variable** has no driver, so it
    /// keeps the value the force left it holding.
    ///
    /// iverilog 12.0 prints `1` for this design after the release, not the
    /// `0` the variable held before the force.
    #[test]
    fn test_release_leaves_a_variable_holding_the_forced_value() {
        let mut simulator = simulator_for(
            r#"
            module released();
                reg v, b;
                initial begin
                    v = 1'b0;
                    b = 1'b1;
                    #5 force v = b;
                    #5 v = 1'b1;
                    #5 release v;
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.get("v").unwrap().to_binary(), "0");

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("v").unwrap().to_binary(), "1");

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("v").unwrap().to_binary(), "1");

        // The release changes nothing: `v` is a variable, so it holds `1`.
        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("v").unwrap().to_binary(), "1");
    }

    /// A **net**, unlike a variable, does revert when released — not because
    /// anything is put back, but because its continuous driver reaches it
    /// again on the next pass.
    #[test]
    fn test_release_lets_a_net_return_to_its_driver() {
        let mut simulator = simulator_for(
            r#"
            module released(input d);
                wire w;
                assign w = d;
                initial begin
                    #5 force w = 1'b0;
                    #5 release w;
                end
            endmodule
        "#,
        );

        simulator.poke("d", one()).unwrap();
        assert_eq!(simulator.get("w").unwrap().to_binary(), "1");

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("w").unwrap().to_binary(), "0");

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("w").unwrap().to_binary(), "1");
    }

    /// A net **nothing drives** floats when it is released. There is no
    /// driver to put a value back and a net has no value of its own, so
    /// leaving the forced one standing would claim something is driving it.
    ///
    /// iverilog 12.0, over a `wire [1:0]` forced to `10` and then released:
    ///
    /// ```text
    /// held  bare=10
    /// freed bare=zz
    /// ```
    #[test]
    fn test_release_floats_a_net_nothing_drives() {
        let mut simulator = simulator_for(
            r#"
            module released();
                wire [1:0] bare;
                initial begin
                    #5 force bare = 2'b10;
                    #5 release bare;
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.get("bare").unwrap().to_binary(), "zz");

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("bare").unwrap().to_binary(), "10");

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("bare").unwrap().to_binary(), "zz");
    }

    /// Drive precedence is per **bit**, not per signal name.
    ///
    /// iverilog 12.0, forcing `r[1]` of a `reg [3:0] r` that starts `0000`:
    /// `r = 4'b1100` gives `1110` (the write is masked, not refused),
    /// `r[0] = 1` gives `1111`, and `r[1] = 0` leaves `1111`.
    #[test]
    fn test_force_holds_only_the_bits_it_names() {
        let mut simulator = simulator_for(
            r#"
            module held();
                reg [3:0] r;
                initial begin
                    r = 4'b0000;
                    force r[1] = 1'b1;
                    #5 r = 4'b1100;
                    #5 r[0] = 1'b1;
                    #5 r[1] = 1'b0;
                end
            endmodule
        "#,
        );

        // A whole-signal write lands everywhere except the forced bit.
        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("r").unwrap().to_binary(), "1110");

        // An unforced bit takes its write.
        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("r").unwrap().to_binary(), "1111");

        // The forced bit does not.
        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("r").unwrap().to_binary(), "1111");
    }

    /// And per **word** as well, for an array. `force array[2] = …` says
    /// nothing at all about `array[1]`, so a word released while another word
    /// is still forced goes back to its driver.
    ///
    /// Measured against iverilog 12.0, which prints `forced 0 1 / f2 1 0 /
    /// f3 2 0` for the design below. Holding the array by its *name* leaves
    /// `array1[1]` stuck at `1` for the rest of the run, because the force on
    /// `array1[2]` makes every word of the array unwritable — the released
    /// word never reverts and the continuous assignment beneath it never gets
    /// through (corpus `array_lval_select4a`).
    #[test]
    fn test_a_force_on_one_memory_word_holds_only_that_word() {
        let mut simulator = simulator_for(
            r#"
            module top();
                wire [1:0] array1[2:1];
                reg [1:0] var1;
                assign array1[1] = 2'd0;
                assign array1[2] = 2'd0;
                initial begin
                    force array1[1] = 2'd1;
                    #1 $display("forced   %h %h", array1[2], array1[1]);
                    release array1[1];
                    force array1[2] = var1;
                    var1 = 2'd1;
                    #1 $display("f2       %h %h", array1[2], array1[1]);
                    var1 = 2'd2;
                    #1 $display("f3       %h %h", array1[2], array1[1]);
                end
            endmodule
        "#,
        );

        simulator.advance(10).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "forced   0 1\nf2       1 0\nf3       2 0\n"
        );
    }

    /// Two forces may hold different parts of one vector at once, and a
    /// `release` of one leaves the other standing.
    ///
    /// A drive used to be recorded per *signal name*, so `force bus[3:2]`
    /// evicted `force bus[0]` and `release bus[0]` then took the `[3:2]` force
    /// away with it — both silently. iverilog 12.0 prints
    /// `0001 1101 110z 110z` for this design (corpus `force_release_reg_pv`,
    /// `force_release_wire_pv`, `force_release_wire8_pv`,
    /// `assign_deassign_pv`).
    #[test]
    fn test_two_forces_hold_separate_parts_of_one_vector() {
        let mut simulator = simulator_for(
            r#"
            module held();
                reg [3:0] bus;
                initial begin
                    bus = 4'b0000;
                    force bus[0] = 1'b1;
                    #5 force bus[3:2] = 2'b11;
                    #5 release bus[0];
                    bus = 4'b000z;
                    #5 release bus[3:2];
                end
            endmodule
        "#,
        );

        // The first force alone.
        simulator.advance(3).expect("time should advance");
        assert_eq!(simulator.get("bus").unwrap().to_binary(), "0001");

        // The second force does not displace the first.
        simulator.advance(3).expect("time should advance");
        assert_eq!(simulator.get("bus").unwrap().to_binary(), "1101");

        // Releasing bit 0 leaves `[3:2]` held, so the whole-signal write that
        // follows it is masked rather than landing everywhere.
        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("bus").unwrap().to_binary(), "110z");

        // A released `reg` keeps what the force left it holding — nothing is
        // put back — so the `11` stays even once the force is gone.
        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("bus").unwrap().to_binary(), "110z");
    }

    /// A force is a *continuous* drive: it is re-evaluated whenever an operand
    /// of its right hand side moves, not once when the statement ran.
    #[test]
    fn test_a_forced_signal_follows_its_expression() {
        let mut simulator = simulator_for(
            r#"
            module following();
                reg v, a;
                initial begin
                    a = 1'b0;
                    force v = ~a;
                    #5 a = 1'b1;
                end
            endmodule
        "#,
        );

        assert_eq!(simulator.get("v").unwrap().to_binary(), "1");

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("v").unwrap().to_binary(), "0");
    }

    /// A write to a forced signal is discarded rather than applied and then
    /// overwritten — so it never reaches the store's journal and never wakes a
    /// block. The same write after the `release` does wake one, which is what
    /// says the counter was capable of moving all along.
    #[test]
    fn test_a_write_to_a_forced_signal_wakes_nothing() {
        let mut simulator = simulator_for(
            r#"
            module quiet();
                reg v, other;
                reg [7:0] wakes;
                initial begin
                    wakes = 8'd0;
                    v = 1'b0;
                    other = 1'b0;
                    force v = 1'b0;
                    #5 other = 1'b1;
                    #5 release v;
                    #5 other = 1'b0;
                end
                always @(other) v = 1'b1;
                always @(v) wakes = wakes + 8'd1;
            endmodule
        "#,
        );

        // Declaring `v` and writing it `0` is itself an edge, so the counter
        // starts at one.
        assert_eq!(number(&simulator, "wakes"), 1);

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("v").unwrap().to_binary(), "0");
        assert_eq!(number(&simulator, "wakes"), 1);

        // The release puts back the same `0`, so it is not an edge either.
        simulator.advance(5).expect("time should advance");
        assert_eq!(number(&simulator, "wakes"), 1);

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("v").unwrap().to_binary(), "1");
        assert_eq!(number(&simulator, "wakes"), 2);
    }

    /// A gate is a continuous driver, so instantiating one settles its output
    /// the way an `assign` settles a net.
    #[test]
    fn test_gate_primitives_drive_their_outputs() {
        let mut simulator = simulator_for(
            r#"
            module gate_logic();
                reg a, b;
                wire w_and, w_or, w_xor, w_nand, w_nor, w_xnor, w_buf, w_not;
                and  (w_and, a, b);
                or   (w_or, a, b);
                xor  (w_xor, a, b);
                nand (w_nand, a, b);
                nor  (w_nor, a, b);
                xnor (w_xnor, a, b);
                buf  (w_buf, a);
                not  (w_not, a);
                initial begin
                    a = 1'b1;
                    b = 1'b0;
                end
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();

        assert_eq!(level(&simulator, "w_and"), "0");
        assert_eq!(level(&simulator, "w_or"), "1");
        assert_eq!(level(&simulator, "w_xor"), "1");
        assert_eq!(level(&simulator, "w_nand"), "1");
        assert_eq!(level(&simulator, "w_nor"), "0");
        assert_eq!(level(&simulator, "w_xnor"), "0");
        assert_eq!(level(&simulator, "w_buf"), "1");
        assert_eq!(level(&simulator, "w_not"), "0");
    }

    /// The four-state half of a gate, which is the half a two-state simulator
    /// gets wrong: an unknown input that cannot change the answer does not make
    /// the answer unknown. Every value here is `iverilog`'s.
    #[test]
    fn test_gates_propagate_x_and_z() {
        let mut simulator = simulator_for(
            r#"
            module gate_unknowns();
                reg a, b, c;
                wire and_0x, and_1x, or_1x, or_0z, xor_0x, buf_z, not_z;
                and (and_0x, a, b);
                and (and_1x, c, b);
                or  (or_1x, c, b);
                or  (or_0z, a, a);
                xor (xor_0x, a, b);
                buf (buf_z, a);
                not (not_z, a);
                initial begin
                    a = 1'bz;
                    b = 1'bx;
                    c = 1'b1;
                end
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();

        // A `z` reaches a logic gate as an `x`, so `a` behaves as unknown.
        assert_eq!(level(&simulator, "and_0x"), "x");
        assert_eq!(level(&simulator, "and_1x"), "x");
        assert_eq!(level(&simulator, "or_1x"), "1");
        assert_eq!(level(&simulator, "or_0z"), "x");
        assert_eq!(level(&simulator, "xor_0x"), "x");
        assert_eq!(level(&simulator, "buf_z"), "x");
        assert_eq!(level(&simulator, "not_z"), "x");
    }

    /// The dominance rule, which is the whole point of a four-state table:
    /// a `0` into an `and` and a `1` into an `or` decide the answer whatever
    /// the other input is.
    #[test]
    fn test_a_known_input_can_decide_a_gate() {
        let mut simulator = simulator_for(
            r#"
            module gate_dominance();
                reg unknown, floating;
                wire and_out, or_out;
                and (and_out, 1'b0, unknown);
                or  (or_out, 1'b1, floating);
                initial begin
                    unknown = 1'bx;
                    floating = 1'bz;
                end
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();

        assert_eq!(level(&simulator, "and_out"), "0");
        assert_eq!(level(&simulator, "or_out"), "1");
    }

    /// A three-state buffer drives `z` when it is disabled, and `x` when it
    /// cannot tell whether it is enabled.
    #[test]
    fn test_three_state_buffers() {
        let mut simulator = simulator_for(
            r#"
            module three_state();
                reg data, enable, unknown;
                wire b1, b0, n1, n0, unsure;
                bufif1 (b1, data, enable);
                bufif0 (b0, data, enable);
                notif1 (n1, data, enable);
                notif0 (n0, data, enable);
                bufif1 (unsure, data, unknown);
                initial begin
                    data = 1'b1;
                    enable = 1'b0;
                    unknown = 1'bx;
                end
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();

        assert_eq!(level(&simulator, "b1"), "z");
        assert_eq!(level(&simulator, "b0"), "1");
        assert_eq!(level(&simulator, "n1"), "z");
        assert_eq!(level(&simulator, "n0"), "0");
        assert_eq!(level(&simulator, "unsure"), "x");
    }

    /// Two drivers on one net, which is what a gate makes possible and an
    /// `assign` never did. Whichever wrote last would be exactly the wrong
    /// answer; the net is resolved between them instead.
    #[test]
    fn test_a_three_state_bus_resolves_between_its_drivers() {
        let mut simulator = simulator_for(
            r#"
            module bus_resolution();
                reg e0, e1;
                wire bus;
                bufif1 (bus, 1'b0, e0);
                bufif1 (bus, 1'b1, e1);
                initial begin
                    e0 = 1'b0;
                    e1 = 1'b0;
                    #1 e0 = 1'b1;
                    #1 e0 = 1'b0;
                    #1 e1 = 1'b1;
                    #1 e0 = 1'b1;
                end
            endmodule
        "#,
        );

        // Both drivers off: the bus floats.
        simulator.advance(0).unwrap();
        assert_eq!(level(&simulator, "bus"), "z");
        // One driver each, then both at once, which the net cannot settle.
        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "bus"), "0");
        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "bus"), "z");
        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "bus"), "1");
        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "bus"), "x");
    }

    /// A `pullup` is a *weak* driver: it holds a net nothing else is driving
    /// and loses to anything that is.
    #[test]
    fn test_a_pull_source_loses_to_a_real_driver() {
        let mut simulator = simulator_for(
            r#"
            module pulls();
                reg enable;
                wire up, down;
                pullup (up);
                pulldown (down);
                bufif1 (up, 1'b0, enable);
                bufif1 (down, 1'b1, enable);
                initial begin
                    enable = 1'b0;
                    #1 enable = 1'b1;
                end
            endmodule
        "#,
        );

        simulator.advance(0).unwrap();
        assert_eq!(level(&simulator, "up"), "1");
        assert_eq!(level(&simulator, "down"), "0");

        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "up"), "0");
        assert_eq!(level(&simulator, "down"), "1");
    }

    /// A pull also loses to a continuous assignment, which reaches the same
    /// resolution the gates do rather than a write of its own.
    #[test]
    fn test_a_pull_source_resolves_against_an_assignment() {
        let mut simulator = simulator_for(
            r#"
            module pull_and_assign();
                reg drive;
                wire net;
                pullup (net);
                assign net = drive ? 1'b0 : 1'bz;
                initial begin
                    drive = 1'b0;
                    #1 drive = 1'b1;
                end
            endmodule
        "#,
        );

        simulator.advance(0).unwrap();
        assert_eq!(level(&simulator, "net"), "1");

        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "net"), "0");
    }

    /// An open drain: `(highz0, strong1)` drives its `1` and floats instead of
    /// driving its `0`, which is what leaves the pull in charge.
    #[test]
    fn test_a_drive_strength_can_float_one_half() {
        let mut simulator = simulator_for(
            r#"
            module open_drain();
                reg data;
                wire net;
                pulldown (net);
                buf (highz0, strong1) (net, data);
                initial begin
                    data = 1'b1;
                    #1 data = 1'b0;
                end
            endmodule
        "#,
        );

        simulator.advance(0).unwrap();
        assert_eq!(level(&simulator, "net"), "1");

        // Driving a `0` through a `highz0` half drives nothing at all, so the
        // `pulldown` is what the net reads.
        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "net"), "0");
    }

    /// A gate wakes an `always` block exactly as an `assign` does, because it
    /// settles in the same fixpoint and its writes are journalled the same way.
    #[test]
    fn test_a_gate_output_wakes_a_sensitive_block() {
        let mut simulator = simulator_for(
            r#"
            module gate_edges();
                reg a, b;
                reg [7:0] wakes;
                wire y;
                and (y, a, b);
                always @(posedge y) wakes = wakes + 1;
                initial begin
                    wakes = 0;
                    a = 1'b0;
                    b = 1'b1;
                    #1 a = 1'b1;
                    #1 a = 1'b0;
                    #1 a = 1'b1;
                end
            endmodule
        "#,
        );
        simulator.advance(4).unwrap();

        assert_eq!(number(&simulator, "wakes"), 2);
    }

    /// An array of instances is expanded at elaboration: a terminal as wide as
    /// the array is sliced a bit per instance, and a scalar reaches all of them.
    #[test]
    fn test_an_array_of_gate_instances() {
        let mut simulator = simulator_for(
            r#"
            module gate_array();
                reg [3:0] data;
                reg enable;
                wire [3:0] bus;
                pullup pu [3:0] (bus);
                bufif1 drv [3:0] (bus, data, enable);
                initial begin
                    data = 4'b1010;
                    enable = 1'b0;
                    #1 enable = 1'b1;
                end
            endmodule
        "#,
        );

        // Every bit floats, so every pullup holds its own.
        simulator.advance(0).unwrap();
        assert_eq!(level(&simulator, "bus"), "1111");

        // Enabled, the buffers drive each bit of `data` onto its own bit.
        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "bus"), "1010");
    }

    /// An array's bounds are an ordinary declared range, so a parameter sizes
    /// the array exactly as it sizes the net the array drives.
    #[test]
    fn test_an_array_of_gate_instances_sized_by_a_parameter() {
        let mut simulator = simulator_for(
            r#"
            module parameterised_array();
                parameter N = 4;
                reg [N-1:0] data;
                reg enable;
                wire [N-1:0] bus;
                bufif1 drv [N-1:0] (bus, data, enable);
                initial begin
                    data = 4'b0110;
                    enable = 1'b1;
                end
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();

        assert_eq!(level(&simulator, "bus"), "0110");
    }

    /// Which terminals a gate drives is decided by its kind, so `buf` with
    /// three terminals drives two nets from one input.
    #[test]
    fn test_a_buffer_may_drive_several_outputs() {
        let mut simulator = simulator_for(
            r#"
            module fan_out();
                reg in;
                wire o1, o2, o3;
                buf (o1, o2, in);
                not (o3, in);
                initial in = 1'b1;
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();

        assert_eq!(level(&simulator, "o1"), "1");
        assert_eq!(level(&simulator, "o2"), "1");
        assert_eq!(level(&simulator, "o3"), "0");
    }

    /// A `tran` makes its two terminals one node: `q` follows `p`'s driver,
    /// and when that driver lets go **both** float rather than `q` latching the
    /// value it was handed.
    ///
    /// Measured against iverilog 12.0, which prints `driven p=1 q=1` then
    /// `released p=z q=z` for this design. The latch is exactly what a `tran`
    /// modelled as a copy between its terminals would produce, and it is why
    /// one is a node instead.
    #[test]
    fn test_a_tran_makes_one_node_that_floats_together() {
        let mut simulator = simulator_for(
            r#"
            module joined();
                reg a, en;
                wire p, q;
                bufif1 b1(p, a, en);
                tran t1(p, q);
                initial begin
                    a = 1'b1;
                    en = 1'b1;
                    #2 en = 1'b0;
                end
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "p"), "1");
        assert_eq!(level(&simulator, "q"), "1");

        simulator.advance(2).unwrap();
        assert_eq!(level(&simulator, "p"), "z");
        assert_eq!(level(&simulator, "q"), "z");
    }

    /// A `tranif1` joins its terminals only while the control says so, which is
    /// why connectivity is recomputed every propagation pass rather than fixed
    /// at elaboration.
    ///
    /// iverilog 12.0 on this design prints `en=1 p=1 q=1`, `en=0 p=1 q=z` and
    /// `en=x p=1 q=x`. The unknown control is the one departure: this takes the
    /// switch not to conduct, so `q` is `z` where iverilog gives `x` — saying
    /// "unknown" there needs a strength that is a range rather than a level.
    #[test]
    fn test_a_tranif_joins_only_while_its_control_says_so() {
        let mut simulator = simulator_for(
            r#"
            module gated();
                reg a, en;
                wire p, q;
                assign p = a;
                tranif1 t1(p, q, en);
                initial begin
                    a = 1'b1;
                    en = 1'b1;
                    #2 en = 1'b0;
                    #2 en = 1'b1;
                    #2 en = 1'bx;
                end
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "q"), "1");

        simulator.advance(2).unwrap();
        assert_eq!(level(&simulator, "p"), "1");
        assert_eq!(level(&simulator, "q"), "z");

        simulator.advance(2).unwrap();
        assert_eq!(level(&simulator, "q"), "1");

        simulator.advance(2).unwrap();
        assert_eq!(level(&simulator, "p"), "1");
        assert_eq!(level(&simulator, "q"), "z");
    }

    /// Two drivers that meet through a `tran` resolve against each other, since
    /// the switch pools them into one driver list rather than copying either
    /// one across.
    ///
    /// iverilog 12.0 on this design prints `disagree p=x q=x`, `one side z p=1
    /// q=1` and `both z p=z q=z`.
    #[test]
    fn test_drivers_meeting_through_a_tran_resolve_against_each_other() {
        let mut simulator = simulator_for(
            r#"
            module contended();
                reg a, b;
                wire p, q;
                assign p = a;
                assign q = b;
                tran t1(p, q);
                initial begin
                    a = 1'b1;
                    b = 1'b0;
                    #2 b = 1'bz;
                    #2 a = 1'bz;
                end
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "p"), "x");
        assert_eq!(level(&simulator, "q"), "x");

        simulator.advance(2).unwrap();
        assert_eq!(level(&simulator, "p"), "1");
        assert_eq!(level(&simulator, "q"), "1");

        simulator.advance(2).unwrap();
        assert_eq!(level(&simulator, "p"), "z");
        assert_eq!(level(&simulator, "q"), "z");
    }

    /// A node is a set of *bits*, not of nets: `tran (a[0], a[1]);` joins two
    /// bits of one vector, which is corpus `pr3296466a`. iverilog 12.0 prints
    /// `11` for it.
    #[test]
    fn test_a_tran_can_join_two_bits_of_one_net() {
        let mut simulator = simulator_for(
            r#"
            module within();
                reg foo;
                tri [1:0] a;
                assign a[0] = foo;
                tran t1(a[0], a[1]);
                initial begin
                    foo = 1'b1;
                    #2 foo = 1'b0;
                end
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "a"), "11");

        simulator.advance(2).unwrap();
        assert_eq!(level(&simulator, "a"), "00");
    }

    /// A weak keeper: a switch gated on the very net it holds up, which is the
    /// case a connectivity partition built once at elaboration cannot express.
    /// This is corpus `tran-keeper`, and iverilog 12.0 prints `PASSED`.
    #[test]
    fn test_a_switch_gated_on_its_own_net_keeps_that_net() {
        let mut simulator = simulator_for(
            r#"
            module keeper();
                wire pin;
                pullup   (weak1) (keep1);
                pulldown (weak0) (keep0);
                tranif1 (pin, keep1, pin);
                tranif0 (pin, keep0, pin);
                reg value, enable;
                bufif1 (pin, value, enable);
                initial begin
                    value = 1'b0;
                    enable = 1'b1;
                    #2 enable = 1'b0;
                    #2 value = 1'b1;
                       enable = 1'b1;
                    #2 enable = 1'b0;
                end
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "pin"), "0");
        // The drive is removed; the keeper holds the pin where it was.
        simulator.advance(2).unwrap();
        assert_eq!(level(&simulator, "pin"), "0");

        simulator.advance(2).unwrap();
        assert_eq!(level(&simulator, "pin"), "1");
        simulator.advance(2).unwrap();
        assert_eq!(level(&simulator, "pin"), "1");
    }

    /// A `force` on a net in a node is a **driver** of that node, not just a
    /// write to the net it names: it wins outright on its own net and contends
    /// with the far side's drivers through the switch.
    ///
    /// iverilog 12.0 on this design prints `x=0 y=x` — `x` reads the value it
    /// was forced to, while `y`, driven to `1` by its own assignment, is the
    /// contention between that and the forced `0`. This is corpus
    /// `pr2937417b`.
    #[test]
    fn test_a_force_on_a_node_contends_through_the_switch() {
        let mut simulator = simulator_for(
            r#"
            module forced_node();
                reg a;
                wire x, y;
                assign y = a;
                tran (x, y);
                initial begin
                    a = 1'b1;
                    #2 force x = 1'b0;
                    #2 release x;
                end
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "x"), "1");
        assert_eq!(level(&simulator, "y"), "1");

        simulator.advance(2).unwrap();
        assert_eq!(level(&simulator, "x"), "0");
        assert_eq!(level(&simulator, "y"), "x");

        simulator.advance(2).unwrap();
        assert_eq!(level(&simulator, "x"), "1");
        assert_eq!(level(&simulator, "y"), "1");
    }

    /// An arrayed switch may be wired to a *part* of a net, which is how
    /// corpus `pr3296466d` joins `a` to the bottom half of `c`. The slice is
    /// taken from the part select's own bounds rather than the net's, and both
    /// are walked from their least significant end.
    #[test]
    fn test_an_arrayed_switch_slices_a_part_select_terminal() {
        let mut simulator = simulator_for(
            r#"
            module halves();
                reg foo;
                tri [1:0] a;
                tri [3:0] c;
                assign a[0] = foo;
                tran t1(a[0], a[1]);
                tran t2[1:0](a, c[1:0]);
                initial foo = 1'b1;
            endmodule
        "#,
        );
        simulator.advance(1).unwrap();
        assert_eq!(level(&simulator, "a"), "11");
        assert_eq!(level(&simulator, "c"), "zz11");
    }

    /// A terminal that is not a net or a bit of one has no node to be part of,
    /// and is a named error rather than a switch that silently joins nothing.
    #[test]
    fn test_a_tran_terminal_that_is_not_a_net_is_reported_by_name() {
        let (_, module) = parse_module_declaration(
            r#"
            module bad_terminal();
                wire a, b, c;
                tran t1({a, b}, c);
            endmodule
        "#,
        )
        .expect("a `tran` should parse");
        let mut simulator = Simulator::new(module);
        simulator.setup().expect("elaboration records the switch");
        assert_eq!(
            simulator.run(),
            Err(SimulationError::Unsupported(
                "a `tran` terminal that is not a net or one bit of one"
            ))
        );
    }

    /// A terminal count no gate of that kind can take is a named error too.
    #[test]
    fn test_a_gate_with_too_few_terminals_is_reported_by_name() {
        let (_, module) = parse_module_declaration(
            r#"
            module short_gate();
                wire out;
                and (out);
            endmodule
        "#,
        )
        .expect("the statement should parse");

        let mut simulator = Simulator::new(module);
        assert_eq!(
            simulator.setup(),
            Err(SimulationError::GateTerminals {
                gate: "and",
                found: 1
            })
        );
    }

    /// An arrayed instance whose terminal is neither shared nor sliceable is
    /// reported rather than misconnected.
    #[test]
    fn test_an_unsliceable_array_terminal_is_reported_by_name() {
        let (_, module) = parse_module_declaration(
            r#"
            module bad_array();
                reg [7:0] wide;
                reg enable;
                wire [3:0] bus;
                bufif1 drv [3:0] (bus, wide, enable);
            endmodule
        "#,
        )
        .expect("the statement should parse");

        let mut simulator = Simulator::new(module);
        assert_eq!(
            simulator.setup(),
            Err(SimulationError::GateArrayTerminal {
                gate: "bufif1",
                expected: 4,
                found: 8
            })
        );
    }

    /// An arrayed instance's terminal may be an arbitrary expression of the
    /// array's width, and each instance takes one bit of what it evaluates to.
    ///
    /// `{2'b10, lo}` names no storage, so a bit of it is not something a
    /// `BitSelect` can name — it is read by a **shift** instead, which is all
    /// one can ask of an expression, and all an *input* terminal needs.
    /// iverilog 12.0 prints `1001` then `zzzz` for this design (corpus
    /// `bufif`, `npmos2`, `rnpmos2`).
    #[test]
    fn test_an_array_terminal_may_be_an_expression() {
        let mut simulator = simulator_for(
            r#"
            module top();
                wire [3:0] bus;
                reg [1:0] lo;
                reg en;
                bufif0 drv [3:0] (bus, {2'b10, lo}, en);
                initial begin
                    lo = 2'b01; en = 1'b0;
                    #1 $display("%b", bus);
                    en = 1'b1;
                    #1 $display("%b", bus);
                end
            endmodule
        "#,
        );
        simulator.advance(3).expect("time should advance");
        assert_eq!(simulator.output().lines(), vec!["1001", "zzzz"]);
    }

    /// A gate inside an instantiated module drives the parent's net, because
    /// its terminals are renamed into the flat store like everything else.
    #[test]
    fn test_a_gate_inside_an_instance() {
        let child = parse_module_declaration(
            r#"
            module inverter(input wire a, output wire y);
                not (y, a);
            endmodule
        "#,
        )
        .unwrap()
        .1;
        let top = parse_module_declaration(
            r#"
            module top();
                reg in;
                wire out;
                inverter dut (.a(in), .y(out));
                initial in = 1'b0;
            endmodule
        "#,
        )
        .unwrap()
        .1;

        let mut simulator = Simulator::with_modules(vec![top, child], "top");
        simulator.setup().unwrap();
        simulator.advance(1).unwrap();

        assert_eq!(level(&simulator, "out"), "1");
    }

    /// A named block is a scope: the variable it declares is its own, and the
    /// design signal of the same name outside it is untouched.
    #[test]
    fn test_a_named_block_local_shadows_an_outer_signal() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg [7:0] value;
                initial begin
                    value = 1;
                    begin : blk
                        reg [7:0] value;
                        value = 9;
                        $display("inner=%0d", value);
                    end
                    $display("outer=%0d", value);
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "inner=9\nouter=1\n");
        // The two live side by side in the flat store, the block's under the
        // dotted name its scope gives it.
        assert_eq!(simulator.get("value").expect("declared").to_u128(), Some(1));
        assert_eq!(
            simulator.get("blk.value").expect("declared").to_u128(),
            Some(9)
        );
    }

    /// Two blocks that name a variable the same way are two variables, and a
    /// block inside a block takes both names.
    #[test]
    fn test_named_blocks_do_not_share_a_variable() {
        let mut simulator = simulator_for(
            r#"
            module main();
                initial begin
                    begin : first
                        integer i;
                        i = 1;
                        begin : inner
                            integer i;
                            i = 3;
                        end
                    end
                    begin : second
                        integer i;
                        i = 2;
                    end
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(
            simulator.get("first.i").expect("declared").to_u128(),
            Some(1)
        );
        assert_eq!(
            simulator.get("second.i").expect("declared").to_u128(),
            Some(2)
        );
        assert_eq!(
            simulator.get("first.inner.i").expect("declared").to_u128(),
            Some(3)
        );
    }

    /// A block inside an instance is qualified like everything else in it, so
    /// two instances of one module have a variable each.
    #[test]
    fn test_a_named_block_inside_an_instance_is_qualified() {
        let child = parse_module_declaration(
            r#"
            module counter();
                initial begin : body
                    integer count;
                    count = 7;
                end
            endmodule
        "#,
        )
        .unwrap()
        .1;
        let top = parse_module_declaration(
            r#"
            module top();
                counter one ();
                counter two ();
            endmodule
        "#,
        )
        .unwrap()
        .1;

        let mut simulator = Simulator::with_modules(vec![top, child], "top");
        simulator.setup().expect("design should elaborate");
        simulator.advance(1).expect("time should advance");

        assert_eq!(
            simulator.get("one.body.count").expect("declared").to_u128(),
            Some(7)
        );
        assert_eq!(
            simulator.get("two.body.count").expect("declared").to_u128(),
            Some(7)
        );
    }

    /// A `wait` on something already true falls straight through; one on
    /// something that is not suspends until whatever it names moves.
    #[test]
    fn test_wait_suspends_until_its_condition_becomes_true() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg flag;
                reg [7:0] seen;
                initial begin
                    flag = 0;
                    #10 flag = 1;
                end
                initial begin
                    seen = 0;
                    wait (flag) seen = 1;
                    $display("seen=%0d at %0t", seen, $time);
                    wait (seen == 1) $display("through at %0t", $time);
                end
            endmodule
        "#,
        );

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("seen").expect("declared").to_u128(), Some(0));

        simulator.advance(10).expect("time should advance");
        assert_eq!(simulator.get("seen").expect("declared").to_u128(), Some(1));
        assert_eq!(simulator.output().text(), "seen=1 at 10\nthrough at 10\n");
    }

    /// `fork`/`join` branches that consume no time run one after another,
    /// which is what running them at once would come to.
    #[test]
    fn test_fork_join_runs_every_branch() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg [7:0] a, b, c;
                initial begin
                    fork
                        a = 1;
                        b = 2;
                    join
                    c = a + b;
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.get("c").expect("declared").to_u128(), Some(3));
    }

    /// A branch that consumes time gives the others a turn while it waits, so
    /// the block goes on only once the *last* branch has finished — the
    /// maximum of their finish times, not the sum. Measured against iverilog
    /// 12.0.
    #[test]
    fn test_a_fork_resumes_at_the_last_branch_to_finish() {
        let mut simulator = simulator_for(
            r#"
            module main();
                integer a, b;
                initial begin
                    a = 0; b = 0;
                    fork begin #10 a = 1; end  begin #5 b = 2; end join
                    $display("t=%0d a=%0d b=%0d", $time, a, b);
                    fork #3 a = 7;  #1 b = 9; join
                    $display("t=%0d a=%0d b=%0d", $time, a, b);
                end
            endmodule
        "#,
        );

        simulator.advance(100).expect("time should advance");
        assert_eq!(simulator.output().text(), "t=10 a=1 b=2\nt=13 a=7 b=9\n");
    }

    /// A branch runs *while* its siblings do, not after them: a shorter branch
    /// finishes at its own time rather than being pushed out by a longer one.
    #[test]
    fn test_fork_branches_keep_their_own_timelines() {
        let mut simulator = simulator_for(
            r#"
            module main();
                initial begin
                    fork
                        begin #10 $display("one at %0t", $time); end
                        begin #20 $display("two at %0t", $time); end
                        begin #1  $display("three at %0t", $time); end
                    join
                    $display("join at %0t", $time);
                end
            endmodule
        "#,
        );

        simulator.advance(100).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "three at 1\none at 10\ntwo at 20\njoin at 20\n"
        );
    }

    /// A `fork` inside a branch of another one needs nothing new: the inner
    /// join's parent is the branch cursor, which carries the outer join's
    /// identity. Measured against iverilog 12.0.
    #[test]
    fn test_a_fork_nested_inside_a_branch_joins_in_order() {
        let mut simulator = simulator_for(
            r#"
            module main();
                integer c;
                initial begin
                    fork
                        begin
                            fork
                                #7 c = 1;
                                #3 c = 2;
                            join
                            $display("inner join at %0t c=%0d", $time, c);
                        end
                        #2 $display("sibling at %0t", $time);
                    join
                    $display("outer join at %0t", $time);
                end
            endmodule
        "#,
        );

        simulator.advance(100).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "sibling at 2\ninner join at 7 c=1\nouter join at 7\n"
        );
    }

    /// A `fork` written in a task's body is spliced into whatever enabled it,
    /// so its branch program counters have to be the spliced ones. Measured
    /// against iverilog 12.0.
    #[test]
    fn test_a_fork_inside_an_enabled_task_runs_its_branches() {
        let mut simulator = simulator_for(
            r#"
            module main();
                integer a, b;
                task t;
                    begin
                        fork
                            begin #10 a = 1; end
                            begin #5  b = 2; end
                        join
                        $display("task join at %0t a=%0d b=%0d", $time, a, b);
                    end
                endtask
                initial begin
                    t;
                    $display("after task at %0t", $time);
                end
            endmodule
        "#,
        );

        simulator.advance(100).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "task join at 10 a=1 b=2\nafter task at 10\n"
        );
    }

    /// `disable` of a scope holding a running `fork` cancels *every* branch and
    /// the block parked at the join — nothing after the join runs. Measured
    /// against iverilog 12.0.
    #[test]
    fn test_disable_cancels_every_branch_of_a_running_fork() {
        let mut simulator = simulator_for(
            r#"
            module main();
                integer a, b;
                initial begin : blk
                    fork
                        begin #10 a = 1; $display("branch one at %0t", $time); end
                        begin #20 b = 2; $display("branch two at %0t", $time); end
                    join
                    $display("after join at %0t", $time);
                end
                initial begin
                    #5 disable blk;
                    $display("disabled at %0t", $time);
                end
            endmodule
        "#,
        );

        simulator.advance(100).expect("time should advance");
        assert_eq!(simulator.output().text(), "disabled at 5\n");
    }

    /// A `disable` naming a scope inside a **sibling** branch stops that one
    /// thread and nothing else — and the thread still arrives at the `join`,
    /// so the `fork` completes.
    ///
    /// iverilog 12.0 on the same design:
    ///
    /// ```text
    /// 1 disabled
    /// 1 joined q=00
    /// 6 done q=00
    /// ```
    ///
    /// The join resumes at 1 rather than waiting for the `#2` the cancelled
    /// task would have taken, and `q` never takes the write past the
    /// `disable`. Cancelling the whole block instead loses the `fork` the
    /// cancelled branch belonged to.
    #[test]
    fn test_disable_of_a_sibling_branchs_scope_still_reaches_the_join() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg [7:0] q;
                task slow;
                    begin
                        #2;
                        q = 8'hAA;
                        $display("%0t task finished", $time);
                    end
                endtask
                initial begin
                    q = 0;
                    fork
                        slow;
                        begin
                            #1;
                            disable slow;
                            $display("%0t disabled", $time);
                        end
                    join
                    $display("%0t joined q=%h", $time, q);
                    #5 $display("%0t done q=%h", $time, q);
                end
            endmodule
        "#,
        );

        simulator.advance(100).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "1 disabled\n1 joined q=00\n6 done q=00\n"
        );
    }

    /// A `fork` whose branches consume no time cannot tell concurrent from
    /// sequential, so it is still compiled as a plain block — which is what
    /// keeps one legal inside a `function`, where there is no driver to hand
    /// out threads.
    #[test]
    fn test_a_zero_delay_fork_still_runs_as_a_block() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg [7:0] a, b;
                initial begin
                    fork
                        a = 1;
                        b = 2;
                    join
                    $display("a=%0d b=%0d at %0t", a, b, $time);
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "a=1 b=2 at 0\n");
    }

    /// A `disable` written inside a branch, naming a scope the `fork` itself
    /// sits in, stops *every* branch and the block parked at the join — so it
    /// cannot be the jump a local `disable` compiles to, which would stop the
    /// one thread that ran it and leave the join waiting for ever.
    ///
    /// Execution continues where the named scope ends, which here is past the
    /// statement after the `join` — that statement is inside `blk` too.
    /// iverilog 12.0:
    ///
    /// ```text
    /// 5 disabling
    /// 20 done a=x
    /// ```
    #[test]
    fn test_a_disable_of_the_scope_around_a_fork_stops_every_branch() {
        let mut simulator = simulator_for(
            r#"
            module main();
                integer a;
                initial begin : blk
                    fork
                        begin
                            #5 $display("%0t disabling", $time);
                            disable blk;
                            $display("%0t not reached", $time);
                        end
                        begin #10 a = 1; $display("%0t branch two", $time); end
                    join
                    $display("%0t after join", $time);
                end
                initial #20 $display("%0t done a=%0d", $time, a);
            endmodule
        "#,
        );

        simulator.advance(50).expect("time should advance");
        assert_eq!(
            simulator.output().lines(),
            vec!["5 disabling", "20 done a=x"]
        );
    }

    /// A `fork` may carry the label itself, and `disable` of it ends at the
    /// `join` rather than at the end of the block around it — so the statement
    /// after the `join` *does* run. iverilog 12.0 prints `5 PASSED` and then
    /// `30 FAILED`, the second `initial` being untouched by the `disable`.
    #[test]
    fn test_a_disable_of_a_forks_own_label_carries_on_after_the_join() {
        let mut simulator = simulator_for(
            r#"
            module test();
                initial begin
                    fork : F
                        forever #10;
                        #5 disable F;
                    join
                    $display("%0t PASSED", $time);
                end
                initial begin
                    #30; $display("%0t FAILED", $time);
                end
            endmodule
        "#,
        );

        simulator.advance(50).expect("time should advance");
        assert_eq!(simulator.output().lines(), vec!["5 PASSED", "30 FAILED"]);
    }

    /// A branch that never arrives holds the join for ever, which is what the
    /// LRM says — but it must not hold the *simulator*: the other branches
    /// still run and time still moves.
    #[test]
    fn test_a_branch_that_never_finishes_leaves_the_join_waiting() {
        let mut simulator = simulator_for(
            r#"
            module main();
                initial begin
                    fork
                        begin #10 $display("finished at %0t", $time); end
                        begin wait (0); $display("never"); end
                    join
                    $display("never either");
                end
                initial #30 $display("clock still moves at %0t", $time);
            endmodule
        "#,
        );

        simulator.advance(100).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "finished at 10\nclock still moves at 30\n"
        );
    }

    /// A `fork` that neither waits nor lets time move is a spinning block, and
    /// the resumption budget is what sees it — an error rather than a hang.
    #[test]
    fn test_a_zero_delay_fork_in_a_forever_loop_is_reported_not_hung() {
        let error = setup_error(
            r#"
            module main();
                reg [7:0] a, b;
                initial forever begin
                    fork
                        begin #0 a = 1; end
                        begin #0 b = 2; end
                    join
                end
            endmodule
        "#,
        );

        assert!(matches!(error, SimulationError::NoConvergence { .. }));
    }

    /// An `always` block part way through a `fork` has not finished the run it
    /// is on, so an edge arriving meanwhile must not start a second copy of it.
    #[test]
    fn test_an_edge_does_not_restart_a_block_waiting_at_a_join() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg clk;
                integer runs;
                initial begin
                    runs = 0;
                    clk = 0;
                    #1 clk = 1;
                    #1 clk = 0;
                    #1 clk = 1;
                end
                always @(posedge clk) begin
                    runs = runs + 1;
                    fork
                        #10 ;
                        #20 ;
                    join
                end
                initial #40 $display("runs=%0d", runs);
            endmodule
        "#,
        );

        simulator.advance(100).expect("time should advance");
        assert_eq!(simulator.output().text(), "runs=1\n");
    }

    /// An event control written in front of a statement suspends the block it
    /// is in until the edge arrives, and only counts edges taken *after* it
    /// was reached — the write the block made itself on its way there is not
    /// one.
    #[test]
    fn test_statement_level_event_control_waits_for_a_later_edge() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg clk;
                reg [3:0] q;
                initial begin
                    clk = 0;
                    #1 clk = 1;
                    #1 clk = 0;
                end
                initial begin
                    clk = 0;
                    @(negedge clk) q = 4'ha;
                    $display("q=%h at %0t", q, $time);
                end
            endmodule
        "#,
        );

        // The block wrote `clk = 0` itself before arming the wait, so the
        // x -> 0 transition that write made must not wake it.
        simulator.advance(1).expect("time should advance");
        assert!(simulator.output().text().is_empty());

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.output().text(), "q=a at 2\n");
    }

    /// `value = @(ev) 4'h5;` reads the right hand side now and writes it when
    /// the event arrives, and the free-running block it is in arms again the
    /// moment the write lands.
    #[test]
    fn test_intra_assignment_event_control_writes_when_the_event_arrives() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg [3:0] value;
                reg trigger;
                initial begin
                    value = 0;
                    #5 trigger = 0;
                    #5 trigger = 1;
                end
                always value = @(trigger) 4'h5;
            endmodule
        "#,
        );

        simulator.advance(4).expect("time should advance");
        assert_eq!(simulator.get("value").expect("declared").to_u128(), Some(0));

        // x -> 0 is a change of `trigger`, so the held 5 lands at time 5.
        simulator.advance(2).expect("time should advance");
        assert_eq!(simulator.get("value").expect("declared").to_u128(), Some(5));
    }

    /// A `repeat` in front of an intra-assignment event control counts the
    /// events, so the write lands on the last of them.
    #[test]
    fn test_intra_assignment_event_control_counts_its_repeats() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg [3:0] value;
                reg trigger;
                initial begin
                    value = 0;
                    trigger = 0;
                    #5 trigger = 1;
                    #5 trigger = 0;
                    #5 trigger = 1;
                end
                initial value = repeat (3) @(trigger) 4'h5;
            endmodule
        "#,
        );

        simulator.advance(12).expect("time should advance");
        assert_eq!(simulator.get("value").expect("declared").to_u128(), Some(0));

        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.get("value").expect("declared").to_u128(), Some(5));
    }

    /// Two blocks that each `repeat` count independently: the hidden counter
    /// is named by an instruction index, and both blocks start at zero, so a
    /// shared name would let them count each other down. Measured against
    /// iverilog 12.0, which prints `a=5 b=5` (corpus `pr923`, whose two
    /// `repeat (10)` loops stopped halfway).
    #[test]
    fn test_two_blocks_repeat_their_own_number_of_times() {
        let mut simulator = simulator_for(
            r#"
            module main();
                integer a, b;
                initial begin
                    a = 0;
                    repeat (5) #1 a = a + 1;
                end
                initial begin
                    b = 0;
                    repeat (5) #1 b = b + 1;
                end
            endmodule
        "#,
        );

        simulator.advance(20).expect("time should advance");
        assert_eq!(simulator.get("a").expect("declared").to_u128(), Some(5));
        assert_eq!(simulator.get("b").expect("declared").to_u128(), Some(5));
    }

    /// A function is evaluated at one instant, so a wait inside one is a named
    /// error rather than a call that quietly returns whatever it found.
    #[test]
    fn test_a_wait_inside_a_function_is_a_named_error() {
        let error = setup_error(
            r#"
            module main();
                reg flag;
                function f;
                    input a;
                    begin
                        wait (flag) f = a;
                    end
                endfunction
                initial flag = f(1);
            endmodule
        "#,
        );

        assert_eq!(
            error,
            SimulationError::Unsupported("a wait or event control inside a function")
        );
    }

    /// `@*` in front of a *statement* is sensitive to what that statement
    /// reads, where the same token in front of a block is sensitive to what
    /// the block reads. Here the outer one waits on `b` and `c`, the inner one
    /// on `c` alone.
    #[test]
    fn test_statement_level_implicit_event_control_reads_its_own_statement() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg a, b, c;
                always @* begin
                    a = b;
                    $display("one at %0t", $time);
                    @* a = c;
                    $display("two at %0t", $time);
                end
                initial begin
                    #10 b = 0;
                    #10 c = 0;
                    #10 b = 1;
                end
            endmodule
        "#,
        );

        simulator.advance(40).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "one at 10\ntwo at 20\none at 30\n"
        );
    }

    /// An `@*` with no statement after it to read has nothing to wait on, so
    /// it is an error naming it rather than a wait that never ends.
    #[test]
    fn test_an_implicit_event_control_with_nothing_to_read_is_a_named_error() {
        let error = setup_error(
            r#"
            module main();
                reg [3:0] a, b;
                initial a = @* b;
            endmodule
        "#,
        );

        assert_eq!(
            error,
            SimulationError::Unsupported("an `@(*)` event control that reads nothing")
        );
    }

    /// `disable` of the block the statement is written inside is an early exit
    /// from it: the statements after the `disable` but still inside the block
    /// never run, and the ones after the block do.
    #[test]
    fn test_disable_of_the_enclosing_block_is_an_early_exit() {
        let mut simulator = simulator_for(
            r#"
            module main();
                initial begin
                    begin : body
                        $display("in");
                        disable body;
                        $display("not reached");
                    end
                    $display("after");
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "in\nafter\n");
    }

    /// The same, out of a loop several levels down: a `disable` inside a `for`
    /// inside the named block leaves the whole block, not just the iteration.
    #[test]
    fn test_disable_leaves_a_loop_nested_inside_the_block_it_names() {
        let mut simulator = simulator_for(
            r#"
            module main();
                integer i;
                initial begin
                    begin : configloop
                        for (i = 0; i < 4; i = i + 1) begin
                            $display("%0d", i);
                            if (i == 1) disable configloop;
                        end
                        $display("not reached");
                    end
                    $display("i is %0d", i);
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "0\n1\ni is 1\n");
    }

    /// `disable <task>` written inside the task itself is how Verilog spells an
    /// early return, and the body is inlined where it was enabled — so the
    /// caller carries on with the statement after the enable.
    #[test]
    fn test_disable_of_a_task_from_inside_it_returns_to_the_caller() {
        let mut simulator = simulator_for(
            r#"
            module main();
                task t;
                    begin
                        $display("entered");
                        disable t;
                        $display("not reached");
                    end
                endtask
                initial begin
                    t;
                    $display("back");
                end
            endmodule
        "#,
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "entered\nback\n");
    }

    /// A block suspended on a `#delay` is cancelled by a `disable` written in
    /// another block: the statements it had left never run.
    #[test]
    fn test_disable_cancels_a_block_suspended_on_a_delay() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg working;
                initial begin : my_block
                    working = 1;
                    #10;
                    working = 0;
                end
                initial begin
                    #5 disable my_block;
                end
            endmodule
        "#,
        );

        simulator.advance(20).expect("time should advance");
        assert_eq!(simulator.get("working").unwrap().to_binary(), "1");
    }

    /// A block due at the **same timestamp** as the `disable` that names it is
    /// cancelled too, even though `advance` has already taken it off the queue
    /// for this round. Corpus `sdw_dsbl`, which iverilog 12.0 answers
    /// `PASSED` — the `disable` at time 15 runs first, and the statements
    /// `my_block` had left at time 15 never run.
    ///
    /// The `disable` is queued for 15 at time 0 and `my_block`'s continuation
    /// at time 10, so the queue's FIFO order puts the `disable` first. What it
    /// could not see was the round itself: both cursors are off the queue by
    /// the time either of them runs.
    #[test]
    fn test_disable_cancels_a_block_due_at_the_same_timestamp() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg working;
                initial begin : my_block
                    working = 1;
                    #5;
                    working = 1;
                    #5;
                    working = 1;
                    #5;
                    working = 0;
                    #5;
                end
                initial begin
                    #15;
                    disable my_block;
                end
            endmodule
        "#,
        );

        simulator.advance(30).expect("time should advance");
        assert_eq!(simulator.get("working").unwrap().to_binary(), "1");
    }

    /// The disabled block picks up at the statement following the block it
    /// named, at the time it was disabled — and after whatever the block that
    /// disabled it went on to print, which is where iverilog puts it.
    #[test]
    fn test_a_disabled_block_continues_after_the_scope_it_named() {
        let mut simulator = simulator_for(
            r#"
            module main();
                initial begin
                    begin : b
                        #10;
                        $display("%0t inside b", $time);
                    end
                    $display("%0t after b", $time);
                end
                initial begin
                    #5 disable b;
                    $display("%0t disabled", $time);
                end
            endmodule
        "#,
        );

        simulator.advance(20).expect("time should advance");
        assert_eq!(simulator.output().text(), "5 disabled\n5 after b\n");
    }

    /// A block waiting on the design rather than on the clock is cancelled the
    /// same way, and a *free-running* `always` then starts again — which is
    /// what makes `disable` the way a design restarts one.
    #[test]
    fn test_disable_restarts_a_free_running_always_block() {
        let mut simulator = simulator_for(
            r#"
            module main();
                always begin : restartable
                    $display("%0t runs", $time);
                    wait (0);
                    $display("FAILED");
                end
                initial begin
                    #10 disable restartable;
                    #10 $finish;
                end
            endmodule
        "#,
        );

        simulator.advance(30).expect("time should advance");
        assert_eq!(simulator.output().text(), "0 runs\n10 runs\n");
    }

    /// An edge-triggered `always` block that was disabled mid-activation is
    /// simply not running any more, and the next edge starts it afresh.
    #[test]
    fn test_disable_cancels_one_activation_of_an_edge_triggered_block() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg clk, q;
                always @(posedge clk) begin : ff
                    #2;
                    q = ~q;
                end
                initial begin
                    q = 0;
                    clk = 0;
                    #1 clk = 1;
                    #1 disable ff;
                    #5 clk = 0;
                    #1 clk = 1;
                    #5 $display("q is %b", q);
                end
            endmodule
        "#,
        );

        simulator.advance(30).expect("time should advance");
        assert_eq!(simulator.output().text(), "q is 1\n");
    }

    /// Disabling a scope that exists but is not running anywhere is the LRM's
    /// no-op — `always #6 disable t;` says nothing about the times no enable of
    /// `t` is in flight.
    #[test]
    fn test_disabling_a_scope_that_is_not_running_does_nothing() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg [3:0] value;
                task t;
                    value = #2 1;
                endtask
                initial begin
                    value = 0;
                    #5 t;
                    #4 $display("value is %0d", value);
                end
                always #6 disable t;
            endmodule
        "#,
        );

        simulator.advance(30).expect("time should advance");
        assert_eq!(simulator.output().text(), "value is 0\n");
    }

    /// A `disable` naming nothing the design has is an error saying so. A
    /// design that thought it cancelled something and did not is the hardest
    /// kind of wrong answer to find.
    #[test]
    fn test_disabling_a_scope_the_design_does_not_have_is_a_named_error() {
        let error = setup_error(
            r#"
            module main();
                initial disable nowhere;
            endmodule
        "#,
        );

        assert_eq!(error, SimulationError::UnknownScope("nowhere".to_string()));
    }

    /// A delay is an expression, so a clock generator may be written in terms
    /// of the parameter that gives its period — which is how nearly every
    /// design writes one.
    #[test]
    fn test_a_delay_expression_reads_the_parameter_it_names() {
        let mut simulator = simulator_for(
            r#"
            module oscillator(output reg clk);
                parameter PERIOD = 20;
                initial clk = 1'b0;
                always #(PERIOD / 2) clk = ~clk;
            endmodule
        "#,
        );

        assert_eq!(simulator.get("clk").unwrap().to_binary(), "0");
        simulator.advance(10).unwrap();
        assert_eq!(simulator.get("clk").unwrap().to_binary(), "1");
        simulator.advance(10).unwrap();
        assert_eq!(simulator.get("clk").unwrap().to_binary(), "0");
    }

    /// `#n` for a variable `n` is read when the block reaches it, not when the
    /// block was compiled — so a design that changes `n` waits differently the
    /// next time round.
    #[test]
    fn test_a_delay_naming_a_variable_is_read_when_it_is_waited_on() {
        let mut simulator = simulator_for(
            r#"
            module main();
                integer n;
                initial begin
                    n = 3;
                    #n $display("%0t first", $time);
                    n = 7;
                    #n $display("%0t second", $time);
                end
            endmodule
        "#,
        );

        simulator.advance(30).unwrap();
        assert_eq!(simulator.output().text(), "3 first\n10 second\n");
    }

    /// `assign #10 a = b;` — the net follows its expression ten time units
    /// later, and reads `x` until the first value lands. Measured against
    /// iverilog 12.0.
    #[test]
    fn test_a_delayed_continuous_assignment_lands_after_its_delay() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg b;
                wire a;
                assign #10 a = b;
                initial begin
                    $display("%0t a=%b", $time, a);
                    b = 1;
                    #5 $display("%0t a=%b", $time, a);
                    #10 $display("%0t a=%b", $time, a);
                end
            endmodule
        "#,
        );

        simulator.advance(50).expect("time should advance");
        assert_eq!(simulator.output().text(), "0 a=x\n5 a=x\n15 a=1\n");
    }

    /// `#(rise, fall)` — which of the three delays applies is decided by the
    /// value being driven **to**, not by the one being left.
    ///
    /// An omitted turn-off delay is the *shorter* of the two, which is why the
    /// move to `z` takes 2 rather than 6. iverilog 12.0 prints
    /// `0 a=x / 2 a=0 / 26 a=1 / 42 a=0 / 62 a=z` for this design.
    #[test]
    fn test_a_delay3_picks_its_delay_from_the_value_driven_to() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg b;
                wire a;
                assign #(6, 2) a = b;
                initial begin
                    b = 1'b0;
                    #20 b = 1'b1;
                    #20 b = 1'b0;
                    #20 b = 1'bz;
                end
                initial $monitor("%0t a=%b", $time, a);
            endmodule
        "#,
        );

        simulator.advance(80).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "0 a=x\n2 a=0\n26 a=1\n42 a=0\n62 a=z\n"
        );
    }

    /// A value more than one bit wide is **one** transaction at the
    /// **longest** of the delays its changed bits ask for — the bits that did
    /// not move ask for nothing, and there is no intermediate value.
    ///
    /// iverilog 12.0, for `assign #(6, 2) o = i;` on a `[3:0]`: `1100 -> 0011`
    /// (two bits falling, two rising) lands at 6, and `1111 -> 1100` (only
    /// falls) lands at 2.
    #[test]
    fn test_a_wide_delayed_assignment_lands_once_at_the_longest_delay() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg [3:0] i;
                wire [3:0] o;
                assign #(6, 2) o = i;
                initial begin
                    i = 4'b1111;
                    #20 i = 4'b1100;
                    #20 i = 4'b0011;
                end
                initial $monitor("%0t o=%b", $time, o);
            endmodule
        "#,
        );

        simulator.advance(60).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "0 o=xxxx\n6 o=1111\n22 o=1100\n46 o=0011\n"
        );
    }

    /// A **gate** delay is simulated, on the same machinery a delayed
    /// `assign` uses: the delay changes which value the gate asserts rather
    /// than whether it asserts one, so nothing about strength resolution or
    /// the change journal had to learn about it.
    ///
    /// iverilog 12.0 prints `0 o=x / 2 o=0 / 26 o=1 / 42 o=0` for this design
    /// (corpus `rise_fall_delay2`, `rise_fall_decay2`).
    #[test]
    fn test_a_gate_delay_is_simulated() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg i;
                wire o;
                buf #(6, 2) g (o, i);
                initial begin
                    i = 1'b0;
                    #20 i = 1'b1;
                    #20 i = 1'b0;
                end
                initial $monitor("%0t o=%b", $time, o);
            endmodule
        "#,
        );

        simulator.advance(60).expect("time should advance");
        assert_eq!(simulator.output().text(), "0 o=x\n2 o=0\n26 o=1\n42 o=0\n");
    }

    /// A **user-defined** primitive is a continuous driver like a gate, so a
    /// `#(...)` on its instantiation is a delay and goes through the identical
    /// machinery. `#(6, 2)` on a UDP buffer traces exactly what the same
    /// delays on a `buf` do — iverilog 12.0 prints
    /// `0 out=x / 2 out=0 / 26 out=1 / 42 out=0` for both.
    ///
    /// The tokens read as a *parameter override* — that is what `#(...)` means
    /// on a module instantiation — and only the module being instantiated says
    /// otherwise, which is why the decision is made where the instance is
    /// created (corpus `udp_bufg2`, which writes the one-delay `BUFG #5`).
    #[test]
    fn test_a_primitive_instance_delay_is_simulated() {
        let source = r#"
            primitive BUFG (O, I);
                output O;
                input I;
                table
                    0 : 0 ;
                    1 : 1 ;
                endtable
            endprimitive

            module main();
                reg in;
                wire out;
                BUFG #(6, 2) bg (out, in);
                initial $monitor("%0t out=%b", $time, out);
                initial begin
                    in = 1'b0;
                    #20 in = 1'b1;
                    #20 in = 1'b0;
                end
            endmodule
        "#;
        let (rest, modules) =
            crate::parsers::source::parse_verilog_source(source).expect("design should parse");
        assert!(rest.trim().is_empty(), "unparsed input: {}", rest);
        let mut simulator = Simulator::with_modules(modules, "main");
        simulator.setup().expect("design should elaborate");

        simulator.advance(60).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "0 out=x\n2 out=0\n26 out=1\n42 out=0\n"
        );
    }

    /// The delay is **inertial**, not transport: a pulse shorter than the
    /// delay never reaches the net, because the value in flight is replaced
    /// rather than queued behind. Measured against iverilog 12.0.
    #[test]
    fn test_a_delayed_assignment_swallows_a_pulse_shorter_than_its_delay() {
        let mut simulator = simulator_for(
            r#"
            module main();
                reg b;
                wire a;
                assign #10 a = b;
                initial $monitor("%0t a=%b b=%b", $time, a, b);
                initial begin
                    b = 0;
                    #20 b = 1;
                    #5  b = 0;
                    #40 b = 1;
                    #20 b = 0;
                end
            endmodule
        "#,
        );

        simulator.advance(125).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "0 a=x b=0\n10 a=0 b=0\n20 a=0 b=1\n25 a=0 b=0\n\
             65 a=0 b=1\n75 a=1 b=1\n85 a=1 b=0\n95 a=0 b=0\n"
        );
    }

    /// A delay on an `assign` may be an expression naming a parameter, and a
    /// comma-separated list shares it exactly as it shares a strength.
    #[test]
    fn test_a_delayed_assignment_list_shares_one_delay() {
        let mut simulator = simulator_for(
            r#"
            module main();
                parameter LAG = 4;
                reg d;
                wire x, y;
                assign #(LAG) x = d, y = ~d;
                initial d = 1;
            endmodule
        "#,
        );

        simulator.advance(3).expect("time should advance");
        assert_eq!(simulator.get("x").unwrap().to_binary(), "x");
        assert_eq!(simulator.get("y").unwrap().to_binary(), "x");

        simulator.advance(2).expect("time should advance");
        assert_eq!(simulator.get("x").unwrap().to_binary(), "1");
        assert_eq!(simulator.get("y").unwrap().to_binary(), "0");
    }
}

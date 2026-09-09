//! System tasks — the `$display` / `$write` families, `$finish` and `$time`.
//!
//! A self-checking Verilog test states its result by printing it, so a system
//! task is not decoration: `$display("PASSED")` is *how* a test passes. The
//! output therefore does not go to stdout, where nothing could assert on it.
//! It accumulates in an [`Output`] buffer on the [`TaskContext`], which
//! [`Simulator::output`](crate::simulator::runner::Simulator::output) hands
//! back, so "did this design print PASSED?" is a plain assertion. A caller that
//! wants it on a terminal can print the buffer itself.
//!
//! Which `$name`s are meaningful is settled at compile time by
//! [`TaskCall::compile`], not while the design is running: an unrecognised task
//! is an error that names it, never a silent no-op that would make a test look
//! as though it had passed.
//!
//! A `$name` in this family is three decisions spelled as one word, and
//! [`split_task_name`] takes it apart: an `f` prefix says the first argument is
//! a file descriptor, a `b`/`h`/`o` suffix says which [`Radix`] an argument with
//! no format specifier prints in, and what is left is the task itself. So
//! `$fdisplayh` is "to a descriptor, one line, hex by default".

use std::fs;
use std::path::{Path, PathBuf};

use crate::parsers::behavior::{SystemTaskArgument, SystemTaskCall};
use crate::parsers::expr::Expression;
use crate::register::{Register, ONE, REAL_WIDTH, X, Z, ZERO};
use crate::simulator::elaborate::rename_expression;
use crate::simulator::eval::{eval, string_bits, SYSTEM_FUNCTIONS};
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::StateStore;

/// Everything a design has printed, as one buffer.
///
/// `$display` appends a newline and `$write` does not, so a line is only
/// complete once something terminates it; [`Output::lines`] reads the buffer
/// back split on newlines, with a trailing unterminated `$write` as its own
/// last entry.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Output {
    text: String,
}

impl Output {
    /// Everything printed so far, newlines and all.
    pub fn text(&self) -> &str {
        &self.text
    }

    /// The printed lines, without their newlines.
    pub fn lines(&self) -> Vec<&str> {
        self.text.lines().collect()
    }

    /// Whether the design has printed anything at all.
    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    fn push(&mut self, text: &str) {
        self.text.push_str(text);
    }

    fn push_line(&mut self, text: &str) {
        self.text.push_str(text);
        self.text.push('\n');
    }
}

/// The base an argument with no format specifier prints in.
///
/// `$display` prints such an argument in decimal; `$displayb`, `$displayh` and
/// `$displayo` are the same task with a different default. A specifier in a
/// format string still says what it says — `$displayh("%0d", a)` prints `a` in
/// decimal — so this is only ever the *default*.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Radix {
    Decimal,
    Binary,
    Hexadecimal,
    Octal,
}

impl Radix {
    /// The base a `%` specifier asks for, or `None` if it is not a radix.
    fn from_specifier(specifier: char) -> Option<Radix> {
        match specifier {
            'd' | 'D' => Some(Radix::Decimal),
            'b' | 'B' => Some(Radix::Binary),
            'h' | 'H' | 'x' | 'X' => Some(Radix::Hexadecimal),
            'o' | 'O' => Some(Radix::Octal),
            _ => None,
        }
    }

    /// Whether a memory file may be written in this base. `$readmemh` and
    /// `$readmemb` are the whole family — there is no `$readmemo` and no
    /// `$readmem`.
    fn is_memory_file(self) -> bool {
        matches!(self, Radix::Binary | Radix::Hexadecimal)
    }

    /// A value in this base, together with the width it pads to when the caller
    /// did not ask for one: as wide as the widest value of that many bits.
    ///
    /// A **real** is rounded to a whole number first — half away from zero, so
    /// `2.5` is `3` — and printed as narrowly as it goes, with no width to pad
    /// to, because the bits the digits came from are not the bits the value is
    /// stored in.
    ///
    /// Decimal renders the *number* and every other base renders the sixty-four
    /// bit two's complement integer it converts to. Corpus `br1029a` is where
    /// the difference shows: `%0d` of `-0.4` is `-0` — the sign survives a
    /// value that rounds to zero — while `%0x` of it is `0`, and `%0x` of
    /// `-0.5` is `ffffffffffffffff`.
    fn render(self, value: &Register) -> (String, usize) {
        if value.is_real() {
            let rounded = value.to_f64().round();
            if self == Radix::Decimal {
                // Rounded already, so this only writes the digits: C rounds a
                // `.5` to even here, where Verilog rounds away from zero.
                return (format!("{:.0}", rounded), 0);
            }
            let whole = Register::integer_from_f64(rounded, REAL_WIDTH);
            let (text, _) = self.render(&whole);
            return (without_leading_zeros(text), 0);
        }
        match self {
            Radix::Decimal => (
                decimal(value),
                decimal_width(value.width(), value.is_signed()),
            ),
            Radix::Binary => (binary(value), value.width()),
            Radix::Hexadecimal => (hex(value), value.width().div_ceil(4)),
            Radix::Octal => (octal(value), value.width().div_ceil(3)),
        }
    }

    /// Whether this base pads a value out with *digits* rather than spaces, so
    /// that a `%0…` asking for the narrowest rendering has leading zeros to
    /// drop. Decimal never grows a leading zero in the first place.
    fn pads_with_digits(self) -> bool {
        !matches!(self, Radix::Decimal)
    }
}

/// A `$display` or `$write`, with the two things that vary between the members
/// of the family.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Print {
    /// Whether the line is ended afterwards: `$display` does, `$write` does not.
    newline: bool,
    /// The base an unadorned argument prints in.
    radix: Radix,
    /// Whether the first argument is a file descriptor — the `$f…` half of the
    /// family. See [`TaskContext::check_descriptor`].
    descriptor: bool,
}

/// A system task this simulator can carry out.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SystemTask {
    /// Format the arguments and print them, now.
    Print(Print),
    /// `$strobe` — format the arguments and print them at the *end* of the
    /// current timestep, once everything else that runs in it has run.
    Strobe(Print),
    /// `$monitor` — arm a standing watch. It prints when it is armed and again
    /// at the end of any timestep in which one of its arguments moved. Only one
    /// is ever active: a second `$monitor` replaces the first.
    Monitor(Print),
    /// `$monitoron` (`true`) and `$monitoroff` (`false`) — whether the armed
    /// monitor reports.
    MonitorControl(bool),
    /// `$readmemh` / `$readmemb` — fill a memory from a text file of words in
    /// this radix.
    ReadMemory(Radix),
    /// `$writememh` / `$writememb` — the reverse: a memory's words to a file.
    WriteMemory(Radix),
    /// `$timeformat` — how `%t` renders a time value from here on.
    TimeFormat,
    /// End the simulation.
    Finish,
    /// The current simulated time. Meaningful as an argument; as a statement of
    /// its own the value has nowhere to go.
    Time,
}

/// One argument of a compiled task call.
#[derive(Clone, Debug, PartialEq)]
pub enum TaskArgument {
    /// A string literal, which is both a format string and printable text.
    Text(String),
    /// An expression, evaluated against the store when the call runs. A bare
    /// `$time` or `$random` argument is one of these too — the expression
    /// grammar owns system functions, so there is one implementation of what
    /// `$time` means.
    Value(Expression),
}

/// A system task call, with its name resolved and its arguments checked.
#[derive(Clone, Debug, PartialEq)]
pub struct TaskCall {
    task: SystemTask,
    arguments: Vec<TaskArgument>,
    /// The hierarchical name of the scope the call was written in, which is
    /// what `%m` prints: `top`, `top.dut`, `top.dut.blk`, `top.dut.load` for
    /// one inside task `load`.
    ///
    /// It is filled in twice, because the two halves are known in two places:
    /// `Program::compile` stamps the *block* path it is already carrying, and
    /// `elaborate` puts the instance path in front of it once it knows which
    /// instance the block belongs to. A call that never reaches elaboration —
    /// which is only ever a test — keeps the block path alone.
    scope: String,
}

impl TaskCall {
    /// Resolves a parsed `$name(...)` against the tasks the simulator
    /// implements.
    ///
    /// Fails on a task that is not recognised, reporting the name, so a design
    /// never prints nothing by accident.
    pub fn compile(call: &SystemTaskCall) -> Result<TaskCall, SimulationError> {
        let task = resolve_task(&call.name)?;

        let arguments = call
            .arguments
            .iter()
            .map(|argument| match argument {
                SystemTaskArgument::String(text) => Ok(TaskArgument::Text(text.clone())),
                // An empty slot renders as exactly one space — that is the
                // whole of what `$display("a",, b)` uses it for — so it is a
                // one-character literal and nothing downstream has to know.
                SystemTaskArgument::Empty => Ok(TaskArgument::Text(" ".to_string())),
                SystemTaskArgument::Expression(expression) => {
                    Ok(TaskArgument::Value(expression.clone()))
                }
                // A bare `$name` argument is a system function call with no
                // arguments. Whether it means anything is settled here rather
                // than when the design runs, so a name nothing implements is
                // reported before it can print nothing.
                SystemTaskArgument::SystemFunction(name)
                    if SYSTEM_FUNCTIONS.contains(&name.as_str()) =>
                {
                    Ok(TaskArgument::Value(Expression::SystemFunctionCall(
                        name.clone(),
                        Vec::new(),
                    )))
                }
                SystemTaskArgument::SystemFunction(name) => Err(unknown_task(name)),
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(TaskCall {
            task,
            arguments,
            scope: String::new(),
        })
    }

    /// Records the block path this call sits in — what `%m` prints, before the
    /// instance in front of it is known.
    pub fn set_scope(&mut self, scope: &str) {
        self.scope = scope.trim_end_matches('.').to_string();
    }

    /// Puts the instance path in front of the block path, which is the other
    /// half of `%m` and the half only `elaborate` knows.
    pub fn qualify_scope(&mut self, hierarchy: &dyn Fn(&str) -> String) {
        self.scope = hierarchy(&self.scope);
    }

    /// Rewrites every signal the call reads through `resolve`, so a call inside
    /// an instantiated module reads the parent's store. See
    /// [`Program::rename`](crate::simulator::program::Program::rename).
    pub fn rename(&mut self, resolve: &dyn Fn(&str) -> String) {
        for argument in &mut self.arguments {
            if let TaskArgument::Value(expression) = argument {
                rename_expression(expression, resolve);
            }
        }
    }

    /// Rewrites every argument expression through `replace`, which may put a
    /// different node in an argument's place — what substituting a genvar for
    /// the integer it is bound to has to do.
    pub fn substitute(&mut self, replace: &dyn Fn(&mut Expression)) {
        for argument in &mut self.arguments {
            if let TaskArgument::Value(expression) = argument {
                replace(expression);
            }
        }
    }
}

fn unknown_task(name: &str) -> SimulationError {
    SimulationError::SystemTask(format!("unknown system task `${}`", name))
}

/// Whether `$name` names a system task or function this simulator implements.
///
/// Backed by the real resolvers rather than by a list kept alongside them: a
/// survey that maintains its own copy reports a feature as missing for ever
/// after it ships, which is how a triage heuristic quietly goes stale.
pub fn is_supported_system_name(name: &str) -> bool {
    resolve_task(name).is_ok() || SYSTEM_FUNCTIONS.contains(&name)
}

/// Resolves a `$name` to the task it means.
fn resolve_task(name: &str) -> Result<SystemTask, SimulationError> {
    // `$finish` starts with an `f` that is not the file-descriptor prefix and
    // `$monitoroff` ends with one that is not a radix, so the names that are
    // whole words are matched before the family is split.
    match name {
        "finish" => return Ok(SystemTask::Finish),
        "time" => return Ok(SystemTask::Time),
        "timeformat" => return Ok(SystemTask::TimeFormat),
        "monitoron" => return Ok(SystemTask::MonitorControl(true)),
        "monitoroff" => return Ok(SystemTask::MonitorControl(false)),
        _ => {}
    }

    let (descriptor, base, radix) = split_task_name(name);
    match base {
        "display" => Ok(SystemTask::Print(Print {
            newline: true,
            radix,
            descriptor,
        })),
        "write" => Ok(SystemTask::Print(Print {
            newline: false,
            radix,
            descriptor,
        })),
        // `$strobe` and `$monitor` end a line the way `$display` does; what
        // makes them different is only *when* the line is produced.
        "strobe" => Ok(SystemTask::Strobe(Print {
            newline: true,
            radix,
            descriptor,
        })),
        "monitor" => Ok(SystemTask::Monitor(Print {
            newline: true,
            radix,
            descriptor,
        })),
        // The memory-loading pair carry no descriptor and their radix is not a
        // default but the whole file format, so there is no `$readmem` and no
        // `$freadmemh`: those spellings are names nothing implements.
        "readmem" if !descriptor && radix.is_memory_file() => Ok(SystemTask::ReadMemory(radix)),
        "writemem" if !descriptor && radix.is_memory_file() => Ok(SystemTask::WriteMemory(radix)),
        _ => Err(unknown_task(name)),
    }
}

/// Splits a printing task's name into its three parts: whether it takes a file
/// descriptor, the task itself, and the radix its unadorned arguments print in.
///
/// No base name ends in `b`, `h` or `o`, so the suffix is unambiguous.
fn split_task_name(name: &str) -> (bool, &str, Radix) {
    let (descriptor, name) = match name.strip_prefix('f') {
        Some(rest) => (true, rest),
        None => (false, name),
    };
    let radix = match name.chars().last() {
        Some('b') => Radix::Binary,
        Some('h') => Radix::Hexadecimal,
        Some('o') => Radix::Octal,
        _ => Radix::Decimal,
    };
    let name = if radix == Radix::Decimal {
        name
    } else {
        &name[..name.len() - 1]
    };
    (descriptor, name, radix)
}

/// How `%t` renders a time value.
///
/// `$timeformat(units, precision, suffix, min_width)` sets all four. The
/// simulator's clock counts *ticks* and carries no timescale — nothing hands
/// `Simulator` the `` `timescale `` the front end recorded — so `units` is
/// range-checked and then taken to name the unit a tick already is. That is
/// the identity for the `` `timescale 1ns `` plus `$timeformat(-9, …)` pairing
/// that covers nearly every design using either, and it is why `units` is not
/// stored: with no second unit to convert between there is nothing to scale by.
///
/// What is left is real formatting: `precision` fractional digits after the
/// tick count, then `suffix`, right-aligned in `min_width`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TimeFormat {
    precision: usize,
    suffix: String,
    min_width: usize,
}

/// The field `%t` pads to when `$timeformat` has not said otherwise.
const DEFAULT_TIME_WIDTH: usize = 20;

/// The powers of ten `$timeformat` may name, seconds down to femtoseconds.
/// Anything outside is a named error rather than a silently odd unit.
const TIME_UNIT_BOUNDS: (i128, i128) = (-15, 2);

impl Default for TimeFormat {
    fn default() -> Self {
        TimeFormat {
            precision: 0,
            suffix: String::new(),
            min_width: DEFAULT_TIME_WIDTH,
        }
    }
}

impl TimeFormat {
    /// One time value as this format asks for it — without the field padding,
    /// which `%t` applies itself so an explicit `%12t` can override
    /// `min_width`.
    fn render(&self, value: &Register) -> String {
        if value.has_unknown() {
            return unknown(value);
        }
        // `%t` counts ticks, so a real time — `$realtime` — is rounded to one
        // before it is rendered. Nothing rescales either form; see the type's
        // own documentation.
        let rounded;
        let value = if value.is_real() {
            rounded = Register::integer_from_f64(value.to_f64().round(), REAL_WIDTH);
            &rounded
        } else {
            value
        };
        let mut text = decimal(value);
        if self.precision > 0 {
            text.push('.');
            for _ in 0..self.precision {
                text.push('0');
            }
        }
        text.push_str(&self.suffix);
        text
    }
}

/// The one standing `$monitor`.
///
/// The snapshot is what makes "an argument moved" answerable at the end of a
/// timestep: it holds the values the last printed line was made of, so a step
/// that moved nothing the monitor reads produces no line at all. It is `None`
/// until the first line is printed, because arming a `$monitor` does not print
/// — it *asks* to print at the end of the timestep it was armed in, so a value
/// the same block goes on to write is the value the first line carries.
#[derive(Clone, Debug)]
struct Monitor {
    call: TaskCall,
    snapshot: Option<Vec<Register>>,
    enabled: bool,
}

impl Monitor {
    /// How the armed call prints. It is on the call already, so keeping a
    /// second copy beside it could only ever disagree with it.
    fn print(&self) -> Print {
        match self.call.task {
            SystemTask::Monitor(print) => print,
            _ => unreachable!("only a `$monitor` is ever armed as one"),
        }
    }
}

/// What a system task acts on: where output goes, whether the design has
/// called `$finish`, and what is owed to the *end* of the current timestep
/// rather than to the moment a task ran.
///
/// What time it is lives on the [`StateStore`] instead, because
/// [`eval`] needs it too: `$time` is an expression operand as well as a task
/// argument, and one clock is better than two.
#[derive(Clone, Debug, Default)]
pub struct TaskContext {
    output: Output,
    finished: bool,
    /// The `$strobe` calls made in the current timestep, in the order they were
    /// made. Rendered by [`TaskContext::flush`] rather than when they ran,
    /// which is the whole difference between `$strobe` and `$display`.
    strobes: Vec<TaskCall>,
    /// The one armed `$monitor`. A second `$monitor` replaces it.
    monitor: Option<Monitor>,
    /// How `%t` renders.
    time_format: TimeFormat,
    /// Directories a relative `$readmemh` path is looked for in, after the
    /// process working directory. See [`TaskContext::resolve_read_path`].
    search_paths: Vec<PathBuf>,
}

impl TaskContext {
    pub fn new() -> Self {
        TaskContext::default()
    }

    /// Everything the design has printed.
    pub fn output(&self) -> &Output {
        &self.output
    }

    /// Whether the design has called `$finish`.
    pub fn finished(&self) -> bool {
        self.finished
    }

    /// Adds a directory to look in for a relative `$readmemh` / `$readmemb`
    /// path. See [`TaskContext::resolve_read_path`].
    pub fn add_search_path(&mut self, directory: impl Into<PathBuf>) {
        self.search_paths.push(directory.into());
    }

    /// Forgets everything one elaboration produced — the output, the `$finish`
    /// mark, the deferred queues and the `%t` format — while keeping the search
    /// path, which belongs to the caller rather than to the design.
    pub fn reset(&mut self) {
        self.output = Output::default();
        self.finished = false;
        self.strobes.clear();
        self.monitor = None;
        self.time_format = TimeFormat::default();
    }

    /// Whether anything is owed to the end of the current timestep.
    ///
    /// [`Simulator::advance`](crate::simulator::runner::Simulator::advance)
    /// asks once per timestep and `poke` once per call, so this sits on the hot
    /// path: a design that uses neither task pays a load and a branch.
    pub fn has_deferred(&self) -> bool {
        !self.strobes.is_empty() || self.monitor.is_some()
    }

    /// Runs what the current timestep deferred: every `$strobe` made in it, in
    /// call order, and then the armed `$monitor` if a value it last printed has
    /// moved.
    ///
    /// This is the end-of-timestep slot the two deferred tasks needed. It reads
    /// the store *after* the delta cycles have settled, which is exactly why a
    /// `$strobe` reports a value a `$display` on the same line would have
    /// missed.
    pub fn flush(&mut self, store: &StateStore) -> Result<(), SimulationError> {
        let strobes = std::mem::take(&mut self.strobes);
        for call in &strobes {
            let SystemTask::Strobe(print) = call.task else {
                unreachable!("only a `$strobe` is ever queued as one");
            };
            self.print_call(print, &call.arguments, store, &call.scope)?;
        }

        // Taking the monitor out keeps `self` free to print with; nothing
        // between here and putting it back can arm a different one.
        let Some(mut monitor) = self.monitor.take() else {
            return Ok(());
        };
        if monitor.enabled {
            let values = self.snapshot(&monitor.call, store)?;
            // A monitor that has not printed yet has no snapshot to differ
            // from, which is how the line owed to the timestep it was armed in
            // gets printed even though nothing moved.
            if monitor.snapshot.as_ref() != Some(&values) {
                monitor.snapshot = Some(values);
                self.print_call(
                    monitor.print(),
                    &monitor.call.arguments,
                    store,
                    &monitor.call.scope,
                )?;
            }
        }
        self.monitor = Some(monitor);
        Ok(())
    }

    /// Carries out one call, appending whatever it prints to the output.
    ///
    /// The store is taken by mutable reference because `$readmemh` writes one:
    /// a system task is not only an output. Everything else here reads it.
    pub fn run(&mut self, call: &TaskCall, store: &mut StateStore) -> Result<(), SimulationError> {
        match call.task {
            SystemTask::Print(print) => {
                self.print_call(print, &call.arguments, store, &call.scope)?
            }
            // A `$strobe` is kept rather than run: what it prints is whatever
            // its arguments hold once the timestep has finished moving.
            SystemTask::Strobe(_) => self.strobes.push(call.clone()),
            // Arming does not print. `$monitor` reports at the *end* of a
            // timestep, and that includes the timestep it was armed in, so
            // `a = 1; $monitor("%b", a);` reports the 1 rather than whatever
            // `a` held before the block ran. Leaving the snapshot empty is what
            // owes that first line to the next flush.
            SystemTask::Monitor(_) => {
                self.monitor = Some(Monitor {
                    call: call.clone(),
                    snapshot: None,
                    enabled: true,
                });
            }
            SystemTask::MonitorControl(enabled) => self.set_monitoring(enabled, store)?,
            SystemTask::ReadMemory(radix) => self.read_memory(call, radix, store)?,
            SystemTask::WriteMemory(radix) => self.write_memory(call, radix, store)?,
            SystemTask::TimeFormat => self.set_time_format(&call.arguments, store)?,
            // `$finish` takes an optional diagnostic level, which says how much
            // the simulator should report about itself on the way out.
            SystemTask::Finish => self.finished = true,
            SystemTask::Time => {}
        }
        Ok(())
    }

    /// `$monitoron` / `$monitoroff`. Turning monitoring back on reports at
    /// once, the way the LRM asks, and re-bases the snapshot so the end of that
    /// same timestep does not repeat the line.
    fn set_monitoring(&mut self, enabled: bool, store: &StateStore) -> Result<(), SimulationError> {
        let Some(mut monitor) = self.monitor.take() else {
            return Ok(());
        };
        let was_enabled = monitor.enabled;
        monitor.enabled = enabled;
        if enabled && !was_enabled {
            self.print_call(
                monitor.print(),
                &monitor.call.arguments,
                store,
                &monitor.call.scope,
            )?;
            monitor.snapshot = Some(self.snapshot(&monitor.call, store)?);
        }
        self.monitor = Some(monitor);
        Ok(())
    }

    /// The values an armed `$monitor`'s arguments hold now — the ones that can
    /// move, which is what the next end of timestep compares against.
    ///
    /// A format string is left out because it cannot move, and so is the
    /// simulated clock: a `$monitor` watches the *variables* it prints, so
    /// `$monitor("%0t %b", $time, a)` reports when `a` moves rather than once
    /// per timestep for ever.
    fn snapshot(
        &self,
        call: &TaskCall,
        store: &StateStore,
    ) -> Result<Vec<Register>, SimulationError> {
        call.arguments
            .iter()
            .filter_map(|argument| match argument {
                TaskArgument::Value(expression) if !is_clock(expression) => {
                    Some(eval(expression, store).map_err(Into::into))
                }
                _ => None,
            })
            .collect()
    }

    /// Formats an argument list and appends it to the output, checking the file
    /// descriptor first when the task is one of the `$f…` family.
    ///
    /// This is what `$display` does when it runs and what `$strobe` and
    /// `$monitor` do when [`TaskContext::flush`] reaches them, so all three
    /// print the same way and only their timing differs.
    fn print_call(
        &mut self,
        print: Print,
        arguments: &[TaskArgument],
        store: &StateStore,
        scope: &str,
    ) -> Result<(), SimulationError> {
        let arguments = if print.descriptor {
            let (descriptor, rest) = arguments.split_first().ok_or_else(|| {
                SimulationError::SystemTask(
                    "a `$f…` task needs a file descriptor as its first argument".to_string(),
                )
            })?;
            self.check_descriptor(descriptor, store)?;
            rest
        } else {
            arguments
        };
        let text = self.render(arguments, store, print.radix, scope)?;
        if print.newline {
            self.output.push_line(&text);
        } else {
            self.output.push(&text);
        }
        Ok(())
    }

    /// Checks that a `$f…` descriptor names the one channel this simulator has.
    ///
    /// There is no file I/O here and the output sink is a buffer, so the only
    /// descriptor that can be honoured is standard output: the multi-channel
    /// descriptor `1`, or the file descriptor `32'h8000_0001`. Anything else
    /// names a file nothing opened, and is an error saying so — writing it into
    /// the buffer would put a design's file output where a test looks for its
    /// terminal output, and dropping it would make a design that printed
    /// nothing look exactly like one that passed.
    fn check_descriptor(
        &self,
        argument: &TaskArgument,
        store: &StateStore,
    ) -> Result<(), SimulationError> {
        let value = self.value_of(argument, store)?;
        let channel = value
            .to_u128()
            .filter(|_| !value.has_unknown())
            .ok_or_else(|| {
                SimulationError::SystemTask(format!(
                    "a `$f…` file descriptor must be a known value, and `{}` is not",
                    value.to_binary()
                ))
            })?;

        // Bit 31 marks a file descriptor; without it the value is a bit mask of
        // multi-channel descriptors, whose bit 0 is standard output.
        const FILE_DESCRIPTOR: u128 = 1 << 31;
        let stdout = if channel & FILE_DESCRIPTOR != 0 {
            channel & !FILE_DESCRIPTOR == 1
        } else {
            channel == 1
        };
        if stdout {
            return Ok(());
        }
        Err(SimulationError::SystemTask(format!(
            "a `$f…` task can only write to standard output, and descriptor `{}` names a file nothing opened",
            channel
        )))
    }

    /// Formats an argument list the way `$display` does: a string argument is a
    /// format string and consumes as many of the arguments after it as it has
    /// specifiers; anything left over is printed in `radix`, the default the
    /// task's name asked for.
    fn render(
        &self,
        arguments: &[TaskArgument],
        store: &StateStore,
        radix: Radix,
        scope: &str,
    ) -> Result<String, SimulationError> {
        let mut text = String::new();
        let mut index = 0;
        while index < arguments.len() {
            match &arguments[index] {
                TaskArgument::Text(format) => {
                    index += 1;
                    self.render_format(format, arguments, &mut index, store, &mut text, scope)?;
                }
                argument => {
                    let value = self.value_of(argument, store)?;
                    // An argument with no specifier at all prints as a real
                    // whatever base the task's name asked for: there is no
                    // number of bits to show, and iverilog renders one to six
                    // significant digits — `$display(1.5)` is `1.50000`.
                    if value.is_real() {
                        text.push_str(&real_text(value.to_f64(), RealFormat::Bare, None));
                        index += 1;
                        continue;
                    }
                    let (rendered, width) = radix.render(&value);
                    text.push_str(&pad(rendered, width, ' '));
                    index += 1;
                }
            }
        }
        Ok(text)
    }

    /// Expands one format string, taking an argument from `index` for every
    /// specifier it carries.
    fn render_format(
        &self,
        format: &str,
        arguments: &[TaskArgument],
        index: &mut usize,
        store: &StateStore,
        text: &mut String,
        scope: &str,
    ) -> Result<(), SimulationError> {
        let mut characters = format.chars().peekable();
        while let Some(character) = characters.next() {
            if character != '%' {
                text.push(character);
                continue;
            }

            // `%%` is a literal percent sign and takes no argument.
            if characters.peek() == Some(&'%') {
                characters.next();
                text.push('%');
                continue;
            }

            // A leading `0` is a zero *fill*, exactly as in C: `%08d` pads with
            // zeros where `%8d` pads with spaces. What is left after it is the
            // width, and a width of `0` — plain `%0d`, `%0h` — means "as narrow
            // as the value allows", which is by far the most common specifier
            // in the wild. Any other width pads on the left.
            let zero_fill = characters.peek() == Some(&'0');
            if zero_fill {
                characters.next();
            }
            let mut width = String::new();
            while characters.peek().is_some_and(|c| c.is_ascii_digit()) {
                width.push(characters.next().expect("peeked digit must exist"));
            }
            let fill = if zero_fill { '0' } else { ' ' };
            let width: Option<usize> = match width.as_str() {
                "" => zero_fill.then_some(0),
                digits => Some(digits.parse().map_err(|_| {
                    SimulationError::SystemTask(format!("format width `{}` is too large", digits))
                })?),
            };

            // `.2` of `%5.2f` — how many digits follow the point. It belongs to
            // the real conversions and to nothing else, which is why it is read
            // here and consulted only by them.
            let mut precision: Option<usize> = None;
            if characters.peek() == Some(&'.') {
                characters.next();
                let mut digits = String::new();
                while characters.peek().is_some_and(|c| c.is_ascii_digit()) {
                    digits.push(characters.next().expect("peeked digit must exist"));
                }
                precision = Some(digits.parse().unwrap_or(0));
            }

            let specifier = characters
                .next()
                .ok_or_else(|| bad_format("a trailing `%` with no specifier"))?;

            // `%m` is the scope the call sits in, and it takes no argument at
            // all — so it is answered before one is fetched, and a `$display`
            // whose format string is nothing but `%m` needs none.
            if specifier.eq_ignore_ascii_case(&'m') {
                text.push_str(&pad(scope.to_string(), width.unwrap_or(0), fill));
                continue;
            }

            let argument = arguments.get(*index).ok_or_else(|| {
                bad_format(&format!(
                    "`%{}` has no argument left to format",
                    specifier.to_lowercase()
                ))
            })?;
            *index += 1;

            // `%f`, `%e` and `%g` are the real conversions, and they read any
            // argument: an integer is converted, so `$display("%f", 3)` is
            // `3.000000`. A width of zero pads nothing and does *not* drop the
            // decimals — `%0f` of 2.5 is `2.500000` — which is the one place
            // the "narrowest rendering" reading of `%0` does not apply.
            if let Some(format) = real_format(specifier) {
                let value = self.value_of(argument, store)?;
                let rendered = real_text(value.to_f64(), format, precision);
                // `%E` and `%G` are `%e` and `%g` in capitals, and that reaches
                // the whole rendering: C prints `INF` for `%E` of an infinity
                // (corpus `pr1699519`).
                let rendered = if specifier.is_ascii_uppercase() {
                    rendered.to_uppercase()
                } else {
                    rendered
                };
                text.push_str(&pad(rendered, width.unwrap_or(0), fill));
                continue;
            }

            // `%s` on a vector pads to the characters the vector *has* rather
            // than to the characters it spells, so a 32 bit register holding
            // `"A"` prints as `"   A"`; `%0s` is the same text with the padding
            // left off. A literal is already exactly as wide as it is.
            if specifier.eq_ignore_ascii_case(&'s') {
                let (rendered, default_width) = match argument {
                    TaskArgument::Text(literal) => (literal.clone(), 0),
                    other => {
                        let value = self.value_of(other, store)?;
                        // The bits of a real are an IEEE-754 encoding, not
                        // characters, so there is no text in one to print.
                        // iverilog warns and prints `<%s>`; there is no warning
                        // channel here, so it is an error that says so.
                        if value.is_real() {
                            return Err(bad_format("`%s` has no meaning for a real value"));
                        }
                        (ascii(&value), value.width().div_ceil(8))
                    }
                };
                text.push_str(&pad(rendered, width.unwrap_or(default_width), fill));
                continue;
            }

            // `%c` is one character: the low eight bits, whatever the value
            // is wider than that. An unknown byte has no character, so it
            // prints as a NUL would — nothing at all.
            if specifier.eq_ignore_ascii_case(&'c') {
                let value = self.value_of(argument, store)?;
                let rendered = match value.resize(8).to_u128() {
                    Some(code) => char::from(code as u8).to_string(),
                    None => String::new(),
                };
                text.push_str(&pad(rendered, width.unwrap_or(0), fill));
                continue;
            }

            // `%v` is the *strength* of each bit, `_` between them.
            if specifier.eq_ignore_ascii_case(&'v') {
                let value = self.value_of(argument, store)?;
                text.push_str(&pad(strengths(&value), width.unwrap_or(0), fill));
                continue;
            }

            // `%t` is a time value, and `$timeformat` — not the radix the task
            // name asked for — says how it reads. An explicit width still wins
            // over the one `$timeformat` set, so `%0t` never pads.
            if specifier.eq_ignore_ascii_case(&'t') {
                let value = self.value_of(argument, store)?;
                let rendered = self.time_format.render(&value);
                text.push_str(&pad(
                    rendered,
                    width.unwrap_or(self.time_format.min_width),
                    fill,
                ));
                continue;
            }

            let radix = Radix::from_specifier(specifier).ok_or_else(|| {
                bad_format(&format!(
                    "`%{}` is not a format this simulator understands",
                    specifier
                ))
            })?;
            let value = self.value_of(argument, store)?;
            let (rendered, default_width) = radix.render(&value);
            // `%0h` asks for the narrowest rendering, and in a base that pads
            // with digits that means dropping the leading zeros: `%h` of a
            // fourteen bit `65` is `0041` and `%0h` of it is `41`.
            let rendered = if width == Some(0) && radix.pads_with_digits() {
                without_leading_zeros(rendered)
            } else {
                rendered
            };
            text.push_str(&pad(rendered, width.unwrap_or(default_width), fill));
        }
        Ok(())
    }

    /// `$timeformat(units, precision, suffix, min_width)`, or `$timeformat`
    /// with no arguments, which puts `%t` back to its default.
    fn set_time_format(
        &mut self,
        arguments: &[TaskArgument],
        store: &StateStore,
    ) -> Result<(), SimulationError> {
        if arguments.is_empty() {
            self.time_format = TimeFormat::default();
            return Ok(());
        }
        if arguments.len() != 4 {
            return Err(SimulationError::SystemTask(format!(
                "`$timeformat` takes units, precision, a suffix and a minimum width, or nothing at all — not {} arguments",
                arguments.len()
            )));
        }

        // The units are checked even though nothing scales by them: a design
        // that names a unit this simulator could not mean should hear so.
        let units = self.integer_argument(&arguments[0], store, "`$timeformat` units")?;
        if units < TIME_UNIT_BOUNDS.0 || units > TIME_UNIT_BOUNDS.1 {
            return Err(SimulationError::SystemTask(format!(
                "`$timeformat` units must be a power of ten between {} and {}, not {}",
                TIME_UNIT_BOUNDS.0, TIME_UNIT_BOUNDS.1, units
            )));
        }
        let precision = self.field_argument(&arguments[1], store, "`$timeformat` precision")?;
        let suffix = self.text_argument(&arguments[2], store)?;
        let min_width = self.field_argument(&arguments[3], store, "`$timeformat` minimum width")?;

        self.time_format = TimeFormat {
            precision,
            suffix,
            min_width,
        };
        Ok(())
    }

    /// `$readmemh(file, memory)` / `$readmemb(file, memory[, start[, finish]])`
    /// — fills a memory from a text file of whitespace-separated words.
    ///
    /// The load runs from `start` towards `finish`, which default to the first
    /// and last addresses the memory declares, so a `mem [15:0]` loads
    /// downwards exactly as its declaration reads. An `@<hex>` entry in the
    /// file moves the load address without ending the load.
    fn read_memory(
        &mut self,
        call: &TaskCall,
        radix: Radix,
        store: &mut StateStore,
    ) -> Result<(), SimulationError> {
        let arguments = &call.arguments;
        if arguments.len() < 2 || arguments.len() > 4 {
            return Err(SimulationError::SystemTask(format!(
                "`$readmem…` takes a file name, a memory, and optionally a start and a finish address — not {} arguments",
                arguments.len()
            )));
        }

        let name = self.text_argument(&arguments[0], store)?;
        let memory_name = memory_argument(&arguments[1], "$readmem…")?;
        let memory = store
            .memory(&memory_name)
            .ok_or_else(|| not_a_memory(&memory_name))?;
        let (first, last) = memory.addresses();

        let start = match arguments.get(2) {
            Some(argument) => self.address_argument(argument, store, "start")?,
            None => first,
        };
        // Whether the design said where to stop decides what a file with more
        // words in it than that means. An explicit finish is an *instruction*
        // to stop there — `$readmemh(f, mem, 0, 3)` against an eight word file
        // loads four words and leaves the rest of the memory alone. A finish
        // that came from the declaration is a *description* of the memory, and
        // a file too big for it is a real mismatch between the two.
        let bounded = arguments.len() > 3;
        let finish = match arguments.get(3) {
            Some(argument) => self.address_argument(argument, store, "finish")?,
            None => last,
        };

        let path = self.resolve_read_path(&name)?;
        let text = fs::read_to_string(&path).map_err(|error| {
            SimulationError::SystemTask(format!(
                "`$readmem…` could not read `{}`: {}",
                path.display(),
                error
            ))
        })?;

        // Downwards is not an error: `mem [7:0]` declares its addresses that
        // way round and loads in that order.
        let step: i64 = if start <= finish { 1 } else { -1 };
        let mut cursor = start;
        for entry in memory_entries(&text, radix, &name)? {
            match entry {
                MemoryEntry::Address(address) => cursor = address,
                MemoryEntry::Word(digits) => {
                    if (step > 0 && cursor > finish) || (step < 0 && cursor < finish) {
                        if bounded {
                            return Ok(());
                        }
                        return Err(SimulationError::SystemTask(format!(
                            "`$readmem…` ran off the end of `{}` at address {}: `{}` holds more words than addresses {}..{} can take",
                            memory_name, cursor, name, start, finish
                        )));
                    }
                    store.set_word(&memory_name, cursor, &memory_word(&digits, radix)?);
                    cursor += step;
                }
            }
        }
        Ok(())
    }

    /// `$writememh(file, memory[, start[, finish]])` — the reverse of
    /// [`TaskContext::read_memory`], one word per line.
    ///
    /// The path is taken as written: a file being created cannot be searched
    /// for, so the search path plays no part and a relative name lands under
    /// the process working directory.
    fn write_memory(
        &self,
        call: &TaskCall,
        radix: Radix,
        store: &StateStore,
    ) -> Result<(), SimulationError> {
        let arguments = &call.arguments;
        if arguments.len() < 2 || arguments.len() > 4 {
            return Err(SimulationError::SystemTask(format!(
                "`$writemem…` takes a file name, a memory, and optionally a start and a finish address — not {} arguments",
                arguments.len()
            )));
        }

        let name = self.text_argument(&arguments[0], store)?;
        let memory_name = memory_argument(&arguments[1], "$writemem…")?;
        let memory = store
            .memory(&memory_name)
            .ok_or_else(|| not_a_memory(&memory_name))?;
        let (first, last) = memory.addresses();
        let start = match arguments.get(2) {
            Some(argument) => self.address_argument(argument, store, "start")?,
            None => first,
        };
        let finish = match arguments.get(3) {
            Some(argument) => self.address_argument(argument, store, "finish")?,
            None => last,
        };

        let step: i64 = if start <= finish { 1 } else { -1 };
        let mut text = String::new();
        let mut cursor = start;
        loop {
            let word = memory.word(Some(cursor));
            text.push_str(&match radix {
                Radix::Binary => binary(&word),
                _ => hex(&word),
            });
            text.push('\n');
            if cursor == finish {
                break;
            }
            cursor += step;
        }

        fs::write(&name, text).map_err(|error| {
            SimulationError::SystemTask(format!(
                "`$writemem…` could not write `{}`: {}",
                name, error
            ))
        })
    }

    /// Finds the file a relative `$readmem…` path names.
    ///
    /// A design says `$readmemh("data.hex", mem)` and means "next to me", but
    /// nothing hands this simulator the file the design was parsed from — a
    /// `Simulator` is built from modules, not from paths. So a relative name is
    /// resolved against the process working directory first and then against
    /// every directory a caller added with
    /// [`Simulator::add_search_path`](crate::simulator::runner::Simulator::add_search_path),
    /// which is the seam a harness that *does* know where the design came from
    /// uses. Finding nothing is an error naming the file and everywhere it was
    /// looked for — never an empty memory, which would leave a design reading
    /// `x` and looking exactly like one that simply ran.
    fn resolve_read_path(&self, name: &str) -> Result<PathBuf, SimulationError> {
        let path = Path::new(name);
        if path.is_file() {
            return Ok(path.to_path_buf());
        }
        if !path.is_absolute() {
            for directory in &self.search_paths {
                let candidate = directory.join(path);
                if candidate.is_file() {
                    return Ok(candidate);
                }
            }
        }

        let mut tried = vec![match std::env::current_dir() {
            Ok(directory) => directory.display().to_string(),
            Err(_) => "the working directory".to_string(),
        }];
        tried.extend(
            self.search_paths
                .iter()
                .map(|directory| directory.display().to_string()),
        );
        Err(SimulationError::SystemTask(format!(
            "`$readmem…` cannot find `{}`; looked in {}",
            name,
            tried.join(", ")
        )))
    }

    /// A string-valued argument: a literal, or a `reg` holding packed ASCII,
    /// which is how a design that builds a file name at run time passes one.
    fn text_argument(
        &self,
        argument: &TaskArgument,
        store: &StateStore,
    ) -> Result<String, SimulationError> {
        match argument {
            TaskArgument::Text(text) => Ok(text.clone()),
            value => Ok(ascii(&self.value_of(value, store)?)),
        }
    }

    /// A whole-number argument, read with the sign its value carries.
    fn integer_argument(
        &self,
        argument: &TaskArgument,
        store: &StateStore,
        what: &str,
    ) -> Result<i128, SimulationError> {
        let value = self.value_of(argument, store)?;
        integer_value(&value).ok_or_else(|| {
            SimulationError::SystemTask(format!(
                "{} must be a known whole number, and `{}` is not",
                what,
                value.to_binary()
            ))
        })
    }

    /// A field width or a digit count: a whole number that fits in a field this
    /// simulator is prepared to pad out.
    fn field_argument(
        &self,
        argument: &TaskArgument,
        store: &StateStore,
        what: &str,
    ) -> Result<usize, SimulationError> {
        let value = self.integer_argument(argument, store, what)?;
        if value < 0 || value > MAX_TIME_FIELD {
            return Err(SimulationError::SystemTask(format!(
                "{} must be between 0 and {}, not {}",
                what, MAX_TIME_FIELD, value
            )));
        }
        Ok(value as usize)
    }

    /// A memory address bound for `$readmem…` / `$writemem…`.
    fn address_argument(
        &self,
        argument: &TaskArgument,
        store: &StateStore,
        what: &str,
    ) -> Result<i64, SimulationError> {
        let value = self.integer_argument(argument, store, &format!("a `{}` address", what))?;
        i64::try_from(value).map_err(|_| {
            SimulationError::SystemTask(format!("the `{}` address {} is out of range", what, value))
        })
    }

    fn value_of(
        &self,
        argument: &TaskArgument,
        store: &StateStore,
    ) -> Result<Register, SimulationError> {
        match argument {
            TaskArgument::Value(expression) => Ok(eval(expression, store)?),
            // A string *is* a value — eight bits a character, most significant
            // character first — and `$display("%d", "A")` is 65. It reaches
            // here as text rather than as an expression because a task tries
            // the format-string reading of a string argument first, which is
            // what it has to do for the one that is a format string.
            TaskArgument::Text(text) => Ok(string_bits(text)),
        }
    }
}

fn bad_format(what: &str) -> SimulationError {
    SimulationError::SystemTask(format!("in a system task format string, {}", what))
}

/// The widest field `$timeformat` may ask `%t` to pad to, and the most
/// fractional digits it may ask for. A bound rather than a machine integer's
/// range because the padding is a real allocation.
const MAX_TIME_FIELD: i128 = 1024;

/// A whole number read out of a register with the sign the value carries.
/// `None` for an unknown value or one too wide to hold.
fn integer_value(register: &Register) -> Option<i128> {
    if register.has_unknown() {
        return None;
    }
    if register.is_signed() {
        register.to_i128()
    } else {
        register
            .to_u128()
            .and_then(|value| i128::try_from(value).ok())
    }
}

/// The name of the memory a `$readmem…` or `$writemem…` call is about.
///
/// It has to be a plain identifier: the argument names a *memory*, not a value,
/// and there is nothing an expression could evaluate to that would name one.
fn memory_argument(argument: &TaskArgument, task: &str) -> Result<String, SimulationError> {
    match argument {
        TaskArgument::Value(Expression::Identifier(identifier)) => Ok(identifier.name.clone()),
        _ => Err(SimulationError::SystemTask(format!(
            "`{}`'s second argument names the memory, and must be a plain identifier",
            task
        ))),
    }
}

fn not_a_memory(name: &str) -> SimulationError {
    SimulationError::SystemTask(format!(
        "`{}` is not a memory, so there is nothing for a memory file to be read into or written from",
        name
    ))
}

/// One thing a memory file says: a word to load, or an address to load the next
/// word at.
#[derive(Clone, Debug, PartialEq, Eq)]
enum MemoryEntry {
    /// `@1f` — the load address moves here and the load carries on.
    Address(i64),
    /// A word, still as digits: how wide it ends up is the memory's business.
    Word(String),
}

/// Reads a memory file: whitespace-separated words, `//` and `/* */` comments,
/// and `@<hex>` address jumps.
fn memory_entries(
    text: &str,
    radix: Radix,
    name: &str,
) -> Result<Vec<MemoryEntry>, SimulationError> {
    let stripped = strip_memory_comments(text, name)?;
    stripped
        .split_whitespace()
        .map(|token| match token.strip_prefix('@') {
            Some(digits) => i64::from_str_radix(digits, 16)
                .map(MemoryEntry::Address)
                .map_err(|_| {
                    SimulationError::SystemTask(format!(
                        "in memory file `{}`, `@{}` is not a hexadecimal address",
                        name, digits
                    ))
                }),
            // The digits are only checked against the radix once the memory's
            // width is known, which is where an unreadable one is reported.
            None => Ok(MemoryEntry::Word(token.to_string())),
        })
        .collect::<Result<Vec<_>, _>>()
        .and_then(|entries| {
            // Checking every word here rather than at load time means a file
            // with a typo halfway through does not leave half a memory loaded.
            for entry in &entries {
                if let MemoryEntry::Word(digits) = entry {
                    check_memory_digits(digits, radix, name)?;
                }
            }
            Ok(entries)
        })
}

/// Replaces every `//` and `/* */` comment with the whitespace that separates
/// the tokens around it. An unterminated block comment is a named error: the
/// alternative is silently swallowing the rest of the file.
fn strip_memory_comments(text: &str, name: &str) -> Result<String, SimulationError> {
    let mut stripped = String::with_capacity(text.len());
    let mut characters = text.chars().peekable();
    while let Some(character) = characters.next() {
        if character != '/' {
            stripped.push(character);
            continue;
        }
        match characters.peek() {
            Some('/') => {
                for skipped in characters.by_ref() {
                    if skipped == '\n' {
                        break;
                    }
                }
                stripped.push('\n');
            }
            Some('*') => {
                characters.next();
                let mut previous = '\0';
                let mut closed = false;
                for skipped in characters.by_ref() {
                    if previous == '*' && skipped == '/' {
                        closed = true;
                        break;
                    }
                    previous = skipped;
                }
                if !closed {
                    return Err(SimulationError::SystemTask(format!(
                        "memory file `{}` ends inside a `/*` comment",
                        name
                    )));
                }
                stripped.push(' ');
            }
            _ => stripped.push(character),
        }
    }
    Ok(stripped)
}

fn check_memory_digits(digits: &str, radix: Radix, name: &str) -> Result<(), SimulationError> {
    memory_word(digits, radix).map(|_| ()).map_err(|_| {
        SimulationError::SystemTask(format!(
            "in memory file `{}`, `{}` is not a {} word",
            name,
            digits,
            match radix {
                Radix::Binary => "binary",
                _ => "hexadecimal",
            }
        ))
    })
}

/// One word of a memory file, at the width its digits give it.
/// [`Memory::set_word`](crate::simulator::state_store::Memory::set_word) then
/// sizes it to the memory: a short word is zero-filled on the left and a long
/// one loses its most significant digits, which is what `$readmemh` does.
///
/// `x`, `z` and `?` are digits like any other: a memory file may leave a word
/// or a nibble unknown, and `?` is the spelling `z` also goes by.
fn memory_word(digits: &str, radix: Radix) -> Result<Register, SimulationError> {
    let mut bits: Vec<u8> = Vec::with_capacity(digits.len() * 4);
    for character in digits.chars() {
        if character == '_' {
            continue;
        }
        let (code, digit) = match character {
            'x' | 'X' => (Some(X), None),
            'z' | 'Z' | '?' => (Some(Z), None),
            _ => (
                None,
                character.to_digit(if radix == Radix::Binary { 2 } else { 16 }),
            ),
        };
        match (code, digit) {
            (Some(code), _) if radix == Radix::Binary => bits.push(code),
            (Some(code), _) => bits.extend([code; 4]),
            (_, Some(value)) if radix == Radix::Binary => {
                bits.push(if value == 1 { ONE } else { ZERO })
            }
            (_, Some(value)) => {
                bits.extend((0..4).rev().map(
                    |bit| {
                        if (value >> bit) & 1 == 1 {
                            ONE
                        } else {
                            ZERO
                        }
                    },
                ))
            }
            _ => {
                return Err(SimulationError::SystemTask(format!(
                    "`{}` is not a digit a memory file can hold",
                    character
                )))
            }
        }
    }
    if bits.is_empty() {
        return Err(SimulationError::SystemTask(
            "a memory file word has no digits".to_string(),
        ));
    }
    Ok(Register::from_bits(bits))
}

/// Left-pads to `width` with `fill`, which a wider value simply overflows.
///
/// A zero fill goes *after* a minus sign — `%08d` of -10 is `-0000010`, the way
/// C writes it — because a sign in the middle of a number is not a number.
fn pad(text: String, width: usize, fill: char) -> String {
    let length = text.chars().count();
    if length >= width {
        return text;
    }
    let mut padded = String::with_capacity(width);
    let body = match text.strip_prefix('-') {
        Some(rest) if fill == '0' => {
            padded.push('-');
            rest
        }
        _ => &text,
    };
    for _ in 0..width - length {
        padded.push(fill);
    }
    padded.push_str(body);
    padded
}

/// The strength of every bit of `value`, most significant first, `_` between
/// them: `St0_Pu1_Pu1_St0`.
///
/// **Only the two strengths a value alone can tell are reported.** A bit that
/// is `z` is driven by nothing, which is `HiZ`; every other bit is reported as
/// `St`, because an ordinary continuous assignment and a gate both drive at
/// `strong` and that is what almost every design has. A `pullup`, a
/// `tri0`/`tri1` or an `assign (pull1, strong0)` really is weaker, and this
/// prints `St1` where iverilog prints `Pu1` — the store keeps a *value* per
/// signal and not a strength, so there is nothing here to read the difference
/// from. Corpus `multi_bit_strength` is that gap.
fn strengths(value: &Register) -> String {
    let bit = |code: u8| match code {
        ZERO => "St0",
        ONE => "St1",
        X => "StX",
        _ => "HiZ",
    };
    (0..value.width())
        .rev()
        .map(|index| bit(value.bit_from_lsb(index).unwrap_or(X)))
        .collect::<Vec<_>>()
        .join("_")
}

/// The real conversion a `%` specifier asks for, or `None` if it is not one.
fn real_format(specifier: char) -> Option<RealFormat> {
    match specifier {
        'f' | 'F' => Some(RealFormat::Fixed),
        'e' | 'E' => Some(RealFormat::Scientific),
        'g' | 'G' => Some(RealFormat::Shortest),
        _ => None,
    }
}

/// How a real prints: the three C conversions Verilog inherits, plus the one
/// an argument with no specifier at all takes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RealFormat {
    /// `%f` — a fixed point number, six decimals unless a precision says
    /// otherwise.
    Fixed,
    /// `%e` — a mantissa and a two digit exponent: `3.000000e+00`.
    Scientific,
    /// `%g` — whichever of the two is shorter, with the trailing zeros dropped.
    Shortest,
    /// An argument printed with no specifier. It is `%g` with C's `#` flag —
    /// six *significant* digits with the trailing zeros **kept** — which is why
    /// iverilog prints `1.5` as `1.50000` and `400.0` as `400.000`.
    Bare,
}

/// The number of significant digits `%g` keeps when nothing asks for another.
const DEFAULT_SIGNIFICANT_DIGITS: usize = 6;

/// The number of decimals `%f` and `%e` print when nothing asks for another.
const DEFAULT_DECIMALS: usize = 6;

/// One real as `format` renders it.
///
/// A value with no digits to print — an infinity or a NaN — renders as the word
/// for it, which is what C does and what iverilog prints for `1.0/0.0`.
fn real_text(value: f64, format: RealFormat, precision: Option<usize>) -> String {
    if !value.is_finite() {
        return match (value.is_nan(), value.is_sign_negative()) {
            (true, _) => "nan".to_string(),
            (false, true) => "-inf".to_string(),
            (false, false) => "inf".to_string(),
        };
    }
    match format {
        RealFormat::Fixed => format!("{:.*}", precision.unwrap_or(DEFAULT_DECIMALS), value),
        RealFormat::Scientific => scientific(value, precision.unwrap_or(DEFAULT_DECIMALS)),
        RealFormat::Shortest | RealFormat::Bare => {
            // C's `%g`: `digits` significant figures, printed as `%e` when the
            // exponent is outside the range a fixed point rendering reads well
            // in, and as `%f` when it is not. Only `%g` proper drops the
            // trailing zeros afterwards.
            let digits = precision.unwrap_or(DEFAULT_SIGNIFICANT_DIGITS).max(1);
            let exponent = decimal_exponent(value, digits);
            let text = if exponent < -4 || exponent >= digits as i32 {
                scientific(value, digits - 1)
            } else {
                // The exponent is at least -4 here, so the count of decimals
                // this asks for is never negative.
                format!("{:.*}", (digits as i32 - 1 - exponent) as usize, value)
            };
            match format {
                RealFormat::Shortest => without_trailing_zeros(text),
                _ => text,
            }
        }
    }
}

/// `value` in C's `%e` form: one digit, `precision` decimals, then an exponent
/// of at least two digits with its sign.
///
/// Rust writes `3e0` where C writes `3.000000e+00`, so the two halves are
/// assembled here.
fn scientific(value: f64, precision: usize) -> String {
    let mantissa = format!("{:.*e}", precision, value);
    match mantissa.split_once('e') {
        Some((digits, exponent)) => {
            let exponent: i32 = exponent.parse().unwrap_or(0);
            format!(
                "{}e{}{:02}",
                digits,
                if exponent < 0 { '-' } else { '+' },
                exponent.abs()
            )
        }
        None => mantissa,
    }
}

/// The exponent `%g` decides on: the power of ten of `value` *after* it has
/// been rounded to `digits` significant figures, so that `999999.9` at six
/// figures is `1.00000e+06` rather than a seven digit fixed point number.
fn decimal_exponent(value: f64, digits: usize) -> i32 {
    if value == 0.0 {
        return 0;
    }
    format!("{:.*e}", digits.saturating_sub(1), value)
        .split_once('e')
        .and_then(|(_, exponent)| exponent.parse().ok())
        .unwrap_or(0)
}

/// A rendering with its trailing zeros dropped, and the decimal point with them
/// when nothing is left after it. `%g` does this and the bare form deliberately
/// does not.
fn without_trailing_zeros(text: String) -> String {
    let (digits, exponent) = match text.split_once('e') {
        Some((digits, exponent)) => (digits, Some(exponent)),
        None => (text.as_str(), None),
    };
    if !digits.contains('.') {
        return text;
    }
    let trimmed = digits.trim_end_matches('0').trim_end_matches('.');
    match exponent {
        Some(exponent) => format!("{}e{}", trimmed, exponent),
        None => trimmed.to_string(),
    }
}

/// The same digits with the leading zeros dropped, which is what a width of
/// `0` asks for. All of them being zero leaves one behind: a number has to have
/// a digit. An unknown digit is not a zero and stops the trim, so `%0h` of
/// `12'bxxxx_0000_0001` is still `x01`.
fn without_leading_zeros(text: String) -> String {
    match text.trim_start_matches('0') {
        "" => "0".to_string(),
        trimmed => trimmed.to_string(),
    }
}

/// How many decimal digits the widest value of `bits` bits takes, counting the
/// room a signed value needs for its sign.
///
/// A signed value's widest field is its most *negative* one — a 32 bit
/// `integer` reaches -2147483648, eleven characters — so a signed field is one
/// wider than the unsigned field of the same magnitude, not of the same width.
fn decimal_width(bits: usize, signed: bool) -> usize {
    match (signed, bits) {
        (_, 0) => 1,
        // `2**bits - 1` has as many digits as `2**bits` for every width but
        // zero, since no power of two past one is a power of ten.
        (false, bits) => digits_of_power_of_two(bits),
        (true, bits) => digits_of_power_of_two(bits - 1) + 1,
    }
}

/// How many decimal digits `2**power` takes.
fn digits_of_power_of_two(power: usize) -> usize {
    match 1u128.checked_shl(power as u32) {
        Some(value) => value.to_string().len(),
        // log10(2) * power, which is exact enough to size a value no radix
        // conversion in this crate can produce anyway.
        None => (power as f64 * std::f64::consts::LOG10_2).floor() as usize + 1,
    }
}

/// The character a group of bits renders as when any of them is unknown, or
/// `None` when they are all known.
///
/// Verilog's rule is about *agreement*. A digit whose bits are all `x`, or all
/// `z`, prints that in lower case; a digit that mixes an unknown bit with a
/// known one — or an `x` with a `z` — prints in upper case. So `4'bxxxx` is `x`
/// and `4'bzzxx` is `X`: the capital says there is something in this digit that
/// the letter cannot show. An `x` outranks a `z`, so any `x` at all makes the
/// digit one of the `x` pair.
fn unknown_digit(codes: impl Iterator<Item = u8>) -> Option<char> {
    let (mut any_x, mut any_z, mut any_known) = (false, false, false);
    for code in codes {
        match code {
            X => any_x = true,
            Z => any_z = true,
            _ => any_known = true,
        }
    }
    match (any_x, any_z, any_known) {
        (false, false, _) => None,
        (true, false, false) => Some('x'),
        (false, true, false) => Some('z'),
        (true, _, _) => Some('X'),
        _ => Some('Z'),
    }
}

/// `x`, `X`, `z` or `Z` — what an unknown value renders as in a radix that
/// shows the whole value as one digit. See [`unknown_digit`] for which case.
fn unknown(register: &Register) -> String {
    unknown_digit(bit_codes(register, 0..register.width()))
        .expect("the caller has already found an unknown bit")
        .to_string()
}

/// The bit codes at `range`, counted from the least significant end.
fn bit_codes(register: &Register, range: std::ops::Range<usize>) -> impl Iterator<Item = u8> + '_ {
    range.map(|index| {
        register
            .bit_from_lsb(index)
            .expect("the range is inside the register's width")
    })
}

/// A value in a radix that shows `bits` of it per digit, most significant digit
/// first.
///
/// The top digit is short when the width is not a whole number of digits, and
/// it is judged on the bits it actually has rather than on a padded nibble:
/// `5'bxxxxx` is `xx` in hexadecimal, not `Xx`.
fn digits(register: &Register, bits: usize) -> String {
    let count = register.width().div_ceil(bits);
    let mut text = String::with_capacity(count);
    for digit in (0..count).rev() {
        let low = digit * bits;
        let range = low..(low + bits).min(register.width());
        text.push(match unknown_digit(bit_codes(register, range.clone())) {
            Some(character) => character,
            None => {
                let value = bit_codes(register, range)
                    .enumerate()
                    .fold(0u32, |value, (place, code)| {
                        value | ((code as u32 & 1) << place)
                    });
                char::from_digit(value, 1 << bits).expect("a digit of this many bits")
            }
        });
    }
    text
}

/// Decimal, going through `to_u128` rather than `Register::to_decimal` because
/// that one accumulates into a machine integer. A value wider than 128 bits is
/// printed from its low 128 bits.
///
/// A *signed* value prints as a two's complement number, minus sign and all:
/// `%d` on an `integer` holding -12 has to read `-12`, not `4294967284`.
fn decimal(register: &Register) -> String {
    if register.has_unknown() {
        return unknown(register);
    }
    let narrowed;
    let register = if register.width() > 128 {
        narrowed = register.resize(128);
        &narrowed
    } else {
        register
    };
    if register.is_signed() {
        register
            .to_i128()
            .expect("a known value of at most 128 bits")
            .to_string()
    } else {
        register
            .to_u128()
            .expect("a known value of at most 128 bits")
            .to_string()
    }
}

/// Binary, which is the one radix whose digit is a single bit — so a digit is
/// never a mixture and an unknown one is always lower case.
fn binary(register: &Register) -> String {
    register.to_binary()
}

/// Hexadecimal, lower case the way Verilog prints it.
fn hex(register: &Register) -> String {
    digits(register, 4)
}

/// Octal, a digit to every three bits.
fn octal(register: &Register) -> String {
    digits(register, 3)
}

/// A register read as text, most significant byte first, the way `%s` prints a
/// vector.
///
/// *Leading* NULs are dropped, which is what makes `"ok"` come back out of a
/// register wide enough to have been zero extended. A NUL with text on both
/// sides is a space instead: it is a character of the value, and dropping it
/// would close a gap the vector really has.
fn ascii(register: &Register) -> String {
    let bits = register.to_binary();
    let padding = (8 - bits.len() % 8) % 8;
    let bits = format!("{}{}", "0".repeat(padding), bits);
    bits.as_bytes()
        .chunks(8)
        .map(|byte| {
            let code = byte.iter().fold(0u32, |code, bit| {
                (code << 1) | if *bit == b'1' { 1 } else { 0 }
            });
            char::from_u32(code).unwrap_or('?')
        })
        .skip_while(|character| *character == '\0')
        .map(|character| if character == '\0' { ' ' } else { character })
        .collect()
}

/// Whether an argument is the simulated clock rather than something the design
/// drives. See [`TaskContext::snapshot`].
fn is_clock(expression: &Expression) -> bool {
    matches!(
        expression,
        Expression::SystemFunctionCall(name, _)
            if matches!(name.as_str(), "time" | "stime" | "realtime")
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parsers::behavior::parse_system_task;

    fn store_with(signals: &[(&str, &str)]) -> StateStore {
        let mut store = StateStore::new();
        for (name, bits) in signals {
            let register = Register::from_binary(bits);
            let range = (register.width() as i64 - 1, 0);
            store.set_ranged(*name, register, range);
        }
        store
    }

    /// [`store_with`], for signals whose declaration carried a `signed`
    /// qualifier — which `%d` can tell apart, since a signed field has to leave
    /// room for a minus sign.
    fn signed_store_with(signals: &[(&str, &str)]) -> StateStore {
        let mut store = StateStore::new();
        for (name, bits) in signals {
            let register = Register::from_binary(bits);
            let range = (register.width() as i64 - 1, 0);
            store.declare_signed(*name, range, true);
            store.set_ranged(*name, register, range);
        }
        store
    }

    /// Runs one `$…;` statement against a store and returns everything printed.
    fn printed(source: &str, store: &StateStore) -> String {
        let mut context = TaskContext::new();
        let mut store = store.clone();
        run_in(&mut context, source, &mut store);
        context.output().text().to_string()
    }

    fn run_in(context: &mut TaskContext, source: &str, store: &mut StateStore) {
        let (remaining, call) = parse_system_task(source).expect("task should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        let call = TaskCall::compile(&call).expect("task should compile");
        context.run(&call, store).expect("task should run");
    }

    fn error(source: &str) -> String {
        let (_, call) = parse_system_task(source).expect("task should parse");
        let mut store = store_with(&[]);
        let compiled = match TaskCall::compile(&call) {
            Err(error) => return error.to_string(),
            Ok(compiled) => compiled,
        };
        TaskContext::new()
            .run(&compiled, &mut store)
            .expect_err("task should fail")
            .to_string()
    }

    #[test]
    fn test_display_prints_a_line() {
        assert_eq!(
            printed(r#"$display("PASSED");"#, &store_with(&[])),
            "PASSED\n"
        );
    }

    #[test]
    fn test_write_does_not_end_the_line() {
        let mut store = store_with(&[]);
        let mut context = TaskContext::new();
        run_in(&mut context, r#"$write("PAS");"#, &mut store);
        run_in(&mut context, r#"$write("SED");"#, &mut store);
        assert_eq!(context.output().text(), "PASSED");
        assert_eq!(context.output().lines(), vec!["PASSED"]);

        run_in(&mut context, r#"$display("!");"#, &mut store);
        assert_eq!(context.output().text(), "PASSED!\n");
    }

    #[test]
    fn test_decimal_formats() {
        let store = store_with(&[("a", "00001010")]);
        // `%0d` is minimum width; a bare `%d` pads to the width the signal's
        // widest value would need, which for eight bits is three digits.
        assert_eq!(printed(r#"$display("%0d", a);"#, &store), "10\n");
        assert_eq!(printed(r#"$display("%d", a);"#, &store), " 10\n");
        assert_eq!(printed(r#"$display("%5d", a);"#, &store), "   10\n");
    }

    #[test]
    fn test_radix_formats() {
        let store = store_with(&[("a", "10101100")]);
        assert_eq!(printed(r#"$display("%b", a);"#, &store), "10101100\n");
        // Verilog prints hexadecimal in lower case.
        assert_eq!(printed(r#"$display("%h", a);"#, &store), "ac\n");
        assert_eq!(printed(r#"$display("%o", a);"#, &store), "254\n");
    }

    #[test]
    fn test_hex_of_a_width_that_is_not_a_whole_number_of_nibbles() {
        // Five bits is two nibbles, and the value has to land in the low one.
        let store = store_with(&[("a", "10001")]);
        assert_eq!(printed(r#"$display("%h", a);"#, &store), "11\n");
    }

    #[test]
    fn test_string_and_percent_formats() {
        let store = store_with(&[]);
        assert_eq!(printed(r#"$display("%s", "ok");"#, &store), "ok\n");
        assert_eq!(printed(r#"$display("100%%");"#, &store), "100%\n");
        assert_eq!(printed(r#"$display("");"#, &store), "\n");
        assert_eq!(printed(r#"$display();"#, &store), "\n");
    }

    #[test]
    fn test_string_format_of_a_vector_reads_it_as_text() {
        let store = store_with(&[("word", "0100111101001011")]);
        assert_eq!(printed(r#"$display("%s", word);"#, &store), "OK\n");
    }

    #[test]
    fn test_several_arguments_and_text_between_them() {
        let store = store_with(&[("a", "0011"), ("b", "0101")]);
        assert_eq!(
            printed(r#"$display("a=%0d b=%0d", a, b);"#, &store),
            "a=3 b=5\n"
        );
    }

    #[test]
    fn test_an_expression_argument_is_evaluated() {
        let store = store_with(&[("a", "0011"), ("b", "0101")]);
        assert_eq!(printed(r#"$display("%0d", a + b);"#, &store), "8\n");
    }

    #[test]
    fn test_unknown_and_high_impedance_values() {
        let store = store_with(&[("a", "01x1"), ("b", "zzzz")]);
        // Binary shows which bits are unknown. `%d` shows the whole value as
        // one character, and `%h` one character per nibble — upper case for a
        // nibble that mixes an unknown bit with a known one, since a lower case
        // `x` would claim the whole nibble was unknown.
        assert_eq!(printed(r#"$display("%b", a);"#, &store), "01x1\n");
        assert_eq!(printed(r#"$display("%0d", a);"#, &store), "X\n");
        assert_eq!(printed(r#"$display("%h", a);"#, &store), "X\n");
        assert_eq!(printed(r#"$display("%0d", b);"#, &store), "z\n");
        assert_eq!(printed(r#"$display("%b", b);"#, &store), "zzzz\n");
    }

    #[test]
    fn test_arguments_past_the_format_string_print_in_decimal() {
        let store = store_with(&[("a", "0011")]);
        // A four-bit value's widest decimal is two digits, so the default
        // width pads `3` by one.
        assert_eq!(printed(r#"$display("x", a);"#, &store), "x 3\n");
    }

    /// A signed value prints in decimal as the two's complement number it is.
    #[test]
    fn test_decimal_prints_a_signed_value_with_its_sign() {
        let store = store_with(&[]);
        assert_eq!(printed(r#"$display("%0d", -12);"#, &store), "-12\n");
        // The same bits read as unsigned are the large number they spell.
        assert_eq!(
            printed(r#"$display("%0d", $unsigned(-12));"#, &store),
            "4294967284\n"
        );
    }

    #[test]
    fn test_time_is_an_argument() {
        let mut store = store_with(&[]);
        store.set_time(42);
        let mut context = TaskContext::new();
        run_in(&mut context, r#"$display("t=%0d", $time);"#, &mut store);
        assert_eq!(context.output().text(), "t=42\n");
    }

    #[test]
    fn test_a_bare_system_function_argument_is_evaluated() {
        let mut store = store_with(&[]);
        store.set_time(9);
        assert_eq!(printed(r#"$display("%0d", $stime);"#, &store), "9\n");
        // `$random` reads the store's stream: a bare `$name` argument is an
        // expression like any other, not a second table of task names. It
        // draws a *signed* integer, so half the stream prints negative.
        let drawn = printed(r#"$display("%0d", $random);"#, &store);
        assert!(
            drawn.trim().parse::<i32>().is_ok(),
            "expected a number, got {:?}",
            drawn
        );
    }

    /// `$displayb` / `$displayh` / `$displayo` are `$display` with a different
    /// default base for an argument that carries no format specifier.
    #[test]
    fn test_radix_variants_of_display() {
        let store = store_with(&[("a", "10101100")]);
        assert_eq!(printed(r#"$display(a);"#, &store), "172\n");
        assert_eq!(printed(r#"$displayb(a);"#, &store), "10101100\n");
        assert_eq!(printed(r#"$displayh(a);"#, &store), "ac\n");
        assert_eq!(printed(r#"$displayo(a);"#, &store), "254\n");
    }

    /// `$write` has the same family, and still does not end the line.
    #[test]
    fn test_radix_variants_of_write() {
        let mut store = store_with(&[("a", "10101100")]);
        let mut context = TaskContext::new();
        run_in(&mut context, r#"$writeb(a);"#, &mut store);
        run_in(&mut context, r#"$writeh(a);"#, &mut store);
        run_in(&mut context, r#"$writeo(a);"#, &mut store);
        assert_eq!(context.output().text(), "10101100ac254");
    }

    /// A specifier in a format string says what it says; the task's name only
    /// sets the default for arguments that have none.
    #[test]
    fn test_a_format_specifier_overrides_the_task_radix() {
        let store = store_with(&[("a", "10101100")]);
        assert_eq!(printed(r#"$displayh("%0d=%h", a, a);"#, &store), "172=ac\n");
        // The trailing argument has no specifier, so it takes the default.
        assert_eq!(printed(r#"$displayh("%0d ", a, a);"#, &store), "172 ac\n");
    }

    /// An unknown value renders in a radix variant exactly as it does under the
    /// matching specifier: binary shows which bits are unknown, and the others
    /// show a digit at a time, in upper case wherever a digit mixes.
    #[test]
    fn test_unknown_values_in_each_radix_variant() {
        let store = store_with(&[("nibble", "01x1"), ("three", "x01"), ("hiz", "zzzz")]);
        assert_eq!(printed(r#"$displayb(nibble);"#, &store), "01x1\n");
        assert_eq!(printed(r#"$displayh(nibble);"#, &store), "X\n");
        assert_eq!(printed(r#"$displayo(three);"#, &store), "X\n");
        assert_eq!(printed(r#"$display(nibble);"#, &store), " X\n");
        assert_eq!(printed(r#"$displayb(hiz);"#, &store), "zzzz\n");
        assert_eq!(printed(r#"$displayh(hiz);"#, &store), "z\n");
        assert_eq!(printed(r#"$displayo(three);"#, &store), "X\n");
    }

    /// The whole of Verilog's case rule for an unknown digit, in every radix
    /// that has digits.
    ///
    /// A digit whose bits *agree* — all `x`, or all `z` — prints in lower case;
    /// one that mixes an unknown bit with a known one, or an `x` with a `z`,
    /// prints in upper case, because the lower case letter would claim the
    /// whole digit was unknown. `iverilog` prints `12'b0000_0000_00xx` as
    /// `00X` in hexadecimal and `000X` in octal for exactly that reason.
    #[test]
    fn test_an_unknown_digit_is_upper_case_only_when_it_mixes() {
        let store = store_with(&[
            ("all_x", "xxxxxxxxxxxx"),
            ("all_z", "zzzzzzzzzzzz"),
            ("low_x", "0000000000xx"),
            ("low_z", "00000000zz11"),
            ("x_and_z", "xxxxzzzz0000"),
        ]);

        // A hexadecimal digit is four bits, an octal one is three, so the same
        // value can mix in one radix and agree in the other: the low octal
        // digit of `x_and_z` is `zz0`, which is a `Z`, while its low nibble is
        // four zeros.
        for (name, hex, octal, binary) in [
            ("all_x", "xxx", "xxxx", "xxxxxxxxxxxx"),
            ("all_z", "zzz", "zzzz", "zzzzzzzzzzzz"),
            ("low_x", "00X", "000X", "0000000000xx"),
            ("low_z", "00Z", "00ZZ", "00000000zz11"),
            ("x_and_z", "xz0", "xXZ0", "xxxxzzzz0000"),
        ] {
            assert_eq!(
                printed(&format!(r#"$display("%h", {});"#, name), &store),
                format!("{}\n", hex),
                "hexadecimal of {}",
                name
            );
            assert_eq!(
                printed(&format!(r#"$display("%o", {});"#, name), &store),
                format!("{}\n", octal),
                "octal of {}",
                name
            );
            // A binary digit is one bit and so can never mix, which is why
            // binary is the radix that never shows a capital.
            assert_eq!(
                printed(&format!(r#"$display("%b", {});"#, name), &store),
                format!("{}\n", binary),
                "binary of {}",
                name
            );
        }
    }

    /// `%d` shows the whole value as one digit, and takes the same case rule:
    /// `4'bxxxx` is `x`, `4'bzzxx` is `X` and `4'b00zz` is `Z`. This is corpus
    /// `disp_dec`, line for line.
    #[test]
    fn test_decimal_takes_the_same_case_rule_over_the_whole_value() {
        let store = store_with(&[]);
        for (literal, expected) in [
            ("4'bxxxx", " x"),
            ("4'bzzxx", " X"),
            ("4'bzzzz", " z"),
            ("4'b00zz", " Z"),
            ("4'b0000", " 0"),
            ("4'b0011", " 3"),
        ] {
            assert_eq!(
                printed(&format!(r#"$display("%d", {});"#, literal), &store),
                format!("{}\n", expected),
                "decimal of {}",
                literal
            );
        }
    }

    /// A width that is not a whole number of digits leaves a short digit at the
    /// top, and it is judged on the bits it actually has rather than on a
    /// nibble padded out with zeros — `5'bxxxxx` is `xx`, not `Xx`.
    #[test]
    fn test_a_short_top_digit_is_judged_on_the_bits_it_has() {
        let store = store_with(&[("five", "1xxxx"), ("five_x", "xxxxx")]);
        assert_eq!(printed(r#"$display("%h", five);"#, &store), "1x\n");
        assert_eq!(printed(r#"$display("%o", five);"#, &store), "Xx\n");
        assert_eq!(printed(r#"$display("%h", five_x);"#, &store), "xx\n");
        assert_eq!(printed(r#"$display("%o", five_x);"#, &store), "xx\n");
    }

    /// A width of `0` asks for the narrowest rendering, and in a base that pads
    /// with digits that means dropping the leading zeros. This is corpus
    /// `disp_leading_z` and `disp_parm`.
    #[test]
    fn test_a_zero_width_drops_leading_zeros() {
        let store = store_with(&[("wide", "0000000011"), ("zero", "0000000000")]);
        assert_eq!(
            printed(r#"$display("|%b|", wide);"#, &store),
            "|0000000011|\n"
        );
        assert_eq!(printed(r#"$display("|%0b|", wide);"#, &store), "|11|\n");
        // Every digit being zero still leaves one behind: a number has to have
        // a digit.
        assert_eq!(
            printed(r#"$display("|%b|", zero);"#, &store),
            "|0000000000|\n"
        );
        assert_eq!(printed(r#"$display("|%0b|", zero);"#, &store), "|0|\n");

        let store = store_with(&[("word", "00000001000001")]);
        assert_eq!(printed(r#"$display("|%0h|", word);"#, &store), "|41|\n");
        assert_eq!(printed(r#"$display("|%0o|", word);"#, &store), "|101|\n");
        assert_eq!(
            printed(r#"$display("|%0b|", word);"#, &store),
            "|1000001|\n"
        );
    }

    /// An unknown digit is not a zero, so it stops the trim: dropping it would
    /// move the value's bits.
    #[test]
    fn test_a_zero_width_stops_at_an_unknown_digit() {
        let store = store_with(&[("high_x", "xxxx00000001"), ("low_x", "00000000xxxx")]);
        assert_eq!(printed(r#"$display("%0h", high_x);"#, &store), "x01\n");
        assert_eq!(printed(r#"$display("%0h", low_x);"#, &store), "x\n");
    }

    /// A *leading* zero in the width is a zero fill, exactly as in C: `%08d`
    /// pads with zeros where `%8d` pads with spaces, and the zeros go after a
    /// minus sign. This is corpus `test_extended`.
    #[test]
    fn test_a_leading_zero_in_the_width_fills_with_zeros() {
        let store = store_with(&[("word", "00000001000001")]);
        assert_eq!(
            printed(r#"$display("|%08d|", word);"#, &store),
            "|00000065|\n"
        );
        assert_eq!(
            printed(r#"$display("|%8d|", word);"#, &store),
            "|      65|\n"
        );
        assert_eq!(printed(r#"$display("|%03d|", word);"#, &store), "|065|\n");
        assert_eq!(printed(r#"$display("|%3d|", word);"#, &store), "| 65|\n");
        assert_eq!(
            printed(r#"$display("|%08h|", word);"#, &store),
            "|00000041|\n"
        );
        // A sign is not a digit, so it stays at the front of the field.
        assert_eq!(
            printed(r#"$display("|%08d|", -10);"#, &store),
            "|-0000010|\n"
        );
    }

    /// The default field a `%d` pads to is as wide as the widest value the
    /// signal can hold — and for a signed signal that is its most *negative*
    /// value, so the field is one wider than the same bits read unsigned. An
    /// `integer` holding `4` therefore prints in eleven columns, which is
    /// corpus `pr1746848`.
    #[test]
    fn test_a_signed_decimal_field_leaves_room_for_the_sign() {
        let unsigned = store_with(&[
            ("four", "0100"),
            ("sixteen", "0000000000000001"),
            ("thirty_two", "00000000000000000000000000000001"),
        ]);
        assert_eq!(printed(r#"$display("|%d|", four);"#, &unsigned), "| 4|\n");
        assert_eq!(
            printed(r#"$display("|%d|", sixteen);"#, &unsigned),
            "|    1|\n"
        );
        assert_eq!(
            printed(r#"$display("|%d|", thirty_two);"#, &unsigned),
            "|         1|\n"
        );

        let signed = signed_store_with(&[
            ("four", "0100"),
            ("sixteen", "0000000000000001"),
            ("thirty_two", "00000000000000000000000000000001"),
        ]);
        assert_eq!(printed(r#"$display("|%d|", four);"#, &signed), "| 4|\n");
        assert_eq!(
            printed(r#"$display("|%d|", sixteen);"#, &signed),
            "|     1|\n"
        );
        assert_eq!(
            printed(r#"$display("|%d|", thirty_two);"#, &signed),
            "|          1|\n"
        );
    }

    /// `%s` on a vector pads to the characters the vector *has* rather than to
    /// the characters it spells, so a thirty-two bit register holding `"A"`
    /// prints in four columns. `%0s` is the same text with the padding left
    /// off, and a NUL between two characters is a space rather than nothing —
    /// it is a character the vector really has. Corpus `test_width`.
    #[test]
    fn test_string_format_pads_a_vector_to_its_own_bytes() {
        let store = store_with(&[
            ("word", "00000001000001"),
            ("wide", "00000000000000000000000001000001"),
            ("gapped", "010000010000000001000010"),
        ]);
        assert_eq!(printed(r#"$display("|%s|", word);"#, &store), "| A|\n");
        assert_eq!(printed(r#"$display("|%0s|", word);"#, &store), "|A|\n");
        assert_eq!(printed(r#"$display("|%s|", wide);"#, &store), "|   A|\n");
        assert_eq!(printed(r#"$display("|%s|", gapped);"#, &store), "|A B|\n");
        // A literal is already exactly as wide as it is, and its spaces are
        // characters like any other.
        assert_eq!(printed(r#"$display("|%s|", "   A");"#, &store), "|   A|\n");
        assert_eq!(printed(r#"$display("|%0s|", "   A");"#, &store), "|   A|\n");
    }

    /// Arming a `$monitor` does not print. It reports at the *end* of a
    /// timestep, and that includes the one it was armed in, so the block that
    /// armed it can go on to write the value the first line carries.
    #[test]
    fn test_a_monitor_prints_at_the_end_of_the_step_that_armed_it() {
        let mut store = store_with(&[("a", "0000")]);
        let mut context = TaskContext::new();
        run_in(&mut context, r#"$monitor("a=%0d", a);"#, &mut store);
        assert_eq!(context.output().text(), "", "arming printed early");
        assert!(context.has_deferred());

        store.set("a", Register::from_binary("0111"));
        context.flush(&store).expect("flush should succeed");
        assert_eq!(context.output().text(), "a=7\n");

        // And a later timestep that moves nothing it reads adds no line.
        context.flush(&store).expect("flush should succeed");
        assert_eq!(context.output().text(), "a=7\n");
    }

    /// A `$monitor` watches the *variables* it prints. The clock moves every
    /// timestep, so a monitor that reports it would otherwise never stop —
    /// which is corpus `br_ml20150315`.
    #[test]
    fn test_a_monitor_does_not_report_the_clock_moving() {
        let mut store = store_with(&[("a", "0001")]);
        store.set_time(1);
        let mut context = TaskContext::new();
        run_in(
            &mut context,
            r#"$monitor("t=%0t a=%0d", $time, a);"#,
            &mut store,
        );
        context.flush(&store).expect("flush should succeed");
        assert_eq!(context.output().text(), "t=1 a=1\n");

        store.set_time(2);
        context.flush(&store).expect("flush should succeed");
        assert_eq!(
            context.output().text(),
            "t=1 a=1\n",
            "the clock alone triggered a `$monitor`"
        );
    }

    /// `$fdisplay` writes to a descriptor. There is no file I/O here, so the
    /// one descriptor that can be honoured is standard output — the buffer.
    #[test]
    fn test_fdisplay_to_standard_output_prints_into_the_buffer() {
        let mut store = store_with(&[("a", "10101100")]);
        assert_eq!(printed(r#"$fdisplay(1, "PASSED");"#, &store), "PASSED\n");
        // `32'h8000_0001` is the same channel written as a file descriptor.
        assert_eq!(
            printed(r#"$fdisplay(32'h80000001, "PASSED");"#, &store),
            "PASSED\n"
        );
        assert_eq!(printed(r#"$fdisplayh(1, a);"#, &store), "ac\n");
        let mut context = TaskContext::new();
        run_in(&mut context, r#"$fwriteb(1, a);"#, &mut store);
        assert_eq!(context.output().text(), "10101100");
    }

    /// Any other descriptor names a file nothing opened. That is an error, not
    /// a no-op: a design whose output vanished would look like one that passed.
    #[test]
    fn test_fdisplay_to_a_file_is_an_error_rather_than_a_no_op() {
        let message = error(r#"$fdisplay(4, "PASSED");"#);
        assert!(
            message.contains("nothing opened"),
            "unexpected message: {}",
            message
        );
        let message = error(r#"$fdisplay();"#);
        assert!(
            message.contains("needs a file descriptor"),
            "unexpected message: {}",
            message
        );
    }

    /// The `f` of `$finish` and the `f` at the end of `$monitoroff` are neither
    /// of them the family's affixes, and the deferred tasks decompose the same
    /// way the printing ones do.
    #[test]
    fn test_the_task_family_is_split_without_swallowing_other_names() {
        assert_eq!(
            resolve_task("finish").expect("finish should resolve"),
            SystemTask::Finish
        );
        assert_eq!(
            resolve_task("monitoroff").expect("monitoroff should resolve"),
            SystemTask::MonitorControl(false)
        );
        assert_eq!(
            resolve_task("monitoron").expect("monitoron should resolve"),
            SystemTask::MonitorControl(true)
        );
        for (name, expected) in [
            (
                "strobeh",
                SystemTask::Strobe(print(true, Radix::Hexadecimal, false)),
            ),
            (
                "fmonitor",
                SystemTask::Monitor(print(true, Radix::Decimal, true)),
            ),
            (
                "fstrobeb",
                SystemTask::Strobe(print(true, Radix::Binary, true)),
            ),
        ] {
            assert_eq!(
                resolve_task(name).expect("should resolve"),
                expected,
                "unexpected task for ${}",
                name
            );
        }
        assert_eq!(
            resolve_task("fnothing")
                .expect_err("should be rejected")
                .to_string(),
            "unknown system task `$fnothing`"
        );
    }

    fn print(newline: bool, radix: Radix, descriptor: bool) -> Print {
        Print {
            newline,
            radix,
            descriptor,
        }
    }

    #[test]
    fn test_finish_is_recorded_rather_than_exiting() {
        let mut store = store_with(&[]);
        let mut context = TaskContext::new();
        assert!(!context.finished());
        run_in(&mut context, "$finish;", &mut store);
        assert!(context.finished());
    }

    #[test]
    fn test_an_unknown_task_is_an_error_that_names_it() {
        assert_eq!(error("$nosuchthing;"), "unknown system task `$nosuchthing`");
        // A name that only looks like one of the memory-file tasks is still
        // nothing this simulator has heard of.
        assert_eq!(
            error("$readmem(\"f.hex\", mem);"),
            "unknown system task `$readmem`"
        );
        assert_eq!(
            error("$readmemo(\"f.oct\", mem);"),
            "unknown system task `$readmemo`"
        );
        assert_eq!(
            error(r#"$display("%0d", $nosuchfunction);"#),
            "unknown system task `$nosuchfunction`"
        );
    }

    #[test]
    fn test_a_strobe_prints_nothing_until_the_timestep_ends() {
        let mut store = store_with(&[("a", "0001")]);
        let mut context = TaskContext::new();
        run_in(&mut context, r#"$strobe("a=%0d", a);"#, &mut store);
        assert_eq!(context.output().text(), "", "a `$strobe` printed early");
        assert!(context.has_deferred());

        store.set("a", Register::from_binary("0111"));
        context.flush(&store).expect("flush should succeed");
        assert_eq!(context.output().text(), "a=7\n");
        // The queue is emptied by the flush, so the next timestep starts clean.
        assert!(!context.has_deferred());
    }

    #[test]
    fn test_a_format_the_simulator_does_not_understand_is_an_error() {
        let message = error(r#"$display("%q", 1);"#);
        assert!(message.contains("`%q`"), "unexpected message: {}", message);
    }

    /// `%c` is the low eight bits as a character and `%v` is the strength of
    /// every bit, `_` between them — both measured from iverilog 12.0. The
    /// strength of a value nothing weaker drives is `St`; see [`strengths`].
    #[test]
    fn test_character_and_strength_formats() {
        let store = store_with(&[("a", "0110"), ("byte", "01000001")]);
        assert_eq!(printed(r#"$display("[%c]", byte);"#, &store), "[A]\n");
        assert_eq!(printed(r#"$display("[%c]", 8'h42);"#, &store), "[B]\n");
        assert_eq!(
            printed(r#"$display("[%v]", a);"#, &store),
            "[St0_St1_St1_St0]\n"
        );
        assert_eq!(
            printed(r#"$display("[%v]", 4'b01xz);"#, &store),
            "[St0_St1_StX_HiZ]\n"
        );
    }

    /// A string is a value — eight bits a character — so a numeric format
    /// takes one: `$display("%d", "A")` is 65, right-aligned in the three
    /// columns eight bits ask for (iverilog 12.0).
    #[test]
    fn test_a_string_reaches_a_numeric_format_as_its_bytes() {
        let store = store_with(&[]);
        assert_eq!(printed(r#"$display("[%d]", "A");"#, &store), "[ 65]\n");
        assert_eq!(printed(r#"$display("[%0d]", "AB");"#, &store), "[16706]\n");
        assert_eq!(printed(r#"$display("[%h]", "A");"#, &store), "[41]\n");
    }

    #[test]
    fn test_a_specifier_with_no_argument_is_an_error() {
        let message = error(r#"$display("%0d");"#);
        assert!(
            message.contains("no argument left"),
            "unexpected message: {}",
            message
        );
    }
    #[test]
    fn test_a_memory_file_is_read_as_words_comments_and_address_jumps() {
        let entries = memory_entries(
            "// header\n0a 0b /* skipped */ @1f\n0c\n",
            Radix::Hexadecimal,
            "f.hex",
        )
        .expect("the file should read");
        assert_eq!(
            entries,
            vec![
                MemoryEntry::Word("0a".to_string()),
                MemoryEntry::Word("0b".to_string()),
                MemoryEntry::Address(31),
                MemoryEntry::Word("0c".to_string()),
            ]
        );
    }

    #[test]
    fn test_a_memory_file_that_cannot_be_read_is_an_error_that_says_why() {
        let message = memory_entries("0a /* never closed", Radix::Hexadecimal, "f.hex")
            .expect_err("the comment never ends")
            .to_string();
        assert!(
            message.contains("ends inside a `/*` comment"),
            "{}",
            message
        );

        let message = memory_entries("0a 0g", Radix::Hexadecimal, "f.hex")
            .expect_err("`g` is not a hex digit")
            .to_string();
        assert!(
            message.contains("`0g` is not a hexadecimal word"),
            "{}",
            message
        );

        let message = memory_entries("@zz", Radix::Hexadecimal, "f.hex")
            .expect_err("the address is not hexadecimal")
            .to_string();
        assert!(
            message.contains("`@zz` is not a hexadecimal"),
            "{}",
            message
        );
    }

    /// `x`, `z` and `?` are digits a memory file may hold, and one hex digit of
    /// them is four bits of them.
    #[test]
    fn test_a_memory_file_word_may_be_unknown() {
        assert_eq!(
            memory_word("1x", Radix::Hexadecimal).expect("should read"),
            Register::from_binary("0001xxxx")
        );
        assert_eq!(
            memory_word("1?0", Radix::Binary).expect("should read"),
            Register::from_binary("1z0")
        );
    }

    /// A file with more words in it than the load can take means two different
    /// things depending on who chose the bound.
    #[test]
    fn test_a_finish_address_bounds_the_load_but_a_declaration_does_not() {
        let directory = std::env::temp_dir().join("visilog-readmem-bounds");
        std::fs::create_dir_all(&directory).expect("scratch directory");
        let file = directory.join("eight.hex");
        std::fs::write(&file, "0 1 2 3 4 5 6 7\n").expect("data file");

        let mut store = StateStore::new();
        store.declare_memory("mem", (0, 3), (7, 0), false);
        let mut context = TaskContext::new();

        // An explicit finish says where to stop, so the four words past it are
        // simply not loaded.
        let source = format!(r#"$readmemh("{}", mem, 0, 3);"#, file.display());
        run_in(&mut context, &source, &mut store);
        assert_eq!(
            store.memory("mem").expect("mem").word(Some(3)),
            Register::from_u128(3, 8)
        );

        // With no finish the bound came from the declaration, and a file that
        // does not fit it is a mismatch worth reporting.
        let (_, call) =
            parse_system_task(&format!(r#"$readmemh("{}", mem);"#, file.display())).expect("parse");
        let call = TaskCall::compile(&call).expect("compile");
        let message = TaskContext::new()
            .run(&call, &mut store)
            .expect_err("the file is twice the size of the memory")
            .to_string();
        assert!(message.contains("ran off the end of `mem`"), "{}", message);
    }

    #[test]
    fn test_readmem_argument_mistakes_are_reported() {
        let message = error(r#"$readmemh("f.hex");"#);
        assert!(
            message.contains("`$readmem…` takes a file name"),
            "{}",
            message
        );

        let message = error(r#"$readmemh("f.hex", 1 + 2);"#);
        assert!(
            message.contains("must be a plain identifier"),
            "{}",
            message
        );

        // `mem` here is not declared at all, which is the same answer as a name
        // that is a signal: there is no memory to fill.
        let message = error(r#"$readmemh("f.hex", mem);"#);
        assert!(message.contains("`mem` is not a memory"), "{}", message);
    }

    #[test]
    fn test_timeformat_checks_its_arguments() {
        let message = error(r#"$timeformat(-9, 2);"#);
        assert!(
            message.contains("takes units, precision, a suffix and a minimum width"),
            "{}",
            message
        );

        let message = error(r#"$timeformat(-40, 2, "ns", 10);"#);
        assert!(
            message.contains("must be a power of ten between -15 and 2"),
            "{}",
            message
        );

        let message = error(r#"$timeformat(-9, 2, "ns", 99999);"#);
        assert!(
            message.contains("must be between 0 and 1024"),
            "{}",
            message
        );
    }

    /// `$timeformat` with no arguments puts `%t` back where it started.
    #[test]
    fn test_timeformat_with_no_arguments_restores_the_default() {
        let mut store = store_with(&[]);
        store.set_time(5);
        let mut context = TaskContext::new();
        run_in(&mut context, r#"$timeformat(-9, 1, "ns", 6);"#, &mut store);
        run_in(&mut context, r#"$display("[%t]", $time);"#, &mut store);
        run_in(&mut context, "$timeformat;", &mut store);
        run_in(&mut context, r#"$display("[%t]", $time);"#, &mut store);

        assert_eq!(
            context.output().lines(),
            vec!["[ 5.0ns]", "[                   5]"]
        );
    }
}

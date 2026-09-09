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
use crate::register::{Register, ONE, X, Z, ZERO};
use crate::simulator::elaborate::rename_expression;
use crate::simulator::eval::{eval, SYSTEM_FUNCTIONS};
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
    fn render(self, value: &Register) -> (String, usize) {
        match self {
            Radix::Decimal => (decimal(value), decimal_width(value.width())),
            Radix::Binary => (binary(value), value.width()),
            Radix::Hexadecimal => (hex(value), value.width().div_ceil(4)),
            Radix::Octal => (octal(value), value.width().div_ceil(3)),
        }
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

        Ok(TaskCall { task, arguments })
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
}

fn unknown_task(name: &str) -> SimulationError {
    SimulationError::SystemTask(format!("unknown system task `${}`", name))
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
/// that moved nothing the monitor reads produces no line at all.
#[derive(Clone, Debug)]
struct Monitor {
    call: TaskCall,
    snapshot: Vec<Register>,
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
            self.print_call(print, &call.arguments, store)?;
        }

        // Taking the monitor out keeps `self` free to print with; nothing
        // between here and putting it back can arm a different one.
        let Some(mut monitor) = self.monitor.take() else {
            return Ok(());
        };
        if monitor.enabled {
            let values = self.snapshot(&monitor.call, store)?;
            if values != monitor.snapshot {
                monitor.snapshot = values;
                self.print_call(monitor.print(), &monitor.call.arguments, store)?;
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
            SystemTask::Print(print) => self.print_call(print, &call.arguments, store)?,
            // A `$strobe` is kept rather than run: what it prints is whatever
            // its arguments hold once the timestep has finished moving.
            SystemTask::Strobe(_) => self.strobes.push(call.clone()),
            SystemTask::Monitor(print) => {
                // Arming prints once, immediately, and the values that line was
                // made of become the baseline every later end-of-timestep is
                // measured against — so the step that armed it does not then
                // report itself a second time.
                self.print_call(print, &call.arguments, store)?;
                let snapshot = self.snapshot(call, store)?;
                self.monitor = Some(Monitor {
                    call: call.clone(),
                    snapshot,
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
            self.print_call(monitor.print(), &monitor.call.arguments, store)?;
            monitor.snapshot = self.snapshot(&monitor.call, store)?;
        }
        self.monitor = Some(monitor);
        Ok(())
    }

    /// The values an armed `$monitor`'s arguments hold now — the expression
    /// arguments only, since a format string cannot move.
    fn snapshot(
        &self,
        call: &TaskCall,
        store: &StateStore,
    ) -> Result<Vec<Register>, SimulationError> {
        call.arguments
            .iter()
            .filter_map(|argument| match argument {
                TaskArgument::Value(expression) => {
                    Some(eval(expression, store).map_err(Into::into))
                }
                TaskArgument::Text(_) => None,
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
        let text = self.render(arguments, store, print.radix)?;
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
    ) -> Result<String, SimulationError> {
        let mut text = String::new();
        let mut index = 0;
        while index < arguments.len() {
            match &arguments[index] {
                TaskArgument::Text(format) => {
                    index += 1;
                    self.render_format(format, arguments, &mut index, store, &mut text)?;
                }
                argument => {
                    let value = self.value_of(argument, store)?;
                    let (rendered, width) = radix.render(&value);
                    text.push_str(&pad(rendered, width));
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

            // A width of `0` means "as narrow as the value allows", which is
            // what `%0d` — by far the most common specifier in the wild — asks
            // for. Any other width pads on the left.
            let mut width = String::new();
            while characters.peek().is_some_and(|c| c.is_ascii_digit()) {
                width.push(characters.next().expect("peeked digit must exist"));
            }
            let width: Option<usize> = match width.as_str() {
                "" => None,
                digits => Some(digits.parse().map_err(|_| {
                    SimulationError::SystemTask(format!("format width `{}` is too large", digits))
                })?),
            };

            let specifier = characters
                .next()
                .ok_or_else(|| bad_format("a trailing `%` with no specifier"))?;

            let argument = arguments.get(*index).ok_or_else(|| {
                bad_format(&format!(
                    "`%{}` has no argument left to format",
                    specifier.to_lowercase()
                ))
            })?;
            *index += 1;

            if specifier.eq_ignore_ascii_case(&'s') {
                let rendered = match argument {
                    TaskArgument::Text(literal) => literal.clone(),
                    other => ascii(&self.value_of(other, store)?),
                };
                text.push_str(&pad(rendered, width.unwrap_or(0)));
                continue;
            }

            // `%t` is a time value, and `$timeformat` — not the radix the task
            // name asked for — says how it reads. An explicit width still wins
            // over the one `$timeformat` set, so `%0t` never pads.
            if specifier.eq_ignore_ascii_case(&'t') {
                let value = self.value_of(argument, store)?;
                let rendered = self.time_format.render(&value);
                text.push_str(&pad(rendered, width.unwrap_or(self.time_format.min_width)));
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
            text.push_str(&pad(rendered, width.unwrap_or(default_width)));
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
            TaskArgument::Text(text) => Err(bad_format(&format!(
                "the string \"{}\" is not a value a numeric format can take",
                text
            ))),
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

/// Left-pads with spaces to `width`, which a wider value simply overflows.
fn pad(text: String, width: usize) -> String {
    if text.len() >= width {
        return text;
    }
    let mut padded = " ".repeat(width - text.len());
    padded.push_str(&text);
    padded
}

/// How many decimal digits the widest value of `bits` bits takes.
fn decimal_width(bits: usize) -> usize {
    if bits >= 128 {
        // log10(2) * bits, which is exact enough to size a value no radix
        // conversion in this crate can produce anyway.
        return (bits as f64 * std::f64::consts::LOG10_2).floor() as usize + 1;
    }
    ((1u128 << bits) - 1).to_string().len().max(1)
}

/// `x` or `z` — what an unknown value renders as in a radix that cannot show
/// individual bits. A value that is entirely high-impedance reads as `z`; any
/// other unknown bit makes it `x`.
fn unknown(register: &Register) -> String {
    if register.to_binary().chars().all(|bit| bit == 'z') {
        "z".to_string()
    } else {
        "x".to_string()
    }
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
        return register
            .to_i128()
            .map_or_else(|| unknown(register), |value| value.to_string());
    }
    register
        .to_u128()
        .map_or_else(|| unknown(register), |value| value.to_string())
}

/// Binary, which is the one radix that shows an `x` or a `z` bit by bit.
fn binary(register: &Register) -> String {
    register.to_binary()
}

/// Hexadecimal, lower case the way Verilog prints it. The register is first
/// widened to a whole number of nibbles, so that its most significant bits land
/// in the digit they belong to.
fn hex(register: &Register) -> String {
    let widened = register.resize(register.width().div_ceil(4) * 4);
    widened
        .to_hex()
        .map_or_else(|| unknown(register), |hex| hex.to_lowercase())
}

/// Octal, widened to a whole number of digits the way [`hex`] is.
fn octal(register: &Register) -> String {
    let widened = register.resize(register.width().div_ceil(3) * 3);
    widened.to_octal().unwrap_or_else(|| unknown(register))
}

/// A register read as text, most significant byte first, the way `%s` prints a
/// vector. Padding NULs are dropped, which is what makes `"ok"` come back out
/// of a register wide enough to have been zero extended.
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
        .filter(|character| *character != '\0')
        .collect()
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
        // Binary is the radix that can show which bits are unknown; the others
        // report the whole value as `x`, or as `z` when nothing else is left.
        assert_eq!(printed(r#"$display("%b", a);"#, &store), "01x1\n");
        assert_eq!(printed(r#"$display("%0d", a);"#, &store), "x\n");
        assert_eq!(printed(r#"$display("%h", a);"#, &store), "x\n");
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
    /// matching specifier: binary shows which bits are unknown, the others
    /// report the whole value as `x`, or `z` when nothing else is left.
    #[test]
    fn test_unknown_values_in_each_radix_variant() {
        let store = store_with(&[("nibble", "01x1"), ("three", "x01"), ("hiz", "zzzz")]);
        assert_eq!(printed(r#"$displayb(nibble);"#, &store), "01x1\n");
        assert_eq!(printed(r#"$displayh(nibble);"#, &store), "x\n");
        assert_eq!(printed(r#"$displayo(three);"#, &store), "x\n");
        assert_eq!(printed(r#"$display(nibble);"#, &store), " x\n");
        assert_eq!(printed(r#"$displayb(hiz);"#, &store), "zzzz\n");
        assert_eq!(printed(r#"$displayh(hiz);"#, &store), "z\n");
        assert_eq!(printed(r#"$displayo(three);"#, &store), "x\n");
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
        let message = error(r#"$display("%v", 1);"#);
        assert!(message.contains("`%v`"), "unexpected message: {}", message);
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

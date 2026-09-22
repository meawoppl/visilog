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

use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;

use crate::parsers::behavior::{SystemTaskArgument, SystemTaskCall};
use crate::parsers::expr::Expression;
use crate::parsers::gates::DriveStrength;
use crate::parsers::preprocessor::{TimeSpec, Timescale};
use crate::register::{Register, ONE, REAL_WIDTH, X, Z, ZERO};
use crate::simulator::elaborate::rename_expression;
use crate::simulator::eval::{eval, string_bits, SYSTEM_FUNCTIONS};
use crate::simulator::exec::{drive, resolve_target, ResolvedTarget};
use crate::simulator::gates::Strength;
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::StateStore;
use crate::simulator::vcd::{Control, DumpTarget, VcdDump};

/// The descriptor a task outside the `$f…` family writes to: bit 0 of a
/// multi-channel mask, which is standard output.
const STANDARD_OUTPUT: u32 = 1;

/// Everything a design has printed, as one buffer.
///
/// `$display` appends a newline and `$write` does not, so a line is only
/// complete once something terminates it; [`Output::lines`] reads the buffer
/// back split on newlines, with a trailing unterminated `$write` as its own
/// last entry.
///
/// The text sits behind an [`Rc`] so that a *handle* to it can be given to the
/// [`StateStore`], which is the only thing
/// [`eval`](crate::simulator::eval::eval) is handed — that is what lets a
/// `$display` written inside a function body print into the design's own
/// buffer, in the order it ran, rather than into a context nobody reads. It is
/// the same shape the `$random` stream and the file table already use, and
/// pushing through a shared reference is what makes it work at all.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Output {
    text: Rc<RefCell<String>>,
}

impl Output {
    /// Everything printed so far, newlines and all.
    pub fn text(&self) -> String {
        self.text.borrow().clone()
    }

    /// The printed lines, without their newlines.
    pub fn lines(&self) -> Vec<String> {
        self.text.borrow().lines().map(str::to_string).collect()
    }

    /// Whether the design has printed anything at all.
    pub fn is_empty(&self) -> bool {
        self.text.borrow().is_empty()
    }

    /// How many bytes have been printed. `propagate` reads it either side of a
    /// pass to find out whether a continuous assignment printed anything.
    pub fn len(&self) -> usize {
        self.text.borrow().len()
    }

    /// Throws away everything printed so far *without* breaking the handle, so
    /// a store already sharing this buffer keeps sharing it.
    fn clear(&self) {
        self.text.borrow_mut().clear();
    }

    fn push(&self, text: &str) {
        self.text.borrow_mut().push_str(text);
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
    /// `$sformat` / `$swrite` — format the arguments the way `$write` does and
    /// write the text into a *register* instead of printing it, as one eight
    /// bit character per byte.
    ///
    /// The flag is what tells the two apart: `$sformat`'s second argument is
    /// **always** the format string, even when it is a `reg` holding one, while
    /// `$swrite` follows the `$display` rule that only a literal is one.
    Format(Print, bool),
    /// `$fclose` — closes every file its descriptor names.
    CloseFile,
    /// `$fflush` — pushes what is buffered out to the files its descriptor
    /// names, or to all of them when it is given none.
    FlushFile,
    /// `$timeformat` — how `%t` renders a time value from here on.
    TimeFormat,
    /// `$dumpfile` — names the waveform file the dump is written to.
    DumpFile,
    /// `$dumpvars` — records what to dump, and how far down a scope to go.
    DumpVars,
    /// `$dumpon` / `$dumpoff` / `$dumpall` — suspend, resume, or write a full
    /// snapshot now.
    Dump(Control),
    /// `$dumpflush` — pushes what has been written out to the file.
    DumpFlush,
    /// `$dumplimit` — stops the dump once the file reaches this many bytes.
    DumpLimit,
    /// `$printtimescale` — reports the `` `timescale `` of the scope it names,
    /// or of the one it was written in.
    PrintTimescale,
    /// End the simulation. `$stop` is the same thing here — see
    /// [`resolve_task`].
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
    /// Every expression among this call's arguments. A string argument is held
    /// as text rather than as an expression, so it carries none.
    pub fn expressions(&self) -> Vec<&Expression> {
        self.arguments
            .iter()
            .filter_map(|argument| match argument {
                TaskArgument::Value(expression) => Some(expression),
                TaskArgument::Text(_) => None,
            })
            .collect()
    }

    /// Whether this call prints where it stands, rather than deferring to the
    /// end of a timestep or reaching for state of its own. It is the one
    /// question a function body's analysis asks of a task.
    pub fn prints_now(&self) -> bool {
        matches!(self.task, SystemTask::Print(_))
    }

    /// Resolves a parsed `$name(...)` against the tasks the simulator
    /// implements.
    ///
    /// Fails on a task that is not recognised, reporting the name, so a design
    /// never prints nothing by accident.
    pub fn compile(call: &SystemTaskCall) -> Result<TaskCall, SimulationError> {
        let task = resolve_task(&call.name)?;

        // `$printtimescale(dut)` names a *scope*, not a value: an instance, a
        // module, a task, a named block or a signal inside one. None of those
        // is something to evaluate, and an event or a task name could not be
        // evaluated at all — so the argument is kept as the text it was
        // written as. Doing it here rather than later is what keeps it out of
        // `TaskCall::rename`, which would rewrite an identifier into the flat
        // store name and lose the hierarchy the answer is about.
        if task == SystemTask::PrintTimescale {
            let arguments = call
                .arguments
                .iter()
                .map(|argument| match argument {
                    SystemTaskArgument::Expression(expression) => {
                        Ok(TaskArgument::Text(expression.to_contracted_string()))
                    }
                    SystemTaskArgument::String(text) => Ok(TaskArgument::Text(text.clone())),
                    SystemTaskArgument::Empty => Ok(TaskArgument::Text(String::new())),
                    SystemTaskArgument::SystemFunction(name) => Err(unknown_task(name)),
                })
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(TaskCall {
                task,
                arguments,
                scope: String::new(),
            });
        }

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

/// How `$printtimescale` writes a scale: `1us / 1ns`, with the spaces
/// iverilog puts round the slash, and `1s / 1s` for a module that declared no
/// `` `timescale `` — the default the LRM gives.
fn rendered_timescale(timescale: Option<Timescale>) -> String {
    match timescale {
        Some(timescale) => format!("{} / {}", timescale.unit, timescale.precision),
        None => "1s / 1s".to_string(),
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
        // `$stop` asks the simulator to suspend and hand control back to
        // whoever is driving it. There is nobody: visilog has no console, and
        // a `Simulator` is a library object whose caller is waiting on
        // `advance`. That is exactly the case iverilog spells `vvp -n`, whose
        // own help text reads "Non-interactive ($stop = $finish)", so this is
        // the equivalence iverilog itself draws rather than one invented here.
        // Left to its debugger, `vvp` instead prints a
        // `** VVP Stop(N) **` banner naming the source line and — reading an
        // empty console — carries on; no gold file in the corpus records one,
        // and visilog has no line numbers to name.
        "stop" => return Ok(SystemTask::Finish),
        "time" => return Ok(SystemTask::Time),
        "timeformat" => return Ok(SystemTask::TimeFormat),
        // `$printtimescale` ends in an `e` that is no radix and begins with a
        // `p` that is no descriptor prefix, so there is nothing for the split
        // to find — but it is spelled out here beside the other whole words
        // rather than left to fall through to an error.
        "printtimescale" => return Ok(SystemTask::PrintTimescale),
        "monitoron" => return Ok(SystemTask::MonitorControl(true)),
        "monitoroff" => return Ok(SystemTask::MonitorControl(false)),
        // `$fflush` ends in an `h` that is not a radix, the same way
        // `$monitoroff` ends in an `f` that is not the descriptor prefix.
        "fflush" => return Ok(SystemTask::FlushFile),
        // The waveform family is spelled out rather than split: `$dumpflush`
        // ends in an `h` that is not a radix and `$dumpvars` in an `s` that is
        // not one either, so there is nothing for the split to find.
        "dumpfile" => return Ok(SystemTask::DumpFile),
        "dumpvars" => return Ok(SystemTask::DumpVars),
        "dumpon" => return Ok(SystemTask::Dump(Control::On)),
        "dumpoff" => return Ok(SystemTask::Dump(Control::Off)),
        "dumpall" => return Ok(SystemTask::Dump(Control::All)),
        "dumpflush" => return Ok(SystemTask::DumpFlush),
        "dumplimit" => return Ok(SystemTask::DumpLimit),
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
        // `$sformat` and `$swrite` take a target rather than a descriptor, so
        // the `f` prefix is not one of their spellings, but the radix suffix is:
        // `$swriteb` comes out of the same split `$displayb` does. Which
        // argument is the format string is the only difference between the two,
        // and it rides on the flag.
        "sformat" | "swrite" if !descriptor => Ok(SystemTask::Format(
            Print {
                newline: false,
                radix,
                descriptor: false,
            },
            base == "sformat",
        )),
        "close" if descriptor && radix == Radix::Decimal => Ok(SystemTask::CloseFile),
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
/// `$timeformat(units, precision, suffix, min_width)` sets all four, and
/// `units` is a real scale factor rather than a label. The clock counts ticks
/// of the `` `timescale `` **unit** of the module a call sits in, and `%t`
/// restates one of those in whatever power of ten the design named — so
/// `` `timescale 1ns `` with `$timeformat(-6, …)` prints `10` as `0` and
/// `$timeformat(-12, …)` prints it as `10000`. The units are held as
/// femtoseconds, because a `` `timescale `` term is `1`, `10` or `100` of a
/// unit and only the finest of them makes every scale an exact integer.
///
/// `None` is the unit the LRM gives a design that never called `$timeformat`:
/// the **finest precision** any `` `timescale `` in it declared, which is why
/// it cannot be resolved here — only [`TaskContext`] knows the design.
///
/// What is left is formatting: `precision` fractional digits, then `suffix`,
/// right-aligned in `min_width`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TimeFormat {
    units: Option<u128>,
    precision: usize,
    suffix: String,
    min_width: usize,
}

/// The field `%t` pads to when `$timeformat` has not said otherwise.
const DEFAULT_TIME_WIDTH: usize = 20;

/// One second, in femtoseconds — the `` `timescale `` a module that declared
/// none is at, so a design with no directive anywhere counts ticks of a second
/// and prints them in seconds, which is the identity it always was.
const DEFAULT_SCALE_FS: u128 = 1_000_000_000_000_000;

/// The powers of ten `$timeformat` may name, seconds down to femtoseconds.
/// Anything outside is a named error rather than a silently odd unit.
const TIME_UNIT_BOUNDS: (i128, i128) = (-15, 2);

/// Femtoseconds in one unit of the power of ten `$timeformat` names.
fn time_units_fs(exponent: i128) -> u128 {
    10u128.pow((exponent - TIME_UNIT_BOUNDS.0) as u32)
}

impl Default for TimeFormat {
    fn default() -> Self {
        TimeFormat {
            units: None,
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
    ///
    /// `tick_fs` is what one tick of the clock is worth where the call was
    /// written and `default_fs` the unit a defaulted `$timeformat` prints in;
    /// both are the caller's to know.
    fn render(&self, value: &Register, tick_fs: u128, default_fs: u128) -> String {
        if value.has_unknown() {
            return unknown(value);
        }
        let units = self.units.unwrap_or(default_fs);
        // A real time — `$realtime`, or a literal — carries a fraction of a
        // tick, so it is scaled as a double and rounded by the field. An
        // integer one is scaled exactly and *truncated*; both are iverilog's.
        let mut text = if value.is_real() {
            let scaled = value.to_f64() * tick_fs as f64 / units as f64;
            format!("{:.*}", self.precision, scaled)
        } else {
            scaled_ticks(&decimal(value), tick_fs, units, self.precision)
        };
        text.push_str(&self.suffix);
        text
    }
}

/// A tick count restated in units of `units` femtoseconds, with `digits`
/// fractional digits.
///
/// The digits are **truncated** rather than rounded, which is what iverilog
/// 12.0 does: 1500 ticks of `1ns` at `$timeformat(-6, 0, …)` is `1` and at
/// `$timeformat(-6, 1, …)` is `1.5`. The whole scaling is integer, so a
/// picosecond of a design running for a year is still exact.
fn scaled_ticks(decimal: &str, tick_fs: u128, units: u128, digits: usize) -> String {
    let (sign, magnitude) = match decimal.strip_prefix('-') {
        Some(rest) => ("-", rest),
        None => ("", decimal),
    };
    let scaled = magnitude
        .parse::<u128>()
        .ok()
        .zip(10u128.checked_pow(digits.min(u32::MAX as usize) as u32))
        .and_then(|(ticks, shift)| ticks.checked_mul(tick_fs)?.checked_mul(shift))
        .map(|femtoseconds| femtoseconds / units);
    let Some(scaled) = scaled else {
        // Nothing this simulator can count reaches here — it takes a tick
        // count near 2**128 femtoseconds, or a field asking for more digits
        // than a `u128` holds — so the raw ticks are a better answer than a
        // panic.
        return decimal.to_string();
    };
    let mut text = scaled.to_string();
    if digits > 0 {
        while text.len() <= digits {
            text.insert(0, '0');
        }
        text.insert(text.len() - digits, '.');
    }
    format!("{}{}", sign, text)
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
    /// The design's waveform dump, once `$dumpfile` or `$dumpvars` has asked
    /// for one. `None` is a design that dumps nothing, which is what keeps the
    /// dumper off its settle loop entirely.
    dump: Option<VcdDump>,
    /// The name of the top module, which is the root scope a `$dumpvars`
    /// argument is resolved against and the outermost `$scope` in the header.
    /// The flat store carries no prefix for it, so it has to be told.
    top: String,
    /// One tick of the simulation clock — the finest precision any module
    /// declared — which is what `$timescale` says and what `%t` prints in when
    /// `$timeformat` has named nothing.
    /// [`Simulator::setup`](crate::simulator::runner::Simulator::setup) hands
    /// it over again after every [`TaskContext::reset`]; `None` is a context
    /// nothing described, whose clock counts seconds.
    clock: Option<TimeSpec>,
    /// Qualified port name → the store entry it was aliased onto, which is the
    /// only record that an instance's port has a name of its own.
    aliases: HashMap<String, String>,
    /// Every module instance by its hierarchical name, and every module by its
    /// own name, each with the `` `timescale `` it was written at.
    ///
    /// The two lists are kept apart because they answer different questions and
    /// a name can be in either: `$printtimescale(top.dut)` names an instance,
    /// while `$printtimescale(other)` names a module the design never
    /// instantiated — which iverilog answers, and which nothing in a flattened
    /// design could otherwise know about.
    instances: Vec<(String, Option<Timescale>)>,
    module_scales: Vec<(String, Option<Timescale>)>,
}

impl TaskContext {
    pub fn new() -> Self {
        TaskContext::default()
    }

    /// A context whose `$display` lands in someone else's buffer.
    ///
    /// This is what a function call's body runs against:
    /// [`FunctionDefinition::call`](crate::simulator::program::FunctionDefinition::call)
    /// hands it the frame's [`Output`] handle, which is the design's own, so a
    /// line printed inside a function is appended the moment it runs — before
    /// whatever the statement that made the call goes on to print. iverilog
    /// 12.0 puts it there: `$display("outer %0d", f(1));` prints f's line
    /// first.
    pub fn printing_into(output: Output) -> Self {
        TaskContext {
            output,
            ..TaskContext::default()
        }
    }

    /// Everything the design has printed.
    pub fn output(&self) -> &Output {
        &self.output
    }

    /// Whether the design has called `$finish`.
    pub fn finished(&self) -> bool {
        self.finished
    }

    /// Records what the design is called and what a tick of its clock is —
    /// the two things a waveform header states that no task argument carries.
    pub fn describe_design(&mut self, top: impl Into<String>, clock: TimeSpec) {
        self.top = top.into();
        self.clock = Some(clock);
    }

    /// The ports that are another signal under a second name, which a
    /// waveform declares beside the entry they share. See
    /// [`VcdDump::add`].
    pub fn name_aliases(&mut self, aliases: HashMap<String, String>) {
        self.aliases = aliases;
    }

    /// Records the design's scopes and their timescales, which is what
    /// `$printtimescale` reports.
    ///
    /// Flattening throws hierarchy away, so the instance list is handed over
    /// rather than reconstructed; the module list comes from the parsed
    /// source, because a module the design never instantiates still has a
    /// timescale and `$printtimescale` will answer for it.
    pub fn describe_scopes(
        &mut self,
        instances: Vec<(String, Option<Timescale>)>,
        modules: Vec<(String, Option<Timescale>)>,
    ) {
        self.instances = instances;
        self.module_scales = modules;
    }

    /// Whether anything is being recorded, which is the question the settle
    /// loop asks before it hands the change journal over.
    pub fn is_dumping(&self) -> bool {
        self.dump.as_ref().is_some_and(VcdDump::is_active)
    }

    /// Hands the names written since the last round to the dump. See
    /// [`VcdDump::note_changes`].
    pub fn note_changes<'a>(&mut self, names: impl Iterator<Item = &'a str>) {
        if let Some(dump) = &mut self.dump {
            dump.note_changes(names);
        }
    }

    /// Pushes what the dump has written out to its file: `$dumpflush`, and
    /// the end of every [`Simulator::advance`](crate::simulator::runner::Simulator::advance),
    /// so a caller that reads the waveform after a run reads all of it.
    pub fn flush_dump_file(&self, store: &StateStore) {
        if let Some(dump) = &self.dump {
            dump.flush_file(store);
        }
    }

    /// Writes the final `#<time>` and pushes the file out, which is what a
    /// design reaching `$finish` owes its waveform.
    pub fn close_dump(&mut self, store: &StateStore, time: i64) {
        if let Some(dump) = &mut self.dump {
            dump.close(store, time);
        }
    }

    /// Forgets everything one elaboration produced — the output, the `$finish`
    /// mark, the deferred queues and the `%t` format.
    pub fn reset(&mut self) {
        // Cleared rather than replaced: a store handed this buffer's handle by
        // `setup` goes on holding it across an elaboration, and a fresh
        // `Output` would leave the two pointing at different strings.
        self.output.clear();
        self.finished = false;
        self.strobes.clear();
        self.monitor = None;
        self.time_format = TimeFormat::default();
        self.dump = None;
    }

    /// Whether anything is owed to the end of the current timestep.
    ///
    /// [`Simulator::advance`](crate::simulator::runner::Simulator::advance)
    /// asks once per timestep and `poke` once per call, so this sits on the hot
    /// path: a design that uses neither task pays a load and a branch.
    pub fn has_deferred(&self) -> bool {
        !self.strobes.is_empty() || self.monitor.is_some() || self.dump.is_some()
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
            self.flush_dump(store);
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
        self.flush_dump(store);
        Ok(())
    }

    /// Writes the waveform section this timestep owes, and whatever the dump
    /// itself has to say about doing so.
    fn flush_dump(&mut self, store: &StateStore) {
        let Some(mut dump) = self.dump.take() else {
            return;
        };
        let printed = dump.flush(store, store.time());
        self.dump = Some(dump);
        self.output.push(&printed);
    }

    /// The design's dump, started if this is the first task to ask for one.
    fn dump_state(&mut self) -> &mut VcdDump {
        let clock = self.clock;
        self.dump.get_or_insert_with(|| VcdDump::new(clock))
    }

    /// `$dumpvars`, `$dumpvars(levels)` and `$dumpvars(levels, scope, …)`.
    ///
    /// The file is opened here rather than at the end of the timestep, because
    /// iverilog's `VCD info:` line lands before whatever the rest of the block
    /// prints. The *header* still waits: a second `$dumpvars` in the same
    /// timestep may add more variables to declare.
    fn dump_vars(
        &mut self,
        call: &TaskCall,
        store: &mut StateStore,
    ) -> Result<(), SimulationError> {
        let levels = match call.arguments.first() {
            Some(argument) => self.field_argument(argument, store, "`$dumpvars`'s level")?,
            None => 0,
        };
        let targets = call
            .arguments
            .iter()
            .skip(1)
            .map(|argument| dump_target(argument, store))
            .collect::<Result<Vec<_>, _>>()?;

        let top = self.top.clone();
        let aliases = self.aliases.clone();
        // A `$dumpvars` after the header has been written is too late to add
        // anything to it, which is what iverilog reports and carries on from.
        if self.dump_state().is_started() {
            let time = store.time();
            self.output.push(&format!(
                "VCD warning: $dumpvars ignored, previously called at simtime {}\n",
                time
            ));
            return Ok(());
        }
        let opened = self.dump_state().open(store);
        self.output.push(&opened);
        self.dump_state()
            .add(levels, &targets, &top, &aliases, store)
            .map_err(|detail| SimulationError::SystemTask(format!("`$dumpvars`: {}", detail)))
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
            SystemTask::Format(print, format_argument) => {
                self.format_into(print, format_argument, call, store)?
            }
            SystemTask::CloseFile => {
                let argument = call.arguments.first().ok_or_else(|| {
                    SimulationError::SystemTask("`$fclose` needs a file descriptor".to_string())
                })?;
                let descriptor = self.channel_mask(argument, store)?;
                store.close_channels(descriptor);
            }
            // A bare `$fflush;` names nothing, and flushes every open file.
            SystemTask::FlushFile => {
                let descriptor = match call.arguments.first() {
                    Some(argument) => Some(self.channel_mask(argument, store)?),
                    None => None,
                };
                store.flush_channels(descriptor);
            }
            SystemTask::MonitorControl(enabled) => self.set_monitoring(enabled, store)?,
            SystemTask::ReadMemory(radix) => self.read_memory(call, radix, store)?,
            SystemTask::WriteMemory(radix) => self.write_memory(call, radix, store)?,
            SystemTask::TimeFormat => self.set_time_format(&call.arguments, store)?,
            SystemTask::DumpFile => {
                let argument = call.arguments.first().ok_or_else(|| {
                    SimulationError::SystemTask("`$dumpfile` needs a file name".to_string())
                })?;
                let name = self.text_argument(argument, store)?;
                self.dump_state().set_file(&name);
            }
            SystemTask::DumpVars => self.dump_vars(call, store)?,
            SystemTask::Dump(control) => self.dump_state().control(control),
            SystemTask::DumpFlush => self.flush_dump_file(store),
            SystemTask::DumpLimit => {
                let argument = call.arguments.first().ok_or_else(|| {
                    SimulationError::SystemTask("`$dumplimit` needs a size".to_string())
                })?;
                let size = self.integer_argument(argument, store, "`$dumplimit`'s size")?;
                self.dump_state().set_limit(size.max(0) as u64);
            }
            // `$finish` takes an optional diagnostic level, which says how much
            // the simulator should report about itself on the way out.
            SystemTask::PrintTimescale => self.print_timescale(call, store)?,
            SystemTask::Finish => self.finished = true,
            SystemTask::Time => {}
        }
        Ok(())
    }

    /// `$printtimescale` — one line per argument, and one for the enclosing
    /// scope when it is given none.
    ///
    /// iverilog 12.0 prints `Time scale of (top.dut) is 10ns / 10ps`, and a
    /// module that declared no `` `timescale `` is at the default, `1s / 1s`.
    fn print_timescale(
        &mut self,
        call: &TaskCall,
        store: &StateStore,
    ) -> Result<(), SimulationError> {
        // `$printtimescale;` and `$printtimescale()` both ask about the scope
        // the call sits in, which for a call inside a named block is the
        // instance around it — the same stripping a named argument goes
        // through.
        let named: Vec<String> = call
            .arguments
            .iter()
            .filter_map(|argument| match argument {
                TaskArgument::Text(text) if !text.trim().is_empty() => {
                    Some(text.trim().to_string())
                }
                _ => None,
            })
            .collect();
        let wanted = if named.is_empty() {
            vec![self.enclosing_instance(&call.scope)]
        } else {
            named
        };
        for name in wanted {
            let (path, timescale) = self.resolve_scope(&name, &call.scope)?;
            self.output.push(&format!(
                "Time scale of ({}) is {}\n",
                self.rendered_path(&path, store),
                rendered_timescale(timescale)
            ));
        }
        Ok(())
    }

    /// The scope a `$printtimescale` argument names, and its timescale.
    ///
    /// A name is tried **as written** first, shedding a trailing segment at a
    /// time until what is left is an instance or a module, and only then
    /// qualified by the calling scope. That order is what tells
    /// `$printtimescale(othertop)` — a module the design never instantiated —
    /// from `$printtimescale(dut)`, which is an instance of the module the
    /// call sits in; the other way round, `top.othertop` would shed its tail
    /// and answer for `top` (corpus `pr1701855`).
    ///
    /// What comes back is the *whole* candidate rather than the prefix that
    /// matched, because that is what iverilog prints: `top.ipval` reports as
    /// `(top.ipval)` at `top`'s scale, not as `(top)`.
    fn resolve_scope(
        &self,
        name: &str,
        scope: &str,
    ) -> Result<(String, Option<Timescale>), SimulationError> {
        // A select is not part of the path: `top.rgval[0]` is a bit of a
        // signal in `top`, and the brackets are put back when it is printed.
        let (path, select) = match name.find('[') {
            Some(at) => (&name[..at], &name[at..]),
            None => (name, ""),
        };
        let qualified = format!("{}.{}", self.enclosing_instance(scope), path);
        for candidate in [path.to_string(), qualified] {
            if let Some(timescale) = self.scale_of(&candidate) {
                return Ok((format!("{}{}", candidate, select), timescale));
            }
        }
        Err(SimulationError::SystemTask(format!(
            "`$printtimescale` names `{}`, which is no scope in this design",
            name
        )))
    }

    /// The timescale of the innermost scope `candidate` lies in, shedding a
    /// trailing segment at a time. `None` when no prefix of it names one.
    fn scale_of(&self, candidate: &str) -> Option<Option<Timescale>> {
        let mut path = candidate;
        loop {
            if let Some((_, timescale)) = self
                .instances
                .iter()
                .chain(self.module_scales.iter())
                .find(|(name, _)| name == path)
            {
                return Some(*timescale);
            }
            path = path.rsplit_once('.')?.0;
        }
    }

    /// What one unit of a time value is worth, in femtoseconds, where a call
    /// was written: the `` `timescale `` **unit** of the module the scope
    /// belongs to, because that is the unit `$time` and `$realtime` report in
    /// there. A scope that names no module, or a module that declared no
    /// directive, is at one second — the `1s / 1s` the LRM gives a module with
    /// no `` `timescale ``.
    fn tick_fs(&self, scope: &str) -> u128 {
        self.scale_of(scope)
            .flatten()
            .map_or(DEFAULT_SCALE_FS, |scale| {
                u128::from(scale.unit.femtoseconds())
            })
    }

    /// How many decimals a bare `$realtime` written in `scope` prints with:
    /// the number of powers of ten its module's precision is finer than its
    /// unit, so none at all for a module that declared no `` `timescale ``.
    fn realtime_digits(&self, scope: &str) -> usize {
        self.scale_of(scope).flatten().map_or(0, |scale| {
            let steps = scale.unit.femtoseconds() / scale.precision.femtoseconds();
            steps.ilog10() as usize
        })
    }

    /// The unit `%t` prints in when `$timeformat` has named none: the finest
    /// **precision** any `` `timescale `` in the design declared, which is the
    /// clock's own tick and what the LRM asks for. `` `timescale 1ns/100ps ``
    /// with no `$timeformat` at all therefore prints `#5` as `50`, measured
    /// against iverilog 12.0.
    fn default_time_units(&self) -> u128 {
        self.clock
            .map_or(DEFAULT_SCALE_FS, |clock| u128::from(clock.femtoseconds()))
    }

    /// The instance a call sits in: its `%m` scope with any named block or
    /// inlined task shed off the end.
    ///
    /// That is what a bare `$printtimescale` reports and what an unqualified
    /// argument is resolved against. A call inside `initial begin : blk` has
    /// the scope `top.blk`, and iverilog answers `(top)` for it — a block has
    /// no timescale of its own (corpus `pr1701855b`).
    fn enclosing_instance(&self, scope: &str) -> String {
        let mut path = scope;
        loop {
            if self.instances.iter().any(|(name, _)| name == path) {
                return path.to_string();
            }
            match path.rsplit_once('.') {
                Some((head, _)) => path = head,
                None => return scope.to_string(),
            }
        }
    }

    /// The path as iverilog writes it back: a bit select of a *vector* is
    /// rendered as the one-bit part select it stands for (`rgval[0:0]`), while
    /// a word of a memory keeps its single index (`rgarr[0]`). Only the
    /// declaration tells the two apart, which is why the store is asked.
    fn rendered_path(&self, path: &str, store: &StateStore) -> String {
        let Some(at) = path.find('[') else {
            return path.to_string();
        };
        let (name, select) = path.split_at(at);
        let index = select.trim_matches(|c| c == '[' || c == ']');
        // The top module is the root of the flat store and carries no prefix,
        // so the store key is the path with that leading segment dropped.
        let key = name.strip_prefix(&format!("{}.", self.top)).unwrap_or(name);
        if store.memory(key).is_some() || index.contains(':') {
            return path.to_string();
        }
        format!("{}[{}:{}]", name, index, index)
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

    /// Formats an argument list and sends it wherever the task's descriptor
    /// says, which for everything outside the `$f…` family is the design's
    /// output buffer.
    ///
    /// This is what `$display` does when it runs and what `$strobe` and
    /// `$monitor` do when [`TaskContext::flush`] reaches them, so all three
    /// print the same way and only their timing differs.
    ///
    /// The arguments are rendered **before** the descriptor is consulted, and
    /// deliberately even when it names nothing: `$fdisplay(0, …)` produces no
    /// output but still evaluates what it was given, so a call to a design's
    /// own function still has its side effect.
    fn print_call(
        &mut self,
        print: Print,
        arguments: &[TaskArgument],
        store: &StateStore,
        scope: &str,
    ) -> Result<(), SimulationError> {
        let (descriptor, arguments) = if print.descriptor {
            let (descriptor, rest) = arguments.split_first().ok_or_else(|| {
                SimulationError::SystemTask(
                    "a `$f…` task needs a file descriptor as its first argument".to_string(),
                )
            })?;
            (self.channel_mask(descriptor, store)?, rest)
        } else {
            (STANDARD_OUTPUT, arguments)
        };
        let mut text = self.render(arguments, store, print.radix, scope)?;
        if print.newline {
            text.push('\n');
        }
        // Standard output is bit 0 of a multi-channel descriptor, which is what
        // makes `$fdisplay(fp|1, …)` reach a file *and* the buffer a
        // self-checking test reads.
        if store.write_channels(descriptor, &text) {
            self.output.push(&text);
        }
        Ok(())
    }

    /// `$sformat` / `$swrite` — the same formatting, written into the register
    /// the first argument names instead of printed.
    ///
    /// The text becomes a bit vector of eight bit characters and is then driven
    /// like any other assignment, so the target's width does the rest: a wider
    /// one is zero extended on the left, which `%s` renders back as leading
    /// spaces, and a narrower one keeps the *last* characters — `$sformat` of
    /// `"abcdef"` into a `reg [15:0]` is `"ef"`, which is what iverilog 12.0
    /// gives.
    fn format_into(
        &mut self,
        print: Print,
        format_argument: bool,
        call: &TaskCall,
        store: &mut StateStore,
    ) -> Result<(), SimulationError> {
        let (target, arguments) = call.arguments.split_first().ok_or_else(|| {
            SimulationError::SystemTask(
                "`$sformat` needs a variable to format into as its first argument".to_string(),
            )
        })?;
        let TaskArgument::Value(target) = target else {
            return Err(SimulationError::SystemTask(
                "`$sformat` cannot format into a string literal".to_string(),
            ));
        };
        // `$sformat(s, fmt, 7)` for a `reg` holding `"a=%0d"` formats with it,
        // where the same argument to `$swrite` would be *printed*. Reading the
        // register's characters here is the whole of that difference; past this
        // point the two are one task.
        let mut arguments = arguments.to_vec();
        if format_argument {
            if let Some(argument @ TaskArgument::Value(_)) = arguments.first() {
                let text = ascii(&self.value_of(argument, store)?);
                arguments[0] = TaskArgument::Text(text);
            }
        }
        let text = self.render(&arguments, store, print.radix, &call.scope)?;
        let target = target.clone();
        drive(store, &target, &string_bits(&text))?;
        Ok(())
    }

    /// The file descriptor or channel mask a `$f…` argument evaluates to.
    ///
    /// A descriptor decides where output goes, so a value that is not fully
    /// known names nowhere and is an error saying so — dropping the call would
    /// make a design that printed nothing look exactly like one that passed. A
    /// *known* value naming a channel nothing opened is not an error: see
    /// [`StateStore::write_channels`].
    fn channel_mask(
        &self,
        argument: &TaskArgument,
        store: &StateStore,
    ) -> Result<u32, SimulationError> {
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
        Ok(channel as u32)
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
                    //
                    // A bare `$realtime` is the exception: it prints with as
                    // many decimals as its module's precision is finer than its
                    // unit, so `1.23` under `` `timescale 1ns/10ps `` and `5`
                    // with no `` `timescale `` at all, where `$realtime + 1.0`
                    // beside it is `2.23000` (iverilog 12.0).
                    if value.is_real() {
                        let rendered = if is_realtime_call(argument) {
                            format!("{:.*}", self.realtime_digits(scope), value.to_f64())
                        } else {
                            real_text(value.to_f64(), RealFormat::Bare, None)
                        };
                        text.push_str(&rendered);
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
                let (rendered, default_width) = {
                    // A literal reaches `%s` as its own bytes, exactly as it
                    // reaches `%d` as a number: `""` is one NUL character and
                    // `"\000a\000b"` is four, so both pad to the characters the
                    // vector has and both drop their *leading* NULs. Printing
                    // the literal's text instead would make `%s` the one
                    // specifier a string argument did not go through a vector
                    // for (corpus `string13`, `string14`).
                    let value = match argument {
                        TaskArgument::Text(literal) => string_bits(literal),
                        other => self.value_of(other, store)?,
                    };
                    // The bits of a real are an IEEE-754 encoding, not
                    // characters, so there is no text in one to print.
                    // iverilog warns and prints `<%s>`; there is no warning
                    // channel here, so it is an error that says so.
                    if value.is_real() {
                        return Err(bad_format("`%s` has no meaning for a real value"));
                    }
                    (ascii(&value), value.width().div_ceil(8))
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
                let rendered = match argument {
                    TaskArgument::Value(expression) => {
                        strengths(&value, resolved(expression, store))
                    }
                    // A string literal names no net, so its bits are read as
                    // an ordinary driver's.
                    TaskArgument::Text(_) => strengths(&value, None),
                };
                text.push_str(&pad(rendered, width.unwrap_or(0), fill));
                continue;
            }

            // `%t` is a time value, and `$timeformat` — not the radix the task
            // name asked for — says how it reads. An explicit width still wins
            // over the one `$timeformat` set, so `%0t` never pads.
            if specifier.eq_ignore_ascii_case(&'t') {
                let value = self.value_of(argument, store)?;
                let rendered =
                    self.time_format
                        .render(&value, self.tick_fs(scope), self.default_time_units());
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
            units: Some(time_units_fs(units)),
            precision,
            suffix,
            min_width,
        };
        Ok(())
    }

    /// `$readmemh(file, memory)` / `$readmemb(file, memory[, start[, finish]])`
    /// — fills a memory from a text file of whitespace-separated words.
    ///
    /// The load runs from `start` towards `finish`. A defaulted `start` is the
    /// memory's **lowest** address and a defaulted `finish` its **highest**, so
    /// the load runs upward whichever way round the memory was declared —
    /// `mem [7:0]` fills `mem[0]` first. That is IEEE 1364-2005, which reversed
    /// the 1364-2001 rule of following the declaration; iverilog 12.0 warns
    /// about exactly this and defaults to 2005, and the corpus is a 2005 suite.
    /// Explicit bounds still set the direction: `$readmemh(f, mem, 5, 3)` loads
    /// `mem[5]`, `mem[4]`, `mem[3]`. An `@<hex>` entry in the file moves the
    /// load address without ending the load.
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
        let (first, last) = linear_addresses(memory.addresses(), &memory_name)?;
        let (lowest, highest) = (first.min(last), first.max(last));

        let start = match arguments.get(2) {
            Some(argument) => self.address_argument(argument, store, "start")?,
            None => lowest,
        };
        // Whether the design said where to stop decides what a file with more
        // words in it than that means. An explicit finish is an *instruction*
        // to stop there — `$readmemh(f, mem, 0, 3)` against an eight word file
        // loads four words and leaves the rest of the memory alone. A finish
        // that came from the declaration is a *description* of the memory, and
        // a file too big for it is a real mismatch between the two.
        let bounded = arguments.len() > 3;
        // With a start but no finish the load still runs *upward*, to the
        // highest address — not towards whichever end was declared last.
        let finish = match arguments.get(3) {
            Some(argument) => self.address_argument(argument, store, "finish")?,
            None => highest,
        };

        let path = Self::resolve_read_path(store, &name)?;
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
                    store.set_word(&memory_name, &[cursor], &memory_word(&digits, radix)?);
                    cursor += step;
                }
            }
        }
        Ok(())
    }

    /// `$writememh(file, memory[, start[, finish]])` — the reverse of
    /// [`TaskContext::read_memory`], one word per line.
    ///
    /// Its bounds default by the same IEEE 1364-2005 rule the load follows —
    /// lowest address first, whichever way round the memory was declared, and
    /// upward from a `start` given alone — while two explicit bounds write in
    /// the direction they name. iverilog 12.0 writes a descending `mem [3:0]`
    /// as `mem[0]` first.
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
        let (first, last) = linear_addresses(memory.addresses(), &memory_name)?;
        let (lowest, highest) = (first.min(last), first.max(last));
        let start = match arguments.get(2) {
            Some(argument) => self.address_argument(argument, store, "start")?,
            None => lowest,
        };
        let finish = match arguments.get(3) {
            Some(argument) => self.address_argument(argument, store, "finish")?,
            None => highest,
        };

        let step: i64 = if start <= finish { 1 } else { -1 };
        let mut text = String::new();
        let mut cursor = start;
        loop {
            let word = memory.word(Some(&[cursor]));
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

        // The same write path `$fopen` resolves against, so a design that
        // writes a memory and a design that writes a file put their output in
        // the same place.
        let path = store.resolve_write_path(&name);
        fs::write(&path, text).map_err(|error| {
            SimulationError::SystemTask(format!(
                "`$writemem…` could not write `{}`: {}",
                path.display(),
                error
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
    ///
    /// The list lives on the [`StateStore`] because `$fopen(name, "r")` is a
    /// system *function* and has to search the same directories from `eval`.
    fn resolve_read_path(store: &StateStore, name: &str) -> Result<PathBuf, SimulationError> {
        if let Some(path) = store.resolve_read_path(name) {
            return Ok(path);
        }
        let mut tried = vec![match std::env::current_dir() {
            Ok(directory) => directory.display().to_string(),
            Err(_) => "the working directory".to_string(),
        }];
        tried.extend(
            store
                .search_paths()
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

/// What one `$dumpvars` argument names.
///
/// It is a *name*, not a value: `$dumpvars(0, top.u1)` names a scope, which no
/// expression could evaluate to. A memory word is the one argument that
/// carries an index, and the index is evaluated because it may be a parameter.
fn dump_target(argument: &TaskArgument, store: &StateStore) -> Result<DumpTarget, SimulationError> {
    match argument {
        TaskArgument::Value(Expression::Identifier(identifier)) => {
            Ok(DumpTarget::Name(identifier.name.clone()))
        }
        TaskArgument::Value(Expression::BitSelect(identifier, index)) => {
            let address = eval(index, store)?;
            let address = integer_value(&address).ok_or_else(|| {
                SimulationError::SystemTask(
                    "`$dumpvars` needs a known address to dump a memory word".to_string(),
                )
            })?;
            Ok(DumpTarget::Word(identifier.name.clone(), address as i64))
        }
        _ => Err(SimulationError::SystemTask(
            "`$dumpvars` takes a level and then the scopes and variables to dump".to_string(),
        )),
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

/// The one address range a memory file is read into or written from.
///
/// Both `$readmem…` and `$writemem…` run an array from one address to another,
/// which only a *one-dimensional* array has. A multi-dimensional one is named
/// rather than loaded row by row: the file format says nothing about where one
/// row ends, so any order chosen for it would be this simulator's invention.
fn linear_addresses(addresses: &[(i64, i64)], name: &str) -> Result<(i64, i64), SimulationError> {
    match addresses {
        [single] => Ok(*single),
        _ => Err(SimulationError::SystemTask(format!(
            "`{}` has {} dimensions, and a memory file names one address per word",
            name,
            addresses.len()
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

/// The strengths a `%v` argument names, when it names a *net* whose drivers
/// were resolved.
///
/// Only a net that goes through `Simulator::resolve_contributions` has one
/// recorded, so anything else — a `reg`, a bit of one, an expression — answers
/// `None` and is rendered from its value. The bits come back most significant
/// first, the order `strengths` walks them in.
fn resolved(expression: &Expression, store: &StateStore) -> Option<Vec<Strength>> {
    match expression {
        Expression::Identifier(id) => {
            let signal = store.get_signal(&id.name)?;
            Some(signal.strengths()?.to_vec())
        }
        Expression::BitSelect(..) | Expression::PartSelect(..) => {
            let ResolvedTarget::Bits { name, indices } = resolve_target(store, expression).ok()?
            else {
                return None;
            };
            let signal = store.get_signal(&name)?;
            let levels = signal.strengths()?;
            Some(
                indices
                    .iter()
                    .map(|index| {
                        signal
                            .bit_position(*index)
                            .and_then(|position| levels.get(position).copied())
                            .unwrap_or(Strength::HIGHZ)
                    })
                    .collect(),
            )
        }
        _ => None,
    }
}

/// The strength of every bit, most significant first, `_` between them:
/// `St0_Pu1_Pu1_St0`.
///
/// A net whose drivers were resolved carries the levels they settled on
/// (`levels`); anything else is read off the *value*, which is what an
/// ordinary continuous assignment or a procedural write would have given it —
/// a `z` bit is driven by nothing, which is `HiZ`, and every other bit is
/// `strong`.
fn strengths(value: &Register, levels: Option<Vec<Strength>>) -> String {
    let width = value.width();
    (0..width)
        .rev()
        .map(|index| {
            let code = value.bit_from_lsb(index).unwrap_or(X);
            let strength = levels
                .as_ref()
                .and_then(|levels| levels.get(width - 1 - index).copied())
                // A recorded strength and the value must agree: a `force` or a
                // procedural write lands on the net *after* the resolution
                // that recorded one, so a stale level would print a strength
                // for a value it no longer describes.
                .filter(|strength| strength.value() == code)
                .unwrap_or_else(|| Strength::driven(code, DriveStrength::STRONG));
            render_strength(strength)
        })
        .collect::<Vec<_>>()
        .join("_")
}

/// The two-letter mnemonic of one strength level, IEEE 1364-2005 Table 7-5.
const STRENGTH_NAMES: [&str; 8] = ["Hi", "Sm", "Me", "We", "La", "Pu", "St", "Su"];

/// One bit's strength as the three characters `%v` prints.
///
/// The interval is what makes the rendering: a range that drives one way only
/// has a *value* and prints the level's mnemonic beside it (`St0`, `Pu1`), one
/// that reaches high impedance is that value "or `z`" and prints `L` or `H`
/// (`StL`, `SuH`), and one that straddles zero is unknown and prints `X`.
/// Where the two ends disagree about the **level**, the mnemonic has nowhere
/// to put both, so the two digits are printed instead — `65X` is a `strong`
/// `0` against a `pull` `1`, and `650` a definite `0` somewhere between the
/// two. Every one of those spellings is a line of corpus `pr544`'s gold file.
fn render_strength(strength: Strength) -> String {
    let (low, high) = strength.bounds();
    if low == 0 && high == 0 {
        return "HiZ".to_string();
    }
    // The two levels the range runs between, strongest first, and the value
    // they leave. `L` and `H` are the ranges that reach high impedance, and
    // their weak end is that `0` rather than a level to print.
    let (strongest, weakest, value) = if low < 0 && high > 0 {
        (-low, high, 'X')
    } else if high <= 0 {
        (-low, -high, if high == 0 { 'L' } else { '0' })
    } else {
        (high, low, if low == 0 { 'H' } else { '1' })
    };
    if value == 'L' || value == 'H' || strongest == weakest {
        format!("{}{}", STRENGTH_NAMES[strongest as usize], value)
    } else {
        format!("{}{}{}", strongest, weakest, value)
    }
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
pub(crate) fn ascii(register: &Register) -> String {
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

/// Whether an argument is a bare `$realtime`, which prints at its module's
/// precision rather than as any other real does.
fn is_realtime_call(argument: &TaskArgument) -> bool {
    matches!(
        argument,
        TaskArgument::Value(Expression::SystemFunctionCall(name, _)) if name == "realtime"
    )
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

    /// A descriptor naming a channel nothing opened writes nowhere and does not
    /// stop the design — which is what iverilog 12.0 does with one; it warns on
    /// standard error and carries on. A descriptor that is not a *known* value
    /// still is an error, and so is a `$f…` task with no descriptor at all.
    #[test]
    fn test_an_unopened_channel_writes_nowhere_and_a_missing_one_is_an_error() {
        assert_eq!(printed(r#"$fdisplay(4, "PASSED");"#, &store_with(&[])), "");
        // Bit 0 is still standard output even beside a channel nothing opened.
        assert_eq!(
            printed(r#"$fdisplay(5, "PASSED");"#, &store_with(&[])),
            "PASSED\n"
        );

        let message = error(r#"$fdisplay(1'bx, "PASSED");"#);
        assert!(
            message.contains("must be a known value"),
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

    /// A scratch directory of this test's own, so two tests never share a file.
    fn scratch(name: &str) -> PathBuf {
        let directory = std::env::temp_dir().join(format!("visilog-{}", name));
        let _ = fs::remove_dir_all(&directory);
        fs::create_dir_all(&directory).expect("scratch directory should be creatable");
        directory
    }

    /// `$fopen` allocates a *bit* of a multi-channel descriptor, from bit 1 up,
    /// because bit 0 is standard output. Corpus `fopen1` and `fopen2` check the
    /// numbers themselves: 2, then 4, then 8.
    #[test]
    fn test_fopen_allocates_multi_channel_descriptor_bits() {
        let mut store = StateStore::new();
        store.set_output_directory(scratch("mcd"));
        assert_eq!(store.open_channel("one.txt"), 2);
        assert_eq!(store.open_channel("two.txt"), 4);
        assert_eq!(store.open_channel("three.txt"), 8);

        // A closed bit goes back into the pool, which is what `fopen2` asserts
        // by opening a fourth file after closing its second.
        store.close_channels(4);
        assert_eq!(store.open_channel("four.txt"), 4);
    }

    /// A file that cannot be opened is 0, not an error: `if (fp == 0)` is how
    /// a design says so itself, and an error would take that report away.
    #[test]
    fn test_a_file_that_cannot_be_opened_is_zero() {
        let store = StateStore::new();
        assert_eq!(store.open_channel("/no/such/directory/anywhere.txt"), 0);
        assert_eq!(store.open_descriptor("/no/such/directory/x.txt", "w"), 0);
        // A mode nothing means is 0 too, rather than a file opened some other
        // way round.
        assert_eq!(store.open_descriptor("/no/such/directory/x.txt", "zz"), 0);
    }

    /// The whole point of the bit mask: `$fdisplay(fp|1, …)` writes to the file
    /// *and* to the buffer a self-checking test reads. That is how a corpus
    /// design's file output reaches the harness at all.
    #[test]
    fn test_fdisplay_writes_to_a_file_and_to_standard_output_at_once() {
        let directory = scratch("both");
        let mut store = StateStore::new();
        store.set_output_directory(&directory);
        let channel = store.open_channel("out.txt");
        assert_eq!(channel, 2);
        store.set_ranged("fp", Register::from_u128(channel as u128, 32), (31, 0));

        let mut context = TaskContext::new();
        run_in(&mut context, r#"$fdisplay(fp|1, "both");"#, &mut store);
        run_in(&mut context, r#"$fdisplay(fp, "file only");"#, &mut store);
        run_in(&mut context, r#"$fdisplay(1, "buffer only");"#, &mut store);
        assert_eq!(context.output().text(), "both\nbuffer only\n");

        // Closing is what flushes the buffered writer.
        store.close_channels(channel);
        let written = fs::read_to_string(directory.join("out.txt")).expect("file should exist");
        assert_eq!(written, "both\nfile only\n");
    }

    /// `$fopen` is a system *function*, so it runs in `eval` against a
    /// `&StateStore` — which is exactly why the file table lives on the store.
    /// A name built out of a concatenation is the case corpus `sp2` and `pr1065`
    /// need.
    #[test]
    fn test_fopen_is_an_expression_and_takes_a_built_up_name() {
        let directory = scratch("expr");
        let mut store = StateStore::new();
        store.set_output_directory(&directory);
        let expression = crate::parsers::expr::verilog_expression(r#"$fopen({"sub", ".txt"})"#)
            .expect("expression should parse")
            .1;
        let value = eval(&expression, &store).expect("$fopen should evaluate");
        assert_eq!(value.to_u128(), Some(2));
        store.close_channels(2);
        assert!(directory.join("sub.txt").is_file());
    }

    /// `$sformat` formats into a register instead of printing, and the target's
    /// width does the rest — measured against iverilog 12.0: a wider target is
    /// zero extended on the left, and a narrower one keeps the *last*
    /// characters.
    #[test]
    fn test_sformat_writes_the_rendered_text_into_a_register() {
        let mut store = store_with(&[("s", &"0".repeat(80)), ("narrow", "0000000000000000")]);
        let mut context = TaskContext::new();
        run_in(
            &mut context,
            r#"$sformat(s, "x=%0d y=%s", 42, "hi");"#,
            &mut store,
        );
        assert_eq!(context.output().text(), "", "$sformat printed");
        assert_eq!(ascii(&store.get("s").expect("s should exist")), "x=42 y=hi");

        run_in(&mut context, r#"$sformat(narrow, "abcdef");"#, &mut store);
        assert_eq!(
            ascii(&store.get("narrow").expect("narrow should exist")),
            "ef"
        );

        // `$swrite` is the same task under the other spelling, and its radix
        // suffix decomposes the way every other one does.
        run_in(&mut context, r#"$swriteb(s, 4'b1010);"#, &mut store);
        assert_eq!(ascii(&store.get("s").expect("s should exist")), "1010");
    }

    /// The one difference between the two spellings: `$sformat`'s second
    /// argument is the format string even when it is a register holding one,
    /// where `$swrite` prints the same argument as a value.
    #[test]
    fn test_sformat_takes_a_variable_format_string_and_swrite_does_not() {
        let mut store = store_with(&[("s", &"0".repeat(80))]);
        store.set_ranged("fmt", string_bits("a=%0d").resize(40), (39, 0));
        let mut context = TaskContext::new();

        run_in(&mut context, r#"$sformat(s, fmt, 7);"#, &mut store);
        assert_eq!(ascii(&store.get("s").expect("s should exist")), "a=7");

        run_in(&mut context, r#"$swrite(s, fmt, 7);"#, &mut store);
        assert_ne!(ascii(&store.get("s").expect("s should exist")), "a=7");
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

    /// `$printtimescale` in a context that was never told the design's scopes —
    /// which is every `TaskContext` not built by `Simulator::setup`, including
    /// the one `exec::execute_statements` runs a block against — reports the
    /// name it was given rather than inventing a scale for it. A plausible
    /// `1s / 1s` there would look exactly like a design whose modules really
    /// declared nothing.
    #[test]
    fn test_printtimescale_without_a_scope_table_names_what_it_could_not_find() {
        let message = error("$printtimescale(top.dut);");
        assert!(message.contains("top.dut"), "{}", message);
    }

    /// `$stop` ends the run exactly as `$finish` does, printing nothing.
    ///
    /// That is the equivalence iverilog itself draws: `vvp -n` reads
    /// "Non-interactive ($stop = $finish)" in its own help text, and visilog —
    /// a library with no console — is by construction that case. Left to its
    /// debugger `vvp` instead prints `** VVP Stop(0) **` and, reading an empty
    /// console, carries on; no gold file in the corpus records that banner and
    /// visilog has no source line to name in it. The optional argument is a
    /// diagnostic level and is ignored, the way `$finish`'s is.
    #[test]
    fn test_stop_ends_the_run_the_way_finish_does() {
        let mut store = store_with(&[]);
        let mut context = TaskContext::new();
        run_in(&mut context, "$stop;", &mut store);
        assert!(context.finished());
        assert_eq!(context.output().text(), "");

        let mut context = TaskContext::new();
        run_in(&mut context, "$stop(2);", &mut store);
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

    /// Every three-character spelling `%v` has, driven straight off the
    /// interval so the rendering is tested apart from the resolution.
    ///
    /// Each is a line of corpus `pr544`'s gold file, which iverilog 12.0
    /// produces for two `bufif1`s on one net at `(pull0, pull1)` and
    /// `(strong0, strong1)`: `HiZ`, `PuL`, `StL`, `PuH`, `StH`, `Pu0`, `St0`,
    /// `Pu1`, `St1`, `PuX`, `StX`, `650`, `65X`, `651`, `56X`.
    #[test]
    fn test_every_strength_spelling() {
        for (low, high, expected) in [
            (0, 0, "HiZ"),
            (-5, 0, "PuL"),
            (-6, 0, "StL"),
            (0, 5, "PuH"),
            (0, 6, "StH"),
            (-5, -5, "Pu0"),
            (-6, -6, "St0"),
            (5, 5, "Pu1"),
            (6, 6, "St1"),
            (-5, 5, "PuX"),
            (-6, 6, "StX"),
            (-6, -5, "650"),
            (-6, 5, "65X"),
            (5, 6, "651"),
            (-5, 6, "56X"),
            (-7, 7, "SuX"),
            (-7, -6, "760"),
            (6, 7, "761"),
        ] {
            assert_eq!(
                render_strength(Strength::span(low, high)),
                expected,
                "[{}, {}]",
                low,
                high
            );
        }
    }

    /// A `%v` of a net whose drivers were *resolved* reports the level they
    /// settled on, where one of a plain register reports `strong`.
    ///
    /// Measured against iverilog 12.0: `assign (pull1, strong0) net = 4'b0110;`
    /// with `$display("%v", net)` prints `St0_Pu1_Pu1_St0` (corpus
    /// `multi_bit_strength`, whose whole point is that the two halves differ).
    #[test]
    fn test_a_resolved_net_reports_the_level_it_settled_at() {
        let mut store = store_with(&[("net", "0110")]);
        store.set_strengths(
            "net",
            vec![
                Strength::span(-6, -6),
                Strength::span(5, 5),
                Strength::span(5, 5),
                Strength::span(-6, -6),
            ],
        );
        assert_eq!(
            printed(r#"$display("[%v]", net);"#, &store),
            "[St0_Pu1_Pu1_St0]\n"
        );
        // A bit select of one reads the same levels, picked by index.
        assert_eq!(printed(r#"$display("[%v]", net[2]);"#, &store), "[Pu1]\n");
        assert_eq!(
            printed(r#"$display("[%v]", net[2:1]);"#, &store),
            "[Pu1_Pu1]\n"
        );
    }

    /// A level a later write left behind is **not** printed. A `force` or a
    /// procedural write lands on a net after the resolution that recorded its
    /// strength, so a stale level would name a strength for a value the net no
    /// longer holds — `St1` beside a `0` is not something any driver produces.
    #[test]
    fn test_a_strength_that_no_longer_matches_the_value_is_dropped() {
        let mut store = store_with(&[("net", "0000")]);
        store.set_strengths("net", vec![Strength::span(5, 5); 4]);
        assert_eq!(
            printed(r#"$display("[%v]", net);"#, &store),
            "[St0_St0_St0_St0]\n"
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
        store.declare_memory("mem", vec![(0, 3)], (7, 0), false);
        let mut context = TaskContext::new();

        // An explicit finish says where to stop, so the four words past it are
        // simply not loaded.
        let source = format!(r#"$readmemh("{}", mem, 0, 3);"#, file.display());
        run_in(&mut context, &source, &mut store);
        assert_eq!(
            store.memory("mem").expect("mem").word(Some(&[3])),
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

    /// `$timeformat` with no arguments puts `%t` back where it started —
    /// twenty columns of the tick this design counts, which with no
    /// `` `timescale `` anywhere is a second. `$timeformat(-9, …)` asks for
    /// that same tick in nanoseconds, so it is a thousand million of them;
    /// iverilog 12.0 prints `7000000000.00 ns` for the same pairing.
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
            vec!["[5000000000.0ns]", "[                   5]"]
        );
    }
}

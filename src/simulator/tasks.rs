//! System tasks — `$display`, `$write`, `$finish`, `$time`.
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

use crate::parsers::behavior::{SystemTaskArgument, SystemTaskCall};
use crate::parsers::expr::Expression;
use crate::register::Register;
use crate::simulator::elaborate::rename_expression;
use crate::simulator::eval::eval;
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

/// A system task this simulator can carry out.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SystemTask {
    /// Format the arguments, print them, and end the line.
    Display,
    /// [`SystemTask::Display`] without the newline.
    Write,
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
    /// `$time`.
    Time,
    /// An expression, evaluated against the store when the call runs.
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
    /// Fails on a task that is not recognised, and on `$strobe` / `$monitor`,
    /// which are recognised but defer their output to the end of a time step —
    /// a scheduling slot that does not exist here. Both report the name, so a
    /// design never prints nothing by accident.
    pub fn compile(call: &SystemTaskCall) -> Result<TaskCall, SimulationError> {
        let task = match call.name.as_str() {
            "display" => SystemTask::Display,
            "write" => SystemTask::Write,
            "finish" => SystemTask::Finish,
            "time" => SystemTask::Time,
            "strobe" | "monitor" => {
                return Err(SimulationError::SystemTask(format!(
                    "`${}` defers its output to the end of a time step, which is not scheduled",
                    call.name
                )))
            }
            other => return Err(unknown_task(other)),
        };

        let arguments = call
            .arguments
            .iter()
            .map(|argument| match argument {
                SystemTaskArgument::String(text) => Ok(TaskArgument::Text(text.clone())),
                SystemTaskArgument::Expression(expression) => {
                    Ok(TaskArgument::Value(expression.clone()))
                }
                SystemTaskArgument::SystemFunction(name) if name == "time" => {
                    Ok(TaskArgument::Time)
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

/// What a system task acts on: where output goes, what time it is, and whether
/// the design has called `$finish`.
#[derive(Clone, Debug, Default)]
pub struct TaskContext {
    time: i64,
    output: Output,
    finished: bool,
}

impl TaskContext {
    pub fn new() -> Self {
        TaskContext::default()
    }

    /// Everything the design has printed.
    pub fn output(&self) -> &Output {
        &self.output
    }

    /// The time `$time` reports.
    pub fn time(&self) -> i64 {
        self.time
    }

    /// Tells the context what time it is. The driver does this before it
    /// resumes a block, so `$time` reads the timestamp the block runs at.
    pub fn set_time(&mut self, time: i64) {
        self.time = time;
    }

    /// Whether the design has called `$finish`.
    pub fn finished(&self) -> bool {
        self.finished
    }

    /// Carries out one call, appending whatever it prints to the output.
    pub fn run(&mut self, call: &TaskCall, store: &StateStore) -> Result<(), SimulationError> {
        match call.task {
            SystemTask::Display => {
                let text = self.render(&call.arguments, store)?;
                self.output.push_line(&text);
            }
            SystemTask::Write => {
                let text = self.render(&call.arguments, store)?;
                self.output.push(&text);
            }
            // `$finish` takes an optional diagnostic level, which says how much
            // the simulator should report about itself on the way out.
            SystemTask::Finish => self.finished = true,
            SystemTask::Time => {}
        }
        Ok(())
    }

    /// Formats an argument list the way `$display` does: a string argument is a
    /// format string and consumes as many of the arguments after it as it has
    /// specifiers; anything left over is printed in the default format, which
    /// is decimal.
    fn render(
        &self,
        arguments: &[TaskArgument],
        store: &StateStore,
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
                    text.push_str(&pad(decimal(&value), decimal_width(value.width())));
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

            let value = self.value_of(argument, store)?;
            let (rendered, default_width) = match specifier {
                'd' | 'D' => (decimal(&value), decimal_width(value.width())),
                'b' | 'B' => (binary(&value), value.width()),
                'h' | 'H' | 'x' | 'X' => (hex(&value), value.width().div_ceil(4)),
                'o' | 'O' => (octal(&value), value.width().div_ceil(3)),
                other => {
                    return Err(bad_format(&format!(
                        "`%{}` is not a format this simulator understands",
                        other
                    )))
                }
            };
            text.push_str(&pad(rendered, width.unwrap_or(default_width)));
        }
        Ok(())
    }

    fn value_of(
        &self,
        argument: &TaskArgument,
        store: &StateStore,
    ) -> Result<Register, SimulationError> {
        match argument {
            TaskArgument::Value(expression) => Ok(eval(expression, store)?),
            TaskArgument::Time => Ok(Register::from_u128(self.time.unsigned_abs() as u128, 64)),
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
        run_in(&mut context, source, store);
        context.output().text().to_string()
    }

    fn run_in(context: &mut TaskContext, source: &str, store: &StateStore) {
        let (remaining, call) = parse_system_task(source).expect("task should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        let call = TaskCall::compile(&call).expect("task should compile");
        context.run(&call, store).expect("task should run");
    }

    fn error(source: &str) -> String {
        let (_, call) = parse_system_task(source).expect("task should parse");
        let store = store_with(&[]);
        let compiled = match TaskCall::compile(&call) {
            Err(error) => return error.to_string(),
            Ok(compiled) => compiled,
        };
        TaskContext::new()
            .run(&compiled, &store)
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
        let store = store_with(&[]);
        let mut context = TaskContext::new();
        run_in(&mut context, r#"$write("PAS");"#, &store);
        run_in(&mut context, r#"$write("SED");"#, &store);
        assert_eq!(context.output().text(), "PASSED");
        assert_eq!(context.output().lines(), vec!["PASSED"]);

        run_in(&mut context, r#"$display("!");"#, &store);
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

    #[test]
    fn test_time_is_an_argument() {
        let store = store_with(&[]);
        let mut context = TaskContext::new();
        context.set_time(42);
        run_in(&mut context, r#"$display("t=%0d", $time);"#, &store);
        assert_eq!(context.output().text(), "t=42\n");
    }

    #[test]
    fn test_finish_is_recorded_rather_than_exiting() {
        let store = store_with(&[]);
        let mut context = TaskContext::new();
        assert!(!context.finished());
        run_in(&mut context, "$finish;", &store);
        assert!(context.finished());
    }

    #[test]
    fn test_an_unknown_task_is_an_error_that_names_it() {
        assert_eq!(error("$nosuchthing;"), "unknown system task `$nosuchthing`");
        assert_eq!(
            error(r#"$display("%0d", $nosuchfunction);"#),
            "unknown system task `$nosuchfunction`"
        );
    }

    #[test]
    fn test_deferred_output_tasks_are_rejected_by_name() {
        for source in [r#"$strobe("a");"#, r#"$monitor("a");"#] {
            let message = error(source);
            assert!(
                message.contains("defers its output"),
                "unexpected message: {}",
                message
            );
        }
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
}

//! One reproducible simulation run: a configuration in, a record out.
//!
//! This is the contract a command line or an embedding client drives. A
//! [`RunConfig`] names everything a run depends on — the source files in
//! order, the top module, include directories, defines, plus-args, the
//! directories data files are read from and written to, and the limits — and
//! [`run`] hands back a [`RunRecord`] saying exactly how it ended. The record
//! carries the tool version and a hash of every source, so two runs can be
//! compared and one can be repeated.
//!
//! Nothing here prints. The design's own output is in the record, and the
//! caller decides where it goes.

use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::parsers::preprocessor::{Preprocessor, Timescale};
use crate::parsers::source::{parse_expanded, reachable_modules, root_module, SourceError};
use crate::simulator::eval::EvalError;
use crate::simulator::runner::{SimulationError, Simulator};

/// The version of the [`RunRecord`] layout. It moves when a field changes
/// meaning or goes away; adding a field does not move it.
pub const RUN_RECORD_SCHEMA: u32 = 1;

/// Everything a run depends on.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct RunConfig {
    /// The source files, read in order as one compilation unit: a
    /// `` `define `` in one is visible in every file after it.
    pub sources: Vec<PathBuf>,
    /// The module to elaborate. `None` picks the one nothing instantiates.
    pub top: Option<String>,
    /// Where `` `include `` looks, in order.
    pub include_dirs: Vec<PathBuf>,
    /// Macros defined before the first file, as `-DNAME=value` would.
    pub defines: Vec<(String, String)>,
    /// `+name=value` words for `$test$plusargs` / `$value$plusargs`.
    pub plusargs: Vec<String>,
    /// Where `$readmemh` and a read-mode `$fopen` look for a relative path,
    /// after the working directory.
    pub search_paths: Vec<PathBuf>,
    /// Where a relative `$dumpfile`, `$fopen` or `$writememh` lands.
    /// `None` is the working directory.
    pub output_dir: Option<PathBuf>,
    /// The `` `timescale `` of a module written before any directive, as
    /// iverilog's `+timescale+1ns/1ps` sets it.
    pub default_timescale: Option<String>,
    /// Stop once simulated time passes this many units of the top module's
    /// `` `timescale ``. `None` is no limit.
    pub time_limit: Option<i64>,
    /// Stop after this many timesteps. `None` is no limit, as in iverilog.
    /// A timestep costs the same however far apart two are, so this — rather
    /// than a span of simulated time — is what bounds a design that never
    /// finishes.
    pub step_limit: Option<u64>,
}

/// The name and version of the tool that produced a record.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ToolInfo {
    pub name: String,
    pub version: String,
}

impl ToolInfo {
    pub fn current() -> ToolInfo {
        ToolInfo {
            name: env!("CARGO_PKG_NAME").to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
        }
    }
}

/// One source file as it was read, so a later run can tell whether it is
/// looking at the same design.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceRecord {
    pub path: PathBuf,
    pub bytes: usize,
    pub sha256: String,
}

/// Why a run stopped. Exactly one applies.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StopReason {
    /// The design called `$finish` (or `$stop`, or `$fatal`).
    Finished,
    /// Nothing was left scheduled: the design had stopped moving.
    Quiescent,
    /// Simulated time reached [`RunConfig::time_limit`].
    TimeLimit,
    /// The run used [`RunConfig::step_limit`] timesteps.
    StepLimit,
    /// The caller asked the run to stop.
    Cancelled,
    /// A source could not be read.
    Io,
    /// The preprocessor or the grammar rejected a source.
    Compile,
    /// The design uses something the simulator does not implement.
    Unsupported,
    /// Elaboration refused the design for a reason of its own.
    Elaboration,
    /// Running the design raised an error.
    Runtime,
}

impl StopReason {
    /// Whether the design ran to an end of its own choosing, with nothing
    /// stopping it from outside.
    pub fn completed(self) -> bool {
        matches!(self, StopReason::Finished | StopReason::Quiescent)
    }
}

/// How serious a [`Diagnostic`] is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Severity {
    Error,
    Warning,
    Note,
}

/// Something the tool has to say about a run, apart from the design's own
/// output.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Diagnostic {
    pub severity: Severity,
    /// A stable, machine-readable name for the kind of diagnostic.
    pub code: String,
    pub message: String,
    /// `file:line`, when the tool knows one.
    pub location: Option<String>,
}

/// How a run ended and everything needed to repeat it.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RunRecord {
    pub schema: u32,
    pub tool: ToolInfo,
    pub config: RunConfig,
    pub sources: Vec<SourceRecord>,
    /// The module that was elaborated, once one was chosen.
    pub top: Option<String>,
    pub stop: StopReason,
    /// Simulated time at the end, in clock ticks.
    pub end_ticks: i64,
    /// How long one clock tick is, in femtoseconds.
    pub tick_femtoseconds: u64,
    /// How many clock ticks one unit of the top module's `` `timescale `` is.
    pub ticks_per_unit: i64,
    /// Timesteps the run took.
    pub steps: u64,
    /// How many `$error`s, `$fatal`s and failed assertions the design reported.
    pub assertion_failures: u64,
    /// Everything the design printed, in order.
    pub output: String,
    pub diagnostics: Vec<Diagnostic>,
}

impl RunRecord {
    /// The process exit status a command line reports this record with.
    ///
    /// | status | meaning |
    /// | --- | --- |
    /// | 0 | completed with no assertion failure |
    /// | 1 | completed, but the design reported an assertion failure |
    /// | 2 | a source could not be read or did not compile |
    /// | 3 | the design uses something unsupported |
    /// | 4 | elaboration or the run itself raised an error |
    /// | 5 | a time or step limit was reached |
    /// | 6 | cancelled |
    pub fn exit_status(&self) -> i32 {
        match self.stop {
            StopReason::Finished | StopReason::Quiescent => {
                if self.assertion_failures == 0 {
                    0
                } else {
                    1
                }
            }
            StopReason::Io | StopReason::Compile => 2,
            StopReason::Unsupported => 3,
            StopReason::Elaboration | StopReason::Runtime => 4,
            StopReason::TimeLimit | StopReason::StepLimit => 5,
            StopReason::Cancelled => 6,
        }
    }

    fn fail(&mut self, stop: StopReason, code: &str, message: String, location: Option<String>) {
        self.stop = stop;
        self.diagnostics.push(Diagnostic {
            severity: Severity::Error,
            code: code.to_string(),
            message,
            location,
        });
    }
}

/// Runs the design `config` describes.
///
/// `cancel` is polled between timesteps; setting it stops the run at the next
/// one with [`StopReason::Cancelled`], leaving the design at a settled
/// timestamp.
pub fn run(config: &RunConfig, cancel: Option<&AtomicBool>) -> RunRecord {
    let mut record = RunRecord {
        schema: RUN_RECORD_SCHEMA,
        tool: ToolInfo::current(),
        config: config.clone(),
        sources: Vec::new(),
        top: None,
        stop: StopReason::Io,
        end_ticks: 0,
        tick_femtoseconds: 0,
        ticks_per_unit: 1,
        steps: 0,
        assertion_failures: 0,
        output: String::new(),
        diagnostics: Vec::new(),
    };

    let mut texts = Vec::with_capacity(config.sources.len());
    for path in &config.sources {
        match std::fs::read_to_string(path) {
            Ok(text) => {
                record.sources.push(SourceRecord {
                    path: path.clone(),
                    bytes: text.len(),
                    sha256: sha256_hex(text.as_bytes()),
                });
                texts.push(text);
            }
            Err(error) => {
                record.fail(
                    StopReason::Io,
                    "io",
                    format!("cannot read {}: {}", path.display(), error),
                    None,
                );
                return record;
            }
        }
    }

    let mut simulator = match build(config, &texts) {
        Ok(simulator) => simulator,
        Err((stop, code, message, location)) => {
            record.fail(stop, code, message, location);
            return record;
        }
    };
    record.top = Some(simulator.top().to_string());

    if let Err(error) = simulator.setup() {
        let stop = error_stop(&error, StopReason::Elaboration);
        record.fail(stop, stop_code(stop), error.to_string(), None);
        return record;
    }
    record.tick_femtoseconds = simulator.tick_femtoseconds();
    record.ticks_per_unit = simulator.ticks_per_unit();

    let outcome = drive(&mut simulator, config, cancel, &mut record.steps);
    record.end_ticks = simulator.now();
    record.output = simulator.output().text();
    record.assertion_failures = simulator.assertion_failures();
    match outcome {
        Ok(stop) => {
            record.stop = stop;
            match stop {
                StopReason::TimeLimit => record.diagnostics.push(Diagnostic {
                    severity: Severity::Warning,
                    code: "time_limit".into(),
                    message: format!(
                        "stopped at the time limit of {} units",
                        config.time_limit.unwrap_or_default()
                    ),
                    location: None,
                }),
                StopReason::StepLimit => record.diagnostics.push(Diagnostic {
                    severity: Severity::Warning,
                    code: "step_limit".into(),
                    message: format!("stopped after {} timesteps", record.steps),
                    location: None,
                }),
                _ => {}
            }
        }
        Err(error) => {
            let stop = error_stop(&error, StopReason::Runtime);
            record.fail(stop, stop_code(stop), error.to_string(), None);
        }
    }
    record
}

type BuildError = (StopReason, &'static str, String, Option<String>);

/// Preprocesses and parses the sources and picks the top.
fn build(config: &RunConfig, texts: &[String]) -> Result<Simulator, BuildError> {
    let mut preprocessor = Preprocessor::new();
    for dir in &config.include_dirs {
        preprocessor = preprocessor.with_include_dir(dir.clone());
    }
    // A file's own directory is where iverilog looks for an `include first.
    for path in &config.sources {
        if let Some(dir) = path.parent() {
            preprocessor = preprocessor.with_include_dir(dir.to_path_buf());
        }
    }
    for (name, value) in &config.defines {
        preprocessor = preprocessor.with_define(name.clone(), value.clone());
    }
    if let Some(text) = &config.default_timescale {
        let timescale = Timescale::parse(text).map_err(|why| {
            (
                StopReason::Compile,
                "config",
                format!("default timescale `{}`: {}", text, why),
                None,
            )
        })?;
        preprocessor = preprocessor.with_default_timescale(timescale);
    }

    let names: Vec<String> = config
        .sources
        .iter()
        .map(|path| path.display().to_string())
        .collect();
    let files: Vec<(&str, &str)> = names
        .iter()
        .zip(texts)
        .map(|(name, text)| (name.as_str(), text.as_str()))
        .collect();
    let expanded = preprocessor
        .preprocess_files(&files)
        .map_err(|error| (StopReason::Compile, "preprocess", error.to_string(), None))?;
    let parsed = parse_expanded(expanded).map_err(|error| match error {
        SourceError::Parse { at, detail } => (StopReason::Compile, "parse", detail, Some(at)),
        other => (StopReason::Compile, "preprocess", other.to_string(), None),
    })?;

    // A named top is iverilog's `-s`: only what it reaches is the design.
    // Without one every module counts, which is what iverilog does when it
    // elaborates every root.
    let (top, modules) = match &config.top {
        Some(top) => (top.clone(), reachable_modules(parsed.modules, top)),
        None => {
            let top = root_module(&parsed.modules).ok_or_else(|| {
                (
                    StopReason::Compile,
                    "no_modules",
                    "the sources declare no module".to_string(),
                    None,
                )
            })?;
            (top, parsed.modules)
        }
    };
    let mut simulator = Simulator::with_modules(modules, top);
    for directory in &config.search_paths {
        simulator.add_search_path(directory.clone());
    }
    for plusarg in &config.plusargs {
        simulator.add_plusarg(plusarg);
    }
    if let Some(dir) = &config.output_dir {
        simulator.set_output_directory(dir.clone());
    }
    Ok(simulator)
}

/// Steps the design from one scheduled timestamp to the next until it ends,
/// runs out of budget or is cancelled.
fn drive(
    simulator: &mut Simulator,
    config: &RunConfig,
    cancel: Option<&AtomicBool>,
    steps: &mut u64,
) -> Result<StopReason, SimulationError> {
    let limit = config
        .time_limit
        .map(|units| units.saturating_mul(simulator.ticks_per_unit()));
    // Time zero is a timestep like any other: the `initial` blocks run in it.
    simulator.advance(0)?;
    loop {
        if simulator.finished() {
            return Ok(StopReason::Finished);
        }
        if cancel.is_some_and(|flag| flag.load(Ordering::Relaxed)) {
            return Ok(StopReason::Cancelled);
        }
        let Some(next) = simulator.next_time() else {
            return Ok(StopReason::Quiescent);
        };
        if limit.is_some_and(|limit| next > limit) {
            return Ok(StopReason::TimeLimit);
        }
        if config.step_limit.is_some_and(|limit| *steps >= limit) {
            return Ok(StopReason::StepLimit);
        }
        simulator.advance(next - simulator.now())?;
        *steps += 1;
    }
}

/// Which stop an error is: something the simulator names as unsupported is
/// told apart from a design it refused.
fn error_stop(error: &SimulationError, otherwise: StopReason) -> StopReason {
    if is_unsupported(error) {
        StopReason::Unsupported
    } else {
        otherwise
    }
}

fn is_unsupported(error: &SimulationError) -> bool {
    matches!(
        error,
        SimulationError::Unsupported(_)
            | SimulationError::Eval(EvalError::UnsupportedFunctionCall(_))
    )
}

fn stop_code(stop: StopReason) -> &'static str {
    match stop {
        StopReason::Unsupported => "unsupported",
        StopReason::Elaboration => "elaboration",
        StopReason::Runtime => "runtime",
        StopReason::Compile => "compile",
        StopReason::Io => "io",
        StopReason::Finished
        | StopReason::Quiescent
        | StopReason::TimeLimit
        | StopReason::StepLimit
        | StopReason::Cancelled => "stop",
    }
}

fn sha256_hex(bytes: &[u8]) -> String {
    Sha256::digest(bytes)
        .iter()
        .map(|byte| format!("{:02x}", byte))
        .collect()
}

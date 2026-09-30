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

use std::fmt;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::parsers::modules::VerilogModule;
use crate::parsers::preprocessor::{Preprocessor, Timescale};
use crate::parsers::source::{parse_expanded, reachable_modules, root_module, SourceError};
use crate::simulator::elaborate::{OmissionKind, TimingOmission};
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
    /// Refuse to run a design that asks for timing this simulator does not
    /// carry out — see [`Capabilities`]. Off, the run goes ahead functionally
    /// and says what it left out.
    #[serde(default)]
    pub strict_timing: bool,
}

/// What a run simulated and what it did not.
///
/// Simulation here is **functional**. The timing a design expresses through
/// procedural delays, delayed continuous assignments and gate delays is
/// simulated; the timing a `specify` block expresses is parsed and recorded
/// and nothing more. A design built on vendor cell models that finishes
/// cleanly has therefore not been timing-verified, and this is where the
/// record says so rather than leaving a reader to infer it from silence.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Capabilities {
    /// `functional`, or `strict` when [`RunConfig::strict_timing`] was set.
    pub mode: String,
    /// The timing semantics this simulator carries out, whatever the design.
    pub simulated: Vec<String>,
    /// The timing constructs this design wrote that were not carried out.
    pub not_simulated: Vec<Omitted>,
}

/// One kind of timing construct a design wrote that was not carried out.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Omitted {
    /// `specify_path_delay`, `timing_check` or `switch_delay`.
    pub kind: String,
    /// How many there were across every instance.
    pub count: usize,
    /// The modules they were written in.
    pub modules: Vec<String>,
}

/// The timing semantics simulated for every design, for [`Capabilities`].
const SIMULATED_TIMING: &[&str] = &[
    "procedural delays (#n, intra-assignment, @, wait)",
    "continuous assignment delays (inertial, rise/fall/turn-off)",
    "gate and user-defined primitive delays (inertial, rise/fall/turn-off)",
    "non-blocking assignment ordering and delta cycles",
];

/// How many example sites a timing diagnostic names before it summarises.
const OMISSION_EXAMPLES: usize = 5;

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
    /// A breakpoint an interactive
    /// [`Session`](crate::inspect::Session) was watching for was hit. A
    /// [`run`] sets none, so its record never says this.
    Breakpoint,
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
    /// What was simulated and what was not, once the design elaborated.
    pub capabilities: Option<Capabilities>,
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
    /// | 7 | stopped at a breakpoint (an interactive session only) |
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
            StopReason::Breakpoint => 7,
        }
    }

    fn fail(&mut self, stop: StopReason, code: &str, message: String, location: Option<String>) {
        self.stop = stop;
        self.diagnostics
            .push(error_diagnostic(code, message, location));
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
        capabilities: None,
    };

    let mut simulator = match load(config) {
        Ok(loaded) => {
            record.sources = loaded.sources;
            loaded.simulator
        }
        Err(error) => {
            record.sources = error.sources;
            record.stop = error.stop;
            record.diagnostics.push(error.diagnostic);
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

    disclose_timing(
        &mut record,
        simulator.timing_omissions(),
        config.strict_timing,
    );
    if config.strict_timing && !simulator.timing_omissions().is_empty() {
        record.stop = StopReason::Unsupported;
        return record;
    }

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

/// Fills in [`RunRecord::capabilities`] and adds one diagnostic per kind of
/// timing the design wrote and the run leaves out: a warning when the run goes
/// ahead functionally, an error under `strict`. Each names how many there
/// were and the first few sites, so a netlist of ten thousand cells yields
/// three diagnostics rather than ten thousand.
fn disclose_timing(record: &mut RunRecord, omissions: &[TimingOmission], strict: bool) {
    let mut kinds: Vec<OmissionKind> = omissions.iter().map(|omission| omission.kind).collect();
    kinds.sort();
    kinds.dedup();
    let mut not_simulated = Vec::new();
    for kind in kinds {
        let sites: Vec<&TimingOmission> = omissions.iter().filter(|o| o.kind == kind).collect();
        let mut modules: Vec<String> = sites.iter().map(|site| site.module.clone()).collect();
        modules.sort();
        modules.dedup();
        let examples = sites
            .iter()
            .take(OMISSION_EXAMPLES)
            .map(|site| format!("{} {}", site.instance, site.detail))
            .collect::<Vec<_>>()
            .join("; ");
        let more = sites.len().saturating_sub(OMISSION_EXAMPLES);
        let what = match kind {
            OmissionKind::PathDelay => "specify path delays are recorded but not simulated",
            OmissionKind::TimingCheck => "specify timing checks are recorded but never checked",
            OmissionKind::SwitchDelay => "bidirectional switch delays are not simulated",
        };
        record.diagnostics.push(Diagnostic {
            severity: if strict {
                Severity::Error
            } else {
                Severity::Warning
            },
            code: kind.code().to_string(),
            message: format!(
                "{} ({}): {}{}",
                what,
                sites.len(),
                examples,
                if more > 0 {
                    format!("; and {} more", more)
                } else {
                    String::new()
                }
            ),
            location: None,
        });
        not_simulated.push(Omitted {
            kind: kind.code().to_string(),
            count: sites.len(),
            modules,
        });
    }
    record.capabilities = Some(Capabilities {
        mode: if strict { "strict" } else { "functional" }.to_string(),
        simulated: SIMULATED_TIMING.iter().map(|s| s.to_string()).collect(),
        not_simulated,
    });
}

/// A design [`load`] read and parsed, ready for [`Simulator::setup`].
pub struct Loaded {
    pub simulator: Simulator,
    /// Every source as it was read.
    pub sources: Vec<SourceRecord>,
}

/// Why [`load`] could not produce a [`Simulator`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoadError {
    /// [`StopReason::Io`] or [`StopReason::Compile`] from [`load`];
    /// [`Session::new`](crate::inspect::Session::new), which also sets the
    /// design up, may add [`StopReason::Elaboration`] and
    /// [`StopReason::Unsupported`].
    pub stop: StopReason,
    pub diagnostic: Diagnostic,
    /// The sources read before the failure, including the one that failed
    /// to compile.
    pub sources: Vec<SourceRecord>,
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.diagnostic.location {
            Some(at) => write!(f, "{}: {}", at, self.diagnostic.message),
            None => write!(f, "{}", self.diagnostic.message),
        }
    }
}

impl LoadError {
    fn compile(code: &str, message: String, location: Option<String>) -> LoadError {
        LoadError {
            stop: StopReason::Compile,
            diagnostic: error_diagnostic(code, message, location),
            sources: Vec::new(),
        }
    }
}

/// A design read, preprocessed and parsed, with its top chosen — everything a
/// run needs before elaboration, and all a [`design_graph`](crate::graph) needs.
#[derive(Debug)]
pub struct Design {
    /// The modules that make up the design. With a named top, only what it
    /// reaches.
    pub modules: Vec<VerilogModule>,
    pub top: String,
    /// Every source as it was read.
    pub sources: Vec<SourceRecord>,
}

/// Reads, preprocesses and parses the design `config` describes, picks its
/// top and configures a [`Simulator`] for it — everything short of
/// [`Simulator::setup`], which is the caller's so that it can tell an
/// elaboration failure from a compile one.
///
/// This is the one way a [`RunConfig`] becomes a simulation: [`run`] goes
/// through it, and so does an interactive
/// [`Session`](crate::inspect::Session).
pub fn load(config: &RunConfig) -> Result<Loaded, LoadError> {
    let design = load_design(config)?;
    let mut simulator = Simulator::with_modules(design.modules, design.top);
    for directory in &config.search_paths {
        simulator.add_search_path(directory.clone());
    }
    for plusarg in &config.plusargs {
        simulator.add_plusarg(plusarg);
    }
    if let Some(dir) = &config.output_dir {
        simulator.set_output_directory(dir.clone());
    }
    Ok(Loaded {
        simulator,
        sources: design.sources,
    })
}

/// Reads, preprocesses and parses the sources `config` names and picks the
/// top: the half of [`load`] a [`design_graph`](crate::graph::design_graph)
/// needs, since it elaborates without ever building a [`Simulator`].
pub fn load_design(config: &RunConfig) -> Result<Design, LoadError> {
    let mut sources = Vec::with_capacity(config.sources.len());
    let mut texts = Vec::with_capacity(config.sources.len());
    for path in &config.sources {
        match std::fs::read_to_string(path) {
            Ok(text) => {
                sources.push(SourceRecord {
                    path: path.clone(),
                    bytes: text.len(),
                    sha256: sha256_hex(text.as_bytes()),
                });
                texts.push(text);
            }
            Err(error) => {
                return Err(LoadError {
                    stop: StopReason::Io,
                    diagnostic: error_diagnostic(
                        "io",
                        format!("cannot read {}: {}", path.display(), error),
                        None,
                    ),
                    sources,
                });
            }
        }
    }
    match parse(config, &texts) {
        Ok((modules, top)) => Ok(Design {
            modules,
            top,
            sources,
        }),
        Err(error) => Err(LoadError { sources, ..error }),
    }
}

pub(crate) fn error_diagnostic(
    code: &str,
    message: String,
    location: Option<String>,
) -> Diagnostic {
    Diagnostic {
        severity: Severity::Error,
        code: code.to_string(),
        message,
        location,
    }
}

/// Preprocesses and parses the sources and picks the top.
fn parse(config: &RunConfig, texts: &[String]) -> Result<(Vec<VerilogModule>, String), LoadError> {
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
            LoadError::compile(
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
        .map_err(|error| LoadError::compile("preprocess", error.to_string(), None))?;
    let parsed = parse_expanded(expanded).map_err(|error| match error {
        SourceError::Parse { at, detail } => LoadError::compile("parse", detail, Some(at)),
        other => LoadError::compile("preprocess", other.to_string(), None),
    })?;

    // A named top is iverilog's `-s`: only what it reaches is the design.
    // Without one every module counts, which is what iverilog does when it
    // elaborates every root.
    let (top, modules) = match &config.top {
        Some(top) => (top.clone(), reachable_modules(parsed.modules, top)),
        None => {
            let top = root_module(&parsed.modules).ok_or_else(|| {
                LoadError::compile(
                    "no_modules",
                    "the sources declare no module".to_string(),
                    None,
                )
            })?;
            (top, parsed.modules)
        }
    };
    Ok((modules, top))
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
pub(crate) fn error_stop(error: &SimulationError, otherwise: StopReason) -> StopReason {
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
            | SimulationError::TimeOverflow
            | SimulationError::Eval(EvalError::UnsupportedFunctionCall(_))
    )
}

pub(crate) fn stop_code(stop: StopReason) -> &'static str {
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
        | StopReason::Cancelled
        | StopReason::Breakpoint => "stop",
    }
}

fn sha256_hex(bytes: &[u8]) -> String {
    Sha256::digest(bytes)
        .iter()
        .map(|byte| format!("{:02x}", byte))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Writes `files` into a fresh scratch directory and returns a config
    /// naming them in order, with the directory as the output directory.
    fn config(test: &str, files: &[(&str, &str)]) -> RunConfig {
        let dir = std::env::temp_dir().join(format!("visilog-run-{}-{}", test, std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        RunConfig {
            sources: files
                .iter()
                .map(|(name, text)| {
                    let path = dir.join(name);
                    std::fs::write(&path, text).unwrap();
                    path
                })
                .collect(),
            output_dir: Some(dir),
            ..RunConfig::default()
        }
    }

    const SPECIFIED: &str = "
        module buffer_cell(input a, output z);
            assign z = a;
            specify
                (a => z) = (1, 2);
                $setup(a, posedge z, 1);
            endspecify
        endmodule
        module tb;
            reg a = 0;
            wire z;
            buffer_cell c1(a, z);
            buffer_cell c2(a, );
            initial begin #1 a = 1; #1 $display(\"z=%b\", z); $finish; end
        endmodule
    ";

    #[test]
    fn test_a_clean_run_finishes_with_status_zero_and_hashes_its_sources() {
        let config = config(
            "clean",
            &[
                ("defs.v", "`define MSG \"hello\"\n`timescale 1ns/1ps\n"),
                ("tb.v", "module tb; initial #5 $display(`MSG); endmodule\n"),
            ],
        );
        let record = run(&config, None);
        assert_eq!(record.stop, StopReason::Quiescent);
        assert_eq!(record.exit_status(), 0);
        assert_eq!(record.output, "hello\n");
        assert_eq!(record.top.as_deref(), Some("tb"));
        assert_eq!(record.end_ticks, 5000, "5ns counted in 1ps ticks");
        assert_eq!(record.tick_femtoseconds, 1000);
        assert_eq!(record.sources.len(), 2);
        assert_eq!(record.sources[0].sha256.len(), 64);
        let capabilities = record.capabilities.expect("the design elaborated");
        assert_eq!(capabilities.mode, "functional");
        assert!(capabilities.not_simulated.is_empty());
    }

    #[test]
    fn test_each_way_a_run_ends_has_its_own_stop_and_status() {
        let failing = run(
            &config(
                "fail",
                &[(
                    "t.v",
                    "module t; initial begin assert (0); $finish; end endmodule",
                )],
            ),
            None,
        );
        assert_eq!(
            (failing.stop, failing.exit_status()),
            (StopReason::Finished, 1)
        );
        assert_eq!(failing.assertion_failures, 1);

        let broken = run(
            &config("parse", &[("t.v", "module t; initial begin endmodule")]),
            None,
        );
        assert_eq!(
            (broken.stop, broken.exit_status()),
            (StopReason::Compile, 2)
        );
        assert!(broken.diagnostics[0].location.is_some());

        let mut forever = config(
            "limit",
            &[("t.v", "module t; reg c = 0; always #1 c = ~c; endmodule")],
        );
        forever.time_limit = Some(10);
        let limited = run(&forever, None);
        assert_eq!(
            (limited.stop, limited.exit_status()),
            (StopReason::TimeLimit, 5)
        );
        assert_eq!(limited.end_ticks, 10);

        forever.time_limit = None;
        forever.step_limit = Some(3);
        let stepped = run(&forever, None);
        assert_eq!((stepped.stop, stepped.steps), (StopReason::StepLimit, 3));

        forever.step_limit = None;
        let cancel = AtomicBool::new(true);
        let cancelled = run(&forever, Some(&cancel));
        assert_eq!(
            (cancelled.stop, cancelled.exit_status()),
            (StopReason::Cancelled, 6)
        );

        let missing = RunConfig {
            sources: vec![PathBuf::from("/nonexistent/visilog.v")],
            ..RunConfig::default()
        };
        assert_eq!(run(&missing, None).exit_status(), 2);
    }

    #[test]
    fn test_specify_timing_is_disclosed_and_the_run_still_goes_ahead() {
        let record = run(&config("specify", &[("t.v", SPECIFIED)]), None);
        assert_eq!(record.stop, StopReason::Finished);
        assert_eq!(record.output, "z=1\n", "functionally the path is a wire");
        let capabilities = record.capabilities.unwrap();
        let kinds: Vec<(&str, usize)> = capabilities
            .not_simulated
            .iter()
            .map(|omitted| (omitted.kind.as_str(), omitted.count))
            .collect();
        assert_eq!(kinds, vec![("specify_path_delay", 2), ("timing_check", 2)]);
        assert_eq!(capabilities.not_simulated[0].modules, vec!["buffer_cell"]);
        let warnings: Vec<&Diagnostic> = record
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .collect();
        assert_eq!(warnings.len(), 2);
        assert!(
            warnings[0].message.contains("tb.c1 (a => z)"),
            "{}",
            warnings[0].message
        );
        assert!(warnings[1].message.contains("$setup"));
    }

    #[test]
    fn test_strict_timing_refuses_what_would_not_be_simulated() {
        let mut strict = config("strict", &[("t.v", SPECIFIED)]);
        strict.strict_timing = true;
        let record = run(&strict, None);
        assert_eq!(
            (record.stop, record.exit_status()),
            (StopReason::Unsupported, 3)
        );
        assert!(record.output.is_empty(), "nothing ran");
        assert!(record
            .diagnostics
            .iter()
            .all(|d| d.severity == Severity::Error));
        assert_eq!(record.capabilities.unwrap().mode, "strict");

        // Procedural, continuous and gate delays are simulated, so strict mode
        // has nothing to refuse in a design that uses only those.
        let mut delays = config(
            "strict-ok",
            &[(
                "t.v",
                "module t; reg a = 0; wire b, c; assign #2 b = a; buf #(1, 2) g(c, a);
                 initial begin #1 a = 1; #5 $display(\"%b%b\", b, c); end endmodule",
            )],
        );
        delays.strict_timing = true;
        let record = run(&delays, None);
        assert_eq!(record.stop, StopReason::Quiescent);
        assert_eq!(record.output, "11\n");
    }

    #[test]
    fn test_a_switch_delay_is_disclosed() {
        let record = run(
            &config(
                "switch",
                &[(
                    "t.v",
                    "module t; wire a, b; reg g = 1; tranif1 #(5) s(a, b, g); endmodule",
                )],
            ),
            None,
        );
        let capabilities = record.capabilities.unwrap();
        assert_eq!(capabilities.not_simulated[0].kind, "switch_delay");
        assert!(record.diagnostics[0].message.contains("tranif1 #(…)"));
    }

    #[test]
    fn test_a_record_round_trips_through_json() {
        let record = run(&config("json", &[("t.v", SPECIFIED)]), None);
        let json = serde_json::to_string(&record).unwrap();
        let back: RunRecord = serde_json::from_str(&json).unwrap();
        assert_eq!(back, record);
        assert!(json.contains("\"stop\":\"finished\""));
    }
}

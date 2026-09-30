//! `visilog` — the command line over [`visilog::run`].
//!
//! It is deliberately thin: it turns arguments into a [`RunConfig`], runs it,
//! prints what the design printed, and writes the [`RunRecord`] as JSON. Every
//! decision about the run itself lives in the library.

use std::path::PathBuf;
use std::process::ExitCode;

use visilog::run::{run, RunConfig, RunRecord, Severity};
use visilog::waveform::{compare, Waveform};

const USAGE: &str = "\
usage: visilog run [options] <source.v>...
       visilog compare <reference.vcd> <candidate.vcd>
       visilog --version

Runs a design and exits with a status saying how it ended:
  0 completed   1 assertion failure   2 read or compile error
  3 unsupported construct   4 elaboration or runtime error
  5 time or step limit   6 cancelled   64 usage error

options:
  -s, --top <module>        the module to elaborate (default: the root)
  -I <dir>                  add a directory to search for `include files
  -D <name>[=<value>]       define a macro before the first file
  +<name>[=<value>]         a plus-arg for $test$plusargs / $value$plusargs
  --search <dir>            where $readmemh and $fopen(…, \"r\") look
  -o, --out <dir>           where $dumpfile, $fopen and the run record land
  --timescale <u>/<p>       the default `timescale, as +timescale+ would
  --time-limit <units>      stop after this much time, in the top's units
  --step-limit <n>          stop after this many timesteps
  --record <file>           where to write the JSON run record
                            (default: <out>/visilog-run.json when --out is given)
  --strict-timing           refuse a design whose `specify` timing or switch
                            delays would not be simulated (exit 3), rather
                            than running it functionally with a warning
  -q, --quiet               do not print the design's output
";

/// What the command line asked for, apart from the run itself.
struct Invocation {
    config: RunConfig,
    record: Option<PathBuf>,
    quiet: bool,
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args.first().map(String::as_str) {
        Some("--version") | Some("-V") => {
            println!("visilog {}", env!("CARGO_PKG_VERSION"));
            ExitCode::SUCCESS
        }
        Some("run") => match parse_run(&args[1..]) {
            Ok(invocation) => execute(invocation),
            Err(problem) => usage_error(&problem),
        },
        Some("compare") => match &args[1..] {
            [reference, candidate] => compare_dumps(reference, candidate),
            _ => usage_error("compare takes a reference dump and a candidate dump"),
        },
        Some("--help") | Some("-h") => {
            print!("{}", USAGE);
            ExitCode::SUCCESS
        }
        _ => usage_error("expected a command"),
    }
}

fn usage_error(problem: &str) -> ExitCode {
    eprintln!("visilog: {}\n\n{}", problem, USAGE);
    ExitCode::from(64)
}

fn parse_run(args: &[String]) -> Result<Invocation, String> {
    let mut config = RunConfig::default();
    let mut record = None;
    let mut quiet = false;
    let mut args = args.iter();
    while let Some(arg) = args.next() {
        let mut value = |flag: &str| {
            args.next()
                .cloned()
                .ok_or_else(|| format!("{} needs a value", flag))
        };
        match arg.as_str() {
            "-s" | "--top" => config.top = Some(value(arg)?),
            "-I" => config.include_dirs.push(value(arg)?.into()),
            "-D" => config.defines.push(define(&value(arg)?)),
            "--search" => config.search_paths.push(value(arg)?.into()),
            "-o" | "--out" => config.output_dir = Some(value(arg)?.into()),
            "--timescale" => config.default_timescale = Some(value(arg)?),
            "--time-limit" => config.time_limit = Some(number(arg, &value(arg)?)?),
            "--step-limit" => config.step_limit = Some(number(arg, &value(arg)?)?),
            "--record" => record = Some(value(arg)?.into()),
            "-q" | "--quiet" => quiet = true,
            "--strict-timing" => config.strict_timing = true,
            // iverilog's spellings with the value attached.
            _ if arg.starts_with("-I") && arg.len() > 2 => {
                config.include_dirs.push(arg[2..].into())
            }
            _ if arg.starts_with("-D") && arg.len() > 2 => config.defines.push(define(&arg[2..])),
            _ if arg.starts_with('+') => config.plusargs.push(arg.clone()),
            _ if arg.starts_with('-') => return Err(format!("unknown option `{}`", arg)),
            _ => config.sources.push(arg.into()),
        }
    }
    if config.sources.is_empty() {
        return Err("no source files".to_string());
    }
    if record.is_none() {
        record = config
            .output_dir
            .as_ref()
            .map(|dir| dir.join("visilog-run.json"));
    }
    Ok(Invocation {
        config,
        record,
        quiet,
    })
}

/// `NAME=value`, or a bare `NAME`, which iverilog defines as `1`.
fn define(text: &str) -> (String, String) {
    match text.split_once('=') {
        Some((name, value)) => (name.to_string(), value.to_string()),
        None => (text.to_string(), "1".to_string()),
    }
}

fn number<T: std::str::FromStr>(flag: &str, text: &str) -> Result<T, String> {
    text.parse()
        .map_err(|_| format!("{} expects a number, not `{}`", flag, text))
}

fn execute(invocation: Invocation) -> ExitCode {
    if let Some(dir) = &invocation.config.output_dir {
        if let Err(error) = std::fs::create_dir_all(dir) {
            eprintln!("visilog: cannot create {}: {}", dir.display(), error);
            return ExitCode::from(2);
        }
    }
    let record = run(&invocation.config, None);
    if !invocation.quiet {
        print!("{}", record.output);
    }
    report(&record);
    if let Some(path) = &invocation.record {
        let json = serde_json::to_string_pretty(&record).expect("a run record serialises");
        if let Err(error) = std::fs::write(path, json + "\n") {
            eprintln!("visilog: cannot write {}: {}", path.display(), error);
        }
    }
    ExitCode::from(record.exit_status() as u8)
}

/// Compares two value change dumps and prints the result as JSON. Exits 0 when
/// they agree, 1 when they do not and 2 when one could not be read.
fn compare_dumps(reference: &str, candidate: &str) -> ExitCode {
    let read = |path: &str| {
        std::fs::read_to_string(path)
            .map_err(|error| format!("cannot read {}: {}", path, error))
            .and_then(|text| Waveform::parse(&text).map_err(|error| format!("{}: {}", path, error)))
    };
    match (read(reference), read(candidate)) {
        (Ok(reference), Ok(candidate)) => {
            let comparison = compare(&reference, &candidate);
            println!(
                "{}",
                serde_json::to_string_pretty(&comparison).expect("a comparison serialises")
            );
            ExitCode::from(if comparison.agrees() { 0 } else { 1 })
        }
        (Err(problem), _) | (_, Err(problem)) => {
            eprintln!("visilog: {}", problem);
            ExitCode::from(2)
        }
    }
}

/// The diagnostics and a one-line summary, on standard error so they never mix
/// with what the design printed.
fn report(record: &RunRecord) {
    for diagnostic in &record.diagnostics {
        let severity = match diagnostic.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
        };
        match &diagnostic.location {
            Some(at) => eprintln!(
                "visilog: {}: {}: [{}] {}",
                at, severity, diagnostic.code, diagnostic.message
            ),
            None => eprintln!(
                "visilog: {}: [{}] {}",
                severity, diagnostic.code, diagnostic.message
            ),
        }
    }
    eprintln!(
        "visilog: stop={} time={} ticks ({} fs/tick) steps={} assertion_failures={}",
        serde_json::to_string(&record.stop)
            .unwrap_or_default()
            .trim_matches('"'),
        record.end_ticks,
        record.tick_femtoseconds,
        record.steps,
        record.assertion_failures
    );
}

//! Real-project differential qualification (#378).
//!
//! The ivtest corpus scores visilog against a regression suite; this scores it
//! against real, maintained designs. Every bench in
//! `qualification/benches.json` is run twice with the same sources, defines
//! and top — once under iverilog, once under visilog — and the two runs are
//! compared three ways:
//!
//! - **waveform**: both dumps normalised by [`Waveform::parse`] and compared
//!   signal by signal at every instant of physical time, on the signals both
//!   dumped;
//! - **output**: what each printed, with iverilog's `file:line:` prefixes and
//!   its own `$finish called at` lines taken out, since visilog has no source
//!   lines to print;
//! - **assertions**: how many `ERROR:`/`FATAL:` reports each made.
//!
//! Whether the *reference* passes the testbench's own checks is recorded but is
//! a separate question: several of these testbenches fail under iverilog as
//! well, and what qualifies visilog is that it agrees with the reference, not
//! that the design is right. A bench iverilog cannot compile has nothing to
//! compare and is reported as such rather than counted against visilog.
//!
//! The projects are private and never vendored. Point `VISILOG_PROJECTS` at a
//! directory holding `MagicSchoolBus`, `widlar` and `fpga-tesla` checked out at
//! the revisions the manifest pins, and `VISILOG_ICE40_CELLS` at yosys's
//! `ice40/cells_sim.v` (default `/usr/share/yosys/ice40/cells_sim.v`):
//!
//! ```bash
//! VISILOG_PROJECTS=~/repos cargo test --release --test project_qualification \
//!     -- --ignored --nocapture
//! ```
//!
//! `VISILOG_QUAL_ONLY=widlar/spi` runs one bench. Artifacts — both logs, both
//! dumps, the run record, the comparison and a summary naming the commands,
//! tool versions and source hashes — land in `target/qualification/<bench>/`.
//! The line `QUALIFICATION_METRICS …` is the machine-readable result, and the
//! test fails only when a bench the manifest expects to `agree` does not.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::Command;

use serde::{Deserialize, Serialize};
use visilog::run::{run, RunConfig, RunRecord, StopReason};
use visilog::waveform::{compare, Comparison, Waveform};

#[derive(Debug, Deserialize)]
struct Manifest {
    reference: ReferenceCommands,
    projects: BTreeMap<String, Project>,
    benches: Vec<Bench>,
    common: Common,
}

#[derive(Debug, Deserialize)]
struct ReferenceCommands {
    compile: Vec<String>,
    run: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct Project {
    repo: String,
    rev: String,
}

#[derive(Debug, Deserialize)]
struct Common {
    defines: Vec<(String, String)>,
}

#[derive(Debug, Deserialize)]
struct Bench {
    name: String,
    project: String,
    top: String,
    expect: Expect,
    sources: Vec<String>,
    #[serde(default)]
    note: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
enum Expect {
    Agree,
    Diverge,
    Unsupported,
    ReferenceCompileFailure,
}

/// What comparing one bench found.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case", tag = "verdict", content = "detail")]
enum Verdict {
    Agree,
    Diverge(String),
    Unsupported(String),
    VisilogError(String),
    ReferenceCompileFailure(String),
}

impl Verdict {
    fn matches(&self, expect: Expect) -> bool {
        matches!(
            (self, expect),
            (Verdict::Agree, Expect::Agree)
                | (Verdict::Diverge(_), Expect::Diverge)
                | (Verdict::Unsupported(_), Expect::Unsupported)
                | (
                    Verdict::ReferenceCompileFailure(_),
                    Expect::ReferenceCompileFailure
                )
        )
    }

    fn label(&self) -> &'static str {
        match self {
            Verdict::Agree => "agree",
            Verdict::Diverge(_) => "diverge",
            Verdict::Unsupported(_) => "unsupported",
            Verdict::VisilogError(_) => "visilog_error",
            Verdict::ReferenceCompileFailure(_) => "reference_compile_failure",
        }
    }
}

/// Everything recorded about one bench, written as `summary.json`.
#[derive(Debug, Serialize)]
struct Summary<'a> {
    bench: &'a str,
    project: &'a str,
    repo: &'a str,
    pinned_rev: &'a str,
    checked_out_rev: Option<String>,
    top: &'a str,
    expect: Expect,
    verdict: &'a Verdict,
    note: Option<&'a str>,
    reference_version: String,
    reference_compile: Vec<String>,
    reference_run: Vec<String>,
    reference_passes_own_checks: Option<bool>,
    reference_seconds: f64,
    visilog_seconds: f64,
    visilog: Option<&'a RunRecord>,
    comparison: Option<&'a Comparison>,
    first_output_difference: Option<String>,
}

fn manifest() -> Manifest {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("qualification/benches.json");
    let text = std::fs::read_to_string(&path).expect("qualification/benches.json is readable");
    serde_json::from_str(&text).expect("qualification/benches.json parses")
}

fn projects_root() -> Option<PathBuf> {
    let root = std::env::var_os("VISILOG_PROJECTS")
        .map(PathBuf::from)
        .or_else(|| {
            std::env::var_os("HOME").map(|home| Path::new(&home).join(".cache/visilog/projects"))
        })?;
    root.is_dir().then_some(root)
}

fn cell_library() -> Option<PathBuf> {
    let path = std::env::var_os("VISILOG_ICE40_CELLS")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("/usr/share/yosys/ice40/cells_sim.v"));
    path.is_file().then_some(path)
}

/// The commit a project directory is at: its git HEAD, or the `.pinned-sha`
/// file a tarball download leaves.
fn checked_out_rev(dir: &Path) -> Option<String> {
    let git = Command::new("git")
        .args(["-C", &dir.to_string_lossy(), "rev-parse", "HEAD"])
        .output()
        .ok()
        .filter(|out| out.status.success())
        .map(|out| String::from_utf8_lossy(&out.stdout).trim().to_string());
    git.or_else(|| {
        std::fs::read_to_string(dir.join(".pinned-sha"))
            .ok()
            .map(|sha| sha.trim().to_string())
    })
}

fn tool_version(tool: &str) -> String {
    Command::new(tool)
        .arg("-V")
        .output()
        .ok()
        .and_then(|out| {
            String::from_utf8_lossy(&out.stdout)
                .lines()
                .next()
                .map(str::to_string)
        })
        .unwrap_or_else(|| format!("{} not found", tool))
}

/// What a design printed, in a form both simulators can be compared on:
/// iverilog's `file:line:` after a severity label and its own `$finish called
/// at` lines are removed, since visilog has no source lines to print.
fn normalised_output(text: &str) -> Vec<String> {
    text.lines()
        .filter(|line| !line.contains(": $finish called at") && !line.contains(": $stop called at"))
        .map(|line| {
            for label in ["ERROR: ", "WARNING: ", "INFO: ", "FATAL: "] {
                if let Some(rest) = line.strip_prefix(label) {
                    if let Some((location, message)) = rest.split_once(": ") {
                        if location.contains(':')
                            && location
                                .rsplit(':')
                                .next()
                                .is_some_and(|l| l.parse::<u32>().is_ok())
                        {
                            return format!("{}{}", label, message);
                        }
                    }
                }
            }
            line.trim_end().to_string()
        })
        .collect()
}

/// How many `$error`/`$fatal` reports — failed assertions among them — a run
/// printed. A severity task's report is its label line followed by an
/// indented `Time: … Scope: …` line; a testbench that `$display`s its own
/// text starting `ERROR:` has no such line and is not one.
fn reported_failures(lines: &[String]) -> usize {
    lines
        .windows(2)
        .filter(|pair| {
            (pair[0].starts_with("ERROR: ") || pair[0].starts_with("FATAL: "))
                && pair[1].trim_start().starts_with("Time: ")
        })
        .count()
}

/// Whether a testbench's own checks passed, as these testbenches report it:
/// the projects' runners fail a bench whose output mentions an error.
fn passes_own_checks(lines: &[String]) -> bool {
    !lines
        .iter()
        .any(|line| line.to_ascii_lowercase().contains("error"))
}

fn first_difference(expected: &[String], found: &[String]) -> Option<String> {
    let longest = expected.len().max(found.len());
    (0..longest).find_map(|index| {
        let (want, got) = (expected.get(index), found.get(index));
        (want != got).then(|| {
            format!(
                "line {}: expected {:?} got {:?}",
                index + 1,
                want.map(String::as_str).unwrap_or("<end>"),
                got.map(String::as_str).unwrap_or("<end>")
            )
        })
    })
}

fn only_vcd(dir: &Path) -> Option<Waveform> {
    let path = std::fs::read_dir(dir)
        .ok()?
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .find(|path| path.extension().is_some_and(|ext| ext == "vcd"))?;
    Waveform::parse(&std::fs::read_to_string(path).ok()?).ok()
}

#[test]
#[ignore]
fn project_qualification() {
    let manifest = manifest();
    let Some(root) = projects_root() else {
        eprintln!(
            "skipping: set VISILOG_PROJECTS to a directory holding the qualification projects"
        );
        return;
    };
    let Some(cells) = cell_library() else {
        eprintln!("skipping: set VISILOG_ICE40_CELLS to yosys's ice40/cells_sim.v");
        return;
    };
    let only = std::env::var("VISILOG_QUAL_ONLY").ok();
    let artifacts = Path::new(env!("CARGO_MANIFEST_DIR")).join("target/qualification");
    let reference_version = tool_version("iverilog");

    let mut results: Vec<(String, Expect, Verdict)> = Vec::new();
    for bench in &manifest.benches {
        if only.as_deref().is_some_and(|only| only != bench.name) {
            continue;
        }
        let project = &manifest.projects[&bench.project];
        let dir = root.join(&bench.project);
        if !dir.is_dir() {
            eprintln!(
                "{}: project {} not found under {}",
                bench.name,
                bench.project,
                root.display()
            );
            continue;
        }
        let checked_out = checked_out_rev(&dir);
        if checked_out.as_deref() != Some(project.rev.as_str()) {
            eprintln!(
                "{}: {} is at {:?}, the manifest pins {}",
                bench.name, bench.project, checked_out, project.rev
            );
        }
        let mut sources: Vec<PathBuf> = bench
            .sources
            .iter()
            .map(|source| dir.join(source))
            .collect();
        sources.push(cells.clone());

        let out = artifacts.join(&bench.name);
        let (reference_dir, visilog_dir) = (out.join("reference"), out.join("visilog"));
        let _ = std::fs::remove_dir_all(&out);
        std::fs::create_dir_all(&reference_dir).unwrap();
        std::fs::create_dir_all(&visilog_dir).unwrap();

        // The reference: iverilog's compile, then vvp, run where its dump lands.
        let mut compile: Vec<String> = manifest.reference.compile.clone();
        for (name, value) in &manifest.common.defines {
            compile.push(format!("-D{}={}", name, value));
        }
        compile.extend([
            "-s".to_string(),
            bench.top.clone(),
            "-o".to_string(),
            "sim".to_string(),
        ]);
        compile.extend(sources.iter().map(|path| path.display().to_string()));
        let started = std::time::Instant::now();
        let compiled = Command::new(&compile[0])
            .args(&compile[1..])
            .current_dir(&reference_dir)
            .output()
            .expect("iverilog runs");
        std::fs::write(
            reference_dir.join("compile.log"),
            [&compiled.stdout[..], &compiled.stderr[..]].concat(),
        )
        .unwrap();
        let mut run_command = manifest.reference.run.clone();
        run_command.push("sim".to_string());

        let mut reference_lines = Vec::new();
        let mut comparison = None;
        let mut record = None;
        let mut first_output_difference = None;
        let mut visilog_seconds = 0.0;
        let verdict = if !compiled.status.success() {
            Verdict::ReferenceCompileFailure(
                String::from_utf8_lossy(&compiled.stderr)
                    .lines()
                    .next()
                    .unwrap_or("")
                    .to_string(),
            )
        } else {
            let ran = Command::new(&run_command[0])
                .args(&run_command[1..])
                .current_dir(&reference_dir)
                .output()
                .expect("vvp runs");
            std::fs::write(reference_dir.join("run.log"), &ran.stdout).unwrap();
            reference_lines = normalised_output(&String::from_utf8_lossy(&ran.stdout));

            let config = RunConfig {
                sources: sources.clone(),
                top: Some(bench.top.clone()),
                defines: manifest.common.defines.clone(),
                output_dir: Some(visilog_dir.clone()),
                ..RunConfig::default()
            };
            let started = std::time::Instant::now();
            let visilog = run(&config, None);
            visilog_seconds = started.elapsed().as_secs_f64();
            std::fs::write(visilog_dir.join("run.log"), &visilog.output).unwrap();
            std::fs::write(
                visilog_dir.join("visilog-run.json"),
                serde_json::to_string_pretty(&visilog).unwrap(),
            )
            .unwrap();
            let found_lines = normalised_output(&visilog.output);
            first_output_difference = first_difference(&reference_lines, &found_lines);

            let verdict = match visilog.stop {
                StopReason::Unsupported => {
                    Verdict::Unsupported(visilog.diagnostics[0].message.clone())
                }
                StopReason::Finished | StopReason::Quiescent => {
                    let dumps = (only_vcd(&reference_dir), only_vcd(&visilog_dir));
                    let waveforms = match &dumps {
                        (Some(reference), Some(candidate)) => Some(compare(reference, candidate)),
                        _ => None,
                    };
                    let verdict =
                        if let Some(first) = waveforms.as_ref().and_then(Comparison::first) {
                            Verdict::Diverge(format!(
                                "{} at {} fs: expected {:?} found {:?}",
                                first.signal, first.time, first.expected, first.found
                            ))
                        } else if waveforms
                            .as_ref()
                            .is_some_and(|c| !c.width_mismatches.is_empty())
                        {
                            Verdict::Diverge(format!(
                                "width of {:?}",
                                waveforms.as_ref().unwrap().width_mismatches
                            ))
                        } else if dumps.0.is_some() != dumps.1.is_some() {
                            Verdict::Diverge("only one side wrote a dump".into())
                        } else if let Some(difference) = &first_output_difference {
                            Verdict::Diverge(format!("output {}", difference))
                        } else if reported_failures(&reference_lines) as u64
                            != visilog.assertion_failures
                        {
                            Verdict::Diverge(format!(
                                "{} assertion failures, iverilog reported {}",
                                visilog.assertion_failures,
                                reported_failures(&reference_lines)
                            ))
                        } else {
                            Verdict::Agree
                        };
                    if let Some(waveforms) = &waveforms {
                        std::fs::write(
                            out.join("comparison.json"),
                            serde_json::to_string_pretty(waveforms).unwrap(),
                        )
                        .unwrap();
                    }
                    comparison = waveforms;
                    verdict
                }
                other => Verdict::VisilogError(format!(
                    "{:?}: {}",
                    other,
                    visilog
                        .diagnostics
                        .iter()
                        .map(|d| d.message.as_str())
                        .collect::<Vec<_>>()
                        .join("; ")
                )),
            };
            record = Some(visilog);
            verdict
        };
        let reference_seconds = started.elapsed().as_secs_f64() - visilog_seconds;

        let summary = Summary {
            bench: &bench.name,
            project: &bench.project,
            repo: &project.repo,
            pinned_rev: &project.rev,
            checked_out_rev: checked_out,
            top: &bench.top,
            expect: bench.expect,
            verdict: &verdict,
            note: bench.note.as_deref(),
            reference_version: reference_version.clone(),
            reference_compile: compile,
            reference_run: run_command,
            reference_passes_own_checks: compiled
                .status
                .success()
                .then(|| passes_own_checks(&reference_lines)),
            reference_seconds,
            visilog_seconds,
            visilog: record.as_ref(),
            comparison: comparison.as_ref(),
            first_output_difference,
        };
        std::fs::write(
            out.join("summary.json"),
            serde_json::to_string_pretty(&summary).unwrap(),
        )
        .unwrap();
        println!(
            "{:<28} expect={:<26} got={:<26} ref {:>6.1}s visilog {:>6.1}s",
            bench.name,
            format!("{:?}", bench.expect),
            verdict.label(),
            reference_seconds,
            visilog_seconds
        );
        if let Verdict::Diverge(detail)
        | Verdict::Unsupported(detail)
        | Verdict::VisilogError(detail) = &verdict
        {
            println!("    {}", detail);
        }
        results.push((bench.name.clone(), bench.expect, verdict));
    }

    let count = |label: &str| {
        results
            .iter()
            .filter(|(_, _, v)| v.label() == label)
            .count()
    };
    let gated = results
        .iter()
        .filter(|(_, expect, _)| *expect == Expect::Agree)
        .count();
    let failures: Vec<&str> = results
        .iter()
        .filter(|(_, expect, verdict)| *expect == Expect::Agree && *verdict != Verdict::Agree)
        .map(|(name, _, _)| name.as_str())
        .collect();
    let surprises: Vec<&str> = results
        .iter()
        .filter(|(_, expect, verdict)| !verdict.matches(*expect))
        .map(|(name, _, _)| name.as_str())
        .collect();
    println!(
        "QUALIFICATION_METRICS total={} agree={} diverge={} unsupported={} visilog_error={} reference_compile_failure={} gated={} gate_failures={}",
        results.len(),
        count("agree"),
        count("diverge"),
        count("unsupported"),
        count("visilog_error"),
        count("reference_compile_failure"),
        gated,
        failures.len()
    );
    if !surprises.is_empty() {
        println!("not what the manifest expects: {}", surprises.join(", "));
    }
    assert!(
        failures.is_empty(),
        "benches expected to agree with iverilog did not: {}",
        failures.join(", ")
    );
}

#[test]
fn the_manifest_is_well_formed() {
    let manifest = manifest();
    let mut names = std::collections::BTreeSet::new();
    for bench in &manifest.benches {
        assert!(names.insert(&bench.name), "{} is listed twice", bench.name);
        assert!(
            manifest.projects.contains_key(&bench.project),
            "{} names an unknown project",
            bench.name
        );
        assert!(!bench.sources.is_empty());
        if bench.expect != Expect::Agree {
            assert!(
                bench.note.is_some(),
                "{} is not expected to agree and says nothing about why",
                bench.name
            );
        }
    }
    for project in manifest.projects.values() {
        assert_eq!(
            project.rev.len(),
            40,
            "{} is pinned by full commit hash",
            project.repo
        );
    }
}

#[test]
fn output_normalisation_drops_what_only_iverilog_can_print() {
    let iverilog = "VCD info: dumpfile output.vcd opened for output.\n\
                    ERROR: /a/b/tb.v:106: data is incorrect\n       Time: 87  Scope: tb\n\
                    /a/b/tb.v:161: $finish called at 263 (1s)\n";
    let visilog = "VCD info: dumpfile output.vcd opened for output.\n\
                   ERROR: data is incorrect\n       Time: 87  Scope: tb\n";
    let (expected, found) = (normalised_output(iverilog), normalised_output(visilog));
    assert_eq!(expected, found);
    assert_eq!(reported_failures(&expected), 1);
    assert!(!passes_own_checks(&expected));
    assert_eq!(
        normalised_output("ERROR: expected 4 bytes, got 8"),
        vec!["ERROR: expected 4 bytes, got 8"],
        "a message with a colon in it is not a location"
    );
    assert_eq!(
        reported_failures(&normalised_output("ERROR: expected 4 bytes, got 8\ndone")),
        0,
        "a $display that starts with ERROR is not an $error"
    );
}

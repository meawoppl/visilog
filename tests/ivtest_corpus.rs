//! Measures visilog's front end against the Icarus Verilog regression corpus.
//!
//! `ivtest` is iverilog's own regression suite. Its `regress-vlg.list` is the
//! subset its authors describe as "tests that should work using any simulator
//! that supports standard Verilog (1364-2005)" — so it is a fair, external,
//! simulator-agnostic yardstick rather than a set of expectations we wrote
//! ourselves.
//!
//! The corpus is **GPL-2.0** and visilog is MIT, so it is deliberately *not*
//! vendored into this repository. It is cloned locally and read from a cache:
//!
//! ```text
//! git clone --depth 1 --filter=blob:none --sparse \
//!     https://github.com/steveicarus/iverilog ~/.cache/visilog/ivtest
//! cd ~/.cache/visilog/ivtest && git sparse-checkout set ivtest
//! ```
//!
//! Then: `cargo test --test ivtest_corpus -- --ignored --nocapture`
//!
//! Both tests are `#[ignore]`d: they need that clone, so they must never make
//! CI depend on the network.

use std::collections::BTreeMap;
use std::path::PathBuf;

use visilog::parsers::modules::VerilogModule;
use visilog::parsers::source::parse_verilog_source;
use visilog::parsers::statements::ModuleStatement;
use visilog::simulator::runner::Simulator;

/// Where the corpus lives. `VISILOG_IVTEST` overrides the default cache path.
fn corpus_root() -> Option<PathBuf> {
    let root = match std::env::var_os("VISILOG_IVTEST") {
        Some(path) => PathBuf::from(path),
        None => PathBuf::from(std::env::var_os("HOME")?)
            .join(".cache")
            .join("visilog")
            .join("ivtest"),
    };
    root.join("ivtest")
        .join("ivltests")
        .is_dir()
        .then_some(root)
}

/// One entry of a `regress-*.list`: a test name and what is expected of it.
struct Entry {
    name: String,
    kind: String,
}

/// Parses a regression list. Lines are `name<tab>kind<tab>directory [# comment]`.
fn entries(list: &str) -> Vec<Entry> {
    list.lines()
        .map(|line| line.split('#').next().unwrap_or("").trim())
        .filter(|line| !line.is_empty())
        .filter_map(|line| {
            let mut fields = line.split_whitespace();
            Some(Entry {
                name: fields.next()?.to_string(),
                kind: fields.next()?.to_string(),
            })
        })
        .collect()
}

/// Whether the source names a `$task` the simulator does not implement.
///
/// Counting every `$` would keep reporting system tasks as a blocker after
/// they were implemented, which is how a survey heuristic quietly goes stale.
fn unsupported_system_names(source: &str) -> bool {
    const SUPPORTED: [&str; 4] = ["display", "write", "finish", "time"];
    source.match_indices('$').any(|(at, _)| {
        let name: String = source[at + 1..]
            .chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect();
        !name.is_empty() && !SUPPORTED.contains(&name.as_str())
    })
}

/// Which known-missing features a rejected file uses.
///
/// Counted **independently**, not first-match-wins: a typical corpus file needs
/// several of these before it will parse, so "N files contain X" is the honest
/// framing. Fixing the top entry does not unblock N files — it removes one of
/// several reasons each of them fails.
///
/// These are heuristics over source text, not parser diagnostics.
fn blockers_in(source: &str) -> Vec<&'static str> {
    // A comment before the first `module` is fine — `parse_verilog_source`
    // skips those. One after it lands inside a body, which nothing consumes.
    let body = source
        .find("module")
        .map(|at| &source[at..])
        .unwrap_or(source);

    let mut found = Vec::new();
    let mut note = |present: bool, label: &'static str| {
        if present {
            found.push(label);
        }
    };

    note(
        unsupported_system_names(source),
        "unsupported system function ($random, $signed, $monitor, ...)",
    );
    note(
        source.lines().any(|line| {
            let line = line.trim_start();
            line.starts_with("`define")
                || line.starts_with("`include")
                || line.starts_with("`ifdef")
                || line.starts_with("`ifndef")
                || line.starts_with("`timescale")
                || line.starts_with("`undef")
                || line.starts_with("`celldefine")
        }),
        "preprocessor directive (`define, `include, `timescale)",
    );
    note(
        body.contains("function") || body.contains("task"),
        "function / task",
    );
    note(
        body.contains("for (")
            || body.contains("for(")
            || body.contains("while")
            || body.contains("repeat")
            || body.contains("forever"),
        "loop statement",
    );
    note(
        body.contains("integer ") || body.contains("real "),
        "integer / real declaration",
    );
    note(body.contains("generate"), "generate block");
    note(
        body.contains("casez") || body.contains("casex"),
        "casez / casex",
    );
    note(body.contains("fork"), "fork / join");
    note(body.contains("signed"), "signed types");
    found
}

/// Loads the corpus, or explains why it is absent and returns `None`.
fn load() -> Option<(PathBuf, Vec<Entry>)> {
    let Some(root) = corpus_root() else {
        eprintln!(
            "ivtest corpus not found. Clone it first (see this file's header), \
             or set VISILOG_IVTEST. Skipping."
        );
        return None;
    };
    let list = std::fs::read_to_string(root.join("ivtest").join("regress-vlg.list"))
        .expect("regress-vlg.list should be readable");
    Some((root, entries(&list)))
}

/// How many of the standard-Verilog corpus files the front end accepts.
///
/// Reports rather than asserting a rate: the number is a measurement to track,
/// and the assertion at the end only guards against a *regression* to zero,
/// which would mean the harness itself broke.
#[test]
#[ignore]
fn ivtest_corpus_parse_rate() {
    let Some((root, entries)) = load() else {
        return;
    };
    let dir = root.join("ivtest").join("ivltests");

    let normal: Vec<&Entry> = entries
        .iter()
        .filter(|e| e.kind.starts_with("normal"))
        .collect();

    let (mut parsed, mut rejected, mut missing) = (0usize, 0usize, 0usize);
    let mut blockers: BTreeMap<&'static str, usize> = BTreeMap::new();
    let mut clean = 0usize;
    let mut unexplained: Vec<String> = Vec::new();

    for entry in &normal {
        let path = dir.join(format!("{}.v", entry.name));
        let Ok(source) = std::fs::read_to_string(&path) else {
            missing += 1;
            continue;
        };
        match parse_verilog_source(&source) {
            Ok(_) => parsed += 1,
            Err(_) => {
                rejected += 1;
                let found = blockers_in(&source);
                if found.is_empty() {
                    clean += 1;
                    if unexplained.len() < 12 {
                        unexplained.push(entry.name.clone());
                    }
                }
                for blocker in found {
                    *blockers.entry(blocker).or_default() += 1;
                }
            }
        }
    }

    let attempted = parsed + rejected;
    println!("\n=== ivtest regress-vlg.list, `normal` tests ===");
    println!("attempted : {}", attempted);
    println!(
        "parsed    : {} ({:.1}%)",
        parsed,
        100.0 * parsed as f64 / attempted.max(1) as f64
    );
    println!("rejected  : {}", rejected);
    if missing > 0 {
        println!("missing   : {} (listed but no .v file)", missing);
    }

    println!("\n--- how many rejected files use each missing feature (independent counts) ---");
    let mut ranked: Vec<_> = blockers.into_iter().collect();
    ranked.sort_by(|a, b| b.1.cmp(&a.1));
    for (blocker, count) in ranked {
        println!(
            "{:>5}  {:>5.1}%  {}",
            count,
            100.0 * count as f64 / rejected.max(1) as f64,
            blocker
        );
    }
    println!(
        "{:>5}  {:>5.1}%  none of the above (rejected for some other reason)",
        clean,
        100.0 * clean as f64 / rejected.max(1) as f64
    );

    // Naming a few keeps the survey honest as coverage grows: once the known
    // blockers stop explaining most failures, these are what to look at next.
    if !unexplained.is_empty() {
        println!("\nsample of unexplained rejections:");
        for name in &unexplained {
            println!("  {}", name);
        }
    }

    assert!(attempted > 0, "corpus present but no tests were attempted");
}

/// The `CE` entries are files that must *fail* to compile — a conformance suite
/// for rejection. Accepting one is a sign the grammar is too permissive.
///
/// Reported separately and without judgement for now: visilog rejects a great
/// deal it should accept, so a high "correctly rejected" score here is mostly
/// measuring incompleteness rather than strictness.
#[test]
#[ignore]
fn ivtest_corpus_compile_error_cases() {
    let Some((root, entries)) = load() else {
        return;
    };
    let dir = root.join("ivtest").join("ivltests");

    let (mut rejected, mut accepted) = (0usize, 0usize);
    let mut accepted_names: Vec<String> = Vec::new();
    for entry in entries.iter().filter(|e| e.kind == "CE") {
        let path = dir.join(format!("{}.v", entry.name));
        let Ok(source) = std::fs::read_to_string(&path) else {
            continue;
        };
        match parse_verilog_source(&source) {
            Ok(_) => {
                accepted += 1;
                accepted_names.push(entry.name.clone());
            }
            Err(_) => rejected += 1,
        }
    }

    println!("\n=== ivtest `CE` tests (must not compile) ===");
    println!("rejected (correct) : {}", rejected);
    println!(
        "accepted (too permissive, or accidentally right) : {}",
        accepted
    );
    // Named, because this count only grows as the grammar accepts more, and
    // "we do not model that semantic rule" versus "the grammar went loose" can
    // only be told apart by looking at which files they are.
    for name in &accepted_names {
        println!("  {}", name);
    }
}

/// A control for the corpus tests: the harness must be able to accept
/// *something*. A `0%` corpus result is only meaningful if this passes — it
/// rules out a harness bug being reported as a parser limitation.
#[test]
fn harness_accepts_known_good_source() {
    let source = r#"
        // a leading comment, which is legal between modules
        module adder(input wire [3:0] a, input wire [3:0] b, output wire [3:0] sum);
            assign sum = a + b;
        endmodule

        module top(input wire [3:0] x, output wire [3:0] y);
            adder inst(.a(x), .b(x), .sum(y));
        endmodule
    "#;
    let (rest, modules) = parse_verilog_source(source).expect("control source should parse");
    assert!(rest.trim().is_empty());
    assert_eq!(modules.len(), 2);
}

/// How far simulated time is advanced before a design is judged. Generous
/// enough for the corpus's self-checking testbenches, which typically finish
/// within a few hundred time units, and bounded so a free-running design
/// cannot run the suite forever.
const TIME_BUDGET: i64 = 10_000;

/// The module to elaborate: one that nothing else instantiates.
///
/// A corpus file is a self-contained testbench plus the modules it exercises,
/// with no marker saying which is which. The testbench is the one at the root
/// of the instantiation graph. Ties are broken by the conventional names, then
/// by source order, which matters because picking a leaf module would elaborate
/// a design with no stimulus and score it as silent.
fn top_module(modules: &[VerilogModule]) -> Option<String> {
    let instantiated: Vec<&str> = modules
        .iter()
        .flat_map(|module| &module.statements)
        .filter_map(|statement| match statement {
            ModuleStatement::ModuleInstantiation(instance) => {
                Some(instance.module_name.name.as_str())
            }
            _ => None,
        })
        .collect();

    let roots: Vec<&str> = modules
        .iter()
        .map(|module| module.identifier.name.as_str())
        .filter(|name| !instantiated.contains(name))
        .collect();

    for conventional in ["main", "top", "test", "tb", "bench"] {
        if roots.contains(&conventional) {
            return Some(conventional.to_string());
        }
    }
    roots
        .last()
        .map(|name| name.to_string())
        .or_else(|| modules.last().map(|m| m.identifier.name.clone()))
}

/// What became of one corpus file.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Outcome {
    /// The front end rejected it.
    ParseFailed,
    /// It parsed, but elaboration refused it.
    SetupFailed(String),
    /// It elaborated, but running it raised an error.
    RunFailed(String),
    /// It ran and reported success.
    Passed,
    /// It ran and reported *failure* — we simulated it and got the wrong
    /// answer. This is the only outcome that indicates a correctness bug
    /// rather than a missing feature.
    WrongAnswer,
    /// It ran and printed nothing, so it never reached its own check.
    Silent,
}

fn judge(source: &str) -> Outcome {
    let Ok((_, modules)) = parse_verilog_source(source) else {
        return Outcome::ParseFailed;
    };
    let Some(top) = top_module(&modules) else {
        return Outcome::ParseFailed;
    };

    let mut simulator = Simulator::with_modules(modules, top);
    if let Err(error) = simulator.setup() {
        return Outcome::SetupFailed(error_kind(&error));
    }
    if let Err(error) = simulator.advance(TIME_BUDGET) {
        return Outcome::RunFailed(error_kind(&error));
    }

    let output = simulator.output().text();
    // A corpus test prints FAILED for every check it fails and PASSED once at
    // the end, so any FAILED outweighs a PASSED.
    if output.contains("FAILED") {
        Outcome::WrongAnswer
    } else if output.contains("PASSED") {
        Outcome::Passed
    } else {
        Outcome::Silent
    }
}

/// The variant name alone, so outcomes group by kind rather than by the
/// specific signal or module a message happens to mention.
fn error_kind(error: &impl std::fmt::Debug) -> String {
    let text = format!("{:?}", error);
    text.split(['(', ' ', '{'])
        .next()
        .unwrap_or("Unknown")
        .to_string()
}

/// **The headline metric: how many corpus tests actually pass.**
///
/// The corpus is self-checking — a test prints `PASSED` when it is satisfied —
/// so parsing a file says nothing about whether the simulator got the right
/// answer. Measuring `parsed` alone overstates progress, and it is blind to the
/// worst outcome of all: a design that runs and produces a wrong result.
#[test]
#[ignore]
fn ivtest_corpus_closure_rate() {
    let Some((root, entries)) = load() else {
        return;
    };
    let dir = root.join("ivtest").join("ivltests");

    let mut outcomes: Vec<(String, Outcome)> = Vec::new();
    for entry in entries.iter().filter(|e| e.kind.starts_with("normal")) {
        let Ok(source) = std::fs::read_to_string(dir.join(format!("{}.v", entry.name))) else {
            continue;
        };
        outcomes.push((entry.name.clone(), judge(&source)));
    }

    let total = outcomes.len();
    let count = |f: &dyn Fn(&Outcome) -> bool| outcomes.iter().filter(|(_, o)| f(o)).count();

    let parsed = count(&|o| *o != Outcome::ParseFailed);
    let elaborated = count(&|o| !matches!(o, Outcome::ParseFailed | Outcome::SetupFailed(_)));
    let ran = count(&|o| matches!(o, Outcome::Passed | Outcome::WrongAnswer | Outcome::Silent));
    let passed = count(&|o| *o == Outcome::Passed);
    let wrong = count(&|o| *o == Outcome::WrongAnswer);
    let silent = count(&|o| *o == Outcome::Silent);

    let pct = |n: usize| 100.0 * n as f64 / total.max(1) as f64;
    println!("\n=== ivtest closure: `regress-vlg.list`, `normal` tests ===");
    println!("{:>5}         corpus files", total);
    println!("{:>5}  {:>5.1}%  parsed", parsed, pct(parsed));
    println!("{:>5}  {:>5.1}%  elaborated", elaborated, pct(elaborated));
    println!("{:>5}  {:>5.1}%  ran without error", ran, pct(ran));
    println!("{:>5}  {:>5.1}%  PASSED   <-- closure", passed, pct(passed));
    println!("{:>5}  {:>5.1}%  wrong answer", wrong, pct(wrong));
    println!(
        "{:>5}  {:>5.1}%  ran but printed nothing",
        silent,
        pct(silent)
    );

    // Where the ones that never ran fell over.
    let mut stages: BTreeMap<String, usize> = BTreeMap::new();
    for (_, outcome) in &outcomes {
        match outcome {
            Outcome::SetupFailed(kind) => {
                *stages.entry(format!("setup: {}", kind)).or_default() += 1
            }
            Outcome::RunFailed(kind) => *stages.entry(format!("run:   {}", kind)).or_default() += 1,
            _ => {}
        }
    }
    if !stages.is_empty() {
        println!("\n--- stopped before reporting ---");
        let mut ranked: Vec<_> = stages.into_iter().collect();
        ranked.sort_by(|a, b| b.1.cmp(&a.1));
        for (stage, n) in ranked {
            println!("{:>5}  {}", n, stage);
        }
    }

    // Naming these matters more than counting them: each one is a design the
    // simulator understood well enough to run and still got wrong.
    let wrong_names: Vec<&str> = outcomes
        .iter()
        .filter(|(_, o)| *o == Outcome::WrongAnswer)
        .map(|(name, _)| name.as_str())
        .collect();
    if !wrong_names.is_empty() {
        println!("\n--- wrong answers (correctness bugs, not missing features) ---");
        for name in wrong_names {
            println!("  {}", name);
        }
    }

    assert!(total > 0, "corpus present but no tests were attempted");
    // A floor, not a target: this guards against a change that silently stops
    // designs running at all. Raise it when closure improves.
    assert!(
        passed >= 80,
        "closure dropped to {}; it has been at least 80",
        passed
    );
}

/// A control for [`ivtest_corpus_closure_rate`], and the reason a low closure
/// number can be trusted: a self-checking design must make it all the way to
/// `PASSED` through the same code path the corpus uses.
#[test]
fn harness_reaches_passed_on_a_self_checking_design() {
    // Deliberately avoids `#5 if (...)`: a delay may currently prefix only an
    // assignment, so a control written the obvious way would fail on a parser
    // gap rather than on anything it was meant to check.
    let source = r#"
        module main;
            reg [3:0] counter;
            initial begin
                counter = 4'b0000;
                #5 counter = counter + 1;
                #5 counter = counter + 1;
                if (counter == 4'b0010) $display("PASSED");
                else $display("FAILED");
            end
        endmodule
    "#;
    assert_eq!(judge(source), Outcome::Passed);
}

/// The other half of the control: a design that computes the wrong thing must
/// be reported as a wrong answer, not quietly as a pass.
#[test]
fn harness_reports_a_wrong_answer_rather_than_passing_it() {
    let source = r#"
        module main;
            initial $display("FAILED");
        endmodule
    "#;
    assert_eq!(judge(source), Outcome::WrongAnswer);
}

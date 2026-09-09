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
//! Every corpus test is `#[ignore]`d: they need that clone, so they must never
//! make CI depend on the network. The control tests below them are not, so a
//! broken harness cannot masquerade as a low score.
//!
//! The corpus validates a test one of two ways, and the list says which: most
//! entries print `PASSED`, while a `gold=<file>` entry is judged by comparing
//! its output to `ivtest/gold/<file>`. Both count towards closure.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use visilog::parsers::modules::VerilogModule;
use visilog::parsers::preprocessor::Preprocessor;
use visilog::parsers::source::{parse_expanded, parse_verilog_source, ParsedSource, SourceError};
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

/// One entry of a `regress-*.list`: a test name, what is expected of it, and —
/// for the quarter of the corpus that is validated by comparison rather than by
/// printing `PASSED` — the gold file its output must match.
struct Entry {
    name: String,
    kind: String,
    gold: Option<String>,
}

/// Parses a regression list. Lines are
/// `name<tab>kind<tab>directory [modulename] [gold=file] [# comment]`.
///
/// `gold=` is optional and may sit in either trailing field, because the
/// optional top-module name comes first when it is present — so every field
/// past the directory is scanned rather than one fixed position.
fn entries(list: &str) -> Vec<Entry> {
    list.lines()
        .map(|line| line.split('#').next().unwrap_or("").trim())
        .filter(|line| !line.is_empty())
        .filter_map(|line| {
            let mut fields = line.split_whitespace();
            let name = fields.next()?.to_string();
            let kind = fields.next()?.to_string();
            let gold = fields
                .find_map(|field| field.strip_prefix("gold="))
                .map(str::to_string);
            Some(Entry { name, kind, gold })
        })
        .collect()
}

/// Whether the source names a `$task` the simulator does not implement.
///
/// Counting every `$` would keep reporting system tasks as a blocker after
/// they were implemented, which is how a survey heuristic quietly goes stale.
fn unsupported_system_names(source: &str) -> bool {
    const SUPPORTED: [&str; 10] = [
        "display", "write", "finish", "time", "stime", "signed", "unsigned", "random", "bits",
        "clog2",
    ];
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
        "unsupported system function ($monitor, $fdisplay, $realtime, ...)",
    );
    // `function` has shipped; a row that counts a feature the front end has
    // would keep reporting it as a blocker for ever.
    note(body.contains("task"), "task");
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

/// A preprocessor that can resolve the corpus's own `` `include `` paths, which
/// are written relative to `ivtest/` and to `ivtest/ivltests/` alike.
fn corpus_preprocessor(root: &Path) -> Preprocessor {
    Preprocessor::new()
        .with_include_dir(root.join("ivtest"))
        .with_include_dir(root.join("ivtest").join("ivltests"))
}

/// The whole front end: expand the directives, then parse what comes out.
fn front_end(preprocessor: &Preprocessor, source: &str) -> Result<ParsedSource, SourceError> {
    let expanded = preprocessor
        .preprocess(source, "<corpus>")
        .map_err(SourceError::Preprocess)?;
    parse_expanded(expanded)
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
    let preprocessor = corpus_preprocessor(&root);

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
        match front_end(&preprocessor, &source) {
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
    let preprocessor = corpus_preprocessor(&root);

    let (mut rejected, mut accepted) = (0usize, 0usize);
    let mut accepted_names: Vec<String> = Vec::new();
    for entry in entries.iter().filter(|e| e.kind == "CE") {
        let path = dir.join(format!("{}.v", entry.name));
        let Ok(source) = std::fs::read_to_string(&path) else {
            continue;
        };
        match front_end(&preprocessor, &source) {
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
    /// It is validated against a gold file, and its output matched. A pass,
    /// kept distinct from [`Outcome::Passed`] so the two validation styles
    /// stay separable in the report.
    GoldMatch,
    /// It is validated against a gold file, ran, and produced *different*
    /// output. Like [`Outcome::WrongAnswer`], a correctness bug rather than a
    /// missing feature. Carries a rendering of the first differing line.
    GoldMismatch(String),
    /// It ran and printed nothing, so it never reached its own check.
    Silent,
}

/// The gold-file lines a comparison is made over.
///
/// Three normalisations, each for a reason:
///
/// * **Trailing whitespace is trimmed per line.** Leading whitespace is *not* —
///   column alignment is often exactly what a `$display` test is checking.
/// * **`VCD info:` lines are dropped from both sides.** iverilog's `$dumpfile`
///   writes `VCD info: dumpfile … opened for output.` into seven of the gold
///   files. visilog has no waveform dumper, so it never emits one; keeping the
///   line would fail those comparisons on an unimplemented side effect rather
///   than on the output the test is actually about.
/// * **A trailing newline is not a difference.** `str::lines` yields the same
///   sequence for `"a\n"` and `"a"`, so a gold file written either way compares
///   equal.
fn gold_lines(text: &str) -> Vec<&str> {
    text.lines()
        .map(str::trim_end)
        .filter(|line| !line.starts_with("VCD info:"))
        .collect()
}

/// The first line at which the output departs from the gold file, rendered for
/// a report. `None` when they agree.
fn first_difference(expected: &str, got: &str) -> Option<String> {
    let (expected, got) = (gold_lines(expected), gold_lines(got));
    let show = |line: Option<&&str>| match line {
        Some(text) if text.chars().count() > 60 => {
            format!("{:?}...", text.chars().take(60).collect::<String>())
        }
        Some(text) => format!("{:?}", text),
        None => "<end of output>".to_string(),
    };
    (0..expected.len().max(got.len()))
        .find(|at| expected.get(*at) != got.get(*at))
        .map(|at| {
            format!(
                "line {}: expected {} got {}",
                at + 1,
                show(expected.get(at)),
                show(got.get(at))
            )
        })
}

fn judge(source: &str) -> Outcome {
    judge_with(&Preprocessor::new(), source, None)
}

/// [`judge`], with an include path — which only the corpus itself needs, since
/// its files include one another by relative path — and with the contents of
/// the entry's gold file, when it has one.
fn judge_with(preprocessor: &Preprocessor, source: &str, gold: Option<&str>) -> Outcome {
    let Ok(parsed) = front_end(preprocessor, source) else {
        return Outcome::ParseFailed;
    };
    let modules = parsed.modules;
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
    if let Some(gold) = gold {
        // A design that printed nothing is Silent, never a match — not even
        // against a gold file that is also empty, because "produced exactly
        // the right emptiness" and "never reached its own checks" cannot be
        // told apart from here and only one of them is a pass. No `normal`
        // entry actually names an empty gold file today, so the rule only
        // reroutes designs that produced nothing against a gold file that
        // expected something; those are failures either way, and calling them
        // Silent keeps the mismatch list to designs that really did print.
        if gold_lines(&output).is_empty() {
            return Outcome::Silent;
        }
        return match first_difference(gold, &output) {
            None => Outcome::GoldMatch,
            Some(difference) => Outcome::GoldMismatch(difference),
        };
    }
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
    let preprocessor = corpus_preprocessor(&root);

    let mut outcomes: Vec<(String, Outcome)> = Vec::new();
    let mut gold_entries = 0usize;
    let mut gold_missing = 0usize;
    let mut gold_names: Vec<String> = Vec::new();
    for entry in entries.iter().filter(|e| e.kind.starts_with("normal")) {
        let Ok(source) = std::fs::read_to_string(dir.join(format!("{}.v", entry.name))) else {
            continue;
        };
        // A `gold=` entry is judged by comparison, not by looking for PASSED.
        // If the file the list names is absent, fall back to PASSED scoring
        // rather than scoring the entry against nothing.
        let gold = entry.gold.as_ref().map(|name| {
            gold_entries += 1;
            gold_names.push(entry.name.clone());
            std::fs::read_to_string(root.join("ivtest").join("gold").join(name))
        });
        let gold = match gold {
            Some(Ok(text)) => Some(text),
            Some(Err(_)) => {
                gold_missing += 1;
                None
            }
            None => None,
        };
        outcomes.push((
            entry.name.clone(),
            judge_with(&preprocessor, &source, gold.as_deref()),
        ));
    }

    let total = outcomes.len();
    let count = |f: &dyn Fn(&Outcome) -> bool| outcomes.iter().filter(|(_, o)| f(o)).count();

    let parsed = count(&|o| *o != Outcome::ParseFailed);
    let elaborated = count(&|o| !matches!(o, Outcome::ParseFailed | Outcome::SetupFailed(_)));
    let ran = count(&|o| {
        matches!(
            o,
            Outcome::Passed
                | Outcome::WrongAnswer
                | Outcome::GoldMatch
                | Outcome::GoldMismatch(_)
                | Outcome::Silent
        )
    });
    let passed = count(&|o| *o == Outcome::Passed);
    let wrong = count(&|o| *o == Outcome::WrongAnswer);
    let gold_match = count(&|o| *o == Outcome::GoldMatch);
    let gold_mismatch = count(&|o| matches!(o, Outcome::GoldMismatch(_)));
    let silent = count(&|o| *o == Outcome::Silent);
    // Gold entries that ran and printed nothing. Deliberately *not* matches,
    // even against an empty gold file: see `judge_with`. Counted so the choice
    // stays visible rather than being an invisible subtraction from closure.
    let gold_silent = outcomes
        .iter()
        .filter(|(name, outcome)| {
            *outcome == Outcome::Silent && gold_names.iter().any(|gold| gold == name)
        })
        .count();
    // A gold test that matches its file is a passing test, so closure is the
    // two populations together — not the `PASSED` count alone, which is blind
    // to the quarter of the corpus that never prints the word.
    let closure = passed + gold_match;

    let pct = |n: usize| 100.0 * n as f64 / total.max(1) as f64;
    println!("\n=== ivtest closure: `regress-vlg.list`, `normal` tests ===");
    println!("{:>5}         corpus files", total);
    println!(
        "{:>5}         of them validated against a gold file",
        gold_entries
    );
    println!("{:>5}  {:>5.1}%  parsed", parsed, pct(parsed));
    println!("{:>5}  {:>5.1}%  elaborated", elaborated, pct(elaborated));
    println!("{:>5}  {:>5.1}%  ran without error", ran, pct(ran));
    println!(
        "{:>5}  {:>5.1}%  closure (PASSED + gold match)",
        closure,
        pct(closure)
    );
    println!(
        "{:>5}  {:>5.1}%    of which printed PASSED",
        passed,
        pct(passed)
    );
    println!(
        "{:>5}  {:>5.1}%    of which matched a gold file",
        gold_match,
        pct(gold_match)
    );
    println!("{:>5}  {:>5.1}%  wrong answer", wrong, pct(wrong));
    println!(
        "{:>5}  {:>5.1}%  gold mismatch",
        gold_mismatch,
        pct(gold_mismatch)
    );
    println!(
        "{:>5}  {:>5.1}%  ran but printed nothing",
        silent,
        pct(silent)
    );
    println!(
        "{:>5}         of those, gold entries that printed nothing (never a match)",
        gold_silent
    );
    if gold_missing > 0 {
        println!(
            "{:>5}         gold file named by the list but not present (scored by PASSED instead)",
            gold_missing
        );
    }

    // One machine-readable line, so CI reports the trend by grepping a stable
    // key rather than by scraping the table above — which is free to change.
    // Keys are only ever added, never renamed or dropped.
    println!(
        "\nCORPUS_METRICS total={} parsed={} elaborated={} ran={} passed={} wrong={} silent={} \
         gold={} gold_match={} gold_mismatch={} gold_silent={} closure={}",
        total,
        parsed,
        elaborated,
        ran,
        passed,
        wrong,
        silent,
        gold_entries,
        gold_match,
        gold_mismatch,
        gold_silent,
        closure
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

    // The same treatment for the gold population: a file that ran and produced
    // *different* output is a precise, self-maintaining bug list. A short
    // first-difference for the leading few is what makes it actionable without
    // burying the rest.
    let mismatches: Vec<(&str, &str)> = outcomes
        .iter()
        .filter_map(|(name, outcome)| match outcome {
            Outcome::GoldMismatch(difference) => Some((name.as_str(), difference.as_str())),
            _ => None,
        })
        .collect();
    if !mismatches.is_empty() {
        println!("\n--- gold mismatches (ran, but output differs from the gold file) ---");
        for (at, (name, difference)) in mismatches.iter().enumerate() {
            if at < 12 {
                println!("  {}\n      {}", name, difference);
            } else {
                println!("  {}", name);
            }
        }
    }

    assert!(total > 0, "corpus present but no tests were attempted");
    // A floor, not a target: this guards against a change that silently stops
    // designs running at all. Raise it when closure improves.
    assert!(
        closure >= 440,
        "closure dropped to {}; it has been at least 440",
        closure
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

/// The control for the *other* validation style, and the reason the gold half
/// of the closure number can be trusted: a design whose output matches its gold
/// file must score a match, and one that differs must score a mismatch.
///
/// A comparator that said "match" to everything would inflate closure by 358
/// with nothing to show for it, and a comparator that said "mismatch" to
/// everything would look exactly like a simulator that got 358 answers wrong.
/// Neither can hide behind an `#[ignore]`, because this test is not ignored.
#[test]
fn harness_scores_a_gold_test_by_comparing_its_output() {
    // Two lines, indented, so the comparison is over something with structure
    // rather than a single word that a substring search would also have found.
    let source = r#"
        module main;
            initial begin
                $display("  a = %0d", 3);
                $display("  b = %0d", 4);
            end
        endmodule
    "#;
    let gold = "  a = 3\n  b = 4\n";
    assert_eq!(
        judge_with(&Preprocessor::new(), source, Some(gold)),
        Outcome::GoldMatch
    );

    // Trailing whitespace and a missing final newline are normalised away; the
    // leading indent is not, because column alignment is what these tests check.
    let sloppy = "  a = 3   \n  b = 4";
    assert_eq!(
        judge_with(&Preprocessor::new(), source, Some(sloppy)),
        Outcome::GoldMatch
    );

    // A different value must be reported as a mismatch, naming where it parted.
    let wrong = "  a = 3\n  b = 5\n";
    let outcome = judge_with(&Preprocessor::new(), source, Some(wrong));
    match outcome {
        Outcome::GoldMismatch(difference) => {
            assert!(
                difference.starts_with("line 2:"),
                "should point at the first differing line, got {:?}",
                difference
            );
        }
        other => panic!("expected a gold mismatch, got {:?}", other),
    }

    // And a design that prints nothing is not a match against an empty gold
    // file — that pairing cannot be told apart from one that never ran.
    let mute = r#"
        module main;
            reg a;
            initial a = 1;
        endmodule
    "#;
    assert_eq!(
        judge_with(&Preprocessor::new(), mute, Some("")),
        Outcome::Silent
    );
}

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

use visilog::parsers::source::parse_verilog_source;

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
    for entry in entries.iter().filter(|e| e.kind == "CE") {
        let path = dir.join(format!("{}.v", entry.name));
        let Ok(source) = std::fs::read_to_string(&path) else {
            continue;
        };
        match parse_verilog_source(&source) {
            Ok(_) => accepted += 1,
            Err(_) => rejected += 1,
        }
    }

    println!("\n=== ivtest `CE` tests (must not compile) ===");
    println!("rejected (correct) : {}", rejected);
    println!(
        "accepted (too permissive, or accidentally right) : {}",
        accepted
    );
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

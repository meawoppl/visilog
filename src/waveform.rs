//! Reading a value change dump back, and comparing two of them.
//!
//! Two simulators that agree about a design write VCD files that still differ
//! in nearly every byte: the identifier codes, the `$date`, the order the
//! variables are declared in, whether a variable is a `wire`, a `reg` or an
//! `integer`, how many leading digits a vector keeps, and the unit time is
//! counted in. [`Waveform::parse`] throws all of that away and keeps what the
//! design did — every variable by its hierarchical name and width, and the
//! value it held from each instant of *physical* time — and [`compare`] finds
//! the first instant two of them disagree.

use std::collections::BTreeMap;
use std::fmt;

use serde::{Deserialize, Serialize};

/// One variable's history: its width, and the value it took at each instant it
/// changed, in femtoseconds, with no two consecutive entries equal.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Trace {
    pub width: usize,
    pub changes: Vec<(u128, String)>,
}

impl Trace {
    /// The value at `time`: the last change at or before it, or `None` before
    /// the first.
    pub fn value_at(&self, time: u128) -> Option<&str> {
        let index = self.changes.partition_point(|(at, _)| *at <= time);
        index
            .checked_sub(1)
            .map(|index| self.changes[index].1.as_str())
    }
}

/// A dump, normalised: hierarchical name → [`Trace`].
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Waveform {
    pub traces: BTreeMap<String, Trace>,
    /// The last time the dump mentions, in femtoseconds.
    pub end: u128,
}

/// Why a dump could not be read.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WaveformError(pub String);

impl fmt::Display for WaveformError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for WaveformError {}

/// A variable as the header declared it.
struct Declared {
    names: Vec<String>,
    width: usize,
}

impl Waveform {
    /// Reads a VCD file's text.
    ///
    /// A variable inside a scope whose name begins with `$` is left out — that
    /// is a simulator's own bookkeeping, like iverilog's `$ivl_for_loop0`
    /// around a loop variable, and not something both sides can be expected to
    /// dump. So is a `$var parameter`, which iverilog writes and which never
    /// changes. One identifier code declared under several names — a port and
    /// the signal it is bound to — gives each name the same trace.
    pub fn parse(text: &str) -> Result<Waveform, WaveformError> {
        let mut tokens = text.split_whitespace().peekable();
        let mut scope: Vec<String> = Vec::new();
        let mut declared: BTreeMap<String, Declared> = BTreeMap::new();
        let mut femtoseconds_per_tick: u128 = 1;

        // The header.
        while let Some(token) = tokens.next() {
            match token {
                "$timescale" => {
                    let mut spec = String::new();
                    for token in tokens.by_ref() {
                        if token == "$end" {
                            break;
                        }
                        spec.push_str(token);
                    }
                    femtoseconds_per_tick = timescale_femtoseconds(&spec)?;
                }
                "$scope" => {
                    let _kind = tokens.next();
                    let name = tokens
                        .next()
                        .ok_or_else(|| WaveformError("a $scope with no name".into()))?;
                    scope.push(name.to_string());
                    skip_to_end(&mut tokens);
                }
                "$upscope" => {
                    scope.pop();
                    skip_to_end(&mut tokens);
                }
                "$var" => {
                    let kind = tokens.next().unwrap_or_default();
                    let width: usize = tokens
                        .next()
                        .and_then(|w| w.parse().ok())
                        .ok_or_else(|| WaveformError("a $var with no width".into()))?;
                    let code = tokens
                        .next()
                        .ok_or_else(|| WaveformError("a $var with no identifier".into()))?;
                    let mut name = tokens.next().unwrap_or_default().to_string();
                    // `name [7:0]` is a range written after the name; `name[3]`
                    // is one bit or word, and part of the name.
                    for token in tokens.by_ref() {
                        if token == "$end" {
                            break;
                        }
                        if !token.starts_with('[') || !name.is_empty() && token.contains(':') {
                            continue;
                        }
                        name.push_str(token);
                    }
                    let hidden = kind == "parameter" || scope.iter().any(|s| s.starts_with('$'));
                    if !hidden {
                        let full = scope
                            .iter()
                            .map(String::as_str)
                            .chain(std::iter::once(name.as_str()))
                            .collect::<Vec<_>>()
                            .join(".");
                        declared
                            .entry(code.to_string())
                            .or_insert_with(|| Declared {
                                names: Vec::new(),
                                width,
                            })
                            .names
                            .push(full);
                    }
                }
                "$enddefinitions" => {
                    skip_to_end(&mut tokens);
                    break;
                }
                _ if token.starts_with('$') => skip_to_end(&mut tokens),
                _ => {}
            }
        }

        // The value changes.
        let mut by_code: BTreeMap<String, Vec<(u128, String)>> = BTreeMap::new();
        let mut now: u128 = 0;
        let mut end: u128 = 0;
        while let Some(token) = tokens.next() {
            let first = token.as_bytes()[0];
            match first {
                b'#' => {
                    let ticks: u128 = token[1..]
                        .parse()
                        .map_err(|_| WaveformError(format!("bad time `{}`", token)))?;
                    now = ticks * femtoseconds_per_tick;
                    end = end.max(now);
                }
                b'$' => {
                    // `$dumpvars`, `$dumpon`, … wrap value changes, which are
                    // read like any other; `$comment` wraps text, which is not.
                    if token == "$comment" {
                        skip_to_end(&mut tokens);
                    }
                }
                b'b' | b'B' | b'r' | b'R' => {
                    let code = tokens
                        .next()
                        .ok_or_else(|| WaveformError(format!("`{}` names nothing", token)))?;
                    if let Some(var) = declared.get(code) {
                        let value = if first == b'r' || first == b'R' {
                            token[1..].to_string()
                        } else {
                            extended(&token[1..].to_ascii_lowercase(), var.width)
                        };
                        record(by_code.entry(code.to_string()).or_default(), now, value);
                    }
                }
                _ => {
                    let (value, code) = token.split_at(1);
                    if let Some(var) = declared.get(code) {
                        let value = extended(&value.to_ascii_lowercase(), var.width);
                        record(by_code.entry(code.to_string()).or_default(), now, value);
                    }
                }
            }
        }

        let mut traces = BTreeMap::new();
        for (code, var) in declared {
            let changes = by_code.remove(&code).unwrap_or_default();
            for name in var.names {
                traces.insert(
                    name,
                    Trace {
                        width: var.width,
                        changes: changes.clone(),
                    },
                );
            }
        }
        Ok(Waveform { traces, end })
    }
}

fn skip_to_end<'a>(tokens: &mut impl Iterator<Item = &'a str>) {
    for token in tokens.by_ref() {
        if token == "$end" {
            break;
        }
    }
}

/// Appends a change, replacing one already at the same instant and dropping
/// one that changes nothing.
fn record(changes: &mut Vec<(u128, String)>, at: u128, value: String) {
    if let Some((last_at, last)) = changes.last_mut() {
        if *last_at == at {
            *last = value;
            if changes.len() >= 2 && changes[changes.len() - 2].1 == changes[changes.len() - 1].1 {
                changes.pop();
            }
            return;
        }
        if *last == value {
            return;
        }
    }
    changes.push((at, value));
}

/// A vector value at its declared width, the way a VCD reader re-extends a
/// trimmed one: a leading `x` or `z` repeats, anything else pads with `0`.
fn extended(digits: &str, width: usize) -> String {
    if digits.len() >= width {
        return digits[digits.len() - width..].to_string();
    }
    let fill = match digits.as_bytes().first() {
        Some(b'x') => 'x',
        Some(b'z') => 'z',
        _ => '0',
    };
    let mut out: String = std::iter::repeat_n(fill, width - digits.len()).collect();
    out.push_str(digits);
    out
}

/// `1ns`, `10 ps`, `100us` → femtoseconds.
fn timescale_femtoseconds(spec: &str) -> Result<u128, WaveformError> {
    let split = spec
        .find(|c: char| !c.is_ascii_digit())
        .ok_or_else(|| WaveformError(format!("bad $timescale `{}`", spec)))?;
    let (count, unit) = spec.split_at(split);
    let count: u128 = count
        .parse()
        .map_err(|_| WaveformError(format!("bad $timescale `{}`", spec)))?;
    let unit: u128 = match unit {
        "s" => 1_000_000_000_000_000,
        "ms" => 1_000_000_000_000,
        "us" => 1_000_000_000,
        "ns" => 1_000_000,
        "ps" => 1_000,
        "fs" => 1,
        _ => return Err(WaveformError(format!("bad $timescale unit `{}`", unit))),
    };
    Ok(count * unit)
}

/// Where two dumps first disagree about one variable.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Divergence {
    pub signal: String,
    /// Femtoseconds.
    pub time: u128,
    pub expected: Option<String>,
    pub found: Option<String>,
}

/// What comparing a candidate dump against a reference found.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Comparison {
    /// Variables both dumps hold.
    pub compared: usize,
    /// Variables that differ, earliest disagreement first.
    pub divergences: Vec<Divergence>,
    /// Variables both hold at different widths.
    pub width_mismatches: Vec<String>,
    /// Variables only the reference dumped.
    pub only_in_reference: Vec<String>,
    /// Variables only the candidate dumped.
    pub only_in_candidate: Vec<String>,
}

impl Comparison {
    /// Whether every variable both dumps hold agrees at every instant.
    pub fn agrees(&self) -> bool {
        self.divergences.is_empty() && self.width_mismatches.is_empty()
    }

    /// The earliest disagreement, if there is one.
    pub fn first(&self) -> Option<&Divergence> {
        self.divergences.first()
    }
}

/// Compares `candidate` against `reference`, variable by variable, up to the
/// end of the shorter of the two.
///
/// Only the instants up to where both dumps reach are compared, because a run
/// that stopped earlier has said nothing about what came after.
pub fn compare(reference: &Waveform, candidate: &Waveform) -> Comparison {
    let horizon = reference.end.min(candidate.end);
    let mut comparison = Comparison::default();
    for (name, expected) in &reference.traces {
        let Some(found) = candidate.traces.get(name) else {
            comparison.only_in_reference.push(name.clone());
            continue;
        };
        comparison.compared += 1;
        if expected.width != found.width {
            comparison.width_mismatches.push(name.clone());
            continue;
        }
        let mut instants: Vec<u128> = expected
            .changes
            .iter()
            .chain(&found.changes)
            .map(|(at, _)| *at)
            .filter(|at| *at <= horizon)
            .collect();
        instants.sort_unstable();
        instants.dedup();
        for at in instants {
            let (want, got) = (expected.value_at(at), found.value_at(at));
            if want != got {
                comparison.divergences.push(Divergence {
                    signal: name.clone(),
                    time: at,
                    expected: want.map(str::to_string),
                    found: got.map(str::to_string),
                });
                break;
            }
        }
    }
    comparison.only_in_candidate = candidate
        .traces
        .keys()
        .filter(|name| !reference.traces.contains_key(*name))
        .cloned()
        .collect();
    comparison
        .divergences
        .sort_by(|a, b| a.time.cmp(&b.time).then_with(|| a.signal.cmp(&b.signal)));
    comparison
}

#[cfg(test)]
mod tests {
    use super::*;

    const ICARUS: &str = "\
$date Wed $end
$version Icarus Verilog $end
$timescale 1ns $end
$scope module t $end
$var reg 1 ! clk $end
$var reg 4 \" q [3:0] $end
$var parameter 32 # W $end
$scope begin $ivl_for_loop0 $end
$var integer 32 $ i [31:0] $end
$upscope $end
$upscope $end
$enddefinitions $end
#0
$dumpvars
0!
bx \"
b1000 #
$end
#5
1!
b11 \"
#10
0!
";

    const VISILOG: &str = "\
$date 2026 $end
$version visilog $end
$timescale 1ps $end
$scope module t $end
$var reg 4 ! q [3:0] $end
$var wire 1 \" clk $end
$upscope $end
$enddefinitions $end
#0
$dumpvars
0\"
bxxxx !
$end
#5000
1\"
b0011 !
#10000
0\"
";

    #[test]
    fn test_identifiers_units_kinds_and_trimming_are_normalised_away() {
        let reference = Waveform::parse(ICARUS).unwrap();
        let candidate = Waveform::parse(VISILOG).unwrap();
        assert_eq!(
            reference.traces["t.q"].changes[1],
            (5_000_000, "0011".into())
        );
        assert!(
            !reference.traces.contains_key("t.W"),
            "a parameter is left out"
        );
        assert!(
            !reference.traces.keys().any(|k| k.contains("$ivl")),
            "a simulator's own scope is left out"
        );
        let comparison = compare(&reference, &candidate);
        assert!(comparison.agrees(), "{:?}", comparison);
        assert_eq!(comparison.compared, 2);
    }

    #[test]
    fn test_the_first_divergence_names_the_signal_and_the_instant() {
        let reference = Waveform::parse(ICARUS).unwrap();
        let candidate = Waveform::parse(&VISILOG.replace("b0011 !", "b0111 !")).unwrap();
        let comparison = compare(&reference, &candidate);
        let first = comparison.first().expect("q differs");
        assert_eq!(first.signal, "t.q");
        assert_eq!(first.time, 5_000_000);
        assert_eq!(first.expected.as_deref(), Some("0011"));
        assert_eq!(first.found.as_deref(), Some("0111"));
    }

    #[test]
    fn test_a_change_at_the_same_instant_replaces_the_one_before() {
        let text = "$timescale 1s $end $scope module m $end $var reg 1 ! a $end \
                    $upscope $end $enddefinitions $end #0 0! #1 1! 0! #2 1!";
        let waveform = Waveform::parse(text).unwrap();
        assert_eq!(
            waveform.traces["m.a"].changes,
            vec![
                (0, "0".to_string()),
                (2 * 1_000_000_000_000_000, "1".to_string())
            ]
        );
    }

    #[test]
    fn test_comparison_stops_where_the_shorter_dump_does() {
        let reference = Waveform::parse(ICARUS).unwrap();
        let cut = VISILOG.split("#10000").next().unwrap();
        let candidate = Waveform::parse(cut).unwrap();
        assert!(compare(&reference, &candidate).agrees());
    }
}

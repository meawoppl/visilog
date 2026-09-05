//! The Verilog preprocessor: a lexical pass that runs *before* the grammar.
//!
//! Verilog's backtick directives are not part of the module grammar — they are
//! text substitution over the source file. This module consumes source text and
//! produces source text, so [`crate::parsers::source::parse_verilog_source`]
//! never has to know they existed.
//!
//! Expansion moves text around, so a byte offset in the output no longer lines
//! up with the input. Every emission therefore records where it came from, and
//! the resulting [`SourceMap`] turns an offset back into a file, a line, and —
//! when the text came out of a macro — the name of that macro.

use std::collections::HashMap;
use std::fmt;
use std::path::{Path, PathBuf};

/// Directives that carry no meaning for this front end but must not be a parse
/// error. Each is consumed together with the rest of its line.
const IGNORED_DIRECTIVES: [&str; 24] = [
    "begin_keywords",
    "end_keywords",
    "resetall",
    "celldefine",
    "endcelldefine",
    "default_nettype",
    "unconnected_drive",
    "nounconnected_drive",
    "line",
    "pragma",
    "protect",
    "endprotect",
    "suppress_faults",
    "nosuppress_faults",
    "enable_portfaults",
    "disable_portfaults",
    "delay_mode_path",
    "delay_mode_unit",
    "delay_mode_zero",
    "delay_mode_distributed",
    "default_decay_time",
    "default_trireg_strength",
    "autoexpand_vectornets",
    "uselib",
];

// ---------------------------------------------------------------------------
// Timescale
// ---------------------------------------------------------------------------

/// The unit half of a `` `timescale `` term.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TimeUnit {
    Seconds,
    Milliseconds,
    Microseconds,
    Nanoseconds,
    Picoseconds,
    Femtoseconds,
}

impl TimeUnit {
    pub fn suffix(self) -> &'static str {
        match self {
            TimeUnit::Seconds => "s",
            TimeUnit::Milliseconds => "ms",
            TimeUnit::Microseconds => "us",
            TimeUnit::Nanoseconds => "ns",
            TimeUnit::Picoseconds => "ps",
            TimeUnit::Femtoseconds => "fs",
        }
    }

    /// The power of ten this unit represents, in seconds.
    pub fn exponent(self) -> i32 {
        match self {
            TimeUnit::Seconds => 0,
            TimeUnit::Milliseconds => -3,
            TimeUnit::Microseconds => -6,
            TimeUnit::Nanoseconds => -9,
            TimeUnit::Picoseconds => -12,
            TimeUnit::Femtoseconds => -15,
        }
    }

    fn parse(text: &str) -> Option<TimeUnit> {
        match text {
            "s" => Some(TimeUnit::Seconds),
            "ms" => Some(TimeUnit::Milliseconds),
            "us" => Some(TimeUnit::Microseconds),
            "ns" => Some(TimeUnit::Nanoseconds),
            "ps" => Some(TimeUnit::Picoseconds),
            "fs" => Some(TimeUnit::Femtoseconds),
            _ => None,
        }
    }
}

impl fmt::Display for TimeUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.suffix())
    }
}

/// One `` `timescale `` term: a magnitude of 1, 10 or 100 and a unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TimeSpec {
    pub value: u32,
    pub unit: TimeUnit,
}

impl TimeSpec {
    /// The term expressed in femtoseconds — the finest unit Verilog has, so
    /// every legal timescale is an exact integer number of them.
    pub fn femtoseconds(self) -> u64 {
        let steps = (self.unit.exponent() + 15) as u32;
        u64::from(self.value) * 10u64.pow(steps)
    }

    fn parse(text: &str) -> Result<TimeSpec, String> {
        let split = text
            .find(|c: char| !c.is_ascii_digit())
            .ok_or_else(|| format!("`{}` has no unit", text))?;
        let (value, suffix) = text.split_at(split);
        let value: u32 = value
            .parse()
            .map_err(|_| format!("`{}` has no magnitude", text))?;
        if !matches!(value, 1 | 10 | 100) {
            return Err(format!("magnitude {} is not 1, 10 or 100", value));
        }
        let unit =
            TimeUnit::parse(suffix.trim()).ok_or_else(|| format!("unknown unit `{}`", suffix))?;
        Ok(TimeSpec { value, unit })
    }
}

impl fmt::Display for TimeSpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.value, self.unit)
    }
}

/// A `` `timescale <unit> / <precision> `` directive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Timescale {
    pub unit: TimeSpec,
    pub precision: TimeSpec,
}

impl Timescale {
    fn parse(text: &str) -> Result<Timescale, String> {
        let (unit, precision) = text
            .split_once('/')
            .ok_or_else(|| "expected `<unit> / <precision>`".to_string())?;
        let unit = TimeSpec::parse(unit.trim())?;
        let precision = TimeSpec::parse(precision.trim())?;
        if precision.femtoseconds() > unit.femtoseconds() {
            return Err(format!(
                "precision {} is coarser than unit {}",
                precision, unit
            ));
        }
        Ok(Timescale { unit, precision })
    }
}

impl fmt::Display for Timescale {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.unit, self.precision)
    }
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// What went wrong, without the "where".
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    /// A `` `name `` that was never defined. Deliberately not an empty
    /// expansion: a macro that silently vanished would turn a typo into a
    /// mystery parse error somewhere else entirely.
    UndefinedMacro(String),
    /// A macro that, directly or through others, expands to itself.
    RecursiveMacro(String),
    /// A function-like macro invoked with the wrong argument list.
    BadArguments { name: String, detail: String },
    /// A directive whose own syntax did not hold up.
    MalformedDirective { directive: String, detail: String },
    /// An `` `else ``/`` `endif `` with no `` `ifdef ``, or the other way round.
    UnbalancedConditional(String),
    /// An `` `include `` that could not be resolved or read.
    Include { path: String, detail: String },
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::UndefinedMacro(name) => write!(f, "undefined macro `{}", name),
            ErrorKind::RecursiveMacro(name) => write!(f, "macro `{} expands to itself", name),
            ErrorKind::BadArguments { name, detail } => {
                write!(f, "bad arguments to macro `{}: {}", name, detail)
            }
            ErrorKind::MalformedDirective { directive, detail } => {
                write!(f, "malformed `{} directive: {}", directive, detail)
            }
            ErrorKind::UnbalancedConditional(detail) => {
                write!(f, "unbalanced conditional compilation: {}", detail)
            }
            ErrorKind::Include { path, detail } => {
                write!(f, "cannot include \"{}\": {}", path, detail)
            }
        }
    }
}

/// A preprocessing failure, reported against the original source position.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocessError {
    pub kind: ErrorKind,
    /// `file:line`, plus the macro being expanded when the text is not the
    /// file's own.
    pub at: String,
}

impl fmt::Display for PreprocessError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.at, self.kind)
    }
}

impl std::error::Error for PreprocessError {}

// ---------------------------------------------------------------------------
// Source map
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Loc {
    file: usize,
    line: usize,
    expansion: Option<usize>,
}

#[derive(Debug, Clone, Copy)]
struct Segment {
    start: usize,
    loc: Loc,
}

/// Where a stretch of expanded text came from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location<'a> {
    pub file: &'a str,
    pub line: usize,
    /// The macro this text was expanded from, if any. `line` then names the
    /// *invocation*, since a macro body has no line of its own in the file.
    pub expansion_of: Option<&'a str>,
}

impl fmt::Display for Location<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.file, self.line)?;
        if let Some(name) = self.expansion_of {
            write!(f, " (expanding `{})", name)?;
        }
        Ok(())
    }
}

/// Maps an offset in the expanded text back to its origin.
///
/// Built as the text is emitted rather than reconstructed afterwards: once a
/// macro body has been spliced in, nothing about the output says which file or
/// line it came from, and no amount of diffing recovers it.
#[derive(Debug, Clone, Default)]
pub struct SourceMap {
    names: Vec<String>,
    segments: Vec<Segment>,
}

impl SourceMap {
    /// The origin of the byte at `offset`, or `None` if the map is empty.
    pub fn locate(&self, offset: usize) -> Option<Location<'_>> {
        let index = match self
            .segments
            .binary_search_by(|segment| segment.start.cmp(&offset))
        {
            Ok(index) => index,
            Err(0) => return None,
            Err(index) => index - 1,
        };
        let loc = self.segments[index].loc;
        Some(Location {
            file: &self.names[loc.file],
            line: loc.line,
            expansion_of: loc.expansion.map(|name| self.names[name].as_str()),
        })
    }

    /// [`SourceMap::locate`], rendered for a diagnostic.
    pub fn describe(&self, offset: usize) -> String {
        match self.locate(offset) {
            Some(location) => location.to_string(),
            None => "<unknown>".to_string(),
        }
    }
}

/// Expanded source, plus everything the directives said along the way.
#[derive(Debug, Clone)]
pub struct Preprocessed {
    pub text: String,
    pub map: SourceMap,
    /// The last `` `timescale `` seen. Recorded rather than discarded because
    /// simulated time is otherwise a bare integer with no unit attached.
    pub timescale: Option<Timescale>,
}

// ---------------------------------------------------------------------------
// The preprocessor
// ---------------------------------------------------------------------------

/// A macro definition. `params` is `None` for an object-like macro and
/// `Some(vec![])` for `` `define f() … `` — the difference decides whether a
/// following `(` belongs to the invocation or to the code after it.
#[derive(Debug, Clone)]
struct MacroDef {
    params: Option<Vec<Param>>,
    body: String,
}

#[derive(Debug, Clone)]
struct Param {
    name: String,
    default: Option<String>,
}

/// One `` `ifdef `` frame. `taken` records whether an arm has already been
/// emitted, which is what makes an `` `elsif `` chain exclusive.
#[derive(Debug, Clone, Copy)]
struct Cond {
    parent_active: bool,
    active: bool,
    taken: bool,
}

/// Configuration for a preprocessing run.
#[derive(Debug, Clone, Default)]
pub struct Preprocessor {
    include_dirs: Vec<PathBuf>,
}

impl Preprocessor {
    pub fn new() -> Preprocessor {
        Preprocessor::default()
    }

    /// Add a directory to search for `` `include `` files.
    pub fn with_include_dir(mut self, dir: impl Into<PathBuf>) -> Preprocessor {
        self.include_dirs.push(dir.into());
        self
    }

    /// Expand `source`, which is reported as coming from the file `name`.
    pub fn preprocess(&self, source: &str, name: &str) -> Result<Preprocessed, PreprocessError> {
        let mut run = Run {
            config: self,
            macros: HashMap::new(),
            names: Vec::new(),
            ids: HashMap::new(),
            out: Emitter::default(),
            conds: Vec::new(),
            expanding: Vec::new(),
            open_includes: Vec::new(),
            timescale: None,
        };
        let file = run.intern(name);
        run.scan(source, Origin::File(file))?;

        if !run.conds.is_empty() {
            let at = run.at(Loc {
                file,
                line: source.lines().count().max(1),
                expansion: None,
            });
            let open = run.conds.len();
            return Err(PreprocessError {
                kind: ErrorKind::UnbalancedConditional(format!(
                    "{} `ifdef without a matching `endif",
                    open
                )),
                at,
            });
        }

        Ok(Preprocessed {
            text: run.out.text,
            map: SourceMap {
                names: run.names,
                segments: run.out.segments,
            },
            timescale: run.timescale,
        })
    }
}

/// Which file and line the text being scanned belongs to.
///
/// A macro body has no position of its own, so everything it emits is pinned to
/// the invocation instead — which is the position a diagnostic should name.
#[derive(Debug, Clone, Copy)]
enum Origin {
    File(usize),
    Macro {
        file: usize,
        line: usize,
        name: usize,
    },
}

impl Origin {
    fn loc(self, line: usize) -> Loc {
        match self {
            Origin::File(file) => Loc {
                file,
                line,
                expansion: None,
            },
            Origin::Macro {
                file,
                line: invoked,
                name,
            } => Loc {
                file,
                line: invoked,
                expansion: Some(name),
            },
        }
    }
}

#[derive(Debug, Default)]
struct Emitter {
    text: String,
    segments: Vec<Segment>,
    last: Option<Loc>,
}

impl Emitter {
    fn emit(&mut self, loc: Loc, text: &str) {
        if text.is_empty() {
            return;
        }
        if self.last != Some(loc) {
            self.segments.push(Segment {
                start: self.text.len(),
                loc,
            });
            self.last = Some(loc);
        }
        self.text.push_str(text);
    }
}

struct Run<'a> {
    config: &'a Preprocessor,
    macros: HashMap<String, MacroDef>,
    names: Vec<String>,
    ids: HashMap<String, usize>,
    out: Emitter,
    conds: Vec<Cond>,
    expanding: Vec<String>,
    open_includes: Vec<PathBuf>,
    timescale: Option<Timescale>,
}

impl Run<'_> {
    fn intern(&mut self, name: &str) -> usize {
        if let Some(&id) = self.ids.get(name) {
            return id;
        }
        let id = self.names.len();
        self.names.push(name.to_string());
        self.ids.insert(name.to_string(), id);
        id
    }

    fn at(&self, loc: Loc) -> String {
        let mut at = format!("{}:{}", self.names[loc.file], loc.line);
        if let Some(name) = loc.expansion {
            at.push_str(&format!(" (expanding `{})", self.names[name]));
        }
        at
    }

    fn error(&self, loc: Loc, kind: ErrorKind) -> PreprocessError {
        PreprocessError {
            kind,
            at: self.at(loc),
        }
    }

    fn malformed(&self, loc: Loc, directive: &str, detail: impl Into<String>) -> PreprocessError {
        self.error(
            loc,
            ErrorKind::MalformedDirective {
                directive: directive.to_string(),
                detail: detail.into(),
            },
        )
    }

    fn emitting(&self) -> bool {
        self.conds.last().map_or(true, |cond| cond.active)
    }

    /// The main lexical pass. Comments and string literals are recognised so a
    /// backtick inside either is text rather than a directive.
    fn scan(&mut self, text: &str, origin: Origin) -> Result<(), PreprocessError> {
        let bytes = text.as_bytes();
        let mut i = 0usize;
        let mut line = 1usize;

        while i < bytes.len() {
            let loc = origin.loc(line);
            let emitting = self.emitting();

            if bytes[i] == b'\n' {
                if emitting {
                    self.out.emit(loc, "\n");
                }
                line += 1;
                i += 1;
                continue;
            }

            if bytes[i] == b'/' && bytes.get(i + 1) == Some(&b'/') {
                let end = line_end(bytes, i);
                if emitting {
                    self.out.emit(loc, &text[i..end]);
                    // A macro body that ends inside a line comment would
                    // otherwise comment out whatever follows the invocation.
                    if end == bytes.len() {
                        self.out.emit(loc, "\n");
                    }
                }
                i = end;
                continue;
            }

            if bytes[i] == b'/' && bytes.get(i + 1) == Some(&b'*') {
                let Some(end) = block_end(bytes, i) else {
                    return Err(self.malformed(loc, "/*", "unterminated block comment"));
                };
                if emitting {
                    self.out.emit(loc, &text[i..end]);
                }
                line += bytes[i..end].iter().filter(|&&byte| byte == b'\n').count();
                i = end;
                continue;
            }

            if bytes[i] == b'"' {
                let end = string_end(bytes, i);
                if emitting {
                    self.out.emit(loc, &text[i..end]);
                }
                i = end;
                continue;
            }

            // An escaped identifier runs from the backslash to the next
            // whitespace and may contain anything at all — including a
            // backtick, which is not a directive there.
            if bytes[i] == b'\\' && bytes.get(i + 1).is_some_and(|b| !b.is_ascii_whitespace()) {
                let mut end = i + 1;
                while end < bytes.len() && !bytes[end].is_ascii_whitespace() {
                    end += 1;
                }
                if emitting {
                    self.out.emit(loc, &text[i..end]);
                }
                i = end;
                continue;
            }

            if bytes[i] == b'`' {
                self.backtick(text, &mut i, &mut line, origin)?;
                continue;
            }

            // Ordinary text, copied a run at a time. Every byte that ends a run
            // is ASCII, so both ends of the slice are character boundaries.
            let start = i;
            i += 1;
            while i < bytes.len() && !matches!(bytes[i], b'/' | b'"' | b'`' | b'\n' | b'\\') {
                i += 1;
            }
            if emitting {
                self.out.emit(loc, &text[start..i]);
            }
        }
        Ok(())
    }

    /// Everything that starts with a backtick: the escapes, then the directives,
    /// then — by elimination — a macro invocation.
    fn backtick(
        &mut self,
        text: &str,
        i: &mut usize,
        line: &mut usize,
        origin: Origin,
    ) -> Result<(), PreprocessError> {
        let bytes = text.as_bytes();
        let loc = origin.loc(*line);
        let emitting = self.emitting();

        // `\`" produces an escaped quote, `" a bare one, `` a token paste that
        // leaves nothing behind. Order matters: `\`" opens with a backtick too.
        if text[*i..].starts_with("`\\`\"") {
            if emitting {
                self.out.emit(loc, "\\\"");
            }
            *i += 4;
            return Ok(());
        }
        if bytes.get(*i + 1) == Some(&b'`') {
            *i += 2;
            return Ok(());
        }
        if bytes.get(*i + 1) == Some(&b'"') {
            if emitting {
                self.out.emit(loc, "\"");
            }
            *i += 2;
            return Ok(());
        }

        let len = ident_len(bytes, *i + 1);
        if len == 0 {
            return Err(self.malformed(
                loc,
                "`",
                "a backtick must be followed by a directive or macro name",
            ));
        }
        let name = text[*i + 1..*i + 1 + len].to_string();
        *i += 1 + len;

        match name.as_str() {
            "define" => {
                let (macro_name, def) = read_define(text, i, line)
                    .map_err(|detail| self.malformed(loc, "define", detail))?;
                if emitting {
                    self.macros.insert(macro_name, def);
                }
            }
            "undef" => {
                let target = read_name(text, i)
                    .ok_or_else(|| self.malformed(loc, "undef", "expected a macro name"))?;
                if emitting {
                    self.macros.remove(&target);
                }
            }
            "undefineall" => {
                if emitting {
                    self.macros.clear();
                }
            }
            "ifdef" | "ifndef" => {
                let target = read_name(text, i)
                    .ok_or_else(|| self.malformed(loc, &name, "expected a macro name"))?;
                let defined = self.macros.contains_key(&target);
                let wanted = if name == "ifdef" { defined } else { !defined };
                let parent_active = emitting;
                let active = parent_active && wanted;
                self.conds.push(Cond {
                    parent_active,
                    active,
                    taken: active,
                });
            }
            "elsif" => {
                let target = read_name(text, i)
                    .ok_or_else(|| self.malformed(loc, "elsif", "expected a macro name"))?;
                let defined = self.macros.contains_key(&target);
                let Some(top) = self.conds.last_mut() else {
                    return Err(self.error(
                        loc,
                        ErrorKind::UnbalancedConditional("`elsif without `ifdef".to_string()),
                    ));
                };
                top.active = top.parent_active && !top.taken && defined;
                top.taken |= top.active;
            }
            "else" => {
                let Some(top) = self.conds.last_mut() else {
                    return Err(self.error(
                        loc,
                        ErrorKind::UnbalancedConditional("`else without `ifdef".to_string()),
                    ));
                };
                top.active = top.parent_active && !top.taken;
                top.taken = true;
            }
            "endif" => {
                if self.conds.pop().is_none() {
                    return Err(self.error(
                        loc,
                        ErrorKind::UnbalancedConditional("`endif without `ifdef".to_string()),
                    ));
                }
            }
            "timescale" => {
                let rest = take_line(text, i);
                if emitting {
                    let timescale = Timescale::parse(rest.trim())
                        .map_err(|detail| self.malformed(loc, "timescale", detail))?;
                    self.timescale = Some(timescale);
                }
            }
            "include" => {
                let rest = take_line(text, i).to_string();
                if emitting {
                    // The path may itself be a macro (`` `include `FILE ``), so
                    // expand the rest of the line before reading the string out
                    // of it.
                    let expanded = self.expand_aside(&rest, loc)?;
                    let expanded = expanded.trim();
                    let path = expanded
                        .strip_prefix('"')
                        .and_then(|quoted| quoted.split('"').next())
                        .ok_or_else(|| {
                            self.error(
                                loc,
                                ErrorKind::Include {
                                    path: expanded.to_string(),
                                    detail: "an include path must be a quoted string".to_string(),
                                },
                            )
                        })?
                        .to_string();
                    self.include(&path, loc)?;
                }
            }
            "__FILE__" => {
                if emitting {
                    let quoted = format!("\"{}\"", self.names[loc.file]);
                    self.out.emit(loc, &quoted);
                }
            }
            "__LINE__" => {
                if emitting {
                    let number = loc.line.to_string();
                    self.out.emit(loc, &number);
                }
            }
            _ if IGNORED_DIRECTIVES.contains(&name.as_str()) => {
                take_line(text, i);
            }
            _ => {
                if emitting {
                    self.expand(&name, text, i, line, loc)?;
                }
            }
        }
        Ok(())
    }

    /// Expand a fragment into a buffer of its own rather than into the output.
    /// An `` `include `` path is text the preprocessor has to *read*, not emit.
    fn expand_aside(&mut self, text: &str, loc: Loc) -> Result<String, PreprocessError> {
        let name = self.intern("include");
        let origin = Origin::Macro {
            file: loc.file,
            line: loc.line,
            name,
        };
        let outer = std::mem::take(&mut self.out);
        let result = self.scan(text, origin);
        let aside = std::mem::replace(&mut self.out, outer);
        result.map(|()| aside.text)
    }

    fn include(&mut self, path: &str, loc: Loc) -> Result<(), PreprocessError> {
        let relative = Path::new(path);
        let mut candidates: Vec<PathBuf> = self
            .config
            .include_dirs
            .iter()
            .map(|dir| dir.join(relative))
            .collect();
        candidates.push(relative.to_path_buf());

        let Some(found) = candidates.into_iter().find(|candidate| candidate.is_file()) else {
            return Err(self.error(
                loc,
                ErrorKind::Include {
                    path: path.to_string(),
                    detail: "not found on the include path".to_string(),
                },
            ));
        };
        if self.open_includes.contains(&found) {
            return Err(self.error(
                loc,
                ErrorKind::Include {
                    path: path.to_string(),
                    detail: "include cycle".to_string(),
                },
            ));
        }
        let source = std::fs::read_to_string(&found).map_err(|err| {
            self.error(
                loc,
                ErrorKind::Include {
                    path: path.to_string(),
                    detail: err.to_string(),
                },
            )
        })?;

        let file = self.intern(&found.display().to_string());
        self.open_includes.push(found);
        let result = self.scan(&source, Origin::File(file));
        self.open_includes.pop();
        result
    }

    /// Expand `` `name ``, collecting an argument list first if the macro takes
    /// one, and re-scanning the result so nested macros expand too.
    fn expand(
        &mut self,
        name: &str,
        text: &str,
        i: &mut usize,
        line: &mut usize,
        loc: Loc,
    ) -> Result<(), PreprocessError> {
        let Some(def) = self.macros.get(name).cloned() else {
            return Err(self.error(loc, ErrorKind::UndefinedMacro(name.to_string())));
        };
        if self.expanding.iter().any(|active| active == name) {
            return Err(self.error(loc, ErrorKind::RecursiveMacro(name.to_string())));
        }

        let bad = |detail: String| ErrorKind::BadArguments {
            name: name.to_string(),
            detail,
        };
        let body = match &def.params {
            None => def.body.clone(),
            Some(params) => {
                let supplied =
                    collect_args(text, i, line).map_err(|detail| self.error(loc, bad(detail)))?;
                let bound =
                    bind(params, &supplied).map_err(|detail| self.error(loc, bad(detail)))?;
                substitute(&def.body, params, &bound)
            }
        };

        let name_id = self.intern(name);
        let origin = Origin::Macro {
            file: loc.file,
            line: loc.line,
            // An expansion nested inside another keeps the outer macro's name:
            // the outermost invocation is the position worth reporting.
            name: loc.expansion.unwrap_or(name_id),
        };
        self.expanding.push(name.to_string());
        let result = self.scan(&body, origin);
        self.expanding.pop();
        result
    }
}

// ---------------------------------------------------------------------------
// Lexical helpers
// ---------------------------------------------------------------------------

fn is_ident_start(byte: u8) -> bool {
    byte.is_ascii_alphabetic() || byte == b'_'
}

fn is_ident_continue(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_' || byte == b'$'
}

/// The length of the identifier starting at `from`, or zero if none does.
fn ident_len(bytes: &[u8], from: usize) -> usize {
    if from >= bytes.len() || !is_ident_start(bytes[from]) {
        return 0;
    }
    let mut end = from + 1;
    while end < bytes.len() && is_ident_continue(bytes[end]) {
        end += 1;
    }
    end - from
}

/// The index of the next newline, or the end of input.
fn line_end(bytes: &[u8], from: usize) -> usize {
    bytes[from..]
        .iter()
        .position(|&byte| byte == b'\n')
        .map_or(bytes.len(), |at| from + at)
}

/// The index just past the `*/` closing a block comment opened at `from`.
fn block_end(bytes: &[u8], from: usize) -> Option<usize> {
    let mut at = from + 2;
    while at + 1 < bytes.len() {
        if bytes[at] == b'*' && bytes[at + 1] == b'/' {
            return Some(at + 2);
        }
        at += 1;
    }
    None
}

/// The index just past the closing quote of a string opened at `from`, or the
/// end of the line if it is never closed — an unterminated string is the
/// grammar's problem to report, not the preprocessor's.
fn string_end(bytes: &[u8], from: usize) -> usize {
    let mut at = from + 1;
    while at < bytes.len() {
        match bytes[at] {
            b'\\' if at + 1 < bytes.len() && bytes[at + 1] != b'\n' => at += 2,
            b'"' => return at + 1,
            b'\n' => return at,
            _ => at += 1,
        }
    }
    bytes.len()
}

fn skip_blanks(bytes: &[u8], mut at: usize) -> usize {
    while at < bytes.len() && matches!(bytes[at], b' ' | b'\t' | b'\r') {
        at += 1;
    }
    at
}

fn char_width(text: &str, at: usize) -> usize {
    text[at..].chars().next().map_or(1, char::len_utf8)
}

/// Consume the rest of the physical line, not including its newline, and hand
/// it back. Leaving the newline behind keeps line numbering honest.
fn take_line<'a>(text: &'a str, i: &mut usize) -> &'a str {
    let end = line_end(text.as_bytes(), *i);
    let rest = &text[*i..end];
    *i = end;
    rest
}

/// Consume blanks and then an identifier.
fn read_name(text: &str, i: &mut usize) -> Option<String> {
    let bytes = text.as_bytes();
    let start = skip_blanks(bytes, *i);
    let len = ident_len(bytes, start);
    if len == 0 {
        return None;
    }
    *i = start + len;
    Some(text[start..start + len].to_string())
}

/// Read a `` `define ``: a name, an optional parameter list that must touch the
/// name, and a body running to the end of the logical line.
fn read_define(text: &str, i: &mut usize, line: &mut usize) -> Result<(String, MacroDef), String> {
    let bytes = text.as_bytes();
    let start = skip_blanks(bytes, *i);
    let len = ident_len(bytes, start);
    if len == 0 {
        return Err("expected a macro name".to_string());
    }
    let name = text[start..start + len].to_string();
    let mut at = start + len;

    // The parenthesis has to touch the name: `` `define A (x) `` defines `A` as
    // the text `(x)`, not a macro of one argument.
    let params = if bytes.get(at) == Some(&b'(') {
        let (params, next) = read_params(text, at, line)?;
        at = next;
        Some(params)
    } else {
        None
    };

    let (body, next) = read_body(text, at, line);
    *i = next;
    Ok((name, MacroDef { params, body }))
}

fn read_params(text: &str, from: usize, line: &mut usize) -> Result<(Vec<Param>, usize), String> {
    let bytes = text.as_bytes();
    let mut at = from + 1;
    let mut params = Vec::new();
    let mut current = String::new();
    let mut depth = 0usize;

    loop {
        if at >= bytes.len() {
            return Err("unterminated parameter list".to_string());
        }
        match bytes[at] {
            b'\\' if bytes.get(at + 1) == Some(&b'\n') => {
                *line += 1;
                current.push(' ');
                at += 2;
            }
            b'\n' => {
                *line += 1;
                current.push(' ');
                at += 1;
            }
            b'(' | b'[' | b'{' => {
                depth += 1;
                current.push(bytes[at] as char);
                at += 1;
            }
            b')' if depth == 0 => {
                at += 1;
                break;
            }
            b')' | b']' | b'}' => {
                depth -= 1;
                current.push(bytes[at] as char);
                at += 1;
            }
            b',' if depth == 0 => {
                params.push(parse_param(&current)?);
                current.clear();
                at += 1;
            }
            _ => {
                let width = char_width(text, at);
                current.push_str(&text[at..at + width]);
                at += width;
            }
        }
    }
    if !current.trim().is_empty() || !params.is_empty() {
        params.push(parse_param(&current)?);
    }
    Ok((params, at))
}

fn parse_param(text: &str) -> Result<Param, String> {
    let (name, default) = match text.split_once('=') {
        Some((name, default)) => (name.trim(), Some(default.trim().to_string())),
        None => (text.trim(), None),
    };
    if name.is_empty() {
        return Err("a macro parameter needs a name".to_string());
    }
    Ok(Param {
        name: name.to_string(),
        default,
    })
}

/// A macro body runs to the end of the line, except that a backslash
/// immediately before a newline continues it. The backslash-newline becomes a
/// plain newline, which is what lets a `//` comment inside a body terminate
/// rather than swallow the rest of the macro.
fn read_body(text: &str, from: usize, line: &mut usize) -> (String, usize) {
    let bytes = text.as_bytes();
    let mut at = skip_blanks(bytes, from);
    let mut body = String::new();

    while at < bytes.len() {
        if bytes[at] == b'\n' {
            break;
        }
        if bytes[at] == b'\\' {
            if bytes.get(at + 1) == Some(&b'\n') {
                body.push('\n');
                *line += 1;
                at += 2;
                continue;
            }
            if bytes.get(at + 1) == Some(&b'\r') && bytes.get(at + 2) == Some(&b'\n') {
                body.push('\n');
                *line += 1;
                at += 3;
                continue;
            }
        }
        let width = char_width(text, at);
        body.push_str(&text[at..at + width]);
        at += width;
    }
    (body.trim_end().to_string(), at)
}

/// Collect a function-like macro's argument list, splitting on the commas that
/// are not nested inside brackets, strings or comments.
fn collect_args(text: &str, i: &mut usize, line: &mut usize) -> Result<Vec<String>, String> {
    let bytes = text.as_bytes();
    let mut at = *i;
    while at < bytes.len() && bytes[at].is_ascii_whitespace() {
        if bytes[at] == b'\n' {
            *line += 1;
        }
        at += 1;
    }
    if bytes.get(at) != Some(&b'(') {
        return Err("expected an argument list".to_string());
    }
    at += 1;

    let mut args = Vec::new();
    let mut current = String::new();
    let mut depth = 0usize;

    loop {
        if at >= bytes.len() {
            return Err("unterminated argument list".to_string());
        }
        match bytes[at] {
            b'\n' => {
                *line += 1;
                current.push(' ');
                at += 1;
            }
            b'"' => {
                let end = string_end(bytes, at);
                current.push_str(&text[at..end]);
                at = end;
            }
            b'/' if bytes.get(at + 1) == Some(&b'/') => at = line_end(bytes, at),
            b'/' if bytes.get(at + 1) == Some(&b'*') => {
                let end = block_end(bytes, at).ok_or("unterminated block comment")?;
                *line += bytes[at..end].iter().filter(|&&byte| byte == b'\n').count();
                at = end;
            }
            b'(' | b'[' | b'{' => {
                depth += 1;
                current.push(bytes[at] as char);
                at += 1;
            }
            b')' if depth == 0 => {
                at += 1;
                break;
            }
            b')' | b']' | b'}' => {
                depth -= 1;
                current.push(bytes[at] as char);
                at += 1;
            }
            b',' if depth == 0 => {
                args.push(current.trim().to_string());
                current.clear();
                at += 1;
            }
            _ => {
                let width = char_width(text, at);
                current.push_str(&text[at..at + width]);
                at += width;
            }
        }
    }
    args.push(current.trim().to_string());
    *i = at;
    Ok(args)
}

/// Match the supplied arguments against the declared parameters, filling in
/// defaults for the ones that were left out.
fn bind(params: &[Param], args: &[String]) -> Result<Vec<String>, String> {
    let count = |found: usize| format!("expected {} argument(s), found {}", params.len(), found);
    // `` `M() `` is one empty argument to a one-parameter macro, and none at all
    // to a zero-parameter one.
    let supplied: &[String] = if params.is_empty() && args.len() == 1 && args[0].is_empty() {
        &[]
    } else {
        args
    };
    if supplied.len() > params.len() {
        return Err(count(supplied.len()));
    }

    let mut bound = Vec::with_capacity(params.len());
    for (index, param) in params.iter().enumerate() {
        let arg = supplied.get(index).filter(|arg| !arg.is_empty());
        match (arg, &param.default) {
            (Some(arg), _) => bound.push(arg.clone()),
            (None, Some(default)) => bound.push(default.clone()),
            // An argument that was written but left empty binds to nothing.
            (None, None) if index < supplied.len() => bound.push(String::new()),
            (None, None) => return Err(count(supplied.len())),
        }
    }
    Ok(bound)
}

/// Replace parameter identifiers in a macro body with the bound arguments.
///
/// Skips string literals and comments, and steps over a nested `` `name `` whole
/// so a macro reference is never mistaken for a parameter.
fn substitute(body: &str, params: &[Param], args: &[String]) -> String {
    let bytes = body.as_bytes();
    let mut out = String::with_capacity(body.len());
    let mut at = 0usize;

    while at < bytes.len() {
        match bytes[at] {
            b'`' => {
                if body[at..].starts_with("`\\`\"") {
                    out.push_str("`\\`\"");
                    at += 4;
                } else if matches!(bytes.get(at + 1), Some(&b'`') | Some(&b'"')) {
                    out.push_str(&body[at..at + 2]);
                    at += 2;
                } else {
                    let len = ident_len(bytes, at + 1);
                    out.push_str(&body[at..at + 1 + len]);
                    at += 1 + len;
                }
            }
            b'"' => {
                let end = string_end(bytes, at);
                out.push_str(&body[at..end]);
                at = end;
            }
            b'/' if bytes.get(at + 1) == Some(&b'/') => {
                let end = line_end(bytes, at);
                out.push_str(&body[at..end]);
                at = end;
            }
            b'/' if bytes.get(at + 1) == Some(&b'*') => {
                let end = block_end(bytes, at).unwrap_or(bytes.len());
                out.push_str(&body[at..end]);
                at = end;
            }
            byte if is_ident_start(byte) => {
                let len = ident_len(bytes, at);
                let word = &body[at..at + len];
                match params.iter().position(|param| param.name == word) {
                    Some(index) => out.push_str(&args[index]),
                    None => out.push_str(word),
                }
                at += len;
            }
            _ => {
                let width = char_width(body, at);
                out.push_str(&body[at..at + width]);
                at += width;
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::source::parse_source;

    fn expand(source: &str) -> String {
        Preprocessor::new()
            .preprocess(source, "test.v")
            .expect("source should preprocess")
            .text
    }

    fn error(source: &str) -> PreprocessError {
        Preprocessor::new()
            .preprocess(source, "test.v")
            .expect_err("source should be rejected")
    }

    #[test]
    fn test_source_without_directives_is_untouched() {
        let source = "module m;\n  // a comment\n  assign a = b /* here */ + \"c`d\";\nendmodule\n";
        assert_eq!(expand(source), source);
    }

    #[test]
    fn test_object_like_macro() {
        assert_eq!(expand("`define WIDTH 8\nx = `WIDTH;\n"), "\nx = 8;\n");
    }

    #[test]
    fn test_object_like_macro_with_an_empty_body() {
        assert_eq!(expand("`define NOTHING\na `NOTHING b\n"), "\na  b\n");
    }

    #[test]
    fn test_a_macro_is_expanded_wherever_it_appears() {
        assert_eq!(
            expand("`define ONE 1\n`ONE+`ONE\n"),
            "\n1+1\n",
            "a macro is text substitution, not a statement"
        );
    }

    #[test]
    fn test_function_like_macro() {
        assert_eq!(
            expand("`define sum(a, b) ((a) + (b))\nx = `sum(1, 2);\n"),
            "\nx = ((1) + (2));\n"
        );
    }

    #[test]
    fn test_function_like_macro_arguments_may_nest_and_span_lines() {
        assert_eq!(
            expand("`define pick(a, b) (a)\nx = `pick(f(1, 2),\n  {3, 4});\n"),
            "\nx = (f(1, 2));\n"
        );
    }

    #[test]
    fn test_function_like_macro_argument_defaults() {
        let source = "`define at(x, y = 9) (x + y)\na = `at(1);\nb = `at(1, 2);\n";
        assert_eq!(expand(source), "\na = (1 + 9);\nb = (1 + 2);\n");
    }

    #[test]
    fn test_a_parameter_is_not_substituted_inside_a_string() {
        assert_eq!(
            expand("`define say(x) $display(\"x is %d\", x)\n`say(7)\n"),
            "\n$display(\"x is %d\", 7)\n"
        );
    }

    #[test]
    fn test_a_macro_body_may_invoke_another_macro() {
        let source = "`define INNER 3\n`define OUTER (`INNER + 1)\nx = `OUTER;\n";
        assert_eq!(expand(source), "\n\nx = (3 + 1);\n");
    }

    #[test]
    fn test_wrong_argument_count_is_an_error() {
        let kind = error("`define two(a, b) a\n`two(1)\n").kind;
        assert_eq!(
            kind,
            ErrorKind::BadArguments {
                name: "two".to_string(),
                detail: "expected 2 argument(s), found 1".to_string(),
            }
        );
    }

    #[test]
    fn test_a_function_like_macro_needs_its_argument_list() {
        let kind = error("`define one(a) a\nx = `one;\n").kind;
        assert!(matches!(kind, ErrorKind::BadArguments { .. }));
    }

    #[test]
    fn test_line_continuations_join_a_macro_body() {
        // Each continuation becomes a newline, so the body keeps the shape it
        // was written in — including the one right after `pair`.
        let source = "`define pair \\\n  a = 1; \\\n  b = 2;\n`pair\n";
        assert_eq!(expand(source), "\n\n  a = 1; \n  b = 2;\n");
    }

    /// The smallest interesting file in the ivtest corpus: a continued body
    /// whose second line carries a comment, and the comment carries the
    /// continuation. The backslash-newline has to become a real newline, or the
    /// `//` swallows the rest of the macro.
    #[test]
    fn test_a_comment_inside_a_macro_body_ends_at_its_line() {
        let source = "`define display_passed \\\n  initial begin // comment \\\n    \
                      $display(\"PASSED\"); \\\n  end\n\nmodule test();\n`display_passed\nendmodule\n";
        let expanded = expand(source);
        assert!(
            expanded.contains("$display(\"PASSED\");"),
            "the comment swallowed the body: {:?}",
            expanded
        );

        let parsed = parse_source(source).expect("a macro used in a module body should parse");
        assert_eq!(parsed.modules.len(), 1);
        assert_eq!(parsed.modules[0].identifier, "test".into());
    }

    #[test]
    fn test_a_trailing_line_comment_in_a_body_cannot_escape_the_expansion() {
        assert_eq!(
            expand("`define ONE 1 // note\nx = `ONE + 2;\n"),
            "\nx = 1 // note\n + 2;\n"
        );
    }

    #[test]
    fn test_ifdef_selects_one_arm() {
        let source = "`define A\n`ifdef A\nyes\n`else\nno\n`endif\n";
        assert_eq!(expand(source).trim(), "yes");
    }

    #[test]
    fn test_ifndef_selects_the_other_arm() {
        let source = "`ifndef A\nyes\n`else\nno\n`endif\n";
        assert_eq!(expand(source).trim(), "yes");
    }

    #[test]
    fn test_elsif_chains_are_exclusive() {
        let source = "`define B\n`ifdef A\na\n`elsif B\nb\n`elsif C\nc\n`else\nd\n`endif\n";
        assert_eq!(expand(source).trim(), "b");
    }

    #[test]
    fn test_nested_ifdef() {
        let source = "`define OUTER\n`define INNER\n\
                      `ifdef OUTER\n  `ifdef INNER\n  both\n  `else\n  outer\n  `endif\n\
                      `else\n  neither\n`endif\n";
        assert_eq!(expand(source).trim(), "both");
    }

    #[test]
    fn test_a_nested_ifdef_inside_a_skipped_arm_stays_skipped() {
        let source = "`define INNER\n\
                      `ifdef MISSING\n  `ifdef INNER\n  inner\n  `endif\n  outer\n`endif\nafter\n";
        assert_eq!(
            expand(source).trim(),
            "after",
            "a true `ifdef inside a false one must not re-enable emission"
        );
    }

    #[test]
    fn test_a_directive_inside_a_skipped_arm_is_inert() {
        let source = "`ifdef MISSING\n`define X 1\n`undef Y\n`MISSING_MACRO\n`endif\nx = 2;\n";
        assert_eq!(expand(source).trim(), "x = 2;");
    }

    #[test]
    fn test_undef_removes_a_macro() {
        let source = "`define A\n`undef A\n`ifdef A\nyes\n`else\nno\n`endif\n";
        assert_eq!(expand(source).trim(), "no");
    }

    #[test]
    fn test_undef_makes_a_later_use_an_error() {
        let kind = error("`define A 1\n`undef A\nx = `A;\n").kind;
        assert_eq!(kind, ErrorKind::UndefinedMacro("A".to_string()));
    }

    #[test]
    fn test_unbalanced_conditionals_are_errors() {
        assert!(matches!(
            error("`endif\n").kind,
            ErrorKind::UnbalancedConditional(_)
        ));
        assert!(matches!(
            error("`else\n").kind,
            ErrorKind::UnbalancedConditional(_)
        ));
        assert!(matches!(
            error("`ifdef A\nx\n").kind,
            ErrorKind::UnbalancedConditional(_)
        ));
    }

    #[test]
    fn test_timescale_is_recorded() {
        let result = Preprocessor::new()
            .preprocess("`timescale 1ns / 10ps\nmodule m; endmodule\n", "test.v")
            .unwrap();
        assert_eq!(
            result.timescale,
            Some(Timescale {
                unit: TimeSpec {
                    value: 1,
                    unit: TimeUnit::Nanoseconds
                },
                precision: TimeSpec {
                    value: 10,
                    unit: TimeUnit::Picoseconds
                },
            })
        );
        assert_eq!(result.timescale.unwrap().to_string(), "1ns/10ps");
        assert_eq!(result.text.trim(), "module m; endmodule");
    }

    #[test]
    fn test_timescale_terms_convert_to_femtoseconds() {
        assert_eq!(
            TimeSpec {
                value: 1,
                unit: TimeUnit::Nanoseconds
            }
            .femtoseconds(),
            1_000_000
        );
        assert_eq!(
            TimeSpec {
                value: 100,
                unit: TimeUnit::Femtoseconds
            }
            .femtoseconds(),
            100
        );
    }

    #[test]
    fn test_a_malformed_timescale_is_an_error() {
        assert!(matches!(
            error("`timescale 3ns / 1ps\n").kind,
            ErrorKind::MalformedDirective { .. }
        ));
        assert!(matches!(
            error("`timescale 1ns\n").kind,
            ErrorKind::MalformedDirective { .. }
        ));
        assert!(
            matches!(
                error("`timescale 1ps / 1ns\n").kind,
                ErrorKind::MalformedDirective { .. }
            ),
            "a precision coarser than the unit is not a timescale"
        );
    }

    #[test]
    fn test_an_undefined_macro_is_a_named_error() {
        let error = error("module m;\n  x = `MISSING;\nendmodule\n");
        assert_eq!(error.kind, ErrorKind::UndefinedMacro("MISSING".to_string()));
        assert_eq!(error.at, "test.v:2");
        assert_eq!(error.to_string(), "test.v:2: undefined macro `MISSING");
    }

    #[test]
    fn test_a_recursive_macro_is_an_error_rather_than_a_hang() {
        let error = error("`define LOOP `LOOP\nx = `LOOP;\n");
        assert_eq!(error.kind, ErrorKind::RecursiveMacro("LOOP".to_string()));
    }

    #[test]
    fn test_indirect_recursion_is_an_error_too() {
        let error = error("`define A `B\n`define B `A\nx = `A;\n");
        assert_eq!(error.kind, ErrorKind::RecursiveMacro("A".to_string()));
    }

    #[test]
    fn test_a_stray_backtick_is_an_error() {
        assert!(matches!(
            error("x = ` ;\n").kind,
            ErrorKind::MalformedDirective { .. }
        ));
    }

    #[test]
    fn test_ignored_directives_do_not_fail() {
        let source = "`begin_keywords \"1364-2005\"\n`celldefine\n`resetall\n\
                      `default_nettype none\nmodule m; endmodule\n`endcelldefine\n`end_keywords\n";
        assert_eq!(expand(source).trim(), "module m; endmodule");
    }

    #[test]
    fn test_builtin_file_and_line_macros() {
        assert_eq!(
            expand("a\n$display(`__FILE__, `__LINE__);\n"),
            "a\n$display(\"test.v\", 2);\n"
        );
    }

    #[test]
    fn test_stringification_and_token_pasting() {
        assert_eq!(
            expand("`define name(x) `\"x`\"\ns = `name(hello);\n"),
            "\ns = \"hello\";\n"
        );
        assert_eq!(
            expand("`define join(a, b) a``b\nx = `join(fo, o);\n"),
            "\nx = foo;\n"
        );
    }

    #[test]
    fn test_a_backtick_inside_a_comment_or_string_is_text() {
        assert_eq!(expand("// `MISSING\n"), "// `MISSING\n");
        assert_eq!(expand("/* `MISSING */\n"), "/* `MISSING */\n");
        assert_eq!(expand("s = \"`MISSING\";\n"), "s = \"`MISSING\";\n");
    }

    #[test]
    fn test_source_map_points_at_the_original_line() {
        let source = "`define GREET hello\nmodule m;\n`GREET\nendmodule\n";
        let result = Preprocessor::new().preprocess(source, "greet.v").unwrap();
        assert_eq!(result.text, "\nmodule m;\nhello\nendmodule\n");

        let module = result.text.find("module").unwrap();
        assert_eq!(
            result.map.locate(module),
            Some(Location {
                file: "greet.v",
                line: 2,
                expansion_of: None,
            })
        );

        let expanded = result.text.find("hello").unwrap();
        assert_eq!(
            result.map.locate(expanded),
            Some(Location {
                file: "greet.v",
                line: 3,
                expansion_of: Some("GREET"),
            })
        );
        assert_eq!(
            result.map.describe(expanded),
            "greet.v:3 (expanding `GREET)"
        );

        let endmodule = result.text.find("endmodule").unwrap();
        assert_eq!(result.map.locate(endmodule).unwrap().line, 4);
    }

    #[test]
    fn test_source_map_of_an_empty_file() {
        let result = Preprocessor::new().preprocess("", "empty.v").unwrap();
        assert_eq!(result.map.locate(0), None);
        assert_eq!(result.map.describe(0), "<unknown>");
    }

    #[test]
    fn test_include_reads_a_file_from_the_search_path() {
        let dir = std::env::temp_dir().join("visilog_preprocessor_include");
        std::fs::create_dir_all(&dir).unwrap();
        let included = dir.join("shared.vh");
        std::fs::write(&included, "`define SHARED 42\n").unwrap();

        let result = Preprocessor::new()
            .with_include_dir(&dir)
            .preprocess("`include \"shared.vh\"\nx = `SHARED;\n", "test.v")
            .unwrap();
        assert_eq!(result.text.trim(), "x = 42;");

        std::fs::remove_file(&included).unwrap();
    }

    #[test]
    fn test_a_missing_include_is_a_named_error() {
        let error = error("`include \"nowhere.vh\"\n");
        assert_eq!(
            error.kind,
            ErrorKind::Include {
                path: "nowhere.vh".to_string(),
                detail: "not found on the include path".to_string(),
            }
        );
    }

    #[test]
    fn test_an_include_path_may_come_from_a_macro() {
        let dir = std::env::temp_dir().join("visilog_preprocessor_macro_include");
        std::fs::create_dir_all(&dir).unwrap();
        let included = dir.join("named.vh");
        std::fs::write(&included, "`define NAMED 7\n").unwrap();

        let result = Preprocessor::new()
            .with_include_dir(&dir)
            .preprocess(
                "`define FILE \"named.vh\"\n`include `FILE\nx = `NAMED;\n",
                "test.v",
            )
            .unwrap();
        assert_eq!(result.text.trim(), "x = 7;");

        std::fs::remove_file(&included).unwrap();
    }

    #[test]
    fn test_an_unquoted_include_path_is_rejected_by_name() {
        assert_eq!(
            error("`include nowhere.vh\n").kind,
            ErrorKind::Include {
                path: "nowhere.vh".to_string(),
                detail: "an include path must be a quoted string".to_string(),
            }
        );
    }

    #[test]
    fn test_an_escaped_identifier_is_not_scanned_for_directives() {
        let source = "module m;\n  reg \\`~!-=+|[]{} ;\nendmodule\n";
        assert_eq!(expand(source), source);
    }

    #[test]
    fn test_a_define_whose_paren_is_detached_is_object_like() {
        assert_eq!(
            expand("`define A (x)\ny = `A;\n"),
            "\ny = (x);\n",
            "a space before the paren makes it part of the body"
        );
    }

    #[test]
    fn test_a_macro_defined_with_no_parameters_takes_none() {
        assert_eq!(expand("`define f() 1\nx = `f();\n"), "\nx = 1;\n");
    }
}

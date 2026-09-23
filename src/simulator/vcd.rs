//! The value change dump — `$dumpfile`, `$dumpvars` and the rest of the family.
//!
//! A VCD file is the one artifact this simulator produces for a human rather
//! than for a test: a waveform viewer reads it, and everything a design did
//! over its whole run is in it. The format is IEEE 1364 §18, and every detail
//! here was measured against iverilog 12.0 rather than read off the standard —
//! the header fields, the identifier alphabet, how a vector's leading digits
//! are trimmed and what an `x` or a `z` looks like in each place.
//!
//! **The dump costs the signals that moved, not the signals that exist.** The
//! [`StateStore`] already journals every write, and
//! [`Simulator::settle`](crate::simulator::runner::Simulator) takes that
//! journal once per delta cycle; [`VcdDump::note_changes`] is handed the same
//! list, so a timestep costs one hash lookup per *written* name. A dumper that
//! walked the design once a timestep would be the same cost as the simulation
//! it is recording.
//!
//! **A value change is written at the end of a timestep**, from
//! [`Simulator::end_of_timestep`](crate::simulator::runner::Simulator) — the
//! moment the design has stopped moving, which is where `$strobe` and
//! `$monitor` already report. That is also what makes `a = 1; a = 0; a = 1;` in
//! one timestep a single line rather than three, which is what iverilog writes.

use std::collections::{BTreeMap, HashMap};
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
use std::time::{SystemTime, UNIX_EPOCH};

use crate::parsers::preprocessor::TimeSpec;
use crate::register::{Register, ONE, X, Z, ZERO};
use crate::simulator::state_store::StateStore;

/// What `$version` says. A viewer shows it; nothing reads it.
const VERSION: &str = "visilog";

/// The `$timescale` of a design that declared none.
///
/// It is what iverilog writes for one, and it is honest here for the same
/// reason: with no `` `timescale `` in the source a tick is just a tick.
const DEFAULT_TIMESCALE: &str = "1s";

/// The file `$dumpvars` writes when the design never named one, which is the
/// name iverilog 12.0 uses.
const DEFAULT_DUMPFILE: &str = "dump.vcd";

/// The printable ASCII run VCD identifiers are drawn from: `!` (33) through
/// `~` (126).
const ALPHABET: u8 = 94;

/// What a variable is declared as in the header.
///
/// The store keeps a *declared* net flag beside each signal, so this is read
/// off the declaration rather than guessed from the value.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum VarKind {
    Wire,
    Reg,
    Real,
}

impl VarKind {
    fn keyword(self) -> &'static str {
        match self {
            VarKind::Wire => "wire",
            VarKind::Reg => "reg",
            VarKind::Real => "real",
        }
    }
}

/// Where a dumped variable reads its value from.
///
/// A memory word is the one that is not a plain signal: `$dumpvars(1, arr[4])`
/// names one word of an array, which lives in the store's memory map and not
/// in its signal map — the same split that tells `m[3]` from `a[3]` everywhere
/// else.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Source {
    Signal(String),
    Word(String, i64),
}

impl Source {
    /// The store entry that has to move for this variable to be worth
    /// re-reading, which is what the change journal is matched against.
    fn key(&self) -> &str {
        match self {
            Source::Signal(name) | Source::Word(name, _) => name,
        }
    }

    fn value(&self, store: &StateStore) -> Option<Register> {
        match self {
            Source::Signal(name) => store.get(name).cloned(),
            Source::Word(name, address) => store
                .memory(name)
                .map(|memory| memory.word(Some(std::slice::from_ref(address)))),
        }
    }
}

/// One variable in the dump: where it reads from, how it is declared, and what
/// it last reported.
#[derive(Clone, Debug)]
struct Var {
    source: Source,
    /// The scopes it sits under, outermost first, and the name inside them.
    scopes: Vec<String>,
    name: String,
    kind: VarKind,
    width: usize,
    /// The declared `(msb, lsb)`, written after the name for a vector.
    range: (i64, i64),
    id: String,
    /// Whether this variable only *names* a store entry another one already
    /// dumps — an instance port aliased onto its parent's signal. It shares
    /// that variable's identifier, which is what iverilog does with one, so it
    /// is declared in the header and never writes a value of its own.
    mirrors: bool,
    /// The value last written to the file. `None` until the first one is.
    last: Option<Register>,
}

/// What one `$dumpvars` argument names: a scope or a variable by name, or one
/// word of a memory.
///
/// The two are told apart by the *shape* of the argument — `arr[4]` is a
/// select and `top.u1` is not — because nothing else can: the store is what
/// says whether a name is a memory, and it is asked when the target is
/// resolved.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DumpTarget {
    Name(String),
    Word(String, i64),
}

/// A `$dumpon` / `$dumpoff` / `$dumpall` made during the current timestep.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Control {
    On,
    Off,
    All,
}

/// The whole state of a design's waveform dump.
///
/// The *file* is deliberately not here: it is opened through the
/// [`StateStore`]'s file table, the same one `$fopen` uses, so a relative name
/// lands wherever
/// [`Simulator::set_output_directory`](crate::simulator::runner::Simulator::set_output_directory)
/// says and a corpus run does not scatter `.vcd` files through the repository.
/// What is kept here is a descriptor, which is a number.
#[derive(Clone, Debug, Default)]
pub struct VcdDump {
    /// The name `$dumpfile` gave, before it is resolved against the output
    /// directory. `None` until one is named.
    file_name: Option<String>,
    /// The descriptor the file was opened on, once `$dumpvars` has opened it.
    descriptor: Option<u32>,
    vars: Vec<Var>,
    /// Store entry → the variables that read it. This is what makes a timestep
    /// cost the changes rather than the design.
    watched: HashMap<String, Vec<usize>>,
    /// The variables whose store entry has been written since the last flush.
    pending: Vec<usize>,
    /// Whether a variable is already in `pending`, indexed like `vars`. A flag
    /// per variable rather than a set, since a busy timestep writes the same
    /// name many times.
    dirty: Vec<bool>,
    /// Whether `$dumpvars` has been called at all.
    armed: bool,
    /// Whether the header and the opening `$dumpvars` block have been written.
    /// Once they have, a further `$dumpvars` is ignored the way iverilog
    /// ignores one.
    started: bool,
    /// Whether `$dumpoff` has suspended recording.
    enabled: bool,
    /// The `$dumpall`s, `$dumpon`s and `$dumpoff`s made in the current
    /// timestep, in call order.
    controls: Vec<Control>,
    /// `$dumplimit` — how many bytes the file may reach.
    limit: Option<u64>,
    written: u64,
    /// Whether the limit stopped the dump, which is permanent.
    stopped: bool,
    /// What `$timescale` says.
    timescale: String,
    /// The last `#<time>` written, so a timestep that changes nothing writes
    /// no marker and the final time is not written twice.
    marked: Option<i64>,
    /// How many identifiers have been handed out. Not the same as the number
    /// of variables: two names for one store entry share one.
    ids: usize,
}

impl VcdDump {
    /// A dump that has been asked for but has recorded nothing yet.
    ///
    /// `clock` is one tick of the simulation clock, which is what every
    /// `#<time>` in the file counts — so it is what `$timescale` states.
    pub fn new(clock: Option<TimeSpec>) -> Self {
        VcdDump {
            enabled: true,
            timescale: match clock {
                Some(clock) => clock.to_string(),
                None => DEFAULT_TIMESCALE.to_string(),
            },
            ..VcdDump::default()
        }
    }

    /// `$dumpfile("name.vcd")`.
    ///
    /// A second one, or one made after `$dumpvars` has already opened the file,
    /// keeps the file that is open — which is what iverilog does with one, and
    /// it warns rather than stopping the design.
    pub fn set_file(&mut self, name: &str) {
        if self.descriptor.is_none() {
            self.file_name = Some(name.to_string());
        }
    }

    /// Whether anything is being recorded, which is what puts the end of a
    /// timestep on the dumper's path at all.
    pub fn is_active(&self) -> bool {
        self.armed && !self.stopped
    }

    /// Adds a variable, unless its store entry is already in the dump — a
    /// signal named twice is dumped once, the way iverilog skips a duplicate.
    fn declare(&mut self, source: Source, scopes: Vec<String>, name: String, store: &StateStore) {
        if self
            .vars
            .iter()
            .any(|var| var.source.key() == source.key() && var.name == name)
        {
            return;
        }
        let Some(value) = source.value(store) else {
            return;
        };
        let (kind, range) = match &source {
            Source::Signal(signal) => {
                let state = store
                    .get_signal(signal)
                    .expect("a signal with a value is declared");
                let kind = if state.is_real() {
                    VarKind::Real
                } else if state.is_net() {
                    VarKind::Wire
                } else {
                    VarKind::Reg
                };
                (kind, state.range())
            }
            Source::Word(memory, _) => {
                let declared = store
                    .memory(memory)
                    .expect("a memory word with a value is declared");
                let kind = if declared.is_real() {
                    VarKind::Real
                } else {
                    VarKind::Reg
                };
                (kind, declared.range())
            }
        };
        // A port bound to a parent's signal is *one* store entry under two
        // names, so the second name is declared against the first one's
        // identifier and writes nothing itself. That is what iverilog does,
        // and it is what keeps one transition from writing two lines. Two
        // *words* of one memory share a store entry and are not that: they
        // move independently, so the test is the whole source and not its key.
        let mirrored = self.vars.iter().position(|var| var.source == source);
        let index = self.vars.len();
        let id = match mirrored {
            Some(first) => self.vars[first].id.clone(),
            None => {
                self.watched
                    .entry(source.key().to_string())
                    .or_default()
                    .push(index);
                self.ids += 1;
                identifier(self.ids - 1)
            }
        };
        self.vars.push(Var {
            source,
            scopes,
            name,
            kind,
            width: value.width(),
            range,
            id,
            mirrors: mirrored.is_some(),
            last: None,
        });
        self.dirty.push(false);
        self.armed = true;
    }

    /// Marks the dump as asked for even when the scope named holds no
    /// variables, so an empty `$dumpvars` still writes a header.
    pub fn arm(&mut self) {
        self.armed = true;
    }

    /// `$dumpvars(levels, scope, …)` — adds what the arguments name.
    ///
    /// `levels` is how far below each named scope to descend, and `0` is all
    /// the way down. With no target at all the whole design is dumped, which
    /// is what a bare `$dumpvars;` asks for.
    ///
    /// A target that names neither a scope, a signal nor a memory word is an
    /// error carrying the name: a design that asked for a signal and silently
    /// got none would produce a waveform that looks exactly like one where the
    /// signal never moved.
    pub fn add(
        &mut self,
        levels: usize,
        targets: &[DumpTarget],
        top: &str,
        aliases: &HashMap<String, String>,
        store: &StateStore,
    ) -> Result<(), String> {
        self.arm();
        if targets.is_empty() {
            self.add_scope("", levels, top, aliases, store);
            return Ok(());
        }
        for target in targets {
            match target {
                DumpTarget::Word(name, address) => {
                    let name = relative(name, top);
                    match store.memory(&name) {
                        None => return Err(format!("`{}` is not a memory", name)),
                        // One index names a word only of a one-dimensional
                        // array; anything else would dump a word that reads
                        // `x` for ever rather than the one the design meant.
                        Some(memory) if memory.dimensions() != 1 => {
                            return Err(format!(
                                "`{}` has {} dimensions, so one index names no word of it",
                                name,
                                memory.dimensions()
                            ))
                        }
                        Some(_) => {}
                    }
                    let (scopes, leaf) = split(&name, top);
                    self.declare(
                        Source::Word(name, *address),
                        scopes,
                        format!("\\{}[{}]", leaf, address),
                        store,
                    );
                }
                DumpTarget::Name(name) => {
                    let relative = relative(name, top);
                    if relative.is_empty() {
                        self.add_scope("", levels, top, aliases, store);
                    } else if let Some(entry) = resolved(&relative, aliases, store) {
                        let (scopes, leaf) = split(&relative, top);
                        self.declare(Source::Signal(entry), scopes, leaf, store);
                    } else if store.memory(&relative).is_some() {
                        return Err(format!(
                            "`{}` is a memory, and only a word of one can be dumped",
                            name
                        ));
                    } else {
                        let prefix = format!("{}.", relative);
                        let known = store.names().iter().any(|entry| entry.starts_with(&prefix))
                            || aliases.keys().any(|entry| entry.starts_with(&prefix));
                        if !known {
                            return Err(format!("`{}` names no scope or variable", name));
                        }
                        self.add_scope(&prefix, levels, top, aliases, store);
                    }
                }
            }
        }
        Ok(())
    }

    /// Every signal under `prefix`, down `levels` scopes — all of them when
    /// `levels` is zero.
    ///
    /// The store's names come back sorted, so the header a design writes is
    /// the same on every run.
    fn add_scope(
        &mut self,
        prefix: &str,
        levels: usize,
        top: &str,
        aliases: &HashMap<String, String>,
        store: &StateStore,
    ) {
        let mut names: Vec<String> = store
            .names()
            .into_iter()
            .map(str::to_string)
            .chain(aliases.keys().cloned())
            .filter(|name| name.starts_with(prefix))
            .collect();
        names.sort();
        for name in names {
            let inside = &name[prefix.len()..];
            // A hidden slot — a `repeat` counter, an intra-assignment hold —
            // is not a variable the design can name, so it is not one a
            // waveform should show.
            if inside.starts_with('$') || inside.contains(".$") {
                continue;
            }
            let depth = inside.split('.').count();
            if levels != 0 && depth > levels {
                continue;
            }
            let Some(entry) = resolved(&name, aliases, store) else {
                continue;
            };
            let (scopes, leaf) = split(&name, top);
            self.declare(Source::Signal(entry), scopes, leaf, store);
        }
    }

    /// Whether the header has been written, which is what makes a further
    /// `$dumpvars` too late to add anything.
    pub fn is_started(&self) -> bool {
        self.started
    }

    /// `$dumplimit(size)`.
    pub fn set_limit(&mut self, bytes: u64) {
        self.limit = Some(bytes);
    }

    /// Queues a `$dumpon` / `$dumpoff` / `$dumpall` for the end of the
    /// timestep it was called in.
    ///
    /// It is queued rather than written because the values it reports have to
    /// be the ones the timestep settled on: a `$dumpall` that ran halfway
    /// through a block would report what the design was passing through.
    pub fn control(&mut self, control: Control) {
        self.controls.push(control);
    }

    /// Notes the names written since the last round — what
    /// [`StateStore::take_changes`] handed over.
    ///
    /// This is the whole of the dumper's per-timestep cost, and it is one hash
    /// lookup per name *written*.
    pub fn note_changes<'a>(&mut self, names: impl Iterator<Item = &'a str>) {
        for name in names {
            let Some(watchers) = self.watched.get(name) else {
                continue;
            };
            for &index in watchers {
                if !self.dirty[index] {
                    self.dirty[index] = true;
                    self.pending.push(index);
                }
            }
        }
    }

    /// Writes everything the timestep owes: the header and the opening
    /// `$dumpvars` block the first time, then one section per timestep in
    /// which a dumped variable moved.
    ///
    /// Returns what the dump printed for the design's output buffer — the
    /// `VCD info:` line iverilog prints when it opens the file, and the
    /// warning it prints when `$dumplimit` is reached.
    pub fn flush(&mut self, store: &StateStore, time: i64) -> String {
        let mut printed = String::new();
        if !self.is_active() {
            self.controls.clear();
            return printed;
        }
        if !self.started {
            self.write_header(store);
            self.mark(store, time);
            self.write_block("$dumpvars", store);
            self.started = true;
            self.clear_pending();
        } else if self.enabled {
            self.write_changes(store, time);
        } else {
            self.clear_pending();
        }

        for control in std::mem::take(&mut self.controls) {
            match control {
                Control::Off if self.enabled => {
                    self.enabled = false;
                    self.mark(store, time);
                    self.write_unknowns(store);
                }
                Control::On if !self.enabled => {
                    self.enabled = true;
                    self.mark(store, time);
                    self.write_block("$dumpon", store);
                }
                Control::All => {
                    self.mark(store, time);
                    self.write_block("$dumpall", store);
                }
                _ => {}
            }
        }
        printed.push_str(&self.report_limit(store));
        printed
    }

    /// Writes the final `#<time>`, which is how a viewer knows how long the
    /// design ran after its last transition. iverilog writes one when the
    /// design calls `$finish`.
    pub fn close(&mut self, store: &StateStore, time: i64) {
        if !self.started || self.stopped {
            return;
        }
        self.mark(store, time);
        store.flush_channels(self.descriptor);
    }

    /// `$dumpflush` — pushes what is buffered out to the file.
    pub fn flush_file(&self, store: &StateStore) {
        if self.descriptor.is_some() {
            store.flush_channels(self.descriptor);
        }
    }

    /// Opens the file, which happens at the first `$dumpvars` rather than at
    /// `$dumpfile`: a design that names a file and never dumps anything writes
    /// no file at all, which is what iverilog does.
    ///
    /// It reports at the moment `$dumpvars` runs rather than at the end of the
    /// timestep, because iverilog's `VCD info:` line lands *before* whatever
    /// the rest of the block goes on to print and seven corpus gold files say
    /// so.
    pub fn open(&mut self, store: &StateStore) -> String {
        if self.descriptor.is_some() {
            return String::new();
        }
        let name = self
            .file_name
            .clone()
            .unwrap_or_else(|| DEFAULT_DUMPFILE.to_string());
        let descriptor = store.open_descriptor(&name, "w");
        if descriptor == 0 {
            return format!("VCD Error: unable to open {} for output.\n", name);
        }
        self.descriptor = Some(descriptor);
        format!("VCD info: dumpfile {} opened for output.\n", name)
    }

    fn write(&mut self, store: &StateStore, text: &str) {
        if self.stopped {
            return;
        }
        if let Some(descriptor) = self.descriptor {
            store.write_channels(descriptor, text);
            self.written += text.len() as u64;
        }
    }

    /// Stops the dump once `$dumplimit` is reached, saying so in the file and
    /// in the design's output the way iverilog says it.
    fn report_limit(&mut self, store: &StateStore) -> String {
        let Some(limit) = self.limit else {
            return String::new();
        };
        if self.stopped || self.written <= limit {
            return String::new();
        }
        self.write(
            store,
            &format!(
                "$comment Dump file limit ({} bytes) exceeded. $end\n",
                limit
            ),
        );
        store.flush_channels(self.descriptor);
        self.stopped = true;
        format!("WARNING: Dump file limit ({} bytes) exceeded.\n", limit)
    }

    fn write_header(&mut self, store: &StateStore) {
        let header = format!(
            "$date\n\t{}\n$end\n$version\n\t{}\n$end\n$timescale\n\t{}\n$end\n",
            now(),
            VERSION,
            self.timescale
        );
        self.write(store, &header);
        self.write_scopes(store);
        self.write(store, "$enddefinitions $end\n");
    }

    /// The `$scope` / `$var` / `$upscope` tree.
    ///
    /// The variables are in the order they were declared in, which for a
    /// `$dumpvars` over a scope is the store's own sorted order — so the tree
    /// is deterministic and a design dumps the same file on every run.
    fn write_scopes(&mut self, store: &StateStore) {
        // Grouping by scope path before walking is what puts a scope's own
        // variables ahead of the scopes nested in it: a `BTreeMap` keyed by the
        // path orders a prefix before what extends it, which is exactly a
        // pre-order walk of the tree.
        let mut tree: BTreeMap<Vec<String>, Vec<usize>> = BTreeMap::new();
        for (index, var) in self.vars.iter().enumerate() {
            tree.entry(var.scopes.clone()).or_default().push(index);
        }
        let mut open: Vec<String> = Vec::new();
        let mut text = String::new();
        for (scopes, indices) in tree {
            let shared = open
                .iter()
                .zip(scopes.iter())
                .take_while(|(a, b)| a == b)
                .count();
            for _ in shared..open.len() {
                text.push_str("$upscope $end\n");
            }
            open.truncate(shared);
            for scope in &scopes[shared..] {
                text.push_str(&format!("$scope module {} $end\n", scope));
                open.push(scope.clone());
            }
            for index in indices {
                text.push_str(&self.declaration(index));
            }
        }
        for _ in 0..open.len() {
            text.push_str("$upscope $end\n");
        }
        self.write(store, &text);
    }

    /// One `$var` line. A vector carries its declared range after the name,
    /// which is how a viewer knows `[7:0]` from `[0:7]`.
    fn declaration(&self, index: usize) -> String {
        let var = &self.vars[index];
        let range = if var.width > 1 && var.kind != VarKind::Real {
            format!(" [{}:{}]", var.range.0, var.range.1)
        } else {
            String::new()
        };
        // A real is one *value*, not sixty-four bits, so its declared size is
        // 1 — which is what iverilog writes for one.
        let size = if var.kind == VarKind::Real {
            1
        } else {
            var.width
        };
        format!(
            "$var {} {} {} {}{} $end\n",
            var.kind.keyword(),
            size,
            var.id,
            var.name,
            range
        )
    }

    /// A `$dumpvars` / `$dumpall` / `$dumpon` block: every variable's value
    /// right now, between the keyword and its `$end`.
    fn write_block(&mut self, keyword: &str, store: &StateStore) {
        let mut text = String::new();
        text.push_str(keyword);
        text.push('\n');
        for index in 0..self.vars.len() {
            if self.vars[index].mirrors {
                continue;
            }
            let Some(value) = self.vars[index].source.value(store) else {
                continue;
            };
            text.push_str(&rendered(&value, &self.vars[index].id));
            self.vars[index].last = Some(value);
        }
        text.push_str("$end\n");
        self.write(store, &text);
    }

    /// The `$dumpoff` block: every variable reads `x` while the dump is
    /// suspended, so a viewer shows a gap rather than a value that is no
    /// longer being recorded.
    fn write_unknowns(&mut self, store: &StateStore) {
        let mut text = String::from("$dumpoff\n");
        for var in self.vars.iter().filter(|var| !var.mirrors) {
            let unknown = if var.kind == VarKind::Real {
                format!("rNaN {}\n", var.id)
            } else if var.width == 1 {
                format!("x{}\n", var.id)
            } else {
                format!("bx {}\n", var.id)
            };
            text.push_str(&unknown);
        }
        text.push_str("$end\n");
        self.write(store, &text);
    }

    /// One timestep's changes: the variables whose store entry moved and whose
    /// value really differs from what was last written.
    fn write_changes(&mut self, store: &StateStore, time: i64) {
        let mut text = String::new();
        for &index in &self.pending {
            self.dirty[index] = false;
            let Some(value) = self.vars[index].source.value(store) else {
                continue;
            };
            if self.vars[index].last.as_ref() == Some(&value) {
                continue;
            }
            text.push_str(&rendered(&value, &self.vars[index].id));
            self.vars[index].last = Some(value);
        }
        self.pending.clear();
        if text.is_empty() {
            return;
        }
        self.mark(store, time);
        self.write(store, &text);
    }

    fn clear_pending(&mut self) {
        for &index in &self.pending {
            self.dirty[index] = false;
        }
        self.pending.clear();
    }

    /// Writes `#<time>` unless it is already the section being written into.
    fn mark(&mut self, store: &StateStore, time: i64) {
        if self.marked == Some(time) {
            return;
        }
        self.marked = Some(time);
        self.write(store, &format!("#{}\n", time));
    }
}

/// The store entry a name reads, following the alias an instance port is.
///
/// A port bound to a plain identifier has no entry of its own — it *is* the
/// parent's signal — so its qualified name is only ever in the alias table.
fn resolved(name: &str, aliases: &HashMap<String, String>, store: &StateStore) -> Option<String> {
    if store.contains(name) {
        return Some(name.to_string());
    }
    aliases
        .get(name)
        .filter(|entry| store.contains(entry.as_str()))
        .cloned()
}

/// A hierarchical name as the flat store spells it.
///
/// The top module is the root of the store's name space and carries no prefix,
/// so `top.u1.count` and `u1.count` are the same entry and `top` alone is the
/// whole design — an empty prefix.
fn relative(name: &str, top: &str) -> String {
    if name == top {
        return String::new();
    }
    match name.strip_prefix(&format!("{}.", top)) {
        Some(rest) => rest.to_string(),
        None => name.to_string(),
    }
}

/// A store name split into the scopes it sits under — the top module first,
/// which the flat name does not carry — and the name inside them.
fn split(name: &str, top: &str) -> (Vec<String>, String) {
    let mut segments: Vec<String> = name.split('.').map(str::to_string).collect();
    let leaf = segments.pop().unwrap_or_default();
    let mut scopes = vec![top.to_string()];
    scopes.append(&mut segments);
    (scopes, leaf)
}

/// How a value is written: `1!` for a scalar, `b1010 !` for a vector, `r1.5 !`
/// for a real.
fn rendered(value: &Register, id: &str) -> String {
    if value.is_real() {
        return format!("r{} {}\n", value.to_f64(), id);
    }
    if value.width() == 1 {
        return format!("{}{}\n", digit(value.bit_from_lsb(0).unwrap_or(X)), id);
    }
    format!("b{} {}\n", trimmed(&value.to_binary()), id)
}

fn digit(code: u8) -> char {
    match code {
        ZERO => '0',
        ONE => '1',
        Z => 'z',
        _ => 'x',
    }
}

/// A vector's leading digits, trimmed the way IEEE 1364 §18.2.1 allows and
/// iverilog 12.0 does it.
///
/// A reader left-extends the value it is given: with `0` when the leading digit
/// is `0` or `1`, and with the digit itself when it is `x` or `z`. So a run of
/// leading `0`s can go entirely — unless the digit under it is an `x` or a `z`,
/// where one `0` has to stay or the reader would extend the unknown — and a run
/// of leading `x`s or `z`s collapses to one. A leading `1` is never dropped:
/// `1` is not the extension digit for anything.
fn trimmed(bits: &str) -> &str {
    let mut characters = bits.char_indices();
    let Some((_, leading)) = characters.next() else {
        return bits;
    };
    if leading == '1' {
        return bits;
    }
    let first = characters
        .find(|(_, digit)| *digit != leading)
        .map(|(index, digit)| (index, digit));
    match first {
        // Every digit is the same one, so one of it says all of it.
        None => &bits[..1],
        // A run of unknowns extends itself; keeping one is enough.
        Some((index, _)) if leading != '0' => &bits[index - 1..],
        // A `0` in front of an unknown is load bearing: dropping it would let
        // the unknown extend over the whole value.
        Some((index, digit)) if digit == 'x' || digit == 'z' => &bits[index - 1..],
        Some((index, _)) => &bits[index..],
    }
}

/// The identifier code for the `index`th variable: base 94 over the printable
/// ASCII run starting at `!`, least significant digit first — which is what
/// iverilog hands out.
fn identifier(index: usize) -> String {
    let mut code = String::new();
    let mut left = index;
    loop {
        code.push((b'!' + (left % ALPHABET as usize) as u8) as char);
        left /= ALPHABET as usize;
        if left == 0 {
            return code;
        }
    }
}

/// What `$date` says on a target with no clock: `SystemTime::now()` panics on
/// `wasm32-unknown-unknown` rather than returning an error.
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
fn now() -> String {
    "unknown".to_string()
}

/// What `$date` says: the wall clock, as an ISO 8601 instant in UTC.
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
fn now() -> String {
    let Ok(elapsed) = SystemTime::now().duration_since(UNIX_EPOCH) else {
        return "unknown".to_string();
    };
    let seconds = elapsed.as_secs();
    let (days, rest) = (seconds / 86_400, seconds % 86_400);
    let (year, month, day) = civil_from_days(days as i64);
    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}Z",
        year,
        month,
        day,
        rest / 3600,
        (rest % 3600) / 60,
        rest % 60
    )
}

/// Howard Hinnant's `civil_from_days`: a day number since 1970-01-01 to a
/// calendar date, with no dependency behind it.
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
fn civil_from_days(days: i64) -> (i64, u32, u32) {
    let shifted = days + 719_468;
    let era = shifted.div_euclid(146_097);
    let day_of_era = shifted.rem_euclid(146_097);
    let year_of_era =
        (day_of_era - day_of_era / 1460 + day_of_era / 36_524 - day_of_era / 146_096) / 365;
    let year = year_of_era + era * 400;
    let day_of_year = day_of_era - (365 * year_of_era + year_of_era / 4 - year_of_era / 100);
    let month_position = (5 * day_of_year + 2) / 153;
    let day = (day_of_year - (153 * month_position + 2) / 5 + 1) as u32;
    let month = if month_position < 10 {
        month_position + 3
    } else {
        month_position - 9
    } as u32;
    (year + i64::from(month <= 2), month, day)
}

#[cfg(test)]
mod tests {
    use crate::parsers::source::parse_source;
    use crate::simulator::runner::Simulator;
    use std::path::PathBuf;

    /// Runs a design in a scratch directory of its own and hands back what it
    /// printed and the waveform it wrote, with the `$date` line — the one part
    /// of a VCD that cannot be the same twice — replaced.
    fn dump_of(name: &str, source: &str, run_for: i64) -> (String, String) {
        let directory: PathBuf = std::env::temp_dir().join(format!("visilog-vcd-{}", name));
        let _ = std::fs::remove_dir_all(&directory);
        std::fs::create_dir_all(&directory).expect("scratch directory");

        let parsed = parse_source(source).expect("source should parse");
        let top = parsed.modules[0].identifier.name.clone();
        let mut simulator = Simulator::with_modules(parsed.modules, top);
        simulator.set_output_directory(directory.clone());
        simulator.setup().expect("design should elaborate");
        simulator.advance(run_for).expect("design should run");

        let written = std::fs::read_to_string(directory.join(format!("{}.vcd", name)))
            .expect("the design should have written a waveform");
        let text = written
            .lines()
            .enumerate()
            .map(|(line, text)| if line == 1 { "\t<date>" } else { text })
            .collect::<Vec<_>>()
            .join("\n");
        (simulator.output().text().to_string(), text + "\n")
    }

    /// The format itself, pinned as text: a header, a scope tree, an opening
    /// `$dumpvars` block and then one section per timestep that moved
    /// something. The clock counts the finest precision declared, so
    /// `` `timescale 1ns / 1ps `` is `$timescale 1ps` and `#5` is `#5000`,
    /// which is what iverilog 12.0 writes for the same design.
    #[test]
    fn test_a_design_writes_a_readable_value_change_dump() {
        let (printed, dump) = dump_of(
            "wave",
            r#"
`timescale 1ns / 1ps
module wave;
  reg clk;
  reg [3:0] count;
  wire [3:0] doubled;
  sub u1 (clk);
  assign doubled = count << 1;
  initial begin
    $dumpfile("wave.vcd");
    $dumpvars(0, wave);
    clk = 0;
    count = 4'b0011;
    #5 clk = 1;
    #5 clk = 0;
    count = 4'bxx01;
    #5 $finish;
  end
endmodule
module sub (input a);
  reg b;
  initial b = 1;
endmodule
"#,
            100_000,
        );

        assert_eq!(printed, "VCD info: dumpfile wave.vcd opened for output.\n");
        assert_eq!(
            dump,
            "\
$date
\t<date>
$end
$version
\tvisilog
$end
$timescale
\t1ps
$end
$scope module wave $end
$var reg 1 ! clk $end
$var reg 4 \" count [3:0] $end
$var wire 4 # doubled [3:0] $end
$scope module u1 $end
$var reg 1 ! a $end
$var reg 1 $ b $end
$upscope $end
$upscope $end
$enddefinitions $end
#0
$dumpvars
0!
b11 \"
b110 #
1$
$end
#5000
1!
#10000
0!
bx01 \"
bx010 #
#15000
"
        );
    }

    /// Everything after `$enddefinitions`: the part of a dump that records
    /// what the design did.
    fn body(dump: &str) -> &str {
        let start = dump
            .find("$enddefinitions $end\n")
            .expect("a dump has an end to its definitions");
        &dump[start + "$enddefinitions $end\n".len()..]
    }

    /// `$dumpoff` writes every variable as unknown, time moving while it is
    /// off leaves no section at all, `$dumpon` writes every value again, and
    /// `$dumpall` is a full snapshot — the shape iverilog 12.0 writes for the
    /// same design, down to the `bx` of a vector and the `rNaN` of a real.
    #[test]
    fn test_dumpoff_dumpon_and_dumpall_write_whole_blocks() {
        let (_, dump) = dump_of(
            "control",
            r#"
module control;
  reg a;
  reg [7:0] v;
  real r;
  initial begin
    $dumpfile("control.vcd");
    $dumpvars;
    a = 0; v = 8'h0f; r = 1.5;
    #5 $dumpoff;
    #5 a = 1; v = 8'd200;
    #5 $dumpon;
    #5 a = 0;
    #5 $dumpall;
    #5 $finish;
  end
endmodule
"#,
            100,
        );
        assert_eq!(
            body(&dump),
            "\
#0
$dumpvars
0!
r1.5 \"
b1111 #
$end
#5
$dumpoff
x!
rNaN \"
bx #
$end
#15
$dumpon
1!
r1.5 \"
b11001000 #
$end
#20
0!
#25
$dumpall
0!
r1.5 \"
b11001000 #
$end
#30
"
        );
    }

    /// `$dumplimit` stops the file once it passes the limit, and says so both
    /// in the file and in the design's output, in iverilog's words.
    #[test]
    fn test_dumplimit_stops_the_dump_and_says_so() {
        let (printed, dump) = dump_of(
            "limit",
            r#"
module limit;
  reg [7:0] a;
  initial begin
    $dumpfile("limit.vcd");
    $dumplimit(200);
    $dumpvars;
    a = 0;
    repeat (20) #1 a = a + 1;
  end
endmodule
"#,
            100,
        );
        assert_eq!(
            printed,
            "VCD info: dumpfile limit.vcd opened for output.\n\
             WARNING: Dump file limit (200 bytes) exceeded.\n"
        );
        assert!(dump.ends_with("$comment Dump file limit (200 bytes) exceeded. $end\n"));
        assert!(!dump.contains("#20\n"), "nothing is written past the limit");
    }

    /// With no `$dumpfile` the dump goes to `dump.vcd`, and `$dumpvars(1, …)`
    /// stops at the scope it names.
    #[test]
    fn test_a_level_limits_the_depth_and_the_file_name_defaults() {
        let directory = std::env::temp_dir().join("visilog-vcd-default");
        let _ = std::fs::remove_dir_all(&directory);
        std::fs::create_dir_all(&directory).unwrap();
        let parsed = parse_source(
            r#"
module top;
  reg t;
  leaf u ();
  initial begin
    $dumpvars(1, top);
    t = 1;
  end
endmodule
module leaf;
  reg l;
  initial l = 0;
endmodule
"#,
        )
        .unwrap();
        let mut simulator = Simulator::with_modules(parsed.modules, "top");
        simulator.set_output_directory(directory.clone());
        simulator.setup().unwrap();
        simulator.advance(10).unwrap();
        assert_eq!(
            simulator.output().text(),
            "VCD info: dumpfile dump.vcd opened for output.\n"
        );
        let dump = std::fs::read_to_string(directory.join("dump.vcd")).unwrap();
        assert!(dump.contains("$var reg 1 ! t $end\n"));
        assert!(
            !dump.contains("$scope module u $end"),
            "level 1 is `top` alone"
        );
    }

    /// A `$dumpvars` naming nothing the design has is an error that names it:
    /// a waveform that silently left a signal out would look exactly like one
    /// where it never moved.
    #[test]
    fn test_dumping_something_that_does_not_exist_is_a_named_error() {
        let parsed =
            parse_source("module top; reg a; initial $dumpvars(0, top.nothing); endmodule\n")
                .unwrap();
        let mut simulator = Simulator::with_modules(parsed.modules, "top");
        simulator.set_output_directory(std::env::temp_dir().join("visilog-vcd-missing"));
        let _ = std::fs::create_dir_all(std::env::temp_dir().join("visilog-vcd-missing"));
        let error = simulator
            .setup()
            .and_then(|()| simulator.advance(10))
            .unwrap_err();
        assert!(
            format!("{:?}", error).contains("`nothing` names no scope or variable"),
            "the error should name what was asked for: {:?}",
            error
        );
    }

    /// The trimming rule, measured against iverilog 12.0: a run of leading
    /// `0`s goes unless an unknown is under it, a run of `x`s or `z`s collapses
    /// to one, and a leading `1` is never touched.
    #[test]
    fn test_leading_digits_are_trimmed_the_way_iverilog_trims_them() {
        for (bits, expected) in [
            ("00001010", "1010"),
            ("00000000", "0"),
            ("xxxxxxxx", "x"),
            ("00zzzzzz", "0zzzzzz"),
            ("000xx000", "0xx000"),
            ("0011zz01", "11zz01"),
            ("xxxx0000", "x0000"),
            ("zz001111", "z001111"),
            ("0000xxxx", "0xxxx"),
            ("1x000000", "1x000000"),
            ("x1x1x1x1", "x1x1x1x1"),
            ("xx01", "x01"),
            ("z0z1", "z0z1"),
        ] {
            assert_eq!(super::trimmed(bits), expected, "trimming {}", bits);
        }
    }

    /// Identifiers are base 94 over `!`..`~`, least significant digit first.
    #[test]
    fn test_identifiers_are_handed_out_the_way_iverilog_hands_them_out() {
        assert_eq!(super::identifier(0), "!");
        assert_eq!(super::identifier(93), "~");
        assert_eq!(super::identifier(94), "!\"");
        assert_eq!(super::identifier(95), "\"\"");
        assert_eq!(super::identifier(188), "!#");
    }
}

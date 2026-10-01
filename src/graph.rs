//! A renderer-independent graph of an elaborated design.
//!
//! [`design_graph`] elaborates a design exactly as
//! [`Simulator::setup`](crate::simulator::runner::Simulator) does and reports
//! what elaboration built, as data a structure view, a source view and a
//! waveform view can all key into: the instance tree with each instance's
//! module and resolved parameter values, every port with its direction and
//! range, every net and variable, which ports share a store entry with a
//! parent signal, what each port was connected to down to the bit, and which
//! processes read and write which signals.
//!
//! # Identities
//!
//! Every ID is a hierarchical name **including the top module's name**,
//! dot-joined:
//!
//! - an instance is its path — `tb`, `tb.dut`, `tb.stage[0].u`;
//! - a signal is its instance's path and its local name — `tb.dut.count` —
//!   which is exactly the name a value change dump's `$scope` path gives it and
//!   the key [`Waveform::traces`](crate::waveform::Waveform) uses. The flat
//!   store key is the same name with the top segment dropped (`dut.count`);
//! - a process is its instance's path, a `/`, its kind and its position among
//!   that instance's processes of that kind — `tb.dut/always0`, `tb/assign2`.
//!
//! A port aliased onto its parent's signal has no store entry of its own —
//! the two are one value — so its [`Signal`] carries its own ID and names the
//! entry it shares in [`Signal::storage`].
//!
//! # What it does not claim
//!
//! This is the **behavioural RTL** elaboration produced, not a synthesised gate
//! netlist: a process is an `always` block, not the logic it would become, and
//! its reads and writes are the names its statements mention, not a
//! bit-accurate dataflow. There are no coordinates or any other layout. Source
//! positions are one line per module — where its `module` keyword was written
//! — and finer spans (per port, per statement) are future work: the parser does
//! not keep spans on its AST.
//!
//! The layout is versioned by [`GRAPH_SCHEMA`]. It moves when a field changes
//! meaning or goes away; adding a field does not move it.

use std::collections::{BTreeSet, HashMap};

use serde::{Deserialize, Serialize};

use crate::parsers::behavior::{EventControl, EventTriggers};
use crate::parsers::expr::Expression;
use crate::parsers::modules::{PortDirection, VerilogModule};
use crate::parsers::parameter::ParameterKind;
use crate::parsers::preprocessor::SourceLocation;
use crate::parsers::statements::ModuleStatement;
use crate::register::Register;
use crate::run::ToolInfo;
use crate::simulator::elaborate::{
    elaborate, expression_names, program_names, rename_expression, BindingKind, BlockKind,
    Elaborated,
};
use crate::simulator::eval::{eval, expression_width};
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::{SignalState, StateStore};
use crate::simulator::tasks::ascii;

/// The version of the [`DesignGraph`] layout.
pub const GRAPH_SCHEMA: u32 = 1;

/// An elaborated design, as data.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DesignGraph {
    /// [`GRAPH_SCHEMA`].
    pub schema: u32,
    pub tool: ToolInfo,
    /// The top module's name, which is also the root instance's ID and the
    /// first segment of every other ID.
    pub top: String,
    /// Every module the design was handed, instantiated or not.
    pub modules: Vec<Module>,
    /// Every instance, the top first and each before its children.
    pub instances: Vec<Instance>,
    /// Every net, variable, parameter, memory and named event, sorted by ID.
    pub signals: Vec<Signal>,
    /// Every connected port of every instance but the top.
    pub connections: Vec<Connection>,
    /// Every procedural block, continuous assignment and primitive.
    pub processes: Vec<Process>,
}

/// A module or user-defined primitive declaration.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Module {
    pub name: String,
    /// Whether this is a `primitive … endprimitive` rather than a module.
    pub primitive: bool,
    /// Where the `module` keyword was written; `None` when the design was not
    /// read through the preprocessor.
    pub source: Option<SourceLocation>,
    /// The port names, in declaration order.
    pub ports: Vec<String>,
}

/// One instance of a module.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Instance {
    pub id: String,
    /// The instance name as the parent wrote it — `dut`, `u[3]` for one
    /// element of an arrayed instance — or as elaboration gave it (`$BUFG1` for
    /// an unnamed primitive). The module's own name for the top.
    pub name: String,
    pub module: String,
    pub parent: Option<String>,
    pub children: Vec<String>,
    /// The module's `parameter`s and `localparam`s, with the values this
    /// instance resolved them to after every override.
    pub parameters: Vec<Parameter>,
    pub ports: Vec<Port>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    /// The parameter's signal ID.
    pub signal: String,
    /// Whether it was declared `localparam`.
    pub local: bool,
    pub value: Value,
}

/// A value as elaboration left it.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Value {
    pub width: usize,
    /// Four-state bits, most significant first: `0`, `1`, `x`, `z`.
    pub bits: String,
    pub signed: bool,
    /// The number, in decimal, when every bit is known and it fits in 128
    /// bits — signed when the value is.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub decimal: Option<String>,
    /// The number, when the value is a `real`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub real: Option<f64>,
    /// The string, when the value was written as text.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Direction {
    Input,
    Output,
    Inout,
}

impl From<PortDirection> for Direction {
    fn from(direction: PortDirection) -> Direction {
        match direction {
            PortDirection::Input => Direction::Input,
            PortDirection::Output => Direction::Output,
            PortDirection::InOut => Direction::Inout,
        }
    }
}

/// One port of an instance.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Port {
    pub name: String,
    pub direction: Direction,
    /// The port's own signal ID, which is its instance's path and its name
    /// whether or not it has a store entry of its own.
    pub signal: String,
    /// `[msb, lsb]` as resolved, parameters and all.
    pub range: Option<[i64; 2]>,
    pub width: Option<usize>,
    /// Whether the parent connected it. Always `false` for the top.
    pub connected: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SignalKind {
    Net,
    Variable,
    Parameter,
    Memory,
    Event,
}

/// One net, variable, parameter, memory or named event.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Signal {
    pub id: String,
    /// The instance it belongs to.
    pub instance: String,
    /// The name within that instance — `count`, `blk.tmp` for a named block's
    /// variable, `stage[0].sig` for a generate block's net, `load.a` for a
    /// task's argument.
    pub name: String,
    pub kind: SignalKind,
    /// `[msb, lsb]`; for a memory, the range of one word. `None` for an event.
    pub range: Option<[i64; 2]>,
    pub width: Option<usize>,
    pub signed: bool,
    pub real: bool,
    /// For a memory, the address range of each dimension.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub addresses: Option<Vec<[i64; 2]>>,
    /// When the signal is a port, its direction.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub port: Option<Direction>,
    /// For a port aliased onto a parent signal, the ID of the store entry the
    /// two share. `None` for a signal with an entry of its own.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub storage: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Binding {
    /// The port and the parent signal it names are one store entry.
    Alias,
    /// An input with an entry of its own, driven from the parent's expression
    /// by a continuous assignment.
    Driven,
    /// An output with an entry of its own, driving a select or concatenation
    /// of the parent's by a continuous assignment.
    Driving,
    /// An `inout` with an entry of its own, each bit joined to the matching
    /// bit of the connection.
    Bonded,
}

impl From<BindingKind> for Binding {
    fn from(kind: BindingKind) -> Binding {
        match kind {
            BindingKind::Alias => Binding::Alias,
            BindingKind::Driven => Binding::Driven,
            BindingKind::Driving => Binding::Driving,
            BindingKind::Bonded => Binding::Bonded,
        }
    }
}

/// What one port of one instance was connected to.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Connection {
    /// The child instance.
    pub instance: String,
    pub port: String,
    /// The port's signal ID.
    pub signal: String,
    pub direction: Direction,
    pub binding: Binding,
    /// The connection as written in the parent, with every name replaced by
    /// its signal ID.
    pub expression: String,
    /// How wide the connection is.
    pub width: usize,
    /// The connection broken into pieces, **most significant first**, whose
    /// widths add up to [`Connection::width`]. The port's least significant bit
    /// meets the last piece's least significant bit.
    pub segments: Vec<Segment>,
}

/// One piece of a port connection.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Segment {
    /// Bits `msb` down to `lsb` of a signal, in its declared index space — or
    /// of one word of a memory, when `word` gives its address.
    Signal {
        signal: String,
        msb: i64,
        lsb: i64,
        width: usize,
        #[serde(skip_serializing_if = "Option::is_none")]
        word: Option<Vec<i64>>,
    },
    /// A literal.
    Constant { value: String, width: usize },
    /// Anything else — `a + 1`, a select whose index is not a constant — with
    /// the signals it reads.
    Expression {
        expression: String,
        width: usize,
        reads: Vec<String>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ProcessKind {
    Initial,
    Always,
    /// A continuous assignment — written as `assign`, as a net declaration's
    /// initialiser, or added by elaboration to carry a port connection.
    Assign,
    /// A built-in gate primitive.
    Gate,
    /// An instance of a user-defined primitive.
    Udp,
    /// A bidirectional pass switch (`tran`, `tranif1`, …).
    Switch,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Trigger {
    /// `always` with no event control, or an `initial` block.
    None,
    /// `@*` — sensitive to what the body reads.
    Implicit,
    /// An explicit sensitivity list.
    Events,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Edge {
    Posedge,
    Negedge,
    Any,
}

/// One entry of a sensitivity list.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Sensitivity {
    pub edge: Edge,
    /// The entry as written, names replaced by their IDs.
    pub expression: String,
    pub signals: Vec<String>,
}

/// A block, assignment or primitive, with the signals it reads and writes.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Process {
    pub id: String,
    pub instance: String,
    pub kind: ProcessKind,
    /// The primitive's keyword for a gate or switch, its module name for a
    /// user-defined primitive.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub primitive: Option<String>,
    /// For an `assign`, whether elaboration added it to carry a port
    /// connection — an input bound to an expression, an output bound to a
    /// select, a port of a different width — rather than the design writing
    /// it. The [`Connection`] it carries says the same thing structurally.
    #[serde(skip_serializing_if = "std::ops::Not::not", default)]
    pub port_connection: bool,
    /// What wakes an `always` block.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trigger: Option<Trigger>,
    /// The sensitivity list. For `@*`, one `any` entry per signal it reads.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub sensitivity: Vec<Sensitivity>,
    /// The signals an expression in it reads, by ID and sorted. For a block
    /// this counts every statement but the arguments of a system task.
    pub reads: Vec<String>,
    /// The signals it assigns, by ID and sorted.
    pub writes: Vec<String>,
}

/// Elaborates `modules` under `top` and reports what it built.
pub fn design_graph(modules: &[VerilogModule], top: &str) -> Result<DesignGraph, SimulationError> {
    let index = modules
        .iter()
        .position(|module| module.identifier.name == top)
        .ok_or_else(|| SimulationError::UnknownModule(top.to_string()))?;
    let elaborated = elaborate(modules, index)?;
    Ok(Builder::new(modules, top, &elaborated).build())
}

/// Whether a store name is one of the simulator's own slots rather than
/// something the design declared.
fn is_hidden(name: &str) -> bool {
    name.contains("$repeat$") || name.contains("$hold$")
}

struct Builder<'a> {
    modules: &'a [VerilogModule],
    top: &'a str,
    elaborated: &'a Elaborated,
    /// Flat prefix (`"dut."`) to instance index.
    prefixes: HashMap<&'a str, usize>,
}

impl<'a> Builder<'a> {
    fn new(modules: &'a [VerilogModule], top: &'a str, elaborated: &'a Elaborated) -> Self {
        let prefixes = elaborated
            .hierarchy
            .instances
            .iter()
            .enumerate()
            .map(|(index, instance)| (instance.prefix.as_str(), index))
            .collect();
        Builder {
            modules,
            top,
            elaborated,
            prefixes,
        }
    }

    fn store(&self) -> &StateStore {
        &self.elaborated.state
    }

    /// The ID of a flat store name.
    fn id(&self, flat: &str) -> String {
        if flat.is_empty() {
            self.top.to_string()
        } else {
            format!("{}.{}", self.top, flat)
        }
    }

    fn instance_id(&self, index: usize) -> String {
        self.elaborated.hierarchy.instances[index].path.clone()
    }

    /// The instance a flat name belongs to — the one with the longest prefix
    /// of it — and the name within that instance.
    fn owner<'n>(&self, flat: &'n str) -> (usize, &'n str) {
        let mut end = flat.len();
        while let Some(dot) = flat[..end].rfind('.') {
            if let Some(index) = self.prefixes.get(&flat[..=dot]) {
                return (*index, &flat[dot + 1..]);
            }
            end = dot;
        }
        (0, flat)
    }

    /// The store entry a flat name reads, through a port alias.
    fn signal(&self, flat: &str) -> Option<&SignalState> {
        self.store().get_signal(flat).or_else(|| {
            self.elaborated
                .aliases
                .get(flat)
                .and_then(|storage| self.store().get_signal(storage))
        })
    }

    fn ids(&self, names: impl IntoIterator<Item = String>) -> Vec<String> {
        let set: BTreeSet<String> = names
            .into_iter()
            .filter(|name| !is_hidden(name))
            .map(|name| self.id(&name))
            .collect();
        set.into_iter().collect()
    }

    /// `expression` as text, every name replaced by its ID.
    fn text(&self, expression: &Expression) -> String {
        let mut copy = expression.clone();
        rename_expression(&mut copy, &|name| self.id(name));
        copy.to_contracted_string()
    }

    fn build(&self) -> DesignGraph {
        DesignGraph {
            schema: GRAPH_SCHEMA,
            tool: ToolInfo::current(),
            top: self.top.to_string(),
            modules: self.modules.iter().map(module_node).collect(),
            instances: self.instances(),
            signals: self.signals(),
            connections: self.connections(),
            processes: self.processes(),
        }
    }

    fn instances(&self) -> Vec<Instance> {
        let records = &self.elaborated.hierarchy.instances;
        records
            .iter()
            .enumerate()
            .map(|(index, record)| {
                let module = &self.modules[record.module];
                let parameters = module
                    .statements
                    .iter()
                    .filter_map(|statement| match statement {
                        ModuleStatement::ParameterDeclaration(parameters) => Some(parameters),
                        _ => None,
                    })
                    .flatten()
                    .filter_map(|parameter| {
                        let flat = format!("{}{}", record.prefix, parameter.name.name);
                        let signal = self.store().get_signal(&flat)?;
                        Some(Parameter {
                            name: parameter.name.name.clone(),
                            signal: self.id(&flat),
                            local: parameter.kind == ParameterKind::LocalParam,
                            value: value(signal.register(), self.store().is_text(&flat)),
                        })
                    })
                    .collect();
                let ports = module
                    .ports
                    .iter()
                    .map(|port| {
                        let flat = format!("{}{}", record.prefix, port.identifier.name);
                        let range = self.signal(&flat).map(|signal| signal.range());
                        Port {
                            name: port.identifier.name.clone(),
                            direction: port.direction.into(),
                            signal: self.id(&flat),
                            range: range.map(|(msb, lsb)| [msb, lsb]),
                            width: range.map(width),
                            connected: record
                                .connections
                                .iter()
                                .any(|connection| connection.port == port.identifier.name),
                        }
                    })
                    .collect();
                Instance {
                    id: record.path.clone(),
                    name: record.name.clone(),
                    module: module.identifier.name.clone(),
                    parent: record.parent.map(|parent| self.instance_id(parent)),
                    children: records
                        .iter()
                        .filter(|child| child.parent == Some(index))
                        .map(|child| child.path.clone())
                        .collect(),
                    parameters,
                    ports,
                }
            })
            .collect()
    }

    fn signals(&self) -> Vec<Signal> {
        // Which flat names are parameters, and which are ports and how.
        let mut parameters = BTreeSet::new();
        let mut ports = HashMap::new();
        for record in &self.elaborated.hierarchy.instances {
            let module = &self.modules[record.module];
            for statement in &module.statements {
                if let ModuleStatement::ParameterDeclaration(declared) = statement {
                    for parameter in declared {
                        parameters.insert(format!("{}{}", record.prefix, parameter.name.name));
                    }
                }
            }
            for port in &module.ports {
                ports.insert(
                    format!("{}{}", record.prefix, port.identifier.name),
                    Direction::from(port.direction),
                );
            }
        }

        let mut signals = Vec::new();
        let mut node = |flat: &str, kind, state: Option<&SignalState>, storage: Option<&str>| {
            let (instance, name) = self.owner(flat);
            let range = state.map(|state| state.range());
            signals.push(Signal {
                id: self.id(flat),
                instance: self.instance_id(instance),
                name: name.to_string(),
                kind,
                range: range.map(|(msb, lsb)| [msb, lsb]),
                width: range.map(width),
                signed: state.is_some_and(|state| state.is_signed()),
                real: state.is_some_and(|state| state.is_real()),
                addresses: None,
                port: ports.get(flat).copied(),
                storage: storage.map(|storage| self.id(storage)),
            });
        };
        for flat in self.store().names() {
            if is_hidden(flat) {
                continue;
            }
            let state = self.store().get_signal(flat);
            let kind = if parameters.contains(flat) {
                SignalKind::Parameter
            } else if state.is_some_and(|state| state.is_net()) {
                SignalKind::Net
            } else {
                SignalKind::Variable
            };
            node(flat, kind, state, None);
        }
        for (flat, storage) in &self.elaborated.aliases {
            let state = self.store().get_signal(storage);
            let kind = if state.is_some_and(|state| state.is_net()) {
                SignalKind::Net
            } else {
                SignalKind::Variable
            };
            node(flat, kind, state, Some(storage));
        }
        for flat in self.store().event_names() {
            node(flat, SignalKind::Event, None, None);
        }
        for flat in self.store().memory_names() {
            let memory = self.store().memory(flat).expect("a listed memory exists");
            let (instance, name) = self.owner(flat);
            let range = memory.range();
            signals.push(Signal {
                id: self.id(flat),
                instance: self.instance_id(instance),
                name: name.to_string(),
                kind: SignalKind::Memory,
                range: Some([range.0, range.1]),
                width: Some(memory.width()),
                signed: memory.is_signed(),
                real: memory.is_real(),
                addresses: Some(
                    memory
                        .addresses()
                        .iter()
                        .map(|&(first, last)| [first, last])
                        .collect(),
                ),
                port: None,
                storage: None,
            });
        }
        signals.sort_by(|left, right| left.id.cmp(&right.id));
        signals
    }

    fn connections(&self) -> Vec<Connection> {
        let mut connections = Vec::new();
        for record in &self.elaborated.hierarchy.instances {
            for connection in &record.connections {
                let mut segments = Vec::new();
                self.segments(&connection.connection, &mut segments);
                connections.push(Connection {
                    instance: record.path.clone(),
                    port: connection.port.clone(),
                    signal: self.id(&format!("{}{}", record.prefix, connection.port)),
                    direction: connection.direction.into(),
                    binding: connection.binding.into(),
                    expression: self.text(&connection.connection),
                    width: segments.iter().map(Segment::width).sum(),
                    segments,
                });
            }
        }
        connections
    }

    /// `expression` with every port alias followed to the entry it shares, so
    /// the store can answer questions about it.
    fn stored(&self, expression: &Expression) -> Expression {
        let mut copy = expression.clone();
        if !self.elaborated.aliases.is_empty() {
            rename_expression(&mut copy, &|name| {
                self.elaborated
                    .aliases
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| name.to_string())
            });
        }
        copy
    }

    /// A constant index, evaluated against the parameters in the store.
    fn index(&self, expression: &Expression) -> Option<i64> {
        eval(&self.stored(expression), self.store())
            .ok()?
            .to_i128()
            .and_then(|value| i64::try_from(value).ok())
    }

    fn segments(&self, expression: &Expression, out: &mut Vec<Segment>) {
        if let Some(segment) = self.signal_segment(expression) {
            out.push(segment);
            return;
        }
        match expression {
            Expression::Parenthetical(inner) => self.segments(inner, out),
            Expression::Concatenation(parts) => {
                for part in parts {
                    self.segments(part, out);
                }
            }
            Expression::Replication(count, parts) => match self.index(count) {
                Some(count) if (0..=4096).contains(&count) => {
                    for _ in 0..count {
                        for part in parts {
                            self.segments(part, out);
                        }
                    }
                }
                _ => out.push(self.opaque(expression)),
            },
            Expression::Constant(_) => out.push(Segment::Constant {
                value: expression.to_contracted_string(),
                width: expression_width(expression, self.store()),
            }),
            _ => out.push(self.opaque(expression)),
        }
    }

    /// A whole signal, a constant select of one, or a word of a memory.
    fn signal_segment(&self, expression: &Expression) -> Option<Segment> {
        let whole = |name: &str, msb: i64, lsb: i64, word: Option<Vec<i64>>| Segment::Signal {
            signal: self.id(name),
            msb,
            lsb,
            width: width((msb, lsb)),
            word,
        };
        let vector = |name: &str| {
            self.signal(name)
                .filter(|_| self.store().packed(self.stored_name(name)).is_none())
        };
        match expression {
            Expression::Identifier(id) => {
                let (msb, lsb) = self.signal(&id.name)?.range();
                Some(whole(&id.name, msb, lsb, None))
            }
            Expression::BitSelect(id, index) => {
                let index = self.index(index)?;
                if vector(&id.name).is_some() {
                    return Some(whole(&id.name, index, index, None));
                }
                let memory = self.store().memory(&id.name)?;
                if memory.dimensions() != 1 {
                    return None;
                }
                let (msb, lsb) = memory.range();
                Some(whole(&id.name, msb, lsb, Some(vec![index])))
            }
            Expression::PartSelect(id, first, second) => {
                vector(&id.name)?;
                Some(whole(
                    &id.name,
                    self.index(first)?,
                    self.index(second)?,
                    None,
                ))
            }
            Expression::IndexedPartSelect {
                id,
                base,
                width: span,
                upward,
            } => {
                let (msb, lsb) = vector(&id.name)?.range();
                let base = self.index(base)?;
                let span = self.index(span)?;
                if span < 1 {
                    return None;
                }
                let (low, high) = if *upward {
                    (base, base + span - 1)
                } else {
                    (base - span + 1, base)
                };
                // Written in the signal's own direction, most significant first.
                Some(if msb >= lsb {
                    whole(&id.name, high, low, None)
                } else {
                    whole(&id.name, low, high, None)
                })
            }
            _ => None,
        }
    }

    fn stored_name<'n>(&'n self, name: &'n str) -> &'n str {
        self.elaborated
            .aliases
            .get(name)
            .map(String::as_str)
            .unwrap_or(name)
    }

    fn opaque(&self, expression: &Expression) -> Segment {
        let (reads, _) = expression_names(expression, false);
        Segment::Expression {
            expression: self.text(expression),
            width: expression_width(&self.stored(expression), self.store()),
            reads: self.ids(reads),
        }
    }

    fn processes(&self) -> Vec<Process> {
        let hierarchy = &self.elaborated.hierarchy;
        let mut counts: HashMap<(usize, ProcessKind), usize> = HashMap::new();
        let mut process = |owner: usize, kind: ProcessKind| {
            let count = counts.entry((owner, kind)).or_default();
            let id = format!("{}/{}{}", self.instance_id(owner), kind.name(), count);
            *count += 1;
            Process {
                id,
                instance: self.instance_id(owner),
                kind,
                primitive: None,
                port_connection: false,
                trigger: None,
                sensitivity: Vec::new(),
                reads: Vec::new(),
                writes: Vec::new(),
            }
        };
        let mut processes = Vec::new();

        for (block, &owner) in self.elaborated.blocks.iter().zip(&hierarchy.block_owner) {
            let kind = match block.kind {
                BlockKind::Initial => ProcessKind::Initial,
                BlockKind::Always => ProcessKind::Always,
            };
            let mut node = process(owner, kind);
            let (reads, writes) = program_names(&block.program);
            node.reads = self.ids(reads);
            node.writes = self.ids(writes);
            if kind == ProcessKind::Always {
                node.trigger = Some(match &block.control {
                    EventControl::None => Trigger::None,
                    EventControl::Implicit => Trigger::Implicit,
                    EventControl::Events(_) => Trigger::Events,
                });
                node.sensitivity = match &block.control {
                    EventControl::None => Vec::new(),
                    EventControl::Implicit => block
                        .implicit_reads
                        .iter()
                        .filter(|name| !is_hidden(name))
                        .map(|name| Sensitivity {
                            edge: Edge::Any,
                            expression: self.id(name),
                            signals: vec![self.id(name)],
                        })
                        .collect(),
                    EventControl::Events(events) => events
                        .iter()
                        .map(|event| Sensitivity {
                            edge: match event.trigger {
                                EventTriggers::PosEdge => Edge::Posedge,
                                EventTriggers::NegEdge => Edge::Negedge,
                                EventTriggers::EitherEdge => Edge::Any,
                            },
                            expression: self.text(&event.expression),
                            signals: self.ids(expression_names(&event.expression, false).0),
                        })
                        .collect(),
                };
            }
            processes.push(node);
        }

        for (index, (assignment, &owner)) in self
            .elaborated
            .assignments
            .iter()
            .zip(&hierarchy.assignment_owner)
            .enumerate()
        {
            let mut node = process(owner, ProcessKind::Assign);
            node.port_connection = hierarchy.port_assignments.contains(&index);
            let (mut reads, writes) = expression_names(assignment.lhs(), true);
            reads.extend(expression_names(assignment.rhs(), false).0);
            node.reads = self.ids(reads);
            node.writes = self.ids(writes);
            processes.push(node);
        }

        for (gate, &owner) in self.elaborated.gates.iter().zip(&hierarchy.gate_owner) {
            let mut node = process(owner, ProcessKind::Gate);
            node.primitive = Some(gate.kind.keyword().to_string());
            let mut reads = BTreeSet::new();
            let mut writes = BTreeSet::new();
            for output in &gate.outputs {
                let (indices, written) = expression_names(output, true);
                reads.extend(indices);
                writes.extend(written);
            }
            for input in &gate.inputs {
                reads.extend(expression_names(input, false).0);
            }
            node.reads = self.ids(reads);
            node.writes = self.ids(writes);
            processes.push(node);
        }

        for (udp, &owner) in self.elaborated.udps.iter().zip(&hierarchy.udp_owner) {
            let mut node = process(owner, ProcessKind::Udp);
            node.primitive = Some(udp.name.clone());
            let (mut reads, writes) = expression_names(&udp.output, true);
            for input in &udp.inputs {
                reads.extend(expression_names(input, false).0);
            }
            node.reads = self.ids(reads);
            node.writes = self.ids(writes);
            processes.push(node);
        }

        for (switch, &owner) in self
            .elaborated
            .pass_switches
            .iter()
            .zip(&hierarchy.switch_owner)
        {
            // A port bond is how an `inout` connection is carried, which the
            // connection already reports; it is not a switch the design wrote.
            if switch.port {
                continue;
            }
            let mut node = process(owner, ProcessKind::Switch);
            node.primitive = Some(switch.kind.keyword().to_string());
            // Both terminals are read and written: a switch conducts both ways.
            let mut names = BTreeSet::new();
            for terminal in &switch.terminals {
                names.extend(expression_names(terminal, false).0);
            }
            node.writes = self.ids(names.clone());
            if let Some((control, _)) = &switch.control {
                names.extend(expression_names(control, false).0);
            }
            node.reads = self.ids(names);
            processes.push(node);
        }
        processes
    }
}

impl ProcessKind {
    /// How the kind is spelled in the JSON and in a process ID.
    pub fn name(self) -> &'static str {
        match self {
            ProcessKind::Initial => "initial",
            ProcessKind::Always => "always",
            ProcessKind::Assign => "assign",
            ProcessKind::Gate => "gate",
            ProcessKind::Udp => "udp",
            ProcessKind::Switch => "switch",
        }
    }
}

impl Segment {
    pub fn width(&self) -> usize {
        match self {
            Segment::Signal { width, .. }
            | Segment::Constant { width, .. }
            | Segment::Expression { width, .. } => *width,
        }
    }
}

fn module_node(module: &VerilogModule) -> Module {
    Module {
        name: module.identifier.name.clone(),
        primitive: module
            .statements
            .iter()
            .any(|statement| matches!(statement, ModuleStatement::PrimitiveTable(_))),
        source: module.source.clone(),
        ports: module
            .ports
            .iter()
            .map(|port| port.identifier.name.clone())
            .collect(),
    }
}

fn width((msb, lsb): (i64, i64)) -> usize {
    (msb - lsb).unsigned_abs() as usize + 1
}

fn value(register: &Register, text: bool) -> Value {
    let real = register.is_real();
    Value {
        width: register.width(),
        bits: register.to_binary(),
        signed: register.is_signed(),
        decimal: if real {
            None
        } else if register.is_signed() {
            register.to_i128().map(|value| value.to_string())
        } else {
            register.to_u128().map(|value| value.to_string())
        },
        real: real.then(|| register.to_f64()),
        text: text.then(|| ascii(register)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::source::parse_source;

    /// A three-level design with every shape a port connection can take: a
    /// plain name (aliased), a part select, a concatenation, a literal, a
    /// replication, an expression, an output driving a select, and instances
    /// made by a generate loop.
    const DESIGN: &str = "
module leaf #(parameter W = 4) (input [W-1:0] a, input [1:0] b, input [3:0] c,
                                output [W-1:0] y);
  assign y = a;
endmodule

module mid (input [7:0] bus, input x, input y, output [3:0] out);
  wire [3:0] lo;
  leaf #(.W(4)) u (.a(bus[3:0]), .b({x, y}), .c(4'b1010), .y(out));
  leaf #(.W(2)) v (.a(bus[5:4] + 2'd1), .b(bus[7:6]), .c({2{x, y}}), .y(lo[1:0]));
endmodule

module top;
  reg [7:0] bus;
  reg x, y;
  wire [3:0] out;
  mid m (.bus(bus), .x(x), .y(y), .out(out));
  genvar i;
  generate for (i = 0; i < 2; i = i + 1) begin : stage
    wire [3:0] q;
    leaf #(.W(4)) u (.a(bus[i*4 +: 4]), .b(2'b00), .c(4'd0), .y(q));
  end endgenerate
  always @(posedge x) bus <= bus + 1;
  and g (w, x, y);
  initial repeat (2) #1 x = ~x;
endmodule
";

    fn graph() -> DesignGraph {
        let modules = parse_source(DESIGN).unwrap().modules;
        design_graph(&modules, "top").unwrap()
    }

    fn instance<'g>(graph: &'g DesignGraph, id: &str) -> &'g Instance {
        graph
            .instances
            .iter()
            .find(|instance| instance.id == id)
            .unwrap_or_else(|| panic!("no instance {}", id))
    }

    fn signal<'g>(graph: &'g DesignGraph, id: &str) -> &'g Signal {
        graph
            .signals
            .iter()
            .find(|signal| signal.id == id)
            .unwrap_or_else(|| panic!("no signal {}", id))
    }

    fn connection<'g>(graph: &'g DesignGraph, instance: &str, port: &str) -> &'g Connection {
        graph
            .connections
            .iter()
            .find(|connection| connection.instance == instance && connection.port == port)
            .unwrap_or_else(|| panic!("no connection {}.{}", instance, port))
    }

    fn bits(signal: &str, msb: i64, lsb: i64) -> Segment {
        Segment::Signal {
            signal: signal.to_string(),
            msb,
            lsb,
            width: width((msb, lsb)),
            word: None,
        }
    }

    #[test]
    fn test_the_graph_is_versioned_and_names_its_top() {
        let graph = graph();
        assert_eq!(graph.schema, GRAPH_SCHEMA);
        assert_eq!(graph.schema, 1);
        assert_eq!(graph.top, "top");
        assert_eq!(graph.tool.name, "visilog");
    }

    #[test]
    fn test_instances_form_the_hierarchy_with_generated_identities() {
        let graph = graph();
        let ids: Vec<&str> = graph.instances.iter().map(|i| i.id.as_str()).collect();
        assert_eq!(
            ids,
            vec![
                "top",
                "top.m",
                "top.m.u",
                "top.m.v",
                "top.stage[0].u",
                "top.stage[1].u"
            ]
        );
        let top = instance(&graph, "top");
        assert_eq!(top.parent, None);
        assert_eq!(
            top.children,
            vec!["top.m", "top.stage[0].u", "top.stage[1].u"]
        );
        let mid = instance(&graph, "top.m");
        assert_eq!(mid.module, "mid");
        assert_eq!(mid.name, "m");
        assert_eq!(mid.parent.as_deref(), Some("top"));
        assert_eq!(mid.children, vec!["top.m.u", "top.m.v"]);
        let generated = instance(&graph, "top.stage[1].u");
        assert_eq!(generated.name, "u");
        assert_eq!(generated.module, "leaf");
        assert_eq!(generated.parent.as_deref(), Some("top"));
    }

    #[test]
    fn test_parameters_are_the_values_each_instance_resolved() {
        let graph = graph();
        let w = |id: &str| instance(&graph, id).parameters[0].clone();
        assert_eq!(w("top.m.u").value.decimal.as_deref(), Some("4"));
        assert_eq!(w("top.m.v").value.decimal.as_deref(), Some("2"));
        assert_eq!(w("top.m.v").signal, "top.m.v.W");
        assert!(!w("top.m.v").local);
        assert_eq!(signal(&graph, "top.m.v.W").kind, SignalKind::Parameter);
    }

    #[test]
    fn test_ports_carry_direction_and_the_resolved_range() {
        let graph = graph();
        let v = instance(&graph, "top.m.v");
        let a = &v.ports[0];
        assert_eq!(a.name, "a");
        assert_eq!(a.direction, Direction::Input);
        assert_eq!(a.signal, "top.m.v.a");
        assert_eq!(a.range, Some([1, 0]));
        assert_eq!(a.width, Some(2));
        assert!(a.connected);
        assert_eq!(v.ports[3].direction, Direction::Output);
        assert!(instance(&graph, "top").ports.is_empty());
    }

    /// A port bound to a plain name shares its parent's store entry, so its
    /// signal names the entry it is stored in; a port bound to anything else
    /// has one of its own.
    #[test]
    fn test_an_aliased_port_names_the_entry_it_shares() {
        let graph = graph();
        let bus = signal(&graph, "top.m.bus");
        assert_eq!(bus.instance, "top.m");
        assert_eq!(bus.name, "bus");
        assert_eq!(bus.port, Some(Direction::Input));
        assert_eq!(bus.storage.as_deref(), Some("top.bus"));
        assert_eq!(bus.range, Some([7, 0]));
        assert_eq!(connection(&graph, "top.m", "bus").binding, Binding::Alias);

        let own = signal(&graph, "top.m.u.a");
        assert_eq!(own.storage, None);
        assert_eq!(own.kind, SignalKind::Net);
        assert_eq!(connection(&graph, "top.m.u", "a").binding, Binding::Driven);
    }

    #[test]
    fn test_signals_belong_to_the_instance_or_generate_block_that_declared_them() {
        let graph = graph();
        let q = signal(&graph, "top.stage[0].q");
        assert_eq!(q.instance, "top");
        assert_eq!(q.name, "stage[0].q");
        assert_eq!(q.width, Some(4));
        assert_eq!(signal(&graph, "top.bus").kind, SignalKind::Variable);
        assert_eq!(signal(&graph, "top.out").kind, SignalKind::Net);
        // The gate's output is an implicit net.
        assert_eq!(signal(&graph, "top.w").width, Some(1));
        // The `repeat` counter is the simulator's, not the design's.
        assert!(graph.signals.iter().all(|signal| !signal.id.contains('$')));
    }

    /// A connection is broken into pieces, most significant first, and a name
    /// under an aliased port of the parent keeps the port's own ID.
    #[test]
    fn test_connections_break_into_slices_constants_and_expressions() {
        let graph = graph();
        let a = connection(&graph, "top.m.u", "a");
        assert_eq!(a.expression, "top.m.bus[3:0]");
        assert_eq!(a.segments, vec![bits("top.m.bus", 3, 0)]);
        assert_eq!(a.width, 4);

        let b = connection(&graph, "top.m.u", "b");
        assert_eq!(
            b.segments,
            vec![bits("top.m.x", 0, 0), bits("top.m.y", 0, 0)]
        );
        assert_eq!(b.width, 2);

        let c = connection(&graph, "top.m.u", "c");
        match &c.segments[..] {
            [Segment::Constant { value, width }] => {
                assert!(value.contains("1010"), "{}", value);
                assert_eq!(*width, 4);
            }
            other => panic!("expected one constant, got {:?}", other),
        }

        let sum = connection(&graph, "top.m.v", "a");
        match &sum.segments[..] {
            [Segment::Expression { reads, .. }] => assert_eq!(reads, &vec!["top.m.bus"]),
            other => panic!("expected one expression, got {:?}", other),
        }

        let replicated = connection(&graph, "top.m.v", "c");
        assert_eq!(
            replicated.segments,
            vec![
                bits("top.m.x", 0, 0),
                bits("top.m.y", 0, 0),
                bits("top.m.x", 0, 0),
                bits("top.m.y", 0, 0)
            ]
        );

        let out = connection(&graph, "top.m.v", "y");
        assert_eq!(out.binding, Binding::Driving);
        assert_eq!(out.direction, Direction::Output);
        assert_eq!(out.signal, "top.m.v.y");
        assert_eq!(out.segments, vec![bits("top.m.lo", 1, 0)]);
    }

    /// A genvar reaches a connection as a number, so each iteration's slice
    /// is a constant range of the parent's bus.
    #[test]
    fn test_a_generated_instance_connects_to_its_own_slice() {
        let graph = graph();
        let a = connection(&graph, "top.stage[1].u", "a");
        assert_eq!(a.segments, vec![bits("top.bus", 7, 4)]);
        let y = connection(&graph, "top.stage[1].u", "y");
        assert_eq!(y.binding, Binding::Alias);
        assert_eq!(y.segments, vec![bits("top.stage[1].q", 3, 0)]);
    }

    #[test]
    fn test_processes_report_what_they_read_and_write() {
        let graph = graph();
        let process = |id: &str| {
            graph
                .processes
                .iter()
                .find(|process| process.id == id)
                .unwrap_or_else(|| panic!("no process {}", id))
        };
        let always = process("top/always0");
        assert_eq!(always.kind, ProcessKind::Always);
        assert_eq!(always.trigger, Some(Trigger::Events));
        assert_eq!(
            always.sensitivity,
            vec![Sensitivity {
                edge: Edge::Posedge,
                expression: "top.x".into(),
                signals: vec!["top.x".into()],
            }]
        );
        assert_eq!(always.reads, vec!["top.bus"]);
        assert_eq!(always.writes, vec!["top.bus"]);

        let gate = process("top/gate0");
        assert_eq!(gate.primitive.as_deref(), Some("and"));
        assert_eq!(gate.reads, vec!["top.x", "top.y"]);
        assert_eq!(gate.writes, vec!["top.w"]);

        // Elaboration carries each of `v`'s four connections with an
        // assignment of its own, and says so; the design wrote only one.
        let assigns: Vec<&Process> = graph
            .processes
            .iter()
            .filter(|process| process.instance == "top.m.v" && process.kind == ProcessKind::Assign)
            .collect();
        assert_eq!(assigns.len(), 5);
        let (carried, written): (Vec<&Process>, Vec<&Process>) = assigns
            .into_iter()
            .partition(|process| process.port_connection);
        assert_eq!(carried.len(), 4);
        assert_eq!(carried[0].writes, vec!["top.m.v.a"]);
        assert_eq!(carried[0].reads, vec!["top.bus"]);
        assert_eq!(carried[3].writes, vec!["top.m.lo"]);
        assert_eq!(carried[3].reads, vec!["top.m.v.y"]);
        assert_eq!(written[0].writes, vec!["top.m.v.y"]);
        assert_eq!(written[0].reads, vec!["top.m.v.a"]);

        let initial = process("top/initial0");
        assert_eq!(initial.trigger, None);
        assert_eq!(initial.writes, vec!["top.x"]);
    }

    #[test]
    fn test_modules_carry_where_they_were_written() {
        let graph = graph();
        let lines: Vec<(&str, usize)> = graph
            .modules
            .iter()
            .map(|module| (module.name.as_str(), module.source.as_ref().unwrap().line))
            .collect();
        assert_eq!(lines, vec![("leaf", 2), ("mid", 7), ("top", 13)]);
        assert_eq!(graph.modules[0].ports, vec!["a", "b", "c", "y"]);
        assert!(!graph.modules[0].primitive);
    }

    #[test]
    fn test_the_graph_round_trips_through_json() {
        let graph = graph();
        let json = serde_json::to_string(&graph).unwrap();
        let back: DesignGraph = serde_json::from_str(&json).unwrap();
        assert_eq!(back, graph);
        let value: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(value["schema"], 1);
        assert_eq!(value["connections"][0]["segments"][0]["kind"], "signal");
    }

    /// A plain name bound to a port of another width cannot share its entry,
    /// and the connection reports what elaboration made of it rather than
    /// what the parent wrote.
    #[test]
    fn test_a_port_of_another_width_is_reported_as_carried() {
        let modules = parse_source(
            "module c (input [3:0] a); endmodule
             module t; reg [7:0] r; reg [3:0] s; c u (.a(r)); c k (.a(s)); endmodule",
        )
        .unwrap()
        .modules;
        let graph = design_graph(&modules, "t").unwrap();
        assert_eq!(connection(&graph, "t.u", "a").binding, Binding::Driven);
        assert_eq!(signal(&graph, "t.u.a").storage, None);
        assert_eq!(connection(&graph, "t.k", "a").binding, Binding::Alias);
        assert_eq!(signal(&graph, "t.k.a").storage.as_deref(), Some("t.s"));
    }

    #[test]
    fn test_an_unknown_top_is_named() {
        let modules = parse_source(DESIGN).unwrap().modules;
        assert!(matches!(
            design_graph(&modules, "nope"),
            Err(SimulationError::UnknownModule(name)) if name == "nope"
        ));
    }
}

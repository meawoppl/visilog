//! Turning a module hierarchy into the flat simulation model.
//!
//! The simulator's core is deliberately flat: one [`StateStore`] of signal
//! names, one list of continuous assignments, one list of compiled procedural
//! blocks. Hierarchy is resolved here, at elaboration time, by walking the
//! instantiation tree and inlining every child into those same flat structures.
//! Nothing about hierarchy survives into the run loop, so a design with
//! instances costs exactly what the equivalent hand-flattened design costs.
//!
//! # Naming
//!
//! An instance's internal signals take a dotted, qualified name: instantiating
//! `counter dut (...)` inside `top` puts the child's `count` register in the
//! store as `dut.count`, and a further instance inside that one reaches
//! `dut.inner.sig`. Two instances of the same module therefore never collide.
//!
//! # Port binding
//!
//! A port connected to a plain identifier is *aliased*: the child's port and
//! the parent's signal are the same store entry, resolved statically here, so
//! there is no propagation step between them and no value can go stale. The
//! qualified spelling (`dut.clk`) is still recorded in
//! [`Elaborated::aliases`] so it can be read back, but it holds no state of its
//! own.
//!
//! A port connected to a general expression (`.a(x + 1)`) cannot be aliased.
//! An input gets a real signal of its own plus a continuous assignment from the
//! parent's expression; an output bound to something *writable* gets the same
//! assignment run outwards, and one bound to something that cannot be written
//! at all is reported as [`SimulationError::UndrivablePort`]. An `inout` is
//! neither — see [`Binding::Bonded`].
//!
//! An unconnected input is floating, so it is declared `z`. An unconnected
//! output is a real signal the child drives that simply nobody reads, so it
//! starts `x` like any other.
//!
//! Aliasing is only sound when the two really are one signal, so a port whose
//! declared width is **not** the width of what it was bound to is not aliased
//! at all: [`Elaborator::reconcile_port_widths`] turns it back into a port with
//! an entry of its own plus a continuous assignment in the port's own
//! direction, which is the shape a port bound to an *expression* already had.
//! The extension, the truncation and the signedness are then an assignment's.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use crate::parsers::{
    assignment::ContinuousAssignment,
    behavior::{
        Event, EventControl, FunctionDeclaration, FunctionVariable, ProceduralStatements,
        TaskDeclaration,
    },
    constants::VerilogConstant,
    delay::{Delay, GateDelay},
    expr::Expression,
    gates::{DriveStrength, GateInstantiation, GateKind, StrengthLevel},
    generate::{DefparamAssignment, GenerateBlock, GenerateItem, GenerateLoop},
    identifier::Identifier,
    modules::{
        ModuleInitArguments, ModuleInstantiation, NetType, Port, PortDirection, VerilogModule,
    },
    nets::NetType as WireKind,
    operators::BinaryOperator,
    parameter::ParameterDeclaration,
    preprocessor::Timescale,
    primitive::UdpTable,
    simple::Range,
    specify::{SpecParam, SpecParamValue},
    statements::ModuleStatement,
};
use crate::register::{Register, ONE, ZERO};
use crate::simulator::eval::{eval, expression_width};
use crate::simulator::events::{control_fires, signals_read, SignalEdge};
use crate::simulator::exec::{drive, range_width, resolve_target, ResolvedTarget};
use crate::simulator::gates::{Gate, PassSwitch};
use crate::simulator::program::{
    block_scope, FrameVariable, FunctionDefinition, Instruction, Program, TaskDefinition,
    TaskParameter, TaskTable, FUNCTION_DELAY_UNSUPPORTED, FUNCTION_EVENT_UNSUPPORTED,
};
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::{array_depth, StateStore};

use crate::simulator::udp::Udp;

/// A task that prints *now* is fine inside a function body — the frame carries
/// the design's [`Output`](crate::simulator::tasks::Output) handle, so the line
/// lands where and when it was printed. Everything else in the family belongs
/// to a timestep or to a file the frame would take with it: a `$strobe` and a
/// `$monitor` report at the end of a timestep the call is long gone by, a
/// `$readmemh` writes a memory the frame does not hold, and `$finish` sets a
/// mark the driver reads off its own context. Each of those would be swallowed,
/// so each is still named here.
const FUNCTION_TASK_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a deferred system task inside a function");

/// A non-blocking assignment defers its write past the end of the call, and a
/// call ends the moment the expression around it needs the value.
const FUNCTION_NONBLOCKING_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a non-blocking assignment inside a function");

/// A `force` or a procedural `assign` installs a drive that outlives the
/// statement, and a call's frame does not outlive the call — so a drive
/// installed inside one would be discarded before it could ever be re-evaluated.
const FUNCTION_DRIVE_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a `force` or procedural `assign` inside a function");

/// The `$random` stream lives on the store a frame is *copied* from, so a draw
/// made inside a call would be lost with the frame and the next call would draw
/// the same number again.
const FUNCTION_RANDOM_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("`$random` inside a function");

/// What a function body that enables another instance's task reports. A task
/// may consume time and a function may not, so a function that enables one is
/// illegal Verilog — and a frame has no driver behind it to suspend on.
const FUNCTION_ENABLE_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a task enable inside a function");

/// How wide a `time` variable is. A `time` counts simulated time and is
/// defined to be 64 bits, unsigned — the one fixed width beside an `integer`'s
/// 32.
const TIME_RANGE: (i64, i64) = (63, 0);

/// The most words a memory may declare.
///
/// A memory is `n` real registers, so a nonsense dimension is an allocation the
/// host has to make before anything can go wrong with it. This is well past any
/// memory a design plausibly declares and well short of exhausting memory.
const MAX_MEMORY_DEPTH: usize = 1 << 20;

/// The most iterations one `generate` loop may unroll.
///
/// Every iteration is a real copy of the body in the flat model, so a bound
/// that never goes false is an allocation nothing survives rather than a hang.
/// This is well past any design that plausibly elaborates.
const MAX_GENERATE_ITERATIONS: usize = 4096;

/// How many times one module may appear on the path from the top down to what
/// is being walked.
///
/// A module *may* instantiate itself: IEEE 1364-2005 allows it as long as a
/// `generate` condition stops the recursion, which is how a design writes a
/// tree of adders (`sum #(n/2, width)` beside `sum #(n-n/2, width)`, corpus
/// `pr2728812a`). So "this module is already on the stack" is not the
/// question — the question is whether the recursion *terminates*, and a depth
/// bound is what answers it without evaluating the generate conditions twice.
/// It counts repeats of one module rather than the stack's length, so an
/// ordinary deep hierarchy is not limited by it.
///
/// It is deliberately small: `walk` recurses on the host's own stack, so a
/// bound big enough to be generous is a stack overflow rather than an error.
const MAX_INSTANTIATION_DEPTH: usize = 24;

/// How many instances one design may elaborate to in total.
///
/// A depth bound alone bounds the shape of a recursion but not the work it
/// does: a module that instantiates *itself twice* and terminates at depth
/// twenty is 2**20 real instances, every one of them declared into the flat
/// store. Counting the instances is what bounds that, and it is well past any
/// design that plausibly elaborates.
const MAX_INSTANCES: usize = 65_536;

/// A memory bigger than [`MAX_MEMORY_DEPTH`] words.
const MEMORY_TOO_LARGE: SimulationError =
    SimulationError::Unsupported("a memory with more words than can be allocated");

/// What kind of procedural block a compiled program came from.
#[derive(Debug, PartialEq, Eq)]
pub enum BlockKind {
    Initial,
    Always,
}

/// A procedural block compiled to a resumable program, with every name it
/// touches already resolved into the flat store.
pub struct TimedBlock {
    pub kind: BlockKind,
    /// `always` with no event control at all: driven by time, never by edges.
    pub free_running: bool,
    /// The block's trigger. Owned rather than borrowed from the module, both
    /// because the names in it have been rewritten and because holding a
    /// reference would borrow the module for the simulator's lifetime.
    pub control: EventControl,
    /// The `@(*)` read set, computed once here rather than on every delta
    /// cycle. Empty for the other two trigger forms, which never consult it.
    pub implicit_reads: BTreeSet<String>,
    /// Every signal the body assigns, computed once here.
    ///
    /// A block is sensitive only while it is *parked* at its event control, so
    /// a write it makes on its way through cannot wake it: the block is not
    /// waiting when the event happens, and by the time it comes back the event
    /// is in the past. This is the set the driver measures that with — see
    /// [`Simulator::settle`](crate::simulator::runner::Simulator). Empty for
    /// an `@(*)` block, which already leaves its own targets out of its read
    /// set, and for a free-running one, which edges never wake.
    pub writes: BTreeSet<String>,
    pub program: Program,
}

impl TimedBlock {
    /// Whether the edges observed this delta cycle wake this block.
    pub fn fires(&self, edges: &[SignalEdge], state: &StateStore) -> bool {
        control_fires(&self.control, edges, &self.implicit_reads, state)
    }
}

/// A whole hierarchy flattened into the pieces the simulator runs.
pub struct Elaborated {
    pub state: StateStore,
    pub assignments: Vec<ContinuousAssignment>,
    /// The gate primitives, which are continuous drivers and settle in the
    /// same fixpoint the assignments do.
    pub gates: Vec<Gate>,
    /// The user-defined primitives, which are continuous drivers beside the
    /// gates and settle in the same fixpoint.
    pub udps: Vec<Udp>,
    /// The bidirectional pass switches. One is not a driver at all — it joins
    /// two nets into a node whose drivers resolve *together*, which is why it
    /// is held apart from [`Elaborated::gates`].
    pub pass_switches: Vec<PassSwitch>,
    /// The names a gate drives. Those nets are *resolved* between all their
    /// continuous drivers instead of being written by whichever one ran last,
    /// which is the only way a three-state bus or a `pullup` can mean
    /// anything. Empty for a design with no gates in it, which is what keeps
    /// the question off the propagation hot path.
    pub resolved_nets: HashSet<String>,
    /// Nets that drive themselves: `supply0`/`supply1`, which sit at their
    /// rail at `supply` strength, and `tri0`/`tri1`, which are pulled to a
    /// value at `pull` strength so any real driver overrides them.
    ///
    /// Held as one more contribution rather than as a value written into the
    /// store, because that is exactly what they are — a permanent driver — and
    /// it is what lets `resolve_bit` decide between them and everything else
    /// without a second rule.
    pub pulled_nets: Vec<PulledNet>,
    pub blocks: Vec<TimedBlock>,
    /// The *top* module's input ports, the only ones a testbench may drive.
    pub inputs: Vec<String>,
    /// Qualified name to the store entry it aliases, for ports that were bound
    /// to a parent signal and so have no entry of their own.
    pub aliases: HashMap<String, String>,
    /// Every module instance, by the hierarchical name a design writes it
    /// under — `top`, `top.dut`, `top.mid.leaf` — paired with the
    /// `` `timescale `` its module was written at.
    ///
    /// Flattening throws hierarchy away everywhere else, and this is the one
    /// thing that has to survive it: `$printtimescale` is a question *about*
    /// the hierarchy, and a store key alone cannot say which module a signal
    /// came from. The top module is the root and carries the name it was
    /// declared with, where its store entries carry no prefix at all.
    pub instances: Vec<(String, Option<Timescale>)>,
}

/// A net that drives itself, and the bit and strength it drives at.
#[derive(Debug, Clone)]
pub struct PulledNet {
    pub name: String,
    /// The raw four-state bit code this net holds when nothing else drives it.
    pub code: u8,
    pub strength: StrengthLevel,
}

/// Flattens `modules[top]` and everything it instantiates.
pub fn elaborate(modules: &[VerilogModule], top: usize) -> Result<Elaborated, SimulationError> {
    let mut elaborator = Elaborator {
        modules,
        out: Elaborated {
            state: StateStore::new(),
            assignments: Vec::new(),
            gates: Vec::new(),
            udps: Vec::new(),
            pass_switches: Vec::new(),
            resolved_nets: HashSet::new(),
            pulled_nets: Vec::new(),
            blocks: Vec::new(),
            inputs: Vec::new(),
            aliases: HashMap::new(),
            instances: vec![(modules[top].identifier.name.clone(), modules[top].timescale)],
        },
        stack: Vec::new(),
        walked: 0,
        defparams: BTreeMap::new(),
        blocks_generated: 0,
        hierarchical_tasks: HashMap::new(),
    };
    elaborator.walk(top, &mut Scope::root(&modules[top].identifier.name))?;
    // An enable of another instance's task could not be spliced where it was
    // written, because the instance may be created further down the caller's
    // own source. Every one of them is linked here, where the whole hierarchy
    // is in hand.
    elaborator.link_hierarchical_enables()?;
    // A `defparam` is consumed by the instantiation it names. One that is
    // still here named nothing, and an override that quietly did not happen
    // leaves the design running at a width it was told not to use.
    if let Some(path) = elaborator.defparams.keys().next() {
        return Err(SimulationError::UnappliedDefparam(path.clone()));
    }
    // A reference the design wrote to an *aliased port* names an entry the
    // store does not have, and it could not be answered while the hierarchy
    // was being walked — see `resolve_aliased_references`.
    elaborator.resolve_aliased_references();
    elaborator.resolve_multiply_driven_nets();
    Ok(elaborator.out)
}

/// How a child port was connected by its parent.
#[derive(Clone)]
enum Binding {
    /// A plain identifier: the port *is* the parent's signal.
    Alias(String),
    /// A general expression, already rewritten into the flat name space.
    Driven(Expression),
    /// An output bound to something that is not a plain signal but can still
    /// be written — a bit or part select of a parent vector. The port gets a
    /// signal of its own and a continuous assignment carries it outwards,
    /// which is the alias run backwards.
    Driving(Expression),
    /// An `inout` bound to something that is not a plain signal.
    ///
    /// Neither direction of the alias will do: the port is read *and* written,
    /// and a continuous assignment only runs one way. The port gets a signal
    /// of its own and each of its bits is **bonded** to the matching bit of
    /// the connection, which is the node model a `tran` already has — so the
    /// two are not copied into each other, their drivers are pooled.
    Bonded(Expression),
}

/// One instance's — or one generate block's — view of the flat name space.
///
/// A module instance and a generate block are the same kind of thing to the
/// flat model: both give the names inside them a dotted prefix. They differ in
/// what "inside" means. A module cannot see out of itself, so *everything* it
/// names is its own; a generate block is a nested scope, so only the names it
/// declares are, and the rest belong to the module around it. That difference
/// is [`locals`](Scope::locals), and it is the whole of it.
#[derive(Clone)]
struct Scope {
    /// Where a name declared *here* goes: `""` for the top module, `"dut."`
    /// for its instance `dut`, and `"dut.stage[0]."` inside a generate block
    /// within that instance.
    prefix: String,
    /// The prefix of the enclosing module *instance*, which is where a name a
    /// generate block does not declare belongs. Equal to `prefix` outside one.
    module_prefix: String,
    /// This module's port names, as its parent connected them.
    bindings: HashMap<String, Binding>,
    /// Parameter values the parent overrode, already evaluated in the parent's
    /// scope.
    overrides: HashMap<String, Register>,
    /// What the generate blocks in scope declare, to the store entries they
    /// took. Empty outside a generate block, which is what keeps an ordinary
    /// module's resolution exactly what it was.
    locals: HashMap<String, String>,
    /// The genvars the generate loops in scope have bound. A genvar is an
    /// elaboration-time integer and never reaches the [`StateStore`].
    genvars: HashMap<String, i64>,
    /// The top module's name with a `.` on it, which is how an *absolute*
    /// hierarchical name is spelled — `main.dut.count`. The top module is the
    /// root of the flat name space and carries no prefix of its own, so that
    /// leading segment is dropped rather than kept.
    root_name: String,
    /// The `#(...)` on the instantiation that made this scope, when the module
    /// it names is a **primitive** — where it is a delay rather than a
    /// parameter override. `None` for an ordinary module, which reads the same
    /// tokens as `overrides`.
    primitive_delay: Option<GateDelay>,
}

impl Scope {
    fn root(module: &str) -> Self {
        Scope {
            prefix: String::new(),
            module_prefix: String::new(),
            bindings: HashMap::new(),
            overrides: HashMap::new(),
            locals: HashMap::new(),
            genvars: HashMap::new(),
            root_name: format!("{}.", module),
            primitive_delay: None,
        }
    }

    /// The hierarchical name of `block` written inside this scope, which is
    /// what `%m` prints: the top module's own name, then the instance path,
    /// then the named blocks. `""` is the module scope itself.
    fn hierarchy(&self, block: &str) -> String {
        let path = format!("{}{}{}", self.root_name, self.prefix, block);
        path.trim_end_matches('.').to_string()
    }

    fn is_root(&self) -> bool {
        self.prefix.is_empty() && self.locals.is_empty()
    }

    /// Whether a name written in this scope can come out different.
    ///
    /// Not the same question as [`is_root`](Scope::is_root): the top module
    /// has no prefix, but a name written *inside* it may still start at the
    /// top module by name and have that segment dropped.
    fn needs_renaming(&self) -> bool {
        !self.is_root() || !self.root_name.is_empty()
    }

    /// The store entry a name written inside this module refers to.
    ///
    /// A name a generate block in scope declares takes that block's prefix,
    /// and it is looked up by the *head* of the name so that a hierarchical
    /// reference into a nested block — `inner[0].sig` — is qualified by the
    /// block that declares `inner`. An aliased port resolves to the parent's
    /// signal — possibly one the parent itself aliased, so a chain of
    /// connections collapses to the single signal at the top of it. Everything
    /// else is local to the module and takes the instance's prefix.
    fn resolve(&self, local: &str) -> String {
        if !self.locals.is_empty() {
            let head = match local.find(|c| c == '.' || c == '[') {
                Some(at) => &local[..at],
                None => local,
            };
            if let Some(full) = self.locals.get(head) {
                return format!("{}{}", full, &local[head.len()..]);
            }
        }
        // An absolute name starts at the top module, which is the root of the
        // flat name space — `main.dut.count` is the store's `dut.count`. A
        // scope of its own shadows it, which is why this is asked second.
        if let Some(rest) = local.strip_prefix(self.root_name.as_str()) {
            return rest.to_string();
        }
        match self.bindings.get(local) {
            Some(Binding::Alias(outer)) => outer.clone(),
            _ => {
                let mut name = String::with_capacity(self.module_prefix.len() + local.len());
                name.push_str(&self.module_prefix);
                name.push_str(local);
                name
            }
        }
    }

    /// The dotted name of something local to this instance.
    fn qualified(&self, local: &str) -> String {
        let mut name = String::with_capacity(self.prefix.len() + local.len());
        name.push_str(&self.prefix);
        name.push_str(local);
        name
    }
}

struct Elaborator<'m> {
    modules: &'m [VerilogModule],
    out: Elaborated,
    /// Module indices on the path from the top down to what is being walked
    /// now. A module that reaches itself through this more than
    /// [`MAX_INSTANTIATION_DEPTH`] times is recursing without terminating,
    /// which no amount of flattening can settle.
    stack: Vec<usize>,
    /// The `defparam` overrides seen so far, by the flat name of the parameter
    /// each one addresses. An instantiation takes the ones that name it; what
    /// is left over at the end named nothing and is reported.
    defparams: BTreeMap<String, Register>,
    /// How many instances have been walked, which is what bounds a recursion
    /// that *branches* — see [`MAX_INSTANCES`].
    walked: usize,
    /// How many unnamed generate blocks have been given a `genblk` number.
    /// A block with no label still needs a scope — two iterations of an
    /// unnamed loop body would otherwise declare the same names twice.
    blocks_generated: usize,
    /// Every task in the design under the flat path a hierarchical enable
    /// resolves to — `j.set`, `main.test1` — with its body already renamed
    /// into the names of the instance that declares it.
    ///
    /// A task's own table is keyed by the bare name an enable inside its module
    /// spells, and its body is left unresolved so the block it is spliced into
    /// can resolve the whole thing at once. Neither works across instances, so
    /// this is the second copy: qualified where it is declared, and spliced
    /// without renaming by [`Elaborator::link_hierarchical_enables`].
    hierarchical_tasks: HashMap<String, TaskDefinition>,
}

impl<'m> Elaborator<'m> {
    fn walk(&mut self, index: usize, scope: &mut Scope) -> Result<(), SimulationError> {
        let modules = self.modules;
        let module = &modules[index];

        self.walked += 1;
        if self.walked > MAX_INSTANCES
            || self.stack.iter().filter(|walked| **walked == index).count()
                >= MAX_INSTANTIATION_DEPTH
        {
            return Err(SimulationError::RecursiveInstantiation(
                module.identifier.name.clone(),
            ));
        }
        self.stack.push(index);

        // A user-defined primitive is a module as far as instantiation and port
        // binding go, and nothing else: its whole body is the table, so none of
        // the passes below have anything to walk.
        if let Some(table) = primitive_table(module) {
            for port in &module.ports {
                self.declare_port(port, scope, module.unconnected_drive)?;
            }
            self.build_udp(module, table, scope)?;
            self.stack.pop();
            return Ok(());
        }

        // Declarations first, in several passes, so that an instantiation can
        // connect to a net declared further down the file — Verilog puts no
        // ordering requirement on module items.
        //
        // Parameters come before everything else because a declared width may
        // be *made of* them: `output [WIDTH-1:0] q` has no width until `WIDTH`
        // has a value. Their own order among themselves is kept, since a
        // parameter may be written in terms of the one above it.
        //
        // A parameter's value may in turn be a call to one of the module's
        // functions, and a function's return width may be a parameter — so the
        // two are circular in general. The knot is cut by evaluating the
        // parameters first, holding back any that could not be evaluated,
        // compiling the functions, and then retrying the ones held back. A
        // parameter that still cannot be evaluated then reports the reason it
        // could not, which is what it would have reported the first time.
        let deferred = self.declare_parameters(module, scope)?;
        // Tasks sit between the parameters and the functions. A task's argument
        // widths may be made of parameters, so it cannot be compiled before
        // them; and a function is compiled against the task table so that a
        // task enable written inside one is not read as a name nothing
        // declares.
        let tasks = self.declare_tasks(module, scope)?;
        let own_functions: Vec<(&ModuleStatement, &Scope)> = module
            .statements
            .iter()
            .filter(|statement| matches!(statement, ModuleStatement::FunctionDeclaration(_)))
            .map(|statement| (statement, &*scope))
            .collect();
        self.declare_functions(&own_functions, &tasks)?;
        for statement in deferred {
            self.declare(statement, scope)?;
        }
        // A port whose width does not match what the parent bound it to cannot
        // be aliased, so this runs where both widths are first known: the
        // parameters above are what give the port's range a value.
        self.reconcile_port_widths(module, scope)?;
        for port in &module.ports {
            self.declare_port(port, scope, module.unconnected_drive)?;
        }
        // A `defparam` overrides a parameter of an instance this module has yet
        // to create, so it is collected before the build pass reaches that
        // instantiation and applied where the instance is made. Its value is
        // written in *this* module's terms, so it is evaluated here, where the
        // parameters it may name already have values.
        self.collect_defparams(&module.statements, scope)?;

        // A generate region is unrolled once the parameters and the functions
        // are known — a loop bound, an `if` condition and a `case` subject are
        // made of them — and before the declaration passes, because what a
        // region unrolls *to* is declarations. The items it yields then go
        // through exactly the two passes the module's own statements do, each
        // in the scope its block gave it.
        let mut generated: Vec<(&ModuleStatement, Scope)> = Vec::new();
        for statement in &module.statements {
            if let ModuleStatement::GenerateRegion(items) = statement {
                self.expand_generate(items, scope, &mut generated)?;
            }
        }

        // A function declared *inside* a generate block belongs to that block,
        // so it cannot be compiled until the region has been unrolled and the
        // block has a scope. The module's own are compiled a second time
        // alongside it rather than being kept from the first round, because
        // `close_reads` has to see the whole call graph at once — a block's
        // function may call one of the module's, and the outer call's frame has
        // to hold what the inner one reads. A design with no function in a
        // generate block does none of this and pays one `Iterator::any`.
        if generated
            .iter()
            .any(|(statement, _)| matches!(statement, ModuleStatement::FunctionDeclaration(_)))
        {
            let all: Vec<(&ModuleStatement, &Scope)> = module
                .statements
                .iter()
                .filter(|statement| matches!(statement, ModuleStatement::FunctionDeclaration(_)))
                .map(|statement| (statement, &*scope))
                .chain(
                    generated
                        .iter()
                        .map(|(statement, inner)| (*statement, inner)),
                )
                .collect();
            self.declare_functions(&all, &tasks)?;
        }
        // A task declared inside a generate block belongs to that block for the
        // same reason, and is compiled here for it. A design with none gets an
        // empty map and every generated statement builds against the module's
        // own table.
        let block_tasks = self.declare_generated_tasks(&generated, &tasks)?;

        for statement in &module.statements {
            if !matches!(statement, ModuleStatement::ParameterDeclaration(_)) {
                self.declare(statement, scope)?;
            }
        }
        for (statement, inner) in &generated {
            self.declare(statement, inner)?;
        }
        // An undeclared name wired to an instance, to a gate or to the left of
        // an `assign` is an *implicit net*, which is a declaration and not a
        // mistake. It runs after everything explicit, because it is only the
        // names nothing declared that it creates, and before the build pass,
        // because that is what looks them up.
        for statement in &module.statements {
            self.declare_implicit_nets(statement, scope);
        }
        for (statement, inner) in &generated {
            self.declare_implicit_nets(statement, inner);
        }
        // A port's default value — `output reg [31:0] x = 1;` — is applied
        // here rather than in `declare_port`, because a `reg` in the body
        // naming the same port is a second declaration that runs in between
        // and would overwrite it with the `x` an undriven variable starts at.
        // The split is the one a body declaration already makes: a variable
        // port takes the value once, a net port takes it as a continuous
        // assignment and follows its operands for the whole run.
        for port in &module.ports {
            let Some(init) = &port.init else { continue };
            if port_is_variable(port) {
                self.initialise(&port.identifier.name, init, scope)?;
            } else {
                let target =
                    Expression::Identifier(Identifier::new(scope.resolve(&port.identifier.name)));
                self.out
                    .assignments
                    .push(ContinuousAssignment::new(target, renamed(init, scope)));
            }
        }
        for statement in &module.statements {
            self.build(statement, scope, &tasks)?;
        }
        for (statement, inner) in &generated {
            let table = block_tasks.get(&inner.prefix).unwrap_or(&tasks);
            self.build(statement, inner, table)?;
        }

        self.stack.pop();
        Ok(())
    }

    /// Records the `defparam`s written in this scope, keyed by the flat name of
    /// the parameter each one addresses.
    fn collect_defparams(
        &mut self,
        statements: &'m [ModuleStatement],
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        for statement in statements {
            if let ModuleStatement::Defparam(assignments) = statement {
                self.record_defparams(assignments, scope)?;
            }
        }
        Ok(())
    }

    fn record_defparams(
        &mut self,
        assignments: &[DefparamAssignment],
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        for assignment in assignments {
            let value = eval(&renamed(&assignment.value, scope), &self.out.state)?;
            self.defparams
                .insert(scope.resolve(&assignment.path), value);
        }
        Ok(())
    }

    /// Unrolls a generate region into the module items it describes, each
    /// paired with the scope its block gives it.
    ///
    /// Nothing here runs: a region is a *description* of what the module
    /// contains, and unrolling it is choosing which description. That is why it
    /// happens at elaboration and not in the parser — a loop bound may be a
    /// parameter, and a parameter has no value until now.
    fn expand_generate(
        &mut self,
        items: &'m [GenerateItem],
        scope: &Scope,
        out: &mut Vec<(&'m ModuleStatement, Scope)>,
    ) -> Result<(), SimulationError> {
        for item in items {
            match item {
                GenerateItem::Item(statement) => match statement {
                    // A parameter declared inside a block is evaluated as the
                    // block unrolls rather than in the declaration pass that
                    // follows, because a nested loop's bound may be made of it.
                    ModuleStatement::ParameterDeclaration(_) => self.declare(statement, scope)?,
                    ModuleStatement::Defparam(assignments) => {
                        self.record_defparams(assignments, scope)?
                    }
                    _ => out.push((statement, scope.clone())),
                },
                GenerateItem::Block(block) => self.expand_block(block, None, scope, out)?,
                GenerateItem::Loop(repeated) => self.expand_loop(repeated, scope, out)?,
                GenerateItem::If(branch) => {
                    let taken = if self.generate_condition(&branch.condition, scope)? {
                        Some(&branch.then_block)
                    } else {
                        branch.else_block.as_ref()
                    };
                    if let Some(block) = taken {
                        self.expand_block(block, None, scope, out)?;
                    }
                }
                GenerateItem::Case(choice) => {
                    let mut chosen: Option<&'m GenerateBlock> = None;
                    let mut fallback: Option<&'m GenerateBlock> = None;
                    for arm in &choice.items {
                        if arm.labels.is_empty() {
                            fallback = Some(&arm.block);
                            continue;
                        }
                        for label in &arm.labels {
                            // Identity, not equality: a generate `case` picks
                            // its arm the way a `case` statement does, so an `x`
                            // label matches an `x` subject and nothing else.
                            let test = Expression::Binary(
                                Box::new(choice.subject.clone()),
                                BinaryOperator::CaseEquality,
                                Box::new(label.clone()),
                            );
                            if self.generate_condition(&test, scope)? {
                                chosen = Some(&arm.block);
                                break;
                            }
                        }
                        if chosen.is_some() {
                            break;
                        }
                    }
                    if let Some(block) = chosen.or(fallback) {
                        self.expand_block(block, None, scope, out)?;
                    }
                }
            }
        }
        Ok(())
    }

    /// Unrolls one generate block into the scope its label gives it.
    fn expand_block(
        &mut self,
        block: &'m GenerateBlock,
        label: Option<&str>,
        scope: &Scope,
        out: &mut Vec<(&'m ModuleStatement, Scope)>,
    ) -> Result<(), SimulationError> {
        let numbered;
        let label = match label {
            Some(label) => label,
            None => {
                numbered = self.block_label(block);
                &numbered
            }
        };
        let inner = self.generate_scope(scope, label, &block.items);
        self.expand_generate(&block.items, &inner, out)
    }

    /// Unrolls `for (i = 0; i < N; i = i + 1) begin : stage … end`.
    ///
    /// The genvar is bound to the iteration's value in a scope of its own, so
    /// every expression the body holds — a range bound, a port connection, an
    /// index inside an `always` block — is handed the *integer* rather than a
    /// name it could look up. Nothing of the genvar survives into the run.
    fn expand_loop(
        &mut self,
        repeated: &'m GenerateLoop,
        scope: &Scope,
        out: &mut Vec<(&'m ModuleStatement, Scope)>,
    ) -> Result<(), SimulationError> {
        if repeated.genvar != repeated.step_variable {
            return Err(SimulationError::GenerateLoopVariable {
                init: repeated.genvar.name.clone(),
                step: repeated.step_variable.name.clone(),
            });
        }
        let label = self.block_label(&repeated.body);
        let mut value = self.generate_value(&repeated.init, scope)?;
        for _ in 0..MAX_GENERATE_ITERATIONS {
            let mut bound = scope.clone();
            bound.genvars.insert(repeated.genvar.name.clone(), value);
            if !self.generate_condition(&repeated.condition, &bound)? {
                return Ok(());
            }
            let indexed = format!("{}[{}]", label, value);
            self.expand_block(&repeated.body, Some(&indexed), &bound, out)?;
            value = self.generate_value(&repeated.step, &bound)?;
        }
        Err(SimulationError::GenerateLoopBound {
            limit: MAX_GENERATE_ITERATIONS,
        })
    }

    /// The label a generate block's scope takes.
    ///
    /// A named block keeps its name, because that is how a testbench reaches
    /// inside it — `stage[0].u.count`. An unnamed one is numbered instead: it
    /// still needs a scope of its own, since two iterations of an unnamed loop
    /// body would otherwise declare the same names twice.
    fn block_label(&mut self, block: &GenerateBlock) -> String {
        match &block.name {
            Some(name) => name.name.clone(),
            None => {
                self.blocks_generated += 1;
                format!("genblk{}", self.blocks_generated)
            }
        }
    }

    /// The scope a generate block's items are elaborated in: the enclosing one
    /// with the block's prefix put on, and the block's own declarations
    /// recorded so that they resolve into it while everything else still
    /// resolves outwards.
    fn generate_scope(&self, parent: &Scope, label: &str, items: &[GenerateItem]) -> Scope {
        let mut inner = parent.clone();
        inner.prefix = format!("{}{}.", parent.prefix, label);
        for name in declared_names(items) {
            let full = format!("{}{}", inner.prefix, name);
            inner.locals.insert(name, full);
        }
        inner
    }

    /// A generate control expression, evaluated where it was written.
    fn generate_eval(
        &self,
        expression: &Expression,
        scope: &Scope,
    ) -> Result<Register, SimulationError> {
        eval(&renamed(expression, scope), &self.out.state).map_err(|why| {
            SimulationError::UnresolvedGenerate {
                expression: expression.to_contracted_string(),
                why: why.to_string(),
            }
        })
    }

    /// The same, as the integer a loop bound has to be.
    fn generate_value(
        &self,
        expression: &Expression,
        scope: &Scope,
    ) -> Result<i64, SimulationError> {
        let value = self.generate_eval(expression, scope)?;
        let wide = if value.is_signed() {
            value.to_i128()
        } else {
            value.to_u128().and_then(|value| i128::try_from(value).ok())
        };
        wide.and_then(|value| i64::try_from(value).ok())
            .ok_or_else(|| SimulationError::UnresolvedGenerate {
                expression: expression.to_contracted_string(),
                why: "it does not evaluate to an integer".to_string(),
            })
    }

    /// Whether a generate condition selects its branch. An `x` or a `z` is
    /// false, exactly as it is for an `if` statement.
    fn generate_condition(
        &self,
        expression: &Expression,
        scope: &Scope,
    ) -> Result<bool, SimulationError> {
        Ok(self.generate_eval(expression, scope)?.has_one())
    }

    /// Evaluates every `parameter` and `localparam` this module declares,
    /// in the order they were written.
    ///
    /// A declaration that cannot be evaluated yet is handed back rather than
    /// reported: the only thing it can be waiting for is a function, which is
    /// compiled next. The caller retries it then, and a second failure is the
    /// error.
    fn declare_parameters(
        &mut self,
        module: &'m VerilogModule,
        scope: &Scope,
    ) -> Result<Vec<&'m ModuleStatement>, SimulationError> {
        let mut deferred = Vec::new();
        for statement in &module.statements {
            if !matches!(statement, ModuleStatement::ParameterDeclaration(_)) {
                continue;
            }
            if self.declare(statement, scope).is_err() {
                deferred.push(statement);
            }
        }
        Ok(deferred)
    }

    /// The two numbers a declared range describes.
    ///
    /// A literal range was folded where it was written and costs nothing here.
    /// An expression range is evaluated against the store, which by this point
    /// holds the parameters in scope — including any the parent overrode, which
    /// is the whole point: an override is allowed to change a child's widths.
    fn resolve_range(&self, range: &Range, scope: &Scope) -> Result<(i64, i64), SimulationError> {
        match range {
            Range::Constant(msb, lsb) => Ok((*msb, *lsb)),
            Range::Expressions(msb, lsb) => Ok((
                self.resolve_bound(msb, renamed(msb, scope))?,
                self.resolve_bound(lsb, renamed(lsb, scope))?,
            )),
        }
    }

    /// [`resolve_range`](Elaborator::resolve_range) for a declaration written
    /// inside a subprogram or a named block, where a bound may name one of the
    /// scope's own constants: `parameter width = 8; reg [width-1:0] mem …;`
    /// (corpus `pr2132552`). A constant the scope declares takes the scope's
    /// dotted prefix; anything else resolves outwards to the module.
    fn resolve_scoped_range(
        &self,
        range: &Range,
        scope: &Scope,
        inner: &str,
        own: &BTreeSet<&str>,
    ) -> Result<(i64, i64), SimulationError> {
        let resolved = |bound: &Expression| scoped_renamed(bound, scope, inner, own);
        match range {
            Range::Constant(msb, lsb) => Ok((*msb, *lsb)),
            Range::Expressions(msb, lsb) => Ok((
                self.resolve_bound(msb, resolved(msb))?,
                self.resolve_bound(lsb, resolved(lsb))?,
            )),
        }
    }

    /// One bound of an expression range, as a number.
    ///
    /// Anything that is not a number — an unknown name, an `x`, a value too
    /// wide to be a bound — is a **named** error. A width the simulator picked
    /// for itself would be wrong for the whole run and look like nothing at
    /// all had gone wrong. `bound` is the bound as written, which is what the
    /// error names; `resolved` is it with every name resolved into the store.
    fn resolve_bound(
        &self,
        bound: &Expression,
        resolved: Expression,
    ) -> Result<i64, SimulationError> {
        let unresolved = |why: String| SimulationError::UnresolvedRange {
            bound: bound.to_contracted_string(),
            why,
        };
        let value = eval(&resolved, &self.out.state).map_err(|why| unresolved(why.to_string()))?;
        let wide = if value.is_signed() {
            value.to_i128()
        } else {
            value.to_u128().and_then(|value| i128::try_from(value).ok())
        };
        wide.and_then(|value| i64::try_from(value).ok())
            .ok_or_else(|| unresolved("it does not evaluate to an integer".to_string()))
    }

    /// Turns an alias whose two halves are **not the same number of bits**
    /// back into an ordinary connection, which is what makes the conversion
    /// between them happen.
    ///
    /// Aliasing is only sound when the port and the signal it is bound to are
    /// the same signal, and two different widths are not: `slower slwr(sres,
    /// sin);` for an `output signed [31:0] lrtn` bound to a
    /// `wire signed [63:0] sres` has to sign extend the port's thirty-two bits
    /// outwards, and one store entry has nowhere to do that. The port keeps an
    /// entry of its own and a continuous assignment carries the value across in
    /// the port's own direction — exactly what a port bound to an *expression*
    /// already does — so the sign, the padding and the truncation are the ones
    /// an assignment gives (corpus `pr2121536`, `pr2121536b`).
    ///
    /// It has to run here rather than where the binding is made, because a
    /// port's range is made of the child's parameters and they have only just
    /// been declared. An `inout` is left aliased whatever its width: it is read
    /// as well as written, and one assignment only runs one way.
    fn reconcile_port_widths(
        &mut self,
        module: &VerilogModule,
        scope: &mut Scope,
    ) -> Result<(), SimulationError> {
        for port in &module.ports {
            let local = &port.identifier.name;
            let Some(Binding::Alias(outer)) = scope.bindings.get(local) else {
                continue;
            };
            if matches!(port.direction, PortDirection::InOut) {
                continue;
            }
            let outer = outer.clone();
            let Some(signal) = self.out.state.get_signal(&outer) else {
                continue;
            };
            let outer_width = signal.width();
            let range = self.resolve_range(&port.range, scope)?;
            if range_width(range) == outer_width {
                continue;
            }
            // The assignment this becomes is a driver the *design* did not
            // write, and it is the only driver the elaborator adds on its own
            // account — so it cannot know whether the net it lands on is driven
            // already. A design that miswires a port's direction drives both
            // ends (iverilog coerces the port to `inout` and warns), and two
            // plain drivers of one net overwrite each other every pass and
            // never settle. Naming the net resolved is what sends both through
            // `resolve_contributions` instead, where an undriven `z`
            // contributes nothing and the real driver wins outright (corpus
            // `br_gh127c`, `br_gh127f`).
            let binding = match port.direction {
                PortDirection::Input => {
                    self.out.resolved_nets.insert(scope.qualified(local));
                    Binding::Driven(Expression::Identifier(Identifier::new(outer)))
                }
                _ => {
                    self.out.resolved_nets.insert(outer.clone());
                    Binding::Driving(Expression::Identifier(Identifier::new(outer)))
                }
            };
            scope.bindings.insert(local.clone(), binding);
            self.out
                .aliases
                .remove(&format!("{}{}", scope.prefix, local));
        }
        Ok(())
    }

    /// Declares one port, unless it was aliased onto a signal that already
    /// exists.
    fn declare_port(
        &mut self,
        port: &Port,
        scope: &Scope,
        unconnected: Option<bool>,
    ) -> Result<(), SimulationError> {
        let local = &port.identifier.name;
        match scope.bindings.get(local) {
            // The parent's signal *is* this port. Declaring it again would give
            // the port a second, immediately stale copy — but a port the child
            // backs with a `reg` is a *driver* of that signal, so its fill is
            // the one that stands.
            Some(Binding::Alias(target)) => {
                if port_is_variable(port) {
                    let target = target.clone();
                    self.out.state.redeclare_as_variable(&target);
                }
                return Ok(());
            }
            Some(Binding::Driven(expression)) => {
                let name = scope.qualified(local);
                let range = self.resolve_range(&port.range, scope)?;
                self.out.state.declare_net(name.clone(), range, port.signed);
                self.out.assignments.push(ContinuousAssignment::new(
                    Expression::Identifier(Identifier::new(name)),
                    expression.clone(),
                ));
                return Ok(());
            }
            Some(Binding::Driving(target)) => {
                let name = scope.qualified(local);
                let range = self.resolve_range(&port.range, scope)?;
                // The port has an entry of its own, so it is filled the way any
                // other declaration of its kind is: `output reg q` is a
                // variable and starts at `x`, a plain one is a net and starts
                // at `z`. A child that never drives it therefore carries `z`
                // outwards rather than `x`.
                if port_is_variable(port) {
                    self.out
                        .state
                        .declare_signed(name.clone(), range, port.signed);
                } else {
                    self.out.state.declare_net(name.clone(), range, port.signed);
                }
                self.out.assignments.push(ContinuousAssignment::new(
                    target.clone(),
                    Expression::Identifier(Identifier::new(name)),
                ));
                return Ok(());
            }
            Some(Binding::Bonded(target)) => {
                let target = target.clone();
                let name = scope.qualified(local);
                let range = self.resolve_range(&port.range, scope)?;
                self.out.state.declare_net(name.clone(), range, port.signed);
                return self.bond_port(&name, &target);
            }
            None => {}
        }

        let name = scope.qualified(local);
        let range = self.resolve_range(&port.range, scope)?;
        // `output reg q` is a variable and starts at `x`; a plain port is a
        // net and starts at `z`. A `reg` in the *body* naming a port says the
        // same thing, and that declaration runs after this one and overwrites
        // the fill, so both spellings land on `x`.
        if port_is_variable(port) {
            self.out
                .state
                .declare_signed(name.clone(), range, port.signed);
        } else {
            self.out.state.declare_net(name.clone(), range, port.signed);
        }

        if !matches!(port.direction, PortDirection::Input) {
            return Ok(());
        }
        if scope.is_root() {
            self.out.inputs.push(name);
        } else {
            // Nothing at all is driving this input, which is what `z` means —
            // unless the module was declared inside an
            // `` `unconnected_drive pull0 `` / `pull1` region, which says an
            // unconnected input of it reads that level instead (IEEE
            // 1364-2005 §19.9, corpus `uncon_drive`, `br_gh782c`). The
            // directive belongs to the module's *declaration*, not to the
            // instantiation, which is why it arrives with the port.
            let width = range_width(range);
            let floating = match unconnected {
                None => Register::high_impedance(width),
                Some(false) => Register::from_u128(0, width),
                Some(true) => Register::from_u128(u128::MAX, width),
            };
            self.out.state.set_ranged(name, floating, range);
        }
        Ok(())
    }

    /// Compiles every task this module declares and declares the variables its
    /// arguments and locals name.
    ///
    /// A task's storage is static — one set of variables per task, not per
    /// enable — so they are ordinary store entries under a dotted name:
    /// `load.data` for the argument `data` of the task `load`. The body spells
    /// them unqualified because it is spliced into a block that has yet to be
    /// resolved into this instance's names, and resolving twice would prefix
    /// them twice.
    ///
    /// A task may enable one declared further down the file, so this repeats
    /// until a pass compiles nothing new. A pass that makes no progress at all
    /// is a cycle in the enable graph, which inlining cannot terminate on.
    fn declare_tasks(
        &mut self,
        module: &VerilogModule,
        scope: &Scope,
    ) -> Result<TaskTable, SimulationError> {
        let declarations: Vec<&TaskDeclaration> = module
            .statements
            .iter()
            .filter_map(|statement| match statement {
                ModuleStatement::TaskDeclaration(task) => Some(task),
                _ => None,
            })
            .collect();
        let declared: BTreeSet<&str> = declarations
            .iter()
            .map(|task| task.name.name.as_str())
            .collect();

        let mut tasks = TaskTable::new();
        let mut pending = declarations.clone();
        while !pending.is_empty() {
            let waiting = pending.len();
            let mut deferred = Vec::new();
            for task in pending {
                match compile_task(task, &tasks) {
                    Ok(mut definition) => {
                        // A task's body is spliced into every enable of it, so
                        // its `%m` is qualified here — once, where the instance
                        // is known — rather than at each of them.
                        definition
                            .program
                            .qualify_scopes(&|block| scope.hierarchy(block));
                        tasks.insert(task.name.name.clone(), definition);
                    }
                    // The task it enables may be one this pass has not reached
                    // yet. A name this module never declares is a real error
                    // and is reported where it was found.
                    Err(SimulationError::UnknownTask(name)) if declared.contains(name.as_str()) => {
                        deferred.push(task)
                    }
                    Err(error) => return Err(error),
                }
            }
            if deferred.len() == waiting {
                return Err(SimulationError::RecursiveTask(
                    deferred[0].name.name.clone(),
                ));
            }
            pending = deferred;
        }

        for task in &declarations {
            self.register_task(task, &tasks[&task.name.name], scope)?;
        }

        Ok(tasks)
    }

    /// Declares the variables one compiled task names, and records the copy of
    /// it a hierarchical enable splices.
    ///
    /// The copy is the same body resolved into *this* scope's names, under the
    /// flat path an enable of it resolves to. It has to be taken here because
    /// this is the only moment the task and the scope it belongs to are both in
    /// hand; a block that enables one reaches it long afterwards, with a scope
    /// of its own.
    fn register_task(
        &mut self,
        task: &TaskDeclaration,
        definition: &TaskDefinition,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        // A named block inside a task body is scoped under the task, the same
        // way the body's instructions spell it.
        let inside = block_scope("", &task.name.name);
        // A task argument may be sized from a parameter — the module's,
        // `input [WIDTH-1:0] a;`, or the task's own, `parameter width = 8; reg
        // [width-1:0] mem [depth-1:0];` (corpus `pr2132552`) — and this is the
        // one place a task's widths are ever recorded.
        self.declare_scope(
            &task.parameters,
            task.arguments
                .iter()
                .map(|argument| &argument.variable)
                .chain(&task.locals),
            &task.events,
            scope,
            &inside,
        )?;
        self.declare_block_locals(&task.statements, scope, &inside)?;

        let mut qualified = definition.clone();
        if scope.needs_renaming() {
            qualified.program.rename(&|local| scope.resolve(local));
        }
        for argument in &mut qualified.arguments {
            argument.name = scope.resolve(&argument.name);
        }
        self.hierarchical_tasks
            .insert(scope.qualified(&task.name.name), qualified);
        Ok(())
    }

    /// Compiles the tasks declared inside generate blocks, once the region has
    /// been unrolled and each block has a scope, and hands back the table each
    /// such block's own statements are compiled against.
    ///
    /// A block's table is the module's with the block's tasks laid over it, so
    /// a bare enable written inside the block finds the block's task first —
    /// which is the whole of why a task in a block could not share the module's
    /// single table, keyed as it is by the bare name an enable spells. A task
    /// declared by a block is also stored under the block's prefix for a
    /// hierarchical enable (`gen.foo_task;`) exactly as the module's are under
    /// the instance's. A block nested inside another sees the module's tasks
    /// and its own, not its parent block's: a bare enable of one of those is
    /// `UnknownTask` rather than a guess.
    fn declare_generated_tasks(
        &mut self,
        generated: &[(&ModuleStatement, Scope)],
        tasks: &TaskTable,
    ) -> Result<HashMap<String, TaskTable>, SimulationError> {
        let mut tables: HashMap<String, TaskTable> = HashMap::new();
        for (statement, inner) in generated {
            let ModuleStatement::TaskDeclaration(task) = statement else {
                continue;
            };
            let table = tables
                .entry(inner.prefix.clone())
                .or_insert_with(|| tasks.clone());
            let mut definition = compile_task(task, table)?;
            definition
                .program
                .qualify_scopes(&|block| inner.hierarchy(block));
            // A task inside a generate loop may print or index by the loop's
            // genvar, which is a number here and nowhere else — every splice
            // of the body is one iteration's copy.
            if !inner.genvars.is_empty() {
                definition
                    .program
                    .substitute(&|expression| substitute_genvars(expression, &inner.genvars));
            }
            table.insert(task.name.name.clone(), definition.clone());
            self.register_task(task, &definition, inner)?;
        }
        Ok(tables)
    }

    /// Splices in every enable of another instance's task, repeating until
    /// nothing is left to splice.
    ///
    /// A body linked in may itself enable a third instance's task, which is
    /// what the loop is for — and what makes a *cycle* of them non-terminating,
    /// since every round of it is a real copy of every body in the cycle. The
    /// cycle is therefore found in the enable graph first, by name, exactly as
    /// `declare_tasks` finds a cycle among a module's own enables: after that
    /// the chain is finite and the loop ends on its own.
    fn link_hierarchical_enables(&mut self) -> Result<(), SimulationError> {
        if self
            .out
            .blocks
            .iter()
            .all(|block| first_hierarchical_enable(&block.program).is_none())
        {
            return Ok(());
        }
        if let Some(name) = recursive_enable(&self.hierarchical_tasks) {
            return Err(SimulationError::RecursiveTask(name));
        }
        for block in 0..self.out.blocks.len() {
            let mut linked = false;
            while self.out.blocks[block]
                .program
                .link_hierarchical_enables(&self.hierarchical_tasks, block)?
            {
                linked = true;
            }
            if linked {
                self.relist_block_names(block);
            }
        }
        Ok(())
    }

    /// Recomputes what a block reads and writes after a body was linked into
    /// it.
    ///
    /// Both sets were taken where the block was built, when the enable was
    /// still a marker holding nothing but a path — so an `@(*)` block that
    /// enables another instance's task would not wake on what that body reads,
    /// and an edge-triggered one would wake on what it writes. This is the same
    /// question `build` asks of a *local* enable, asked again now that the body
    /// is really there.
    fn relist_block_names(&mut self, block: usize) {
        let names = BodyNames::of(&self.out.blocks[block].program);
        let timed = &mut self.out.blocks[block];
        match timed.control {
            EventControl::Implicit => timed.implicit_reads.extend(names.reads),
            EventControl::Events(_) => {
                timed.writes = written_names(&timed.program);
            }
            EventControl::None => {}
        }
    }

    /// Declares the variables every named block inside a procedural body
    /// holds.
    ///
    /// A block's name is a scope, and its variables are ordinary store entries
    /// under a dotted name — `block_id.tmp`, qualified per instance like
    /// everything else — which is exactly what a task's are. `scope` walks down
    /// with the blocks, so a block inside a block is `outer.inner.tmp` and two
    /// blocks that name a variable the same way are two variables.
    fn declare_block_locals(
        &mut self,
        statements: &[ProceduralStatements],
        scope: &Scope,
        block: &str,
    ) -> Result<(), SimulationError> {
        for statement in statements {
            match statement {
                ProceduralStatements::Block(inner) | ProceduralStatements::Fork(inner) => {
                    let nested = match &inner.name {
                        Some(name) => block_scope(block, &name.name),
                        None => block.to_string(),
                    };
                    // The constants go in first: a local's width may be made of
                    // one, exactly as a module's parameters precede its own
                    // declarations.
                    self.declare_scope(
                        &inner.parameters,
                        inner.locals.iter(),
                        &inner.events,
                        scope,
                        &nested,
                    )?;
                    self.declare_block_locals(&inner.statements, scope, &nested)?;
                }
                ProceduralStatements::If(conditional) => {
                    self.declare_block_locals(&conditional.then_statements, scope, block)?;
                    if let Some(otherwise) = &conditional.else_statements {
                        self.declare_block_locals(otherwise, scope, block)?;
                    }
                }
                ProceduralStatements::Case(case) => {
                    for item in &case.items {
                        self.declare_block_locals(&item.statements, scope, block)?;
                    }
                }
                ProceduralStatements::For(loop_) => {
                    self.declare_block_locals(&loop_.statements, scope, block)?
                }
                ProceduralStatements::While(loop_) => {
                    self.declare_block_locals(&loop_.statements, scope, block)?
                }
                ProceduralStatements::Repeat(loop_) => {
                    self.declare_block_locals(&loop_.statements, scope, block)?
                }
                ProceduralStatements::Wait(statement) => {
                    self.declare_block_locals(&statement.statements, scope, block)?
                }
                ProceduralStatements::Forever(statements)
                | ProceduralStatements::Delayed { statements, .. }
                | ProceduralStatements::EventControlled { statements, .. } => {
                    self.declare_block_locals(statements, scope, block)?
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Compiles every function this module declares and puts it in the store
    /// under its qualified name, so a call anywhere in the design finds it.
    ///
    /// They are staged into one map first because a function may call a sibling
    /// declared further down the file, and closing each one's read set over
    /// what it calls needs all of them in hand.
    fn declare_functions(
        &mut self,
        declarations: &[(&ModuleStatement, &Scope)],
        tasks: &TaskTable,
    ) -> Result<(), SimulationError> {
        let mut staged: BTreeMap<String, FunctionDefinition> = BTreeMap::new();
        for (statement, scope) in declarations {
            if let ModuleStatement::FunctionDeclaration(function) = statement {
                // A function's constants and named events are *not* frame
                // variables: a constant outlives every call and an event is not
                // a value at all, so both are ordinary store entries under the
                // function's dotted name and a call copies the constant in with
                // everything else it reads.
                let inner = block_scope("", &function.name.name);
                self.declare_scope(
                    &function.parameters,
                    std::iter::empty(),
                    &function.events,
                    scope,
                    &inner,
                )?;
                let definition = self.compile_function(function, scope, tasks)?;
                staged.insert(definition.result.name.clone(), definition);
            }
        }
        close_reads(&mut staged);
        for (name, definition) in staged {
            self.out.state.declare_function(name, definition);
        }
        Ok(())
    }

    /// Compiles one function body and works out the frame a call to it needs.
    ///
    /// The body is renamed like any other compiled program, but through a
    /// resolver of two minds: a name the function itself declares — its own
    /// name, an argument, a local — becomes a frame variable, and everything
    /// else resolves into the design's flat store the way the rest of the
    /// module does.
    fn compile_function(
        &self,
        function: &FunctionDeclaration,
        scope: &Scope,
        tasks: &TaskTable,
    ) -> Result<FunctionDefinition, SimulationError> {
        let qualified = scope.qualified(&function.name.name);

        // The function's own name is the variable its body assigns to return a
        // value, so it is a frame variable like the arguments and the locals.
        let mut frame_names: HashMap<&str, String> = HashMap::new();
        frame_names.insert(function.name.name.as_str(), qualified.clone());

        // A width inside the function may be made of one of its own constants,
        // which `declare_functions` has already put in the store.
        let inner = block_scope("", &function.name.name);
        let own: BTreeSet<&str> = function
            .parameters
            .iter()
            .map(|parameter| parameter.name.name.as_str())
            .collect();
        let variable = |variable: &FunctionVariable| {
            // An address dimension makes the local a *memory*, which the frame
            // declares in its memory map. Without it `tmp[1] = …` would write a
            // bit of a scalar where the design wrote a whole word — corpus
            // `constfunc15` and `br_gh674`.
            let dimensions = match &variable.dimensions {
                Some(addresses) => {
                    let addresses = self.resolve_scoped_range(addresses, scope, &inner, &own)?;
                    if range_width(addresses) > MAX_MEMORY_DEPTH {
                        return Err(MEMORY_TOO_LARGE);
                    }
                    Some(addresses)
                }
                None => None,
            };
            Ok(FrameVariable {
                name: format!("{}.{}", qualified, variable.name.name),
                range: self.resolve_scoped_range(&variable.range, scope, &inner, &own)?,
                dimensions,
                signed: variable.signed,
                real: variable.real,
            })
        };
        let arguments: Vec<FrameVariable> = function
            .arguments
            .iter()
            .map(variable)
            .collect::<Result<_, SimulationError>>()?;
        let locals: Vec<FrameVariable> = function
            .locals
            .iter()
            .map(variable)
            .collect::<Result<_, SimulationError>>()?;
        for (declared, frame) in function
            .arguments
            .iter()
            .chain(&function.locals)
            .zip(arguments.iter().chain(&locals))
        {
            frame_names.insert(declared.name.name.as_str(), frame.name.clone());
        }

        // What the *frame* holds is settled here, before the constants and the
        // events are added to the rename: those live in the design's store, so
        // a call copies the constant in with everything else it reads rather
        // than declaring one of its own.
        let own: BTreeSet<String> = frame_names.values().cloned().collect();
        for local in function
            .parameters
            .iter()
            .map(|parameter| parameter.name.name.as_str())
            .chain(function.events.iter().map(|event| event.name.as_str()))
        {
            frame_names.insert(local, format!("{}.{}", qualified, local));
        }

        let mut program = Program::compile(&function.statements, tasks)?;
        // A function inside a generate loop may read the loop's genvar, which
        // is a number rather than a name — so it is substituted before the
        // rename, which would otherwise qualify it into a signal nothing
        // declares.
        if !scope.genvars.is_empty() {
            program.substitute(&|expression| substitute_genvars(expression, &scope.genvars));
        }
        program.rename(&|name| match frame_names.get(name) {
            Some(qualified) => qualified.clone(),
            None => scope.resolve(name),
        });

        let names = analyse_function_body(&program, &own)?;

        Ok(FunctionDefinition {
            result: FrameVariable {
                name: qualified,
                range: self.resolve_range(&function.range, scope)?,
                dimensions: None,
                signed: function.signed,
                real: function.real,
            },
            arguments,
            locals,
            reads: names.reads,
            writes: names.writes,
            calls: names.calls,
            program,
        })
    }

    /// The first pass: everything that brings a name into existence.
    fn declare(
        &mut self,
        statement: &ModuleStatement,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        match statement {
            ModuleStatement::WireDeclaration(nets) => {
                for net in nets {
                    let range = self.resolve_range(net.range(), scope)?;
                    let local = &net.identifier().name;
                    // An address dimension makes the name an *array of nets*,
                    // which is a memory in the store exactly as `reg [7:0]
                    // mem [0:15];` is — the same distinction, recorded the
                    // same way. A pull belongs to a net that has a value, so
                    // an array does not take one.
                    if net.dimensions().is_empty() {
                        self.declare_local_net(local, range, net.is_signed(), scope);
                        self.record_pull(local, net.kind(), scope);
                    } else {
                        let addresses = self.resolve_dimensions(net.dimensions(), scope)?;
                        self.declare_net_memory(local, addresses, range, net.is_signed(), scope)?;
                    }
                }
            }
            ModuleStatement::RegisterDeclaration(registers) => {
                for register in registers {
                    let range = match &register.range {
                        Some(range) => self.resolve_range(range, scope)?,
                        None => (0, 0),
                    };
                    // The address dimensions are what make the name a memory
                    // rather than a vector, and they are the only place that
                    // distinction is ever recorded.
                    if register.dimensions.is_empty() {
                        self.declare_local(&register.name.name, range, register.signed, scope);
                    } else {
                        let addresses = self.resolve_dimensions(&register.dimensions, scope)?;
                        self.declare_memory(
                            &register.name.name,
                            addresses,
                            range,
                            register.signed,
                            scope,
                        )?;
                    }
                }
            }
            ModuleStatement::IntegerDeclaration(integers) => {
                for declaration in integers {
                    // An `integer` is a 32 bit *signed* variable. Signedness is
                    // part of what the keyword means, so there is no qualifier
                    // to read here — it is always true.
                    if declaration.dimensions.is_empty() {
                        self.declare_local(&declaration.name.name, (31, 0), true, scope);
                    } else {
                        let addresses = self.resolve_dimensions(&declaration.dimensions, scope)?;
                        self.declare_memory(
                            &declaration.name.name,
                            addresses,
                            (31, 0),
                            true,
                            scope,
                        )?;
                    }
                }
            }
            ModuleStatement::TimeDeclaration(times) => {
                for declaration in times {
                    // A `time` is 64 bits wide and unsigned; like an `integer`
                    // the keyword is the whole of its type, so there is no
                    // range or qualifier to read.
                    if declaration.dimensions.is_empty() {
                        self.declare_local(&declaration.name.name, TIME_RANGE, false, scope);
                    } else {
                        let addresses = self.resolve_dimensions(&declaration.dimensions, scope)?;
                        self.declare_memory(
                            &declaration.name.name,
                            addresses,
                            TIME_RANGE,
                            false,
                            scope,
                        )?;
                    }
                }
            }
            ModuleStatement::RealDeclaration(reals) => {
                for declaration in reals {
                    // A `real` has no declarable width — the type is the whole
                    // of it, the way an `integer`'s 32 bits are — so there is
                    // no range to resolve, only the array dimensions.
                    if declaration.dimensions.is_empty() {
                        self.out
                            .state
                            .declare_real(scope.qualified(&declaration.name.name));
                    } else {
                        let addresses = self.resolve_dimensions(&declaration.dimensions, scope)?;
                        self.declare_real_memory(&declaration.name.name, addresses, scope)?;
                    }
                }
            }
            ModuleStatement::EventDeclaration(events) => {
                for declaration in events {
                    // An event is neither a signal nor a memory: it holds no
                    // value, so it goes into a namespace of its own and
                    // reading it is an error rather than a number.
                    self.out
                        .state
                        .declare_event(scope.qualified(&declaration.name.name));
                }
            }
            ModuleStatement::ParameterDeclaration(parameters) => {
                for parameter in parameters {
                    let local = &parameter.name.name;
                    // An override was evaluated in the parent's scope, where the
                    // expression it came from was written.
                    let value = match scope.overrides.get(local) {
                        Some(value) => value.clone(),
                        None => eval(&renamed(&parameter.value, scope), &self.out.state)?,
                    };
                    let name = scope.qualified(local);
                    let range = match &parameter.range {
                        Some(range) => Some(self.resolve_range(range, scope)?),
                        None => None,
                    };
                    self.record_parameter(parameter, name, value, range);
                }
            }
            ModuleStatement::SpecifyBlock(block) => {
                // A `specparam` is a constant like a `parameter`, and the module
                // around the block may name it — so it is declared rather than
                // discarded. The paths and the timing checks beside it are
                // recorded by the parser and never reach the simulation; see
                // `parsers/specify.rs` for why.
                for parameter in &block.specparams {
                    self.declare_specparam(parameter, scope)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Records one parameter's value in the store under `name`.
    ///
    /// The declaration's own rules — what it is signed by, whether it is a
    /// real, what width it is coerced to — live here and nowhere else, so a
    /// parameter written inside a task or a named block cannot end up meaning
    /// something different from one written at module level.
    fn record_parameter(
        &mut self,
        parameter: &ParameterDeclaration,
        name: String,
        value: Register,
        range: Option<(i64, i64)>,
    ) {
        // A `signed` qualifier (or an `integer` type) makes the parameter
        // signed. Failing that, **a range is what decides**: a parameter
        // written with one is unsigned unless it says otherwise, and only a
        // rangeless one keeps the signedness its value arrived with. That is
        // IEEE 1364-2005 and it is what iverilog 12.0 does — `parameter [3:0]
        // DAC = 8;` is 8, where reading the bare decimal's own signedness makes
        // it -8 and `pm_next_st[DAC]` selects a bit nothing has (corpus
        // `pr542`).
        //
        // The flag is applied on both sides of `coerced` deliberately: it is
        // read *before*, to widen by sign extension rather than zero
        // extension, and rebuilding a register does not carry it, so it has to
        // be restated *after* or the stored parameter reads unsigned.
        let signed = parameter.signed || (parameter.range.is_none() && value.is_signed());
        // A `real` parameter holds a double whatever its value was written as,
        // so `parameter real HALF = 1;` is `1.0` and not one bit. It is the
        // declaration that says so, exactly as it does for a `real` variable.
        let value = if parameter.real && !value.is_real() {
            Register::from_f64(value.to_f64())
        } else {
            value
        };
        let value = value.with_signedness(signed);
        // A real has no width to be coerced to; a range beside one would be a
        // declaration of something else.
        let value = match range {
            Some(range) if !value.is_real() => value.coerced(range_width(range)),
            _ => value,
        }
        .with_signedness(signed);
        match range {
            // A real carries its own sixty-four bits, so a range written beside
            // one says nothing about it.
            Some(range) if !value.is_real() => self.out.state.set_ranged(name, value, range),
            _ => self.out.state.set(name, value),
        }
    }

    /// Declares what a subprogram or a named block declares inside itself —
    /// its constants, its variables and its named events — under the scope's
    /// own dotted name: `test_task.depth`, `dut.my_block.p`.
    ///
    /// The constants go in **first**, before the scope's variables, for the reason the module's
    /// own parameters go in before everything else: `reg [width-1:0] mem
    /// [depth-1:0];` has no width until `width` has a value. A value that names
    /// a sibling constant resolves to the scope's own entry — `localparam l = p
    /// + 1;` means *this* `p` — and anything else resolves outwards to the
    /// module, which is exactly what a nested scope means.
    ///
    /// A `defparam` naming one is applied here, because the path it writes
    /// (`sub.my_block.p`) is already the flat spelling the constant ends up
    /// under: the two meet with no translation, the same way an instance's do.
    ///
    /// The variables follow. The address dimension is what makes one a
    /// **memory**, and it lands in the store's memory map rather than its
    /// signal map — a name is in one or the other and never both, which is the
    /// whole of what tells `mem[3]` a word from `a[3]` a bit. Without it
    /// `tempram[i] = i;` would write a *bit* of a scalar (corpus `task_mem`).
    ///
    /// The events come last. An event has no value at all, so it goes into the
    /// store's third, valueless namespace under the scope's dotted name — which
    /// is also the name a testbench reaches it by (`-> sub.my_block.trigger;`,
    /// corpus `scoped_events`).
    fn declare_scope<'v>(
        &mut self,
        parameters: &[ParameterDeclaration],
        variables: impl Iterator<Item = &'v FunctionVariable>,
        events: &[Identifier],
        scope: &Scope,
        inner: &str,
    ) -> Result<(), SimulationError> {
        let own: BTreeSet<&str> = parameters
            .iter()
            .map(|parameter| parameter.name.name.as_str())
            .collect();
        for parameter in parameters {
            let name = scope.qualified(&format!("{}{}", inner, parameter.name.name));
            let value = match self.defparams.remove(&name) {
                Some(value) => value,
                None => eval(
                    &scoped_renamed(&parameter.value, scope, inner, &own),
                    &self.out.state,
                )?,
            };
            let range = match &parameter.range {
                Some(range) => Some(self.resolve_scoped_range(range, scope, inner, &own)?),
                None => None,
            };
            self.record_parameter(parameter, name, value, range);
        }

        for variable in variables {
            let range = self.resolve_scoped_range(&variable.range, scope, inner, &own)?;
            let name = scope.qualified(&format!("{}{}", inner, variable.name.name));
            match (&variable.dimensions, variable.real) {
                (Some(addresses), real) => {
                    let addresses = self.resolve_scoped_range(addresses, scope, inner, &own)?;
                    if range_width(addresses) > MAX_MEMORY_DEPTH {
                        return Err(MEMORY_TOO_LARGE);
                    }
                    // A local declares one dimension, where a module-level
                    // array may declare several.
                    match real {
                        true => self.out.state.declare_real_memory(name, vec![addresses]),
                        false => self.out.state.declare_memory(
                            name,
                            vec![addresses],
                            range,
                            variable.signed,
                        ),
                    }
                }
                // A `real` is declared as one, so a value copied into it is
                // converted rather than reinterpreted.
                (None, true) => self.out.state.declare_real(name),
                (None, false) => self.out.state.declare_signed(name, range, variable.signed),
            }
        }

        for event in events {
            self.out
                .state
                .declare_event(scope.qualified(&format!("{}{}", inner, event.name)));
        }
        Ok(())
    }

    /// Declares one `specparam` as the constant it is.
    ///
    /// A real-valued one is a real constant: `specparam tRise = 0.9;` declares
    /// a `real` holding `0.9`, so a module that names it in an expression gets
    /// the number it was written with. The parser keeps the *text* of one —
    /// see `parsers/specify.rs` — because a path delay has no evaluator behind
    /// it, so the conversion happens here.
    fn declare_specparam(
        &mut self,
        parameter: &SpecParam,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        let expression = match &parameter.value {
            SpecParamValue::Expression(expression) => expression,
            SpecParamValue::Real(text) => {
                let name = scope.qualified(&parameter.name.name);
                let value = text.replace('_', "").parse::<f64>().map_err(|_| {
                    SimulationError::Unsupported("a `specparam` real value that is not a number")
                })?;
                self.out.state.set(name, Register::from_f64(value));
                return Ok(());
            }
        };
        let value = eval(&renamed(expression, scope), &self.out.state)?;
        let name = scope.qualified(&parameter.name.name);
        match &parameter.range {
            Some(range) => {
                let range = self.resolve_range(range, scope)?;
                let value = value.coerced(range_width(range));
                self.out.state.set_ranged(name, value, range);
            }
            None => self.out.state.set(name, value),
        }
        Ok(())
    }

    /// Declares a signal local to this instance.
    ///
    /// A redeclaration of an aliased port (`output q;` followed by `reg q;`)
    /// keeps the parent's entry — resetting it to `x` at the *child's* width
    /// would clobber the width aliasing gave it — but it does say the signal is
    /// a **variable**, so an undriven one reads `x` rather than the `z` the
    /// parent's `wire` filled it with. The `reg` is a driver of that net, and
    /// what a driver has not said is `x` (corpus `pr1792108`, `pr1645518`).
    fn declare_local(&mut self, local: &str, range: (i64, i64), signed: bool, scope: &Scope) {
        if let Some(Binding::Alias(target)) = scope.bindings.get(local) {
            let target = target.clone();
            self.out.state.redeclare_as_variable(&target);
            return;
        }
        self.out
            .state
            .declare_signed(scope.qualified(local), range, signed);
    }

    /// [`declare_local`](Elaborator::declare_local) for a net, which starts at
    /// `z` rather than `x` — see [`StateStore::declare_net`].
    fn declare_local_net(&mut self, local: &str, range: (i64, i64), signed: bool, scope: &Scope) {
        if matches!(scope.bindings.get(local), Some(Binding::Alias(_))) {
            return;
        }
        self.out
            .state
            .declare_net(scope.qualified(local), range, signed);
    }

    /// Declares the implicit nets one module item asks for.
    ///
    /// IEEE 1364-2005 §4.5: a name nothing declares becomes a *scalar* net of
    /// the default type where it is wired to a module instance's port, to a
    /// gate or primitive terminal, or to the left of a continuous assignment.
    /// `assign w = 1'b1;` with no `wire w;` above it is a design and not a
    /// mistake, and iverilog 12.0 reads all three that way — a connection
    /// expression too, so `.a(yy + 1)` for an undeclared `yy` reads `z` rather
    /// than refusing to elaborate.
    ///
    /// The width is one bit even when the port it feeds is wider, which is what
    /// iverilog does: `sub u(w, d);` against a four bit port warns and pads.
    ///
    /// The name is declared under exactly what [`Scope::resolve`] answers for
    /// it, which is what the build pass asks for a moment later — declaring it
    /// any other way could disagree with the lookup that follows.
    fn declare_implicit_nets(&mut self, statement: &ModuleStatement, scope: &Scope) {
        let mut names: Vec<&str> = Vec::new();
        match statement {
            ModuleStatement::Assignment(assignments) => {
                for assignment in assignments {
                    if let Expression::Identifier(id) = assignment.lhs() {
                        names.push(&id.name);
                    }
                }
            }
            ModuleStatement::GateInstantiation(instances) => {
                for instance in instances {
                    for terminal in &instance.instance.terminals {
                        operand_names(terminal, &mut names);
                    }
                }
            }
            ModuleStatement::ModuleInstantiation(instantiation) => match &instantiation.arguments {
                ModuleInitArguments::NoArgs => {}
                ModuleInitArguments::Positional(connections) => {
                    for connection in connections.iter().flatten() {
                        operand_names(connection, &mut names);
                    }
                }
                ModuleInitArguments::Keyword(connections) => {
                    for connection in connections.values() {
                        operand_names(connection, &mut names);
                    }
                }
            },
            _ => {}
        }
        for local in names {
            // A hierarchical name reaches something another scope declares, so
            // there is nothing here to declare for it; a genvar is an
            // elaboration-time integer and never reaches the store at all.
            if local.contains('.') || scope.genvars.contains_key(local) {
                continue;
            }
            let name = scope.resolve(local);
            if self.out.state.contains(&name)
                || self.out.state.memory(&name).is_some()
                || self.out.state.is_event(&name)
            {
                continue;
            }
            self.out.state.declare_net(name, (0, 0), false);
        }
    }

    /// The address dimensions of an array declaration, each resolved against
    /// the parameters in scope the way a width is.
    ///
    /// The bound a design writes is an expression — `reg [7:0] a [0:N-1][0:3];`
    /// — so every dimension goes through
    /// [`resolve_range`](Elaborator::resolve_range) and a bound that is not a
    /// constant is the same named error a width's is.
    fn resolve_dimensions(
        &mut self,
        dimensions: &[Range],
        scope: &Scope,
    ) -> Result<Vec<(i64, i64)>, SimulationError> {
        dimensions
            .iter()
            .map(|dimension| self.resolve_range(dimension, scope))
            .collect()
    }

    /// Declares a memory local to this instance: `reg [7:0] mem [0:255];`, or
    /// `reg [7:0] a [0:3][0:15];` for one of more than one dimension.
    ///
    /// A memory cannot be a port, so there is no aliasing to reconcile the way
    /// [`declare_local`](Elaborator::declare_local) has to.
    fn declare_memory(
        &mut self,
        local: &str,
        addresses: Vec<(i64, i64)>,
        range: (i64, i64),
        signed: bool,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        Elaborator::check_depth(&addresses)?;
        self.out
            .state
            .declare_memory(scope.qualified(local), addresses, range, signed);
        Ok(())
    }

    /// [`declare_memory`](Elaborator::declare_memory) for an array of `real`s,
    /// whose words start at `0.0` because a double has no `x`.
    fn declare_real_memory(
        &mut self,
        local: &str,
        addresses: Vec<(i64, i64)>,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        Elaborator::check_depth(&addresses)?;
        self.out
            .state
            .declare_real_memory(scope.qualified(local), addresses);
        Ok(())
    }

    /// [`declare_memory`](Elaborator::declare_memory) for an array of nets,
    /// whose undriven words read `z` rather than `x`.
    fn declare_net_memory(
        &mut self,
        local: &str,
        addresses: Vec<(i64, i64)>,
        range: (i64, i64),
        signed: bool,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        Elaborator::check_depth(&addresses)?;
        self.out
            .state
            .declare_net_memory(scope.qualified(local), addresses, range, signed);
        Ok(())
    }

    /// Refuses an array with more words than [`MAX_MEMORY_DEPTH`] before one is
    /// allocated.
    ///
    /// It is the **product** of the dimensions that is measured, because that
    /// is how many words a multi-dimensional array really holds: four
    /// dimensions each within the bound can still ask for more memory than the
    /// machine has.
    fn check_depth(addresses: &[(i64, i64)]) -> Result<(), SimulationError> {
        if array_depth(addresses) > MAX_MEMORY_DEPTH {
            return Err(MEMORY_TOO_LARGE);
        }
        Ok(())
    }

    /// Elaborates one gate primitive instance into the flat driver list.
    ///
    /// An *array* of instances — `bufif1 drv [7:0] (bus, data, enable);` — is
    /// expanded here into one gate per index, because nothing downstream has a
    /// notion of an instance at all. A terminal one bit wide is shared by every
    /// instance and a terminal as wide as the array is sliced a bit at a time,
    /// which is what the LRM asks for; the two are told apart by width, and a
    /// terminal that is neither is a named error rather than a silent
    /// misconnection.
    fn build_gate(
        &mut self,
        gate: &GateInstantiation,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        let strength = gate.drive_strength();
        let terminals: Vec<Expression> = gate
            .instance
            .terminals
            .iter()
            .map(|terminal| renamed(terminal, scope))
            .collect();
        // `#(PERIOD)` on a gate names a parameter, which belongs to the
        // instance that declared it like every other name the gate holds.
        let delay = gate.delay.as_ref().map(|delay| {
            let mut delay = delay.clone();
            for expression in delay.expressions_mut() {
                *expression = renamed(expression, scope);
            }
            delay
        });
        let Some(range) = &gate.instance.range else {
            return self.push_primitive(gate.kind, strength, terminals, delay);
        };
        // An array's bounds are a `Range` like any declaration's, so
        // `buf drv [N-1:0] (…)` is sized by the parameters in scope.
        let count = range_width(self.resolve_range(range, scope)?);
        for position in 0..count {
            let sliced = terminals
                .iter()
                .map(|terminal| self.array_terminal(gate.kind, terminal, position, count))
                .collect::<Result<Vec<Expression>, SimulationError>>()?;
            self.push_primitive(gate.kind, strength, sliced, delay.clone())?;
        }
        Ok(())
    }

    /// Records one primitive instance, as whichever of the two shapes its
    /// keyword names.
    ///
    /// A **bidirectional** switch has no output terminal, so it is not a driver
    /// and cannot be a [`Gate`]: it joins two nets into a node instead. That
    /// one question is the whole of the difference, and it is asked here so
    /// that an arrayed instantiation goes through it too.
    fn push_primitive(
        &mut self,
        kind: GateKind,
        strength: DriveStrength,
        terminals: Vec<Expression>,
        delay: Option<GateDelay>,
    ) -> Result<(), SimulationError> {
        if kind.is_bidirectional() {
            let switch = PassSwitch::new(kind, terminals)?;
            // Both terminals are resolved nets: every driver of either one has
            // to reach `resolve_contributions` rather than write the store, or
            // the node's pool would be missing it.
            for terminal in &switch.terminals {
                if let Some(name) = assigned_name(terminal) {
                    self.out.resolved_nets.insert(name.to_string());
                }
            }
            self.out.pass_switches.push(switch);
            return Ok(());
        }
        let gate = Gate::new(kind, strength, terminals, delay)?;
        self.push_gate(gate);
        Ok(())
    }

    /// Records a continuous assignment, and the net it drives when it declared
    /// a strength.
    ///
    /// An `assign` that named no strength drives at `strong` and is the only
    /// kind of driver the simulator has ever had, so it keeps being written
    /// straight into the store. One that *did* name a strength has to go
    /// through resolution even when it is the net's only driver, because
    /// `highz` is a half that does not drive at all:
    /// `assign (strong1, highz0) x = 4'b1010;` is `1z1z`, and only
    /// [`resolve_bit`](crate::simulator::gates::resolve_bit) knows that.
    fn push_assignment(&mut self, assignment: ContinuousAssignment) {
        if assignment.strength().is_some() {
            if let Some(name) = assigned_name(assignment.lhs()) {
                self.out.resolved_nets.insert(name.to_string());
            }
        }
        self.out.assignments.push(assignment);
    }

    /// Marks every net that more than one continuous assignment drives, so
    /// that the drivers are *resolved* against each other rather than written
    /// one after the other.
    ///
    /// Two drivers writing the same net in turn is not merely imprecise, it
    /// does not settle: `assign blend = foo; assign blend = bar;` for a `foo`
    /// of `x` and a `bar` of `1` has each pass undo the one before it, so the
    /// fixpoint runs out of passes and the design fails as
    /// `NoConvergence` (corpus `pr1701921`, `pr2013758`, `con_tri`,
    /// `gen_case_opt1`, `drive_strength2`, `pr2219441b`, `pr2715558`).
    /// Resolution answers instead: the bits the drivers agree on pass through
    /// and the ones they do not are `x`, which is what iverilog 12.0 gives.
    ///
    /// It is counted once here rather than asked per push, because the count
    /// is only complete when the whole hierarchy has been walked — one
    /// driver may be an instance's `Binding::Driving` and the other the
    /// parent's own `assign`.
    /// Re-points every reference the design wrote to an *aliased port* at the
    /// entry that port aliases.
    ///
    /// A port bound to a plain identifier is one store entry with the parent's
    /// signal, so the port's own qualified name — `u_bar.x`, `l.p` — has no
    /// entry of its own at all. `Scope::resolve` cannot answer for it while the
    /// hierarchy is being walked: `instantiate` records the alias from the
    /// **build** pass, in source order, so a block written above the
    /// instantiation is compiled and renamed before the table has ever heard of
    /// the name. Asking again here, once everything has been walked, is what
    /// makes `if (x !== u_bar.x)` read one signal twice rather than one signal
    /// and a name nothing declares.
    ///
    /// It is a rewrite rather than a lookup the store falls back on, because
    /// everything else about a name here is settled statically and a run-time
    /// indirection would be the odd one out. One pass settles every reference:
    /// an alias *target* is never itself an alias key, since `Binding::Alias`
    /// collapses a chain of connections to the signal at the top of it where
    /// the entry is recorded.
    ///
    /// Every collection the elaboration produces goes through it, because a
    /// hierarchical reference is legal wherever a name is and one left out
    /// would be silent.
    fn resolve_aliased_references(&mut self) {
        if self.out.aliases.is_empty() {
            return;
        }
        let aliases = self.out.aliases.clone();
        let resolve = |name: &str| match aliases.get(name) {
            Some(entry) => entry.clone(),
            None => name.to_string(),
        };
        let rename = |expression: &mut Expression| rename_expression(expression, &resolve);
        let rename_names = |names: &mut BTreeSet<String>| {
            *names = std::mem::take(names)
                .into_iter()
                .map(|name| resolve(&name))
                .collect();
        };

        for assignment in &mut self.out.assignments {
            let mut lhs = assignment.lhs().clone();
            let mut rhs = assignment.rhs().clone();
            rename(&mut lhs);
            rename(&mut rhs);
            *assignment = ContinuousAssignment::with_timing(
                lhs,
                rhs,
                assignment.strength(),
                assignment.delay().cloned(),
            );
        }
        for gate in &mut self.out.gates {
            for terminal in gate.outputs.iter_mut().chain(&mut gate.inputs) {
                rename(terminal);
            }
        }
        for udp in &mut self.out.udps {
            rename(&mut udp.output);
            for input in &mut udp.inputs {
                rename(input);
            }
        }
        for switch in &mut self.out.pass_switches {
            for terminal in &mut switch.terminals {
                rename(terminal);
            }
            if let Some((control, _)) = &mut switch.control {
                rename(control);
            }
        }
        for net in &mut self.out.pulled_nets {
            net.name = resolve(&net.name);
        }
        self.out.resolved_nets = std::mem::take(&mut self.out.resolved_nets)
            .into_iter()
            .map(|name| resolve(&name))
            .collect();
        for input in &mut self.out.inputs {
            *input = resolve(input);
        }
        for block in &mut self.out.blocks {
            block.program.rename(&resolve);
            if let EventControl::Events(events) = &mut block.control {
                for event in events {
                    rename(&mut event.expression);
                }
            }
            rename_names(&mut block.implicit_reads);
            rename_names(&mut block.writes);
        }
        // A function body may name an instance's port as readily as a block
        // can, and its read set is what a call copies into its frame.
        for definition in self.out.state.functions_mut().values_mut() {
            definition.program.rename(&resolve);
            rename_names(&mut definition.reads);
            rename_names(&mut definition.writes);
        }
    }

    fn resolve_multiply_driven_nets(&mut self) {
        let mut seen: HashSet<&str> = HashSet::new();
        let mut twice: HashSet<String> = HashSet::new();
        for assignment in &self.out.assignments {
            let Some(name) = assigned_name(assignment.lhs()) else {
                continue;
            };
            if !seen.insert(name) {
                twice.insert(name.to_string());
            }
        }
        self.out.resolved_nets.extend(twice);
    }

    /// Records a net that drives itself — `supply0`/`supply1` at their rail,
    /// `tri0`/`tri1` pulled to a value that any real driver overrides.
    ///
    /// A pulled net is one more contribution, so it also has to be *resolved*
    /// rather than written: `tri0 c; assign c = d;` is `0` when `d` is `z` and
    /// `1` when `d` is `1`, which only `resolve_bit` can say.
    fn record_pull(&mut self, local: &str, net_type: WireKind, scope: &Scope) {
        let (code, strength) = match net_type {
            WireKind::Supply0 => (ZERO, StrengthLevel::Supply),
            WireKind::Supply1 => (ONE, StrengthLevel::Supply),
            WireKind::Tri0 => (ZERO, StrengthLevel::Pull),
            WireKind::Tri1 => (ONE, StrengthLevel::Pull),
            _ => return,
        };
        // A net that is also a port bound to a parent signal has no entry of
        // its own, so the pull belongs on the entry it aliases — otherwise it
        // names a signal the store does not have and setup fails.
        let name = match scope.bindings.get(local) {
            Some(Binding::Alias(target)) => target.clone(),
            _ => scope.qualified(local),
        };
        self.out.resolved_nets.insert(name.clone());
        self.out.pulled_nets.push(PulledNet {
            name,
            code,
            strength,
        });
    }

    /// Elaborates one user-defined primitive instance into the flat driver
    /// list.
    ///
    /// A UDP's terminals are its ports, so they are already bound: the output
    /// is the first one and the inputs are the rest, each resolved through the
    /// scope the way any other name in this instance is.
    fn build_udp(
        &mut self,
        module: &VerilogModule,
        table: &UdpTable,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        // A sequential UDP keeps a state and remembers its inputs between
        // lookups. Both live on the instance, so from here on it is an
        // ordinary continuous driver like a combinational one.
        let memory = Udp::memory_for(table, &self.out.state)?;
        let mut terminals = module.ports.iter().map(|port| {
            Expression::Identifier(Identifier::new(scope.resolve(&port.identifier.name)))
        });
        // The parser guarantees an output followed by at least one input.
        let output = terminals.next().expect("a primitive declares an output");
        let inputs: Vec<Expression> = terminals.collect();
        // A UDP drives its net the way a gate does, so that net is resolved
        // between all of its drivers rather than written by the last one.
        if let Some(name) = assigned_name(&output) {
            self.out.resolved_nets.insert(name.to_string());
        }
        self.out.udps.push(Udp {
            name: module.identifier.name.clone(),
            output,
            inputs,
            table: table.clone(),
            memory,
            // A UDP is a continuous driver exactly as a gate is, so its delay
            // is the same `GateDelay` and goes through the same machinery.
            delay: scope.primitive_delay.clone(),
        });
        Ok(())
    }

    /// Joins each bit of an `inout` port to the matching bit of what the
    /// parent bound it to.
    ///
    /// A port bound to a plain identifier is *aliased* — it and the parent's
    /// signal are one store entry — and a select cannot be, because the port
    /// and `bus[0]` are not one entry. An **output** bound to one is carried
    /// out by a continuous assignment, but an `inout` is read as well as
    /// written and one assignment only runs one way. So the port keeps a
    /// signal of its own and the two are made **one node**, bit by bit, which
    /// is exactly what a `tran` between them would mean: the drivers of both
    /// are pooled and resolved together, rather than either side's value being
    /// copied into the other. Copying is the shape that looks right and is
    /// wrong for the reason [`PassSwitch`] gives — once the value has been
    /// copied, a driver letting go leaves the far side holding it.
    ///
    /// Both sides are walked from their **least significant** end, so a
    /// connection narrower than the port leaves the port's high bits joined to
    /// nothing — which is what an unconnected bit of a net already is.
    fn bond_port(&mut self, name: &str, target: &Expression) -> Result<(), SimulationError> {
        let outer = self.bit_expressions(target)?;
        let inner =
            self.bit_expressions(&Expression::Identifier(Identifier::new(name.to_string())))?;
        for (port_bit, outer_bit) in inner.into_iter().rev().zip(outer.into_iter().rev()) {
            let switch = PassSwitch::new(GateKind::Tran, vec![port_bit, outer_bit])?;
            // Both terminals are resolved nets, for the reason a `tran`'s are:
            // every driver of either one has to reach the node's pool rather
            // than write the store.
            for terminal in &switch.terminals {
                if let Some(name) = assigned_name(terminal) {
                    self.out.resolved_nets.insert(name.to_string());
                }
            }
            self.out.pass_switches.push(switch);
        }
        Ok(())
    }

    /// The bits an expression names, each as the one-bit select that names it,
    /// most significant first.
    ///
    /// It goes through `resolve_target` so that a name, a select and a
    /// concatenation of those are all answered by the production that already
    /// decides which bits an assignment writes — the two cannot then disagree
    /// about which bit of `{qh, Q}` is which.
    fn bit_expressions(&self, expression: &Expression) -> Result<Vec<Expression>, SimulationError> {
        let mut bits = Vec::new();
        self.push_bit_expressions(&resolve_target(&self.out.state, expression)?, &mut bits)?;
        Ok(bits)
    }

    fn push_bit_expressions(
        &self,
        target: &ResolvedTarget,
        bits: &mut Vec<Expression>,
    ) -> Result<(), SimulationError> {
        let bit_of = |name: &str, index: i64| {
            Expression::BitSelect(
                Identifier::new(name.to_string()),
                Box::new(Expression::Constant(VerilogConstant::from_int(index))),
            )
        };
        match target {
            ResolvedTarget::Whole(name) => {
                let signal = self
                    .out
                    .state
                    .get_signal(name)
                    .ok_or_else(|| SimulationError::UnknownSignal(name.clone()))?;
                let (msb, lsb) = signal.range();
                for offset in 0..signal.width() as i64 {
                    let index = if msb >= lsb {
                        msb - offset
                    } else {
                        msb + offset
                    };
                    bits.push(bit_of(name, index));
                }
                Ok(())
            }
            ResolvedTarget::Bits { name, indices } => {
                for index in indices {
                    bits.push(bit_of(name, *index));
                }
                Ok(())
            }
            ResolvedTarget::Parts(parts) => {
                for part in parts {
                    self.push_bit_expressions(part, bits)?;
                }
                Ok(())
            }
            // A memory word, an event, or a select whose index is not a
            // constant: nothing here is a bit of a net, so there is no node for
            // it to be part of.
            other => Err(SimulationError::UnsupportedTarget(other.name().to_string())),
        }
    }

    /// Records a gate and the nets it drives, which are the ones that have to
    /// be resolved rather than simply written.
    fn push_gate(&mut self, gate: Gate) {
        for output in &gate.outputs {
            if let Some(name) = assigned_name(output) {
                self.out.resolved_nets.insert(name.to_string());
            }
        }
        self.out.gates.push(gate);
    }

    /// One instance's view of an arrayed gate's terminal.
    fn array_terminal(
        &self,
        kind: GateKind,
        terminal: &Expression,
        position: usize,
        count: usize,
    ) -> Result<Expression, SimulationError> {
        let width = expression_width(terminal, &self.out.state);
        // A scalar reaches every instance, which is how one enable drives a
        // whole array of three-state buffers.
        if width == 1 {
            return Ok(terminal.clone());
        }
        let too_wide = || SimulationError::GateArrayTerminal {
            gate: kind.keyword(),
            expected: count,
            found: width,
        };
        if width != count {
            return Err(too_wide());
        }
        // Only a plain signal or a part select of one can be sliced into a
        // `BitSelect`, which is the form an output needs because it has to be
        // drivable. Anything else of the right width is sliced by a **shift**
        // instead — `{16'b0, regff} >> position` reads bit `position` of
        // whatever the expression evaluates to, which is what an *input*
        // terminal wants and all one can ask of an expression that names no
        // storage. A design that puts such a terminal in an output position
        // fails where a non-arrayed gate with the same output already fails:
        // resolving it as a write target, by name.
        let (id, bounds) = match terminal {
            Expression::Identifier(id) => (id, None),
            Expression::PartSelect(id, first, second) => {
                let bound = |expression: &Expression| -> Result<i64, SimulationError> {
                    eval(expression, &self.out.state)
                        .ok()
                        .and_then(|value| value.to_u128())
                        .and_then(|value| i64::try_from(value).ok())
                        .ok_or_else(too_wide)
                };
                (id, Some((bound(first)?, bound(second)?)))
            }
            other => {
                return Ok(Expression::Binary(
                    Box::new(other.clone()),
                    BinaryOperator::ShiftRight,
                    Box::new(Expression::Constant(VerilogConstant::from_int(
                        position as i64,
                    ))),
                ))
            }
        };
        let Some(signal) = self.out.state.get_signal(&id.name) else {
            return Err(SimulationError::UnknownSignal(id.name.clone()));
        };
        // A part select brings its own bounds; a bare name takes the whole
        // net's. Either way `position` counts from the least significant end,
        // and both the terminal and the instance array are walked that way —
        // so which end either range starts at cannot matter.
        let (msb, lsb) = bounds.unwrap_or_else(|| signal.range());
        let index = if msb >= lsb {
            lsb + position as i64
        } else {
            lsb - position as i64
        };
        Ok(Expression::BitSelect(
            id.clone(),
            Box::new(Expression::Constant(VerilogConstant::from_int(index))),
        ))
    }

    /// Applies a variable initialiser: `reg a = expr;` and `integer i = expr;`.
    ///
    /// This is a single write at elaboration time, *not* a continuous
    /// assignment. The register holds the value until something writes it
    /// again, and a later procedural assignment simply wins — which is the
    /// whole difference between this and the net form below.
    fn initialise(
        &mut self,
        local: &str,
        init: &Expression,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        let value = eval(&renamed(init, scope), &self.out.state)?;
        let target = Expression::Identifier(Identifier::new(scope.resolve(local)));
        drive(&mut self.out.state, &target, &value)?;
        Ok(())
    }

    /// The second pass: everything that runs.
    fn build(
        &mut self,
        statement: &ModuleStatement,
        scope: &Scope,
        tasks: &TaskTable,
    ) -> Result<(), SimulationError> {
        match statement {
            // `wire a = expr;` is a declaration plus a continuous assignment,
            // so the initialiser joins the same list an explicit `assign`
            // uses and settles through the same fixpoint. The net follows its
            // operands for the whole simulation. Its strength and its delay
            // are the assignment's, exactly as an `assign`'s are.
            ModuleStatement::WireDeclaration(nets) => {
                for net in nets {
                    let Some(init) = net.init() else {
                        // `wire #5 w;` delays *every* driver of `w`, which is
                        // a property of the net rather than of one driver and
                        // is not modelled; dropping it would run the design
                        // at the wrong edge times.
                        if net.delay().is_some() {
                            return Err(SimulationError::Unsupported(
                                "a net delay on a net declared without an assignment",
                            ));
                        }
                        continue;
                    };
                    let target = Expression::Identifier(Identifier::new(
                        scope.resolve(&net.identifier().name),
                    ));
                    let delay = net.delay().map(|delay| {
                        let mut delay = delay.clone();
                        for expression in delay.expressions_mut() {
                            *expression = renamed(expression, scope);
                        }
                        delay
                    });
                    self.push_assignment(ContinuousAssignment::with_timing(
                        target,
                        renamed(init, scope),
                        net.strength(),
                        delay,
                    ));
                }
            }
            ModuleStatement::RegisterDeclaration(registers) => {
                for register in registers {
                    if let Some(init) = &register.init {
                        self.initialise(&register.name.name, init, scope)?;
                    }
                }
            }
            ModuleStatement::IntegerDeclaration(integers) => {
                for declaration in integers {
                    if let Some(init) = &declaration.init {
                        self.initialise(&declaration.name.name, init, scope)?;
                    }
                }
            }
            ModuleStatement::TimeDeclaration(times) => {
                for declaration in times {
                    if let Some(init) = &declaration.init {
                        self.initialise(&declaration.name.name, init, scope)?;
                    }
                }
            }
            ModuleStatement::RealDeclaration(reals) => {
                for declaration in reals {
                    if let Some(init) = &declaration.init {
                        self.initialise(&declaration.name.name, init, scope)?;
                    }
                }
            }
            ModuleStatement::Assignment(assignments) => {
                for assignment in assignments {
                    // `assign #(PERIOD) a = b;` names a parameter, which
                    // belongs to the instance that declared it like every
                    // other name the assignment holds.
                    let delay = assignment.delay().map(|delay| {
                        let mut delay = delay.clone();
                        for expression in delay.expressions_mut() {
                            *expression = renamed(expression, scope);
                        }
                        delay
                    });
                    self.push_assignment(ContinuousAssignment::with_timing(
                        renamed(assignment.lhs(), scope),
                        renamed(assignment.rhs(), scope),
                        assignment.strength(),
                        delay,
                    ));
                }
            }
            ModuleStatement::GateInstantiation(instances) => {
                for instance in instances {
                    self.build_gate(instance, scope)?;
                }
            }
            ModuleStatement::AlwaysBlock(block) => {
                self.declare_block_locals(&block.statements, scope, "")?;
                let mut program = Program::compile(&block.statements, tasks)?;
                // A hidden `repeat` counter or intra-assignment hold is named
                // by an instruction index, which two blocks share; the block's
                // position in the flat list is what makes one its own.
                program.tag_slots(self.out.blocks.len());
                program.qualify_scopes(&|block| scope.hierarchy(block));
                if !scope.genvars.is_empty() {
                    program
                        .substitute(&|expression| substitute_genvars(expression, &scope.genvars));
                }
                let control = match &block.event_control {
                    EventControl::None => EventControl::None,
                    EventControl::Implicit => EventControl::Implicit,
                    EventControl::Events(events) => EventControl::Events(
                        events
                            .iter()
                            .map(|event| {
                                Event::new(event.trigger.clone(), renamed(&event.expression, scope))
                            })
                            .collect(),
                    ),
                };
                if scope.needs_renaming() {
                    program.rename(&|name| scope.resolve(name));
                }
                let implicit_reads = match block.event_control {
                    EventControl::Implicit => {
                        let mut reads: BTreeSet<String> = signals_read(&block.statements)
                            .iter()
                            .filter(|name| !scope.genvars.contains_key(name.as_str()))
                            .map(|name| scope.resolve(name))
                            .collect();
                        let names = BodyNames::of(&program);
                        // A call reads whatever the function it names reads, and
                        // an `@(*)` block is sensitive to everything it reads —
                        // so it has to wake when one of those moves too.
                        for called in names.calls {
                            if let Some(definition) = self.out.state.function(&called) {
                                reads.extend(definition.reads.iter().cloned());
                            }
                        }
                        // An enabled task's body has been inlined into the
                        // program, and the statement tree above kept no trace of
                        // what it reads. The instructions did, already resolved.
                        if program.inlines_a_task() {
                            reads.extend(names.reads);
                        }
                        reads
                    }
                    _ => BTreeSet::new(),
                };
                let writes = match block.event_control {
                    EventControl::Events(_) => written_names(&program),
                    _ => BTreeSet::new(),
                };
                self.out.blocks.push(TimedBlock {
                    kind: BlockKind::Always,
                    free_running: block.event_control == EventControl::None,
                    control,
                    implicit_reads,
                    writes,
                    program,
                });
            }
            ModuleStatement::InitialBlock(block) => {
                self.declare_block_locals(&block.statements, scope, "")?;
                let mut program = Program::compile(&block.statements, tasks)?;
                program.tag_slots(self.out.blocks.len());
                program.qualify_scopes(&|block| scope.hierarchy(block));
                if !scope.genvars.is_empty() {
                    program
                        .substitute(&|expression| substitute_genvars(expression, &scope.genvars));
                }
                if scope.needs_renaming() {
                    program.rename(&|name| scope.resolve(name));
                }
                self.out.blocks.push(TimedBlock {
                    kind: BlockKind::Initial,
                    free_running: false,
                    control: EventControl::None,
                    implicit_reads: BTreeSet::new(),
                    writes: BTreeSet::new(),
                    program,
                });
            }
            ModuleStatement::ModuleInstantiation(instantiation) => {
                self.instantiate_each(instantiation, scope)?
            }
            _ => {}
        }
        Ok(())
    }

    /// Instantiates one module, or — for `inv u[3:0] (o, i);` — one per index
    /// of an instance array, named `u[3]`, `u[2]`, … and each handed its own
    /// slice of any connection wider than one instance's port.
    ///
    /// Expanding here, before [`instantiate`](Elaborator::instantiate), is what
    /// lets an array of modules and an array of UDPs share one path: each
    /// element is an ordinary instantiation from then on.
    fn instantiate_each(
        &mut self,
        instantiation: &ModuleInstantiation,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        let Some(range) = &instantiation.range else {
            return self.instantiate(instantiation, scope);
        };
        let (left, right) = self.resolve_range(range, scope)?;
        let count = range_width((left, right));
        let modules = self.modules;
        let wanted = &instantiation.module_name.name;
        let child = modules
            .iter()
            .find(|module| &module.identifier.name == wanted)
            .ok_or_else(|| SimulationError::UnknownModule(wanted.clone()))?;
        for position in 0..count {
            // The *right* index takes the least significant slice, which is
            // what iverilog does: `inv u[3:0] (o, i)` gives `u[0]` bit 0 of `i`,
            // and `u[0:3]` would give it to `u[3]`.
            let index = if left >= right {
                right + position as i64
            } else {
                right - position as i64
            };
            let arguments = match &instantiation.arguments {
                ModuleInitArguments::NoArgs => ModuleInitArguments::NoArgs,
                ModuleInitArguments::Positional(connections) => {
                    let mut sliced = Vec::with_capacity(connections.len());
                    for (port, connection) in child.ports.iter().zip(connections) {
                        sliced.push(match connection {
                            Some(connection) => Some(self.array_connection(
                                instantiation,
                                port,
                                connection,
                                position,
                                count,
                                scope,
                            )?),
                            None => None,
                        });
                    }
                    ModuleInitArguments::Positional(sliced)
                }
                ModuleInitArguments::Keyword(connections) => {
                    let mut sliced = HashMap::with_capacity(connections.len());
                    for (name, connection) in connections {
                        // A name no port has is reported by `instantiate`,
                        // which sees it next; it is passed through unsliced.
                        let expression = match child
                            .ports
                            .iter()
                            .find(|port| port.identifier.name == name.name)
                        {
                            Some(port) => self.array_connection(
                                instantiation,
                                port,
                                connection,
                                position,
                                count,
                                scope,
                            )?,
                            None => connection.clone(),
                        };
                        sliced.insert(name.clone(), expression);
                    }
                    ModuleInitArguments::Keyword(sliced)
                }
            };
            let element = ModuleInstantiation {
                module_name: instantiation.module_name.clone(),
                instance_name: Identifier::new(format!(
                    "{}[{}]",
                    instantiation.instance_name.name, index
                )),
                range: None,
                parameters: instantiation.parameters.clone(),
                arguments,
            };
            self.instantiate(&element, scope)?;
        }
        Ok(())
    }

    /// One element's share of a connection to an arrayed instance.
    ///
    /// A connection exactly as wide as the port reaches every element — which
    /// is how one clock drives a whole array — and one exactly `count` times as
    /// wide is sliced, a port's width apiece from the least significant end.
    /// Anything else is a named error rather than a guessed wiring.
    ///
    /// The slice is built on the parent's *local* name, because
    /// [`instantiate`](Elaborator::instantiate) renames every connection itself
    /// and a slice of an already-renamed one would be qualified twice. Only
    /// the width is measured on a renamed copy, since that is what the store
    /// can answer.
    fn array_connection(
        &self,
        instantiation: &ModuleInstantiation,
        port: &Port,
        connection: &Expression,
        position: usize,
        count: usize,
        scope: &Scope,
    ) -> Result<Expression, SimulationError> {
        // A port whose width is made of the child's parameters has no width
        // until the child is being elaborated, which is after this decision.
        let Range::Constant(port_msb, port_lsb) = port.range else {
            return Err(SimulationError::Unsupported(
                "an instance array whose port width depends on a parameter",
            ));
        };
        let port_width = range_width((port_msb, port_lsb));
        let found = expression_width(&renamed(connection, scope), &self.out.state);
        if found == port_width {
            return Ok(connection.clone());
        }
        let mismatch = || SimulationError::ArrayConnectionWidth {
            instance: instantiation.instance_name.name.clone(),
            port: port.identifier.name.clone(),
            port_width,
            count,
            found,
        };
        if found != port_width * count {
            return Err(mismatch());
        }
        // Only a plain signal can be sliced by index: a slice of
        // `{16'b0, data}` is not something a select can name.
        let Some(id) = plain_identifier(connection) else {
            return Err(mismatch());
        };
        let Some(signal) = self.out.state.get_signal(&scope.resolve(&id.name)) else {
            return Err(SimulationError::UnknownSignal(id.name.clone()));
        };
        let (msb, lsb) = signal.range();
        let offset = (position * port_width) as i64;
        let span = port_width as i64 - 1;
        // Walked from the least significant end, so which way round the parent
        // declared its vector cannot matter.
        let (high, low) = if msb >= lsb {
            (lsb + offset + span, lsb + offset)
        } else {
            (lsb - offset - span, lsb - offset)
        };
        let bound = |index: i64| Box::new(Expression::Constant(VerilogConstant::from_int(index)));
        Ok(if port_width == 1 {
            Expression::BitSelect(id.clone(), bound(low))
        } else {
            Expression::PartSelect(id.clone(), bound(high), bound(low))
        })
    }

    fn instantiate(
        &mut self,
        instantiation: &ModuleInstantiation,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        let modules = self.modules;
        let wanted = &instantiation.module_name.name;
        let index = modules
            .iter()
            .position(|module| &module.identifier.name == wanted)
            .ok_or_else(|| SimulationError::UnknownModule(wanted.clone()))?;
        let child = &modules[index];

        // A module cannot see out of itself, so the instance's prefix is the
        // whole of what a name inside it resolves to — the generate block it
        // may stand in is already part of the prefix it was created under.
        let prefix = format!("{}{}.", scope.prefix, instantiation.instance_name.name);
        // The hierarchical name a design writes this instance under, which is
        // the store prefix with the top module's own name in front: the top is
        // the root of the flat name space and carries no prefix, but a design
        // still calls it `top`.
        self.out.instances.push((
            scope.hierarchy(&instantiation.instance_name.name),
            child.timescale,
        ));
        // `BUFG #5 bg(out, in);` reads as a parameter override, because that
        // is what `#(...)` means on a module instantiation — but a primitive
        // has no parameters and the tokens are a *delay*. Only the module being
        // instantiated says which, so the decision is made here and the answer
        // travels with the scope the way a port binding does.
        let primitive_delay = if primitive_table(child).is_some() {
            primitive_delay(&instantiation.parameters)?
        } else {
            None
        };
        let mut inner = Scope {
            module_prefix: prefix.clone(),
            prefix,
            bindings: HashMap::new(),
            overrides: HashMap::new(),
            locals: HashMap::new(),
            genvars: HashMap::new(),
            root_name: scope.root_name.clone(),
            primitive_delay,
        };

        for (port, connection) in connections(child, &instantiation.arguments)? {
            let local = &port.identifier.name;
            // A genvar reaches a port connection as an *index* — `.a(x[i])` —
            // and as a whole connection in `.a(i)`, which is a constant rather
            // than a signal that could be aliased. Substituting first is what
            // tells the two apart.
            let mut connection = connection.clone();
            if !scope.genvars.is_empty() {
                substitute_genvars(&mut connection, &scope.genvars);
            }
            let binding = match plain_identifier(&connection)
                .filter(|_| self.can_alias(port, &connection, scope))
            {
                Some(id) => {
                    let outer = scope.resolve(&id.name);
                    if !self.out.state.contains(&outer) {
                        return Err(SimulationError::UnknownSignal(outer));
                    }
                    self.out
                        .aliases
                        .insert(format!("{}{}", inner.prefix, local), outer.clone());
                    Binding::Alias(outer)
                }
                None if matches!(port.direction, PortDirection::Input) => {
                    Binding::Driven(renamed(&connection, scope))
                }
                // An output the parent bound to something *writable* —
                // `.y(bus[i])`, which is how a generate loop wires an instance
                // per bit, or `.oB({e, f, g, h})` — is the alias run
                // backwards: the port keeps a signal of its own and a
                // continuous assignment carries it out to the target. An
                // output bound to something that cannot be written at all,
                // like `a + 1`, is still a named error, because there is
                // nowhere for the child's value to go.
                None if port.direction == PortDirection::Output && is_drivable(&connection) => {
                    Binding::Driving(renamed(&connection, scope))
                }
                // An `inout` is read *and* written, so neither direction of
                // the assignment will do. It is bonded instead — see
                // [`Binding::Bonded`].
                None if port.direction == PortDirection::InOut && is_drivable(&connection) => {
                    Binding::Bonded(renamed(&connection, scope))
                }
                None => {
                    return Err(SimulationError::UndrivablePort {
                        instance: instantiation.instance_name.name.clone(),
                        port: local.clone(),
                        connection: connection.to_contracted_string(),
                    })
                }
            };
            inner.bindings.insert(local.clone(), binding);
        }

        inner.overrides = self.overrides(child, instantiation, scope)?;
        // A `defparam` addresses the instance it overrides by name, so it is
        // applied here, where that instance is made — and it beats a `#(...)`
        // written on the instantiation, which is the order the LRM gives.
        for parameter in parameter_names(child) {
            let path = format!("{}{}", inner.prefix, parameter);
            if let Some(value) = self.defparams.remove(&path) {
                inner.overrides.insert(parameter.to_string(), value);
            }
        }

        self.walk(index, &mut inner)
    }

    /// Whether a port bound to a plain identifier may share that signal's
    /// store entry.
    ///
    /// Aliasing makes the port and the parent's signal **one** value, and a
    /// value carries how to read it — so a port whose declaration disagrees
    /// with the parent's about signedness cannot be one: `input signed [31:0]
    /// a` bound to a `reg [31:0]` would read unsigned inside the child, and
    /// `a <= b` would compare `32'h80000000` as the largest number rather than
    /// the smallest (corpus `pr1033`). Such a port keeps an entry of its own
    /// and a continuous assignment carries the value across, which is the
    /// arrangement a port bound to an *expression* already had.
    ///
    /// An `inout` is the one that cannot take it: it is read as well as
    /// written and one assignment only runs one way, so it stays aliased and
    /// keeps the parent's signedness.
    fn can_alias(&self, port: &Port, connection: &Expression, scope: &Scope) -> bool {
        if matches!(port.direction, PortDirection::InOut) {
            return true;
        }
        if port.direction == PortDirection::Output && !is_drivable(connection) {
            return true;
        }
        let Some(id) = plain_identifier(connection) else {
            return true;
        };
        self.out
            .state
            .get_signal(&scope.resolve(&id.name))
            .is_none_or(|outer| outer.is_signed() == port.signed)
    }

    /// Evaluates a `#(...)` block in the *parent's* scope, keyed by the child's
    /// parameter names.
    fn overrides(
        &self,
        child: &VerilogModule,
        instantiation: &ModuleInstantiation,
        scope: &Scope,
    ) -> Result<HashMap<String, Register>, SimulationError> {
        // A UDP declares no parameters, so `BUFG #5 bg(o, i);` is the
        // instance's *delay* rather than an override. A gate's delay is parsed
        // and ignored, and there is nothing more a primitive's can be here:
        // both settle in zero time along with every other continuous driver.
        if primitive_table(child).is_some() {
            return Ok(HashMap::new());
        }

        let declared = parameter_names(child);

        let mut pairs: Vec<(&str, &Expression)> = Vec::new();
        match &instantiation.parameters {
            ModuleInitArguments::NoArgs => {}
            ModuleInitArguments::Positional(expressions) => {
                if expressions.len() > declared.len() {
                    return Err(SimulationError::TooManyArguments {
                        module: child.identifier.name.clone(),
                        what: "parameters",
                        expected: declared.len(),
                        found: expressions.len(),
                    });
                }
                pairs.extend(
                    declared
                        .iter()
                        .copied()
                        .zip(expressions)
                        .filter_map(|(name, expression)| Some((name, expression.as_ref()?))),
                );
            }
            ModuleInitArguments::Keyword(arguments) => {
                let mut named: Vec<(&Identifier, &Expression)> = arguments.iter().collect();
                // A `HashMap` has no order of its own, so sort to keep which
                // error a bad instantiation reports deterministic.
                named.sort_by(|left, right| left.0.name.cmp(&right.0.name));
                for (id, expression) in named {
                    let name = declared
                        .iter()
                        .copied()
                        .find(|declared| *declared == id.name)
                        .ok_or_else(|| SimulationError::UnknownParameter {
                            module: child.identifier.name.clone(),
                            parameter: id.name.clone(),
                        })?;
                    pairs.push((name, expression));
                }
            }
        }

        let mut overrides = HashMap::new();
        for (name, expression) in pairs {
            let value = eval(&renamed(expression, scope), &self.out.state)?;
            overrides.insert(name.to_string(), value);
        }
        Ok(overrides)
    }
}

/// The parameters a module declares, in the order it declares them.
fn parameter_names(module: &VerilogModule) -> Vec<&str> {
    module
        .statements
        .iter()
        .filter_map(|statement| match statement {
            ModuleStatement::ParameterDeclaration(parameters) => Some(parameters),
            _ => None,
        })
        .flatten()
        .map(|parameter| parameter.name.name.as_str())
        .collect()
}

/// Pairs each connected port with the expression the parent bound to it.
///
/// Positional arguments bind in port-declaration order, named arguments by
/// port name. A port nobody mentioned is left out and stays unconnected, and
/// so is one whose position was written blank — `dut u(, b)` connects only the
/// second port.
fn connections<'a>(
    child: &'a VerilogModule,
    arguments: &'a ModuleInitArguments,
) -> Result<Vec<(&'a Port, &'a Expression)>, SimulationError> {
    match arguments {
        ModuleInitArguments::NoArgs => Ok(Vec::new()),
        ModuleInitArguments::Positional(expressions) => {
            if expressions.len() > child.ports.len() {
                return Err(SimulationError::TooManyArguments {
                    module: child.identifier.name.clone(),
                    what: "ports",
                    expected: child.ports.len(),
                    found: expressions.len(),
                });
            }
            Ok(child
                .ports
                .iter()
                .zip(expressions)
                .filter_map(|(port, expression)| Some((port, expression.as_ref()?)))
                .collect())
        }
        ModuleInitArguments::Keyword(arguments) => {
            let mut named: Vec<&Identifier> = arguments.keys().collect();
            named.sort_by(|left, right| left.name.cmp(&right.name));
            for id in named {
                if !child
                    .ports
                    .iter()
                    .any(|port| port.identifier.name == id.name)
                {
                    return Err(SimulationError::UnknownPort {
                        module: child.identifier.name.clone(),
                        port: id.name.clone(),
                    });
                }
            }
            // Walking the ports rather than the map keeps the result in
            // declaration order.
            Ok(child
                .ports
                .iter()
                .filter_map(|port| {
                    arguments
                        .get(&port.identifier)
                        .map(|expression| (port, expression))
                })
                .collect())
        }
    }
}

/// The identifier a connection names, if that is all it is.
///
/// A parenthesized identifier still names one signal, so `.a((b))` aliases just
/// as `.a(b)` does.
fn plain_identifier(expression: &Expression) -> Option<&Identifier> {
    match expression {
        Expression::Identifier(id) => Some(id),
        Expression::Parenthetical(inner) => plain_identifier(inner),
        _ => None,
    }
}

/// Checks what a compiled function body does and reports the design signals it
/// reads and the functions it calls.
///
/// A function is evaluated inside an expression, which fixes what a body may
/// do: it cannot consume time, it cannot print, it cannot defer a write past
/// its own end, and it cannot write anything but its own variables. Each of
/// those is a named error here rather than something that quietly does nothing
/// at every call.
fn analyse_function_body(
    program: &Program,
    own: &BTreeSet<String>,
) -> Result<BodyNames, SimulationError> {
    // The design signals the body assigns. They are not frame variables, so
    // the frame has to be given copies of them to write into and the call has
    // to hand what it wrote back afterwards.
    let mut outside: BTreeSet<String> = BTreeSet::new();
    for instruction in program.instructions() {
        match instruction {
            Instruction::Blocking { target, .. } => {
                let mut names = Vec::new();
                if !assigned_names(target, &mut names) {
                    return Err(SimulationError::UnsupportedTarget(
                        target.to_contracted_string(),
                    ));
                }
                for name in names {
                    if !own.contains(name) {
                        outside.insert(name.to_string());
                    }
                }
            }
            // A scheduled write is a non-blocking assignment with a delay on
            // it, so it fails for both reasons at once; the non-blocking one
            // is the more specific.
            Instruction::NonBlocking { .. } | Instruction::ScheduleWrite { .. } => {
                return Err(FUNCTION_NONBLOCKING_UNSUPPORTED)
            }
            // A drive outlives the call that installed it, and the frame it
            // would be installed on is thrown away when the call returns.
            Instruction::Assign { .. }
            | Instruction::Force { .. }
            | Instruction::Deassign(_)
            | Instruction::Release(_) => return Err(FUNCTION_DRIVE_UNSUPPORTED),
            // `$display` and `$write` print into the buffer the frame shares
            // with the design; anything else in the family needs state the
            // frame throws away.
            Instruction::Task(call) if !call.prints_now() => return Err(FUNCTION_TASK_UNSUPPORTED),
            Instruction::Delay(_) => return Err(FUNCTION_DELAY_UNSUPPORTED),
            // A task may consume time and a function may not, so a function
            // that enables one is illegal Verilog. A *local* enable is spliced
            // into the body before this walk sees it and is caught by whatever
            // the body then does; one naming another instance is still standing
            // here, and naming it is better than the "assigning a signal
            // outside itself" the linked body would have reported.
            Instruction::HierarchicalEnable { .. } => return Err(FUNCTION_ENABLE_UNSUPPORTED),
            // A `wait` and an event control are both suspensions, and a call
            // happens at one instant: there is no later for the body to come
            // back at. `Hold` and `WriteHeld` are the halves of one, so they
            // cannot appear without it.
            Instruction::Wait(_) | Instruction::EventWait(_) => {
                return Err(FUNCTION_EVENT_UNSUPPORTED)
            }
            _ => {}
        }
    }

    // A `disable` of a block the function is written inside is a jump and
    // costs the frame nothing. One naming anything else reaches out of the
    // frame at a block the evaluator cannot see, so it is named here rather
    // than quietly doing nothing when the call runs.
    if let Some(scope) = program.nonlocal_disable() {
        return Err(SimulationError::UnknownScope(scope.to_string()));
    }

    let mut names = BodyNames::of(program);
    if names.random {
        return Err(FUNCTION_RANDOM_UNSUPPORTED);
    }
    // What the function declares itself is not something a call has to copy in.
    names.reads.retain(|name| !own.contains(name));
    // A signal the body writes has to be *in* the frame for the write to land,
    // even when nothing in the body reads it — `outside = 1;` names it nowhere
    // else.
    names.reads.extend(outside.iter().cloned());
    names.writes = outside;
    Ok(names)
}

/// Whether an expression is something a continuous assignment can *write*,
/// which is what an output port bound to it needs.
///
/// It is the set `exec::resolve_target` accepts, and it has to stay that: a
/// port bound to something this admits and that refuses would elaborate and
/// then fail at the first propagation. A concatenation qualifies because #212
/// made one a writable target — every part is resolved on its own and the
/// value is split across them.
fn is_drivable(expression: &Expression) -> bool {
    match expression {
        Expression::Identifier(_)
        | Expression::BitSelect(_, _)
        | Expression::PartSelect(_, _, _)
        | Expression::IndexedPartSelect { .. }
        | Expression::WordSelect { .. } => true,
        Expression::Parenthetical(inner) => is_drivable(inner),
        Expression::Concatenation(parts) => parts.iter().all(is_drivable),
        _ => false,
    }
}

/// Whether a port names a variable rather than a net.
///
/// `output reg q` is a variable, so an undriven one holds `x` because nothing
/// has said what it is. Every other port is a net, so an undriven one holds
/// `z` because nothing is driving it.
fn port_is_variable(port: &Port) -> bool {
    matches!(port.net_type, Some(NetType::Reg))
}

/// The table of a user-defined primitive, or `None` for an ordinary module.
///
/// A UDP is parsed into a [`VerilogModule`] so that instantiation and port
/// binding need no notion of one; the single table statement is the only thing
/// that tells them apart, and this is where that question is asked.
fn primitive_table(module: &VerilogModule) -> Option<&UdpTable> {
    module
        .statements
        .iter()
        .find_map(|statement| match statement {
            ModuleStatement::PrimitiveTable(table) => Some(table),
            _ => None,
        })
}

/// The `#(...)` on a **primitive** instantiation, read as the delay it is.
///
/// A UDP has no parameters, so the tokens the grammar read as a parameter
/// override list are a `delay3` — one, two or three delays, meaning rise, fall
/// and turn-off exactly as a gate's do. `BUFG #5 bg(out, in);` is the common
/// spelling and is one delay standing for all three.
///
/// A *named* override (`#(.x(1))`) names a parameter a primitive cannot have,
/// and more than three delays is not a `delay3`; both are refused by name
/// rather than dropped, because a delay a design wrote and the simulator
/// ignored is a wrong answer at the right values.
fn primitive_delay(parameters: &ModuleInitArguments) -> Result<Option<GateDelay>, SimulationError> {
    let values = match parameters {
        ModuleInitArguments::NoArgs => return Ok(None),
        ModuleInitArguments::Positional(values) => values,
        ModuleInitArguments::Keyword(_) => {
            return Err(SimulationError::Unsupported(
                "a named parameter override on a primitive, which has no parameters",
            ))
        }
    };
    let mut delays = values.iter().flatten().cloned().map(Delay::from_expression);
    let Some(rise) = delays.next() else {
        return Ok(None);
    };
    let fall = delays.next().unwrap_or_else(|| rise.clone());
    let turn_off = delays.next();
    if delays.next().is_some() {
        return Err(SimulationError::Unsupported(
            "more than three delays on a primitive instantiation",
        ));
    }
    Ok(Some(GateDelay::of(rise, fall, turn_off)))
}

/// The signal an assignment target writes, or `None` when the target is not
/// something that names one.
fn assigned_name(target: &Expression) -> Option<&str> {
    match target {
        Expression::Identifier(id)
        | Expression::BitSelect(id, _)
        | Expression::PartSelect(id, _, _)
        | Expression::IndexedPartSelect { id, .. }
        | Expression::WordSelect { id, .. } => Some(&id.name),
        Expression::Parenthetical(inner) => assigned_name(inner),
        _ => None,
    }
}

/// Every signal an assignment target writes, collected into `names`, reporting
/// whether the whole target names signals at all.
///
/// A concatenation names one per part — `{tmp1, tmp2} = v;` is an ordinary
/// Verilog target that `ResolvedTarget::Parts` already writes — and a part that
/// names nothing fails the whole target the way a bare one does (corpus
/// `constfunc14`).
fn assigned_names<'a>(target: &'a Expression, names: &mut Vec<&'a str>) -> bool {
    match target {
        Expression::Concatenation(parts) => parts.iter().all(|part| assigned_names(part, names)),
        Expression::Parenthetical(inner) => assigned_names(inner, names),
        other => match assigned_name(other) {
            Some(name) => {
                names.push(name);
                true
            }
            None => false,
        },
    }
}

/// Every signal a compiled body assigns.
///
/// A target whose name cannot be read off it — a concatenation, which is
/// several — contributes each of its parts, so the set is never short of a
/// name the body really writes. Being short is the direction that matters: it
/// is what a block measures "was that edge one of mine?" against.
fn written_names(program: &Program) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    for instruction in program.instructions() {
        let target = match instruction {
            Instruction::Blocking { target, .. }
            | Instruction::NonBlocking { target, .. }
            | Instruction::Assign { target, .. }
            | Instruction::Force { target, .. }
            | Instruction::Hold { target, .. }
            | Instruction::WriteHeld { target, .. }
            | Instruction::ScheduleWrite { target, .. }
            | Instruction::Deassign(target)
            | Instruction::Release(target) => target,
            _ => continue,
        };
        collect_written_names(target, &mut names);
    }
    names
}

fn collect_written_names(target: &Expression, names: &mut BTreeSet<String>) {
    match assigned_name(target) {
        Some(name) => {
            names.insert(name.to_string());
        }
        None => {
            if let Expression::Concatenation(parts) = target {
                for part in parts {
                    collect_written_names(part, names);
                }
            }
        }
    }
}

/// The names a compiled body uses, gathered in one walk.
///
/// The three are collected together because they all come out of the same
/// expression trees, and asking for them separately would mean walking those
/// trees once per question.
#[derive(Debug, Default)]
struct BodyNames {
    /// Every signal name an expression in the body reads. An assignment target
    /// is not one — writing a signal is not reading it — but an index inside a
    /// target is.
    reads: BTreeSet<String>,
    /// Every function the body calls, under the name it resolved to.
    calls: BTreeSet<String>,
    /// The design signals the body assigns — the ones that are *not* frame
    /// variables. They are filled in by [`analyse_function_body`] rather than
    /// by the walk, because only it knows which names the function declares.
    writes: BTreeSet<String>,
    /// Whether anything in the body draws from the `$random` stream.
    random: bool,
}

impl BodyNames {
    /// The names a whole compiled program uses.
    fn of(program: &Program) -> BodyNames {
        let mut names = BodyNames::default();
        for instruction in program.instructions() {
            names.instruction(instruction);
        }
        names
    }

    fn instruction(&mut self, instruction: &Instruction) {
        match instruction {
            Instruction::Blocking { target, value }
            | Instruction::NonBlocking { target, value }
            | Instruction::Assign { target, value }
            | Instruction::Force { target, value } => {
                self.target(target);
                self.expression(value);
            }
            Instruction::Deassign(target) | Instruction::Release(target) => self.target(target),
            Instruction::JumpIfFalse { condition, .. } => self.expression(condition),
            Instruction::CaseSubject(subject) => self.expression(subject),
            Instruction::JumpIfMatch { label, .. } => self.expression(label),
            Instruction::RepeatInit { count, .. } => self.expression(count),
            Instruction::Wait(condition) => self.expression(condition),
            // A block waiting on an edge reads the signal that edge is of.
            Instruction::EventWait(control) => {
                if let EventControl::Events(events) = control {
                    for event in events {
                        self.expression(&event.expression);
                    }
                }
            }
            Instruction::Hold { target, value, .. } => {
                self.target(target);
                self.expression(value);
            }
            Instruction::WriteHeld { target, .. } => self.target(target),
            Instruction::ScheduleWrite {
                target,
                value,
                delay,
            } => {
                self.target(target);
                self.expression(value);
                for expression in delay.expressions() {
                    self.expression(expression);
                }
            }
            // The arguments of an enable of another instance's task are read
            // where the enable is written, which is inside this body.
            Instruction::HierarchicalEnable { arguments, .. } => {
                for argument in arguments {
                    self.expression(argument);
                }
            }
            // A `disable` names a scope rather than a signal, so there is
            // nothing in one for a frame to copy in.
            Instruction::Jump(_)
            | Instruction::RepeatNext { .. }
            | Instruction::Task(_)
            | Instruction::Delay(_)
            | Instruction::Disable { .. }
            | Instruction::Fork { .. }
            | Instruction::JoinBranch
            | Instruction::Halt => {}
        }
    }

    /// The reads hiding inside an assignment target: a select index, a
    /// part-select bound. The target's own name is a write, not one of them.
    fn target(&mut self, target: &Expression) {
        match target {
            Expression::Identifier(_) => {}
            Expression::Parenthetical(inner) => self.target(inner),
            Expression::BitSelect(_, index) => self.expression(index),
            Expression::PartSelect(_, first, second) => {
                self.expression(first);
                self.expression(second);
            }
            Expression::IndexedPartSelect { base, width, .. } => {
                self.expression(base);
                self.expression(width);
            }
            Expression::WordSelect {
                indices, select, ..
            } => {
                for index in indices {
                    self.expression(index);
                }
                for inner in select.expressions() {
                    self.expression(inner);
                }
            }
            other => self.expression(other),
        }
    }

    fn expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Constant(_) | Expression::RealLiteral(_) | Expression::StringLiteral(_) => {
            }
            Expression::Identifier(id) => {
                self.reads.insert(id.name.clone());
            }
            Expression::Unary(_, inner) | Expression::Parenthetical(inner) => {
                self.expression(inner)
            }
            Expression::Binary(lhs, _, rhs) => {
                self.expression(lhs);
                self.expression(rhs);
            }
            Expression::Conditional(condition, when_true, when_false) => {
                self.expression(condition);
                self.expression(when_true);
                self.expression(when_false);
            }
            Expression::Concatenation(parts) => {
                for part in parts {
                    self.expression(part);
                }
            }
            Expression::Replication(count, parts) => {
                self.expression(count);
                for part in parts {
                    self.expression(part);
                }
            }
            Expression::FunctionCall(id, arguments) => {
                self.calls.insert(id.name.clone());
                for argument in arguments {
                    self.expression(argument);
                }
            }
            Expression::SystemFunctionCall(name, arguments) => {
                self.random |= name == "random";
                for argument in arguments {
                    self.expression(argument);
                }
            }
            Expression::BitSelect(id, index) => {
                self.reads.insert(id.name.clone());
                self.expression(index);
            }
            Expression::PartSelect(id, first, second) => {
                self.reads.insert(id.name.clone());
                self.expression(first);
                self.expression(second);
            }
            Expression::IndexedPartSelect {
                id, base, width, ..
            } => {
                self.reads.insert(id.name.clone());
                self.expression(base);
                self.expression(width);
            }
            Expression::WordSelect {
                id,
                indices,
                select,
            } => {
                self.reads.insert(id.name.clone());
                for index in indices {
                    self.expression(index);
                }
                for inner in select.expressions() {
                    self.expression(inner);
                }
            }
        }
    }
}

/// The store entry a task's argument or local lives in, before this instance's
/// prefix is put on it.
///
/// A Verilog identifier cannot contain a `.`, so a task variable can never
/// collide with a signal the design declares — it is the same shape a
/// function's frame variables take.
fn task_variable(task: &str, variable: &str) -> String {
    format!("{}.{}", task, variable)
}

/// The path of the first enable of another instance's task a program holds.
fn first_hierarchical_enable(program: &Program) -> Option<&str> {
    program
        .instructions()
        .iter()
        .find_map(|instruction| match instruction {
            Instruction::HierarchicalEnable { path, .. } => Some(path.as_str()),
            _ => None,
        })
}

/// A task that reaches itself through a chain of hierarchical enables, if the
/// design has one.
///
/// Inlining does not terminate on a cycle, and a static task's storage means
/// real Verilog cannot recurse either — so this is the same answer
/// `declare_tasks` gives for a cycle among one module's own enables, over the
/// graph the whole design's tasks make between them.
fn recursive_enable(tasks: &HashMap<String, TaskDefinition>) -> Option<String> {
    fn walk<'a>(
        path: &'a str,
        tasks: &'a HashMap<String, TaskDefinition>,
        open: &mut BTreeSet<&'a str>,
        settled: &mut BTreeSet<&'a str>,
    ) -> Option<String> {
        if settled.contains(path) {
            return None;
        }
        if !open.insert(path) {
            return Some(path.to_string());
        }
        if let Some(definition) = tasks.get(path) {
            for instruction in definition.program.instructions() {
                if let Instruction::HierarchicalEnable { path: next, .. } = instruction {
                    if let Some(cycle) = walk(next, tasks, open, settled) {
                        return Some(cycle);
                    }
                }
            }
        }
        open.remove(path);
        settled.insert(path);
        None
    }

    let mut open = BTreeSet::new();
    let mut settled = BTreeSet::new();
    tasks
        .keys()
        .find_map(|path| walk(path, tasks, &mut open, &mut settled))
}

/// Compiles one task body into the shape an enable splices in.
///
/// The body is renamed through the task's own variables only: everything else
/// is left as the module wrote it, because the block the body is spliced into
/// is resolved into the instance's names afterwards and would prefix an
/// already-qualified name twice.
fn compile_task(
    task: &TaskDeclaration,
    tasks: &TaskTable,
) -> Result<TaskDefinition, SimulationError> {
    // A task's constants and its named events are its own names exactly as its
    // variables are, so all three go through the one rename: a `parameter` left
    // bare would resolve outwards to the module and read whatever the module
    // happens to declare under that spelling.
    let names: HashMap<&str, String> = task
        .arguments
        .iter()
        .map(|argument| argument.variable.name.name.as_str())
        .chain(task.locals.iter().map(|local| local.name.name.as_str()))
        .chain(
            task.parameters
                .iter()
                .map(|parameter| parameter.name.name.as_str()),
        )
        .chain(task.events.iter().map(|event| event.name.as_str()))
        .map(|variable| (variable, task_variable(&task.name.name, variable)))
        .collect();

    let mut program =
        Program::compile_body(&task.statements, tasks, &block_scope("", &task.name.name))?;
    program.rename_local(&|name| match names.get(name) {
        Some(qualified) => qualified.clone(),
        None => name.to_string(),
    });

    Ok(TaskDefinition {
        arguments: task
            .arguments
            .iter()
            .map(|argument| TaskParameter {
                name: task_variable(&task.name.name, &argument.variable.name.name),
                direction: argument.direction,
            })
            .collect(),
        program,
    })
}

/// Adds to every function's read and write sets those of the functions it
/// calls, until nothing more is added.
///
/// A call seeds its frame from the store it was called against, so a function
/// that calls another has to copy in what *that* one reads as well — otherwise
/// the inner call would find the design signals it wanted missing. The writes
/// close the same way and for the mirror reason: an inner call hands its write
/// back into the *outer* call's frame, and the outer call has to know to pass
/// it on or the design would never see it. A cycle in the call graph is what
/// the fixpoint is for: a recursive function's reads are its own.
fn close_reads(functions: &mut BTreeMap<String, FunctionDefinition>) {
    loop {
        let mut grew = false;
        let names: Vec<String> = functions.keys().cloned().collect();
        for name in names {
            let mut inherited = BTreeSet::new();
            let mut written = BTreeSet::new();
            for called in &functions[&name].calls {
                if let Some(definition) = functions.get(called) {
                    inherited.extend(definition.reads.iter().cloned());
                    written.extend(definition.writes.iter().cloned());
                }
            }
            let definition = functions.get_mut(&name).expect("a staged function");
            for read in inherited {
                grew |= definition.reads.insert(read);
            }
            for write in written {
                grew |= definition.writes.insert(write.clone());
                definition.reads.insert(write);
            }
        }
        if !grew {
            return;
        }
    }
}

/// [`renamed`] for an expression written inside a subprogram or a named block
/// whose own constants are `own`: one of those takes the scope's dotted prefix
/// `inner`, and every other name resolves outwards to the module.
fn scoped_renamed(
    expression: &Expression,
    scope: &Scope,
    inner: &str,
    own: &BTreeSet<&str>,
) -> Expression {
    let mut copy = expression.clone();
    if !scope.genvars.is_empty() {
        substitute_genvars(&mut copy, &scope.genvars);
    }
    rename_expression(&mut copy, &|local| match own.contains(local) {
        true => scope.qualified(&format!("{}{}", inner, local)),
        false => scope.resolve(local),
    });
    copy
}

/// A copy of `expression` with every signal it names resolved into the flat
/// store.
fn renamed(expression: &Expression, scope: &Scope) -> Expression {
    let mut copy = expression.clone();
    // A genvar goes first, and has to: it is not a name that resolves to
    // anything, so qualifying it would produce a signal nothing declares.
    if !scope.genvars.is_empty() {
        substitute_genvars(&mut copy, &scope.genvars);
    }
    if scope.needs_renaming() {
        rename_expression(&mut copy, &|name| scope.resolve(name));
    }
    copy
}

/// Replaces every genvar an expression names with the integer it is bound to.
///
/// A genvar is an elaboration-time integer: nothing of it survives into the
/// run, so a body that reads one has to be handed the *value* rather than a
/// name it could look up. That is also why this cannot be a rename — an
/// identifier node is replaced by a constant one, which is a different shape.
/// A genvar is never the name being *selected from*, only an index, so the
/// select arms walk their subexpressions and leave the name alone.
fn substitute_genvars(expression: &mut Expression, genvars: &HashMap<String, i64>) {
    match expression {
        Expression::Constant(_) | Expression::RealLiteral(_) | Expression::StringLiteral(_) => {}
        Expression::Identifier(id) => {
            if let Some(value) = genvars.get(&id.name) {
                *expression = Expression::Constant(VerilogConstant::from_int(*value));
            }
        }
        Expression::Unary(_, inner) | Expression::Parenthetical(inner) => {
            substitute_genvars(inner, genvars)
        }
        Expression::Binary(lhs, _, rhs) => {
            substitute_genvars(lhs, genvars);
            substitute_genvars(rhs, genvars);
        }
        Expression::Conditional(condition, when_true, when_false) => {
            substitute_genvars(condition, genvars);
            substitute_genvars(when_true, genvars);
            substitute_genvars(when_false, genvars);
        }
        Expression::Concatenation(parts) => {
            for part in parts {
                substitute_genvars(part, genvars);
            }
        }
        Expression::Replication(count, parts) => {
            substitute_genvars(count, genvars);
            for part in parts {
                substitute_genvars(part, genvars);
            }
        }
        Expression::FunctionCall(_, arguments) | Expression::SystemFunctionCall(_, arguments) => {
            for argument in arguments {
                substitute_genvars(argument, genvars);
            }
        }
        Expression::BitSelect(_, index) => substitute_genvars(index, genvars),
        Expression::PartSelect(_, msb, lsb) => {
            substitute_genvars(msb, genvars);
            substitute_genvars(lsb, genvars);
        }
        Expression::IndexedPartSelect { base, width, .. } => {
            substitute_genvars(base, genvars);
            substitute_genvars(width, genvars);
        }
        Expression::WordSelect {
            indices, select, ..
        } => {
            for index in indices {
                substitute_genvars(index, genvars);
            }
            for inner in select.expressions_mut() {
                substitute_genvars(inner, genvars);
            }
        }
    }
}

/// Rewrites every name an event control waits on through `resolve`.
///
/// A control reaches the flat name space two ways — as a whole block's
/// trigger, and as an [`Instruction::EventWait`] inside one — and both spell
/// the signals they wait on the way the module wrote them.
pub fn rename_event_control(control: &mut EventControl, resolve: &dyn Fn(&str) -> String) {
    if let EventControl::Events(events) = control {
        for event in events {
            rename_expression(&mut event.expression, resolve);
        }
    }
}

/// Every name a generate block's items declare directly.
///
/// A generate block is a *scope*, so only what it declares is renamed into it
/// and everything else belongs to the module around it. Which makes this the
/// list of what "inside" means: the signals and parameters the block declares,
/// the instances it creates, the labels of the generate blocks nested in it,
/// and the labels of the *named blocks* its `initial` and `always` bodies open
/// — a hierarchical reference reaches through all four.
fn declared_names(items: &[GenerateItem]) -> Vec<String> {
    let mut names = Vec::new();
    let mut label = |block: &GenerateBlock| {
        if let Some(name) = &block.name {
            names.push(name.name.clone());
        }
    };
    let mut declarations = Vec::new();
    for item in items {
        match item {
            GenerateItem::Item(statement) => declarations.push(statement),
            GenerateItem::Block(block) => label(block),
            GenerateItem::Loop(repeated) => label(&repeated.body),
            GenerateItem::If(branch) => {
                label(&branch.then_block);
                if let Some(block) = &branch.else_block {
                    label(block);
                }
            }
            GenerateItem::Case(choice) => {
                for arm in &choice.items {
                    label(&arm.block);
                }
            }
        }
    }
    for statement in declarations {
        declared_by(statement, &mut names);
    }
    names
}

/// The names one module statement brings into existence.
fn declared_by(statement: &ModuleStatement, names: &mut Vec<String>) {
    match statement {
        ModuleStatement::WireDeclaration(nets) => {
            names.extend(nets.iter().map(|net| net.identifier().name.clone()))
        }
        ModuleStatement::RegisterDeclaration(registers) => {
            names.extend(registers.iter().map(|register| register.name.name.clone()))
        }
        ModuleStatement::IntegerDeclaration(integers) => {
            names.extend(integers.iter().map(|integer| integer.name.name.clone()))
        }
        ModuleStatement::TimeDeclaration(times) => {
            names.extend(times.iter().map(|time| time.name.name.clone()))
        }
        ModuleStatement::RealDeclaration(reals) => {
            names.extend(reals.iter().map(|real| real.name.name.clone()))
        }
        ModuleStatement::EventDeclaration(events) => {
            names.extend(events.iter().map(|event| event.name.name.clone()))
        }
        ModuleStatement::ParameterDeclaration(parameters) => {
            names.extend(parameters.iter().map(|it| it.name.name.clone()))
        }
        ModuleStatement::ModuleInstantiation(instantiation) => {
            names.push(instantiation.instance_name.name.clone())
        }
        // A function declared inside a generate block is stored under the
        // block's prefix, so a call written inside the block has to resolve
        // through it — without the name here, `funfun(select)` beside the
        // `endfunction` resolves outwards to the module and finds nothing
        // (corpus `generate_case2`).
        ModuleStatement::FunctionDeclaration(function) => names.push(function.name.name.clone()),
        // So is a task, and for the same reason one level down: its arguments
        // and locals are declared under the block's prefix (`gen.foo_task.x`),
        // and its body spells them `foo_task.x` — which resolves outwards to
        // the module and finds nothing unless the task's name is the block's.
        ModuleStatement::TaskDeclaration(task) => names.push(task.name.name.clone()),
        // A named block is a scope, and one opened inside a generate block
        // belongs to that block: `elaborate` declares its variables under the
        // block's prefix, so the label has to be here or a reference to one of
        // them resolves outwards to the module and finds nothing.
        ModuleStatement::InitialBlock(block) => block_labels(&block.statements, names),
        ModuleStatement::AlwaysBlock(block) => block_labels(&block.statements, names),
        _ => {}
    }
}

/// The plain identifiers an expression uses as whole operands.
///
/// What it leaves out is the point: a *select*'s name, a called function's name
/// and a `$name` are all names an implicit net could never be. An implicit net
/// is one bit wide, so a name something indexes was meant to be declared, and
/// declaring a scalar for it would turn a missing `wire [7:0]` into a silent
/// out-of-range read.
fn operand_names<'e>(expression: &'e Expression, names: &mut Vec<&'e str>) {
    match expression {
        Expression::Identifier(id) => names.push(&id.name),
        Expression::Unary(_, inner) | Expression::Parenthetical(inner) => {
            operand_names(inner, names)
        }
        Expression::Binary(lhs, _, rhs) => {
            operand_names(lhs, names);
            operand_names(rhs, names);
        }
        Expression::Conditional(condition, when_true, when_false) => {
            operand_names(condition, names);
            operand_names(when_true, names);
            operand_names(when_false, names);
        }
        Expression::Concatenation(parts) => {
            for part in parts {
                operand_names(part, names);
            }
        }
        Expression::Replication(count, parts) => {
            operand_names(count, names);
            for part in parts {
                operand_names(part, names);
            }
        }
        Expression::FunctionCall(_, arguments) | Expression::SystemFunctionCall(_, arguments) => {
            for argument in arguments {
                operand_names(argument, names);
            }
        }
        Expression::Constant(_)
        | Expression::RealLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::BitSelect(..)
        | Expression::PartSelect(..)
        | Expression::IndexedPartSelect { .. }
        | Expression::WordSelect { .. } => {}
    }
}

/// The labels of the named blocks a procedural body opens *directly*.
///
/// The walk stops at the first named block on each path, because that label is
/// the head segment of every name inside it — a block nested deeper is reached
/// through it (`outer.inner.tmp`) and needs no entry of its own. An unnamed
/// block is grouping and nothing else, so it is walked through.
fn block_labels(statements: &[ProceduralStatements], names: &mut Vec<String>) {
    for statement in statements {
        match statement {
            ProceduralStatements::Block(block) | ProceduralStatements::Fork(block) => {
                match &block.name {
                    Some(name) => names.push(name.name.clone()),
                    None => block_labels(&block.statements, names),
                }
            }
            ProceduralStatements::If(conditional) => {
                block_labels(&conditional.then_statements, names);
                if let Some(otherwise) = &conditional.else_statements {
                    block_labels(otherwise, names);
                }
            }
            ProceduralStatements::Case(case) => {
                for item in &case.items {
                    block_labels(&item.statements, names);
                }
            }
            ProceduralStatements::For(loop_) => block_labels(&loop_.statements, names),
            ProceduralStatements::While(loop_) => block_labels(&loop_.statements, names),
            ProceduralStatements::Repeat(loop_) => block_labels(&loop_.statements, names),
            ProceduralStatements::Wait(statement) => block_labels(&statement.statements, names),
            ProceduralStatements::Forever(statements)
            | ProceduralStatements::Delayed { statements, .. }
            | ProceduralStatements::EventControlled { statements, .. } => {
                block_labels(statements, names)
            }
            _ => {}
        }
    }
}

/// Rewrites every name an expression uses through `resolve`.
///
/// That includes the name of a function it calls, which is qualified exactly as
/// a signal is: a function belongs to the instance that declares it.
pub fn rename_expression(expression: &mut Expression, resolve: &dyn Fn(&str) -> String) {
    match expression {
        Expression::Constant(_) | Expression::RealLiteral(_) | Expression::StringLiteral(_) => {}
        Expression::Identifier(id) => id.name = resolve(&id.name),
        Expression::Unary(_, inner) | Expression::Parenthetical(inner) => {
            rename_expression(inner, resolve)
        }
        Expression::Binary(lhs, _, rhs) => {
            rename_expression(lhs, resolve);
            rename_expression(rhs, resolve);
        }
        Expression::Conditional(condition, when_true, when_false) => {
            rename_expression(condition, resolve);
            rename_expression(when_true, resolve);
            rename_expression(when_false, resolve);
        }
        Expression::Concatenation(parts) => {
            for part in parts {
                rename_expression(part, resolve);
            }
        }
        // A function is qualified like a signal, and for the same reason: an
        // instance's function is its own, so a call inside a child has to
        // resolve to the definition elaborated for *that* instance.
        Expression::Replication(count, parts) => {
            rename_expression(count, resolve);
            for part in parts {
                rename_expression(part, resolve);
            }
        }
        Expression::FunctionCall(id, arguments) => {
            id.name = resolve(&id.name);
            for argument in arguments {
                rename_expression(argument, resolve);
            }
        }
        // A `$name` is the simulator's, not the design's, so it is the one name
        // that is never qualified.
        Expression::SystemFunctionCall(_, arguments) => {
            for argument in arguments {
                rename_expression(argument, resolve);
            }
        }
        Expression::BitSelect(id, index) => {
            id.name = resolve(&id.name);
            rename_expression(index, resolve);
        }
        Expression::PartSelect(id, msb, lsb) => {
            id.name = resolve(&id.name);
            rename_expression(msb, resolve);
            rename_expression(lsb, resolve);
        }
        Expression::IndexedPartSelect {
            id, base, width, ..
        } => {
            id.name = resolve(&id.name);
            rename_expression(base, resolve);
            rename_expression(width, resolve);
        }
        Expression::WordSelect {
            id,
            indices,
            select,
        } => {
            id.name = resolve(&id.name);
            for index in indices {
                rename_expression(index, resolve);
            }
            for inner in select.expressions_mut() {
                rename_expression(inner, resolve);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parsers::modules::parse_module_declaration;
    use crate::simulator::runner::Simulator;

    /// A four-bit counter with its count on a port.
    const COUNTER: &str = r#"
        module counter(
            input clk,
            input rst,
            output reg [3:0] count
        );
            always @(posedge clk or posedge rst) begin
                if (rst) count <= 4'b0000;
                else count <= count + 1;
            end
        endmodule
    "#;

    /// The same counter, but keeping its state in an internal register rather
    /// than on a port, so the register only exists under a qualified name.
    const TICKER: &str = r#"
        module ticker(
            input clk,
            input rst,
            output [3:0] out
        );
            reg [3:0] count;
            always @(posedge clk or posedge rst) begin
                if (rst) count <= 4'b0000;
                else count <= count + 1;
            end
            assign out = count;
        endmodule
    "#;

    /// A stepping counter whose step is a parameter, so an override is visible
    /// in what it counts.
    const STEPPER: &str = r#"
        module stepper(
            input clk,
            input rst,
            output reg [7:0] count
        );
            parameter BASE = 100;
            parameter STEP = 1;
            always @(posedge clk or posedge rst) begin
                if (rst) count <= 8'd0;
                else count <= count + STEP;
            end
        endmodule
    "#;

    fn parse_all(sources: &[&str]) -> Vec<VerilogModule> {
        sources
            .iter()
            .map(|source| {
                let (rest, module) = parse_module_declaration(source).expect("module should parse");
                assert!(rest.trim().is_empty(), "unparsed input: {}", rest);
                module
            })
            .collect()
    }

    fn simulator_for(sources: &[&str], top: &str) -> Simulator {
        let mut simulator = Simulator::with_modules(parse_all(sources), top);
        simulator.setup().expect("design should elaborate");
        simulator
    }

    fn setup_error(sources: &[&str], top: &str) -> SimulationError {
        let mut simulator = Simulator::with_modules(parse_all(sources), top);
        simulator
            .setup()
            .expect_err("elaboration should have failed")
    }

    fn one() -> Register {
        Register::from_u128(1, 1)
    }

    fn zero() -> Register {
        Register::from_u128(0, 1)
    }

    /// Pulse `rst` so a design starts from a known state rather than all `x`.
    fn reset(simulator: &mut Simulator) {
        simulator.poke("rst", one()).unwrap();
        simulator.poke("rst", zero()).unwrap();
    }

    /// A four-bit inverter built out of four one-bit instances, which is the
    /// shape a generate loop exists for: the instance name is indexed by the
    /// genvar, and the ports are wired a bit apiece.
    const BIT_INVERTER: &str = r#"
        module bit_inverter(input a, output y);
            assign y = ~a;
        endmodule
    "#;

    const INVERTER_ARRAY: &str = r#"
        module inverter_array(input [3:0] x, output [3:0] y);
            genvar i;
            generate
                for (i = 0; i < 4; i = i + 1) begin : stage
                    bit_inverter u (.a(x[i]), .y(y[i]));
                end
            endgenerate
        endmodule
    "#;

    /// A generate loop unrolls into one real instance per iteration, each in a
    /// scope named after the loop's block and its index.
    #[test]
    fn test_generate_loop_instantiates_once_per_iteration() {
        let mut simulator = simulator_for(&[INVERTER_ARRAY, BIT_INVERTER], "inverter_array");
        for index in 0..4 {
            assert!(
                simulator.get(&format!("stage[{}].u.a", index)).is_ok(),
                "the instance in iteration {} should exist under its indexed scope",
                index
            );
        }
        assert!(
            simulator.get("stage[4].u.a").is_err(),
            "the loop should stop when its condition goes false"
        );

        simulator.poke("x", Register::from_u128(0b1010, 4)).unwrap();
        assert_eq!(
            simulator.get("y").unwrap().to_u128(),
            Some(0b0101),
            "every bit should be inverted by its own instance"
        );
    }

    /// The genvar is an elaboration-time integer: nothing of it reaches the
    /// store, because by the time anything runs there is one copy of the body
    /// per value rather than one body reading a variable.
    #[test]
    fn test_a_genvar_is_not_a_signal() {
        let simulator = simulator_for(&[INVERTER_ARRAY, BIT_INVERTER], "inverter_array");
        assert!(simulator.get("i").is_err());
        assert!(simulator.get("stage[0].i").is_err());
    }

    /// A signal declared inside a generate block belongs to the block, and a
    /// name it does not declare still belongs to the module around it.
    #[test]
    fn test_a_generate_block_is_a_nested_scope() {
        let source = r#"
            module top(input a, output y);
                wire outer;
                assign outer = ~a;
                generate
                    if (1) begin : only
                        wire inner;
                        assign inner = outer;
                        assign y = inner;
                    end
                endgenerate
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "top");
        assert!(
            simulator.get("only.inner").is_ok(),
            "a name the block declares takes the block's scope"
        );
        assert!(
            simulator.get("only.outer").is_err(),
            "a name it does not declare still belongs to the module"
        );
        simulator.poke("a", zero()).unwrap();
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(1));
    }

    /// A generate `if` elaborates one arm and *only* one: the branch not taken
    /// contributes nothing at all, not even its declarations.
    #[test]
    fn test_generate_if_takes_one_arm() {
        let source = r#"
            module top(output [7:0] q);
                parameter WIDE = 1;
                generate
                    if (WIDE > 0) begin : wide
                        wire [7:0] chosen;
                        assign chosen = 8'hA5;
                        assign q = chosen;
                    end else begin : narrow
                        wire [7:0] chosen;
                        assign chosen = 8'h5A;
                        assign q = chosen;
                    end
                endgenerate
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "top");
        simulator.run().expect("the design should settle");
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(0xA5));
        assert!(simulator.get("wide.chosen").is_ok());
        assert!(
            simulator.get("narrow.chosen").is_err(),
            "the arm not taken contributes nothing"
        );
    }

    /// A named block opened inside a generate block is a scope of *that*
    /// block, so its local is `genblk1.a.i` in the flat store. Declaring it
    /// there while a reference to it resolved outwards to a bare `a.i` is
    /// what corpus `pr2306259` hit, where iverilog 12.0 prints `PASSED`.
    #[test]
    fn test_a_named_block_inside_a_generate_block_takes_its_scope() {
        let source = r#"
            module top;
                generate
                    if (1) begin
                        initial begin : a
                            integer i;
                            i = 7;
                            $display("i=%0d", i);
                        end
                    end
                endgenerate
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "top");
        assert!(
            simulator.get("genblk1.a.i").is_ok(),
            "the block's local belongs to the generate block's scope"
        );
        simulator.advance(1).expect("the design should run");
        assert_eq!(simulator.output().text(), "i=7\n");
    }

    /// A function declared inside a generate block belongs to that block, so it
    /// is stored under the block's prefix — `blk.f` — and a call resolves to it
    /// through the scope the way any other reference does: the bare `f(…)`
    /// written beside the `endfunction`, and the hierarchical `blk.f(…)` written
    /// outside. The block's own signals are what the two spellings have to agree
    /// about, since the body reads them through the same resolver.
    ///
    /// iverilog 12.0 prints `inside=5 outside=9` for this design.
    #[test]
    fn test_a_function_declared_inside_a_generate_block_is_scoped_to_it() {
        let source = r#"
            module top;
                generate
                    if (1) begin : blk
                        function [7:0] twice;
                            input [7:0] a;
                            twice = a + a;
                        endfunction
                        initial $display("inside=%0d", twice(2) + 1);
                    end
                endgenerate
                initial #1 $display("outside=%0d", blk.twice(4) + 1);
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "top");
        simulator.advance(2).expect("the design should run");
        assert_eq!(simulator.output().text(), "inside=5\noutside=9\n");
    }

    /// A task declared in a generate block belongs to the block. A bare enable
    /// inside the block finds it ahead of a module task of the same name, one
    /// outside reaches it by the block's label, its variables live under the
    /// block's prefix, and a loop's genvar is a number inside each iteration's
    /// copy.
    ///
    /// iverilog 12.0 prints `module nudge`, `block nudge top.blk.nudge` twice,
    /// `lp 1`, `lp 0` and `seen=9`.
    #[test]
    fn test_a_task_declared_inside_a_generate_block_belongs_to_it() {
        let source = r#"
            module top;
                task nudge;
                    $display("module nudge");
                endtask
                generate
                    if (1) begin : blk
                        reg [3:0] seen;
                        task nudge;
                            begin
                                seen = 4'd9;
                                $display("block nudge %m");
                            end
                        endtask
                        initial #1 nudge;
                    end
                endgenerate
                genvar i;
                generate
                    for (i = 0; i < 2; i = i + 1) begin : lp
                        task tell;
                            $display("lp %0d", i);
                        endtask
                    end
                endgenerate
                initial begin
                    nudge;
                    #2 blk.nudge;
                    lp[1].tell;
                    lp[0].tell;
                    $display("seen=%0d", blk.seen);
                end
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "top");
        simulator.advance(5).expect("the design should run");
        assert_eq!(
            simulator.output().text(),
            "module nudge\nblock nudge top.blk.nudge\nblock nudge top.blk.nudge\n\
             lp 1\nlp 0\nseen=9\n"
        );
    }

    /// A function inside a generate loop reads the loop's genvar as the number
    /// it is in that iteration, rather than as a signal nothing declares.
    /// iverilog 12.0 prints `0 1`.
    #[test]
    fn test_a_function_inside_a_generate_loop_reads_its_genvar() {
        let source = r#"
            module top;
                genvar k;
                generate
                    for (k = 0; k < 2; k = k + 1) begin : ff
                        function f;
                            input dummy;
                            f = k % 2;
                        endfunction
                    end
                endgenerate
                initial $display("%0d %0d", ff[0].f(0), ff[1].f(0));
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "top");
        simulator.advance(1).expect("the design should run");
        assert_eq!(simulator.output().text(), "0 1\n");
    }

    /// An enable of another instance's task runs that instance's body against
    /// that instance's signals, and a `#delay` inside one suspends the caller
    /// exactly as a local enable's does. The enable written *inside a task* of
    /// the caller is the same thing one level down, and `top.nudge` names the
    /// top module by its own name — the root of the flat name space.
    ///
    /// iverilog 12.0 prints `q=7` then `q=9`.
    #[test]
    fn test_an_enable_of_another_instances_task() {
        let top = r#"
            module top;
                wire [7:0] q;
                child c (q);
                task nudge;
                    begin
                        c.load(8'd7);
                    end
                endtask
                initial begin
                    top.nudge;
                    #2 $display("q=%0d", q);
                    c.load(8'd9);
                    #2 $display("q=%0d", q);
                end
            endmodule
        "#;
        let child = r#"
            module child (out);
                output [7:0] out;
                reg [7:0] out;
                task load;
                    input [7:0] v;
                    begin
                        #1 out = v;
                    end
                endtask
            endmodule
        "#;
        let mut simulator = simulator_for(&[top, child], "top");
        simulator.advance(10).expect("the design should run");
        assert_eq!(simulator.output().text(), "q=7\nq=9\n");
    }

    /// An `output` argument is copied back into the caller's own variable after
    /// the body has run, the same way a local enable's is. iverilog 12.0 prints
    /// `r=6`.
    #[test]
    fn test_an_enable_of_another_instances_task_copies_its_output_back() {
        let top = r#"
            module top;
                reg [7:0] r;
                child c ();
                initial begin
                    c.bump(8'd5, r);
                    $display("r=%0d", r);
                end
            endmodule
        "#;
        let child = r#"
            module child;
                task bump;
                    input [7:0] v;
                    output [7:0] w;
                    begin
                        w = v + 1;
                    end
                endtask
            endmodule
        "#;
        let mut simulator = simulator_for(&[top, child], "top");
        simulator.advance(1).expect("the design should run");
        assert_eq!(simulator.output().text(), "r=6\n");
    }

    /// A path naming a task nothing declares is still `UnknownTask`, because a
    /// dotted name only defers the question until the hierarchy is walked.
    #[test]
    fn test_an_enable_of_a_task_no_instance_declares_is_named() {
        let top = r#"
            module top;
                child c ();
                initial c.missing;
            endmodule
        "#;
        let child = "module child; endmodule";
        assert!(
            matches!(setup_error(&[top, child], "top"), SimulationError::UnknownTask(name) if name == "c.missing")
        );
    }

    /// Two tasks that enable each other across instances never finish being
    /// inlined — every round is a real copy of both bodies — so the cycle is
    /// found by name in the enable graph before any of it is spliced.
    #[test]
    fn test_a_cycle_of_hierarchical_enables_is_reported() {
        let top = r#"
            module top;
                a ua ();
                b ub ();
                initial ua.ping;
            endmodule
        "#;
        let a = r#"
            module a;
                task ping;
                    top.ub.pong;
                endtask
            endmodule
        "#;
        let b = r#"
            module b;
                task pong;
                    top.ua.ping;
                endtask
            endmodule
        "#;
        assert!(matches!(
            setup_error(&[top, a, b], "top"),
            SimulationError::RecursiveTask(_)
        ));
    }

    /// A function may not enable a task — it cannot consume time, and a frame
    /// has no driver behind it — and the refusal names *that* rather than the
    /// "assigning a signal outside itself" the inlined body would report.
    #[test]
    fn test_a_task_enable_inside_a_function_is_named() {
        let top = r#"
            module top;
                child c ();
                function [7:0] f;
                    input [7:0] a;
                    begin
                        c.load(a);
                        f = a;
                    end
                endfunction
                initial $display("%0d", f(1));
            endmodule
        "#;
        let child = r#"
            module child;
                reg [7:0] held;
                task load;
                    input [7:0] v;
                    held = v;
                endtask
            endmodule
        "#;
        assert!(matches!(
            setup_error(&[top, child], "top"),
            SimulationError::Unsupported("a task enable inside a function")
        ));
    }

    /// A generate `case` picks its arm by identity, and falls through to
    /// `default` when nothing matches.
    #[test]
    fn test_generate_case_picks_an_arm() {
        let source = r#"
            module top(output [7:0] q);
                parameter MODE = 2;
                generate
                    case (MODE)
                        0: begin : m0 assign q = 8'd10; end
                        1, 2: begin : m1 assign q = 8'd20; end
                        default: begin : md assign q = 8'd30; end
                    endcase
                endgenerate
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "top");
        simulator.run().expect("the design should settle");
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(20));

        let fallback = source.replace("parameter MODE = 2;", "parameter MODE = 7;");
        let mut simulator = simulator_for(&[&fallback], "top");
        simulator.run().expect("the design should settle");
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(30));
    }

    /// A range bound written inside a generate loop may name the genvar, so the
    /// genvar has to be in scope when the declaration's width is resolved.
    #[test]
    fn test_a_genvar_sizes_a_declaration() {
        let source = r#"
            module top;
                genvar i;
                generate
                    for (i = 0; i < 3; i = i + 1) begin : sized
                        reg [i:0] r;
                    end
                endgenerate
            endmodule
        "#;
        let simulator = simulator_for(&[source], "top");
        for index in 0..3 {
            let name = format!("sized[{}].r", index);
            assert_eq!(
                simulator
                    .get(&name)
                    .expect("the register should exist")
                    .width(),
                index + 1,
                "`reg [i:0]` should be i+1 bits wide in iteration {}",
                index
            );
        }
    }

    /// A loop unrolled twice gives each iteration its own procedural state,
    /// which is what the indexed scope name is for.
    #[test]
    fn test_a_loop_body_runs_once_per_iteration() {
        let source = r#"
            module top;
                genvar i;
                generate
                    for (i = 0; i < 3; i = i + 1) begin : counted
                        reg [7:0] seen;
                        initial seen = i * 2;
                    end
                endgenerate
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "top");
        simulator.advance(1).expect("the design should run");
        for index in 0..3u128 {
            assert_eq!(
                simulator
                    .get(&format!("counted[{}].seen", index))
                    .unwrap()
                    .to_u128(),
                Some(index * 2)
            );
        }
    }

    const VALUED: &str = r#"
        module valued(output [7:0] q);
            parameter VALUE = 1;
            assign q = VALUE;
        endmodule
    "#;

    /// `defparam` overrides a parameter of an instance the module names, and
    /// beats a `#(...)` written on that same instantiation.
    #[test]
    fn test_defparam_overrides_an_instance_parameter() {
        let source = r#"
            module top(output [7:0] a, output [7:0] b);
                valued one (.q(a));
                valued #(.VALUE(3)) two (.q(b));
                defparam one.VALUE = 9;
                defparam two.VALUE = 4;
            endmodule
        "#;
        let mut simulator = simulator_for(&[source, VALUED], "top");
        simulator.run().expect("the design should settle");
        assert_eq!(simulator.get("a").unwrap().to_u128(), Some(9));
        assert_eq!(
            simulator.get("b").unwrap().to_u128(),
            Some(4),
            "a defparam beats the instantiation's own override"
        );
    }

    /// A `defparam` reaches through more than one level, because the path it
    /// names is exactly the flat name the parameter ends up under.
    #[test]
    fn test_defparam_reaches_through_a_hierarchy() {
        let middle = r#"
            module middle(output [7:0] q);
                valued leaf (.q(q));
            endmodule
        "#;
        let source = r#"
            module top(output [7:0] q);
                middle mid (.q(q));
                defparam mid.leaf.VALUE = 7;
            endmodule
        "#;
        let mut simulator = simulator_for(&[source, middle, VALUED], "top");
        simulator.run().expect("the design should settle");
        assert_eq!(simulator.get("q").unwrap().to_u128(), Some(7));
    }

    /// A `defparam` that names nothing is reported. An override that quietly
    /// did not happen leaves the design running on the value it was told not
    /// to use, which is indistinguishable from one that was never written.
    #[test]
    fn test_an_unapplied_defparam_is_named() {
        let source = r#"
            module top(output [7:0] q);
                valued one (.q(q));
                defparam one.MISSPELT = 9;
            endmodule
        "#;
        match setup_error(&[source, VALUED], "top") {
            SimulationError::UnappliedDefparam(path) => assert_eq!(path, "one.MISSPELT"),
            other => panic!("expected an unapplied defparam, got {:?}", other),
        }
    }

    /// A loop whose condition never goes false is reported rather than run:
    /// every iteration is a real copy of the body.
    #[test]
    fn test_a_runaway_generate_loop_is_reported() {
        let source = r#"
            module top;
                genvar i;
                generate
                    for (i = 0; i >= 0; i = i + 1) begin : forever_more
                        wire w;
                    end
                endgenerate
            endmodule
        "#;
        assert!(matches!(
            setup_error(&[source], "top"),
            SimulationError::GenerateLoopBound { .. }
        ));
    }

    /// A loop bound the elaborator cannot evaluate is named, the way a range
    /// bound is. A width or a count the simulator picked for itself would be
    /// the wrong design rather than a wrong number.
    #[test]
    fn test_a_non_constant_generate_bound_is_named() {
        let source = r#"
            module top;
                genvar i;
                generate
                    for (i = 0; i < nothing_declares_this; i = i + 1) begin : sized
                        wire w;
                    end
                endgenerate
            endmodule
        "#;
        assert!(matches!(
            setup_error(&[source], "top"),
            SimulationError::UnresolvedGenerate { .. }
        ));
    }

    /// A hierarchical name reaches into an instance and into a generate block,
    /// because flattening gives both of them exactly the dotted spelling the
    /// reference is written with.
    #[test]
    fn test_a_hierarchical_name_reads_inside_a_scope() {
        let source = r#"
            module top(input [3:0] x, output first, output third);
                wire [3:0] inverted;
                assign first = stage[0].u.y;
                assign third = top.stage[2].u.y;
                genvar i;
                generate
                    for (i = 0; i < 4; i = i + 1) begin : stage
                        bit_inverter u (.a(x[i]), .y(inverted[i]));
                    end
                endgenerate
            endmodule
        "#;
        let mut simulator = simulator_for(&[source, BIT_INVERTER], "top");
        simulator.poke("x", Register::from_u128(0b0100, 4)).unwrap();
        assert_eq!(
            simulator.get("first").unwrap().to_u128(),
            Some(1),
            "a relative hierarchical name reaches the first instance"
        );
        assert_eq!(
            simulator.get("third").unwrap().to_u128(),
            Some(0),
            "a name starting at the top module reaches the third"
        );
    }

    /// A reference to an *aliased port* names an entry the store does not have,
    /// because the port and the parent's signal are one entry under the
    /// parent's name. It resolves anyway, and it resolves even when the
    /// instantiation is written *below* the block that reads it — the alias is
    /// recorded in the build pass, long after that block was renamed.
    ///
    /// iverilog 12.0 prints `x=1010 u.p=1010 same=1`.
    #[test]
    fn test_a_reference_to_an_aliased_port_reads_the_signal_it_aliases() {
        let top = r#"
            module top;
                reg [3:0] x;
                initial begin
                    x = 4'b1010;
                    #1 $display("x=%b u.p=%b same=%b", x, u.p, x === u.p);
                end
                child u (.p(x));
            endmodule
        "#;
        let child = r#"
            module child (p);
                input [3:0] p;
            endmodule
        "#;
        let mut simulator = simulator_for(&[top, child], "top");
        simulator.advance(2).expect("the design should run");
        assert_eq!(simulator.output().text(), "x=1010 u.p=1010 same=1\n");
    }

    /// A *function body* may name one as readily as a block can, and its read
    /// set is what a call copies into its frame — so the definitions on the
    /// store go through the alias pass beside the blocks. iverilog 12.0 prints
    /// `through=1010`.
    #[test]
    fn test_a_function_reads_through_an_aliased_port() {
        let top = r#"
            module top;
                reg [3:0] x;
                function [3:0] through;
                    input dummy;
                    through = u.p;
                endfunction
                initial begin
                    x = 4'b1010;
                    #1 $display("through=%b", through(0));
                end
                child u (.p(x));
            endmodule
        "#;
        let child = r#"
            module child (p);
                input [3:0] p;
            endmodule
        "#;
        let mut simulator = simulator_for(&[top, child], "top");
        simulator.advance(2).expect("the design should run");
        assert_eq!(simulator.output().text(), "through=1010\n");
    }

    /// An output bound to a bit select is the alias run backwards: the port
    /// keeps a signal of its own and a continuous assignment carries it out.
    /// That is how a generate loop wires an instance per bit.
    #[test]
    fn test_an_output_bound_to_a_select_drives_the_parent() {
        let source = r#"
            module top(input a, output [3:0] y);
                bit_inverter u (.a(a), .y(y[2]));
                assign y[0] = 1'b0;
                assign y[1] = 1'b0;
                assign y[3] = 1'b0;
            endmodule
        "#;
        let mut simulator = simulator_for(&[source, BIT_INVERTER], "top");
        simulator.poke("a", zero()).unwrap();
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(0b0100));
    }

    /// A `signed` qualifier is a property of the declaration, so it has to
    /// survive elaboration and land on the store entry — including through an
    /// instance, where the name it lands under is the qualified one.
    #[test]
    fn test_signedness_reaches_the_elaborated_signals() {
        let child = r#"
            module child(input signed [3:0] p, output [3:0] q);
                reg signed [3:0] r;
                wire signed [7:0] w;
                integer i;
                reg [3:0] plain;
                wire [7:0] bare;
            endmodule
        "#;
        let top = r#"
            module top();
                wire [3:0] a, b;
                child dut (.p(a), .q(b));
            endmodule
        "#;

        let simulator = simulator_for(&[top, child], "top");
        let signed = |name: &str| {
            simulator
                .get(name)
                .unwrap_or_else(|_| panic!("no signal {}", name))
                .is_signed()
        };

        assert!(signed("dut.r"), "reg signed");
        assert!(signed("dut.w"), "wire signed");
        // An `integer` is signed by being an `integer`; there is no qualifier.
        assert!(signed("dut.i"), "integer");
        assert!(!signed("dut.plain"), "reg without a qualifier");
        assert!(!signed("dut.bare"), "wire without a qualifier");
        // A port bound to a plain identifier *is* the parent's signal, and the
        // parent declared that one unsigned.
        assert!(!signed("a"), "the parent's own wire");
    }

    #[test]
    fn test_parent_clocks_a_child_counter() {
        let top = r#"
            module top(
                input clk,
                input rst,
                output [3:0] out
            );
                wire [3:0] inner;
                counter dut (.clk(clk), .rst(rst), .count(inner));
                assign out = inner;
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, COUNTER], "top");
        reset(&mut simulator);
        assert_eq!(simulator.get("dut.count").unwrap().to_u128(), Some(0));

        for _ in 0..3 {
            simulator.tick("clk").unwrap();
        }

        // The child counted, and the count reached the parent's output.
        assert_eq!(simulator.get("dut.count").unwrap().to_u128(), Some(3));
        assert_eq!(simulator.get("inner").unwrap().to_u128(), Some(3));
        assert_eq!(simulator.get("out").unwrap().to_u128(), Some(3));
    }

    /// The child's `count` is internal, so its only name is the qualified one —
    /// and the parent has a `count` of its own to collide with if the
    /// namespacing were wrong.
    #[test]
    fn test_child_register_is_namespaced_away_from_the_parents() {
        let top = r#"
            module top(
                input clk,
                input rst,
                output [3:0] out
            );
                reg [3:0] count;
                wire [3:0] inner;
                ticker dut (.clk(clk), .rst(rst), .out(inner));
                always @(posedge clk or posedge rst) begin
                    if (rst) count <= 4'b1111;
                    else count <= count - 1;
                end
                assign out = count;
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, TICKER], "top");
        reset(&mut simulator);
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(15));
        assert_eq!(simulator.get("dut.count").unwrap().to_u128(), Some(0));

        for _ in 0..3 {
            simulator.tick("clk").unwrap();
        }

        // One counts up, the other down, from the same clock.
        assert_eq!(simulator.get("dut.count").unwrap().to_u128(), Some(3));
        assert_eq!(simulator.get("count").unwrap().to_u128(), Some(12));
    }

    /// Two instances of one module, clocked differently. If the flattening put
    /// their state anywhere in common, they would track each other.
    #[test]
    fn test_two_instances_keep_separate_state() {
        let top = r#"
            module top(
                input clk_a,
                input clk_b,
                input rst,
                output [3:0] a,
                output [3:0] b
            );
                wire [3:0] wire_a;
                wire [3:0] wire_b;
                counter one (.clk(clk_a), .rst(rst), .count(wire_a));
                counter two (.clk(clk_b), .rst(rst), .count(wire_b));
                assign a = wire_a;
                assign b = wire_b;
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, COUNTER], "top");
        reset(&mut simulator);

        for _ in 0..3 {
            simulator.tick("clk_a").unwrap();
        }
        simulator.tick("clk_b").unwrap();

        assert_eq!(simulator.get("a").unwrap().to_u128(), Some(3));
        assert_eq!(simulator.get("b").unwrap().to_u128(), Some(1));
        assert_eq!(simulator.get("one.count").unwrap().to_u128(), Some(3));
        assert_eq!(simulator.get("two.count").unwrap().to_u128(), Some(1));
    }

    /// Positional arguments bind in port-declaration order, so this is the same
    /// design as the named version and has to behave identically.
    #[test]
    fn test_positional_binding_matches_port_order() {
        let top = r#"
            module top(
                input clk,
                input rst,
                output [3:0] out
            );
                wire [3:0] inner;
                counter dut (clk, rst, inner);
                assign out = inner;
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, COUNTER], "top");
        reset(&mut simulator);
        for _ in 0..5 {
            simulator.tick("clk").unwrap();
        }
        assert_eq!(simulator.get("out").unwrap().to_u128(), Some(5));
    }

    /// A named connection binds by port name, so writing the ports in a
    /// different order must not change the design.
    #[test]
    fn test_named_binding_ignores_argument_order() {
        let top = r#"
            module top(
                input clk,
                input rst,
                output [3:0] out
            );
                wire [3:0] inner;
                counter dut (.count(inner), .rst(rst), .clk(clk));
                assign out = inner;
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, COUNTER], "top");
        reset(&mut simulator);
        for _ in 0..5 {
            simulator.tick("clk").unwrap();
        }
        assert_eq!(simulator.get("out").unwrap().to_u128(), Some(5));
    }

    /// A port bound to a plain identifier is the parent's signal, not a copy of
    /// it, so both spellings read back the same value at the same instant.
    #[test]
    fn test_an_aliased_port_shares_the_parents_entry() {
        let top = r#"
            module top(
                input clk,
                input rst,
                output [3:0] out
            );
                counter dut (.clk(clk), .rst(rst), .count(out));
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, COUNTER], "top");
        reset(&mut simulator);
        simulator.poke("clk", one()).unwrap();

        assert_eq!(simulator.get("dut.clk").unwrap().to_binary(), "1");
        assert_eq!(simulator.get("clk").unwrap().to_binary(), "1");
        // `count` is the same entry as the parent's `out` port.
        assert_eq!(simulator.get("dut.count").unwrap().to_u128(), Some(1));
        assert_eq!(simulator.get("out").unwrap().to_u128(), Some(1));
    }

    /// A `wire` in the parent bound to an `output reg` in the child reads `x`
    /// before anything drives it, not `z`: the `reg` *is* the net's driver, and
    /// what a driver has not said yet is unknown rather than floating. Both
    /// spellings of the child's declaration say it — `output reg q` in the
    /// header and `output q; reg q;` in the body.
    ///
    /// iverilog 12.0 prints `ansi=x body=x float=z` for this design.
    #[test]
    fn test_a_wire_driven_by_a_childs_reg_starts_unknown() {
        let ansi = r#"
            module ansi(output reg q);
            endmodule
        "#;
        let body = r#"
            module body(q);
                output q;
                reg q;
            endmodule
        "#;
        let floating = r#"
            module floating(q);
                output q;
            endmodule
        "#;
        let top = r#"
            module top();
                wire a, b, c;
                ansi u1 (a);
                body u2 (b);
                floating u3 (c);
                initial $display("ansi=%b body=%b float=%b", a, b, c);
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, ansi, body, floating], "top");
        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().lines(), vec!["ansi=x body=x float=z"]);
    }

    /// A port whose declaration disagrees with the parent's about signedness
    /// is not aliased: the entry it would share carries *one* signedness, and
    /// the child's is the one its own body must read. So `input signed [3:0]`
    /// bound to an unsigned `reg [3:0]` compares `4'b1000` as -8 rather than
    /// as 8, in both header spellings and however the parent wrote the
    /// connection.
    ///
    /// iverilog 12.0 prints `y1=0 y2=0 y3=0` for this design — `7 <= -8` is
    /// false all three ways.
    #[test]
    fn test_a_signed_port_is_signed_inside_the_child() {
        let sub = r#"
            module sub(a, b, y);
                input signed [3:0] a;
                input signed [3:0] b;
                output y;
                assign y = a <= b;
            endmodule
        "#;
        let ansi = r#"
            module ansi(input signed [3:0] a, input signed [3:0] b, output y);
                assign y = a <= b;
            endmodule
        "#;
        let top = r#"
            module top();
                reg [3:0] p, q;
                wire y1, y2, y3;
                sub u1 (p, q, y1);
                ansi u2 (p, q, y2);
                sub u3 (p + 0, q + 0, y3);
                initial begin
                    p = 4'b0111;
                    q = 4'b1000;
                    #1 $display("y1=%b y2=%b y3=%b", y1, y2, y3);
                end
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, sub, ansi], "top");
        simulator.advance(5).expect("time should advance");
        assert_eq!(simulator.output().lines(), vec!["y1=0 y2=0 y3=0"]);
    }

    /// A port that is **not the same number of bits** as the signal it is bound
    /// to cannot be aliased: one store entry has nowhere to extend or truncate.
    /// It keeps an entry of its own and a continuous assignment carries the
    /// value across, so the conversion is the one an assignment gives — a
    /// signed port sign extends, an unsigned one zero extends, and a target
    /// narrower than the port truncates.
    ///
    /// Measured against iverilog 12.0, which warns about each connection and
    /// then prints:
    ///
    /// ```text
    /// signed out   11111101
    /// unsigned out 00001101
    /// narrow out   01
    /// signed in    11111101
    /// unsigned in  00001101
    /// ```
    ///
    /// Aliasing gives the *parent's* width in both directions instead, which
    /// is what made corpus `pr2121536` and `pr2121536b` zero extend a signed
    /// output.
    #[test]
    fn test_a_port_of_a_different_width_is_converted_rather_than_aliased() {
        let top = r#"
            module top;
              reg signed [3:0] s = -3;
              reg [3:0] u = 4'b1101;
              wire signed [7:0] so;
              wire [7:0] uo;
              wire [1:0] narrow;

              widen w1(so, s);
              widenu w2(uo, u);
              narrowd n1(narrow, u);

              initial #1 begin
                $display("signed out   %b", so);
                $display("unsigned out %b", uo);
                $display("narrow out   %b", narrow);
                $display("signed in    %b", w1.lin);
                $display("unsigned in  %b", w2.lin);
              end
            endmodule
        "#;
        let widen = r#"
            module widen(lrtn, lin);
              output signed [3:0] lrtn;
              input signed [7:0] lin;
              assign lrtn = lin[3:0];
            endmodule
        "#;
        let widenu = r#"
            module widenu(lrtn, lin);
              output [3:0] lrtn;
              input [7:0] lin;
              assign lrtn = lin[3:0];
            endmodule
        "#;
        let narrowd = r#"
            module narrowd(lrtn, lin);
              output [3:0] lrtn;
              input [7:0] lin;
              assign lrtn = lin[3:0];
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, widen, widenu, narrowd], "top");
        simulator.advance(20).expect("time should advance");
        assert_eq!(
            simulator.output().text(),
            "signed out   11111101\n\
             unsigned out 00001101\n\
             narrow out   01\n\
             signed in    11111101\n\
             unsigned in  00001101\n"
        );
    }

    /// Combinational output flowing back up into a parent expression.
    #[test]
    fn test_output_port_propagates_into_the_parent() {
        let adder = r#"
            module adder(
                input [7:0] x,
                input [7:0] y,
                output [7:0] z
            );
                assign z = x + y;
            endmodule
        "#;
        let top = r#"
            module top(
                input [7:0] a,
                input [7:0] b,
                output [7:0] doubled
            );
                wire [7:0] sum;
                adder dut (.x(a), .y(b), .z(sum));
                assign doubled = sum + sum;
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, adder], "top");
        simulator.set_input("a", Register::from_u128(3, 8)).unwrap();
        simulator.set_input("b", Register::from_u128(4, 8)).unwrap();
        simulator.run().unwrap();

        assert_eq!(simulator.get("sum").unwrap().to_u128(), Some(7));
        assert_eq!(simulator.get("doubled").unwrap().to_u128(), Some(14));
    }

    /// An input bound to a general expression gets a signal of its own plus a
    /// continuous assignment from the parent, since there is nothing to alias.
    #[test]
    fn test_expression_connections_drive_an_input_port() {
        let adder = r#"
            module adder(
                input [7:0] x,
                input [7:0] y,
                output [7:0] z
            );
                assign z = x + y;
            endmodule
        "#;
        let top = r#"
            module top(
                input [7:0] a,
                output [7:0] sum
            );
                adder dut (.x(a + 1), .y(8'd2), .z(sum));
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, adder], "top");
        simulator.set_input("a", Register::from_u128(5, 8)).unwrap();
        simulator.run().unwrap();

        assert_eq!(simulator.get("dut.x").unwrap().to_u128(), Some(6));
        assert_eq!(simulator.get("dut.y").unwrap().to_u128(), Some(2));
        assert_eq!(simulator.get("sum").unwrap().to_u128(), Some(8));
    }

    /// `@(*)` inside an instance has to be sensitive to the *parent's* signals,
    /// since that is what its reads were rewritten to.
    #[test]
    fn test_implicit_sensitivity_survives_flattening() {
        let masker = r#"
            module masker(
                input [3:0] a,
                output reg [3:0] y
            );
                always @(*) y = a & 4'b0011;
            endmodule
        "#;
        let top = r#"
            module top(
                input [3:0] src,
                output [3:0] masked
            );
                masker dut (.a(src), .y(masked));
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, masker], "top");
        simulator
            .poke("src", Register::from_binary("1111"))
            .unwrap();
        assert_eq!(simulator.get("masked").unwrap().to_binary(), "0011");

        simulator
            .poke("src", Register::from_binary("1010"))
            .unwrap();
        assert_eq!(simulator.get("masked").unwrap().to_binary(), "0010");
    }

    /// A port nobody connected is floating, which is what `z` means.
    #[test]
    fn test_an_unconnected_input_is_high_impedance() {
        let sink = r#"
            module sink(
                input [3:0] in,
                output [3:0] out
            );
                assign out = in;
            endmodule
        "#;
        let top = r#"
            module top(
                output [3:0] out
            );
                sink dut (.out(out));
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, sink], "top");
        assert_eq!(simulator.get("dut.in").unwrap().to_binary(), "zzzz");

        // And the floating value propagates the way any other would.
        simulator.run().unwrap();
        assert_eq!(simulator.get("out").unwrap().to_binary(), "zzzz");
    }

    /// Unless the module was *declared* inside an `` `unconnected_drive ``
    /// region, in which case an unconnected input of it reads that level.
    /// IEEE 1364-2005 §19.9, and corpus `uncon_drive` and `br_gh782c`, which
    /// iverilog 12.0 answers `PASSED`.
    ///
    /// The directive belongs to the module's declaration rather than to the
    /// instantiation, so it is carried on [`VerilogModule::unconnected_drive`]
    /// and reaches `declare_port` with the port.
    #[test]
    fn test_an_unconnected_drive_region_pulls_an_unconnected_input() {
        let source = "\
            module top(output [3:0] hi, output [3:0] lo, output [3:0] floating);\n\
              pullhigh a (.out(hi));\n\
              pulllow  b (.out(lo));\n\
              plain    c (.out(floating));\n\
            endmodule\n\
            module plain(input [3:0] in, output [3:0] out); assign out = in; endmodule\n\
            `unconnected_drive pull1\n\
            module pullhigh(input [3:0] in, output [3:0] out); assign out = in; endmodule\n\
            `nounconnected_drive\n\
            `unconnected_drive pull0\n\
            module pulllow(input [3:0] in, output [3:0] out); assign out = in; endmodule\n\
            `nounconnected_drive\n";

        let expanded = crate::parsers::preprocessor::Preprocessor::new()
            .preprocess(source, "test.v")
            .expect("should preprocess");
        let parsed = crate::parsers::source::parse_expanded(expanded).expect("design should parse");
        let mut simulator = Simulator::with_modules(parsed.modules, "top");
        simulator.setup().expect("design should elaborate");
        simulator.run().expect("design should settle");

        assert_eq!(simulator.get("hi").unwrap().to_binary(), "1111");
        assert_eq!(simulator.get("lo").unwrap().to_binary(), "0000");
        assert_eq!(simulator.get("floating").unwrap().to_binary(), "zzzz");
    }

    /// **A range is what decides a parameter's signedness**, failing a
    /// `signed` qualifier: one written with a range is unsigned unless it says
    /// otherwise, and only a rangeless parameter keeps the signedness its
    /// value arrived with.
    ///
    /// The trap is that a bare decimal is itself signed, so reading the
    /// value's own flag makes `parameter [3:0] DAC = 8;` into `-8` — and a
    /// select through it then names a bit nothing has. iverilog 12.0 prints
    /// `8 0 / -3 1 / -8 1` for this design (corpus `pr542`).
    #[test]
    fn test_a_range_decides_a_parameters_signedness() {
        let mut simulator = simulator_for(
            &[r#"
            module t();
                parameter [3:0] ranged = 8;
                parameter bare = -3;
                parameter signed [3:0] qualified = 8;
                initial begin
                    $display("%0d %0b", ranged, ranged < 0);
                    $display("%0d %0b", bare, bare < 0);
                    $display("%0d %0b", qualified, qualified < 0);
                end
            endmodule
        "#],
            "t",
        );

        simulator.advance(1).expect("time should advance");
        assert_eq!(simulator.output().text(), "8 0\n-3 1\n-8 1\n");
    }

    /// A `signed` qualifier on a parameter has to survive being stored, which
    /// is not automatic: widening a register rebuilds it and does not carry the
    /// flag, so it is applied both before the coercion (to sign extend rather
    /// than zero extend) and after it (to persist).
    #[test]
    fn test_a_signed_parameter_compares_as_two_s_complement() {
        let mut simulator = simulator_for(
            &[r#"
            module main;
                parameter signed [7:0] neg = -1;
                parameter [7:0] pos = 8'hff;
                reg below_zero;
                reg unsigned_below_zero;
                initial begin
                    below_zero = neg < 0;
                    unsigned_below_zero = pos < 0;
                end
            endmodule
        "#],
            "main",
        );
        simulator.advance(10).unwrap();
        assert_eq!(simulator.get("below_zero").unwrap().to_binary(), "1");
        assert_eq!(
            simulator.get("unsigned_below_zero").unwrap().to_binary(),
            "0",
            "the same bits without the qualifier are unsigned"
        );
    }

    /// `integer` names a signed type, so it implies the qualifier.
    #[test]
    fn test_an_integer_parameter_is_signed() {
        let mut simulator = simulator_for(
            &[r#"
            module main;
                parameter integer n = -1;
                reg below_zero;
                initial below_zero = n < 0;
            endmodule
        "#],
            "main",
        );
        simulator.advance(10).unwrap();
        assert_eq!(simulator.get("below_zero").unwrap().to_binary(), "1");
    }

    #[test]
    fn test_parameter_override_changes_a_childs_behaviour() {
        let top = r#"
            module top(
                input clk,
                input rst,
                output [7:0] slow,
                output [7:0] fast
            );
                stepper plain (.clk(clk), .rst(rst), .count(slow));
                stepper #(.STEP(8'd5)) quick (.clk(clk), .rst(rst), .count(fast));
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, STEPPER], "top");
        reset(&mut simulator);
        for _ in 0..3 {
            simulator.tick("clk").unwrap();
        }

        assert_eq!(simulator.get("slow").unwrap().to_u128(), Some(3));
        assert_eq!(simulator.get("fast").unwrap().to_u128(), Some(15));
        // The default is untouched in the instance that did not override it.
        assert_eq!(simulator.get("plain.STEP").unwrap().to_u128(), Some(1));
        assert_eq!(simulator.get("quick.STEP").unwrap().to_u128(), Some(5));
    }

    /// A declared width may be an *expression* over the parameters in scope,
    /// and it is resolved where those have values: at elaboration.
    #[test]
    fn test_a_declared_width_may_be_an_expression() {
        let modules = parse_all(&[r#"
            module sized();
                parameter WIDTH = 8;
                reg [WIDTH-1:0] q;
                wire [0:WIDTH/2-1] half;
                reg [WIDTH-1:0] mem [0:WIDTH-1];
            endmodule
        "#]);
        let elaborated = elaborate(&modules, 0).expect("design should elaborate");

        assert_eq!(elaborated.state.get("q").unwrap().width(), 8);
        assert_eq!(elaborated.state.get("half").unwrap().width(), 4);
        let memory = elaborated.state.memory("mem").expect("mem is a memory");
        assert_eq!(memory.depth(), 8);
        assert_eq!(memory.width(), 8);
    }

    /// The point of the whole thing: an override in the parent decides the
    /// child's widths, so two instances of one module are different sizes.
    #[test]
    fn test_a_parameter_override_changes_a_childs_width() {
        let child = r#"
            module vector(output [WIDTH-1:0] q);
                parameter WIDTH = 4;
                reg [WIDTH-1:0] hold;
            endmodule
        "#;
        let top = r#"
            module top();
                wire [3:0] narrow;
                wire [15:0] wide;
                vector small (.q(narrow));
                vector #(.WIDTH(16)) large (.q(wide));
            endmodule
        "#;
        let modules = parse_all(&[top, child]);
        let elaborated = elaborate(&modules, 0).expect("design should elaborate");

        assert_eq!(elaborated.state.get("small.hold").unwrap().width(), 4);
        assert_eq!(elaborated.state.get("large.hold").unwrap().width(), 16);
    }

    /// A port width is a range like any other, and it is resolved before the
    /// ports are declared — which is why the parameters go first.
    #[test]
    fn test_a_port_width_may_be_a_parameter() {
        let modules = parse_all(&[r#"
            module ported(input [W-1:0] a, output [W-1:0] y);
                parameter W = 12;
                assign y = a;
            endmodule
        "#]);
        let elaborated = elaborate(&modules, 0).expect("design should elaborate");

        assert_eq!(elaborated.state.get("a").unwrap().width(), 12);
        assert_eq!(elaborated.state.get("y").unwrap().width(), 12);
    }

    /// A function's return width and its arguments are ranges too, so they are
    /// parameterised the same way.
    #[test]
    fn test_a_function_width_may_be_a_parameter() {
        let modules = parse_all(&[r#"
            module functional(output [15:0] y);
                parameter W = 16;
                function [W-1:0] widen;
                    input [W-1:0] a;
                    widen = a + 1;
                endfunction
                assign y = widen(16'd41);
            endmodule
        "#]);
        let mut simulator = Simulator::with_modules(modules, "functional");
        simulator.setup().expect("design should elaborate");
        simulator.run().expect("design should settle");
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(42));
    }

    /// A bound that is not a constant is a **named** error. A width the
    /// simulator picked for itself would be wrong for the whole run and look
    /// exactly like nothing having gone wrong.
    #[test]
    fn test_an_unresolvable_range_bound_is_named() {
        let error = setup_error(
            &[r#"
                module unsized();
                    reg [n-1:0] q;
                endmodule
            "#],
            "unsized",
        );
        match error {
            SimulationError::UnresolvedRange { ref bound, .. } => assert_eq!(bound, "n - 1"),
            ref other => panic!("expected an unresolved range, got {:?}", other),
        }
        assert!(error.to_string().contains("n - 1"), "{}", error);
    }

    /// A parameter may be written in terms of a function the module declares,
    /// and a function's width in terms of a parameter. Neither ordering is a
    /// failure, which is what holding back an unevaluated parameter buys.
    #[test]
    fn test_a_parameter_may_still_be_a_function_call() {
        let modules = parse_all(&[r#"
            module called(output [7:0] y);
                parameter N = twice(4);
                function [7:0] twice;
                    input [7:0] a;
                    twice = a * 2;
                endfunction
                assign y = N;
            endmodule
        "#]);
        let mut simulator = Simulator::with_modules(modules, "called");
        simulator.setup().expect("design should elaborate");
        simulator.run().expect("design should settle");
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(8));
    }

    /// A frame is seeded with copies of the design *arrays* the body reads, not
    /// only its signals — an index moves during the body, so there is no one
    /// word to take. iverilog 12.0 prints `total is 6`.
    #[test]
    fn test_a_function_reads_a_design_array() {
        let source = r#"
            module top;
                integer cindex[2:0];
                integer i;
                function integer total;
                    input dummy;
                    integer j;
                    begin
                        total = 0;
                        for (j = 0; j < 3; j = j + 1) total = total + cindex[j];
                    end
                endfunction
                initial begin
                    for (i = 0; i < 3; i = i + 1) cindex[i] = i + 1;
                    $display("total is %0d", total(0));
                end
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "top");
        simulator.advance(1).expect("the design should run");
        assert_eq!(simulator.output().text(), "total is 6\n");
    }

    /// Writing one is refused by name instead. The frame holds a *copy*, and a
    /// call hands its writes back through the signal map — a memory is in the
    /// other one, so the write would be dropped with the frame. iverilog 12.0
    /// carries it out (it prints `3` then `mem0=3`), so this is a gap named
    /// rather than a rule; it was `UnknownSignal("mem")` before the read half
    /// put the array in the frame at all.
    #[test]
    fn test_a_function_writing_a_design_array_is_named() {
        let source = r#"
            module top;
                integer mem[1:0];
                function integer stash;
                    input [31:0] v;
                    begin
                        mem[0] = v;
                        stash = v;
                    end
                endfunction
                initial $display("%0d", stash(3));
            endmodule
        "#;
        let error = setup_error(&[source], "top");
        assert!(
            error
                .to_string()
                .contains("a function writing an array declared outside it"),
            "{}",
            error
        );
    }

    /// Positional overrides bind in the order the child declares its
    /// parameters, which is `BASE` then `STEP`.
    #[test]
    fn test_positional_parameter_override() {
        let top = r#"
            module top(
                input clk,
                input rst,
                output [7:0] out
            );
                stepper #(8'd7, 8'd2) dut (.clk(clk), .rst(rst), .count(out));
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, STEPPER], "top");
        assert_eq!(simulator.get("dut.BASE").unwrap().to_u128(), Some(7));
        assert_eq!(simulator.get("dut.STEP").unwrap().to_u128(), Some(2));

        reset(&mut simulator);
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("out").unwrap().to_u128(), Some(2));
    }

    /// An override expression is evaluated where it was written — in the
    /// parent — so the parent's `BASE` wins over the child's own.
    #[test]
    fn test_override_expressions_are_evaluated_in_the_parents_scope() {
        let top = r#"
            module top(
                input clk,
                input rst,
                output [7:0] out
            );
                localparam BASE = 8'd2;
                stepper #(.STEP(BASE + 1)) dut (.clk(clk), .rst(rst), .count(out));
            endmodule
        "#;

        let simulator = simulator_for(&[top, STEPPER], "top");
        assert_eq!(simulator.get("dut.STEP").unwrap().to_u128(), Some(3));
        // The child's own `BASE` is untouched and did not shadow the parent's.
        assert_eq!(simulator.get("dut.BASE").unwrap().to_u128(), Some(100));
        assert_eq!(simulator.get("BASE").unwrap().to_u128(), Some(2));
    }

    /// Three levels: the leaf's register is only reachable through both
    /// prefixes.
    #[test]
    fn test_nested_hierarchy_two_levels_deep() {
        let middle = r#"
            module middle(
                input clk,
                input rst,
                output [3:0] out
            );
                ticker leaf (.clk(clk), .rst(rst), .out(out));
            endmodule
        "#;
        let top = r#"
            module top(
                input clk,
                input rst,
                output [3:0] out
            );
                middle mid (.clk(clk), .rst(rst), .out(out));
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, middle, TICKER], "top");
        reset(&mut simulator);
        for _ in 0..3 {
            simulator.tick("clk").unwrap();
        }

        assert_eq!(simulator.get("mid.leaf.count").unwrap().to_u128(), Some(3));
        assert_eq!(simulator.get("out").unwrap().to_u128(), Some(3));
        // Every port along the chain aliases the one signal at the top of it.
        assert_eq!(simulator.get("mid.leaf.clk").unwrap().to_binary(), "0");
        assert_eq!(simulator.get("mid.out").unwrap().to_u128(), Some(3));
    }

    #[test]
    fn test_unknown_module_name_is_reported() {
        let top = r#"
            module top(
                input clk
            );
                missing dut (.clk(clk));
            endmodule
        "#;

        assert_eq!(
            setup_error(&[top], "top"),
            SimulationError::UnknownModule("missing".to_string())
        );
    }

    #[test]
    fn test_unknown_top_name_is_reported() {
        assert_eq!(
            setup_error(&[COUNTER], "nonesuch"),
            SimulationError::UnknownModule("nonesuch".to_string())
        );
    }

    #[test]
    fn test_named_argument_for_a_port_that_does_not_exist() {
        let top = r#"
            module top(
                input clk,
                input rst
            );
                counter dut (.clk(clk), .reset(rst));
            endmodule
        "#;

        assert_eq!(
            setup_error(&[top, COUNTER], "top"),
            SimulationError::UnknownPort {
                module: "counter".to_string(),
                port: "reset".to_string(),
            }
        );
    }

    #[test]
    fn test_more_positional_arguments_than_ports() {
        let top = r#"
            module top(
                input clk,
                input rst,
                output [3:0] out
            );
                counter dut (clk, rst, out, clk);
            endmodule
        "#;

        assert_eq!(
            setup_error(&[top, COUNTER], "top"),
            SimulationError::TooManyArguments {
                module: "counter".to_string(),
                what: "ports",
                expected: 3,
                found: 4,
            }
        );
    }

    #[test]
    fn test_override_of_a_parameter_that_does_not_exist() {
        let top = r#"
            module top(
                input clk,
                input rst,
                output [7:0] out
            );
                stepper #(.STRIDE(8'd2)) dut (.clk(clk), .rst(rst), .count(out));
            endmodule
        "#;

        assert_eq!(
            setup_error(&[top, STEPPER], "top"),
            SimulationError::UnknownParameter {
                module: "stepper".to_string(),
                parameter: "STRIDE".to_string(),
            }
        );
    }

    #[test]
    fn test_an_output_bound_to_an_expression_is_not_drivable() {
        let top = r#"
            module top(
                input clk,
                input rst,
                output [3:0] out
            );
                counter dut (.clk(clk), .rst(rst), .count(out + 1));
            endmodule
        "#;

        assert_eq!(
            setup_error(&[top, COUNTER], "top"),
            SimulationError::UndrivablePort {
                instance: "dut".to_string(),
                port: "count".to_string(),
                connection: "out + 1".to_string(),
            }
        );
    }

    /// A name nothing declares that is wired to an instance is an implicit
    /// net, not an error: IEEE 1364-2005 §4.5, and what iverilog 12.0 does —
    /// it warns that the four bit port got one bit and pads the rest, where
    /// refusing would be a design it cannot compile at all.
    #[test]
    fn test_connecting_a_signal_the_parent_never_declared_is_an_implicit_net() {
        let top = r#"
            module top(
                input clk,
                input rst
            );
                counter dut (.clk(clk), .rst(rst), .count(nowhere));
            endmodule
        "#;

        let simulator = simulator_for(&[top, COUNTER], "top");
        // Scalar whatever the port's width is, which is the LRM's rule and
        // iverilog's: `w=0 width=1` for a four bit port.
        assert_eq!(simulator.get("nowhere").unwrap().width(), 1);
    }

    /// `assign w = 1'b1;` with no `wire w;` above it declares `w`, which is
    /// what corpus `pr1693890` is about — its own comment says so, and it
    /// checks `w !== 1'b1` at time 1.
    #[test]
    fn test_a_continuous_assignment_declares_its_undeclared_target() {
        let top = r#"
            module top;
                assign w = 1'b1;
                initial #1 if (w === 1'b1) $display("PASSED");
            endmodule
        "#;

        let mut simulator = simulator_for(&[top], "top");
        simulator.advance(2).expect("the design should run");
        assert_eq!(simulator.output().text(), "PASSED\n");
    }

    /// A gate terminal declares one too, which is how a netlist written
    /// without `wire` declarations between its gates simulates at all
    /// (corpus `pr1587669`, `pr1645518`).
    #[test]
    fn test_a_gate_terminal_declares_an_implicit_net() {
        let top = r#"
            module top;
                reg a;
                not g1 (mid, a);
                not g2 (out, mid);
                initial begin
                    a = 1'b0;
                    #1 $display("mid=%b out=%b", mid, out);
                end
            endmodule
        "#;

        let mut simulator = simulator_for(&[top], "top");
        simulator.advance(2).expect("the design should run");
        assert_eq!(simulator.output().text(), "mid=1 out=0\n");
    }

    /// An implicit net is created for a name used *inside* a connection
    /// expression as well, and an undriven one reads `z` — measured against
    /// iverilog 12.0, which prints `yy=z` for `.a(yy + 1)`.
    #[test]
    fn test_an_undeclared_name_inside_a_connection_is_an_implicit_net() {
        let top = r#"
            module top;
                sub u (.a(yy + 1));
                initial #1 $display("yy=%b", yy);
            endmodule
        "#;
        let sub = r#"
            module sub(a);
                input a;
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, sub], "top");
        simulator.advance(2).expect("the design should run");
        assert_eq!(simulator.output().text(), "yy=z\n");
    }

    /// A name something *indexes* is not implicitly declared: an implicit net
    /// is one bit, so a scalar standing in for a missing `wire [7:0]` would
    /// turn a declaration the design forgot into a silent out-of-range read.
    #[test]
    fn test_a_selected_name_is_not_implicitly_declared() {
        let top = r#"
            module top;
                reg a;
                not g1 (bus[0], a);
            endmodule
        "#;

        let simulator = simulator_for(&[top], "top");
        assert!(
            simulator.get("bus").is_err(),
            "a name under a select should not have been declared"
        );
    }

    #[test]
    fn test_direct_recursion_is_rejected() {
        let spin = r#"
            module spin(
                input clk
            );
                spin inner (.clk(clk));
            endmodule
        "#;

        assert_eq!(
            setup_error(&[spin], "spin"),
            SimulationError::RecursiveInstantiation("spin".to_string())
        );
    }

    #[test]
    fn test_indirect_recursion_is_rejected() {
        let ping = r#"
            module ping(
                input clk
            );
                pong inner (.clk(clk));
            endmodule
        "#;
        let pong = r#"
            module pong(
                input clk
            );
                ping inner (.clk(clk));
            endmodule
        "#;

        assert_eq!(
            setup_error(&[ping, pong], "ping"),
            SimulationError::RecursiveInstantiation("ping".to_string())
        );
    }

    /// A module **may** instantiate itself, as long as a `generate` condition
    /// stops the recursion — IEEE 1364-2005 allows it and it is how a design
    /// writes a tree or a chain of a parameterised depth.
    ///
    /// iverilog 12.0 prints `1` then `0` for this design. Corpus
    /// `pr2728812a` is the branching version, a tree of adders.
    #[test]
    fn test_recursion_a_generate_condition_terminates_is_elaborated() {
        let chain = r#"
            module chain #(parameter n = 4) (input [n-1:0] in, output out);
                generate
                    if (n == 1)
                        assign out = in[0];
                    else begin
                        wire lower;
                        chain #(n-1) c (in[n-2:0], lower);
                        assign out = lower | in[n-1];
                    end
                endgenerate
            endmodule
        "#;
        let top = r#"
            module top();
                wire y;
                reg [3:0] v;
                chain #(4) u (v, y);
                initial begin
                    v = 4'b0100;
                    #1 $display("%b", y);
                    v = 4'b0000;
                    #1 $display("%b", y);
                end
            endmodule
        "#;

        let mut simulator = simulator_for(&[chain, top], "top");
        simulator.advance(3).expect("time should advance");
        assert_eq!(simulator.output().lines(), vec!["1", "0"]);
    }

    /// A recursion that *branches* is still reported, and cheaply: the walk is
    /// depth first, so the first branch reaches the depth bound after
    /// [`MAX_INSTANTIATION_DEPTH`] instances rather than after `2**depth` of
    /// them. [`MAX_INSTANCES`] is what covers the other shape — a recursion
    /// that does terminate, at a depth whose branching makes it enormous.
    #[test]
    fn test_a_branching_recursion_is_rejected() {
        let fork = r#"
            module fork_tree(
                input clk
            );
                fork_tree left (.clk(clk));
                fork_tree right (.clk(clk));
            endmodule
        "#;

        assert_eq!(
            setup_error(&[fork], "fork_tree"),
            SimulationError::RecursiveInstantiation("fork_tree".to_string())
        );
    }

    /// The same module twice in one parent is not recursion — the check is on
    /// the path from the top, not on the set of modules used.
    #[test]
    fn test_repeating_a_module_is_not_recursion() {
        let top = r#"
            module top(
                input clk,
                input rst,
                output [3:0] a,
                output [3:0] b
            );
                counter one (.clk(clk), .rst(rst), .count(a));
                counter two (.clk(clk), .rst(rst), .count(b));
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, COUNTER], "top");
        reset(&mut simulator);
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("a").unwrap().to_u128(), Some(1));
        assert_eq!(simulator.get("b").unwrap().to_u128(), Some(1));
    }

    /// A child that clocks itself with `#delay`. The delay suspends a compiled
    /// program mid-flight, so its resume point has to survive the rewriting
    /// that flattening does to the instruction list.
    #[test]
    fn test_a_child_can_clock_the_design_from_a_delay() {
        let clkgen = r#"
            module clkgen(
                output reg clk
            );
                initial clk = 1'b0;
                always begin
                    #5 clk = ~clk;
                end
            endmodule
        "#;
        let top = r#"
            module top(
                input rst,
                output [3:0] out
            );
                wire clk;
                clkgen gen (.clk(clk));
                counter dut (.clk(clk), .rst(rst), .count(out));
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, clkgen, COUNTER], "top");
        // The child's `initial` ran at time zero, through the parent's wire.
        assert_eq!(simulator.get("clk").unwrap().to_binary(), "0");
        reset(&mut simulator);

        // Two rising edges in twenty time units, and no external stimulus.
        simulator.advance(20).unwrap();
        assert_eq!(simulator.now(), 20);
        assert_eq!(simulator.get("out").unwrap().to_u128(), Some(2));
    }

    /// A `case` compiles to a held subject plus one comparison per label, all
    /// of which name signals that flattening has to rewrite.
    #[test]
    fn test_a_case_statement_inside_an_instance() {
        let decoder = r#"
            module decoder(
                input [1:0] sel,
                output reg [3:0] y
            );
                always @(*) begin
                    case (sel)
                        2'b00: y = 4'b0001;
                        2'b01: y = 4'b0010;
                        default: y = 4'b1000;
                    endcase
                end
            endmodule
        "#;
        let top = r#"
            module top(
                input [1:0] pick,
                output [3:0] lines
            );
                decoder dut (.sel(pick), .y(lines));
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, decoder], "top");
        simulator.poke("pick", Register::from_binary("00")).unwrap();
        assert_eq!(simulator.get("lines").unwrap().to_binary(), "0001");

        simulator.poke("pick", Register::from_binary("01")).unwrap();
        assert_eq!(simulator.get("lines").unwrap().to_binary(), "0010");

        simulator.poke("pick", Register::from_binary("11")).unwrap();
        assert_eq!(simulator.get("lines").unwrap().to_binary(), "1000");
    }

    /// Only the top module's ports are drivable; a child's input is reached
    /// through whatever the parent connected to it.
    #[test]
    fn test_only_the_top_modules_inputs_are_drivable() {
        let top = r#"
            module top(
                input clk,
                input rst,
                output [3:0] out
            );
                counter dut (.clk(clk), .rst(rst), .count(out));
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, COUNTER], "top");
        assert_eq!(
            simulator.set_input("dut.clk", one()),
            Err(SimulationError::NotAnInput("dut.clk".to_string()))
        );
    }

    #[test]
    fn test_a_system_task_in_a_child_reads_the_flattened_signal() {
        // The call is compiled before the child is flattened, so this only
        // prints the right number because `TaskCall::rename` re-points it at
        // `dut.count`.
        let chatty = r#"
            module chatty(
                input clk
            );
                reg [3:0] count;
                initial begin
                    count = 4'd7;
                    $display("child count %0d", count);
                end
            endmodule
        "#;
        let top = r#"
            module top(
                input clk
            );
                chatty dut (.clk(clk));
            endmodule
        "#;

        let simulator = simulator_for(&[top, chatty], "top");
        assert_eq!(simulator.output().lines(), vec!["child count 7"]);
        assert_eq!(simulator.get("dut.count").unwrap().to_u128(), Some(7));
    }

    /// `wire a = expr;` is a continuous assignment, so the net keeps following
    /// its operands long after time zero.
    #[test]
    fn test_net_initialiser_drives_continuously() {
        let source = r#"
            module nets(
                input [3:0] a
            );
                wire [3:0] doubled = a + a;
                wire x = 1, y = 0;
            endmodule
        "#;

        let mut simulator = simulator_for(&[source], "nets");

        // The initialisers joined the continuous assignments, so settling is
        // what applies them — and each name in a list carries its own driver.
        simulator.run().unwrap();
        assert_eq!(simulator.get("x").unwrap().to_u128(), Some(1));
        assert_eq!(simulator.get("y").unwrap().to_u128(), Some(0));

        simulator.poke("a", Register::from_u128(3, 4)).unwrap();
        assert_eq!(simulator.get("doubled").unwrap().to_u128(), Some(6));

        // The operand moves after time zero and the net moves with it, which a
        // one-shot starting value would not do.
        simulator.poke("a", Register::from_u128(5, 4)).unwrap();
        assert_eq!(simulator.get("doubled").unwrap().to_u128(), Some(10));
    }

    /// `wire #(period/3) trace = drive;` is `assign #(period/3)`: the delay is
    /// the declaration assignment's, and it is an expression evaluated when a
    /// transaction is scheduled, so a `period` that moves changes the next
    /// one (corpus `delay5`, which iverilog 12.0 passes).
    #[test]
    fn test_net_declaration_delay_is_scheduled() {
        let source = r#"
            module main;
                time period;
                reg drive;
                wire #(period/3) trace = drive;
                initial begin
                    period = 24;
                    #1 drive = 1;
                    #7 $display("%b", trace);
                    #2 $display("%b", trace);
                    period = 18;
                    drive = 0;
                    #5 $display("%b", trace);
                    #2 $display("%b", trace);
                end
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "main");
        simulator.advance(20).unwrap();
        // Times 8, 10, 15 and 17 — measured against iverilog 12.0.
        assert_eq!(simulator.output().text(), "x\n1\n1\n0\n");
    }

    /// `wire (weak0, weak1) value = pullval;` drives at `weak`, so a gate's
    /// `strong` output overrides it and it holds the net once the gate floats
    /// (corpus `drive_strength1`).
    #[test]
    fn test_net_declaration_strength_resolves() {
        let source = r#"
            module main;
                reg pullval, en;
                wire (weak0, weak1) value = pullval;
                bufif1 (value, 1'b0, en);
                initial begin
                    en = 0; pullval = 1;
                    #1 $display("%b", value);
                    en = 1;
                    #1 $display("%b", value);
                end
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "main");
        simulator.advance(5).unwrap();
        assert_eq!(simulator.output().text(), "1\n0\n");
    }

    /// A delay on a net with no declaration assignment is a property of every
    /// driver of the net, which is not modelled — so it is refused by name
    /// rather than dropped.
    #[test]
    fn test_a_bare_net_delay_is_refused() {
        let source = r#"
            module main;
                reg a;
                wire #5 w;
                assign w = a;
            endmodule
        "#;
        let error = setup_error(&[source], "main");
        assert!(
            matches!(error, SimulationError::Unsupported(_)),
            "{error:?}"
        );
    }

    /// `reg a = expr;` is a starting value, not a driver: a procedural write
    /// owns the register from then on and the initialiser does not fight it.
    #[test]
    fn test_register_initialiser_is_applied_once() {
        let source = r#"
            module regs(
                input clk
            );
                reg [3:0] n = 4'd5;
                always @(posedge clk) n <= n + 1;
            endmodule
        "#;

        let mut simulator = simulator_for(&[source], "regs");
        assert_eq!(simulator.get("n").unwrap().to_u128(), Some(5));

        // Were this a continuous assignment, settling would put 5 back after
        // every clock and the register would never count.
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("n").unwrap().to_u128(), Some(6));
        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("n").unwrap().to_u128(), Some(7));
    }

    /// An `integer` initialiser follows the `reg` rule.
    #[test]
    fn test_integer_initialiser_is_applied_once() {
        let source = r#"
            module counts(
                input clk
            );
                integer i = 0;
                always @(posedge clk) i <= i + 1;
            endmodule
        "#;

        let mut simulator = simulator_for(&[source], "counts");
        assert_eq!(simulator.get("i").unwrap().to_u128(), Some(0));

        simulator.tick("clk").unwrap();
        assert_eq!(simulator.get("i").unwrap().to_u128(), Some(1));
    }

    /// A child's initialisers are resolved into the flat store like everything
    /// else: the net's driver names the qualified signal, and the parent's
    /// value reaches it through the aliased port.
    #[test]
    fn test_child_initialisers_are_namespaced() {
        let child = r#"
            module scaler(
                input [3:0] a,
                output [3:0] out
            );
                wire [3:0] doubled = a + a;
                reg [3:0] seed = 4'd9;
                assign out = doubled;
            endmodule
        "#;
        let top = r#"
            module top(
                input [3:0] a,
                output [3:0] out
            );
                scaler dut (.a(a), .out(out));
            endmodule
        "#;

        let mut simulator = simulator_for(&[top, child], "top");
        assert_eq!(simulator.get("dut.seed").unwrap().to_u128(), Some(9));

        simulator.poke("a", Register::from_u128(6, 4)).unwrap();
        assert_eq!(simulator.get("dut.doubled").unwrap().to_u128(), Some(12));
        assert_eq!(simulator.get("out").unwrap().to_u128(), Some(12));
    }
    /// A memory declares one word per address, not one signal for the whole
    /// array — which is what makes every word read `x` rather than the whole
    /// memory being a single `x` register.
    #[test]
    fn test_a_memory_elaborates_to_one_word_per_address() {
        let modules = parse_all(&[r#"
            module has_memory();
                reg [7:0] mem [0:255];
            endmodule
        "#]);
        let elaborated = elaborate(&modules, 0).expect("design should elaborate");

        let memory = elaborated
            .state
            .memory("mem")
            .expect("mem should elaborate as a memory");
        assert_eq!(memory.depth(), 256);
        assert_eq!(memory.width(), 8);
        // Not a signal: the two maps are what tell a word select from a bit one.
        assert!(elaborated.state.get_signal("mem").is_none());
    }

    /// An `integer` array is a memory of 32 bit signed words. `meminit2` in the
    /// ivtest corpus is exactly this declaration.
    #[test]
    fn test_an_integer_array_elaborates_to_a_signed_memory() {
        let modules = parse_all(&[r#"
            module integer_array();
                integer mem [0:1];
            endmodule
        "#]);
        let elaborated = elaborate(&modules, 0).expect("design should elaborate");

        let memory = elaborated
            .state
            .memory("mem")
            .expect("mem should be a memory");
        assert_eq!(memory.depth(), 2);
        assert_eq!(memory.width(), 32);
        assert!(memory.is_signed());
    }

    /// A memory inside an instance is qualified like any other name, so two
    /// instances have a memory each.
    #[test]
    fn test_each_instance_gets_its_own_memory() {
        let modules = parse_all(&[
            r#"
            module leaf();
                reg [3:0] mem [0:7];
            endmodule
        "#,
            r#"
            module top();
                leaf a ();
                leaf b ();
            endmodule
        "#,
        ]);
        let elaborated = elaborate(&modules, 1).expect("design should elaborate");

        assert_eq!(elaborated.state.memory("a.mem").unwrap().depth(), 8);
        assert_eq!(elaborated.state.memory("b.mem").unwrap().depth(), 8);
        assert!(elaborated.state.memory("mem").is_none());
    }

    /// A `specparam` is a constant the whole module may name, so it is
    /// declared like a parameter — the one thing inside a `specify` block that
    /// is not inert. The paths beside it change only *when* a value arrives,
    /// which this simulator has no model for, so they record and do nothing.
    /// A `specparam` is not one of those: it is a constant the module may name,
    /// real-valued or not.
    #[test]
    fn test_a_specparam_is_declared_as_a_constant() {
        let modules = parse_all(&[r#"
            module gate(input a, output [7:0] z);
                specify
                    specparam tRise = 6, tFall = 7;
                    specparam holdoff = 0.9;
                    (a => z) = (tRise, tFall);
                endspecify
                assign z = tRise + tFall;
            endmodule
        "#]);
        let elaborated = elaborate(&modules, 0).expect("design should elaborate");

        assert_eq!(elaborated.state.get("tRise").unwrap().to_u128(), Some(6));
        assert_eq!(elaborated.state.get("tFall").unwrap().to_u128(), Some(7));
        // A real value is a real constant: the module may name it and read
        // the number it was written with.
        assert_eq!(elaborated.state.get("holdoff").unwrap().to_f64(), 0.9);
    }

    /// A dimension nothing could allocate is a named error rather than an
    /// attempt to reserve it.
    #[test]
    fn test_an_absurd_memory_dimension_is_reported() {
        let modules = parse_all(&[r#"
            module huge();
                reg [7:0] mem [0:99999999];
            endmodule
        "#]);

        assert!(matches!(
            elaborate(&modules, 0),
            Err(SimulationError::Unsupported(_))
        ));
    }

    /// A memory declared inside a task lands in the memory map, so `mem[3]`
    /// writes a whole word rather than a bit of a scalar, and its width may be
    /// made of the task's own constants. iverilog 12.0 prints `8 f0 f3 8`
    /// (corpus `task_mem`, `pr2132552`).
    #[test]
    fn test_a_task_declares_a_memory_sized_by_its_own_parameters() {
        let source = r#"
            module top;
                task load;
                    parameter depth = 4;
                    localparam width = depth * 2;
                    reg [width-1:0] mem [0:depth-1];
                    integer i;
                    begin
                        for (i = 0; i < depth; i = i + 1) mem[i] = i + 8'hf0;
                        $display("%0d %h %h %0d", $bits(mem[0]), mem[0], mem[3], width);
                    end
                endtask
                initial load;
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "top");
        assert!(
            simulator.get("load.mem").is_err(),
            "a memory is not a signal"
        );
        simulator.advance(1).expect("the design should run");
        assert_eq!(simulator.output().text(), "8 f0 f3 8\n");
    }

    /// A memory local to a function is a memory in the call's frame: iverilog
    /// 12.0 prints `a5` for this swap (corpus `constfunc15`, `br_gh674`).
    #[test]
    fn test_a_function_declares_a_memory_in_its_frame() {
        let source = r#"
            module top;
                function [7:0] swap(input [7:0] v);
                    reg [3:0] tmp [1:2];
                    begin
                        tmp[1] = v[3:0];
                        tmp[2] = v[7:4];
                        swap = {tmp[1], tmp[2]};
                    end
                endfunction
                initial $display("%h", swap(8'h5a));
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "top");
        simulator.advance(1).expect("the design should run");
        assert_eq!(simulator.output().text(), "a5\n");
    }

    /// A named block declares constants, memories and events of its own, under
    /// its own dotted name. iverilog 12.0 prints `4 ab` (corpus `pr2533175`).
    #[test]
    fn test_a_named_block_declares_a_parameter_a_memory_and_an_event() {
        let source = r#"
            module top;
                parameter p = 9;
                initial begin : blk
                    parameter p = 3;
                    localparam l = p + 1;
                    reg [7:0] words [0:1];
                    event go;
                    words[1] = 8'hab;
                    $display("%0d %h", l, words[1]);
                end
            endmodule
        "#;
        let mut simulator = simulator_for(&[source], "top");
        assert_eq!(simulator.get("blk.l").unwrap().to_u128(), Some(4));
        simulator.advance(1).expect("the design should run");
        assert_eq!(simulator.output().text(), "4 ab\n");
    }

    /// A `defparam` reaches a constant a named block or a task declares, and
    /// an event a task declares is triggered from outside by its dotted name.
    /// iverilog 12.0 prints `block 5` then `task 6` (corpus `scoped_events`).
    #[test]
    fn test_a_defparam_and_a_trigger_reach_into_a_block_and_a_task() {
        let sub = r#"
            module sub;
                initial begin : my_block
                    parameter p = 0;
                    localparam l = p + 1;
                    event trigger;
                    @trigger $display("block %0d", l);
                end
                task my_task;
                    parameter p = 0;
                    localparam l = p + 1;
                    event trigger;
                    @trigger $display("task %0d", l);
                endtask
                initial my_task;
            endmodule
        "#;
        let top = r#"
            module top;
                sub s();
                defparam s.my_block.p = 4;
                defparam s.my_task.p = 5;
                initial begin
                    #1 -> s.my_block.trigger;
                    #1 -> s.my_task.trigger;
                end
            endmodule
        "#;
        let mut simulator = simulator_for(&[sub, top], "top");
        simulator.advance(5).expect("the design should run");
        assert_eq!(simulator.output().text(), "block 5\ntask 6\n");
    }
}

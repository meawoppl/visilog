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
//! parent's expression; an output is not drivable that way at all and is
//! reported as [`SimulationError::UndrivablePort`].
//!
//! An unconnected input is floating, so it is declared `z`. An unconnected
//! output is a real signal the child drives that simply nobody reads, so it
//! starts `x` like any other.
//!
//! Aliasing means an aliased port takes the *parent* signal's declared width
//! and range rather than its own: there is only one entry, and it is the
//! parent's. That is the price of never having to reconcile the two, and it
//! only differs from Verilog when a connection is deliberately mismatched.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use crate::parsers::{
    assignment::ContinuousAssignment,
    behavior::{
        Event, EventControl, FunctionDeclaration, FunctionVariable, ProceduralStatements,
        TaskDeclaration,
    },
    constants::VerilogConstant,
    expr::Expression,
    gates::{GateInstantiation, GateKind, StrengthLevel},
    generate::{DefparamAssignment, GenerateBlock, GenerateItem, GenerateLoop},
    identifier::Identifier,
    modules::{
        ModuleInitArguments, ModuleInstantiation, NetType, Port, PortDirection, VerilogModule,
    },
    nets::NetType as WireKind,
    operators::BinaryOperator,
    primitive::UdpTable,
    simple::Range,
    specify::{SpecParam, SpecParamValue},
    statements::ModuleStatement,
};
use crate::register::{Register, ONE, ZERO};
use crate::simulator::eval::{eval, expression_width};
use crate::simulator::events::{control_fires, signals_read, SignalEdge};
use crate::simulator::exec::{drive, range_width};
use crate::simulator::gates::Gate;
use crate::simulator::program::{
    block_scope, FrameVariable, FunctionDefinition, Instruction, Program, TaskDefinition,
    TaskParameter, TaskTable, FUNCTION_DELAY_UNSUPPORTED, FUNCTION_EVENT_UNSUPPORTED,
};
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::StateStore;
use crate::simulator::udp::Udp;

/// A function body prints into a [`TaskContext`](crate::simulator::tasks::TaskContext)
/// nobody reads — a call happens inside an expression, and an expression has no
/// way to hand output back — so a body that prints is rejected rather than
/// silently swallowed.
const FUNCTION_TASK_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a system task inside a function");

/// A non-blocking assignment defers its write past the end of the call, and a
/// call ends the moment the expression around it needs the value.
const FUNCTION_NONBLOCKING_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a non-blocking assignment inside a function");

/// A call runs against a frame of its own, so a write to anything outside the
/// function would be thrown away with the frame. Losing it quietly would make a
/// design that depends on it look like one that works.
const FUNCTION_SIDE_EFFECT_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a function assigning a signal outside itself");

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

/// How wide a `time` variable is. A `time` counts simulated time and is
/// defined to be 64 bits, unsigned — the one fixed width beside an `integer`'s
/// 32.
const TIME_RANGE: (i64, i64) = (63, 0);

/// A `real` is IEEE-754 floating point, which is not a four-state bit vector:
/// there is no `x` in a float, none of the arithmetic is the same, and the
/// expression grammar has no floating point literal to feed one with. The
/// front end reads the declaration so that a design using one stops here, by
/// name, rather than on a parse error somewhere in the middle of it.
const REAL_UNSUPPORTED: SimulationError = SimulationError::Unsupported("a `real` variable");

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

/// `walk` compiles a module's functions and tasks before it unrolls a
/// generate region, so one written *inside* a block would be missing from the
/// store a call looks in — a call that quietly found nothing is the hardest
/// kind of wrong answer to find.
const GENERATE_SUBPROGRAM_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a function or task declared inside a generate block");

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
    pub program: Program,
}

impl TimedBlock {
    /// Whether the edges observed this delta cycle wake this block.
    pub fn fires(&self, edges: &[SignalEdge]) -> bool {
        control_fires(&self.control, edges, &self.implicit_reads)
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
            resolved_nets: HashSet::new(),
            pulled_nets: Vec::new(),
            blocks: Vec::new(),
            inputs: Vec::new(),
            aliases: HashMap::new(),
        },
        stack: Vec::new(),
        defparams: BTreeMap::new(),
        blocks_generated: 0,
    };
    elaborator.walk(top, &Scope::root(&modules[top].identifier.name))?;
    // A `defparam` is consumed by the instantiation it names. One that is
    // still here named nothing, and an override that quietly did not happen
    // leaves the design running at a width it was told not to use.
    if let Some(path) = elaborator.defparams.keys().next() {
        return Err(SimulationError::UnappliedDefparam(path.clone()));
    }
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
        }
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
    /// now. A module that reaches itself through this is recursive, which no
    /// amount of flattening can terminate.
    stack: Vec<usize>,
    /// The `defparam` overrides seen so far, by the flat name of the parameter
    /// each one addresses. An instantiation takes the ones that name it; what
    /// is left over at the end named nothing and is reported.
    defparams: BTreeMap<String, Register>,
    /// How many unnamed generate blocks have been given a `genblk` number.
    /// A block with no label still needs a scope — two iterations of an
    /// unnamed loop body would otherwise declare the same names twice.
    blocks_generated: usize,
}

impl<'m> Elaborator<'m> {
    fn walk(&mut self, index: usize, scope: &Scope) -> Result<(), SimulationError> {
        let modules = self.modules;
        let module = &modules[index];

        if self.stack.contains(&index) {
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
                self.declare_port(port, scope)?;
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
        self.declare_functions(module, scope, &tasks)?;
        for statement in deferred {
            self.declare(statement, scope)?;
        }
        for port in &module.ports {
            self.declare_port(port, scope)?;
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

        for statement in &module.statements {
            if !matches!(statement, ModuleStatement::ParameterDeclaration(_)) {
                self.declare(statement, scope)?;
            }
        }
        for (statement, inner) in &generated {
            self.declare(statement, inner)?;
        }
        for statement in &module.statements {
            self.build(statement, scope, &tasks)?;
        }
        for (statement, inner) in &generated {
            self.build(statement, inner, &tasks)?;
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
                    ModuleStatement::FunctionDeclaration(_)
                    | ModuleStatement::TaskDeclaration(_) => {
                        return Err(GENERATE_SUBPROGRAM_UNSUPPORTED)
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
                self.resolve_bound(msb, scope)?,
                self.resolve_bound(lsb, scope)?,
            )),
        }
    }

    /// One bound of an expression range, as a number.
    ///
    /// Anything that is not a number — an unknown name, an `x`, a value too
    /// wide to be a bound — is a **named** error. A width the simulator picked
    /// for itself would be wrong for the whole run and look like nothing at
    /// all had gone wrong.
    fn resolve_bound(&self, bound: &Expression, scope: &Scope) -> Result<i64, SimulationError> {
        let unresolved = |why: String| SimulationError::UnresolvedRange {
            bound: bound.to_contracted_string(),
            why,
        };
        let value = eval(&renamed(bound, scope), &self.out.state)
            .map_err(|why| unresolved(why.to_string()))?;
        let wide = if value.is_signed() {
            value.to_i128()
        } else {
            value.to_u128().and_then(|value| i128::try_from(value).ok())
        };
        wide.and_then(|value| i64::try_from(value).ok())
            .ok_or_else(|| unresolved("it does not evaluate to an integer".to_string()))
    }

    /// Declares one port, unless it was aliased onto a signal that already
    /// exists.
    fn declare_port(&mut self, port: &Port, scope: &Scope) -> Result<(), SimulationError> {
        let local = &port.identifier.name;
        match scope.bindings.get(local) {
            // The parent's signal *is* this port. Declaring it again would give
            // the port a second, immediately stale copy.
            Some(Binding::Alias(_)) => return Ok(()),
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
            // Nothing at all is driving this input, which is what `z` means.
            let floating = Register::high_impedance(range_width(range));
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
                    Ok(definition) => {
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
            // A named block inside a task body is scoped under the task, the
            // same way the body's instructions spell it.
            let inside = block_scope("", &task.name.name);
            self.declare_block_locals(&task.statements, scope, &inside)?;
            for variable in task
                .arguments
                .iter()
                .map(|argument| &argument.variable)
                .chain(&task.locals)
            {
                // A task argument may be sized from a parameter — `input
                // [WIDTH-1:0] a;` — exactly as any other declaration is, and
                // this is the one place a task's widths are ever recorded.
                let range = self.resolve_range(&variable.range, scope)?;
                self.out.state.declare_signed(
                    scope.qualified(&task_variable(&task.name.name, &variable.name.name)),
                    range,
                    variable.signed,
                );
            }
        }

        Ok(tasks)
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
                    for local in &inner.locals {
                        let range = self.resolve_range(&local.range, scope)?;
                        let name = scope.qualified(&format!("{}{}", nested, local.name.name));
                        self.out.state.declare_signed(name, range, local.signed);
                    }
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
        module: &VerilogModule,
        scope: &Scope,
        tasks: &TaskTable,
    ) -> Result<(), SimulationError> {
        let mut staged: BTreeMap<String, FunctionDefinition> = BTreeMap::new();
        for statement in &module.statements {
            if let ModuleStatement::FunctionDeclaration(function) = statement {
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

        let variable = |variable: &FunctionVariable| {
            Ok(FrameVariable {
                name: format!("{}.{}", qualified, variable.name.name),
                range: self.resolve_range(&variable.range, scope)?,
                signed: variable.signed,
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

        let mut program = Program::compile(&function.statements, tasks)?;
        program.rename(&|name| match frame_names.get(name) {
            Some(qualified) => qualified.clone(),
            None => scope.resolve(name),
        });

        let own: BTreeSet<String> = frame_names.values().cloned().collect();
        let names = analyse_function_body(&program, &own)?;

        Ok(FunctionDefinition {
            result: FrameVariable {
                name: qualified,
                range: self.resolve_range(&function.range, scope)?,
                signed: function.signed,
            },
            arguments,
            locals,
            reads: names.reads,
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
                    match net.dimensions() {
                        Some(addresses) => {
                            let addresses = self.resolve_range(addresses, scope)?;
                            self.declare_net_memory(
                                local,
                                addresses,
                                range,
                                net.is_signed(),
                                scope,
                            )?
                        }
                        None => {
                            self.declare_local_net(local, range, net.is_signed(), scope);
                            self.record_pull(local, net.kind(), scope);
                        }
                    }
                }
            }
            ModuleStatement::RegisterDeclaration(registers) => {
                for register in registers {
                    let range = match &register.range {
                        Some(range) => self.resolve_range(range, scope)?,
                        None => (0, 0),
                    };
                    // The address dimension is what makes the name a memory
                    // rather than a vector, and it is the only place that
                    // distinction is ever recorded.
                    match &register.dimensions {
                        Some(addresses) => {
                            let addresses = self.resolve_range(addresses, scope)?;
                            self.declare_memory(
                                &register.name.name,
                                addresses,
                                range,
                                register.signed,
                                scope,
                            )?
                        }
                        None => {
                            self.declare_local(&register.name.name, range, register.signed, scope)
                        }
                    }
                }
            }
            ModuleStatement::IntegerDeclaration(integers) => {
                for declaration in integers {
                    // An `integer` is a 32 bit *signed* variable. Signedness is
                    // part of what the keyword means, so there is no qualifier
                    // to read here — it is always true.
                    match &declaration.dimensions {
                        Some(addresses) => {
                            let addresses = self.resolve_range(addresses, scope)?;
                            self.declare_memory(
                                &declaration.name.name,
                                addresses,
                                (31, 0),
                                true,
                                scope,
                            )?
                        }
                        None => self.declare_local(&declaration.name.name, (31, 0), true, scope),
                    }
                }
            }
            ModuleStatement::TimeDeclaration(times) => {
                for declaration in times {
                    // A `time` is 64 bits wide and unsigned; like an `integer`
                    // the keyword is the whole of its type, so there is no
                    // range or qualifier to read.
                    match &declaration.dimensions {
                        Some(addresses) => {
                            let addresses = self.resolve_range(addresses, scope)?;
                            self.declare_memory(
                                &declaration.name.name,
                                addresses,
                                TIME_RANGE,
                                false,
                                scope,
                            )?
                        }
                        None => {
                            self.declare_local(&declaration.name.name, TIME_RANGE, false, scope)
                        }
                    }
                }
            }
            ModuleStatement::RealDeclaration(_) => return Err(REAL_UNSUPPORTED),
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
                    // A `signed` qualifier (or an `integer` type) overrides
                    // whatever signedness the value arrived with; an
                    // unqualified parameter keeps its value's own.
                    //
                    // The flag is applied on both sides of `coerced`
                    // deliberately: it is read *before*, to widen by sign
                    // extension rather than zero extension, and rebuilding a
                    // register does not carry it, so it has to be restated
                    // *after* or the stored parameter reads unsigned.
                    let signed = parameter.signed || value.is_signed();
                    let value = value.with_signedness(signed);
                    let range = match &parameter.range {
                        Some(range) => Some(self.resolve_range(range, scope)?),
                        None => None,
                    };
                    let value = match range {
                        Some(range) => value.coerced(range_width(range)),
                        None => value,
                    }
                    .with_signedness(signed);
                    match range {
                        Some(range) => self.out.state.set_ranged(name, value, range),
                        None => self.out.state.set(name, value),
                    }
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

    /// Declares one `specparam` as the constant it is.
    ///
    /// A real-valued one declares *nothing*: there is no four-state value to
    /// declare it with, and the only thing that could read one is the path
    /// delay that is not simulated anyway. A design that names it in an
    /// ordinary expression reports the name as unknown, which is the same
    /// answer it gets for a `real` variable it tries to use.
    fn declare_specparam(
        &mut self,
        parameter: &SpecParam,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        let SpecParamValue::Expression(expression) = &parameter.value else {
            return Ok(());
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
    /// A redeclaration of an aliased port (`output q;` followed by `reg q;`) is
    /// skipped: the parent's signal is the one the port names, and resetting it
    /// to `x` at the child's width would clobber it.
    fn declare_local(&mut self, local: &str, range: (i64, i64), signed: bool, scope: &Scope) {
        if matches!(scope.bindings.get(local), Some(Binding::Alias(_))) {
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

    /// Declares a memory local to this instance: `reg [7:0] mem [0:255];`.
    ///
    /// A memory cannot be a port, so there is no aliasing to reconcile the way
    /// [`declare_local`](Elaborator::declare_local) has to.
    fn declare_memory(
        &mut self,
        local: &str,
        addresses: (i64, i64),
        range: (i64, i64),
        signed: bool,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        if range_width(addresses) > MAX_MEMORY_DEPTH {
            return Err(MEMORY_TOO_LARGE);
        }
        self.out
            .state
            .declare_memory(scope.qualified(local), addresses, range, signed);
        Ok(())
    }

    /// [`declare_memory`](Elaborator::declare_memory) for an array of nets,
    /// whose undriven words read `z` rather than `x`.
    fn declare_net_memory(
        &mut self,
        local: &str,
        addresses: (i64, i64),
        range: (i64, i64),
        signed: bool,
        scope: &Scope,
    ) -> Result<(), SimulationError> {
        if range_width(addresses) > MAX_MEMORY_DEPTH {
            return Err(MEMORY_TOO_LARGE);
        }
        self.out
            .state
            .declare_net_memory(scope.qualified(local), addresses, range, signed);
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
        let Some(range) = &gate.instance.range else {
            let gate = Gate::new(gate.kind, strength, terminals)?;
            self.push_gate(gate);
            return Ok(());
        };
        // An array's bounds are a `Range` like any declaration's, so
        // `buf drv [N-1:0] (…)` is sized by the parameters in scope.
        let count = range_width(self.resolve_range(range, scope)?);
        for position in 0..count {
            let sliced = terminals
                .iter()
                .map(|terminal| self.array_terminal(gate.kind, terminal, position, count))
                .collect::<Result<Vec<Expression>, SimulationError>>()?;
            let gate = Gate::new(gate.kind, strength, sliced)?;
            self.push_gate(gate);
        }
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
        // A sequential UDP asks about the previous value of an input and keeps
        // a register of its own. A continuous driver is handed neither, so it
        // stops here by name rather than driving something plausible.
        if table.sequential {
            return Err(SimulationError::SequentialPrimitive(
                module.identifier.name.clone(),
            ));
        }
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
        });
        Ok(())
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
        // Only a plain signal can be sliced: a bit of `{16'b0, data}` is not
        // something a `BitSelect` can name, and an output has to be drivable.
        let Expression::Identifier(id) = terminal else {
            return Err(too_wide());
        };
        let Some(signal) = self.out.state.get_signal(&id.name) else {
            return Err(SimulationError::UnknownSignal(id.name.clone()));
        };
        let (msb, lsb) = signal.range();
        // `position` counts from the least significant end, and both the
        // terminal and the instance array are walked that way — so which end
        // the array's own range starts at cannot matter.
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
            // operands for the whole simulation.
            ModuleStatement::WireDeclaration(nets) => {
                for net in nets {
                    if let Some(init) = net.init() {
                        let target = Expression::Identifier(Identifier::new(
                            scope.resolve(&net.identifier().name),
                        ));
                        self.out
                            .assignments
                            .push(ContinuousAssignment::new(target, renamed(init, scope)));
                    }
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
                self.out.blocks.push(TimedBlock {
                    kind: BlockKind::Always,
                    free_running: block.event_control == EventControl::None,
                    control,
                    implicit_reads,
                    program,
                });
            }
            ModuleStatement::InitialBlock(block) => {
                self.declare_block_locals(&block.statements, scope, "")?;
                let mut program = Program::compile(&block.statements, tasks)?;
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
                    program,
                });
            }
            ModuleStatement::ModuleInstantiation(instantiation) => {
                self.instantiate(instantiation, scope)?
            }
            _ => {}
        }
        Ok(())
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
        let mut inner = Scope {
            module_prefix: prefix.clone(),
            prefix,
            bindings: HashMap::new(),
            overrides: HashMap::new(),
            locals: HashMap::new(),
            genvars: HashMap::new(),
            root_name: scope.root_name.clone(),
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
            let binding = match plain_identifier(&connection) {
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
                // An output the parent bound to a *select* — `.y(bus[i])`,
                // which is how a generate loop wires an instance per bit — is
                // the alias run backwards: the port keeps a signal of its own
                // and a continuous assignment carries it out to the bit. An
                // output bound to anything that cannot be written at all, like
                // a concatenation or `a + 1`, is still a named error, because
                // there is nowhere for the child's value to go — and so is an
                // `inout`, which is read as well as written and would need the
                // assignment to run both ways.
                None if port.direction == PortDirection::Output
                    && assigned_name(&connection).is_some() =>
                {
                    Binding::Driving(renamed(&connection, scope))
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

        self.walk(index, &inner)
    }

    /// Evaluates a `#(...)` block in the *parent's* scope, keyed by the child's
    /// parameter names.
    fn overrides(
        &self,
        child: &VerilogModule,
        instantiation: &ModuleInstantiation,
        scope: &Scope,
    ) -> Result<HashMap<String, Register>, SimulationError> {
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
    for instruction in program.instructions() {
        match instruction {
            Instruction::Blocking { target, .. } => match assigned_name(target) {
                Some(name) if own.contains(name) => {}
                Some(_) => return Err(FUNCTION_SIDE_EFFECT_UNSUPPORTED),
                None => {
                    return Err(SimulationError::UnsupportedTarget(
                        target.to_contracted_string(),
                    ))
                }
            },
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
            Instruction::Task(_) => return Err(FUNCTION_TASK_UNSUPPORTED),
            Instruction::Delay(_) => return Err(FUNCTION_DELAY_UNSUPPORTED),
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
    Ok(names)
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

/// The signal an assignment target writes, or `None` when the target is not
/// something that names one.
fn assigned_name(target: &Expression) -> Option<&str> {
    match target {
        Expression::Identifier(id)
        | Expression::BitSelect(id, _)
        | Expression::PartSelect(id, _, _)
        | Expression::IndexedPartSelect { id, .. } => Some(&id.name),
        Expression::Parenthetical(inner) => assigned_name(inner),
        _ => None,
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
            // A `disable` names a scope rather than a signal, so there is
            // nothing in one for a frame to copy in.
            Instruction::Jump(_)
            | Instruction::RepeatNext { .. }
            | Instruction::Task(_)
            | Instruction::Delay(_)
            | Instruction::Disable(_)
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
            other => self.expression(other),
        }
    }

    fn expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Constant(_) | Expression::StringLiteral(_) => {}
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
    let names: HashMap<&str, String> = task
        .arguments
        .iter()
        .map(|argument| &argument.variable)
        .chain(&task.locals)
        .map(|variable| {
            (
                variable.name.name.as_str(),
                task_variable(&task.name.name, &variable.name.name),
            )
        })
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

/// Adds to every function's read set the reads of the functions it calls, until
/// nothing more is added.
///
/// A call seeds its frame from the store it was called against, so a function
/// that calls another has to copy in what *that* one reads as well — otherwise
/// the inner call would find the design signals it wanted missing. A cycle in
/// the call graph is what the fixpoint is for: a recursive function's reads are
/// its own.
fn close_reads(functions: &mut BTreeMap<String, FunctionDefinition>) {
    loop {
        let mut grew = false;
        let names: Vec<String> = functions.keys().cloned().collect();
        for name in names {
            let mut inherited = BTreeSet::new();
            for called in &functions[&name].calls {
                if let Some(definition) = functions.get(called) {
                    inherited.extend(definition.reads.iter().cloned());
                }
            }
            let definition = functions.get_mut(&name).expect("a staged function");
            for read in inherited {
                grew |= definition.reads.insert(read);
            }
        }
        if !grew {
            return;
        }
    }
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
        Expression::Constant(_) | Expression::StringLiteral(_) => {}
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
/// the instances it creates, and the labels of the blocks nested in it — a
/// hierarchical reference reaches through all three.
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
        _ => {}
    }
}

/// Rewrites every name an expression uses through `resolve`.
///
/// That includes the name of a function it calls, which is qualified exactly as
/// a signal is: a function belongs to the instance that declares it.
pub fn rename_expression(expression: &mut Expression, resolve: &dyn Fn(&str) -> String) {
    match expression {
        Expression::Constant(_) | Expression::StringLiteral(_) => {}
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

    #[test]
    fn test_connecting_a_signal_the_parent_never_declared() {
        let top = r#"
            module top(
                input clk,
                input rst
            );
                counter dut (.clk(clk), .rst(rst), .count(nowhere));
            endmodule
        "#;

        assert_eq!(
            setup_error(&[top, COUNTER], "top"),
            SimulationError::UnknownSignal("nowhere".to_string())
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
        // A real value is not something a four-state register can hold, so it
        // declares nothing at all rather than a rounded stand-in.
        assert!(elaborated.state.get("holdoff").is_none());
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
}

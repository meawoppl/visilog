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

use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::parsers::{
    assignment::ContinuousAssignment,
    behavior::{Event, EventControl, FunctionDeclaration, FunctionVariable},
    expr::Expression,
    identifier::Identifier,
    modules::{ModuleInitArguments, ModuleInstantiation, Port, PortDirection, VerilogModule},
    statements::ModuleStatement,
};
use crate::register::Register;
use crate::simulator::eval::eval;
use crate::simulator::events::{control_fires, signals_read, SignalEdge};
use crate::simulator::exec::{drive, range_width};
use crate::simulator::program::{
    FrameVariable, FunctionDefinition, Instruction, Program, FUNCTION_DELAY_UNSUPPORTED,
};
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::StateStore;

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

/// The `$random` stream lives on the store a frame is *copied* from, so a draw
/// made inside a call would be lost with the frame and the next call would draw
/// the same number again.
const FUNCTION_RANDOM_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("`$random` inside a function");

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
    pub blocks: Vec<TimedBlock>,
    /// The *top* module's input ports, the only ones a testbench may drive.
    pub inputs: Vec<String>,
    /// Qualified name to the store entry it aliases, for ports that were bound
    /// to a parent signal and so have no entry of their own.
    pub aliases: HashMap<String, String>,
}

/// Flattens `modules[top]` and everything it instantiates.
pub fn elaborate(modules: &[VerilogModule], top: usize) -> Result<Elaborated, SimulationError> {
    let mut elaborator = Elaborator {
        modules,
        out: Elaborated {
            state: StateStore::new(),
            assignments: Vec::new(),
            blocks: Vec::new(),
            inputs: Vec::new(),
            aliases: HashMap::new(),
        },
        stack: Vec::new(),
    };
    elaborator.walk(top, &Scope::root())?;
    Ok(elaborator.out)
}

/// How a child port was connected by its parent.
enum Binding {
    /// A plain identifier: the port *is* the parent's signal.
    Alias(String),
    /// A general expression, already rewritten into the flat name space.
    Driven(Expression),
}

/// One instance's view of the flat name space.
struct Scope {
    /// `""` for the top module, `"dut."` for its instance `dut`, and
    /// `"dut.inner."` one level further down.
    prefix: String,
    /// This module's port names, as its parent connected them.
    bindings: HashMap<String, Binding>,
    /// Parameter values the parent overrode, already evaluated in the parent's
    /// scope.
    overrides: HashMap<String, Register>,
}

impl Scope {
    fn root() -> Self {
        Scope {
            prefix: String::new(),
            bindings: HashMap::new(),
            overrides: HashMap::new(),
        }
    }

    fn is_root(&self) -> bool {
        self.prefix.is_empty()
    }

    /// The store entry a name written inside this module refers to.
    ///
    /// An aliased port resolves to the parent's signal — possibly one the
    /// parent itself aliased, so a chain of connections collapses to the single
    /// signal at the top of it. Everything else is local and takes the
    /// instance's prefix.
    fn resolve(&self, local: &str) -> String {
        match self.bindings.get(local) {
            Some(Binding::Alias(outer)) => outer.clone(),
            _ => self.qualified(local),
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

        // Declarations first, in two passes, so that an instantiation can
        // connect to a net declared further down the file — Verilog puts no
        // ordering requirement on module items.
        for port in &module.ports {
            self.declare_port(port, scope)?;
        }
        // Functions come before the declarations rather than with them: a
        // parameter's value may be a call, and the call has to find its
        // definition already compiled.
        self.declare_functions(module, scope)?;
        for statement in &module.statements {
            self.declare(statement, scope)?;
        }
        for statement in &module.statements {
            self.build(statement, scope)?;
        }

        self.stack.pop();
        Ok(())
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
                self.out
                    .state
                    .declare_signed(name.clone(), port.range, port.signed);
                self.out.assignments.push(ContinuousAssignment::new(
                    Expression::Identifier(Identifier::new(name)),
                    expression.clone(),
                ));
                return Ok(());
            }
            None => {}
        }

        let name = scope.qualified(local);
        self.out
            .state
            .declare_signed(name.clone(), port.range, port.signed);

        if !matches!(port.direction, PortDirection::Input) {
            return Ok(());
        }
        if scope.is_root() {
            self.out.inputs.push(name);
        } else {
            // Nothing at all is driving this input, which is what `z` means.
            let floating = Register::high_impedance(range_width(port.range));
            self.out.state.set_ranged(name, floating, port.range);
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
    ) -> Result<(), SimulationError> {
        let mut staged: BTreeMap<String, FunctionDefinition> = BTreeMap::new();
        for statement in &module.statements {
            if let ModuleStatement::FunctionDeclaration(function) = statement {
                let definition = self.compile_function(function, scope)?;
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
    ) -> Result<FunctionDefinition, SimulationError> {
        let qualified = scope.qualified(&function.name.name);

        // The function's own name is the variable its body assigns to return a
        // value, so it is a frame variable like the arguments and the locals.
        let mut frame_names: HashMap<&str, String> = HashMap::new();
        frame_names.insert(function.name.name.as_str(), qualified.clone());

        let variable = |variable: &FunctionVariable| FrameVariable {
            name: format!("{}.{}", qualified, variable.name.name),
            range: variable.range,
            signed: variable.signed,
        };
        let arguments: Vec<FrameVariable> = function.arguments.iter().map(variable).collect();
        let locals: Vec<FrameVariable> = function.locals.iter().map(variable).collect();
        for (declared, frame) in function
            .arguments
            .iter()
            .chain(&function.locals)
            .zip(arguments.iter().chain(&locals))
        {
            frame_names.insert(declared.name.name.as_str(), frame.name.clone());
        }

        let mut program = Program::compile(&function.statements)?;
        program.rename(&|name| match frame_names.get(name) {
            Some(qualified) => qualified.clone(),
            None => scope.resolve(name),
        });

        let own: BTreeSet<String> = frame_names.values().cloned().collect();
        let names = analyse_function_body(&program, &own)?;

        Ok(FunctionDefinition {
            result: FrameVariable {
                name: qualified,
                range: function.range,
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
                    self.declare_local(&net.identifier().name, net.range(), net.is_signed(), scope);
                }
            }
            ModuleStatement::RegisterDeclaration(registers) => {
                for register in registers {
                    self.declare_local(
                        &register.name.name,
                        register.range.unwrap_or((0, 0)),
                        register.signed,
                        scope,
                    );
                }
            }
            ModuleStatement::IntegerDeclaration(integers) => {
                for declaration in integers {
                    // An `integer` is a 32 bit *signed* variable. Signedness is
                    // part of what the keyword means, so there is no qualifier
                    // to read here — it is always true.
                    self.declare_local(&declaration.name.name, (31, 0), true, scope);
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
                    match parameter.range {
                        Some(range) => self.out.state.set_ranged(
                            name,
                            value.coerced(range_width(range)),
                            range,
                        ),
                        None => self.out.state.set(name, value),
                    }
                }
            }
            _ => {}
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
    fn build(&mut self, statement: &ModuleStatement, scope: &Scope) -> Result<(), SimulationError> {
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
            ModuleStatement::Assignment(assignment) => {
                self.out.assignments.push(ContinuousAssignment::new(
                    renamed(assignment.lhs(), scope),
                    renamed(assignment.rhs(), scope),
                ));
            }
            ModuleStatement::AlwaysBlock(block) => {
                let mut program = Program::compile(&block.statements)?;
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
                if !scope.is_root() {
                    program.rename(&|name| scope.resolve(name));
                }
                let implicit_reads = match block.event_control {
                    EventControl::Implicit => {
                        let mut reads: BTreeSet<String> = signals_read(&block.statements)
                            .iter()
                            .map(|name| scope.resolve(name))
                            .collect();
                        // A call reads whatever the function it names reads, and
                        // an `@(*)` block is sensitive to everything it reads —
                        // so it has to wake when one of those moves too.
                        for called in BodyNames::of(&program).calls {
                            if let Some(definition) = self.out.state.function(&called) {
                                reads.extend(definition.reads.iter().cloned());
                            }
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
                let mut program = Program::compile(&block.statements)?;
                if !scope.is_root() {
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

        let mut inner = Scope {
            prefix: format!("{}{}.", scope.prefix, instantiation.instance_name.name),
            bindings: HashMap::new(),
            overrides: HashMap::new(),
        };

        for (port, connection) in connections(child, &instantiation.arguments)? {
            let local = &port.identifier.name;
            let binding = match plain_identifier(connection) {
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
                None => {
                    // The child drives an output, and there is no way to push a
                    // value back through an arbitrary expression.
                    if !matches!(port.direction, PortDirection::Input) {
                        return Err(SimulationError::UndrivablePort {
                            instance: instantiation.instance_name.name.clone(),
                            port: local.clone(),
                            connection: connection.to_contracted_string(),
                        });
                    }
                    Binding::Driven(renamed(connection, scope))
                }
            };
            inner.bindings.insert(local.clone(), binding);
        }

        inner.overrides = self.overrides(child, instantiation, scope)?;

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
        let declared: Vec<&str> = child
            .statements
            .iter()
            .filter_map(|statement| match statement {
                ModuleStatement::ParameterDeclaration(parameters) => Some(parameters),
                _ => None,
            })
            .flatten()
            .map(|parameter| parameter.name.name.as_str())
            .collect();

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
                pairs.extend(declared.iter().copied().zip(expressions));
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

/// Pairs each connected port with the expression the parent bound to it.
///
/// Positional arguments bind in port-declaration order, named arguments by
/// port name. A port nobody mentioned is left out and stays unconnected.
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
            Ok(child.ports.iter().zip(expressions).collect())
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
            Instruction::NonBlocking { .. } => return Err(FUNCTION_NONBLOCKING_UNSUPPORTED),
            Instruction::Task(_) => return Err(FUNCTION_TASK_UNSUPPORTED),
            Instruction::Delay(_) => return Err(FUNCTION_DELAY_UNSUPPORTED),
            _ => {}
        }
    }

    let mut names = BodyNames::of(program);
    if names.random {
        return Err(FUNCTION_RANDOM_UNSUPPORTED);
    }
    // What the function declares itself is not something a call has to copy in.
    names.reads.retain(|name| !own.contains(name));
    Ok(names)
}

/// The signal an assignment target writes, or `None` when the target is not
/// something that names one.
fn assigned_name(target: &Expression) -> Option<&str> {
    match target {
        Expression::Identifier(id)
        | Expression::BitSelect(id, _)
        | Expression::PartSelect(id, _, _) => Some(&id.name),
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
            | Instruction::NonBlocking { target, value } => {
                self.target(target);
                self.expression(value);
            }
            Instruction::JumpIfFalse { condition, .. } => self.expression(condition),
            Instruction::CaseSubject(subject) => self.expression(subject),
            Instruction::JumpIfMatch { label, .. } => self.expression(label),
            Instruction::RepeatInit { count, .. } => self.expression(count),
            Instruction::Jump(_)
            | Instruction::RepeatNext { .. }
            | Instruction::Task(_)
            | Instruction::Delay(_)
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
            other => self.expression(other),
        }
    }

    fn expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Constant(_) => {}
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
        }
    }
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
    if !scope.is_root() {
        rename_expression(&mut copy, &|name| scope.resolve(name));
    }
    copy
}

/// Rewrites every name an expression uses through `resolve`.
///
/// That includes the name of a function it calls, which is qualified exactly as
/// a signal is: a function belongs to the instance that declares it.
pub fn rename_expression(expression: &mut Expression, resolve: &dyn Fn(&str) -> String) {
    match expression {
        Expression::Constant(_) => {}
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
}

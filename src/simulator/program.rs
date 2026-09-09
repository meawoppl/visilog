//! A procedural block compiled into a linear instruction list.
//!
//! A `#delay` can sit anywhere a statement can — including inside an `if` or a
//! `case` arm — so "where did this block get to?" cannot be answered by a
//! statement index. [`Program::compile`] therefore flattens the statement tree
//! into a flat list of [`Instruction`]s whose control flow is carried by
//! jumps. A resume point is then just a program counter.
//!
//! ```text
//! let program = Program::compile(&block.statements)?;
//! match resume(&program, 0, &mut store, &mut tasks)? {
//!     Resume::Halted { pending } => { /* the block finished */ }
//!     Resume::Suspended { pc, delay, pending } => { /* re-enter at `pc` after `delay` */ }
//! }
//! ```
//!
//! The engine keeps Verilog's assignment semantics: a blocking (`=`) write
//! lands immediately, while a non-blocking (`<=`) one resolves its target and
//! evaluates its right hand side now but hands the write back as a
//! [`PendingUpdate`] for [`commit_updates`](super::exec::commit_updates).

use std::collections::{BTreeSet, HashMap};

use crate::parsers::assignment::{
    AssignmentTiming, ProceduralAssignment, ProceduralAssignmentType,
};
use crate::parsers::behavior::{
    BlockStatement, CaseKind, CaseLabel, CaseStatement, Event, EventControl, EventTriggers,
    ForStatement, IfStatement, ProceduralStatements, RepeatStatement, TaskDirection, WaitStatement,
    WhileStatement,
};
use crate::parsers::delay::Delay;
use crate::parsers::expr::Expression;
use crate::parsers::identifier::Identifier;
use crate::register::Register;
use crate::simulator::elaborate::{rename_event_control, rename_expression};
use crate::simulator::eval::{eval, eval_sized};
use crate::simulator::events::signals_read;
use crate::simulator::exec::{
    deassign_drive, drive_resolved, install_drive, release_drive, resolve_target, PendingUpdate,
    ResolvedTarget,
};
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::{DriveLevel, StateStore};
use crate::simulator::tasks::{TaskCall, TaskContext};

/// What a delay reports when the caller cannot hold the resume point a
/// suspension hands back.
pub(crate) const DELAY_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a delay inside a procedural block");

/// The same for the two things that wait on the design rather than on the
/// clock: a `wait` and an event control.
pub(crate) const WAIT_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a wait inside a procedural block");

/// What an intra-assignment timing control on a non-blocking assignment
/// reports. `a <= #5 b;` schedules its write and lets the block carry straight
/// on, where everything here suspends the block that hit it — so running one
/// would hold up statements that are supposed to have already run.
pub(crate) const NONBLOCKING_TIMING_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("an intra-assignment timing control on a non-blocking assignment");

/// What an `@(*)` that heads nothing reports. An implicit sensitivity list is
/// the set of signals the statement it heads reads, so `a = @* b;` — where the
/// right hand side has already been read and there is no statement left to
/// take a list from — asks to wait on nothing at all.
pub(crate) const EMPTY_IMPLICIT_EVENT_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("an `@(*)` event control that reads nothing");

/// What a `fork`…`join` whose branches consume time reports where there is no
/// driver behind the caller to run them on: inside a `function`, and from
/// [`exec::execute_statements`](super::exec::execute_statements).
///
/// Branches that never suspend finish in the order they are run whichever way
/// a simulator schedules them, so those are compiled as a plain block and
/// never get here. One that waits gives the others a turn while it is waiting,
/// and that needs a thread each — which only [`Simulator`](super::runner::Simulator)
/// can hand out.
pub(crate) const FORK_TIMING_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a `fork`/`join` branch that consumes time");

/// What a function body that could consume time reports. A function returns a
/// value into the expression that called it, and an expression is evaluated at
/// one instant, so there is no later for it to resume at.
pub(crate) const FUNCTION_DELAY_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a delay inside a function");

/// What a function body that waits on the design reports. A call is made from
/// inside an expression, so there is no later for it to come back at — the
/// same reason a delay in one cannot work.
pub(crate) const FUNCTION_EVENT_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a wait or event control inside a function");

/// Ceiling on the instructions one [`resume`] may execute before it is called
/// a non-terminating loop.
///
/// A loop with no delay in it — `forever a = 1;`, `while (1) …` — never hands
/// control back on its own, so neither the delta-cycle limit nor the
/// resumption limit in `runner.rs` can see it: both of those count *returns*
/// from here. This is the equivalent bound one level down, and it is what
/// makes a zero-delay `forever` an error rather than a hang.
const MAX_INSTRUCTIONS: usize = 1_000_000;

/// The prefix of the hidden signal a `repeat` counts down in. A Verilog
/// identifier cannot start with `$`, so no design can name one of these.
const REPEAT_COUNTER_PREFIX: &str = "$repeat$";

/// The width of that counter. A `repeat` asking for more iterations than this
/// holds would exhaust [`MAX_INSTRUCTIONS`] long before it ran out of count.
const REPEAT_COUNTER_WIDTH: usize = 64;

/// The prefix of the hidden slot an intra-assignment timing control holds its
/// already-evaluated right hand side in, on the same terms as
/// [`REPEAT_COUNTER_PREFIX`].
const HOLD_SLOT_PREFIX: &str = "$hold$";

/// One step of a compiled procedural block.
///
/// Jump targets are indices into the same instruction list, so any point in the
/// block is addressable by a single `usize`.
#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    /// `a = b;` — evaluate and write the target straight away.
    Blocking {
        target: Expression,
        value: Expression,
    },
    /// `a <= b;` — resolve the target and evaluate the value now, write later.
    NonBlocking {
        target: Expression,
        value: Expression,
    },
    /// `assign a = b;` — install a continuous drive on `a` that outlives the
    /// statement and overrides ordinary writes to it.
    Assign {
        target: Expression,
        value: Expression,
    },
    /// `force a = b;` — the same, at the strength that overrides an `assign`
    /// as well.
    Force {
        target: Expression,
        value: Expression,
    },
    /// `deassign a;` — take the `assign` off.
    Deassign(Expression),
    /// `release a;` — take the `force` off.
    Release(Expression),
    /// Jump when `condition` is not a known non-zero value, so that `x` and `z`
    /// conditions take the branch.
    JumpIfFalse {
        condition: Expression,
        target: usize,
    },
    /// Unconditional jump.
    Jump(usize),
    /// Evaluate a `case` subject and hold it for the comparisons that follow.
    CaseSubject(Expression),
    /// Jump to a `case` arm when `label` matches the held subject. `kind` is
    /// the comparison the statement's keyword asks for.
    JumpIfMatch {
        label: Expression,
        target: usize,
        kind: CaseKind,
    },
    /// `repeat (n)` — evaluate `n` **once** and hold it in the hidden signal
    /// `counter`, then fall through into the loop.
    RepeatInit { counter: String, count: Expression },
    /// Jump to `target` when `counter` has run out; otherwise take one off it
    /// and fall through into the body.
    RepeatNext { counter: String, target: usize },
    /// `#n` — suspend, and resume at the next instruction `n` time units
    /// later.
    ///
    /// The whole [`Delay`] rides here rather than the number it works out to,
    /// because its value is an expression: `#(period / 2)` is not known until
    /// the design has elaborated, and `#n` for a variable `n` is not known
    /// until the block reaches it.
    Delay(Delay),
    /// `wait (c)` — suspend until `c` is true. A condition that is already
    /// true does not suspend at all, so this re-evaluates `c` every time it is
    /// reached.
    Wait(Expression),
    /// `@(posedge clk)` — suspend until an edge the control names is seen. It
    /// is *always* a suspension: an edge is a thing that happens, not a value
    /// that can already be the case.
    EventWait(EventControl),
    /// The first half of an intra-assignment timing control: resolve `target`
    /// and evaluate `value` **now**, holding the result in `slot` until the
    /// control expires.
    Hold {
        slot: String,
        target: Expression,
        value: Expression,
    },
    /// The second half: write what `slot` has been holding to `target`.
    WriteHeld { slot: String, target: Expression },
    /// `a <= #5 b;` — read the right hand side **now** and schedule the write
    /// for `delay` from now, **without suspending the block**.
    ///
    /// That is the whole difference from the blocking form, which suspends and
    /// so can be a `Hold` / `Delay` / `WriteHeld` triple: a non-blocking
    /// assignment schedules its write and lets the block carry on, so the
    /// value has to leave with the update rather than wait in a slot.
    ScheduleWrite {
        target: Expression,
        value: Expression,
        delay: Delay,
    },
    /// `$display(…)` and friends — a call to a system task.
    Task(TaskCall),
    /// `disable blk;` — terminate the activity of the named scope.
    ///
    /// The scope is a range of instructions ([`Program::scopes`]), so when the
    /// program counter is already inside it this is a jump to where that range
    /// ends. When it is not, the scope belongs to some other block and only
    /// the driver can reach it, which is what [`Resume::Disabled`] is for.
    Disable(String),
    /// `fork … join` whose branches consume time — start one thread of
    /// execution per branch and suspend until every one of them has finished.
    ///
    /// `branches` holds the instruction each branch starts at and `join` the
    /// instruction the block carries on at. The branch bodies are laid out
    /// between the two, each ending in a [`Instruction::JoinBranch`], so a
    /// branch is an ordinary run of this same instruction list and a
    /// `#delay` inside one suspends exactly as it does anywhere else.
    Fork { branches: Vec<usize>, join: usize },
    /// The end of one `fork` branch. It ends *that thread*, not the block, and
    /// the last one to arrive is what lets the block past its `join`.
    JoinBranch,
    /// The end of the block.
    Halt,
}

/// A named scope — a `begin : blk` label, or a task whose body was inlined
/// where it was enabled — and the half-open range of instructions it covers.
///
/// `disable` is the only thing that needs this. It names a scope rather than a
/// statement, and "terminate that scope" is exactly "continue at `end`", so
/// the range is the whole of what a disable has to know.
#[derive(Clone, Debug, PartialEq)]
pub struct ScopeRange {
    pub name: String,
    pub start: usize,
    pub end: usize,
}

/// A procedural block flattened into instructions.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Program {
    instructions: Vec<Instruction>,
    /// Half-open instruction ranges that came from an enabled task's body
    /// rather than from this block's own statements.
    ///
    /// The names in one of those have already been resolved against the task
    /// that owns them, so [`rename_local`](Program::rename_local) has to leave
    /// them alone: a task local called `count` and a design signal called
    /// `count` are different variables, and renaming twice would confuse them.
    inlined: Vec<(usize, usize)>,
    /// The named scopes in the program, each with the instructions it covers.
    /// Only a `disable` reads them, so a design that never writes one carries
    /// an empty `Vec` and pays nothing.
    scopes: Vec<ScopeRange>,
}

/// How far a [`Program`] had been built, so that a compilation step can be
/// unwound and taken a second way.
#[derive(Clone, Copy, Debug)]
struct ProgramMark {
    instructions: usize,
    scopes: usize,
    inlined: usize,
}

/// Why [`resume`] gave control back.
#[derive(Clone, Debug, PartialEq)]
pub enum Resume {
    /// Ran off the end of the block.
    Halted { pending: Vec<PendingUpdate> },
    /// Hit a `#delay`. Resume at `pc` once `delay` time units have passed.
    Suspended {
        pc: usize,
        delay: i64,
        pending: Vec<PendingUpdate>,
    },
    /// Hit something that waits on the design rather than on the clock. Resume
    /// at `pc` once `wait` is satisfied.
    Waiting {
        pc: usize,
        wait: WaitReason,
        pending: Vec<PendingUpdate>,
    },
    /// Hit a `disable` naming a scope this block is not inside, so the block
    /// that *is* in it can only be reached by the driver. Cancel it, then come
    /// straight back here at `pc` — a `disable` of somebody else does not
    /// suspend the block that wrote it.
    Disabled {
        scope: String,
        pc: usize,
        pending: Vec<PendingUpdate>,
    },
    /// Hit a `fork` whose branches consume time. Start one thread at each of
    /// `branches` and hold this one at `pc` — the `join` — until the last of
    /// them arrives.
    Forked {
        branches: Vec<usize>,
        pc: usize,
        pending: Vec<PendingUpdate>,
    },
    /// Reached the end of one `fork` branch. The thread is over; the block it
    /// is a branch of carries on once its siblings are over too.
    BranchDone { pending: Vec<PendingUpdate> },
}

/// What a suspended block is waiting for, which is what decides how the driver
/// finds out that it can go on.
#[derive(Clone, Debug, PartialEq)]
pub enum WaitReason {
    /// `wait (c)` — a value, so the driver re-enters the block and lets the
    /// instruction re-evaluate the condition. `pc` is the `Wait` itself.
    Condition,
    /// `@(posedge clk)` — an edge, which is gone by the time the block could
    /// look for it, so the driver matches the control against the edges of the
    /// round instead. `pc` is the instruction *after* the wait.
    Event(EventControl),
}

/// Rewrites every name one instruction uses through `resolve`.
fn rename_instruction(instruction: &mut Instruction, resolve: &dyn Fn(&str) -> String) {
    match instruction {
        Instruction::Blocking { target, value }
        | Instruction::NonBlocking { target, value }
        | Instruction::Assign { target, value }
        | Instruction::Force { target, value } => {
            rename_expression(target, resolve);
            rename_expression(value, resolve);
        }
        Instruction::Deassign(target) | Instruction::Release(target) => {
            rename_expression(target, resolve)
        }
        Instruction::JumpIfFalse { condition, .. } => rename_expression(condition, resolve),
        Instruction::CaseSubject(subject) => rename_expression(subject, resolve),
        Instruction::JumpIfMatch { label, .. } => rename_expression(label, resolve),
        // A `repeat` counter is qualified like any other signal, which is what
        // gives two instances of the same module a counter each rather than one
        // they trample on together.
        Instruction::RepeatInit { counter, count } => {
            *counter = resolve(counter);
            rename_expression(count, resolve);
        }
        Instruction::RepeatNext { counter, .. } => *counter = resolve(counter),
        Instruction::Task(call) => call.rename(resolve),
        Instruction::Wait(condition) => rename_expression(condition, resolve),
        Instruction::EventWait(control) => rename_event_control(control, resolve),
        Instruction::Hold {
            slot,
            target,
            value,
        } => {
            *slot = resolve(slot);
            rename_expression(target, resolve);
            rename_expression(value, resolve);
        }
        Instruction::WriteHeld { slot, target } => {
            *slot = resolve(slot);
            rename_expression(target, resolve);
        }
        Instruction::ScheduleWrite {
            target,
            value,
            delay,
        } => {
            rename_expression(target, resolve);
            rename_expression(value, resolve);
            for expression in delay.expressions_mut() {
                rename_expression(expression, resolve);
            }
        }
        // `#(period / 2)` names a parameter, and a parameter belongs to the
        // instance that declared it like anything else.
        Instruction::Delay(delay) => {
            for expression in delay.expressions_mut() {
                rename_expression(expression, resolve);
            }
        }
        // A `disable` names a scope rather than a signal, so it is renamed
        // beside the scope table it points into — see
        // [`Program::rename_scopes`] — and never through a map of variables.
        Instruction::Jump(_)
        | Instruction::Disable(_)
        | Instruction::Fork { .. }
        | Instruction::JoinBranch
        | Instruction::Halt => {}
    }
}

fn substitute_instruction(instruction: &mut Instruction, replace: &dyn Fn(&mut Expression)) {
    match instruction {
        Instruction::Blocking { target, value }
        | Instruction::NonBlocking { target, value }
        | Instruction::Assign { target, value }
        | Instruction::Force { target, value } => {
            replace(target);
            replace(value);
        }
        Instruction::Deassign(target) | Instruction::Release(target) => replace(target),
        Instruction::JumpIfFalse { condition, .. } => replace(condition),
        Instruction::CaseSubject(subject) => replace(subject),
        Instruction::JumpIfMatch { label, .. } => replace(label),
        Instruction::RepeatInit { count, .. } => replace(count),
        Instruction::Task(call) => call.substitute(replace),
        Instruction::Wait(condition) => replace(condition),
        // A genvar may index a signal a control waits on:
        // `@(posedge clk[i])` inside a generate loop.
        Instruction::EventWait(control) => {
            if let EventControl::Events(events) = control {
                for event in events {
                    replace(&mut event.expression);
                }
            }
        }
        Instruction::Hold { target, value, .. } => {
            replace(target);
            replace(value);
        }
        Instruction::WriteHeld { target, .. } => replace(target),
        Instruction::ScheduleWrite {
            target,
            value,
            delay,
        } => {
            replace(target);
            replace(value);
            for expression in delay.expressions_mut() {
                replace(expression);
            }
        }
        // A generate loop may write its own index into a delay: `#(i * 10)`.
        Instruction::Delay(delay) => {
            for expression in delay.expressions_mut() {
                replace(expression);
            }
        }
        Instruction::Jump(_)
        | Instruction::RepeatNext { .. }
        | Instruction::Disable(_)
        | Instruction::Fork { .. }
        | Instruction::JoinBranch
        | Instruction::Halt => {}
    }
}

impl Program {
    /// Flattens a statement body into instructions.
    pub fn compile(
        statements: &[ProceduralStatements],
        tasks: &TaskTable,
    ) -> Result<Program, SimulationError> {
        let mut program = Program::compile_body(statements, tasks, "")?;
        program.emit(Instruction::Halt);
        program.check_fork_disables()?;
        Ok(program)
    }

    /// Rejects a `disable` written inside a `fork` branch that names a scope
    /// the `fork` itself is inside.
    ///
    /// Terminating such a scope has to stop *every* branch and the block parked
    /// at the join, and the jump a local `disable` compiles to would stop only
    /// the one thread that ran it — leaving its siblings running and the join
    /// waiting for an arrival that can never come. That is a wrong answer with
    /// no symptom, so it is named here instead. The check is a post-pass rather
    /// than part of `compile_fork` because the enclosing block's own range is
    /// not recorded until the block around the `fork` has finished compiling.
    fn check_fork_disables(&self) -> Result<(), SimulationError> {
        for (site, instruction) in self.instructions.iter().enumerate() {
            let Instruction::Fork { join, .. } = instruction else {
                continue;
            };
            for (pc, inner) in self.instructions[site + 1..*join].iter().enumerate() {
                let Instruction::Disable(scope) = inner else {
                    continue;
                };
                let pc = site + 1 + pc;
                if self
                    .scope_end_containing(scope, pc)
                    .is_some_and(|end| end >= *join)
                {
                    return Err(SimulationError::Unsupported(
                        "a `disable` inside a `fork` branch naming a scope around the `fork`",
                    ));
                }
            }
        }
        Ok(())
    }

    /// The same, without the trailing [`Instruction::Halt`] — a task's body,
    /// which is spliced into the middle of whatever enables it and so must not
    /// end the block it lands in.
    ///
    /// `scope` is what a named block inside the body spells its variables
    /// under, so a task's blocks are `load.loop.i` rather than sharing
    /// `loop.i` with every other task that names a block the same way.
    pub fn compile_body(
        statements: &[ProceduralStatements],
        tasks: &TaskTable,
        scope: &str,
    ) -> Result<Program, SimulationError> {
        let mut program = Program::default();
        program.compile_statements(statements, tasks, scope)?;
        Ok(program)
    }

    /// Whether any enabled task's body was inlined into this program.
    ///
    /// The statement tree keeps no trace of what a task's body reads, so an
    /// `@(*)` block that enables one has to take its sensitivity list from the
    /// compiled instructions instead.
    pub fn inlines_a_task(&self) -> bool {
        !self.inlined.is_empty()
    }

    /// Whether the block does nothing at all, i.e. it compiled to a bare
    /// `Halt`.
    pub fn is_empty(&self) -> bool {
        matches!(self.instructions.as_slice(), [Instruction::Halt])
    }

    /// The compiled instructions, in program order.
    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    /// Rewrites every signal the program names through `resolve`.
    ///
    /// Elaborating an instance flattens the child's signals into the parent's
    /// store under qualified names; this is what re-points an already compiled
    /// body at them. Doing it on the instruction list rather than on the
    /// statement tree is what makes it possible at all — the tree is borrowed
    /// from the parsed module and is not `Clone`, while an [`Instruction`]
    /// owns its expressions outright.
    pub fn rename(&mut self, resolve: &dyn Fn(&str) -> String) {
        for instruction in &mut self.instructions {
            rename_instruction(instruction, resolve);
        }
        self.rename_scopes(resolve);
    }

    /// Rewrites the *scope* names — the block labels and task names a
    /// `disable` reaches for — through `resolve`.
    ///
    /// A label is not a signal, so it deliberately does not travel with the
    /// rest of an instruction's names: a task's locals and a named block's
    /// variables are renamed over a range of instructions, and a scope belongs
    /// to the instance the block was elaborated into rather than to either of
    /// those. Both halves move together so that `disable blk` inside instance
    /// `dut` and the scope `dut.blk` still name the same thing.
    fn rename_scopes(&mut self, resolve: &dyn Fn(&str) -> String) {
        for scope in &mut self.scopes {
            scope.name = resolve(&scope.name);
        }
        for instruction in &mut self.instructions {
            if let Instruction::Disable(name) = instruction {
                *name = resolve(name);
            }
        }
    }

    /// The named scopes the program holds, each with the instructions it
    /// covers.
    pub fn scopes(&self) -> &[ScopeRange] {
        &self.scopes
    }

    /// Where `scope` ends, if `pc` is inside it — which is where a `disable`
    /// of it continues.
    ///
    /// Two enables of one task are two ranges under one name, so the range is
    /// chosen by the program counter rather than by the name alone.
    pub fn scope_end_containing(&self, scope: &str, pc: usize) -> Option<usize> {
        self.scopes
            .iter()
            .find(|range| range.name == scope && range.start <= pc && pc < range.end)
            .map(|range| range.end)
    }

    /// The first `disable` in the program naming a scope that does not contain
    /// it, or `None` when every one of them is an exit from a block it is
    /// written inside.
    ///
    /// A non-local `disable` reaches out of the program it is written in, and
    /// a caller with nothing else to reach — a function frame — has to say so
    /// rather than quietly do nothing.
    pub fn nonlocal_disable(&self) -> Option<&str> {
        self.instructions
            .iter()
            .enumerate()
            .find_map(|(pc, instruction)| match instruction {
                Instruction::Disable(scope) if self.scope_end_containing(scope, pc).is_none() => {
                    Some(scope.as_str())
                }
                _ => None,
            })
    }

    /// Rewrites every expression the program holds through `replace`.
    ///
    /// [`rename`](Program::rename) can only map a name onto another name, and a
    /// genvar is not a name at all by the time anything runs — it is the
    /// integer the generate loop bound it to. Substituting it means replacing
    /// an identifier *node* with a constant, which is why this takes the
    /// expression rather than the name.
    pub fn substitute(&mut self, replace: &dyn Fn(&mut Expression)) {
        for instruction in &mut self.instructions {
            substitute_instruction(instruction, replace);
        }
    }

    /// Rewrites the names this program's *own* statements use, leaving the
    /// instructions spliced in from a task's body untouched.
    ///
    /// That is what keeps a task's locals distinct from the caller's: by the
    /// time a body is spliced in its names are already resolved, and a second
    /// pass with the caller's map would re-point a design signal the inner task
    /// read at a variable the outer one happens to declare under the same name.
    pub fn rename_local(&mut self, resolve: &dyn Fn(&str) -> String) {
        self.rename_range(0, self.instructions.len(), resolve);
    }

    /// [`rename_local`](Program::rename_local) over one half-open range of
    /// instructions, which is what a named block's own variables are renamed
    /// through: the block is a scope, and only what it compiled to is in it.
    fn rename_range(&mut self, from: usize, to: usize, resolve: &dyn Fn(&str) -> String) {
        let skip: Vec<(usize, usize)> = self.inlined.clone();
        let mut skipping = skip.iter().peekable();
        let mut index = from;
        while index < to {
            if let Some((start, end)) = skipping.peek() {
                if index >= *end {
                    skipping.next();
                    continue;
                }
                if index == *start {
                    index = *end;
                    skipping.next();
                    continue;
                }
            }
            rename_instruction(&mut self.instructions[index], resolve);
            index += 1;
        }
    }

    /// Appends a task body, recording it as inlined.
    ///
    /// Jump targets are indices into the instruction list, so every one of them
    /// shifts by where the body lands. So does a `repeat` counter's name, which
    /// is derived from the index of its own `RepeatInit`: two enables of one
    /// task are two loops, and a shared counter would let them count each other
    /// down.
    fn splice(&mut self, body: &Program) {
        let offset = self.instructions.len();
        for instruction in &body.instructions {
            let mut instruction = instruction.clone();
            match &mut instruction {
                Instruction::Jump(target)
                | Instruction::JumpIfFalse { target, .. }
                | Instruction::JumpIfMatch { target, .. }
                | Instruction::RepeatNext { target, .. } => *target += offset,
                // A `fork` inside a task's body points at branch bodies in the
                // same list, so its targets move with everything else.
                Instruction::Fork { branches, join } => {
                    for branch in branches.iter_mut() {
                        *branch += offset;
                    }
                    *join += offset;
                }
                _ => {}
            }
            match &mut instruction {
                Instruction::RepeatInit { counter, .. }
                | Instruction::RepeatNext { counter, .. } => {
                    *counter = format!("{}${}", counter, offset)
                }
                Instruction::Hold { slot, .. } | Instruction::WriteHeld { slot, .. } => {
                    *slot = format!("{}${}", slot, offset)
                }
                _ => {}
            }
            self.instructions.push(instruction);
        }
        // A named block written inside a task is a scope of its own, and its
        // range moves by exactly what the body moved by.
        for scope in &body.scopes {
            self.scopes.push(ScopeRange {
                name: scope.name.clone(),
                start: scope.start + offset,
                end: scope.end + offset,
            });
        }
        self.inlined.push((offset, self.instructions.len()));
    }

    fn compile_statements(
        &mut self,
        statements: &[ProceduralStatements],
        tasks: &TaskTable,
        scope: &str,
    ) -> Result<(), SimulationError> {
        for statement in statements {
            match statement {
                ProceduralStatements::Delay(delay) => {
                    self.emit(Instruction::Delay(delay.clone()));
                }
                // `#5 <statement>` waits, then runs the statement — exactly
                // what a bare `#5;` written in front of it would do. The body
                // is compiled inline, so a delay nested in it suspends just as
                // one at the top level does.
                ProceduralStatements::Delayed { delay, statements } => {
                    self.emit(Instruction::Delay(delay.clone()));
                    self.compile_statements(statements, tasks, scope)?;
                }
                ProceduralStatements::Assignment(assignment) => {
                    self.compile_assignment(assignment)?
                }
                // The four drive statements are one instruction each: what they
                // install outlives the block, so there is nothing to flatten.
                ProceduralStatements::Assign { target, value } => {
                    self.emit(Instruction::Assign {
                        target: target.clone(),
                        value: value.clone(),
                    });
                }
                ProceduralStatements::Force { target, value } => {
                    self.emit(Instruction::Force {
                        target: target.clone(),
                        value: value.clone(),
                    });
                }
                ProceduralStatements::Deassign(target) => {
                    self.emit(Instruction::Deassign(target.clone()));
                }
                ProceduralStatements::Release(target) => {
                    self.emit(Instruction::Release(target.clone()));
                }
                ProceduralStatements::If(conditional) => {
                    self.compile_if(conditional, tasks, scope)?
                }
                ProceduralStatements::Case(case) => self.compile_case(case, tasks, scope)?,
                ProceduralStatements::For(statement) => {
                    self.compile_for(statement, tasks, scope)?
                }
                ProceduralStatements::While(statement) => {
                    self.compile_while(statement, tasks, scope)?
                }
                ProceduralStatements::Repeat(statement) => {
                    self.compile_repeat(statement, tasks, scope)?
                }
                ProceduralStatements::Forever(statements) => {
                    self.compile_forever(statements, tasks, scope)?
                }
                ProceduralStatements::Block(block) => self.compile_block(block, tasks, scope)?,
                ProceduralStatements::Fork(block) => self.compile_fork(block, tasks, scope)?,
                // `wait (c) S` is the condition followed by the statement it
                // guards: the instruction falls through the moment `c` is
                // true, so a `wait` on something already true costs a single
                // evaluation and no suspension.
                ProceduralStatements::Wait(WaitStatement {
                    condition,
                    statements,
                }) => {
                    self.emit(Instruction::Wait(condition.clone()));
                    self.compile_statements(statements, tasks, scope)?;
                }
                ProceduralStatements::EventControlled {
                    control,
                    statements,
                } => {
                    self.emit_event_wait(control, statements)?;
                    self.compile_statements(statements, tasks, scope)?;
                }
                // A task's body is spliced in where the enable stands, with its
                // arguments copied in ahead of it and back out behind it. That
                // is what makes a `#delay` inside a task suspend the block that
                // enabled it: the resume point is already a program counter,
                // and the body's instructions are in that same list.
                ProceduralStatements::TaskEnable { name, arguments } => {
                    self.compile_task_enable(name, arguments, tasks)?
                }
                // The name is resolved against the scopes this statement is
                // written inside, innermost first, which is what makes
                // `disable wait_loop` inside task `t` mean `t.wait_loop`
                // rather than a block of that name somewhere else. A name that
                // matches none of them is left as it stands, to be found among
                // the module's own scopes when it runs.
                ProceduralStatements::Disable(name) => {
                    self.emit(Instruction::Disable(enclosing_scope(scope, &name.name)));
                }
                // Which `$name`s exist is settled here rather than while the
                // design runs, so an unrecognised one fails before it can look
                // like a task that quietly printed nothing.
                ProceduralStatements::SystemTask(call) => {
                    let call = TaskCall::compile(call)?;
                    self.emit(Instruction::Task(call));
                }
            }
        }
        Ok(())
    }

    fn compile_assignment(
        &mut self,
        assignment: &ProceduralAssignment,
    ) -> Result<(), SimulationError> {
        let target = assignment.lhs().clone();
        let value = assignment.rhs().clone();
        let Some(timing) = assignment.timing() else {
            self.emit(match assignment.assignment_type() {
                ProceduralAssignmentType::Blocking => Instruction::Blocking { target, value },
                ProceduralAssignmentType::NonBlocking => Instruction::NonBlocking { target, value },
            });
            return Ok(());
        };
        if matches!(
            assignment.assignment_type(),
            ProceduralAssignmentType::NonBlocking
        ) {
            // `a <= #5 b;` reads `b` now and schedules the write, without
            // suspending — one instruction, where the blocking form needs the
            // `Hold` / wait / `WriteHeld` triple precisely because it *does*
            // suspend. An **event** control on a non-blocking assignment would
            // need a watch that outlives the block, which nothing here has, so
            // it stays a named error.
            let AssignmentTiming::Delay(delay) = timing else {
                return Err(NONBLOCKING_TIMING_UNSUPPORTED);
            };
            self.emit(Instruction::ScheduleWrite {
                target,
                value,
                delay: delay.clone(),
            });
            return Ok(());
        }

        // The right hand side is read *now* and written when the control
        // expires, which is the whole of what makes the control an
        // intra-assignment one. The value has to survive the suspension in
        // between, so it goes in a hidden slot named after the instruction
        // that filled it — two of these in one block are two slots.
        let slot = format!("{}{}", HOLD_SLOT_PREFIX, self.next());
        self.emit(Instruction::Hold {
            slot: slot.clone(),
            target: target.clone(),
            value,
        });
        match timing {
            AssignmentTiming::Delay(delay) => {
                self.emit(Instruction::Delay(delay.clone()));
            }
            AssignmentTiming::Event {
                repeat: None,
                control,
            } => self.emit_event_wait(control, &[])?,
            // `repeat (n) @(ev)` waits for the event `n` times, so it is the
            // `repeat` loop with the wait as its whole body. A count of zero
            // therefore writes immediately, which is what the LRM says.
            AssignmentTiming::Event {
                repeat: Some(count),
                control,
            } => {
                let counter = format!("{}{}", REPEAT_COUNTER_PREFIX, self.next());
                self.emit(Instruction::RepeatInit {
                    counter: counter.clone(),
                    count: count.clone(),
                });
                let top = self.next();
                let branch = self.emit(Instruction::RepeatNext { counter, target: 0 });
                self.emit_event_wait(control, &[])?;
                self.emit(Instruction::Jump(top));
                let end = self.next();
                self.patch(branch, end);
            }
        }
        self.emit(Instruction::WriteHeld { slot, target });
        Ok(())
    }

    /// Emits the wait an event control asks for.
    ///
    /// `@*` in front of a statement is sensitive to what *that statement*
    /// reads, where the same token in front of a block is sensitive to what
    /// the block reads — so it is resolved into the explicit list it stands
    /// for here, while the statement it heads is in hand. Everything past this
    /// point then sees one kind of event control instead of two.
    fn emit_event_wait(
        &mut self,
        control: &EventControl,
        body: &[ProceduralStatements],
    ) -> Result<(), SimulationError> {
        let control = match control {
            EventControl::Implicit => implicit_control(body)?,
            other => other.clone(),
        };
        self.emit(Instruction::EventWait(control));
        Ok(())
    }

    /// `begin : name … end` — the statements, with the block's own variables
    /// renamed into the scope its name opens.
    ///
    /// An unnamed block is nothing but grouping and compiles to its contents.
    /// A named one is renamed *after* its body is compiled, over exactly the
    /// instructions the body produced: a nested block has already resolved its
    /// own variables by then, and they are spelled with a `.` this map cannot
    /// match, so the inner scope wins where the two declare the same name.
    fn compile_block(
        &mut self,
        block: &BlockStatement,
        tasks: &TaskTable,
        scope: &str,
    ) -> Result<(), SimulationError> {
        let Some(name) = &block.name else {
            return self.compile_statements(&block.statements, tasks, scope);
        };

        let inner = block_scope(scope, &name.name);
        let start = self.next();
        self.compile_statements(&block.statements, tasks, &inner)?;
        let end = self.next();
        // The label is a scope a `disable` may name, and terminating it is
        // continuing at `end` — so the range is recorded whether or not the
        // block declares anything.
        self.scopes.push(ScopeRange {
            name: inner.trim_end_matches('.').to_string(),
            start,
            end,
        });

        if block.locals.is_empty() {
            return Ok(());
        }
        let locals: HashMap<&str, String> = block
            .locals
            .iter()
            .map(|local| {
                (
                    local.name.name.as_str(),
                    format!("{}{}", inner, local.name.name),
                )
            })
            .collect();
        self.rename_range(start, end, &|name| match locals.get(name) {
            Some(qualified) => qualified.clone(),
            None => name.to_string(),
        });
        Ok(())
    }

    /// `fork … join` — the branches, run at once, with the block carrying on
    /// once the last of them has finished.
    ///
    /// Branches that consume no time cannot tell concurrent from sequential:
    /// each runs to completion without giving another a turn either way. So a
    /// `fork` is compiled exactly as a `begin`…`end` is *first*, and only if
    /// what came out can suspend is it thrown away and compiled again as real
    /// threads. That keeps the common case free of the driver round trip a
    /// thread costs, and it keeps a `fork` legal inside a `function`, where
    /// there is no driver to spawn anything.
    ///
    /// Whether it can suspend is asked of the compiled instructions rather than
    /// of the statements, so a branch that suspends inside an enabled *task's*
    /// body counts — the body is already spliced in by then.
    fn compile_fork(
        &mut self,
        block: &BlockStatement,
        tasks: &TaskTable,
        scope: &str,
    ) -> Result<(), SimulationError> {
        let mark = self.mark();
        self.compile_block(block, tasks, scope)?;
        if block.statements.len() < 2
            || !self.instructions[mark.instructions..]
                .iter()
                .any(instruction_suspends)
        {
            return Ok(());
        }
        self.rewind(mark);
        self.compile_threaded_fork(block, tasks, scope)
    }

    /// The concurrent layout:
    ///
    /// ```text
    ///     Fork { branches: [b0, b1], join: J }
    /// b0: <branch 0>  JoinBranch
    /// b1: <branch 1>  JoinBranch
    /// J:  <after the join>
    /// ```
    ///
    /// Each branch is an ordinary run of this same list, so a `#delay`, a
    /// `wait` or a nested `fork` inside one needs nothing new: a branch is a
    /// program counter like any other.
    fn compile_threaded_fork(
        &mut self,
        block: &BlockStatement,
        tasks: &TaskTable,
        scope: &str,
    ) -> Result<(), SimulationError> {
        let inner = match &block.name {
            Some(name) => block_scope(scope, &name.name),
            None => scope.to_string(),
        };

        let start = self.next();
        let site = self.emit(Instruction::Fork {
            branches: Vec::new(),
            join: 0,
        });
        let mut branches = Vec::with_capacity(block.statements.len());
        for statement in &block.statements {
            branches.push(self.next());
            self.compile_statements(std::slice::from_ref(statement), tasks, &inner)?;
            self.emit(Instruction::JoinBranch);
        }
        let join = self.next();
        match &mut self.instructions[site] {
            Instruction::Fork {
                branches: slot,
                join: target,
            } => {
                *slot = branches;
                *target = join;
            }
            other => unreachable!("cannot patch {:?}", other),
        }

        if block.name.is_none() {
            return Ok(());
        }
        // The label is a scope a `disable` may name, and it covers the `Fork`
        // itself as well as the branches — cancelling it has to reach the
        // parent parked at the join, not only the threads.
        self.scopes.push(ScopeRange {
            name: inner.trim_end_matches('.').to_string(),
            start,
            end: join,
        });
        if block.locals.is_empty() {
            return Ok(());
        }
        let locals: HashMap<&str, String> = block
            .locals
            .iter()
            .map(|local| {
                (
                    local.name.name.as_str(),
                    format!("{}{}", inner, local.name.name),
                )
            })
            .collect();
        self.rename_range(start, join, &|name| match locals.get(name) {
            Some(qualified) => qualified.clone(),
            None => name.to_string(),
        });
        Ok(())
    }

    /// `if (c) T else E` becomes
    /// `JumpIfFalse(c, else); T; Jump(end); else: E; end:`.
    fn compile_if(
        &mut self,
        conditional: &IfStatement,
        tasks: &TaskTable,
        scope: &str,
    ) -> Result<(), SimulationError> {
        let branch = self.emit(Instruction::JumpIfFalse {
            condition: conditional.condition.clone(),
            target: 0,
        });
        self.compile_statements(&conditional.then_statements, tasks, scope)?;

        match &conditional.else_statements {
            Some(else_statements) => {
                let skip_else = self.emit(Instruction::Jump(0));
                let else_start = self.next();
                self.patch(branch, else_start);
                self.compile_statements(else_statements, tasks, scope)?;
                let end = self.next();
                self.patch(skip_else, end);
            }
            None => {
                let end = self.next();
                self.patch(branch, end);
            }
        }
        Ok(())
    }

    /// `while (c) B` becomes
    /// `top: JumpIfFalse(c, end); B; Jump(top); end:`.
    ///
    /// The condition is re-evaluated at the top of every iteration, and an `x`
    /// or `z` one ends the loop — `JumpIfFalse` reads a condition the way `if`
    /// does.
    fn compile_while(
        &mut self,
        statement: &WhileStatement,
        tasks: &TaskTable,
        scope: &str,
    ) -> Result<(), SimulationError> {
        let top = self.next();
        let branch = self.emit(Instruction::JumpIfFalse {
            condition: statement.condition.clone(),
            target: 0,
        });
        self.compile_statements(&statement.statements, tasks, scope)?;
        self.emit(Instruction::Jump(top));
        let end = self.next();
        self.patch(branch, end);
        Ok(())
    }

    /// `for (i; c; s) B` is the `while` shape with the initialiser in front of
    /// it and the step in front of the back-jump, so `continue`-less Verilog
    /// runs `s` after every completed iteration and never after the test fails.
    fn compile_for(
        &mut self,
        statement: &ForStatement,
        tasks: &TaskTable,
        scope: &str,
    ) -> Result<(), SimulationError> {
        self.compile_assignment(&statement.initializer)?;
        let top = self.next();
        let branch = self.emit(Instruction::JumpIfFalse {
            condition: statement.condition.clone(),
            target: 0,
        });
        self.compile_statements(&statement.statements, tasks, scope)?;
        self.compile_assignment(&statement.step)?;
        self.emit(Instruction::Jump(top));
        let end = self.next();
        self.patch(branch, end);
        Ok(())
    }

    /// `repeat (n) B` becomes
    /// `RepeatInit(k, n); top: RepeatNext(k, end); B; Jump(top); end:`.
    ///
    /// The count is evaluated by the `RepeatInit`, which runs once, so a body
    /// that moves one of `n`'s operands cannot change how many iterations are
    /// left. The remaining count lives in the [`StateStore`] rather than in
    /// this loop's stack frame because a `#delay` in the body returns from
    /// [`resume`] entirely: the only state that survives a suspension is the
    /// program counter and the store. The counter's name is derived from the
    /// index of its own `RepeatInit`, so nested and sibling `repeat`s each get
    /// their own.
    fn compile_repeat(
        &mut self,
        statement: &RepeatStatement,
        tasks: &TaskTable,
        scope: &str,
    ) -> Result<(), SimulationError> {
        let counter = format!("{}{}", REPEAT_COUNTER_PREFIX, self.next());
        self.emit(Instruction::RepeatInit {
            counter: counter.clone(),
            count: statement.count.clone(),
        });

        let top = self.next();
        let branch = self.emit(Instruction::RepeatNext { counter, target: 0 });
        self.compile_statements(&statement.statements, tasks, scope)?;
        self.emit(Instruction::Jump(top));
        let end = self.next();
        self.patch(branch, end);
        Ok(())
    }

    /// `forever B` is `top: B; Jump(top)` — nothing ends it, so only a
    /// `#delay` in `B` lets the rest of the simulation get a turn.
    fn compile_forever(
        &mut self,
        statements: &[ProceduralStatements],
        tasks: &TaskTable,
        scope: &str,
    ) -> Result<(), SimulationError> {
        let top = self.next();
        self.compile_statements(statements, tasks, scope)?;
        self.emit(Instruction::Jump(top));
        Ok(())
    }

    /// A `case` becomes its subject, then one `JumpIfMatch` per label in source
    /// order, then a fall-through jump to the `default` arm — or past the whole
    /// statement when there is none — then the arm bodies.
    ///
    /// Only the first `default` is reachable, so later ones are not compiled.
    fn compile_case(
        &mut self,
        case: &CaseStatement,
        tasks: &TaskTable,
        scope: &str,
    ) -> Result<(), SimulationError> {
        self.emit(Instruction::CaseSubject(case.subject.clone()));

        let mut arms: Vec<&[ProceduralStatements]> = Vec::new();
        let mut comparisons: Vec<(usize, usize)> = Vec::new();
        let mut default_arm = None;

        for item in &case.items {
            match &item.label {
                CaseLabel::Expressions(expressions) => {
                    let arm = arms.len();
                    arms.push(&item.statements);
                    for expression in expressions {
                        let site = self.emit(Instruction::JumpIfMatch {
                            label: expression.clone(),
                            target: 0,
                            kind: case.kind,
                        });
                        comparisons.push((site, arm));
                    }
                }
                CaseLabel::Default => {
                    if default_arm.is_none() {
                        default_arm = Some(arms.len());
                        arms.push(&item.statements);
                    }
                }
            }
        }

        let fall_through = self.emit(Instruction::Jump(0));

        let mut starts = Vec::with_capacity(arms.len());
        let mut exits = Vec::with_capacity(arms.len());
        for arm in &arms {
            starts.push(self.next());
            self.compile_statements(arm, tasks, scope)?;
            exits.push(self.emit(Instruction::Jump(0)));
        }

        let end = self.next();
        for (site, arm) in comparisons {
            self.patch(site, starts[arm]);
        }
        // A `case` that matches nothing and has no `default` does nothing.
        self.patch(fall_through, default_arm.map_or(end, |arm| starts[arm]));
        for exit in exits {
            self.patch(exit, end);
        }
        Ok(())
    }

    /// `t(a, b);` becomes the argument copy-in, the body, and the copy-out.
    ///
    /// A task returns nothing, so its `output` and `inout` arguments are the
    /// only way a result gets back to the caller: they are written to the
    /// caller's variables *after* the body has run, which is where the LRM puts
    /// the copy and what iverilog does.
    fn compile_task_enable(
        &mut self,
        name: &Identifier,
        arguments: &[Expression],
        tasks: &TaskTable,
    ) -> Result<(), SimulationError> {
        let definition = tasks
            .get(&name.name)
            .ok_or_else(|| SimulationError::UnknownTask(name.name.clone()))?;
        if arguments.len() != definition.arguments.len() {
            return Err(SimulationError::TaskArity {
                name: name.name.clone(),
                expected: definition.arguments.len(),
                found: arguments.len(),
            });
        }

        // The scope a `disable` of this task names covers the copies as well as
        // the body: a task that was terminated never returned, so it never
        // wrote its `output` arguments back either.
        let start = self.next();

        for (argument, connection) in definition.arguments.iter().zip(arguments) {
            if argument.direction.copies_in() {
                self.emit(Instruction::Blocking {
                    target: argument.variable(),
                    value: connection.clone(),
                });
            }
        }

        self.splice(&definition.program);

        for (argument, connection) in definition.arguments.iter().zip(arguments) {
            if argument.direction.copies_back() {
                self.emit(Instruction::Blocking {
                    target: connection.clone(),
                    value: argument.variable(),
                });
            }
        }

        let end = self.next();
        self.scopes.push(ScopeRange {
            name: name.name.clone(),
            start,
            end,
        });
        Ok(())
    }

    /// Everything a compilation step appends to, so that a step can be
    /// unwound and taken again.
    ///
    /// Only `fork` needs this: whether its branches consume time is a question
    /// about the instructions they compile to, and the honest way to ask it is
    /// to compile them.
    fn mark(&self) -> ProgramMark {
        ProgramMark {
            instructions: self.instructions.len(),
            scopes: self.scopes.len(),
            inlined: self.inlined.len(),
        }
    }

    fn rewind(&mut self, mark: ProgramMark) {
        self.instructions.truncate(mark.instructions);
        self.scopes.truncate(mark.scopes);
        self.inlined.truncate(mark.inlined);
    }

    fn emit(&mut self, instruction: Instruction) -> usize {
        self.instructions.push(instruction);
        self.instructions.len() - 1
    }

    /// The index the next emitted instruction will take.
    fn next(&self) -> usize {
        self.instructions.len()
    }

    fn patch(&mut self, site: usize, target: usize) {
        match &mut self.instructions[site] {
            Instruction::Jump(slot)
            | Instruction::JumpIfFalse { target: slot, .. }
            | Instruction::JumpIfMatch { target: slot, .. }
            | Instruction::RepeatNext { target: slot, .. } => *slot = target,
            other => unreachable!("cannot patch {:?}", other),
        }
    }
}

/// A task the design declares, compiled into the shape an enable needs.
///
/// Unlike a function, a task is not called: its body is *inlined* wherever it
/// is enabled. A task may consume time, and the only state a suspension keeps
/// is a program counter and the [`StateStore`], so a body sitting in the
/// caller's own instruction list is what makes a `#delay` inside one work at
/// all. It also gives a task the static storage the LRM asks for — the
/// argument and local variables live in the store, one set per task, shared by
/// every enable.
#[derive(Clone, Debug, PartialEq)]
pub struct TaskDefinition {
    /// The arguments, in call order, with the names the body already uses.
    pub arguments: Vec<TaskParameter>,
    /// The body, with no `Halt`: it is spliced into the middle of a block.
    pub program: Program,
}

/// One task argument: the store entry the body reads and writes, plus which
/// way it is copied at the enable.
///
/// Only the *name* is kept. A task's variables are declared in the store like
/// any others, so an assignment to one is sized and signed by the declaration
/// the way every other assignment is; carrying a second copy of the width here
/// would be a copy that could disagree.
#[derive(Clone, Debug, PartialEq)]
pub struct TaskParameter {
    pub name: String,
    pub direction: TaskDirection,
}

impl TaskParameter {
    /// The argument named as an expression, which is what a copy in or out
    /// assigns from and to.
    fn variable(&self) -> Expression {
        Expression::Identifier(Identifier::new(self.name.clone()))
    }
}

/// Every task one module declares, by the name an enable writes.
pub type TaskTable = HashMap<String, TaskDefinition>;

/// One variable in a function's frame: an argument, a body-local, or the
/// function's own name — the variable a body assigns to return a value.
#[derive(Clone, Debug, PartialEq)]
pub struct FrameVariable {
    /// The qualified name the frame holds it under: `dut.parity.a`.
    pub name: String,
    pub range: (i64, i64),
    pub signed: bool,
    /// Whether it was declared `real`, which is what makes the frame declare
    /// it as a double rather than as sixty-four bits of integer.
    pub real: bool,
}

/// A function the design declares, compiled into the shape a call needs.
///
/// A call runs the body against a **frame**: a small [`StateStore`] of its own
/// holding the result variable, the arguments, the locals, and copies of the
/// design signals the body reads. That is what lets a call be made from
/// [`eval`](crate::simulator::eval::eval), which holds a `&StateStore` and
/// could not write into the design even if a function were allowed to — and it
/// is what gives every call its own arguments, so recursion is a stack of
/// frames rather than one set of variables the calls trample on together.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    /// The variable the body assigns to return a value, named after the
    /// function itself.
    pub result: FrameVariable,
    /// The arguments, in call order.
    pub arguments: Vec<FrameVariable>,
    pub locals: Vec<FrameVariable>,
    /// The design signals the body reads, including those read by anything it
    /// calls. A call copies exactly these into its frame, so it costs the
    /// function rather than the design.
    pub reads: BTreeSet<String>,
    /// The functions this one calls, which is what `reads` is closed over.
    pub calls: BTreeSet<String>,
    pub program: Program,
}

impl FunctionDefinition {
    /// How many arguments a call has to supply.
    pub fn arity(&self) -> usize {
        self.arguments.len()
    }

    /// Runs the body against a frame of its own and hands back whatever the
    /// function's name was left holding.
    ///
    /// `arguments` are already evaluated, in the caller's scope, because that
    /// is where the expressions that produced them were written.
    pub fn call(
        &self,
        arguments: &[Register],
        store: &StateStore,
    ) -> Result<Register, SimulationError> {
        let mut frame = store.frame();
        for name in &self.reads {
            // A name the design does not have is left out rather than invented:
            // the body reading it is then the same `UnknownIdentifier` it would
            // be anywhere else.
            if let Some(signal) = store.get_signal(name) {
                frame.set_ranged(name.clone(), signal.register().clone(), signal.range());
            }
        }

        for variable in std::iter::once(&self.result)
            .chain(&self.arguments)
            .chain(&self.locals)
        {
            // A `real` frame variable starts at `0.0` like any other real; the
            // rest start unknown. It is the same split `declare` makes in the
            // design's own store.
            if variable.real {
                frame.declare_real(variable.name.clone());
            } else {
                frame.declare_signed(variable.name.clone(), variable.range, variable.signed);
            }
        }
        for (variable, value) in self.arguments.iter().zip(arguments) {
            let target = ResolvedTarget::Whole(variable.name.clone());
            drive_resolved(&mut frame, &target, value)?;
        }

        // Nothing in a function body may print, and nothing in one may
        // suspend: both are rejected when the function is elaborated, which is
        // why the context here is a fresh one nobody reads and a suspension is
        // an error rather than a resume point.
        let mut tasks = TaskContext::new();
        match resume(&self.program, 0, &mut frame, &mut tasks)? {
            Resume::Halted { .. } => {}
            Resume::Suspended { .. } => return Err(FUNCTION_DELAY_UNSUPPORTED),
            Resume::Waiting { .. } => return Err(FUNCTION_EVENT_UNSUPPORTED),
            Resume::Disabled { scope, .. } => return Err(SimulationError::UnknownScope(scope)),
            // A frame has no driver behind it to run threads on. A `fork` of
            // branches that consume no time never gets here — it is compiled
            // as a plain block.
            Resume::Forked { .. } | Resume::BranchDone { .. } => {
                return Err(FORK_TIMING_UNSUPPORTED)
            }
        }

        frame
            .get(&self.result.name)
            .cloned()
            .ok_or_else(|| SimulationError::UnknownSignal(self.result.name.clone()))
    }
}

/// Runs `program` from `pc` until the block ends or hits a delay.
///
/// Blocking writes land in `store` as they execute; non-blocking ones come back
/// in the returned `pending` list, whether the block finished or suspended.
/// Whatever the block prints goes into `tasks`, which also carries the time
/// `$time` reports and takes the mark a `$finish` leaves behind.
pub fn resume(
    program: &Program,
    pc: usize,
    store: &mut StateStore,
    tasks: &mut TaskContext,
) -> Result<Resume, SimulationError> {
    let mut pc = pc;
    let mut pending = Vec::new();
    // The subject of the `case` currently being matched. Comparisons always sit
    // between the `CaseSubject` that fills this and the arm bodies, and a delay
    // can only appear inside a body, so one slot is enough even when `case`
    // statements nest.
    let mut subject: Option<Register> = None;
    // A loop with no delay in it never returns from here on its own, so the
    // budget is what turns `forever a = 1;` into an error instead of a hang.
    let mut steps = 0usize;

    loop {
        steps += 1;
        if steps > MAX_INSTRUCTIONS {
            return Err(SimulationError::NoConvergence { passes: steps });
        }

        let Some(instruction) = program.instructions.get(pc) else {
            return Ok(Resume::Halted { pending });
        };

        match instruction {
            Instruction::Blocking { target, value } => {
                // The target is resolved before the right hand side is
                // evaluated, so a bad target is reported ahead of a bad value —
                // and so that the target's width is known in time to size the
                // right hand side, which is what makes an assignment
                // context-determined.
                let target = resolve_target(store, target)?;
                let value = eval_sized(value, store, target.width(store))?;
                drive_resolved(store, &target, &value)?;
                pc += 1;
            }
            Instruction::NonBlocking { target, value } => {
                let target = resolve_target(store, target)?;
                let value = eval_sized(value, store, target.width(store))?;
                pending.push(PendingUpdate::new(target, value));
                pc += 1;
            }
            Instruction::ScheduleWrite {
                target,
                value,
                delay,
            } => {
                // Read now, land later, and carry on — the block is *not*
                // suspended, so a `#2` after this one measures from here
                // rather than from when the write happens.
                let target = resolve_target(store, target)?;
                let value = eval_sized(value, store, target.width(store))?;
                let at = store.time() + delay.ticks(store)?;
                pending.push(PendingUpdate::scheduled(target, value, at));
                pc += 1;
            }
            Instruction::Assign { target, value } => {
                install_drive(store, target, value, DriveLevel::Assign)?;
                pc += 1;
            }
            Instruction::Force { target, value } => {
                install_drive(store, target, value, DriveLevel::Force)?;
                pc += 1;
            }
            Instruction::Deassign(target) => {
                deassign_drive(store, target)?;
                pc += 1;
            }
            Instruction::Release(target) => {
                release_drive(store, target)?;
                pc += 1;
            }
            Instruction::JumpIfFalse { condition, target } => {
                let condition = eval(condition, store)?;
                pc = if is_true(&condition) { pc + 1 } else { *target };
            }
            Instruction::Jump(target) => pc = *target,
            Instruction::CaseSubject(expression) => {
                subject = Some(eval(expression, store)?);
                pc += 1;
            }
            Instruction::JumpIfMatch {
                label,
                target,
                kind,
            } => {
                let held = subject
                    .as_ref()
                    .ok_or(SimulationError::Unsupported("a case arm without a subject"))?;
                let label = eval(label, store)?;
                pc = if case_matches(held, &label, *kind) {
                    *target
                } else {
                    pc + 1
                };
            }
            // The count is read once, here, and never again: everything
            // after this reads the counter, not the expression.
            Instruction::RepeatInit { counter, count } => {
                let count = eval(count, store)?;
                // A count that is `x` or `z` runs the body zero times.
                let count = count.to_u128().unwrap_or(0);
                store.set(
                    counter.clone(),
                    Register::from_u128(count, REPEAT_COUNTER_WIDTH),
                );
                pc += 1;
            }
            Instruction::RepeatNext { counter, target } => {
                let remaining = store
                    .get(counter)
                    .and_then(|register| register.to_u128())
                    .unwrap_or(0);
                if remaining == 0 {
                    pc = *target;
                } else {
                    store.set(
                        counter.clone(),
                        Register::from_u128(remaining - 1, REPEAT_COUNTER_WIDTH),
                    );
                    pc += 1;
                }
            }
            Instruction::Task(call) => {
                tasks.run(call, store)?;
                // `$finish` ends the simulation, so the rest of the block is
                // not run — and the driver stops advancing time.
                if tasks.finished() {
                    return Ok(Resume::Halted { pending });
                }
                pc += 1;
            }
            // The delay is worked out here rather than where the block was
            // compiled, which is what lets `#n` name a variable the design
            // moves as it runs.
            Instruction::Delay(delay) => {
                return Ok(Resume::Suspended {
                    pc: pc + 1,
                    delay: delay.ticks(store)?,
                    pending,
                })
            }
            // A true condition is not a wait at all, which is why the resume
            // point is this instruction rather than the next one: coming back
            // here re-evaluates it, and that is the whole of the retry.
            Instruction::Wait(condition) => {
                let condition = eval(condition, store)?;
                if is_true(&condition) {
                    pc += 1;
                    continue;
                }
                return Ok(Resume::Waiting {
                    pc,
                    wait: WaitReason::Condition,
                    pending,
                });
            }
            Instruction::EventWait(control) => {
                return Ok(Resume::Waiting {
                    pc: pc + 1,
                    wait: WaitReason::Event(control.clone()),
                    pending,
                })
            }
            Instruction::Hold {
                slot,
                target,
                value,
            } => {
                let target = resolve_target(store, target)?;
                let value = eval_sized(value, store, target.width(store))?;
                store.hold(slot.clone(), value);
                pc += 1;
            }
            Instruction::WriteHeld { slot, target } => {
                let value = store
                    .take_hold(slot)
                    .ok_or_else(|| SimulationError::UnknownSignal(slot.clone()))?;
                let target = resolve_target(store, target)?;
                drive_resolved(store, &target, &value)?;
                pc += 1;
            }
            // Terminating a scope the block is already inside is a jump to
            // where that scope ends — which is exactly "execution continues
            // with the statement following the block". Terminating one it is
            // not inside can only be done by whoever holds the other block's
            // resume point, so it goes back to the driver.
            Instruction::Disable(scope) => match program.scope_end_containing(scope, pc) {
                Some(end) => pc = end,
                None => {
                    return Ok(Resume::Disabled {
                        scope: scope.clone(),
                        pc: pc + 1,
                        pending,
                    })
                }
            },
            // Spawning is the driver's to do — it owns the queue the threads
            // go on — so this hands the branch entry points out and parks the
            // block at the join.
            Instruction::Fork { branches, join } => {
                return Ok(Resume::Forked {
                    branches: branches.clone(),
                    pc: *join,
                    pending,
                })
            }
            Instruction::JoinBranch => return Ok(Resume::BranchDone { pending }),
            Instruction::Halt => return Ok(Resume::Halted { pending }),
        }
    }
}

/// The scope a named block opens, which is what its variables are spelled
/// under: `block_id.` inside nothing, `load.loop.` for a block inside a task.
///
/// A Verilog identifier cannot contain a `.`, so one of these can never
/// collide with a signal the design declares — the same shape a task's
/// variables take.
pub fn block_scope(scope: &str, name: &str) -> String {
    format!("{}{}.", scope, name)
}

/// The scope `name` refers to when it is written inside `scope`.
///
/// `disable` names a block by its bare label, and the label it means is the
/// innermost enclosing one that matches — `disable wait_loop` inside task `t`
/// is `t.wait_loop`, and the same word written at the top of a module is
/// `wait_loop`. A name matching none of the enclosing scopes is left as it
/// stands: it belongs to another block, and only the driver can find it.
fn enclosing_scope(scope: &str, name: &str) -> String {
    let mut rest = scope.trim_end_matches('.');
    while !rest.is_empty() {
        let (head, last) = match rest.rfind('.') {
            Some(at) => (&rest[..at], &rest[at + 1..]),
            None => ("", rest),
        };
        if last == name {
            return rest.to_string();
        }
        rest = head;
    }
    name.to_string()
}

/// The explicit sensitivity list an `@*` in front of `body` stands for: every
/// signal the statement reads, each of them level sensitive.
fn implicit_control(body: &[ProceduralStatements]) -> Result<EventControl, SimulationError> {
    let events: Vec<Event> = signals_read(body)
        .into_iter()
        .map(|name| {
            Event::new(
                EventTriggers::EitherEdge,
                Expression::Identifier(Identifier::new(name)),
            )
        })
        .collect();
    if events.is_empty() {
        return Err(EMPTY_IMPLICIT_EVENT_UNSUPPORTED);
    }
    Ok(EventControl::Events(events))
}

/// Whether an instruction can hand control back before the block is done,
/// which is what a `fork` branch may not do.
fn instruction_suspends(instruction: &Instruction) -> bool {
    matches!(
        instruction,
        Instruction::Delay(_)
            | Instruction::Wait(_)
            | Instruction::EventWait(_)
            // A `fork` of branches that consume time gives control back to the
            // driver, so a `fork` written inside another one makes the outer
            // one time-consuming as well.
            | Instruction::Fork { .. }
    )
}

/// Whether a `case` item matches the subject.
///
/// A plain `case` compares with `==` semantics, so an `x` or `z` on either side
/// makes the comparison unknown, which is not a match. `casez` and `casex`
/// instead read those bits as don't-cares — on *either* side, so a subject bit
/// is as much a wildcard as a label bit — and compare the rest for identity,
/// which is what lets `casez` still tell an `x` apart from a `0`.
fn case_matches(subject: &Register, label: &Register, kind: CaseKind) -> bool {
    match kind {
        CaseKind::Exact => {
            if subject.has_unknown() || label.has_unknown() {
                return false;
            }
            let width = subject.width().max(label.width());
            subject.resize(width) == label.resize(width)
        }
        CaseKind::WildcardZ => subject.matches_ignoring_z(label),
        CaseKind::WildcardXz => subject.matches_ignoring_xz(label),
    }
}

/// Whether a register used as a condition is true. Verilog calls a condition
/// true only when it is a *known* non-zero value: zero, `x` and `z` all take
/// the else branch. An `x` condition is not an "unknown branch" — it is false.
fn is_true(register: &Register) -> bool {
    register.has_one()
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parsers::behavior::parse_block;
    use crate::parsers::modules::parse_module_declaration;
    use crate::simulator::exec::commit_updates;
    use crate::simulator::runner::Simulator;

    /// A store holding each named signal at the width of its binary literal,
    /// declared over `(width - 1, 0)`.
    fn store_with(signals: &[(&str, &str)]) -> StateStore {
        let mut store = StateStore::new();
        for (name, bits) in signals {
            let register = Register::from_binary(bits);
            let range = (register.width() as i64 - 1, 0);
            store.set_ranged(*name, register, range);
        }
        store
    }

    fn compile(source: &str) -> Program {
        let (remaining, statements) = parse_block(source).expect("block should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);
        Program::compile(&statements, &TaskTable::new()).expect("block should compile")
    }

    fn value(store: &StateStore, name: &str) -> String {
        store.get(name).expect("signal should exist").to_binary()
    }

    /// Runs from `pc`, commits whatever the step queued, and reports where the
    /// block stopped: `Some(pc)` when it suspended, `None` when it halted.
    fn step(program: &Program, pc: usize, store: &mut StateStore) -> Option<(usize, i64)> {
        match resume(program, pc, store, &mut TaskContext::new()).expect("resume should succeed") {
            Resume::Halted { pending } => {
                commit_updates(pending, store).unwrap();
                None
            }
            Resume::Suspended { pc, delay, pending } => {
                commit_updates(pending, store).unwrap();
                Some((pc, delay))
            }
            Resume::Waiting { pending, .. }
            | Resume::Disabled { pending, .. }
            | Resume::Forked { pending, .. }
            | Resume::BranchDone { pending } => {
                commit_updates(pending, store).unwrap();
                None
            }
        }
    }

    #[test]
    fn test_empty_block_compiles_to_a_bare_halt() {
        let program = Program::compile(&[], &TaskTable::new()).unwrap();
        assert!(program.is_empty());
        assert_eq!(program.instructions(), &[Instruction::Halt]);

        assert!(!compile("begin a = b; end").is_empty());
    }

    #[test]
    fn test_top_level_delay_suspends_and_resumes() {
        let program = compile("begin a = 4'b0001; #7; a = 4'b0010; end");
        let mut store = store_with(&[("a", "0000")]);

        let (pc, delay) = step(&program, 0, &mut store).expect("should suspend");
        assert_eq!(delay, 7);
        assert_eq!(value(&store, "a"), "0001");
        // The delay is the second instruction, so we come back at the third.
        assert_eq!(pc, 2);
        assert!(matches!(
            program.instructions()[pc],
            Instruction::Blocking { .. }
        ));

        assert!(step(&program, pc, &mut store).is_none());
        assert_eq!(value(&store, "a"), "0010");
    }

    #[test]
    fn test_resume_does_not_rerun_statements_before_the_delay() {
        // `count` is incremented once before the delay. Resuming must not run
        // that increment a second time.
        let program = compile("begin count = count + 1; #5; done = 1'b1; end");
        let mut store = store_with(&[("count", "0000"), ("done", "0")]);

        let (pc, _) = step(&program, 0, &mut store).expect("should suspend");
        assert_eq!(value(&store, "count"), "0001");

        assert!(step(&program, pc, &mut store).is_none());
        assert_eq!(value(&store, "count"), "0001");
        assert_eq!(value(&store, "done"), "1");
    }

    #[test]
    fn test_delay_nested_in_an_if_body_suspends_and_resumes() {
        // The resume point sits inside the `then` arm — the case a statement
        // index cannot express.
        let program = compile(
            r#"begin
                if (sel) begin
                    a = 4'b0001;
                    #9;
                    a = 4'b0011;
                end else begin
                    a = 4'b1000;
                end
                b = 4'b0111;
            end"#,
        );
        let mut store = store_with(&[("sel", "1"), ("a", "0000"), ("b", "0000")]);

        let (pc, delay) = step(&program, 0, &mut store).expect("should suspend");
        assert_eq!(delay, 9);
        assert_eq!(value(&store, "a"), "0001");
        assert_eq!(value(&store, "b"), "0000");

        assert!(step(&program, pc, &mut store).is_none());
        // Resuming finishes the arm and then the statement after the `if`,
        // without re-testing the condition or touching the else arm.
        assert_eq!(value(&store, "a"), "0011");
        assert_eq!(value(&store, "b"), "0111");
    }

    #[test]
    fn test_else_arm_with_a_delay_suspends_in_the_else_arm() {
        let program = compile(
            r#"begin
                if (sel) a = 4'b0001;
                else begin
                    #4;
                    a = 4'b1000;
                end
            end"#,
        );

        // The condition is false, so the else arm runs and suspends.
        let mut store = store_with(&[("sel", "0"), ("a", "0000")]);
        let (pc, delay) = step(&program, 0, &mut store).expect("should suspend");
        assert_eq!(delay, 4);
        assert_eq!(value(&store, "a"), "0000");
        assert!(step(&program, pc, &mut store).is_none());
        assert_eq!(value(&store, "a"), "1000");

        // A true condition takes the then arm and never reaches the delay.
        let mut store = store_with(&[("sel", "1"), ("a", "0000")]);
        assert!(step(&program, 0, &mut store).is_none());
        assert_eq!(value(&store, "a"), "0001");
    }

    #[test]
    fn test_delay_inside_a_case_item_body_suspends_and_resumes() {
        let program = compile(
            r#"begin
                case (sel)
                    2'b00: q = 4'b0001;
                    2'b10: begin
                        q = 4'b0010;
                        #3;
                        q = 4'b0110;
                    end
                    default: q = 4'b1111;
                endcase
                done = 1'b1;
            end"#,
        );

        let mut store = store_with(&[("sel", "10"), ("q", "0000"), ("done", "0")]);
        let (pc, delay) = step(&program, 0, &mut store).expect("should suspend");
        assert_eq!(delay, 3);
        assert_eq!(value(&store, "q"), "0010");
        assert_eq!(value(&store, "done"), "0");

        assert!(step(&program, pc, &mut store).is_none());
        assert_eq!(value(&store, "q"), "0110");
        assert_eq!(value(&store, "done"), "1");

        // An arm without a delay still runs straight through to the end.
        let mut store = store_with(&[("sel", "00"), ("q", "0000"), ("done", "0")]);
        assert!(step(&program, 0, &mut store).is_none());
        assert_eq!(value(&store, "q"), "0001");
        assert_eq!(value(&store, "done"), "1");
    }

    #[test]
    fn test_several_delays_suspend_in_turn() {
        let program = compile("begin #10 a = 1'b1; #10 a = 1'b0; end");
        let mut store = store_with(&[("a", "0")]);

        let (pc, delay) = step(&program, 0, &mut store).expect("first suspend");
        assert_eq!(delay, 10);
        assert_eq!(value(&store, "a"), "0");

        let (pc, delay) = step(&program, pc, &mut store).expect("second suspend");
        assert_eq!(delay, 10);
        assert_eq!(value(&store, "a"), "1");

        assert!(step(&program, pc, &mut store).is_none());
        assert_eq!(value(&store, "a"), "0");
    }

    #[test]
    fn test_pre_delay_compiles_to_a_delay_before_the_assignment() {
        let program = compile("begin #50 clk = 1'b1; end");
        assert_eq!(
            program.instructions()[0],
            Instruction::Delay(Delay::new(50))
        );
        assert!(matches!(
            program.instructions()[1],
            Instruction::Blocking { .. }
        ));
        assert_eq!(program.instructions()[2], Instruction::Halt);
    }

    /// A delay in front of a `begin`…`end` block waits once, then runs the
    /// whole block — the same instructions a bare `#50;` in front of it emits.
    #[test]
    fn test_a_delayed_block_compiles_to_one_delay_then_its_body() {
        let program = compile("begin #50 begin a = 1'b1; b = 1'b0; end end");
        assert_eq!(
            program.instructions()[0],
            Instruction::Delay(Delay::new(50))
        );
        assert!(matches!(
            program.instructions()[1],
            Instruction::Blocking { .. }
        ));
        assert!(matches!(
            program.instructions()[2],
            Instruction::Blocking { .. }
        ));
        assert_eq!(program.instructions()[3], Instruction::Halt);
    }

    /// A delay prefixing a statement *inside* a case arm is the resume point a
    /// statement index cannot name: it is neither the arm nor the case.
    #[test]
    fn test_a_delayed_statement_inside_a_case_arm_suspends_and_resumes() {
        let program = compile(
            r#"begin
                case (sel)
                    2'b01: #6 a = 4'b0001;
                    default: a = 4'b1000;
                endcase
                b = 4'b0111;
            end"#,
        );
        let mut store = store_with(&[("sel", "01"), ("a", "0000"), ("b", "0000")]);

        let (pc, delay) = step(&program, 0, &mut store).expect("should suspend");
        assert_eq!(delay, 6);
        // The arm has not run yet, and neither has the statement after the
        // `case`.
        assert_eq!(value(&store, "a"), "0000");
        assert_eq!(value(&store, "b"), "0000");

        assert!(step(&program, pc, &mut store).is_none());
        assert_eq!(value(&store, "a"), "0001");
        assert_eq!(value(&store, "b"), "0111");
    }

    /// The same, in an `if` arm, and with the delay in front of a nested
    /// block rather than a single assignment.
    #[test]
    fn test_a_delayed_block_inside_an_if_arm_suspends_and_resumes() {
        let program = compile(
            r#"begin
                if (sel) #3 begin
                    a = 4'b0001;
                    b = 4'b0010;
                end
                c = 4'b0100;
            end"#,
        );
        let mut store = store_with(&[("sel", "1"), ("a", "0000"), ("b", "0000"), ("c", "0000")]);

        let (pc, delay) = step(&program, 0, &mut store).expect("should suspend");
        assert_eq!(delay, 3);
        assert_eq!(value(&store, "a"), "0000");

        assert!(step(&program, pc, &mut store).is_none());
        assert_eq!(value(&store, "a"), "0001");
        assert_eq!(value(&store, "b"), "0010");
        assert_eq!(value(&store, "c"), "0100");
    }

    #[test]
    fn test_intra_assignment_delay_holds_the_value_it_read() {
        let program = compile("begin a = #5 b; end");
        let mut store = store_with(&[("a", "0000"), ("b", "1111")]);

        let (pc, delay) = step(&program, 0, &mut store).expect("should suspend");
        assert_eq!(delay, 5);
        assert_eq!(value(&store, "a"), "0000");

        // What lands is what `b` held when the statement ran, not what it
        // holds when the delay expires — that is the whole of what makes the
        // control an intra-assignment one.
        store.set("b", Register::from_binary("0000"));
        assert!(step(&program, pc, &mut store).is_none());
        assert_eq!(value(&store, "a"), "1111");
    }

    /// `a <= #5 b;` compiles to a single `ScheduleWrite`, not the `Hold` /
    /// wait / `WriteHeld` triple the blocking form needs — because it does not
    /// suspend the block.
    #[test]
    fn test_non_blocking_delay_compiles_to_one_scheduled_write() {
        let (_, statements) = parse_block("begin a <= #5 b; end").unwrap();
        let program = Program::compile(&statements, &TaskTable::new()).expect("should compile");
        assert!(
            matches!(
                program.instructions().first(),
                Some(Instruction::ScheduleWrite { .. })
            ),
            "expected a scheduled write, got {:?}",
            program.instructions().first()
        );
    }

    /// An **event** control on a non-blocking assignment stays a named error:
    /// it needs a watch that outlives the block, which nothing here has.
    #[test]
    fn test_non_blocking_event_control_is_rejected() {
        let (_, statements) = parse_block("begin a <= @(posedge clk) b; end").unwrap();
        assert_eq!(
            Program::compile(&statements, &TaskTable::new()),
            Err(NONBLOCKING_TIMING_UNSUPPORTED)
        );
    }

    #[test]
    fn test_non_blocking_updates_survive_a_suspension() {
        // The queued writes come back with the suspension, not after the block
        // has finished, so a caller can commit them at the right time.
        let program = compile("begin a <= b; #5; b <= a; end");
        let mut store = store_with(&[("a", "1010"), ("b", "0101")]);

        let Resume::Suspended { pc, pending, .. } =
            resume(&program, 0, &mut store, &mut TaskContext::new()).unwrap()
        else {
            panic!("should suspend");
        };
        assert_eq!(pending.len(), 1);
        commit_updates(pending, &mut store).unwrap();
        assert_eq!(value(&store, "a"), "0101");

        assert!(step(&program, pc, &mut store).is_none());
        assert_eq!(value(&store, "b"), "0101");
    }

    #[test]
    fn test_a_delay_of_zero_still_suspends() {
        // `#0` reschedules for the same timestamp rather than doing nothing.
        let program = compile("begin #0; a = 1'b1; end");
        let mut store = store_with(&[("a", "0")]);

        let (pc, delay) = step(&program, 0, &mut store).expect("should suspend");
        assert_eq!(delay, 0);
        assert!(step(&program, pc, &mut store).is_none());
        assert_eq!(value(&store, "a"), "1");
    }

    #[test]
    fn test_only_the_first_default_arm_is_compiled() {
        let program = compile(
            r#"begin
                case (sel)
                    default: q = 4'b0001;
                    2'b10: q = 4'b0010;
                    default: q = 4'b1000;
                endcase
            end"#,
        );

        let mut store = store_with(&[("sel", "11"), ("q", "0000")]);
        assert!(step(&program, 0, &mut store).is_none());
        assert_eq!(value(&store, "q"), "0001");
    }

    /// Runs `program` from the start with `sel` as the subject and reports
    /// what the arm that fired wrote to `q`.
    fn case_arm_for(program: &Program, sel: &str) -> String {
        let mut store = store_with(&[("sel", sel), ("q", "0000")]);
        assert!(step(program, 0, &mut store).is_none());
        value(&store, "q")
    }

    #[test]
    fn test_the_case_keyword_decides_the_comparison_every_arm_uses() {
        for (keyword, expected) in [
            ("case", CaseKind::Exact),
            ("casez", CaseKind::WildcardZ),
            ("casex", CaseKind::WildcardXz),
        ] {
            let program = compile(&format!(
                "begin {keyword} (sel) 2'b01: q = 4'b0001; endcase end"
            ));
            let kinds: Vec<CaseKind> = program
                .instructions()
                .iter()
                .filter_map(|instruction| match instruction {
                    Instruction::JumpIfMatch { kind, .. } => Some(*kind),
                    _ => None,
                })
                .collect();
            assert_eq!(kinds, vec![expected], "{keyword}");
        }
    }

    #[test]
    fn test_casez_matches_every_subject_its_wildcard_label_covers() {
        let program = compile(
            r#"begin
                casez (sel)
                    2'b0?: q = 4'b0001;
                    2'b1?: q = 4'b0010;
                    default: q = 4'b1000;
                endcase
            end"#,
        );

        assert_eq!(case_arm_for(&program, "00"), "0001");
        assert_eq!(case_arm_for(&program, "01"), "0001");
        assert_eq!(case_arm_for(&program, "10"), "0010");
        assert_eq!(case_arm_for(&program, "11"), "0010");
        // An `x` is not a don't-care under `casez`, so the known bit it sits
        // beside cannot carry the match on its own.
        assert_eq!(case_arm_for(&program, "x0"), "1000");
    }

    #[test]
    fn test_casez_reads_a_wildcard_in_the_subject_too() {
        let program = compile(
            r#"begin
                casez (sel)
                    2'b10: q = 4'b0001;
                    2'b11: q = 4'b0010;
                    default: q = 4'b1000;
                endcase
            end"#,
        );

        // The subject's low bit is the don't-care here, so both labels match
        // and the first in source order wins.
        assert_eq!(case_arm_for(&program, "1z"), "0001");
        // The bit that is known still has to agree.
        assert_eq!(case_arm_for(&program, "0z"), "1000");
    }

    #[test]
    fn test_casex_matches_an_x_on_either_side() {
        let program = compile(
            r#"begin
                casex (sel)
                    2'b1x: q = 4'b0001;
                    default: q = 4'b1000;
                endcase
            end"#,
        );

        assert_eq!(case_arm_for(&program, "10"), "0001");
        assert_eq!(case_arm_for(&program, "11"), "0001");
        // An `x` in the subject is a don't-care as well, and `casex` takes a
        // `z` for one too.
        assert_eq!(case_arm_for(&program, "1x"), "0001");
        assert_eq!(case_arm_for(&program, "1z"), "0001");
        assert_eq!(case_arm_for(&program, "x0"), "0001");
        // The one known bit of the label still has to agree.
        assert_eq!(case_arm_for(&program, "0x"), "1000");
    }

    #[test]
    fn test_a_plain_case_still_does_not_match_an_unknown_bit() {
        let program = compile(
            r#"begin
                case (sel)
                    2'b1z: q = 4'b0001;
                    2'b10: q = 4'b0010;
                    default: q = 4'b1000;
                endcase
            end"#,
        );

        // An unknown bit on either side makes the comparison unknown, which is
        // not a match — so both a `z` label and a `z` subject fall through.
        assert_eq!(case_arm_for(&program, "1z"), "1000");
        assert_eq!(case_arm_for(&program, "1x"), "1000");
        assert_eq!(case_arm_for(&program, "10"), "0010");
    }

    #[test]
    fn test_running_off_the_end_of_the_program_halts() {
        let program = compile("begin a = 1'b1; end");
        let mut store = store_with(&[("a", "0")]);

        assert_eq!(
            resume(
                &program,
                program.instructions().len(),
                &mut store,
                &mut TaskContext::new()
            ),
            Ok(Resume::Halted {
                pending: Vec::new()
            })
        );
        assert_eq!(value(&store, "a"), "0");
    }

    #[test]
    fn test_a_for_loop_accumulates_a_known_total() {
        // 0 + 1 + … + 9 = 45.
        let program =
            compile("begin total = 0; for (i = 0; i < 10; i = i + 1) total = total + i; end");
        let mut store = store_with(&[("total", "00000000"), ("i", "00000000")]);

        assert!(step(&program, 0, &mut store).is_none());
        assert_eq!(store.get("total").unwrap().to_u128(), Some(45));
        // The loop leaves the counter at the value that failed the test.
        assert_eq!(store.get("i").unwrap().to_u128(), Some(10));
    }

    #[test]
    fn test_a_while_loop_terminates() {
        let program = compile("begin while (i < 5) begin total = total + i; i = i + 1; end end");
        let mut store = store_with(&[("total", "00000000"), ("i", "00000000")]);

        assert!(step(&program, 0, &mut store).is_none());
        assert_eq!(store.get("total").unwrap().to_u128(), Some(10));
        assert_eq!(store.get("i").unwrap().to_u128(), Some(5));
    }

    #[test]
    fn test_a_while_loop_whose_condition_is_false_never_runs_its_body() {
        let program = compile("begin while (go) ran = 1'b1; end");
        let mut store = store_with(&[("go", "0"), ("ran", "0")]);

        assert!(step(&program, 0, &mut store).is_none());
        assert_eq!(value(&store, "ran"), "0");
    }

    /// An `x` condition is false, the rule `if` already uses — not an
    /// "unknown branch" that runs the body anyway.
    #[test]
    fn test_an_unknown_loop_condition_is_false() {
        let program = compile("begin while (go) ran = 1'b1; end");
        let mut store = store_with(&[("ran", "0")]);
        store.set("go", Register::unknown(1));

        assert!(step(&program, 0, &mut store).is_none());
        assert_eq!(value(&store, "ran"), "0");
    }

    #[test]
    fn test_repeat_runs_its_body_exactly_four_times() {
        let program = compile("begin repeat (4) count = count + 1; end");
        let mut store = store_with(&[("count", "00000000")]);

        assert!(step(&program, 0, &mut store).is_none());
        assert_eq!(store.get("count").unwrap().to_u128(), Some(4));
    }

    /// The count is read once, on entry. Under a naive compilation that
    /// re-evaluated it every iteration, a body walking `n` down would stop
    /// where the two met — after three iterations rather than six.
    #[test]
    fn test_repeat_evaluates_its_count_once() {
        let program = compile("begin repeat (n) begin n = n - 1; count = count + 1; end end");
        let mut store = store_with(&[("n", "0110"), ("count", "00000000")]);

        assert!(step(&program, 0, &mut store).is_none());
        assert_eq!(store.get("count").unwrap().to_u128(), Some(6));
        assert_eq!(store.get("n").unwrap().to_u128(), Some(0));
    }

    /// A `repeat` count that is `x` or `z` runs the body no times at all.
    #[test]
    fn test_an_unknown_repeat_count_runs_no_iterations() {
        let program = compile("begin repeat (n) count = count + 1; end");
        let mut store = store_with(&[("count", "00000000")]);
        store.set("n", Register::unknown(4));

        assert!(step(&program, 0, &mut store).is_none());
        assert_eq!(store.get("count").unwrap().to_u128(), Some(0));
    }

    #[test]
    fn test_nested_repeats_each_keep_their_own_counter() {
        let program = compile("begin repeat (3) repeat (5) count = count + 1; end");
        let mut store = store_with(&[("count", "00000000")]);

        assert!(step(&program, 0, &mut store).is_none());
        assert_eq!(store.get("count").unwrap().to_u128(), Some(15));
    }

    /// A `repeat` counter is a signal like any other as far as flattening is
    /// concerned, so two instances of one module count separately rather than
    /// sharing an entry.
    #[test]
    fn test_a_repeat_counter_is_qualified_by_flattening() {
        let mut program = compile("begin repeat (2) a = 1'b1; end");
        program.rename(&|name| format!("dut.{}", name));

        let counters: Vec<&str> = program
            .instructions()
            .iter()
            .filter_map(|instruction| match instruction {
                Instruction::RepeatInit { counter, .. }
                | Instruction::RepeatNext { counter, .. } => Some(counter.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(counters, vec!["dut.$repeat$0", "dut.$repeat$0"]);
    }

    /// A loop body is compiled inline, so a `#delay` in one suspends and
    /// resumes by program counter exactly as a top-level delay does — and the
    /// iteration it stopped in is not run a second time.
    #[test]
    fn test_a_delay_inside_a_loop_body_suspends_and_resumes() {
        let program = compile(
            r#"begin
                i = 0;
                while (i < 3) begin
                    #5;
                    i = i + 1;
                end
                done = 1'b1;
            end"#,
        );
        let mut store = store_with(&[("i", "0000"), ("done", "0")]);

        let mut pc = 0;
        for expected in 0..3 {
            let (next, delay) = step(&program, pc, &mut store).expect("should suspend");
            assert_eq!(delay, 5);
            assert_eq!(store.get("i").unwrap().to_u128(), Some(expected));
            pc = next;
        }

        assert!(step(&program, pc, &mut store).is_none());
        assert_eq!(store.get("i").unwrap().to_u128(), Some(3));
        assert_eq!(value(&store, "done"), "1");
    }

    /// A `repeat` whose body suspends keeps its count across the suspension:
    /// the counter lives in the store, which with the program counter is all
    /// the state a resumption has.
    #[test]
    fn test_a_repeat_counter_survives_a_suspension() {
        let program = compile("begin repeat (3) begin #5; count = count + 1; end end");
        let mut store = store_with(&[("count", "0000")]);

        let mut pc = 0;
        for _ in 0..3 {
            let (next, delay) = step(&program, pc, &mut store).expect("should suspend");
            assert_eq!(delay, 5);
            pc = next;
        }

        assert!(step(&program, pc, &mut store).is_none());
        assert_eq!(store.get("count").unwrap().to_u128(), Some(3));
    }

    /// A loop with no delay in it never returns from `resume`, so neither of
    /// the runner's limits can see it. The instruction budget is what turns it
    /// into an error rather than a hang.
    #[test]
    fn test_a_zero_delay_loop_is_an_error_rather_than_a_hang() {
        // An empty body, so the loop is a bare back-jump: all four forms share
        // this path, and one that did work per iteration would spend the whole
        // budget doing it before the test could finish.
        let program = compile("begin forever begin end end");
        let mut store = store_with(&[("a", "1")]);

        assert!(matches!(
            resume(&program, 0, &mut store, &mut TaskContext::new()),
            Err(SimulationError::NoConvergence { .. })
        ));
    }

    /// A `forever` with a delay in it is bounded by time rather than by the
    /// budget: every iteration gives control back, and `advance` decides how
    /// many of them run.
    #[test]
    fn test_a_forever_with_a_delay_runs_once_per_delay() {
        let source = r#"
            module ticker;
                reg [7:0] ticks;
                initial ticks = 0;
                initial forever begin
                    #5 ticks = ticks + 1;
                end
            endmodule"#;
        let (remaining, module) = parse_module_declaration(source).expect("module should parse");
        assert!(remaining.trim().is_empty(), "unparsed input: {}", remaining);

        let mut simulator = Simulator::new(module);
        simulator.setup().unwrap();

        // Ten iterations land at t = 5, 10, … 50; the eleventh is due at 55.
        simulator.advance(52).unwrap();
        assert_eq!(simulator.get("ticks").unwrap().to_u128(), Some(10));

        simulator.advance(10).unwrap();
        assert_eq!(simulator.get("ticks").unwrap().to_u128(), Some(12));
    }

    /// A named block's variables are renamed into the scope its name opens, so
    /// the design signal spelled the same way is a different variable.
    #[test]
    fn test_a_named_block_local_is_renamed_into_its_scope() {
        let program = compile("begin : blk reg [3:0] tmp; tmp = a; b = tmp; end");

        assert_eq!(
            program.instructions(),
            &[
                Instruction::Blocking {
                    target: Expression::Identifier(Identifier::new("blk.tmp".to_string())),
                    value: Expression::Identifier(Identifier::new("a".to_string())),
                },
                Instruction::Blocking {
                    target: Expression::Identifier(Identifier::new("b".to_string())),
                    value: Expression::Identifier(Identifier::new("blk.tmp".to_string())),
                },
                Instruction::Halt,
            ]
        );
    }

    /// A `wait` hands control back when its condition is false and falls
    /// straight through when it is true, which is what makes it resumable by a
    /// value rather than by the clock.
    #[test]
    fn test_wait_gives_control_back_until_its_condition_is_true() {
        let program = compile("begin wait (flag) a = 4'b0001; end");
        let mut store = store_with(&[("flag", "0"), ("a", "0000")]);

        let Resume::Waiting { pc, wait, .. } =
            resume(&program, 0, &mut store, &mut TaskContext::new()).expect("should wait")
        else {
            panic!("a false condition should not run the block to the end");
        };
        assert_eq!(wait, WaitReason::Condition);
        assert_eq!(value(&store, "a"), "0000");

        store.set("flag", Register::from_binary("1"));
        assert!(step(&program, pc, &mut store).is_none());
        assert_eq!(value(&store, "a"), "0001");
    }
}

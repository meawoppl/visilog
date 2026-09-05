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

use crate::parsers::assignment::{ProceduralAssignment, ProceduralAssignmentType};
use crate::parsers::behavior::{
    CaseKind, CaseLabel, CaseStatement, ForStatement, IfStatement, ProceduralStatements,
    RepeatStatement, WhileStatement,
};
use crate::parsers::expr::Expression;
use crate::register::Register;
use crate::simulator::elaborate::rename_expression;
use crate::simulator::eval::eval;
use crate::simulator::exec::{drive_resolved, resolve_target, PendingUpdate};
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::StateStore;
use crate::simulator::tasks::{TaskCall, TaskContext};

/// What a delay nobody can run reports — either because it cannot be compiled,
/// or because the caller cannot hold the resume point a suspension hands back.
pub(crate) const DELAY_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a delay inside a procedural block");

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
    /// `#n` — suspend, and resume at the next instruction `n` time units later.
    Delay(i64),
    /// `$display(…)` and friends — a call to a system task.
    Task(TaskCall),
    /// The end of the block.
    Halt,
}

/// A procedural block flattened into instructions.
#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    instructions: Vec<Instruction>,
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
}

impl Program {
    /// Flattens a statement body into instructions.
    ///
    /// Fails with [`SimulationError::Unsupported`] on an intra-assignment delay
    /// (`a = #5 b;`), whose right hand side has to be carried across the
    /// suspension: a resume point is only a program counter, so there is
    /// nowhere to keep it.
    pub fn compile(statements: &[ProceduralStatements]) -> Result<Program, SimulationError> {
        let mut program = Program {
            instructions: Vec::new(),
        };
        program.compile_statements(statements)?;
        program.emit(Instruction::Halt);
        Ok(program)
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
            match instruction {
                Instruction::Blocking { target, value }
                | Instruction::NonBlocking { target, value } => {
                    rename_expression(target, resolve);
                    rename_expression(value, resolve);
                }
                Instruction::JumpIfFalse { condition, .. } => rename_expression(condition, resolve),
                Instruction::CaseSubject(subject) => rename_expression(subject, resolve),
                Instruction::JumpIfMatch { label, .. } => rename_expression(label, resolve),
                // A `repeat` counter is qualified like any other signal, which
                // is what gives two instances of the same module a counter
                // each rather than one they trample on together.
                Instruction::RepeatInit { counter, count } => {
                    *counter = resolve(counter);
                    rename_expression(count, resolve);
                }
                Instruction::RepeatNext { counter, .. } => *counter = resolve(counter),
                Instruction::Task(call) => call.rename(resolve),
                Instruction::Jump(_) | Instruction::Delay(_) | Instruction::Halt => {}
            }
        }
    }

    fn compile_statements(
        &mut self,
        statements: &[ProceduralStatements],
    ) -> Result<(), SimulationError> {
        for statement in statements {
            match statement {
                ProceduralStatements::Delay(delay) => {
                    self.emit(Instruction::Delay(delay.ticks()));
                }
                // `#5 <statement>` waits, then runs the statement — exactly
                // what a bare `#5;` written in front of it would do. The body
                // is compiled inline, so a delay nested in it suspends just as
                // one at the top level does.
                ProceduralStatements::Delayed { delay, statements } => {
                    self.emit(Instruction::Delay(delay.ticks()));
                    self.compile_statements(statements)?;
                }
                ProceduralStatements::Assignment(assignment) => {
                    self.compile_assignment(assignment)?
                }
                ProceduralStatements::If(conditional) => self.compile_if(conditional)?,
                ProceduralStatements::Case(case) => self.compile_case(case)?,
                ProceduralStatements::For(statement) => self.compile_for(statement)?,
                ProceduralStatements::While(statement) => self.compile_while(statement)?,
                ProceduralStatements::Repeat(statement) => self.compile_repeat(statement)?,
                ProceduralStatements::Forever(statements) => self.compile_forever(statements)?,
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
        if assignment.assignment_delay().is_some() {
            return Err(DELAY_UNSUPPORTED);
        }

        let target = assignment.lhs().clone();
        let value = assignment.rhs().clone();
        self.emit(match assignment.assignment_type() {
            ProceduralAssignmentType::Blocking => Instruction::Blocking { target, value },
            ProceduralAssignmentType::NonBlocking => Instruction::NonBlocking { target, value },
        });
        Ok(())
    }

    /// `if (c) T else E` becomes
    /// `JumpIfFalse(c, else); T; Jump(end); else: E; end:`.
    fn compile_if(&mut self, conditional: &IfStatement) -> Result<(), SimulationError> {
        let branch = self.emit(Instruction::JumpIfFalse {
            condition: conditional.condition.clone(),
            target: 0,
        });
        self.compile_statements(&conditional.then_statements)?;

        match &conditional.else_statements {
            Some(else_statements) => {
                let skip_else = self.emit(Instruction::Jump(0));
                let else_start = self.next();
                self.patch(branch, else_start);
                self.compile_statements(else_statements)?;
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
    fn compile_while(&mut self, statement: &WhileStatement) -> Result<(), SimulationError> {
        let top = self.next();
        let branch = self.emit(Instruction::JumpIfFalse {
            condition: statement.condition.clone(),
            target: 0,
        });
        self.compile_statements(&statement.statements)?;
        self.emit(Instruction::Jump(top));
        let end = self.next();
        self.patch(branch, end);
        Ok(())
    }

    /// `for (i; c; s) B` is the `while` shape with the initialiser in front of
    /// it and the step in front of the back-jump, so `continue`-less Verilog
    /// runs `s` after every completed iteration and never after the test fails.
    fn compile_for(&mut self, statement: &ForStatement) -> Result<(), SimulationError> {
        self.compile_assignment(&statement.initializer)?;
        let top = self.next();
        let branch = self.emit(Instruction::JumpIfFalse {
            condition: statement.condition.clone(),
            target: 0,
        });
        self.compile_statements(&statement.statements)?;
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
    fn compile_repeat(&mut self, statement: &RepeatStatement) -> Result<(), SimulationError> {
        let counter = format!("{}{}", REPEAT_COUNTER_PREFIX, self.next());
        self.emit(Instruction::RepeatInit {
            counter: counter.clone(),
            count: statement.count.clone(),
        });

        let top = self.next();
        let branch = self.emit(Instruction::RepeatNext { counter, target: 0 });
        self.compile_statements(&statement.statements)?;
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
    ) -> Result<(), SimulationError> {
        let top = self.next();
        self.compile_statements(statements)?;
        self.emit(Instruction::Jump(top));
        Ok(())
    }

    /// A `case` becomes its subject, then one `JumpIfMatch` per label in source
    /// order, then a fall-through jump to the `default` arm — or past the whole
    /// statement when there is none — then the arm bodies.
    ///
    /// Only the first `default` is reachable, so later ones are not compiled.
    fn compile_case(&mut self, case: &CaseStatement) -> Result<(), SimulationError> {
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
            self.compile_statements(arm)?;
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
                // evaluated, so a bad target is reported ahead of a bad value.
                let target = resolve_target(store, target)?;
                let value = eval(value, store)?;
                drive_resolved(store, &target, &value)?;
                pc += 1;
            }
            Instruction::NonBlocking { target, value } => {
                let target = resolve_target(store, target)?;
                let value = eval(value, store)?;
                pending.push(PendingUpdate::new(target, value));
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
            Instruction::Delay(delay) => {
                return Ok(Resume::Suspended {
                    pc: pc + 1,
                    delay: *delay,
                    pending,
                })
            }
            Instruction::Halt => return Ok(Resume::Halted { pending }),
        }
    }
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
        Program::compile(&statements).expect("block should compile")
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
        }
    }

    #[test]
    fn test_empty_block_compiles_to_a_bare_halt() {
        let program = Program::compile(&[]).unwrap();
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
        assert_eq!(program.instructions()[0], Instruction::Delay(50));
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
        assert_eq!(program.instructions()[0], Instruction::Delay(50));
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
    fn test_intra_assignment_delay_is_rejected_at_compile_time() {
        let (_, statements) = parse_block("begin a = #5 b; end").unwrap();
        assert_eq!(Program::compile(&statements), Err(DELAY_UNSUPPORTED));
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
}

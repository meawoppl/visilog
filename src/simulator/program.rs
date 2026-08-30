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
//! match resume(&program, 0, &mut store)? {
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
use crate::parsers::behavior::{CaseLabel, CaseStatement, IfStatement, ProceduralStatements};
use crate::parsers::expr::Expression;
use crate::register::{Register, ONE};
use crate::simulator::eval::eval;
use crate::simulator::exec::{drive_resolved, resolve_target, PendingUpdate};
use crate::simulator::runner::SimulationError;
use crate::simulator::state_store::StateStore;

/// What a delay nobody can run reports — either because it cannot be compiled,
/// or because the caller cannot hold the resume point a suspension hands back.
pub(crate) const DELAY_UNSUPPORTED: SimulationError =
    SimulationError::Unsupported("a delay inside a procedural block");

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
    /// Jump to a `case` arm when `label` matches the held subject.
    JumpIfMatch { label: Expression, target: usize },
    /// `#n` — suspend, and resume at the next instruction `n` time units later.
    Delay(i64),
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

    fn compile_statements(
        &mut self,
        statements: &[ProceduralStatements],
    ) -> Result<(), SimulationError> {
        for statement in statements {
            match statement {
                ProceduralStatements::Delay(delay) => {
                    self.emit(Instruction::Delay(delay.ticks()));
                }
                ProceduralStatements::Assignment(assignment) => {
                    self.compile_assignment(assignment)?
                }
                ProceduralStatements::If(conditional) => self.compile_if(conditional)?,
                ProceduralStatements::Case(case) => self.compile_case(case)?,
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

        // `#5 a = b;` waits before the statement runs, exactly as a bare `#5;`
        // written in front of it would.
        if let Some(delay) = assignment.pre_delay() {
            self.emit(Instruction::Delay(delay.ticks()));
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
            | Instruction::JumpIfMatch { target: slot, .. } => *slot = target,
            other => unreachable!("cannot patch {:?}", other),
        }
    }
}

/// Runs `program` from `pc` until the block ends or hits a delay.
///
/// Blocking writes land in `store` as they execute; non-blocking ones come back
/// in the returned `pending` list, whether the block finished or suspended.
pub fn resume(
    program: &Program,
    pc: usize,
    store: &mut StateStore,
) -> Result<Resume, SimulationError> {
    let mut pc = pc;
    let mut pending = Vec::new();
    // The subject of the `case` currently being matched. Comparisons always sit
    // between the `CaseSubject` that fills this and the arm bodies, and a delay
    // can only appear inside a body, so one slot is enough even when `case`
    // statements nest.
    let mut subject: Option<Register> = None;

    loop {
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
            Instruction::JumpIfMatch { label, target } => {
                let held = subject
                    .as_ref()
                    .ok_or(SimulationError::Unsupported("a case arm without a subject"))?;
                let label = eval(label, store)?;
                pc = if case_matches(held, &label) {
                    *target
                } else {
                    pc + 1
                };
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

/// Whether a `case` item matches the subject. A plain `case` compares with `==`
/// semantics, so an `x` or `z` on either side makes the comparison unknown,
/// which is not a match. (`casex` / `casez` are not parsed yet.)
fn case_matches(subject: &Register, label: &Register) -> bool {
    if subject.has_unknown() || label.has_unknown() {
        return false;
    }
    let width = subject.width().max(label.width());
    subject.resize(width) == label.resize(width)
}

/// Whether a register used as a condition is true. Verilog calls a condition
/// true only when it is a *known* non-zero value: zero, `x` and `z` all take
/// the else branch. An `x` condition is not an "unknown branch" — it is false.
fn is_true(register: &Register) -> bool {
    register.get_raw().iter().any(|&bit| bit == ONE)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parsers::behavior::parse_block;
    use crate::simulator::exec::commit_updates;

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
        match resume(program, pc, store).expect("resume should succeed") {
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

        let Resume::Suspended { pc, pending, .. } = resume(&program, 0, &mut store).unwrap() else {
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

    #[test]
    fn test_running_off_the_end_of_the_program_halts() {
        let program = compile("begin a = 1'b1; end");
        let mut store = store_with(&[("a", "0")]);

        assert_eq!(
            resume(&program, program.instructions().len(), &mut store),
            Ok(Resume::Halted {
                pending: Vec::new()
            })
        );
        assert_eq!(value(&store, "a"), "0");
    }
}

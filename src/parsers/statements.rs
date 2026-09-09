use nom::{branch::alt, combinator::map, error::context, IResult};

use super::{
    assignment::{parse_continuous_assignment, ContinuousAssignment},
    behavior::{
        parse_always_block, parse_function_declaration, parse_initial_block,
        parse_task_declaration, AlwaysBlock, FunctionDeclaration, InitialBlock, TaskDeclaration,
    },
    gates::{parse_gate_instantiation, GateInstantiation},
    generate::{
        parse_defparam, parse_generate_region, parse_genvar_declaration, DefparamAssignment,
        GenerateItem,
    },
    identifier::Identifier,
    integer::{
        parse_event_declaration, parse_integer_declaration, parse_real_declaration,
        parse_time_declaration, EventDeclaration, IntegerDeclaration, RealDeclaration,
        TimeDeclaration,
    },
    modules::{
        parse_module_instantiation_statement, parse_port_declaration, ModuleInstantiation, Port,
    },
    nets::{net_declaration, Net},
    parameter::{parse_parameter_declaration, ParameterDeclaration},
    primitive::UdpTable,
    register::{parse_register_declaration, RegisterDeclaration},
    specify::{parse_specify_block, SpecifyBlock},
};

#[derive(Debug, PartialEq)]
pub enum ModuleStatement {
    /// A Verilog-1995 body port declaration. It never survives
    /// `parse_module_declaration`, which folds it into the module's ports.
    PortDeclaration(Vec<Port>),
    RegisterDeclaration(Vec<RegisterDeclaration>),
    IntegerDeclaration(Vec<IntegerDeclaration>),
    /// `time t;` — a 64-bit unsigned variable.
    TimeDeclaration(Vec<TimeDeclaration>),
    /// `real r;` — parsed so that the simulator can reject it by name.
    RealDeclaration(Vec<RealDeclaration>),
    /// `event e;` — a named event, which has no value.
    EventDeclaration(Vec<EventDeclaration>),
    WireDeclaration(Vec<Net>),
    ParameterDeclaration(Vec<ParameterDeclaration>),
    /// `genvar i;` — a generate loop variable. It is an elaboration-time
    /// integer and never becomes a signal, so the declaration carries nothing
    /// but the names.
    GenvarDeclaration(Vec<Identifier>),
    /// `generate … endgenerate` — unrolled at elaboration into ordinary module
    /// items, since a loop bound may be a parameter and a parameter has no
    /// value until then.
    GenerateRegion(Vec<GenerateItem>),
    /// `defparam dut.WIDTH = 8;` — a parameter override addressed to an
    /// instance by name.
    Defparam(Vec<DefparamAssignment>),
    InitialBlock(InitialBlock),
    AlwaysBlock(AlwaysBlock),
    /// `function [7:0] f; … endfunction` — a function the module's
    /// expressions may call.
    FunctionDeclaration(FunctionDeclaration),
    /// `task t; … endtask` — a task the module's procedural blocks may enable.
    TaskDeclaration(TaskDeclaration),
    /// One `assign`, which may name several targets sharing a strength pair.
    Assignment(Vec<ContinuousAssignment>),
    /// `and g1 (out, a, b);` — one built-in primitive per instance declared,
    /// since one statement may declare several.
    GateInstantiation(Vec<GateInstantiation>),
    ModuleInstantiation(ModuleInstantiation),
    /// `specify … endspecify` — module path delays, timing checks and the
    /// `specparam`s written with them. Only the specparams reach the
    /// simulation; see `specify.rs` for why the rest cannot.
    SpecifyBlock(SpecifyBlock),
    /// The `table … endtable` of a `primitive`. It is deliberately *not* one of
    /// the alternatives below: a table is legal only inside a UDP, and a UDP is
    /// parsed by `primitive.rs` into a module whose one statement is this.
    PrimitiveTable(UdpTable),
}

pub fn parse_module_statement(input: &str) -> IResult<&str, ModuleStatement> {
    context(
        "module statement",
        alt((
            map(parse_port_declaration, ModuleStatement::PortDeclaration),
            map(parse_register_declaration, |d| {
                ModuleStatement::RegisterDeclaration(d)
            }),
            map(parse_integer_declaration, |d| {
                ModuleStatement::IntegerDeclaration(d)
            }),
            map(parse_time_declaration, |d| {
                ModuleStatement::TimeDeclaration(d)
            }),
            map(parse_real_declaration, |d| {
                ModuleStatement::RealDeclaration(d)
            }),
            map(parse_event_declaration, |d| {
                ModuleStatement::EventDeclaration(d)
            }),
            map(net_declaration, |d| ModuleStatement::WireDeclaration(d)),
            map(parse_parameter_declaration, |d| {
                ModuleStatement::ParameterDeclaration(d)
            }),
            map(parse_genvar_declaration, |d| {
                ModuleStatement::GenvarDeclaration(d)
            }),
            map(parse_generate_region, |d| {
                ModuleStatement::GenerateRegion(d)
            }),
            map(parse_defparam, |d| ModuleStatement::Defparam(d)),
            map(parse_initial_block, |d| ModuleStatement::InitialBlock(d)),
            map(parse_function_declaration, |d| {
                ModuleStatement::FunctionDeclaration(d)
            }),
            map(parse_task_declaration, |d| {
                ModuleStatement::TaskDeclaration(d)
            }),
            map(parse_always_block, |d| ModuleStatement::AlwaysBlock(d)),
            map(parse_continuous_assignment, |d| {
                ModuleStatement::Assignment(d)
            }),
            map(parse_specify_block, |d| ModuleStatement::SpecifyBlock(d)),
            // A gate primitive is keyword led — `and g1 (…)` — so it must be
            // tried *before* the module instantiation below, which would
            // otherwise read the keyword as a module name.
            map(parse_gate_instantiation, |d| {
                ModuleStatement::GateInstantiation(d)
            }),
            // NB(meawoppl) a module instantiation is just an identifier followed by
            // an argument block, so it has to come last or it will shadow the
            // statement forms that start with a keyword.
            map(parse_module_instantiation_statement, |d| {
                ModuleStatement::ModuleInstantiation(d)
            }),
        )),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::helpers::assert_parses;
    use crate::parsers::simple::Range;

    #[test]
    fn test_parse_module_statement_variants() {
        assert!(matches!(
            assert_parses(parse_module_statement, "reg [7:0] a;"),
            ModuleStatement::RegisterDeclaration(_)
        ));
        assert!(matches!(
            assert_parses(parse_module_statement, "wire a, b;"),
            ModuleStatement::WireDeclaration(_)
        ));
        assert!(matches!(
            assert_parses(parse_module_statement, "localparam IDLE = 2'b00;"),
            ModuleStatement::ParameterDeclaration(_)
        ));
        assert!(matches!(
            assert_parses(parse_module_statement, "assign a = b;"),
            ModuleStatement::Assignment(_)
        ));
        assert!(matches!(
            assert_parses(parse_module_statement, "always @(posedge clk) a <= b;"),
            ModuleStatement::AlwaysBlock(_)
        ));
        assert!(matches!(
            assert_parses(parse_module_statement, "initial begin a = 'b1; end"),
            ModuleStatement::InitialBlock(_)
        ));
        assert!(matches!(
            assert_parses(parse_module_statement, "task t; input a; b = a; endtask"),
            ModuleStatement::TaskDeclaration(_)
        ));
    }

    /// The declaration forms that carry a *list* keep every name, and a
    /// module-level `integer` reaches the statement grammar at all.
    #[test]
    fn test_parse_declaration_lists() {
        match assert_parses(parse_module_statement, "reg [4:0] result, b;") {
            ModuleStatement::RegisterDeclaration(registers) => {
                assert_eq!(registers.len(), 2);
                assert_eq!(registers[0].name, "result".into());
                assert_eq!(registers[1].name, "b".into());
                assert!(registers
                    .iter()
                    .all(|r| r.range == Some(Range::Constant(4, 0))));
            }
            other => panic!("expected a register declaration, got {:?}", other),
        }

        match assert_parses(parse_module_statement, "wire a, b, c;") {
            ModuleStatement::WireDeclaration(nets) => assert_eq!(nets.len(), 3),
            other => panic!("expected a wire declaration, got {:?}", other),
        }

        match assert_parses(parse_module_statement, "integer i, j;") {
            ModuleStatement::IntegerDeclaration(integers) => {
                assert_eq!(integers.len(), 2);
                assert_eq!(integers[0].name, "i".into());
                assert_eq!(integers[1].name, "j".into());
            }
            other => panic!("expected an integer declaration, got {:?}", other),
        }

        match assert_parses(parse_module_statement, "reg [7:0] mem [0:255];") {
            ModuleStatement::RegisterDeclaration(registers) => {
                assert_eq!(registers.len(), 1);
                assert_eq!(registers[0].range, Some(Range::Constant(7, 0)));
                assert_eq!(registers[0].dimensions, Some(Range::Constant(0, 255)));
            }
            other => panic!("expected a memory declaration, got {:?}", other),
        }
    }

    /// Every declaration form reaches the statement grammar with an
    /// initialiser attached, and a list keeps one per name.
    #[test]
    fn test_parse_declaration_initialisers() {
        match assert_parses(parse_module_statement, "wire a = 1'b1;") {
            ModuleStatement::WireDeclaration(nets) => {
                assert_eq!(nets.len(), 1);
                assert!(nets[0].init().is_some());
            }
            other => panic!("expected a wire declaration, got {:?}", other),
        }

        match assert_parses(parse_module_statement, "reg [3:0] b = 4'h5;") {
            ModuleStatement::RegisterDeclaration(registers) => {
                assert_eq!(registers[0].range, Some(Range::Constant(3, 0)));
                assert!(registers[0].init.is_some());
            }
            other => panic!("expected a register declaration, got {:?}", other),
        }

        match assert_parses(parse_module_statement, "integer i = 0;") {
            ModuleStatement::IntegerDeclaration(integers) => {
                assert!(integers[0].init.is_some());
            }
            other => panic!("expected an integer declaration, got {:?}", other),
        }

        match assert_parses(parse_module_statement, "wire x = 1, y = 2;") {
            ModuleStatement::WireDeclaration(nets) => {
                assert_eq!(nets.len(), 2);
                assert_eq!(nets[0].identifier(), &"x".into());
                assert_eq!(nets[1].identifier(), &"y".into());
                assert_ne!(nets[0].init(), nets[1].init());
            }
            other => panic!("expected a wire declaration, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_module_instantiation_statement() {
        let statement = assert_parses(
            parse_module_statement,
            "adder my_adder (.a(in_a),.b(in_b),.c(sum));",
        );
        match statement {
            ModuleStatement::ModuleInstantiation(instantiation) => {
                assert_eq!(instantiation.module_name, "adder".into());
                assert_eq!(instantiation.instance_name, "my_adder".into());
            }
            _ => panic!("Expected a module instantiation, got {:?}", statement),
        }
    }
}

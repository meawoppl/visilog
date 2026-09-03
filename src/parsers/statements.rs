use nom::{branch::alt, combinator::map, error::context, IResult};

use super::{
    assignment::{parse_continuous_assignment, ContinuousAssignment},
    behavior::{parse_always_block, parse_initial_block, AlwaysBlock, InitialBlock},
    integer::{parse_integer_declaration, IntegerDeclaration},
    modules::{parse_module_instantiation_statement, ModuleInstantiation},
    nets::{net_declaration, Net},
    parameter::{parse_parameter_declaration, ParameterDeclaration},
    register::{parse_register_declaration, RegisterDeclaration},
};

#[derive(Debug, PartialEq)]
pub enum ModuleStatement {
    RegisterDeclaration(Vec<RegisterDeclaration>),
    IntegerDeclaration(Vec<IntegerDeclaration>),
    WireDeclaration(Vec<Net>),
    ParameterDeclaration(Vec<ParameterDeclaration>),
    InitialBlock(InitialBlock),
    AlwaysBlock(AlwaysBlock),
    Assignment(ContinuousAssignment),
    ModuleInstantiation(ModuleInstantiation),
}

pub fn parse_module_statement(input: &str) -> IResult<&str, ModuleStatement> {
    context(
        "module statement",
        alt((
            map(parse_register_declaration, |d| {
                ModuleStatement::RegisterDeclaration(d)
            }),
            map(parse_integer_declaration, |d| {
                ModuleStatement::IntegerDeclaration(d)
            }),
            map(net_declaration, |d| ModuleStatement::WireDeclaration(d)),
            map(parse_parameter_declaration, |d| {
                ModuleStatement::ParameterDeclaration(d)
            }),
            map(parse_initial_block, |d| ModuleStatement::InitialBlock(d)),
            map(parse_always_block, |d| ModuleStatement::AlwaysBlock(d)),
            map(parse_continuous_assignment, |d| {
                ModuleStatement::Assignment(d)
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
                assert!(registers.iter().all(|r| r.range == Some((4, 0))));
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
                assert_eq!(registers[0].range, Some((7, 0)));
                assert_eq!(registers[0].dimensions, Some((0, 255)));
            }
            other => panic!("expected a memory declaration, got {:?}", other),
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

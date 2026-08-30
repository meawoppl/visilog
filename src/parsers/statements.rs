use nom::{branch::alt, combinator::map, error::context, IResult};

use super::{
    assignment::{parse_continuous_assignment, ContinuousAssignment},
    behavior::{parse_always_block, parse_initial_block, AlwaysBlock, InitialBlock},
    modules::{parse_module_instantiation_statement, ModuleInstantiation},
    nets::{net_declaration, Net},
    parameter::{parse_parameter_declaration, ParameterDeclaration},
    register::{parse_register_declaration, RegisterDeclaration},
};

#[derive(Debug, PartialEq)]
pub enum ModuleStatement {
    RegisterDeclaration(RegisterDeclaration),
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

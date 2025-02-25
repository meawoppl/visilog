use nom::{branch::alt, combinator::map, error::context, IResult};

use super::{
    assignment::{parse_continuous_assignment, ContinuousAssignment},
    behavior::{parse_always_block, parse_initial_block, AlwaysBlock, InitialBlock},
    modules::ModuleInstantiation,
    nets::{net_declaration, Net},
    register::{parse_register_declaration, RegisterDeclaration},
};

#[derive(Debug, PartialEq)]
pub enum ModuleStatement {
    // TODO(meawoppl) - add conditional flows here
    RegisterDeclaration(RegisterDeclaration),
    WireDeclaration(Vec<Net>),
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
            map(parse_initial_block, |d| ModuleStatement::InitialBlock(d)),
            map(parse_always_block, |d| ModuleStatement::AlwaysBlock(d)),
            map(parse_continuous_assignment, |d| {
                ModuleStatement::Assignment(d)
            }),
        )),
    )(input)
}

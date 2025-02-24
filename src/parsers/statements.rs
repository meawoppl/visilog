use nom::{branch::alt, combinator::map, multi::many1, IResult};

use super::{
    assignment::ContinuousAssignment,
    behavior::{parse_always_block, parse_initial_block, AlwaysBlock, InitialBlock},
    nets::{net_declaration, Net},
    register::{parse_register_declaration, RegisterDeclaration},
};

#[derive(Debug, PartialEq)]
pub enum ModuleStatement {
    RegisterDeclaration(RegisterDeclaration),
    WireDeclaration(Vec<Net>),
    InitialBlock(InitialBlock),
    AlwaysBlock(AlwaysBlock),
    Assignment(ContinuousAssignment),
}

pub fn parse_module_statement(input: &str) -> IResult<&str, ModuleStatement> {
    alt((
        map(parse_register_declaration, |d| {
            ModuleStatement::RegisterDeclaration(d)
        }),
        map(net_declaration, |d| ModuleStatement::WireDeclaration(d)),
        map(parse_initial_block, |d| ModuleStatement::InitialBlock(d)),
        map(parse_always_block, |d| ModuleStatement::AlwaysBlock(d)),
    ))(input)
}

pub fn parse_module_statements(input: &str) -> IResult<&str, Vec<ModuleStatement>> {
    many1(parse_module_statement)(input)
}

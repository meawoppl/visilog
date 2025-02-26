use nom::{branch::alt, bytes::complete::tag, combinator::map, error::context, IResult};

use super::{
    assignment::{parse_continuous_assignment, ContinuousAssignment},
    behavior::{parse_always_block, parse_initial_block, AlwaysBlock, InitialBlock},
    constants::{verilog_const, VerilogConstant},
    identifier::{self, Identifier},
    modules::ModuleInstantiation,
    nets::{net_declaration, Net},
    register::{parse_register_declaration, RegisterDeclaration},
    simple::ws,
};

use super::identifier::identifier;

#[derive(Debug, PartialEq)]
pub struct LocalParam {
    name: Identifier,
    value: VerilogConstant,
}

pub fn parse_local_param(input: &str) -> IResult<&str, LocalParam> {
    let (input, name) = identifier(input)?;
    let (input, _) = ws(tag("="))(input)?;
    let (input, value) = verilog_const(input)?;
    let (input, _) = ws(tag(";"))(input)?;

    Ok((input, LocalParam { name, value }))
}

//     let (input, identifier) = identifier(input)?
// }

/// These are all the statements that are allowed at the module level
#[derive(Debug, PartialEq)]
pub enum ModuleStatement {
    // TODO(meawoppl) - add conditional flows here
    RegisterDeclaration(RegisterDeclaration),
    WireDeclaration(Vec<Net>),
    InitialBlock(InitialBlock),
    AlwaysBlock(AlwaysBlock),
    Assignment(ContinuousAssignment),
    ModuleInstantiation(ModuleInstantiation),
    LocalParam(LocalParam),
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
            map(parse_local_param, |d| ModuleStatement::LocalParam(d)),
        )),
    )(input)
}

// TODO(meawoppl) - AI gen tests cases for localparam

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0},
    combinator::{map, opt},
    multi::{many0, separated_list0},
    sequence::delimited,
    IResult,
};

use super::{
    identifier::{identifier, Identifier},
    simple::{range, ws},
    statements::{parse_module_statement, ModuleStatement},
};

#[derive(Debug, PartialEq)]
pub struct VerilogModule {
    pub identifier: Identifier,
    pub ports: Vec<Port>,
    pub statements: Vec<ModuleStatement>,
}

#[derive(Debug, PartialEq)]
pub struct Port {
    pub direction: PortDirection,
    pub net_type: Option<NetType>,
    pub range: (i64, i64),
    pub identifier: Identifier,
}

#[derive(Debug, PartialEq)]
pub enum PortDirection {
    Input,
    Output,
    InOut,
}

#[derive(Debug, PartialEq)]
pub enum NetType {
    Wire,
    Reg,
}

fn parse_port_direction(input: &str) -> IResult<&str, PortDirection> {
    alt((
        map(tag("input"), |_| PortDirection::Input),
        map(tag("output"), |_| PortDirection::Output),
        map(tag("inout"), |_| PortDirection::InOut),
    ))(input)
}

fn parse_net_type(input: &str) -> IResult<&str, NetType> {
    alt((
        map(tag("wire"), |_| NetType::Wire),
        map(tag("reg"), |_| NetType::Reg),
    ))(input)
}

fn parse_port(input: &str) -> IResult<&str, Port> {
    let (input, direction) = parse_port_direction(input)?;
    let (input, _) = multispace0(input)?;
    let (input, net_type) = opt(parse_net_type)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, range) = opt(range)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, identifier) = identifier(input)?;
    Ok((
        input,
        Port {
            direction,
            net_type,
            range: range.unwrap_or((0, 0)),
            identifier,
        },
    ))
}

fn parse_ports(input: &str) -> IResult<&str, Vec<Port>> {
    delimited(
        ws(char('(')),
        separated_list0(ws(char(',')), parse_port),
        ws(char(')')),
    )(input)
}

pub fn parse_module_declaration(input: &str) -> IResult<&str, VerilogModule> {
    let (input, _) = ws(tag("module"))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, mod_identifier) = identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, ports) = parse_ports(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = ws(tag(";"))(input)?;
    let (input, statements) = many0(ws(parse_module_statement))(input)?;
    let (input, _) = ws(tag("endmodule"))(input)?;
    Ok((
        input,
        VerilogModule {
            identifier: mod_identifier,
            ports,
            statements,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::helpers::{assert_parses, assert_parses_to};
    use std::fs;

    #[test]
    fn test_parse_port_direction() {
        assert_parses_to(parse_port_direction, "input", PortDirection::Input);
        assert_parses_to(parse_port_direction, "output", PortDirection::Output);
        assert_parses_to(parse_port_direction, "inout", PortDirection::InOut);
    }

    #[test]
    fn test_parse_net_type() {
        assert_parses_to(parse_net_type, "wire", NetType::Wire);
        assert_parses_to(parse_net_type, "reg", NetType::Reg);
    }

    #[test]
    fn test_parse_port() {
        assert_parses_to(
            parse_port,
            "input wire a",
            Port {
                direction: PortDirection::Input,
                net_type: Some(NetType::Wire),
                range: (0, 0),
                identifier: "a".into(),
            },
        );
        assert_parses_to(
            parse_port,
            "output reg b",
            Port {
                direction: PortDirection::Output,
                net_type: Some(NetType::Reg),
                range: (0, 0),
                identifier: "b".into(),
            },
        );
        assert_parses_to(
            parse_port,
            "inout c",
            Port {
                direction: PortDirection::InOut,
                net_type: None,
                range: (0, 0),
                identifier: "c".into(),
            },
        );
    }

    #[test]
    fn test_parse_ports() {
        assert_parses_to(
            parse_ports,
            "( input wire a, output reg b, inout c )",
            vec![
                Port {
                    direction: PortDirection::Input,
                    net_type: Some(NetType::Wire),
                    range: (0, 0),
                    identifier: "a".into(),
                },
                Port {
                    direction: PortDirection::Output,
                    net_type: Some(NetType::Reg),
                    range: (0, 0),
                    identifier: "b".into(),
                },
                Port {
                    direction: PortDirection::InOut,
                    net_type: None,
                    range: (0, 0),
                    identifier: "c".into(),
                },
            ],
        );
    }

    #[test]
    fn test_parse_minimal_module_declaration() {
        let input = r#"
            module my_module (
                input wire a,
                output wire b
            );
            endmodule
        "#;
        let result = parse_module_declaration(input);
        assert!(result.is_ok());
        let (remaining, module) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(module.identifier, "my_module".into());
        assert_eq!(module.ports.len(), 2);
        assert_eq!(module.ports[0].identifier, "a".into());
        assert_eq!(module.ports[1].identifier, "b".into());

        assert_eq!(module.statements.len(), 0);
    }

    #[test]
    fn test_simple_adder() {
        let input = r#"
            module adder(
                input [7:0] a,
                input [7:0] b,
                output [7:0] c
            );
                assign c = a + b;
            endmodule
        "#;

        let (remaining, module) = parse_module_declaration(input).unwrap();
        assert!(remaining.trim().is_empty());
        assert_eq!(module.statements.len(), 1);
    }

    // TODO(meawoppl) this loads and runs all the test files, but we aren't able to parse them all yet
    // #[test]
    fn test_parse_verilog_examples() {
        let example_files_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join("verilog")
            .join("examples");
        let example_files = fs::read_dir(example_files_dir)
            .expect("Unable to read directory")
            .filter_map(|entry| {
                let entry = entry.expect("Unable to read entry");
                let path = entry.path();
                if path.is_file() {
                    Some(path.to_string_lossy().to_string())
                } else {
                    None
                }
            })
            .map(|path| {
                let content = fs::read_to_string(path).expect("Unable to read file");

                assert_parses(parse_module_declaration, &content)
            })
            .collect::<Vec<_>>();

        println!("{:?}", example_files);
    }
}

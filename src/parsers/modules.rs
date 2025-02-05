use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, char, multispace0},
    combinator::{map, opt, recognize},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

use super::{
    behavior::{procedural_statement, ProceduralStatements},
    identifier::{identifier, Identifier},
    simple::ws,
};

#[derive(Debug, PartialEq)]
pub struct VerilogModule {
    pub identifier: Identifier,
    pub ports: Vec<Port>,
    pub statements: Vec<ProceduralStatements>,
}

#[derive(Debug, PartialEq)]
pub struct Port {
    pub direction: PortDirection,
    pub net_type: Option<NetType>,
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
    let (input, identifier) = identifier(input)?;
    Ok((
        input,
        Port {
            direction,
            net_type,
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
    let (input, statements) = many0(ws(procedural_statement))(input)?;
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
    use crate::parsers::helpers::assert_parses_to;
    use std::fs;
    use super::*;

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
                identifier: "a".into(),
            },
        );
        assert_parses_to(
            parse_port,
            "output reg b",
            Port {
                direction: PortDirection::Output,
                net_type: Some(NetType::Reg),
                identifier: "b".into(),
            },
        );
        assert_parses_to(
            parse_port,
            "inout c",
            Port {
                direction: PortDirection::InOut,
                net_type: None,
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
                    identifier: "a".into(),
                },
                Port {
                    direction: PortDirection::Output,
                    net_type: Some(NetType::Reg),
                    identifier: "b".into(),
                },
                Port {
                    direction: PortDirection::InOut,
                    net_type: None,
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
    }

    #[test]
    fn test_parse_verilog_examples() {
        let example_files = vec![
            "src/verilog/examples/simple_module.v",
            "src/verilog/examples/complex_module.v",
        ];

        for file in example_files {
            let content = fs::read_to_string(file).expect("Unable to read file");
            let result = parse_module_declaration(&content);
            assert!(result.is_ok(), "Failed to parse {}", file);
        }
    }
}

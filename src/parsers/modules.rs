use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, char, multispace0},
    combinator::{map, opt, recognize},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

use super::{behavior::{procedural_statement, ProceduralStatements}, identifier::{identifier, Identifier}};

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
        char('('),
        separated_list0(preceded(multispace0, char(',')), parse_port),
        char(')'),
    )(input)
}

pub fn parse_module_declaration(input: &str) -> IResult<&str, VerilogModule> {
    let (input, _) = tag("module")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, mod_identifier) = identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, ports) = parse_ports(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(";")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, statements) = many0(procedural_statement)(input)?;
    let (input, _) = tag("endmodule")(input)?;
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

    #[test]
    fn test_parse_port_direction() {
        assert_eq!(parse_port_direction("input"), Ok(("", PortDirection::Input)));
        assert_eq!(parse_port_direction("output"), Ok(("", PortDirection::Output)));
        assert_eq!(parse_port_direction("inout"), Ok(("", PortDirection::InOut)));
    }

    #[test]
    fn test_parse_net_type() {
        assert_eq!(parse_net_type("wire"), Ok(("", NetType::Wire)));
        assert_eq!(parse_net_type("reg"), Ok(("", NetType::Reg)));
    }

    #[test]
    fn test_parse_port() {
        assert_eq!(
            parse_port("input wire a"),
            Ok((
                "",
                Port {
                    direction: PortDirection::Input,
                    net_type: Some(NetType::Wire),
                    identifier: "a".into(),
                }
            ))
        );
        assert_eq!(
            parse_port("output reg b"),
            Ok((
                "",
                Port {
                    direction: PortDirection::Output,
                    net_type: Some(NetType::Reg),
                    identifier: "b".into()
                }
            ))
        );
        assert_eq!(
            parse_port("inout c"),
            Ok((
                "",
                Port {
                    direction: PortDirection::InOut,
                    net_type: None,
                    identifier: "c".into(),
                }
            ))
        );
    }

    #[test]
    fn test_parse_ports() {
        assert_eq!(
            parse_ports("(input wire a, output reg b, inout c)"),
            Ok((
                "",
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
                ]
            ))
        );
    }

    #[test]
    fn test_parse_module_declaration() {
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
}

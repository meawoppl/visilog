use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, char, multispace0},
    combinator::{map, opt, recognize},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

#[derive(Debug, PartialEq)]
pub struct VerilogModule {
    pub name: String,
    pub ports: Vec<Port>,
}

#[derive(Debug, PartialEq)]
pub struct Port {
    pub direction: PortDirection,
    pub net_type: Option<NetType>,
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub enum PortDirection {
    Input,
    Output,
    Inout,
}

#[derive(Debug, PartialEq)]
pub enum NetType {
    Wire,
    Reg,
}

fn parse_identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    ))(input)
}

fn parse_port_direction(input: &str) -> IResult<&str, PortDirection> {
    alt((
        map(tag("input"), |_| PortDirection::Input),
        map(tag("output"), |_| PortDirection::Output),
        map(tag("inout"), |_| PortDirection::Inout),
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
    let (input, name) = parse_identifier(input)?;
    Ok((
        input,
        Port {
            direction,
            net_type,
            name: name.to_string(),
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
    let (input, name) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, ports) = parse_ports(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(";")(input)?;
    let (input, _) = take_until("endmodule")(input)?;
    let (input, _) = tag("endmodule")(input)?;
    Ok((
        input,
        VerilogModule {
            name: name.to_string(),
            ports,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_identifier() {
        assert_eq!(parse_identifier("my_module"), Ok(("", "my_module")));
        assert_eq!(parse_identifier("_my_module"), Ok(("", "_my_module")));
        assert_eq!(parse_identifier("my_module1"), Ok(("", "my_module1")));
        assert!(parse_identifier("1my_module").is_err());
    }

    #[test]
    fn test_parse_port_direction() {
        assert_eq!(parse_port_direction("input"), Ok(("", PortDirection::Input)));
        assert_eq!(parse_port_direction("output"), Ok(("", PortDirection::Output)));
        assert_eq!(parse_port_direction("inout"), Ok(("", PortDirection::Inout)));
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
                    name: "a".to_string(),
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
                    name: "b".to_string(),
                }
            ))
        );
        assert_eq!(
            parse_port("inout c"),
            Ok((
                "",
                Port {
                    direction: PortDirection::Inout,
                    net_type: None,
                    name: "c".to_string(),
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
                        name: "a".to_string(),
                    },
                    Port {
                        direction: PortDirection::Output,
                        net_type: Some(NetType::Reg),
                        name: "b".to_string(),
                    },
                    Port {
                        direction: PortDirection::Inout,
                        net_type: None,
                        name: "c".to_string(),
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
        assert_eq!(module.name, "my_module");
        assert_eq!(module.ports.len(), 2);
        assert_eq!(module.ports[0].name, "a");
        assert_eq!(module.ports[1].name, "b");
    }
}

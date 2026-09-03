use std::collections::HashMap;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::char,
    combinator::{map, opt},
    multi::{many0, separated_list0, separated_list1},
    sequence::delimited,
    IResult,
};

use super::{
    expr::{verilog_expression, Expression},
    identifier::{identifier, Identifier},
    simple::{range, ws, ws_and_comments},
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
    let (input, _) = ws_and_comments(input)?;
    let (input, net_type) = opt(parse_net_type)(input)?;
    let (input, _) = ws_and_comments(input)?;
    let (input, range) = opt(range)(input)?;
    let (input, _) = ws_and_comments(input)?;
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

/// Parses `module name (ports); … endmodule`.
///
/// The port list is optional: `module top;` is legal Verilog and is how most
/// testbenches are written, since a top-level module has nothing to connect to.
pub fn parse_module_declaration(input: &str) -> IResult<&str, VerilogModule> {
    let (input, _) = ws(tag("module"))(input)?;
    let (input, mod_identifier) = ws(identifier)(input)?;
    let (input, ports) = map(opt(parse_ports), Option::unwrap_or_default)(input)?;
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

#[derive(Debug, PartialEq, Clone)]
pub enum ModuleInitArguments {
    NoArgs,
    Positional(Vec<Expression>),
    Keyword(HashMap<Identifier, Expression>),
}

pub fn parse_positional_arguments(input: &str) -> IResult<&str, ModuleInitArguments> {
    map(separated_list1(tag(","), verilog_expression), |args| {
        ModuleInitArguments::Positional(args)
    })(input)
}

fn kw_arg(input: &str) -> IResult<&str, (Identifier, Expression)> {
    let (input, _) = tag(".")(input)?;
    let (input, identifier) = identifier(input)?;
    let (input, expression) = delimited(tag("("), ws(verilog_expression), tag(")"))(input)?;
    Ok((input, (identifier, expression)))
}

pub fn parse_keyword_arguments(input: &str) -> IResult<&str, ModuleInitArguments> {
    map(separated_list1(tag(","), ws(kw_arg)), |args| {
        let mut map = HashMap::new();
        for (id, expr) in args {
            map.insert(id, expr);
        }
        ModuleInitArguments::Keyword(map)
    })(input)
}

/// Parse a block of arguments, either positional or keyword, without delimiters
/// eg.
///
/// positional: `1,2,3`
///
/// keyword: `.a(1),.b(2),.c(3)`
pub fn parse_arguments(input: &str) -> IResult<&str, ModuleInitArguments> {
    map(
        opt(alt((parse_keyword_arguments, parse_positional_arguments))),
        |args| args.unwrap_or(ModuleInitArguments::NoArgs),
    )(input)
}

/// Parse a block of arguments, either positional or keyword
/// eg.
///
/// positional: (1,2,3)
///
/// keyword: (.a(1),.b(2),.c(3))
fn argument_block(input: &str) -> IResult<&str, ModuleInitArguments> {
    delimited(tag("("), parse_arguments, tag(")"))(input)
}

fn param_block(input: &str) -> IResult<&str, ModuleInitArguments> {
    let (input, _) = tag("#")(input)?;
    argument_block(input)
}

#[derive(Debug, PartialEq)]
pub struct ModuleInstantiation {
    pub module_name: Identifier,
    pub instance_name: Identifier,
    pub parameters: ModuleInitArguments, // NB(meawoppl) parameters have tighter bounds than arguments (we don't check)
    pub arguments: ModuleInitArguments,
}

pub fn parse_module_instantiation_statement(input: &str) -> IResult<&str, ModuleInstantiation> {
    // vdff #(.size(10),.delay(15)) mod_a (.out(out_a),.in(in_a),.clk(clk));
    // vdff mod_b (.out(out_b),.in(in_b),.clk(clk));
    // vdff #(.delay(12)) mod_c (.out(out_c),.in(in_c),.clk(clk));
    // vdff #(.delay( ),.size(10) ) mod_d (.out(out_d),.in(in_d),.clk(clk));

    let (input, module_name) = identifier(input)?;
    let (input, _) = ws_and_comments(input)?;

    let (input, parameters) = map(opt(param_block), |params| {
        params.unwrap_or(ModuleInitArguments::NoArgs)
    })(input)?;
    let (input, _) = ws_and_comments(input)?;

    let (input, instance_name) = identifier(input)?;
    let (input, _) = ws_and_comments(input)?;

    let (input, arguments) = argument_block(input)?;

    let (input, _) = ws(tag(";"))(input)?;

    Ok((
        input,
        ModuleInstantiation {
            module_name,
            instance_name,
            parameters,
            arguments,
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
    fn test_parse_module_without_a_port_list() {
        // `module top;` — how most testbenches are written, since a top-level
        // module has nothing to connect to.
        let module = assert_parses(
            parse_module_declaration,
            "module top;\n  assign a = b;\nendmodule",
        );
        assert_eq!(module.identifier, "top".into());
        assert!(module.ports.is_empty());
        assert_eq!(module.statements.len(), 1);
    }

    #[test]
    fn test_empty_port_list_and_absent_port_list_agree() {
        let with_parens = assert_parses(parse_module_declaration, "module m(); endmodule");
        let without = assert_parses(parse_module_declaration, "module m; endmodule");
        assert_eq!(with_parens, without);
    }

    #[test]
    fn test_an_opened_port_list_must_still_be_well_formed() {
        // An opening paren commits to a port list; `opt` must not let a
        // malformed one be silently abandoned.
        assert!(parse_module_declaration("module m(input a; endmodule").is_err());
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

    #[test]
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

    /// Issue #87: a comment is legal wherever whitespace is, including inside a
    /// port list — between ports, around the parens, and mid-declaration.
    #[test]
    fn test_comments_in_a_port_list() {
        let module = assert_parses(
            parse_module_declaration,
            r#"
            module commented /* named */ ( // the port list opens here
                input wire a,   // the first port
                /* the second port, on its own line */
                input /* the direction is done */ reg [7:0] b,
                /*
                 * a block comment that spans
                 * several lines, mid-declaration
                 */
                output c // the last port
            ) /* after the ports */ ;
            endmodule
            "#,
        );
        assert_eq!(module.identifier, "commented".into());
        assert_eq!(module.ports.len(), 3);
        assert_eq!(module.ports[1].identifier, "b".into());
        assert_eq!(module.ports[1].range, (7, 0));
        assert_eq!(module.ports[2].identifier, "c".into());
    }

    /// A comment between statements, between a statement and its semicolon, and
    /// between the last statement and `endmodule`. A `/* … */` around a whole
    /// statement comments it out.
    #[test]
    fn test_comments_in_a_module_body() {
        let module = assert_parses(
            parse_module_declaration,
            r#"
            module body_comments(input a, input b, output c);
                // a leading line comment
                wire d; /* trailing a declaration */
                /* assign d = 0; */
                assign d = a & b /* before the semicolon */;
                assign c = /* inside the expression */ d;
                // the last word before endmodule
            endmodule
            "#,
        );
        assert_eq!(module.statements.len(), 3);
    }

    /// Comments inside procedural code: a sensitivity list, a `begin … end`
    /// block, and the arms of a `case`.
    #[test]
    fn test_comments_in_procedural_code() {
        let module = assert_parses(
            parse_module_declaration,
            r#"
            module procedural_comments(input clk, input rst, output reg [1:0] q);
                always @(posedge clk /* the clock */ or posedge rst) begin // opens
                    // right after begin
                    if (rst) /* held in reset */ q <= 0;
                    else q <= q + 1;
                    // right before end
                end

                always @(*) begin
                    case (q) // the subject
                        // before the first arm
                        0 /* the label */ : q = 1;
                        1: q = 2; // after an arm
                        /* a whole arm, commented out
                        2: q = 3;
                        */
                        default /* the catch-all */ : q = 0;
                        // after the last arm
                    endcase
                end
            endmodule
            "#,
        );
        assert_eq!(module.statements.len(), 2);

        // The commented-out arm is not one of them.
        let case = match &module.statements[1] {
            ModuleStatement::AlwaysBlock(block) => match &block.statements[0] {
                crate::parsers::behavior::ProceduralStatements::Case(case) => case,
                other => panic!("expected a case statement, got {:?}", other),
            },
            other => panic!("expected an always block, got {:?}", other),
        };
        assert_eq!(case.items.len(), 3);
    }

    /// A comment in a module instantiation, between the module name, the
    /// parameter block, the instance name and the argument list.
    #[test]
    fn test_comments_in_a_module_instantiation() {
        let instantiation = assert_parses(
            parse_module_instantiation_statement,
            "counter /* the module */ dut /* the instance */ ( .clk(clk), .q(q) ) ; // done",
        );
        assert_eq!(instantiation.module_name, "counter".into());
        assert_eq!(instantiation.instance_name, "dut".into());
    }

    fn abc_123() -> ModuleInitArguments {
        let mut expected: HashMap<Identifier, Expression> = HashMap::new();

        expected.insert("a".into(), verilog_expression("1".into()).unwrap().1);
        expected.insert("b".into(), verilog_expression("2".into()).unwrap().1);
        expected.insert("c".into(), verilog_expression("3".into()).unwrap().1);
        ModuleInitArguments::Keyword(expected)
    }

    #[test]
    fn test_parse_kw_args() {
        let examples = vec![
            ".a(1),.b(2),.c(3)",
            ".a(1),.b(2),.c(3)",
            ".a( 1 ), .b(2),.c(3) ",
        ];

        for example in examples {
            assert_parses_to(parse_keyword_arguments, example, abc_123());
        }
    }

    #[test]
    fn test_parse_pos_args() {
        let arg_list_examples = vec![
            "1,2,3",
            "1, 2, 3",
            "a + b, foo, bar",
            "1/0, foo, bar, baz, qux",
        ];

        for example in arg_list_examples {
            let res = assert_parses(parse_positional_arguments, example);

            match res {
                ModuleInitArguments::Positional(args) => {
                    let comma_count = example.chars().filter(|&c| c == ',').count();
                    if comma_count == 0 {
                        assert_eq!(args.len(), 0);
                    } else {
                        assert_eq!(args.len(), comma_count + 1);
                    }
                }
                _ => {
                    panic!("Expected positional arguments, got {:?}", res);
                }
            }
        }
    }

    #[test]
    fn test_parse_named_args() {
        let arg_list_examples = vec![
            ".a(1),.b(2),.c(3)",
            ".a(1),.b(2),.c(3)",
            ".a( 1 ), .b(2),.c(3) ",
        ];

        for example in arg_list_examples {
            assert_parses_to(parse_keyword_arguments, example, abc_123());
        }
    }

    #[test]
    fn test_parse_arguments() {
        let examples = vec!["1,2,3", "1, 2, 3", "1, 2,  3 "];

        for example in examples {
            let res = assert_parses(parse_arguments, example);
            match res {
                ModuleInitArguments::Positional(mapping) => {
                    assert_eq!(mapping.len(), 3);
                }
                _ => {
                    panic!("Expected Positional arguments");
                }
            }
        }
    }
    #[test]
    fn test_parse_argument_block() {
        let examples = vec![
            ("()", ModuleInitArguments::NoArgs),
            (
                "(1,2,3)",
                ModuleInitArguments::Positional(vec![
                    verilog_expression("1".into()).unwrap().1,
                    verilog_expression("2".into()).unwrap().1,
                    verilog_expression("3".into()).unwrap().1,
                ]),
            ),
            (
                "(1, 2, 3)",
                ModuleInitArguments::Positional(vec![
                    verilog_expression("1".into()).unwrap().1,
                    verilog_expression("2".into()).unwrap().1,
                    verilog_expression("3".into()).unwrap().1,
                ]),
            ),
            ("(.a(1),.b(2),.c(3))", abc_123()),
        ];

        for (txt, expected) in examples {
            assert_parses_to(argument_block, txt, expected);
        }
    }

    #[test]
    fn test_parse_param_block() {
        assert_parses_to(param_block, "#(.a(1),.b(2),.c(3))", abc_123());
    }

    #[test]
    fn test_module_instantiation() {
        let test_statements = vec![
            "adder my_adder ();",
            "adder my_adder (1,2,3);",
            "adder my_adder (.a(1),.b(2),.c(3));",
            "adder #() my_adder (1,2,3);",
            "adder #(1) my_adder (.a(in_a),.b(in_b),.c(sum));",
            "adder #(.PARAM1(1),.PARAM2(2)) my_adder (.a(in_a),.b(in_b),.c(sum));",
        ];

        for input in test_statements {
            let res = assert_parses(parse_module_instantiation_statement, input);
            assert_eq!(res.module_name, "adder".into());
            assert_eq!(res.instance_name, "my_adder".into());
        }
    }
}

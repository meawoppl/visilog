use crate::validator::validate_module;
use crate::parsers::modules::{VerilogModule, Port, PortDirection, NetType};
use crate::parsers::identifier::Identifier;
use crate::parsers::assignment::{Assignment, AssignmentType};
use crate::parsers::behavior::{ProceduralStatements, Event, EventTriggers};

#[test]
fn test_validate_identifiers() {
    let module = VerilogModule {
        identifier: Identifier::new("test_module".to_string()),
        ports: vec![
            Port {
                direction: PortDirection::Input,
                net_type: Some(NetType::Wire),
                identifier: Identifier::new("a".to_string()),
            },
            Port {
                direction: PortDirection::Output,
                net_type: Some(NetType::Wire),
                identifier: Identifier::new("b".to_string()),
            },
        ],
        statements: vec![
            ProceduralStatements::Assignment(Assignment {
                pre_delay: None,
                lhs: Identifier::new("a".to_string()).into(),
                assignment_type: AssignmentType::Continuous,
                assignment_delay: None,
                rhs: Identifier::new("b".to_string()).into(),
            }),
        ],
    };

    assert!(validate_module(&module).is_ok());
}

#[test]
fn test_validate_initial_blocks() {
    let module = VerilogModule {
        identifier: Identifier::new("test_module".to_string()),
        ports: vec![
            Port {
                direction: PortDirection::Input,
                net_type: Some(NetType::Wire),
                identifier: Identifier::new("a".to_string()),
            },
            Port {
                direction: PortDirection::Output,
                net_type: Some(NetType::Wire),
                identifier: Identifier::new("b".to_string()),
            },
        ],
        statements: vec![
            ProceduralStatements::InitialBlock(vec![
                ProceduralStatements::Assignment(Assignment {
                    pre_delay: None,
                    lhs: Identifier::new("a".to_string()).into(),
                    assignment_type: AssignmentType::Continuous,
                    assignment_delay: None,
                    rhs: Identifier::new("b".to_string()).into(),
                }),
            ]),
        ],
    };

    assert!(validate_module(&module).is_ok());
}

#[test]
fn test_validate_always_blocks() {
    let module = VerilogModule {
        identifier: Identifier::new("test_module".to_string()),
        ports: vec![
            Port {
                direction: PortDirection::Input,
                net_type: Some(NetType::Wire),
                identifier: Identifier::new("a".to_string()),
            },
            Port {
                direction: PortDirection::Output,
                net_type: Some(NetType::Wire),
                identifier: Identifier::new("b".to_string()),
            },
        ],
        statements: vec![
            ProceduralStatements::AlwaysBlock(
                vec![
                    Event {
                        trigger: EventTriggers::PosEdge,
                        expression: Identifier::new("clk".to_string()).into(),
                    },
                ],
                vec![
                    ProceduralStatements::Assignment(Assignment {
                        pre_delay: None,
                        lhs: Identifier::new("a".to_string()).into(),
                        assignment_type: AssignmentType::Procedural,
                        assignment_delay: None,
                        rhs: Identifier::new("b".to_string()).into(),
                    }),
                ],
            ),
        ],
    };

    assert!(validate_module(&module).is_ok());
}

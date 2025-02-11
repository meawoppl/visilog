use crate::parsers::modules::VerilogModule;
use crate::parsers::identifier::Identifier;
use crate::parsers::assignment::AssignmentType;
use crate::parsers::behavior::ProceduralStatements;

pub mod validator {
    use super::*;

    pub fn validate_module(module: &VerilogModule) -> Result<(), String> {
        validate_identifiers(&module)?;
        validate_initial_blocks(&module)?;
        validate_always_blocks(&module)?;
        Ok(())
    }

    fn validate_identifiers(module: &VerilogModule) -> Result<(), String> {
        let mut defined_identifiers = vec![];

        for port in &module.ports {
            defined_identifiers.push(&port.identifier);
        }

        for statement in &module.statements {
            match statement {
                ProceduralStatements::Assignment(assignment) => {
                    if !defined_identifiers.contains(&assignment.lhs) {
                        return Err(format!("Undefined identifier: {:?}", assignment.lhs));
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn validate_initial_blocks(module: &VerilogModule) -> Result<(), String> {
        for statement in &module.statements {
            if let ProceduralStatements::InitialBlock(assignments) = statement {
                for assignment in assignments {
                    if let ProceduralStatements::Assignment(assignment) = assignment {
                        if assignment.assignment_type != AssignmentType::Continuous {
                            return Err(format!(
                                "Invalid assignment in initial block: {:?}",
                                assignment
                            ));
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn validate_always_blocks(module: &VerilogModule) -> Result<(), String> {
        for statement in &module.statements {
            if let ProceduralStatements::AlwaysBlock(_, assignments) = statement {
                for assignment in assignments {
                    if let ProceduralStatements::Assignment(assignment) = assignment {
                        if assignment.assignment_type != AssignmentType::Procedural {
                            return Err(format!(
                                "Invalid assignment in always block: {:?}",
                                assignment
                            ));
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

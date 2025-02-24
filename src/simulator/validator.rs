use crate::parsers::{identifier::Identifier, modules::VerilogModule};

pub fn gather_definitions(module: &VerilogModule) -> Result<(), String> {
    let mut all_identifiers: Vec<Identifier> = Vec::new();

    // Gather input ports
    for port in &module.ports {
        all_identifiers.push(port.identifier.clone());
    }

    // for statements in &module.statements {
    //     match statements {
    //         Proce
    //     }
    // }

    // // Gather wire declarations
    // for wire in &module.wires {
    //     // TODO: Store wire definitions
    // }

    // // Gather register declarations
    // for reg in &module.registers {
    //     // TODO: Store register definitions
    // }

    Ok(())
}

pub fn validate_module(module: VerilogModule) -> Result<(), String> {
    Ok(())
}

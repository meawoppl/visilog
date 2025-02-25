// Rough order of operations for this module.
// 2. Validate the module, try to provide useful debugging information.
// 3. Recrsively descend all submodules, and gather all register definitions
// 4. Create a StateStore with all the register definitions.
// 5. Descend all the assign statements, and create the expression graph
// 6. Compute all the pos/neg edge statements
// 7. Create the event queue for actual simulation state managment
// 8. Queue all the initial and unconditiona always blocks
// 9. Run the simulation

use std::collections::HashMap;

use crate::parsers::{
    modules::VerilogModule, register::RegisterDeclaration, statements::ModuleStatement,
};

pub fn gather_registers(
    module: &VerilogModule,
    prefix: String,
) -> HashMap<String, &RegisterDeclaration> {
    let mut name_to_reg = HashMap::new();

    for statement in &module.statements {
        match statement {
            ModuleStatement::RegisterDeclaration(register) => {
                name_to_reg.insert(format!("{}.{}", prefix, register.name.name), register);
            }
            ModuleStatement::ModuleInstantiation(_) => {
                // TODO(meawoppl)
            }
            _ => {}
        }
    }
    name_to_reg
}
pub struct Simulator {
    module: VerilogModule,
}

impl Simulator {
    pub fn new(module: VerilogModule) -> Self {
        Self { module }
    }

    pub fn setup(&mut self) {
        // Recursively descend all submodules, and gather all register definitions
        // TODO(meawoppl)
    }

    pub fn run(&mut self) {
        // TODO(meawoppl)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::Parser;

    use crate::parsers::modules::parse_module_declaration;

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

        // Wire signals onto the input/output ports
        // Pseudocode:
        // let simulator = Simulator::new(module);
        // simulator.add_input("a", Signal);
        // simulator.add_input("b", 0);
        // simulator.add_output("c");
        // simulator.run();
        // ot
        // simulator.run(ticks)
        // assert_eq!(simulator.get_output("c"), 0);
    }
}

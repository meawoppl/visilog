use std::{collections::HashMap, path::Path};

use crate::parsers::modules::VerilogModule;

pub struct VerilogBuildContext<'a> {
    top_module: &'a VerilogModule,
    modules: HashMap<String, &'a VerilogModule>,
}

impl<'a> VerilogBuildContext<'a> {
    pub fn from_files(files: Vec<Path>, top_name: String) -> Result<Self, String> {
        for file_path in &files {
            if !file_path.exists() {
                return Err(format!(
                    "Verilog source file not found: {}",
                    file_path.display()
                ));
            }
        }
        let mut modules = HashMap::new();

        // Read and parse each file
        for file_path in files {
            let content = std::fs::read_to_string(&file_path)
                .map_err(|e| format!("Failed to read {}: {}", file_path.display(), e))?;

            let parsed_modules = crate::parsers::verilog_file::parse_verilog_file(&content)?;

            // Add each module to the map
            for module in parsed_modules {
                let name = module.identifier.name.clone();
                if modules.insert(name.clone(), module).is_some() {
                    return Err(format!("Duplicate module definition found: {}", name));
                }
            }
        }

        // Find the top module
        let top_module = modules
            .get(&top_name)
            .ok_or_else(|| format!("Top module '{}' not found", top_name))?
            .clone();

        Ok(Self {
            top_module,
            modules,
        })
    }
}
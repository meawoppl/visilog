pub struct VerilogBuildContext {
    top_module: VerilogModule,
    modules: HashMap<String, VerilogModule>,
}

impl VerilogBuildContext {
    pub fn from_files(files: Vec<Path>, top_name: String) -> Self {
        for file_path in &files {
            if !file_path.exists() {
                return Err(format!(
                    "Verilog source file not found: {}",
                    file_path.display()
                ));
            }
        }

        

        let mut context = VerilogBuildContext {
            top_module: VerilogModule::default(),
            modules: HashMap::new(),
        };
        
    }
}
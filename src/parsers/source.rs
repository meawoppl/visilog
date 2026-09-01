use std::collections::HashMap;
use std::fmt;

use nom::{
    combinator::all_consuming,
    multi::many0,
    sequence::{preceded, terminated},
    IResult,
};

use super::{
    modules::{parse_module_declaration, VerilogModule},
    simple::ws_and_comments,
};

/// Parse a whole source file: zero or more module declarations, separated (and
/// surrounded) by any mix of whitespace and comments. The entire input has to be
/// consumed, so a trailing fragment that is not a module is a parse error rather
/// than a silently ignored remainder.
pub fn parse_verilog_source(input: &str) -> IResult<&str, Vec<VerilogModule>> {
    all_consuming(terminated(
        many0(preceded(ws_and_comments, parse_module_declaration)),
        ws_and_comments,
    ))(input)
}

/// The modules of a design, indexed by name, so an instantiation's `module_name`
/// can be resolved to its definition.
#[derive(Debug)]
pub struct ModuleLibrary {
    modules: Vec<VerilogModule>,
    by_name: HashMap<String, usize>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LibraryError {
    /// Two modules in the same design share a name.
    DuplicateModule(String),
    /// The source did not parse.
    Parse(String),
}

impl fmt::Display for LibraryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LibraryError::DuplicateModule(name) => {
                write!(f, "duplicate module definition: {}", name)
            }
            LibraryError::Parse(message) => write!(f, "failed to parse source: {}", message),
        }
    }
}

impl std::error::Error for LibraryError {}

impl ModuleLibrary {
    /// Index a list of modules by name, preserving source order.
    pub fn from_modules(modules: Vec<VerilogModule>) -> Result<Self, LibraryError> {
        let mut by_name = HashMap::with_capacity(modules.len());
        for (index, module) in modules.iter().enumerate() {
            let name = module.identifier.name.clone();
            if by_name.insert(name.clone(), index).is_some() {
                return Err(LibraryError::DuplicateModule(name));
            }
        }
        Ok(ModuleLibrary { modules, by_name })
    }

    /// Parse a source file and index every module it declares.
    pub fn from_source(source: &str) -> Result<Self, LibraryError> {
        let (_, modules) =
            parse_verilog_source(source).map_err(|err| LibraryError::Parse(err.to_string()))?;
        ModuleLibrary::from_modules(modules)
    }

    pub fn get(&self, name: &str) -> Option<&VerilogModule> {
        self.by_name.get(name).map(|&index| &self.modules[index])
    }

    /// Module names in source order.
    pub fn names(&self) -> Vec<&str> {
        self.modules
            .iter()
            .map(|module| module.identifier.name.as_str())
            .collect()
    }

    pub fn len(&self) -> usize {
        self.modules.len()
    }

    pub fn is_empty(&self) -> bool {
        self.modules.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::helpers::{assert_parses, assert_parses_to};
    use crate::parsers::statements::ModuleStatement;

    const TWO_MODULES: &str = r#"
        module adder(
            input [7:0] a,
            input [7:0] b,
            output [7:0] c
        );
            assign c = a + b;
        endmodule

        module inverter(
            input a,
            output b
        );
            assign b = ~a;
        endmodule
    "#;

    #[test]
    fn test_two_modules_in_one_file() {
        let modules = assert_parses(parse_verilog_source, TWO_MODULES);
        assert_eq!(modules.len(), 2);
        assert_eq!(modules[0].identifier, "adder".into());
        assert_eq!(modules[1].identifier, "inverter".into());
    }

    #[test]
    fn test_modules_separated_by_comments_and_blank_lines() {
        let source = r#"
            // A design with three modules.

            module a(input x, output y);
                assign y = x;
            endmodule

            /* A block comment
               spanning several lines. */

            module b(input x, output y);
                assign y = ~x;
            endmodule
            //
            // Back to back line comments, one of them empty.
            module c(input x, output y);
                assign y = x;
            endmodule

            // Trailing comment with no newline after it."#;

        let modules = assert_parses(parse_verilog_source, source);
        assert_eq!(
            modules
                .iter()
                .map(|module| module.identifier.name.as_str())
                .collect::<Vec<_>>(),
            vec!["a", "b", "c"]
        );
    }

    #[test]
    fn test_empty_file_parses_to_no_modules() {
        assert_parses_to(parse_verilog_source, "", vec![]);
    }

    #[test]
    fn test_whitespace_and_comment_only_file_parses_to_no_modules() {
        assert_parses_to(
            parse_verilog_source,
            "\n\n  // nothing here\n\n  /* nor here */\n",
            vec![],
        );
    }

    #[test]
    fn test_single_module_matches_the_module_parser() {
        let source = r#"
            module my_module (
                input wire a,
                output wire b
            );
                assign b = a;
            endmodule
        "#;

        let single = assert_parses(parse_module_declaration, source);
        let modules = assert_parses(parse_verilog_source, source);
        assert_eq!(modules, vec![single]);
    }

    #[test]
    fn test_trailing_garbage_is_an_error() {
        let source = r#"
            module a(input x, output y);
                assign y = x;
            endmodule

            not_a_module
        "#;

        assert!(parse_verilog_source(source).is_err());
    }

    #[test]
    fn test_dangling_module_keyword_is_an_error() {
        assert!(parse_verilog_source("module a(input x, output y);").is_err());
    }

    #[test]
    fn test_hierarchy_with_an_instantiation() {
        let source = r#"
            module counter(input clk, output [3:0] count);
                reg [3:0] count;
                always @(posedge clk) count <= count + 1;
            endmodule

            module top(input clk, output [3:0] value);
                counter c0 (.clk(clk), .count(value));
            endmodule
        "#;

        let modules = assert_parses(parse_verilog_source, source);
        assert_eq!(modules.len(), 2);

        let top = &modules[1];
        assert_eq!(top.identifier, "top".into());
        assert_eq!(top.statements.len(), 1);
        match &top.statements[0] {
            ModuleStatement::ModuleInstantiation(instantiation) => {
                assert_eq!(instantiation.module_name, "counter".into());
                assert_eq!(instantiation.instance_name, "c0".into());
            }
            other => panic!("Expected a module instantiation, got {:?}", other),
        }
    }

    #[test]
    fn test_library_indexes_modules_by_name() {
        let library = ModuleLibrary::from_source(TWO_MODULES).unwrap();

        assert_eq!(library.len(), 2);
        assert!(!library.is_empty());
        assert_eq!(library.names(), vec!["adder", "inverter"]);
        assert_eq!(
            library.get("inverter").unwrap().identifier,
            "inverter".into()
        );
        assert!(library.get("missing").is_none());
    }

    #[test]
    fn test_empty_library() {
        let library = ModuleLibrary::from_source("// just a comment").unwrap();

        assert!(library.is_empty());
        assert_eq!(library.len(), 0);
        assert!(library.names().is_empty());
    }

    #[test]
    fn test_duplicate_module_names_are_an_error() {
        let source = r#"
            module twice(input x, output y);
                assign y = x;
            endmodule

            module twice(input x, output y);
                assign y = ~x;
            endmodule
        "#;

        assert_eq!(
            ModuleLibrary::from_source(source).unwrap_err(),
            LibraryError::DuplicateModule("twice".to_string())
        );
    }

    #[test]
    fn test_library_reports_parse_failures() {
        let error = ModuleLibrary::from_source("not verilog at all").unwrap_err();
        assert!(matches!(error, LibraryError::Parse(_)));
    }

    #[test]
    fn test_library_error_display() {
        assert_eq!(
            LibraryError::DuplicateModule("dup".to_string()).to_string(),
            "duplicate module definition: dup"
        );
        assert!(LibraryError::Parse("boom".to_string())
            .to_string()
            .starts_with("failed to parse source:"));
    }
}

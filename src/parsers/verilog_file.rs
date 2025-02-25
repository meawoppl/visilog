use nom::multi::many1;

use super::{
    modules::{parse_module_declaration, VerilogModule},
    simple::ws,
};

pub fn parse_verilog_file(input: &str) -> Result<Vec<VerilogModule>, String> {
    let parse_result = many1(ws(parse_module_declaration))(input);
    match parse_result {
        Ok((remaining, modules)) => {
            if !remaining.trim().is_empty() {
                Err(format!("Unexpected input after modules: {}", remaining))
            } else {
                Ok(modules)
            }
        }
        Err(e) => Err(format!("Error parsing Verilog file: {}", e)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_verilog_file() {
        let input = "
            module test1;
            endmodule

            module test2;
            endmodule

            module test3(input a, output b);
            endmodule
        ";

        let parse_result = parse_verilog_file(input).unwrap();
        assert_eq!(parse_result.len(), 3);
        assert_eq!(parse_result[0].identifier.name, "test1");
        assert_eq!(parse_result[1].identifier.name, "test2");
        assert_eq!(parse_result[2].identifier.name, "test3");
        assert_eq!(parse_result[2].identifier.name, "test3");
        assert_eq!(parse_result[2].ports.len(), 2);
        assert_eq!(parse_result[2].ports[0].identifier.name, "a");
        assert_eq!(parse_result[2].ports[1].identifier.name, "b");
    }
}

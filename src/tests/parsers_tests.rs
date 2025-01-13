#[cfg(test)]
mod tests {
    use nom::Parser;

    use super::*;
    use crate::keywords::ALL_KEYWORDS;
    use crate::nets::parse_wire_statement;

    #[test]
    fn test_identifiers() {
        assert!(identifier("var_a").is_ok());
        assert!(identifier("$var_a").is_err());
        assert!(identifier("v$ar_a").is_ok());
        assert!(identifier("2var").is_err());
        assert!(identifier("var23_g").is_ok());
        assert!(identifier("23").is_err());
    }

    #[test]
    fn test_comments() {
        assert_eq!(comment("// This is a single line comment"), Ok(("", "")));
        assert_eq!(
            comment("/* This is a multi-line comment */"),
            Ok(("", " This is a multi-line comment "))
        );
        assert_eq!(
            comment("/* This is a \n multi-line \n comment */"),
            Ok(("", " This is a \n multi-line \n comment "))
        );
    }

    #[test]
    fn test_sized_bits() {
        assert_eq!(
            sized_const("3'b010"),
            Ok(("", ("3", VerilogBaseType::Binary, "010")))
        );
        assert_eq!(
            sized_const("3'd2"),
            Ok(("", ("3", VerilogBaseType::Decimal, "2")))
        );
        assert_eq!(
            sized_const("8'h70"),
            Ok(("", ("8", VerilogBaseType::Hexadecimal, "70")))
        );
        assert_eq!(
            sized_const("9'h1FA"),
            Ok(("", ("9", VerilogBaseType::Hexadecimal, "1FA")))
        );
        assert_eq!(
            sized_const("32'hFACE_47B2"),
            Ok(("", ("32", VerilogBaseType::Hexadecimal, "FACE_47B2")))
        );
        assert_eq!(
            sized_const("8'D234"),
            Ok(("", ("8", VerilogBaseType::Decimal, "234")))
        );
    }

    #[test]
    fn test_all_keywords() {
        for kw in ALL_KEYWORDS {
            assert!(
                keyword_from_string(kw).is_some(),
                "Keyword {} failed to parse",
                kw
            );
            keyword.parse(kw).unwrap();
        }
    }

    #[test]
    #[ignore]
    fn test_identifier_list() {
        identifier_list.parse("a").unwrap();
        identifier_list.parse("a, b, c").unwrap();
    }

    #[test]
    #[ignore]
    fn test_net_declaration() {
        net_declaration.parse("wire z").unwrap();
    }

    #[test]
    fn test_parse_wire_statement() {
        assert_eq!(
            parse_wire_statement("wire a;"),
            Ok((
                "",
                Net {
                    net_type: NetType::Wire,
                    range: None,
                    names: vec!["a".to_string()],
                }
            ))
        );
        assert_eq!(
            parse_wire_statement("wire [7:0] b;"),
            Ok((
                "",
                Net {
                    net_type: NetType::Wire,
                    range: Some((7, 0)),
                    names: vec!["b".to_string()],
                }
            ))
        );
        assert_eq!(
            parse_wire_statement("wire a, b, c;"),
            Ok((
                "",
                Net {
                    net_type: NetType::Wire,
                    range: None,
                    names: vec!["a".to_string(), "b".to_string(), "c".to_string()],
                }
            ))
        );
    }
}

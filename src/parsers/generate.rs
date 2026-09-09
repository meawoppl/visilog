//! `generate` regions, `genvar` declarations and `defparam`.
//!
//! A generate region is not a run-time construct: it is a description of what
//! the module *contains*, resolved once at elaboration against the parameters
//! in scope. So the grammar here only has to capture the shape — the loop, the
//! branch, the labels — and hand it to
//! [`elaborate`](crate::simulator::elaborate), which unrolls it into ordinary
//! module items. Nothing is decided in this file, deliberately: a loop bound
//! may be a parameter, and a parameter has no value until elaboration.

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::char,
    combinator::{map, not, opt, peek, value},
    multi::{many0, separated_list1},
    sequence::{delimited, preceded, terminated},
    IResult,
};

use super::{
    expr::{verilog_expression, Expression},
    identifier::{hierarchical_identifier, identifier, identifier_list, Identifier},
    simple::{ws, ws_and_comments},
    statements::{parse_module_statement, ModuleStatement},
};

/// One item legal inside a `generate … endgenerate` region.
///
/// [`Item`](GenerateItem::Item) is the ordinary case — anything a module body
/// may hold — and the other four are the constructs that only exist here.
#[derive(Debug, PartialEq)]
pub enum GenerateItem {
    Item(ModuleStatement),
    Block(GenerateBlock),
    Loop(GenerateLoop),
    If(GenerateIf),
    Case(GenerateCase),
}

/// A `begin [: name] … end` group, or a single item standing in for one.
///
/// The name matters: it becomes the scope every declaration inside the block
/// takes, and so the way a testbench reaches in — `stage[0].u.count`. An
/// unnamed block still gets a scope, since two iterations of an unnamed loop
/// body would otherwise declare the same names twice.
#[derive(Debug, PartialEq)]
pub struct GenerateBlock {
    pub name: Option<Identifier>,
    pub items: Vec<GenerateItem>,
}

/// `for (i = 0; i < 4; i = i + 1) begin : stage … end`.
///
/// The initialiser and the step are assignments to the loop's genvar, which is
/// an elaboration-time integer rather than a signal, so both are kept as the
/// variable they name plus the expression assigned to it.
#[derive(Debug, PartialEq)]
pub struct GenerateLoop {
    pub genvar: Identifier,
    pub init: Expression,
    pub condition: Expression,
    pub step_variable: Identifier,
    pub step: Expression,
    pub body: GenerateBlock,
}

/// `if (WIDTH > 8) begin : wide … end else … `.
#[derive(Debug, PartialEq)]
pub struct GenerateIf {
    pub condition: Expression,
    pub then_block: GenerateBlock,
    pub else_block: Option<GenerateBlock>,
}

/// One arm of a generate `case`. An empty label list is the `default` arm.
#[derive(Debug, PartialEq)]
pub struct GenerateCaseItem {
    pub labels: Vec<Expression>,
    pub block: GenerateBlock,
}

/// `case (MODE) 0: … default: … endcase`.
#[derive(Debug, PartialEq)]
pub struct GenerateCase {
    pub subject: Expression,
    pub items: Vec<GenerateCaseItem>,
}

/// `defparam dut.WIDTH = 8;` — one override, by the path it names.
///
/// The path is kept already flattened (`stage[0].u.WIDTH`), which is exactly
/// the spelling elaboration gives the parameter it overrides: hierarchy is
/// flattened into dotted names, so the two meet with no translation in
/// between.
#[derive(Debug, PartialEq)]
pub struct DefparamAssignment {
    pub path: String,
    pub value: Expression,
}

/// A keyword, with the word boundary that stops `generated` matching
/// `generate`.
fn keyword<'a>(input: &'a str, word: &str) -> IResult<&'a str, ()> {
    let (input, _) = ws_and_comments(input)?;
    let (input, _) = tag(word)(input)?;
    let (input, _) = peek(not(nom::character::complete::satisfy(|c: char| {
        c.is_alphanumeric() || c == '_' || c == '$'
    })))(input)?;
    Ok((input, ()))
}

/// `genvar i, j;` — the loop variables a generate region may use.
pub fn parse_genvar_declaration(input: &str) -> IResult<&str, Vec<Identifier>> {
    let (input, _) = keyword(input, "genvar")?;
    let (input, names) = ws(identifier_list)(input)?;
    let (input, _) = ws(char(';'))(input)?;
    Ok((input, names))
}

/// `defparam dut.WIDTH = 8, dut.DEPTH = 4;`
pub fn parse_defparam(input: &str) -> IResult<&str, Vec<DefparamAssignment>> {
    let (input, _) = keyword(input, "defparam")?;
    let (input, assignments) = separated_list1(ws(char(',')), defparam_assignment)(input)?;
    let (input, _) = ws(char(';'))(input)?;
    Ok((input, assignments))
}

fn defparam_assignment(input: &str) -> IResult<&str, DefparamAssignment> {
    let (input, path) = ws(hierarchical_identifier)(input)?;
    let (input, _) = ws(char('='))(input)?;
    let (input, value) = verilog_expression(input)?;
    Ok((
        input,
        DefparamAssignment {
            path: path.name,
            value,
        },
    ))
}

/// `generate … endgenerate`.
pub fn parse_generate_region(input: &str) -> IResult<&str, Vec<GenerateItem>> {
    let (input, _) = keyword(input, "generate")?;
    let (input, items) = generate_items(input)?;
    let (input, _) = keyword(input, "endgenerate")?;
    let (input, _) = ws_and_comments(input)?;
    Ok((input, items))
}

/// A run of generate items, skipping the null statements the grammar allows
/// between them — `default : ;` is a legal, empty arm.
fn generate_items(input: &str) -> IResult<&str, Vec<GenerateItem>> {
    let (input, items) = many0(alt((
        map(ws(char(';')), |_| None),
        map(generate_item, Some),
    )))(input)?;
    Ok((input, items.into_iter().flatten().collect()))
}

fn generate_item(input: &str) -> IResult<&str, GenerateItem> {
    // Every module statement parser expects to start on its first token —
    // `parse_module_declaration` wraps them in `ws` from the outside — so the
    // skipping has to happen here rather than inside the `alt`.
    let (input, _) = ws_and_comments(input)?;
    alt((
        map(generate_loop, GenerateItem::Loop),
        map(generate_if, GenerateItem::If),
        map(generate_case, GenerateItem::Case),
        map(generate_named_block, GenerateItem::Block),
        map(parse_module_statement, GenerateItem::Item),
    ))(input)
}

/// `begin [: name] … end`.
fn generate_named_block(input: &str) -> IResult<&str, GenerateBlock> {
    let (input, _) = keyword(input, "begin")?;
    let (input, name) = opt(preceded(ws(char(':')), identifier))(input)?;
    let (input, items) = generate_items(input)?;
    let (input, _) = keyword(input, "end")?;
    Ok((input, GenerateBlock { name, items }))
}

/// The body of a loop, a branch or a case arm: an explicit block, or a single
/// item wrapped in an unnamed one so that everything downstream sees one shape.
fn generate_body(input: &str) -> IResult<&str, GenerateBlock> {
    alt((
        generate_named_block,
        map(generate_item, |item| GenerateBlock {
            name: None,
            items: vec![item],
        }),
    ))(input)
}

/// `i = 0` — an assignment to a genvar, which carries no `;` of its own
/// because the loop header's two separators are the ones that terminate it.
fn genvar_assignment(input: &str) -> IResult<&str, (Identifier, Expression)> {
    let (input, name) = ws(identifier)(input)?;
    let (input, _) = ws(char('='))(input)?;
    let (input, value) = verilog_expression(input)?;
    Ok((input, (name, value)))
}

fn generate_loop(input: &str) -> IResult<&str, GenerateLoop> {
    let (input, _) = keyword(input, "for")?;
    let (input, _) = ws(char('('))(input)?;
    let (input, (genvar, init)) = genvar_assignment(input)?;
    let (input, _) = ws(char(';'))(input)?;
    let (input, condition) = ws(verilog_expression)(input)?;
    let (input, _) = ws(char(';'))(input)?;
    let (input, (step_variable, step)) = genvar_assignment(input)?;
    let (input, _) = ws(char(')'))(input)?;
    let (input, body) = generate_body(input)?;
    Ok((
        input,
        GenerateLoop {
            genvar,
            init,
            condition,
            step_variable,
            step,
            body,
        },
    ))
}

fn generate_if(input: &str) -> IResult<&str, GenerateIf> {
    let (input, _) = keyword(input, "if")?;
    let (input, condition) = delimited(ws(char('(')), verilog_expression, ws(char(')')))(input)?;
    let (input, then_block) = generate_body(input)?;
    let (input, else_block) = opt(preceded(|rest| keyword(rest, "else"), generate_body))(input)?;
    Ok((
        input,
        GenerateIf {
            condition,
            then_block,
            else_block,
        },
    ))
}

fn generate_case(input: &str) -> IResult<&str, GenerateCase> {
    let (input, _) = keyword(input, "case")?;
    let (input, subject) = delimited(ws(char('(')), verilog_expression, ws(char(')')))(input)?;
    let (input, items) = many0(generate_case_item)(input)?;
    let (input, _) = keyword(input, "endcase")?;
    Ok((input, GenerateCase { subject, items }))
}

fn generate_case_item(input: &str) -> IResult<&str, GenerateCaseItem> {
    let (input, labels) = alt((
        value(Vec::new(), |rest| keyword(rest, "default")),
        separated_list1(ws(char(',')), ws(verilog_expression)),
    ))(input)?;
    let (input, _) = ws(char(':'))(input)?;
    // An arm may be empty — `default : ;` — and a null statement leaves no
    // item behind, exactly as it does inside a procedural block.
    let (input, block) = alt((
        map(ws(char(';')), |_| GenerateBlock {
            name: None,
            items: Vec::new(),
        }),
        terminated(generate_body, opt(ws(char(';')))),
    ))(input)?;
    Ok((input, GenerateCaseItem { labels, block }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::helpers::assert_parses;

    #[test]
    fn test_parse_genvar_declaration() {
        assert_eq!(
            assert_parses(parse_genvar_declaration, "genvar i;"),
            vec![Identifier::from("i")]
        );
        assert_eq!(
            assert_parses(parse_genvar_declaration, "genvar i, j;").len(),
            2
        );
    }

    #[test]
    fn test_parse_defparam() {
        let assignments = assert_parses(parse_defparam, "defparam dut.WIDTH = 8;");
        assert_eq!(assignments[0].path, "dut.WIDTH");

        let indexed = assert_parses(parse_defparam, "defparam stage[0].u.WIDTH = 8;");
        assert_eq!(indexed[0].path, "stage[0].u.WIDTH");

        let several = assert_parses(parse_defparam, "defparam a.P = 1, b.Q = 2;");
        assert_eq!(several.len(), 2);
    }

    #[test]
    fn test_parse_generate_loop() {
        let items = assert_parses(
            parse_generate_region,
            r#"
            generate
              for (i = 0; i < 4; i = i + 1) begin : stage
                adder u (.a(x), .b(y));
              end
            endgenerate
            "#,
        );
        assert_eq!(items.len(), 1);
        match &items[0] {
            GenerateItem::Loop(loop_) => {
                assert_eq!(loop_.genvar, "i".into());
                assert_eq!(loop_.step_variable, "i".into());
                assert_eq!(loop_.body.name, Some("stage".into()));
                assert_eq!(loop_.body.items.len(), 1);
            }
            other => panic!("expected a loop, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_generate_if_and_case() {
        let items = assert_parses(
            parse_generate_region,
            r#"
            generate
              if (W > 8) begin : wide
                wire [7:0] extra;
              end else begin : narrow
                wire [3:0] extra;
              end
              case (MODE)
                0: begin : m0 assign q = a; end
                1, 2: assign q = b;
                default : ;
              endcase
            endgenerate
            "#,
        );
        assert_eq!(items.len(), 2);
        match &items[0] {
            GenerateItem::If(branch) => {
                assert_eq!(branch.then_block.name, Some("wide".into()));
                assert_eq!(
                    branch.else_block.as_ref().map(|block| &block.name),
                    Some(&Some("narrow".into()))
                );
            }
            other => panic!("expected an if, got {:?}", other),
        }
        match &items[1] {
            GenerateItem::Case(case) => {
                assert_eq!(case.items.len(), 3);
                assert_eq!(case.items[1].labels.len(), 2);
                assert!(case.items[2].labels.is_empty(), "the default arm");
                assert!(case.items[2].block.items.is_empty());
            }
            other => panic!("expected a case, got {:?}", other),
        }
    }

    /// A genvar declared inside the region, and a bare item as a loop body.
    #[test]
    fn test_parse_generate_bare_body() {
        let items = assert_parses(
            parse_generate_region,
            "generate genvar i; for (i = 0; i < 2; i = i + 1) assign y[i] = a[i]; endgenerate",
        );
        assert_eq!(items.len(), 2);
        assert!(matches!(items[0], GenerateItem::Item(_)));
        match &items[1] {
            GenerateItem::Loop(loop_) => {
                assert_eq!(loop_.body.name, None);
                assert_eq!(loop_.body.items.len(), 1);
            }
            other => panic!("expected a loop, got {:?}", other),
        }
    }
}

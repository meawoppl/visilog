mod git_utils;
mod parsers;
mod register;

fn main() {
    use parsers::behavior::{parse_initial_block, parse_always_block};

    let initial_block = r#"
        initial begin
            a = 'b1;
            b = 'b0;
        end
    "#;

    let always_block = r#"
        always begin
            #50 a = ~a;
        end
    "#;

    match parse_initial_block(initial_block) {
        Ok((_, assignments)) => {
            println!("Parsed initial block assignments: {:?}", assignments);
        }
        Err(e) => {
            println!("Failed to parse initial block: {:?}", e);
        }
    }

    match parse_always_block(always_block) {
        Ok((_, assignments)) => {
            println!("Parsed always block assignments: {:?}", assignments);
        }
        Err(e) => {
            println!("Failed to parse always block: {:?}", e);
        }
    }
}

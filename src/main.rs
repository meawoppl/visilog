mod keywords;
mod register;
mod expressions;
mod parsers;
mod utils;
mod nets;
mod git_utils;

use crate::keywords::VerilogKeyword;
use crate::register::Register;
use crate::expressions::{VerilogBinaryExpression, ReductionOperator};
use crate::parsers::*;
use crate::utils::*;
use crate::git_utils::shallow_clone_and_cache;

fn main() {
    println!("Hello, world!");

    // Test the shallow_clone_and_cache function
    let repo_url = "https://github.com/user/repo.git";
    let commit_hash = "abc123";
    let subdir = "src";

    match shallow_clone_and_cache(repo_url, commit_hash, subdir) {
        Ok(_) => println!("Shallow clone and cache successful"),
        Err(e) => eprintln!("Error: {}", e),
    }
}

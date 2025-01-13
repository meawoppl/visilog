mod expressions;
mod git_utils;
mod parsers;
mod register;

use crate::git_utils::shallow_clone_and_cache;
use crate::parsers::keywords::VerilogKeyword;
use crate::parsers::nets::net_type;

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

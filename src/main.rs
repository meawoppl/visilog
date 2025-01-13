mod keywords;
mod register;
mod expressions;
mod parsers;
mod utils;

use crate::keywords::VerilogKeyword;
use crate::register::Register;
use crate::expressions::{VerilogBinaryExpression, ReductionOperator};
use crate::parsers::*;
use crate::utils::*;

fn main() {
    println!("Hello, world!");
}

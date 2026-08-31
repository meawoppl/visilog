//! A Verilog parser and simulator.
//!
//! The crate is split into a parser front end ([`parsers`]) that turns Verilog
//! source into an AST, a four-state value type ([`register`]), and a
//! [`simulator`] that elaborates a parsed module and runs it.

pub mod git_utils;
pub mod parsers;
pub mod register;
pub mod simulator;

//! Whitespace syntax.
//!
//! This module parses and works with Whitespace syntax, including arbitrary
//! substitutions for the token patterns.

mod ast;
mod builder;
mod scanner;
#[cfg(test)]
mod tests;

pub use ast::*;
pub use builder::*;
pub use scanner::*;

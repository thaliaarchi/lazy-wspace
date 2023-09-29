//! Whitespace syntax.
//!
//! This module parses and works with Whitespace syntax, including arbitrary
//! substitutions for the token patterns.

mod ast;
mod builder;
pub mod lex;
#[cfg(test)]
mod tests;
mod token;

pub use ast::*;
pub use builder::*;
pub use token::*;

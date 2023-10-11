//! Whitespace syntax.
//!
//! This module parses and works with Whitespace syntax, including arbitrary
//! substitutions for the token patterns.

pub mod ast;
pub mod builder;
pub mod dialects;
pub mod lex;

#[cfg(test)]
mod tests;

//! Whitespace syntax.
//!
//! This module parses and works with Whitespace syntax, including arbitrary
//! substitutions for the token patterns.

mod ast;
mod scanner;

pub use ast::*;
pub use scanner::*;

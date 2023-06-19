//! Whitespace assembly syntax.
//!
//! This module parses and works with a permissive Whitespace assembly dialect.
//! It should be able to parse most code of any dialect.

mod lexer;
mod literals;

pub use lexer::*;
pub use literals::*;

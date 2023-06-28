//! Whitespace assembly syntax.
//!
//! This module parses and works with a permissive Whitespace assembly dialect.
//! It should be able to parse most code of any dialect.

pub mod burghard;
mod lexer;

pub use lexer::*;

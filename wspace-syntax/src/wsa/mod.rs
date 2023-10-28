//! Whitespace assembly syntax.
//!
//! The top-level type of the AST is [`Module`].

pub mod burghard;
pub mod compat;
pub mod whitelips;

mod ast;
mod dialect;

pub use ast::*;
pub use dialect::*;

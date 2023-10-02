use crate::ws::{lex::Span, Token};

/// Trait for token lexing.
pub trait Lexer: Iterator<Item = (Token, Span)> {}

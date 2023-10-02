use thiserror::Error;

use crate::ws::{lex::Span, Token};

/// Trait for token lexing.
pub trait Lexer: Iterator<Item = (Token, Span)> {}

#[derive(Clone, Copy, Debug, Error)]
#[error("conflicting patterns")]
pub struct ConflictingPatternError;

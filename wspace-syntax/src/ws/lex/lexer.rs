use std::fmt::{self, Debug, Formatter};
use std::iter::FusedIterator;

use crate::ws::lex::Span;
use crate::ws::{ExtToken, Token};

/// Trait for token lexing.
pub trait Lexer: Iterator<Item = (Token, Span)> {
    /// Converts this lexer into an [`ExtLexer`].
    #[inline]
    fn into_extended(self) -> ExtLexerAdaptor<Self>
    where
        Self: Sized,
    {
        ExtLexerAdaptor::new(self)
    }
}

/// Trait for token lexing, emitting extension tokens.
pub trait ExtLexer: Iterator<Item = (ExtToken, Span)> {}

/// Adaptor to convert a [`Lexer`] into an [`ExtLexer`]c.
pub struct ExtLexerAdaptor<L: Lexer> {
    inner: L,
}

impl<L: Lexer> ExtLexerAdaptor<L> {
    #[inline]
    pub fn new(lex: L) -> Self {
        ExtLexerAdaptor { inner: lex }
    }
}

impl<L: Lexer> ExtLexer for ExtLexerAdaptor<L> {}

impl<L: Lexer> Iterator for ExtLexerAdaptor<L> {
    type Item = (ExtToken, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let (tok, span) = self.inner.next()?;
        Some((ExtToken::Token(tok), span))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<L: Lexer + ExactSizeIterator> ExactSizeIterator for ExtLexerAdaptor<L> {
    #[inline]
    fn len(&self) -> usize {
        self.inner.len()
    }
}

impl<L: Lexer + FusedIterator> FusedIterator for ExtLexerAdaptor<L> {}

impl<L: Lexer> From<L> for ExtLexerAdaptor<L> {
    #[inline]
    fn from(lex: L) -> Self {
        ExtLexerAdaptor::new(lex)
    }
}

impl<L: Lexer + Clone> Clone for ExtLexerAdaptor<L> {
    #[inline]
    fn clone(&self) -> Self {
        ExtLexerAdaptor {
            inner: self.inner.clone(),
        }
    }
}

impl<L: Lexer + Debug> Debug for ExtLexerAdaptor<L> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExtLexerAdaptor")
            .field("inner", &self.inner)
            .finish()
    }
}

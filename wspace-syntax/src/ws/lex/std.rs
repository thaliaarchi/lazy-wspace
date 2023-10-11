//! Lexing for Whitespace tokens matching the reference interpreter.

use std::iter::FusedIterator;
use std::slice::Iter;
use std::str;

use crate::source::Span;
use crate::ws::lex::{ExtLexer, ExtToken, Token};

/// Lexer for Whitespace tokens, matching the reference interpreter.
#[derive(Clone, Debug)]
pub struct StdLexer<'a> {
    iter: Iter<'a, u8>,
    len: usize,
    invalid_utf8: Option<u8>,
}

impl<'a> StdLexer<'a> {
    #[inline]
    pub fn new(src: &'a str, invalid_utf8: Option<usize>) -> Self {
        StdLexer {
            iter: src.as_bytes().iter(),
            len: src.len(),
            invalid_utf8: invalid_utf8.map(|len| len as u8),
        }
    }
}

impl ExtLexer for StdLexer<'_> {}

impl Iterator for StdLexer<'_> {
    type Item = (ExtToken, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&b) = self.iter.next() {
            let tok = match b {
                b' ' => Token::S,
                b'\t' => Token::T,
                b'\n' => Token::L,
                _ => continue,
            };
            let end = self.len - self.iter.len();
            let start = end - 1;
            return Some((tok.into(), Span::from(start..end)));
        }
        if let Some(len) = self.invalid_utf8.take() {
            let end = self.len + len as usize;
            return Some((ExtToken::InvalidUtf8, Span::from(self.len..end)));
        }
        None
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.iter.len()))
    }
}

impl FusedIterator for StdLexer<'_> {}

impl<'a> From<&'a [u8]> for StdLexer<'a> {
    #[inline]
    fn from(src: &'a [u8]) -> Self {
        let (src, invalid_utf8) = match simdutf8::compat::from_utf8(src) {
            Ok(src) => (src, None),
            Err(err) => {
                let error_len = err.error_len().unwrap_or(src.len() - err.valid_up_to());
                // SAFETY: Guaranteed by API invariants.
                let src = unsafe { str::from_utf8_unchecked(&src[..err.valid_up_to()]) };
                (src, Some(error_len))
            }
        };
        StdLexer::new(src, invalid_utf8)
    }
}

impl<'a> From<&'a str> for StdLexer<'a> {
    #[inline]
    fn from(src: &'a str) -> Self {
        StdLexer::new(src, None)
    }
}

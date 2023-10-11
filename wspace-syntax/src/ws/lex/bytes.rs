//! Lexing for Whitespace tokens by byte strings.

use std::iter::FusedIterator;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, FindIter, MatchKind};

use crate::source::Span;
use crate::ws::lex::{Lexer, MatcherError, Token};

/// Builder for [`BytesLexer`].
#[derive(Clone, Debug)]
pub struct BytesMatcher {
    ac: AhoCorasick,
}

/// Lexer for Whitespace tokens represented by arbitrary byte strings.
#[derive(Debug)]
pub struct BytesLexer<'s, 'a> {
    iter: FindIter<'s, 'a>,
}

impl BytesMatcher {
    #[inline]
    pub fn new(s: &[u8], t: &[u8], l: &[u8]) -> Result<Self, MatcherError> {
        if s == t || s == l || t == l {
            return Err(MatcherError::ConflictingPatterns);
        }
        let ac = AhoCorasickBuilder::new()
            .match_kind(MatchKind::LeftmostLongest)
            .build(&[s, t, l])
            .unwrap();
        Ok(BytesMatcher { ac })
    }

    #[inline]
    pub fn lex<'s, 'a>(&'s self, src: &'a [u8]) -> BytesLexer<'s, 'a> {
        BytesLexer {
            iter: self.ac.find_iter(src),
        }
    }
}

impl Lexer for BytesLexer<'_, '_> {}

impl Iterator for BytesLexer<'_, '_> {
    type Item = (Token, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let m = self.iter.next()?;
        let tok = match m.pattern().as_u32() {
            0 => Token::S,
            1 => Token::T,
            2 => Token::L,
            _ => unreachable!(),
        };
        Some((tok, m.range().into()))
    }
}

impl FusedIterator for BytesLexer<'_, '_> {}

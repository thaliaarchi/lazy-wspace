use std::iter::FusedIterator;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, FindIter, MatchKind};

use crate::ws::lex::{Lexer, LexerError, Span};
use crate::ws::Token;

/// Lexer for Whitespace tokens, that recognizes byte string lexemes.
#[derive(Clone, Debug)]
pub struct BytesLexer {
    ac: AhoCorasick,
}

/// Iterator over tokens in source text, created by [`BytesLexer`].
#[derive(Debug)]
pub struct BytesIter<'l, 's> {
    iter: FindIter<'l, 's>,
}

impl BytesLexer {
    #[inline]
    pub fn new(s: &[u8], t: &[u8], l: &[u8]) -> Result<Self, LexerError> {
        if s == t || s == l || t == l {
            return Err(LexerError::ConflictingPatterns);
        }
        let ac = AhoCorasickBuilder::new()
            .match_kind(MatchKind::LeftmostLongest)
            .build(&[s, t, l])
            .unwrap();
        Ok(BytesLexer { ac })
    }

    #[inline]
    pub fn lex<'l, 's>(&'l self, src: &'s [u8]) -> BytesIter<'l, 's> {
        BytesIter {
            iter: self.ac.find_iter(src),
        }
    }
}

impl Lexer for BytesIter<'_, '_> {}

impl Iterator for BytesIter<'_, '_> {
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

impl FusedIterator for BytesIter<'_, '_> {}

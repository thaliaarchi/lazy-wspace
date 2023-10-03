use std::iter::FusedIterator;
use std::slice::Iter;

use crate::ws::lex::{Lexer, LexerError, Span};
use crate::ws::Token;

/// Lexer for Whitespace tokens, that recognizes byte lexemes.
#[derive(Clone, Debug)]
pub struct ByteLexer {
    s: u8,
    t: u8,
    l: u8,
}

/// Iterator over tokens in source text, created by [`ByteLexer`].
#[derive(Clone, Debug)]
pub struct ByteIter<'l, 's> {
    lex: &'l ByteLexer,
    iter: Iter<'s, u8>,
    len: usize,
}

impl ByteLexer {
    #[inline]
    pub fn new(s: u8, t: u8, l: u8) -> Result<Self, LexerError> {
        if s == t || s == l || t == l {
            return Err(LexerError::ConflictingPatterns);
        }
        Ok(ByteLexer { s, t, l })
    }

    #[inline]
    pub fn lex<'l, 's>(&'l self, src: &'s [u8]) -> ByteIter<'l, 's> {
        ByteIter {
            lex: self,
            iter: src.iter(),
            len: src.len(),
        }
    }
}

/// Constructs a lexer with the default space, tab, and LF lexemes.
impl Default for ByteLexer {
    #[inline]
    fn default() -> Self {
        ByteLexer {
            s: b' ',
            t: b'\t',
            l: b'\n',
        }
    }
}

impl Lexer for ByteIter<'_, '_> {}

impl Iterator for ByteIter<'_, '_> {
    type Item = (Token, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&ch) = self.iter.next() {
            let tok = if ch == self.lex.s {
                Token::S
            } else if ch == self.lex.t {
                Token::T
            } else if ch == self.lex.l {
                Token::L
            } else {
                continue;
            };
            let end = self.len - self.iter.len();
            let start = end - 1;
            return Some((tok, Span::from(start..end)));
        }
        None
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.iter.len()))
    }
}

impl FusedIterator for ByteIter<'_, '_> {}

#[test]
fn test_lex() {
    assert_eq!(
        vec![
            (Token::S, Span::from(1..2)),
            (Token::T, Span::from(2..3)),
            (Token::T, Span::from(3..4)),
            (Token::S, Span::from(4..5)),
            (Token::S, Span::from(6..7)),
            (Token::T, Span::from(8..9)),
            (Token::L, Span::from(10..11)),
        ],
        ByteLexer::new(b'A', b'B', b'C')
            .unwrap()
            .lex(b" ABBA A B C ")
            .collect::<Vec<_>>()
    );
}

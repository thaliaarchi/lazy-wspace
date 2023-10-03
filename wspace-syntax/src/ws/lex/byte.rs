use std::iter::FusedIterator;
use std::slice::Iter;

use crate::ws::lex::{Lexer, MatcherError, Span};
use crate::ws::Token;

/// Builder for [`ByteLexer`].
#[derive(Clone, Copy, Debug)]
pub struct ByteMatcher {
    s: u8,
    t: u8,
    l: u8,
}

/// Lexer for Whitespace tokens represented by arbitrary bytes.
#[derive(Clone, Debug)]
pub struct ByteLexer<'a> {
    matcher: ByteMatcher,
    iter: Iter<'a, u8>,
    len: usize,
}

impl ByteMatcher {
    #[inline]
    pub fn new(s: u8, t: u8, l: u8) -> Result<Self, MatcherError> {
        if s == t || s == l || t == l {
            return Err(MatcherError::ConflictingPatterns);
        }
        Ok(ByteMatcher { s, t, l })
    }

    #[inline]
    pub fn lex<'a>(&self, src: &'a [u8]) -> ByteLexer<'a> {
        ByteLexer {
            matcher: *self,
            iter: src.iter(),
            len: src.len(),
        }
    }

    #[inline]
    pub fn token(&self, b: u8) -> Option<Token> {
        if b == self.s {
            Some(Token::S)
        } else if b == self.t {
            Some(Token::T)
        } else if b == self.l {
            Some(Token::L)
        } else {
            None
        }
    }
}

/// Constructs a lexer with the default space, tab, and LF lexemes.
impl Default for ByteMatcher {
    #[inline]
    fn default() -> Self {
        ByteMatcher {
            s: b' ',
            t: b'\t',
            l: b'\n',
        }
    }
}

impl Lexer for ByteLexer<'_> {}

impl Iterator for ByteLexer<'_> {
    type Item = (Token, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&b) = self.iter.next() {
            if let Some(tok) = self.matcher.token(b) {
                let end = self.len - self.iter.len();
                let start = end - 1;
                return Some((tok, Span::from(start..end)));
            }
        }
        None
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.iter.len()))
    }
}

impl FusedIterator for ByteLexer<'_> {}

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
        ByteMatcher::new(b'A', b'B', b'C')
            .unwrap()
            .lex(b" ABBA A B C ")
            .collect::<Vec<_>>()
    );
}

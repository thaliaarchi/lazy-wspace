use std::iter::FusedIterator;
use std::str::Chars;

use crate::ws::lex::{Lexer, MatcherError, Span};
use crate::ws::Token;

/// Builder for [`CharLexer`].
#[derive(Clone, Copy, Debug)]
pub struct CharMatcher {
    s: char,
    t: char,
    l: char,
}

/// Lexer for Whitespace tokens represented by arbitrary chars.
#[derive(Clone, Debug)]
pub struct CharLexer<'s> {
    matcher: CharMatcher,
    iter: Chars<'s>,
    len: usize,
}

impl CharMatcher {
    #[inline]
    pub fn new(s: char, t: char, l: char) -> Result<Self, MatcherError> {
        if s == t || s == l || t == l {
            return Err(MatcherError::ConflictingPatterns);
        }
        Ok(CharMatcher { s, t, l })
    }

    #[inline]
    pub fn lex<'s>(&self, src: &'s str) -> CharLexer<'s> {
        CharLexer {
            matcher: *self,
            iter: src.chars(),
            len: src.len(),
        }
    }

    #[inline]
    pub fn token(&self, ch: char) -> Option<Token> {
        if ch == self.s {
            Some(Token::S)
        } else if ch == self.t {
            Some(Token::T)
        } else if ch == self.l {
            Some(Token::L)
        } else {
            None
        }
    }
}

impl Lexer for CharLexer<'_> {}

impl Iterator for CharLexer<'_> {
    type Item = (Token, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let pre_len = self.iter.as_str().len();
            let ch = self.iter.next()?;
            if let Some(tok) = self.matcher.token(ch) {
                let start = self.len - pre_len;
                let end = self.len - self.iter.as_str().len();
                return Some((tok, Span::from(start..end)));
            }
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.iter.as_str().len()))
    }
}

impl FusedIterator for CharLexer<'_> {}

#[test]
fn test_lex() {
    assert_eq!(
        vec![
            (Token::S, Span::from(0..4)),
            (Token::S, Span::from(4..8)),
            (Token::S, Span::from(9..13)),
            (Token::T, Span::from(14..18)),
            (Token::S, Span::from(18..22)),
            (Token::T, Span::from(22..26)),
            (Token::S, Span::from(26..30)),
            (Token::T, Span::from(30..34)),
            (Token::S, Span::from(34..38)),
            (Token::L, Span::from(39..43)),
        ],
        CharMatcher::new('𝓢', '𝓣', '𝓛')
            .unwrap()
            .lex("𝓢𝓢 𝓢 𝓣𝓢𝓣𝓢𝓣𝓢 𝓛\n")
            .collect::<Vec<_>>()
    );
}

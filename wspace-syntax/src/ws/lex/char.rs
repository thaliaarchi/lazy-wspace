use std::iter::FusedIterator;
use std::str::Chars;

use crate::ws::lex::{ConflictingPatternError, Lexer, Span};
use crate::ws::Token;

/// Lexer for Whitespace tokens, that recognizes `char` lexemes.
#[derive(Clone, Debug)]
pub struct CharLexer {
    s: char,
    t: char,
    l: char,
}

/// Iterator over tokens in source text, created by [`CharLexer`].
#[derive(Clone, Debug)]
pub struct CharIter<'l, 's> {
    lex: &'l CharLexer,
    iter: Chars<'s>,
    len: usize,
}

impl CharLexer {
    #[inline]
    pub fn new(s: char, t: char, l: char) -> Result<Self, ConflictingPatternError> {
        if s == t || s == l || t == l {
            return Err(ConflictingPatternError);
        }
        Ok(CharLexer { s, t, l })
    }

    #[inline]
    pub fn lex<'l, 's>(&'l self, src: &'s str) -> CharIter<'l, 's> {
        CharIter {
            lex: self,
            iter: src.chars(),
            len: src.len(),
        }
    }
}

impl Lexer for CharIter<'_, '_> {}

impl Iterator for CharIter<'_, '_> {
    type Item = (Token, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let pre_len = self.iter.as_str().len();
            let ch = self.iter.next()?;
            let tok = if ch == self.lex.s {
                Token::S
            } else if ch == self.lex.t {
                Token::T
            } else if ch == self.lex.l {
                Token::L
            } else {
                continue;
            };
            let start = self.len - pre_len;
            let end = self.len - self.iter.as_str().len();
            return Some((tok, Span::from(start..end)));
        }
    }
}

impl FusedIterator for CharIter<'_, '_> {}

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
        CharLexer::new('𝓢', '𝓣', '𝓛')
            .unwrap()
            .lex("𝓢𝓢 𝓢 𝓣𝓢𝓣𝓢𝓣𝓢 𝓛\n")
            .collect::<Vec<_>>()
    );
}

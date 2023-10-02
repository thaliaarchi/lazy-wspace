use std::iter::FusedIterator;

use regex_automata::{
    meta::{BuildError, FindMatches, Regex},
    nfa::thompson::WhichCaptures,
    util::syntax,
};
use thiserror::Error;

use crate::ws::lex::{Lexer, Span};
use crate::ws::Token;

/// Lexer for Whitespace tokens, that recognizes by regular expressions.
#[derive(Clone, Debug)]
pub struct RegexLexer {
    re: Regex,
    tokens: [Token; 3],
}

/// Iterator over tokens in source text, created by [`RegexLexer`].
#[derive(Debug)]
pub struct RegexIter<'l, 's> {
    iter: FindMatches<'l, 's>,
    tokens: [Token; 3],
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum RegexLexerError {
    #[error("multiple patterns defined for token")]
    RepeatedToken,
    #[error(transparent)]
    Build(#[from] BuildError),
}

impl RegexLexer {
    #[inline]
    pub fn new(
        token1: Token,
        pattern1: &str,
        token2: Token,
        pattern2: &str,
        token3: Token,
        pattern3: &str,
    ) -> Result<Self, RegexLexerError> {
        if token1 == token2 || token1 == token3 || token2 == token3 {
            return Err(RegexLexerError::RepeatedToken);
        }
        let tokens = [token1, token2, token3];
        let patterns = [pattern1, pattern2, pattern3];
        let re = Regex::builder()
            .syntax(syntax::Config::new().multi_line(true))
            .configure(Regex::config().which_captures(WhichCaptures::Implicit))
            .build_many(&patterns)?;
        Ok(RegexLexer { re, tokens })
    }

    #[inline]
    pub fn lex<'l, 's>(&'l self, src: &'s [u8]) -> RegexIter<'l, 's> {
        RegexIter {
            iter: self.re.find_iter(src),
            tokens: self.tokens,
        }
    }
}

impl Lexer for RegexIter<'_, '_> {}

impl Iterator for RegexIter<'_, '_> {
    type Item = (Token, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let m = self.iter.next()?;
        let tok = self.tokens[m.pattern().as_usize()];
        Some((tok, m.range().into()))
    }
}

impl FusedIterator for RegexIter<'_, '_> {}

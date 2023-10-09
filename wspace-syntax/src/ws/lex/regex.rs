//! Lexing for Whitespace tokens by regular expressions.

use std::iter::FusedIterator;

use regex_automata::{
    meta::{Builder as RegexBuilder, Config as MetaConfig, FindMatches, Regex},
    nfa::thompson::WhichCaptures,
    util::syntax::Config as SyntaxConfig,
    MatchKind,
};
use regex_syntax::hir::Hir;

use crate::ws::lex::{Lexer, MatcherError, Span};
use crate::ws::Token;

/// Builder for [`RegexLexer`].
#[derive(Clone, Debug)]
pub struct RegexMatcher {
    re: Regex,
    tokens: [Token; 3],
}

/// Lexer for Whitespace tokens represented by arbitrary regular expressions.
#[derive(Debug)]
pub struct RegexLexer<'s, 'a> {
    iter: FindMatches<'s, 'a>,
    tokens: [Token; 3],
}

impl RegexMatcher {
    #[inline]
    pub fn new(
        token1: Token,
        pattern1: &str,
        token2: Token,
        pattern2: &str,
        token3: Token,
        pattern3: &str,
        utf8: bool,
    ) -> Result<Self, MatcherError> {
        if token1 == token2 || token1 == token3 || token2 == token3 {
            return Err(MatcherError::RepeatedToken);
        }
        Ok(RegexMatcher {
            re: Self::builder(utf8).build_many(&[pattern1, pattern2, pattern3])?,
            tokens: [token1, token2, token3],
        })
    }

    #[inline]
    pub fn from_hirs(
        token1: Token,
        hir1: Hir,
        token2: Token,
        hir2: Hir,
        token3: Token,
        hir3: Hir,
        utf8: bool,
    ) -> Result<Self, MatcherError> {
        if token1 == token2 || token1 == token3 || token2 == token3 {
            return Err(MatcherError::RepeatedToken);
        }
        Ok(RegexMatcher {
            re: Self::builder(utf8).build_many_from_hir(&[hir1, hir2, hir3])?,
            tokens: [token1, token2, token3],
        })
    }

    pub(crate) fn syntax_config(utf8: bool) -> SyntaxConfig {
        SyntaxConfig::new().utf8(utf8).multi_line(true)
    }

    fn builder(utf8: bool) -> RegexBuilder {
        let metac = MetaConfig::new()
            .utf8_empty(utf8)
            .match_kind(MatchKind::LeftmostFirst)
            .which_captures(WhichCaptures::Implicit);
        let mut builder = Regex::builder();
        builder.syntax(Self::syntax_config(utf8));
        builder.configure(metac);
        builder
    }

    #[inline]
    pub fn lex<'s, 'a>(&'s self, src: &'a [u8]) -> RegexLexer<'s, 'a> {
        RegexLexer {
            iter: self.re.find_iter(src),
            tokens: self.tokens,
        }
    }
}

impl Lexer for RegexLexer<'_, '_> {}

impl Iterator for RegexLexer<'_, '_> {
    type Item = (Token, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let m = self.iter.next()?;
        let tok = self.tokens[m.pattern().as_usize()];
        Some((tok, m.range().into()))
    }
}

impl FusedIterator for RegexLexer<'_, '_> {}

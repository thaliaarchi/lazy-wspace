use std::borrow::Cow;
use std::iter::FusedIterator;

use regex_automata::meta::BuildError as RegexBuildError;
use regex_syntax::hir::{Hir, HirKind};
use thiserror::Error;

use crate::ws::lex::{
    ByteLexer, ByteMatcher, BytesLexer, BytesMatcher, Lexer, RegexLexer, RegexMatcher, Span,
};
use crate::ws::Token;

/// Builder for [`MetaLexer`].
#[derive(Clone, Debug)]
pub enum MetaMatcher {
    Byte(ByteMatcher),
    Bytes(BytesMatcher),
    Regex(RegexMatcher),
}

/// Lexer for Whitespace tokens represented by arbitrary patterns.
#[derive(Debug)]
pub enum MetaLexer<'s, 'a> {
    Byte(ByteLexer<'a>),
    Bytes(BytesLexer<'s, 'a>),
    Regex(RegexLexer<'s, 'a>),
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum MatcherError {
    #[error("multiple patterns defined for token")]
    RepeatedToken,
    #[error("conflicting patterns")]
    ConflictingPatterns,
    #[error(transparent)]
    RegexBuild(Box<RegexBuildError>),
}

impl MetaMatcher {
    pub fn new<'a>(
        token1: Token,
        pattern1: Pattern<'a>,
        token2: Token,
        pattern2: Pattern<'a>,
        token3: Token,
        pattern3: Pattern<'a>,
    ) -> Result<MetaMatcher, MatcherError> {
        if token1 == token2 || token1 == token3 || token2 == token3 {
            return Err(MatcherError::RepeatedToken);
        }

        match (
            pattern1.as_literal(),
            pattern2.as_literal(),
            pattern3.as_literal(),
        ) {
            (Some(lit1), Some(lit2), _)
            | (Some(lit1), _, Some(lit2))
            | (_, Some(lit1), Some(lit2))
                if lit1 == lit2 =>
            {
                return Err(MatcherError::ConflictingPatterns);
            }

            (Some(_), Some(_), Some(_)) => {
                let lit1 = pattern1.try_into_literal().unwrap();
                let lit2 = pattern2.try_into_literal().unwrap();
                let lit3 = pattern3.try_into_literal().unwrap();

                let mut lits = [(token1, lit1), (token2, lit2), (token3, lit3)];
                lits.sort_unstable_by_key(|&(tok, _)| tok as u8);
                let [(_, s), (_, t), (_, l)] = lits;

                if let (&[s], &[t], &[l]) = (&*s, &*t, &*l) {
                    let matcher = ByteMatcher::new(s, t, l)?;
                    Ok(MetaMatcher::Byte(matcher))
                } else {
                    let matcher = BytesMatcher::new(&*s, &*t, &*l)?;
                    Ok(MetaMatcher::Bytes(matcher))
                }
            }

            _ => {
                let hir1 = pattern1.into_hir();
                let hir2 = pattern2.into_hir();
                let hir3 = pattern3.into_hir();
                let matcher = RegexMatcher::from_hirs(token1, hir1, token2, hir2, token3, hir3)?;
                Ok(MetaMatcher::Regex(matcher))
            }
        }
    }

    pub fn lex<'l, 's>(&'l self, src: &'s [u8]) -> MetaLexer<'l, 's> {
        match self {
            MetaMatcher::Byte(inner) => MetaLexer::from(inner.lex(src)),
            MetaMatcher::Bytes(inner) => MetaLexer::from(inner.lex(src)),
            MetaMatcher::Regex(inner) => MetaLexer::from(inner.lex(src)),
        }
    }
}

impl From<ByteMatcher> for MetaMatcher {
    #[inline]
    fn from(matcher: ByteMatcher) -> Self {
        MetaMatcher::Byte(matcher)
    }
}

impl From<BytesMatcher> for MetaMatcher {
    #[inline]
    fn from(matcher: BytesMatcher) -> Self {
        MetaMatcher::Bytes(matcher)
    }
}

impl From<RegexMatcher> for MetaMatcher {
    #[inline]
    fn from(matcher: RegexMatcher) -> Self {
        MetaMatcher::Regex(matcher)
    }
}

impl Lexer for MetaLexer<'_, '_> {}

impl Iterator for MetaLexer<'_, '_> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MetaLexer::Byte(inner) => inner.next(),
            MetaLexer::Bytes(inner) => inner.next(),
            MetaLexer::Regex(inner) => inner.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            MetaLexer::Byte(inner) => inner.size_hint(),
            MetaLexer::Bytes(inner) => inner.size_hint(),
            MetaLexer::Regex(inner) => inner.size_hint(),
        }
    }
}

impl FusedIterator for MetaLexer<'_, '_> {}

impl<'a> From<ByteLexer<'a>> for MetaLexer<'static, 'a> {
    #[inline]
    fn from(lex: ByteLexer<'a>) -> Self {
        MetaLexer::Byte(lex)
    }
}

impl<'l, 's> From<BytesLexer<'l, 's>> for MetaLexer<'l, 's> {
    #[inline]
    fn from(lex: BytesLexer<'l, 's>) -> Self {
        MetaLexer::Bytes(lex)
    }
}

impl<'l, 's> From<RegexLexer<'l, 's>> for MetaLexer<'l, 's> {
    #[inline]
    fn from(lex: RegexLexer<'l, 's>) -> Self {
        MetaLexer::Regex(lex)
    }
}

// TODO: how to do UTF-8 vs bytes?
#[derive(Clone, Debug)]
pub enum Pattern<'a> {
    Literal(Cow<'a, [u8]>),
    Regex(Hir),
}

impl<'a> Pattern<'a> {
    fn as_literal(&self) -> Option<&[u8]> {
        match self {
            Pattern::Literal(lit) => Some(lit.as_ref()),
            Pattern::Regex(hir) => match hir.kind() {
                HirKind::Literal(lit) => Some(&lit.0),
                _ => None,
            },
        }
    }

    fn try_into_literal(self) -> Option<Cow<'a, [u8]>> {
        match self {
            Pattern::Literal(lit) => Some(lit),
            Pattern::Regex(hir) => match hir.kind() {
                HirKind::Literal(_) => {
                    if let HirKind::Literal(lit) = hir.into_kind() {
                        Some(lit.0.into_vec().into())
                    } else {
                        unreachable!()
                    }
                }
                _ => None,
            },
        }
    }

    fn into_hir(self) -> Hir {
        match self {
            Pattern::Literal(lit) => Hir::literal(lit.into_owned()),
            Pattern::Regex(hir) => hir,
        }
    }
}

impl From<RegexBuildError> for MatcherError {
    #[inline]
    fn from(err: RegexBuildError) -> Self {
        MatcherError::RegexBuild(Box::new(err))
    }
}

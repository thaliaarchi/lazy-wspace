use std::borrow::Cow;
use std::iter::FusedIterator;

use regex_automata::meta::BuildError as RegexBuildError;
use regex_syntax::hir::{Hir, HirKind};
use thiserror::Error;

use crate::ws::lex::{
    ByteIter, ByteLexer, BytesIter, BytesLexer, Lexer, RegexIter, RegexLexer, Span,
};
use crate::ws::Token;

#[derive(Clone, Debug)]
pub enum MetaLexer {
    Byte(ByteLexer),
    Bytes(BytesLexer),
    Regex(RegexLexer),
}

#[derive(Debug)]
pub enum MetaIter<'l, 's> {
    Byte(ByteIter<'l, 's>),
    Bytes(BytesIter<'l, 's>),
    Regex(RegexIter<'l, 's>),
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum LexerError {
    #[error("multiple patterns defined for token")]
    RepeatedToken,
    #[error("conflicting patterns")]
    ConflictingPatterns,
    #[error(transparent)]
    RegexBuild(#[from] RegexBuildError),
}

impl MetaLexer {
    pub fn new<'a>(
        token1: Token,
        pattern1: Pattern<'a>,
        token2: Token,
        pattern2: Pattern<'a>,
        token3: Token,
        pattern3: Pattern<'a>,
    ) -> Result<MetaLexer, LexerError> {
        if token1 == token2 || token1 == token3 || token2 == token3 {
            return Err(LexerError::RepeatedToken);
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
                return Err(LexerError::ConflictingPatterns);
            }

            (Some(_), Some(_), Some(_)) => {
                let lit1 = pattern1.try_into_literal().unwrap();
                let lit2 = pattern2.try_into_literal().unwrap();
                let lit3 = pattern3.try_into_literal().unwrap();

                let mut lits = [(token1, lit1), (token2, lit2), (token3, lit3)];
                lits.sort_unstable_by_key(|&(tok, _)| tok as u8);
                let [(_, s), (_, t), (_, l)] = lits;

                if let (&[s], &[t], &[l]) = (&*s, &*t, &*l) {
                    let lex = ByteLexer::new(s, t, l)?;
                    Ok(MetaLexer::Byte(lex))
                } else {
                    let lex = BytesLexer::new(&*s, &*t, &*l)?;
                    Ok(MetaLexer::Bytes(lex))
                }
            }

            _ => {
                let hir1 = pattern1.into_hir();
                let hir2 = pattern2.into_hir();
                let hir3 = pattern3.into_hir();
                let lex = RegexLexer::from_hirs(token1, hir1, token2, hir2, token3, hir3)?;
                Ok(MetaLexer::Regex(lex))
            }
        }
    }

    pub fn lex<'l, 's>(&'l self, src: &'s [u8]) -> MetaIter<'l, 's> {
        match self {
            MetaLexer::Byte(inner) => MetaIter::from(inner.lex(src)),
            MetaLexer::Bytes(inner) => MetaIter::from(inner.lex(src)),
            MetaLexer::Regex(inner) => MetaIter::from(inner.lex(src)),
        }
    }
}

impl From<ByteLexer> for MetaLexer {
    #[inline]
    fn from(lex: ByteLexer) -> Self {
        MetaLexer::Byte(lex)
    }
}

impl From<BytesLexer> for MetaLexer {
    #[inline]
    fn from(lex: BytesLexer) -> Self {
        MetaLexer::Bytes(lex)
    }
}

impl From<RegexLexer> for MetaLexer {
    #[inline]
    fn from(lex: RegexLexer) -> Self {
        MetaLexer::Regex(lex)
    }
}

impl Lexer for MetaIter<'_, '_> {}

impl Iterator for MetaIter<'_, '_> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MetaIter::Byte(inner) => inner.next(),
            MetaIter::Bytes(inner) => inner.next(),
            MetaIter::Regex(inner) => inner.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            MetaIter::Byte(inner) => inner.size_hint(),
            MetaIter::Bytes(inner) => inner.size_hint(),
            MetaIter::Regex(inner) => inner.size_hint(),
        }
    }
}

impl FusedIterator for MetaIter<'_, '_> {}

impl<'l, 's> From<ByteIter<'l, 's>> for MetaIter<'l, 's> {
    #[inline]
    fn from(iter: ByteIter<'l, 's>) -> Self {
        MetaIter::Byte(iter)
    }
}

impl<'l, 's> From<BytesIter<'l, 's>> for MetaIter<'l, 's> {
    #[inline]
    fn from(iter: BytesIter<'l, 's>) -> Self {
        MetaIter::Bytes(iter)
    }
}

impl<'l, 's> From<RegexIter<'l, 's>> for MetaIter<'l, 's> {
    #[inline]
    fn from(iter: RegexIter<'l, 's>) -> Self {
        MetaIter::Regex(iter)
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

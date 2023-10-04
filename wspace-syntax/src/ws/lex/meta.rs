use std::borrow::Cow;
use std::iter::FusedIterator;
use std::str;

use bitvec::order::{Lsb0, Msb0};
use regex_automata::meta::BuildError as RegexBuildError;
use regex_syntax::hir::{Hir, HirKind};
use thiserror::Error;

use crate::ws::lex::{
    BitLexer, ByteLexer, ByteMatcher, BytesLexer, BytesMatcher, CharLexer, CharMatcher,
    DynBitLexer, DynBitOrder, ExtLexer, RegexLexer, RegexMatcher, Span,
};
use crate::ws::{ExtToken, Token};

/// Builder for [`MetaLexer`].
#[derive(Clone, Debug)]
pub struct MetaMatcher {
    inner: MetaMatcherRepr,
    encoding: Encoding,
}

#[derive(Clone, Debug)]
enum MetaMatcherRepr {
    Byte(ByteMatcher),
    Char(CharMatcher),
    Bytes(BytesMatcher),
    Regex(RegexMatcher),
    Bit(DynBitOrder),
}

/// Lexer for Whitespace tokens represented by arbitrary patterns.
#[derive(Debug)]
pub struct MetaLexer<'s, 'a> {
    inner: MetaLexerRepr<'s, 'a>,
    invalid_utf8: Option<Span>,
}

#[derive(Debug)]
enum MetaLexerRepr<'s, 'a> {
    Byte(ByteLexer<'a>),
    Char(CharLexer<'a>),
    Bytes(BytesLexer<'s, 'a>),
    Regex(RegexLexer<'s, 'a>),
    BitLsb0(BitLexer<'a, Lsb0>),
    BitMsb0(BitLexer<'a, Msb0>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Encoding {
    Bytes,
    Utf8,
    LazyUtf8,
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

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum LexerError {
    #[error("invalid UTF-8 sequence")]
    InvalidUtf8,
}

impl MetaMatcher {
    pub fn new<'a>(
        token1: Token,
        pattern1: Pattern<'a>,
        token2: Token,
        pattern2: Pattern<'a>,
        token3: Token,
        pattern3: Pattern<'a>,
        encoding: Encoding,
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
                Err(MatcherError::ConflictingPatterns)
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
                    Ok(MetaMatcher::new_byte(matcher, encoding))
                } else {
                    let matcher = BytesMatcher::new(&*s, &*t, &*l)?;
                    Ok(MetaMatcher::new_bytes(matcher, encoding))
                }
            }

            _ => {
                let hir1 = pattern1.into_hir();
                let hir2 = pattern2.into_hir();
                let hir3 = pattern3.into_hir();
                let matcher = RegexMatcher::from_hirs(token1, hir1, token2, hir2, token3, hir3)?;
                Ok(MetaMatcher::new_regex(matcher, encoding))
            }
        }
    }

    #[inline]
    pub fn new_byte(matcher: ByteMatcher, encoding: Encoding) -> Self {
        MetaMatcher {
            inner: MetaMatcherRepr::Byte(matcher),
            encoding,
        }
    }

    #[inline]
    pub fn new_char(matcher: CharMatcher, encoding: Encoding) -> Self {
        if encoding == Encoding::Bytes {
            panic!("char lexing requires UTF-8");
        }
        MetaMatcher {
            inner: MetaMatcherRepr::Char(matcher),
            encoding,
        }
    }

    #[inline]
    pub fn new_bytes(matcher: BytesMatcher, encoding: Encoding) -> Self {
        MetaMatcher {
            inner: MetaMatcherRepr::Bytes(matcher),
            encoding,
        }
    }

    #[inline]
    pub fn new_regex(matcher: RegexMatcher, encoding: Encoding) -> Self {
        MetaMatcher {
            inner: MetaMatcherRepr::Regex(matcher),
            encoding,
        }
    }

    #[inline]
    pub fn new_bit(order: DynBitOrder, encoding: Encoding) -> Self {
        if encoding != Encoding::Bytes {
            panic!("bit lexing requires bytes");
        }
        MetaMatcher {
            inner: MetaMatcherRepr::Bit(order),
            encoding,
        }
    }

    pub fn lex<'s, 'a>(&'s self, src: &'a [u8]) -> Result<MetaLexer<'s, 'a>, LexerError> {
        let (src_str, invalid_utf8) = match self.encoding {
            Encoding::Bytes => (None, None),
            Encoding::Utf8 => {
                let src = simdutf8::basic::from_utf8(src)?;
                (Some(src), None)
            }
            Encoding::LazyUtf8 => match simdutf8::compat::from_utf8(src) {
                Ok(src) => (Some(src), None),
                Err(err) => {
                    let valid_prefix =
                        unsafe { str::from_utf8_unchecked(&src[..err.valid_up_to()]) };
                    let err_start = err.valid_up_to();
                    let err_end = err
                        .error_len()
                        .map(|len| err_start + len)
                        .unwrap_or(src.len());
                    (Some(valid_prefix), Some(Span::from(err_start..err_end)))
                }
            },
        };
        let src = src_str.map(|s| s.as_bytes()).unwrap_or(src);
        let inner = match &self.inner {
            MetaMatcherRepr::Byte(inner) => MetaLexerRepr::Byte(inner.lex(src)),
            MetaMatcherRepr::Char(inner) => {
                MetaLexerRepr::Char(inner.lex(src_str.expect("char lexing requires UTF-8")))
            }
            MetaMatcherRepr::Bytes(inner) => MetaLexerRepr::Bytes(inner.lex(src)),
            MetaMatcherRepr::Regex(inner) => MetaLexerRepr::Regex(inner.lex(src)),
            MetaMatcherRepr::Bit(order) => match order.lex(src) {
                DynBitLexer::Lsb0(lex) => MetaLexerRepr::BitLsb0(lex),
                DynBitLexer::Msb0(lex) => MetaLexerRepr::BitMsb0(lex),
            },
        };
        Ok(MetaLexer {
            inner,
            invalid_utf8,
        })
    }
}

impl ExtLexer for MetaLexer<'_, '_> {}

impl Iterator for MetaLexer<'_, '_> {
    type Item = (ExtToken, Span);

    fn next(&mut self) -> Option<Self::Item> {
        let res = match &mut self.inner {
            MetaLexerRepr::Byte(inner) => inner.next(),
            MetaLexerRepr::Char(inner) => inner.next(),
            MetaLexerRepr::Bytes(inner) => inner.next(),
            MetaLexerRepr::Regex(inner) => inner.next(),
            MetaLexerRepr::BitLsb0(inner) => inner.next(),
            MetaLexerRepr::BitMsb0(inner) => inner.next(),
        };
        res.map(|(tok, span)| (tok.into(), span)).or_else(|| {
            self.invalid_utf8
                .take()
                .map(|span| (ExtToken::InvalidUtf8, span))
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lo, hi) = match &self.inner {
            MetaLexerRepr::Byte(inner) => inner.size_hint(),
            MetaLexerRepr::Char(inner) => inner.size_hint(),
            MetaLexerRepr::Bytes(inner) => inner.size_hint(),
            MetaLexerRepr::Regex(inner) => inner.size_hint(),
            MetaLexerRepr::BitLsb0(inner) => inner.size_hint(),
            MetaLexerRepr::BitMsb0(inner) => inner.size_hint(),
        };
        let has_err = self.invalid_utf8.is_some() as usize;
        (lo + has_err, hi.map(|hi| hi + has_err))
    }
}

impl FusedIterator for MetaLexer<'_, '_> {}

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

impl From<simdutf8::basic::Utf8Error> for LexerError {
    #[inline]
    fn from(_err: simdutf8::basic::Utf8Error) -> Self {
        LexerError::InvalidUtf8
    }
}

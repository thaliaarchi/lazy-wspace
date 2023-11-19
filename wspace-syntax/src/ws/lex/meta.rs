use std::iter::FusedIterator;
use std::str;
use std::sync::OnceLock;

use bitvec::order::{Lsb0, Msb0};
use regex::{Captures, Regex};
use regex_syntax::hir::{Hir, HirKind};
use thiserror::Error;

use crate::source::Span;
use crate::util::regex_string::{unescape_byte_string, unescape_string, EscapeError};
use crate::ws::lex::{
    bit::{BitLexer, DynBitLexer, DynBitOrder},
    byte::{ByteLexer, ByteMatcher},
    bytes::{BytesLexer, BytesMatcher},
    char::{CharLexer, CharMatcher},
    regex::{RegexLexer, RegexMatcher},
    ExtLexer, ExtToken, Token,
};

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

#[derive(Clone, Debug)]
pub enum Pattern {
    Literal(Vec<u8>),
    Regex(Hir),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Encoding {
    Utf8,
    LazyUtf8,
    Bytes,
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum MatcherError {
    #[error("invalid syntax for matcher")]
    ParseMatcher,
    #[error("invalid token")]
    ParseToken,
    #[error("invalid syntax for pattern")]
    ParsePattern,
    #[error("parsing literal pattern: {0}")]
    ParseLiteralPattern(Box<EscapeError>),
    #[error("parsing regex pattern: {0}")]
    ParseRegexPattern(Box<regex_syntax::Error>),
    #[error("invalid encoding")]
    ParseEncoding,

    #[error("multiple patterns defined for token")]
    RepeatedToken,
    #[error("conflicting patterns")]
    ConflictingPatterns,
    #[error(transparent)]
    BuildRegex(Box<regex_automata::meta::BuildError>),
}

#[derive(Clone, Copy, Debug, Error, PartialEq, Eq)]
#[non_exhaustive]
pub enum LexerError {
    #[error("invalid UTF-8 sequence")]
    InvalidUtf8,
}

static META_RE: OnceLock<Regex> = OnceLock::new();

impl MetaMatcher {
    pub fn new(patterns: &str) -> Result<MetaMatcher, MatcherError> {
        fn get_cap<'a>(caps: &Captures<'a>, index: usize) -> Result<&'a str, MatcherError> {
            caps.get(index)
                .map(|m| m.as_str())
                .ok_or(MatcherError::ParseMatcher)
        }
        fn parse_token(s: &str) -> Result<Token, MatcherError> {
            match s {
                "S" => Ok(Token::S),
                "T" => Ok(Token::T),
                "L" => Ok(Token::L),
                _ => Err(MatcherError::ParseToken),
            }
        }
        fn parse_pattern(s: &str, enc: Encoding) -> Result<Pattern, MatcherError> {
            if s.len() >= 2 {
                let b = s.as_bytes();
                if b[0] == b'"' && b[b.len() - 1] == b'"' {
                    let s = &s[1..s.len() - 1];
                    let lit = match enc {
                        Encoding::Bytes => unescape_byte_string(s)?,
                        Encoding::Utf8 | Encoding::LazyUtf8 => unescape_string(s)?.into_bytes(),
                    };
                    return Ok(Pattern::Literal(lit));
                }
                if b[0] == b'/' && b[b.len() - 1] == b'/' {
                    let pattern = &s[1..s.len() - 1];
                    let utf8 = match enc {
                        Encoding::Utf8 | Encoding::LazyUtf8 => true,
                        Encoding::Bytes => false,
                    };
                    let hir = regex_automata::util::syntax::parse_with(
                        pattern,
                        &RegexMatcher::syntax_config(utf8),
                    )?;
                    return Ok(Pattern::Regex(hir));
                }
            }
            Err(MatcherError::ParsePattern)
        }

        let re = META_RE.get_or_init(|| {
            Regex::new(
                r#"(?x)
                ^
                \s* (\w+) \s* = \s* ("(?:[^"] | \")*" | /(?:[^/] | \/)*/) \s* ,
                \s* (\w+) \s* = \s* ("(?:[^"] | \")*" | /(?:[^/] | \/)*/) \s* ,
                \s* (\w+) \s* = \s* ("(?:[^"] | \")*" | /(?:[^/] | \/)*/) \s*
                (?:, \s*(\w+)\w* )?
                $
                "#,
            )
            .unwrap()
        });

        let caps = re.captures(patterns).ok_or(MatcherError::ParseMatcher)?;
        let enc = match get_cap(&caps, 7) {
            Ok(s) => match s {
                "utf8" => Encoding::Utf8,
                "lazy-utf8" => Encoding::LazyUtf8,
                "bytes" => Encoding::Bytes,
                _ => return Err(MatcherError::ParseEncoding),
            },
            Err(_) => Encoding::Utf8,
        };
        let tok1 = get_cap(&caps, 1).and_then(parse_token)?;
        let pattern1 = get_cap(&caps, 2).and_then(|s| parse_pattern(s, enc))?;
        let tok2 = get_cap(&caps, 3).and_then(parse_token)?;
        let pattern2 = get_cap(&caps, 4).and_then(|s| parse_pattern(s, enc))?;
        let tok3 = get_cap(&caps, 5).and_then(parse_token)?;
        let pattern3 = get_cap(&caps, 6).and_then(|s| parse_pattern(s, enc))?;

        MetaMatcher::from_patterns(tok1, pattern1, tok2, pattern2, tok3, pattern3, enc)
    }

    pub fn from_patterns(
        token1: Token,
        pattern1: Pattern,
        token2: Token,
        pattern2: Pattern,
        token3: Token,
        pattern3: Pattern,
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
                    Ok(MetaMatcher::from_byte(matcher, encoding))
                } else if let (Some(sc), Some(tc), Some(lc)) =
                    (bytes_to_char(&s), bytes_to_char(&t), bytes_to_char(&l))
                {
                    if encoding.is_utf8() {
                        let matcher = CharMatcher::new(sc, tc, lc)?;
                        Ok(MetaMatcher::from_char(matcher, encoding))
                    } else {
                        // TODO: Make a char matcher for non-UTF-8 input.
                        let matcher = BytesMatcher::new(&s, &t, &l)?;
                        Ok(MetaMatcher::from_bytes(matcher, encoding))
                    }
                } else {
                    let matcher = BytesMatcher::new(&s, &t, &l)?;
                    Ok(MetaMatcher::from_bytes(matcher, encoding))
                }
            }

            _ => {
                let matcher = RegexMatcher::from_hirs(
                    token1,
                    pattern1.into_hir(),
                    token2,
                    pattern2.into_hir(),
                    token3,
                    pattern3.into_hir(),
                    encoding.is_utf8(),
                )?;
                Ok(MetaMatcher::from_regex(matcher, encoding))
            }
        }
    }

    #[inline]
    pub fn from_byte(matcher: ByteMatcher, encoding: Encoding) -> Self {
        MetaMatcher {
            inner: MetaMatcherRepr::Byte(matcher),
            encoding,
        }
    }

    #[inline]
    pub fn from_char(matcher: CharMatcher, encoding: Encoding) -> Self {
        assert_ne!(encoding, Encoding::Bytes, "char lexing requires UTF-8");
        MetaMatcher {
            inner: MetaMatcherRepr::Char(matcher),
            encoding,
        }
    }

    #[inline]
    pub fn from_bytes(matcher: BytesMatcher, encoding: Encoding) -> Self {
        MetaMatcher {
            inner: MetaMatcherRepr::Bytes(matcher),
            encoding,
        }
    }

    #[inline]
    pub fn from_regex(matcher: RegexMatcher, encoding: Encoding) -> Self {
        MetaMatcher {
            inner: MetaMatcherRepr::Regex(matcher),
            encoding,
        }
    }

    #[inline]
    pub fn from_bit(order: DynBitOrder, encoding: Encoding) -> Self {
        assert_eq!(encoding, Encoding::Bytes, "bit lexing requires bytes");
        MetaMatcher {
            inner: MetaMatcherRepr::Bit(order),
            encoding,
        }
    }

    pub fn lex<'s, 'a>(&'s self, src: &'a [u8]) -> Result<MetaLexer<'s, 'a>, LexerError> {
        let (src_str, invalid_utf8) = match self.encoding {
            Encoding::Bytes => (None, None),
            Encoding::Utf8 => {
                let src = simdutf8::basic::from_utf8(src).map_err(|_| LexerError::InvalidUtf8)?;
                (Some(src), None)
            }
            Encoding::LazyUtf8 => match simdutf8::compat::from_utf8(src) {
                Ok(src) => (Some(src), None),
                Err(err) => {
                    // SAFETY: Prefix is guaranteed valid.
                    let valid_prefix =
                        unsafe { str::from_utf8_unchecked(&src[..err.valid_up_to()]) };
                    let err_start = err.valid_up_to();
                    let err_end = err.error_len().map_or(src.len(), |len| err_start + len);
                    (Some(valid_prefix), Some(Span::from(err_start..err_end)))
                }
            },
        };
        let src = src_str.map_or(src, str::as_bytes);
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

impl Pattern {
    fn as_literal(&self) -> Option<&[u8]> {
        match self {
            Pattern::Literal(lit) => Some(lit.as_ref()),
            Pattern::Regex(hir) => match hir.kind() {
                HirKind::Literal(lit) => Some(&lit.0),
                _ => None,
            },
        }
    }

    fn try_into_literal(self) -> Option<Vec<u8>> {
        match self {
            Pattern::Literal(lit) => Some(lit),
            Pattern::Regex(hir) => match hir.kind() {
                HirKind::Literal(_) => {
                    if let HirKind::Literal(lit) = hir.into_kind() {
                        Some(lit.0.into_vec())
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
            Pattern::Literal(lit) => Hir::literal(lit),
            Pattern::Regex(hir) => hir,
        }
    }
}

impl Encoding {
    #[inline]
    pub fn is_utf8(&self) -> bool {
        matches!(self, Encoding::Utf8 | Encoding::LazyUtf8)
    }
}

impl From<EscapeError> for MatcherError {
    #[inline]
    fn from(err: EscapeError) -> Self {
        MatcherError::ParseLiteralPattern(Box::new(err))
    }
}

impl From<regex_syntax::Error> for MatcherError {
    #[inline]
    fn from(err: regex_syntax::Error) -> Self {
        MatcherError::ParseRegexPattern(Box::new(err))
    }
}

impl From<regex_automata::meta::BuildError> for MatcherError {
    #[inline]
    fn from(err: regex_automata::meta::BuildError) -> Self {
        MatcherError::BuildRegex(Box::new(err))
    }
}

fn bytes_to_char(b: &[u8]) -> Option<char> {
    if b.len().wrapping_sub(1) < 3 {
        match bstr::decode_utf8(b) {
            (Some(ch), len) if len == b.len() => return Some(ch),
            _ => {}
        }
    }
    None
}

#[test]
fn test_bytes_to_char() {
    assert_eq!(Some('ẞ'), bytes_to_char("ẞ".as_bytes()));
    assert_eq!(None, bytes_to_char("SÜẞ".as_bytes()));
    assert_eq!(None, bytes_to_char(b"*\xff"));
    assert_eq!(None, bytes_to_char(b"\xff"));
    assert_eq!(None, bytes_to_char(b""));
}

use std::iter::{FusedIterator, Peekable};
use std::ops::Range;
use std::slice::Iter;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, MatchKind};
use regex_automata::{
    meta::{BuildError, FindMatches, Regex},
    nfa::thompson::WhichCaptures,
    util::syntax,
};
use thiserror::Error;

use crate::ws::{MappingWriter, Token};

pub struct Mapping {
    pub s: Vec<u8>,
    pub t: Vec<u8>,
    pub l: Vec<u8>,
}

impl Mapping {
    #[inline]
    pub fn new(s: Vec<u8>, t: Vec<u8>, l: Vec<u8>) -> Self {
        Mapping { s, t, l }
    }

    #[inline]
    pub fn writer(&self) -> MappingWriter<'_> {
        MappingWriter::new(self)
    }
}

impl Default for Mapping {
    #[inline]
    fn default() -> Self {
        Mapping {
            s: b" ".to_vec(),
            t: b"\t".to_vec(),
            l: b"\n".to_vec(),
        }
    }
}

/// Trait for token lexing.
pub trait Lexer: Iterator<Item = Token> {
    fn offset(&self) -> usize;
}

/// Constructs a lexer with the default space, tab, and LF lexemes.
#[inline]
pub fn default_lexer(src: &str) -> ByteLexer<'_> {
    ByteLexer::new(b' ', b'\t', b'\n', src.as_bytes())
}

/// Constructs a lexer with the given lexemes, choosing the more efficient
/// algorithm.
#[inline]
pub fn lexer<'a>(s: &[u8], t: &[u8], l: &[u8], src: &'a [u8]) -> Box<dyn Lexer + 'a> {
    if s.len() == 1 && t.len() == 1 && l.len() == 1 {
        Box::new(ByteLexer::new(s[0], t[1], l[1], src))
    } else {
        Box::new(StringLexer::new(s, t, l, src))
    }
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum TokenError {
    #[error("invalid UTF-8")]
    InvalidUtf8,
    #[error("invalid token at {0:?}")]
    Invalid(Range<usize>),
}

/// Lexer for Whitespace tokens, that recognizes 1-byte fixed lexemes using
/// memchr.
#[derive(Clone, Debug)]
pub struct ByteLexer<'a> {
    s: u8,
    t: u8,
    l: u8,
    iter: Iter<'a, u8>,
    len: usize,
}

impl<'a> ByteLexer<'a> {
    #[inline]
    pub fn new(s: u8, t: u8, l: u8, src: &'a [u8]) -> Self {
        ByteLexer {
            s,
            t,
            l,
            iter: src.iter(),
            len: src.len(),
        }
    }
}

impl Lexer for ByteLexer<'_> {
    #[inline]
    fn offset(&self) -> usize {
        self.len - self.iter.as_slice().len()
    }
}

impl Iterator for ByteLexer<'_> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(ch) = self.iter.next() {
            if ch == &self.s {
                return Some(Token::S);
            } else if ch == &self.t {
                return Some(Token::T);
            } else if ch == &self.l {
                return Some(Token::L);
            }
        }
        None
    }
}

impl FusedIterator for ByteLexer<'_> {}

/// Lexer for Whitespace tokens, that recognizes arbitrary-length fixed
/// lexemes using the Aho–Corasick algorithm.
#[derive(Clone, Debug)]
pub struct StringLexer<'a> {
    ac: AhoCorasick,
    src: &'a [u8],
    offset: usize,
}

impl<'a> StringLexer<'a> {
    #[inline]
    pub fn new(s: &[u8], t: &[u8], l: &[u8], src: &'a [u8]) -> Self {
        let ac = AhoCorasickBuilder::new()
            .match_kind(MatchKind::LeftmostLongest)
            .build(&[s, t, l])
            .unwrap();
        StringLexer { ac, src, offset: 0 }
    }
}

impl Lexer for StringLexer<'_> {
    #[inline]
    fn offset(&self) -> usize {
        self.offset
    }
}

impl Iterator for StringLexer<'_> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.ac.find(self.src) {
            Some(m) => {
                let tok = match m.pattern().as_u32() {
                    0 => Token::S,
                    1 => Token::T,
                    2 => Token::L,
                    _ => unreachable!(),
                };
                self.offset += m.end();
                self.src = unsafe { self.src.get_unchecked(m.end()..) };
                Some(tok)
            }
            None => {
                self.offset += self.src.len();
                self.src = &self.src[self.src.len()..];
                None
            }
        }
    }
}

impl FusedIterator for StringLexer<'_> {}

#[derive(Clone, Debug)]
pub struct PatternLexer {
    re: Regex,
    tokens: [Token; 3],
}

#[derive(Debug)]
pub struct PatternIter<'l, 's> {
    lex: &'l PatternLexer,
    iter: Peekable<FindMatches<'l, 's>>,
    offset: usize,
    len: usize,
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum PatternError {
    #[error("multiple patterns defined for token")]
    RepeatedToken,
    #[error(transparent)]
    Build(#[from] BuildError),
}

impl PatternLexer {
    #[inline]
    pub fn new(
        token1: Token,
        pattern1: &str,
        token2: Token,
        pattern2: &str,
        token3: Token,
        pattern3: &str,
        comment_pattern: Option<&str>,
    ) -> Result<Self, PatternError> {
        if token1 == token2 || token1 == token3 || token2 == token3 {
            return Err(PatternError::RepeatedToken);
        }
        let tokens = [token1, token2, token3];
        let patterns = [pattern1, pattern2, pattern3, comment_pattern.unwrap_or("")];
        let patterns = if comment_pattern.is_some() {
            &patterns[..]
        } else {
            &patterns[..3]
        };
        let re = Regex::builder()
            .syntax(syntax::Config::new().multi_line(true))
            .configure(Regex::config().which_captures(WhichCaptures::Implicit))
            .build_many(patterns)?;
        Ok(PatternLexer { re, tokens })
    }

    #[inline]
    pub fn lex<'l, 's>(&'l self, src: &'s str) -> PatternIter<'l, 's> {
        PatternIter {
            lex: self,
            iter: self.re.find_iter(src).peekable(),
            offset: 0,
            len: src.len(),
        }
    }
}

impl Iterator for PatternIter<'_, '_> {
    type Item = Result<Token, TokenError>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&m) = self.iter.peek() {
            let offset = self.offset;
            self.offset = m.end();
            if offset < m.start() {
                return Some(Err(TokenError::Invalid(offset..m.start())));
            }
            self.iter.next();
            if let Some(&tok) = self.lex.tokens.get(m.pattern().as_usize()) {
                return Some(Ok(tok));
            }
        }
        if self.offset < self.len {
            let offset = self.offset;
            self.offset = self.len;
            return Some(Err(TokenError::Invalid(offset..self.len)));
        }
        None
    }
}

impl FusedIterator for PatternIter<'_, '_> {}

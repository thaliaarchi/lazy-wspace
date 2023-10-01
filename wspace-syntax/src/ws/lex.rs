use std::iter::FusedIterator;
use std::slice::Iter;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, MatchKind};
use regex_automata::meta::{BuildError, FindMatches, Regex};
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
    iter: FindMatches<'l, 's>,
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum PatternError {
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
    ) -> Result<Self, PatternError> {
        let re = Regex::new_many(&[pattern1, pattern2, pattern3])?;
        Ok(PatternLexer {
            re,
            tokens: [token1, token2, token3],
        })
    }

    pub fn lex<'l, 's>(&'l self, src: &'s str) -> PatternIter<'l, 's> {
        let iter = self.re.find_iter(src);
        PatternIter { lex: self, iter }
    }
}

impl Iterator for PatternIter<'_, '_> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let m = self.iter.next()?;
        Some(self.lex.tokens[m.pattern().as_usize()])
    }
}

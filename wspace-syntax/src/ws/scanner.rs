use std::iter::FusedIterator;
use std::slice::Iter;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, MatchKind};

use crate::ws::MappingWriter;

/// Whitespace token.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    /// Space
    S,
    /// Tab
    T,
    /// Line feed
    L,
}

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

/// Trait for token scanning.
pub trait Scanner: Iterator<Item = Token> {
    fn offset(&self) -> usize;
}

/// Scan with the default space, tab, and LF lexemes.
#[inline]
pub fn scan_default(src: &str) -> ByteScanner<'_> {
    ByteScanner::new(b' ', b'\t', b'\n', src.as_bytes())
}

/// Scan with the given lexemes, choosing the more efficient algorithm.
#[inline]
pub fn scan<'a>(s: &[u8], t: &[u8], l: &[u8], src: &'a [u8]) -> Box<dyn Scanner + 'a> {
    if s.len() == 1 && t.len() == 1 && l.len() == 1 {
        Box::new(ByteScanner::new(s[0], t[1], l[1], src))
    } else {
        Box::new(StringScanner::new(s, t, l, src))
    }
}

/// Scanner for Whitespace tokens, that recognizes 1-byte fixed lexemes using
/// memchr.
#[derive(Clone, Debug)]
pub struct ByteScanner<'a> {
    s: u8,
    t: u8,
    l: u8,
    iter: Iter<'a, u8>,
    len: usize,
}

impl<'a> ByteScanner<'a> {
    #[inline]
    pub fn new(s: u8, t: u8, l: u8, src: &'a [u8]) -> Self {
        ByteScanner {
            s,
            t,
            l,
            iter: src.iter(),
            len: src.len(),
        }
    }
}

impl Scanner for ByteScanner<'_> {
    #[inline]
    fn offset(&self) -> usize {
        self.len - self.iter.as_slice().len()
    }
}

impl Iterator for ByteScanner<'_> {
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

impl FusedIterator for ByteScanner<'_> {}

/// Scanner for Whitespace tokens, that recognizes arbitrary-length fixed
/// lexemes using the Aho–Corasick algorithm.
#[derive(Clone, Debug)]
pub struct StringScanner<'a> {
    ac: AhoCorasick,
    src: &'a [u8],
    offset: usize,
}

impl<'a> StringScanner<'a> {
    #[inline]
    pub fn new(s: &[u8], t: &[u8], l: &[u8], src: &'a [u8]) -> Self {
        let ac = AhoCorasickBuilder::new()
            .match_kind(MatchKind::LeftmostLongest)
            .build(&[s, t, l])
            .unwrap();
        StringScanner { ac, src, offset: 0 }
    }
}

impl Scanner for StringScanner<'_> {
    #[inline]
    fn offset(&self) -> usize {
        self.offset
    }
}

impl Iterator for StringScanner<'_> {
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

impl FusedIterator for StringScanner<'_> {}

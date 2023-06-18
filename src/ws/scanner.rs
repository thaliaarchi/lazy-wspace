use std::iter::FusedIterator;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, MatchKind};
use memchr::memchr3;

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

/// Trait for token scanning.
pub trait Scanner: Iterator<Item = Token> {
    fn offset(&self) -> usize;
}

/// Scan with the default space, tab, and LF lexemes.
pub fn scan_default(src: &str) -> ByteScanner<'_> {
    ByteScanner::new(b' ', b'\t', b'\n', src.as_bytes())
}

/// Scan with the given lexemes, choosing the more efficient algorithm.
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
    src: &'a [u8],
    offset: usize,
}

impl<'a> ByteScanner<'a> {
    pub fn new(s: u8, t: u8, l: u8, src: &'a [u8]) -> Self {
        ByteScanner {
            s,
            t,
            l,
            src,
            offset: 0,
        }
    }
}

impl Scanner for ByteScanner<'_> {
    fn offset(&self) -> usize {
        self.offset
    }
}

impl Iterator for ByteScanner<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match memchr3(self.s, self.t, self.l, self.src) {
            Some(i) => {
                let ch = self.src[i];
                let tok = if ch == self.s {
                    Token::S
                } else if ch == self.t {
                    Token::T
                } else if ch == self.l {
                    Token::L
                } else {
                    unreachable!()
                };
                self.offset += i + 1;
                self.src = &self.src[i + 1..];
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
    pub fn new(s: &[u8], t: &[u8], l: &[u8], src: &'a [u8]) -> Self {
        let ac = AhoCorasickBuilder::new()
            .match_kind(MatchKind::LeftmostLongest)
            .build(&[s, t, l])
            .unwrap();
        StringScanner { ac, src, offset: 0 }
    }
}

impl Scanner for StringScanner<'_> {
    fn offset(&self) -> usize {
        self.offset
    }
}

impl Iterator for StringScanner<'_> {
    type Item = Token;

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
                self.src = &self.src[m.end()..];
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

#[cfg(test)]
mod tests {
    use super::*;

    #[rustfmt::skip]
    const TUTORIAL_TOKENS: &[Token] = &[
        Token::S, Token::S, Token::S, Token::T, Token::L,
        Token::L, Token::S, Token::S, Token::S, Token::T, Token::S, Token::S, Token::S, Token::S, Token::T, Token::T, Token::L,
        Token::S, Token::L, Token::S,
        Token::T, Token::L, Token::S, Token::T,
        Token::S, Token::S, Token::S, Token::T, Token::S, Token::T, Token::S, Token::L,
        Token::T, Token::L, Token::S, Token::S,
        Token::S, Token::S, Token::S, Token::T, Token::L,
        Token::T, Token::S, Token::S, Token::S,
        Token::S, Token::L, Token::S,
        Token::S, Token::S, Token::S, Token::T, Token::S, Token::T, Token::T, Token::L,
        Token::T, Token::S, Token::S, Token::T,
        Token::L, Token::T, Token::S, Token::S, Token::T, Token::S, Token::S, Token::S, Token::T, Token::S, Token::T, Token::L,
        Token::L, Token::S, Token::L, Token::S, Token::T, Token::S, Token::S, Token::S, Token::S, Token::T, Token::T, Token::L,
        Token::L, Token::S, Token::S, Token::S, Token::T, Token::S, Token::S, Token::S, Token::T, Token::S, Token::T, Token::L,
        Token::S, Token::L, Token::L,
        Token::L, Token::L, Token::L,
    ];

    #[test]
    fn tutorial_byte() {
        let src = "   \t\n\n   \t    \t\t\n \n \t\n \t   \t \t \n\t\n     \t\n\t    \n    \t \t\t\n\t  \t\n\t  \t   \t \t\n\n \n \t    \t\t\n\n   \t   \t \t\n \n\n\n\n\n";
        let scan = scan_default(src);
        assert_eq!(TUTORIAL_TOKENS, scan.collect::<Vec<_>>());
    }

    #[test]
    fn tutorial_string() {
        // https://web.archive.org/web/20150618184706/http://compsoc.dur.ac.uk/whitespace/tutorial.php
        let src = "[Space][Space][Space][Tab][LF]                                            Put a 1 on the stack
[LF][Space][Space][Space][Tab][Space][Space] [Space][Space][Tab][Tab][LF] Set a Label at this point
[Space][LF][Space]                                                        Duplicate the top stack item
[Tab][LF][Space][Tab]                                                     Output the current value
[Space][Space][Space][Tab][Space][Tab][Space][LF]                         Put 10 (newline) on the stack...
[Tab][LF][Space][Space]                                                   ...and output the newline
[Space][Space][Space][Tab][LF]                                            Put a 1 on the stack
[Tab][Space][Space][Space]                                                Addition. This increments our current value.
[Space][LF][Space]                                                        Duplicate that value so we can test it
[Space][Space][Space][Tab][Space][Tab][Tab][LF]                           Push 11 onto the stack
[Tab][Space][Space][Tab]                                                  Subtraction. So if we've reached the end, we have a zero on the stack.
[LF][Tab][Space][Space][Tab][Space][Space] [Space][Tab][Space][Tab][LF]   If we have a zero, jump to the end
[LF][Space][LF][Space][Tab][Space] [Space][Space][Space][Tab][Tab][LF]    Jump to the start
[LF][Space][Space][Space][Tab][Space] [Space][Space][Tab][Space][Tab][LF] Set the end label
[Space][LF][LF]                                                           Discard our accumulator, to be tidy
[LF][LF][LF]                                                              Finish
";
        let scan = StringScanner::new(b"[Space]", b"[Tab]", b"[LF]", src.as_bytes());
        assert_eq!(TUTORIAL_TOKENS, scan.collect::<Vec<_>>());
    }
}

use std::iter::FusedIterator;
use std::str::from_utf8_unchecked;

use memchr::memchr;

use crate::source::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Word,
    String,
    UnterminatedString,
    Space,
    Lf,
    LineComment,
    BlockComment,
    UnopenedBlockComment,
    UnclosedBlockComment,
}

const EOF_BYTE: u8 = 0;

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    src: &'a [u8],
    offset: usize,
}

impl<'a> Lexer<'a> {
    #[inline]
    pub fn new(src: &'a str) -> Self {
        // Source files are required to be valid UTF-8, but the grammar is all
        // ASCII, so we can scan over bytes instead of chars.
        Lexer {
            src: src.as_bytes(),
            offset: 0,
        }
    }

    #[inline]
    pub fn source_text(&self) -> &'a str {
        unsafe { from_utf8_unchecked(self.src) }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.offset;
        let tok = match self.bump()? {
            b' ' | b'\t' => {
                self.eat_while(|b| b == b' ' || b == b'\t');
                Token::Space
            }
            b'\n' => Token::Lf,
            b'"' => self.string(),
            b';' => self.line_comment(),
            b'-' if self.peek() == b'-' => {
                self.bump();
                self.line_comment()
            }
            b'{' if self.peek() == b'-' => {
                self.bump();
                self.block_comment()
            }
            b'-' if self.peek() == b'}' => {
                self.bump();
                Token::UnopenedBlockComment
            }
            _ => self.word(),
        };
        Some((tok, Span::from(start..self.offset)))
    }
}

impl FusedIterator for Lexer<'_> {}

impl Lexer<'_> {
    #[inline]
    fn word(&mut self) -> Token {
        loop {
            match self.peek() {
                b' ' | b'\t' | b'\n' | b'"' | b';' => break,
                b'{' | b'-' if self.peek_n(1) == b'-' => break,
                EOF_BYTE if self.is_eof() => break,
                _ => self.bump(),
            };
        }
        Token::Word
    }

    #[inline]
    fn string(&mut self) -> Token {
        match memchr(b'"', &self.src[self.offset..]) {
            Some(i) => {
                self.offset += i + 1;
                Token::String
            }
            None => {
                self.offset += memchr(b'\n', &self.src[self.offset..]).unwrap_or(self.src.len());
                Token::UnterminatedString
            }
        }
    }

    #[inline]
    fn line_comment(&mut self) -> Token {
        self.eat_while(|b| b != b'\n');
        Token::LineComment
    }

    #[inline]
    fn block_comment(&mut self) -> Token {
        let mut depth = 1usize;
        while let Some(c) = self.bump() {
            match c {
                b'{' if self.peek() == b'-' => {
                    self.bump();
                    depth += 1;
                }
                b'-' if self.peek() == b'}' => {
                    self.bump();
                    depth -= 1;
                    if depth == 0 {
                        return Token::BlockComment;
                    }
                }
                _ => {}
            }
        }
        Token::UnclosedBlockComment
    }

    #[inline]
    fn bump(&mut self) -> Option<u8> {
        match self.src.get(self.offset) {
            Some(b) => {
                self.offset += 1;
                Some(*b)
            }
            None => None,
        }
    }

    #[inline]
    fn peek(&self) -> u8 {
        self.peek_n(0)
    }

    #[inline]
    fn peek_n(&self, n: usize) -> u8 {
        self.src.get(self.offset + n).copied().unwrap_or(EOF_BYTE)
    }

    #[inline]
    fn is_eof(&self) -> bool {
        self.offset >= self.src.len()
    }

    #[inline]
    fn eat_while(&mut self, mut predicate: impl FnMut(u8) -> bool) {
        while predicate(self.peek()) && !self.is_eof() {
            self.bump();
        }
    }
}

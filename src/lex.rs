use std::iter::FusedIterator;
use std::slice::Iter;
use std::str;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    S,
    T,
    L,
    InvalidUtf8,
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    iter: Iter<'a, u8>,
    invalid_utf8: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a [u8]) -> Self {
        let mut invalid_utf8 = false;
        let src = match str::from_utf8(src) {
            Ok(_) => src,
            Err(err) => {
                invalid_utf8 = true;
                &src[..err.valid_up_to()]
            }
        };
        Lexer {
            iter: src.iter(),
            invalid_utf8,
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(ch) = self.iter.next() {
            match ch {
                b' ' => return Some(Token::S),
                b'\t' => return Some(Token::T),
                b'\n' => return Some(Token::L),
                _ => {}
            }
        }
        if self.invalid_utf8 {
            self.invalid_utf8 = false;
            Some(Token::InvalidUtf8)
        } else {
            None
        }
    }
}

impl DoubleEndedIterator for Lexer<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.invalid_utf8 {
            self.invalid_utf8 = false;
            return Some(Token::InvalidUtf8);
        }
        while let Some(ch) = self.iter.next_back() {
            match ch {
                b' ' => return Some(Token::S),
                b'\t' => return Some(Token::T),
                b'\n' => return Some(Token::L),
                _ => {}
            }
        }
        None
    }
}

impl FusedIterator for Lexer<'_> {}

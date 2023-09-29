use std::iter::FusedIterator;
use std::slice::Iter;

use simdutf8::compat::from_utf8;
use wspace_syntax::ws::Token;

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    iter: Iter<'a, u8>,
    pub(crate) invalid_utf8: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a [u8]) -> Self {
        let mut invalid_utf8 = false;
        let src = match from_utf8(src) {
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
        None
    }
}

impl FusedIterator for Lexer<'_> {}

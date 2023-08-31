use bitvec::slice::BitSlice;
use rug::Integer;

use crate::ws::{Mapping, Token};

pub trait FormatTokens {
    fn fmt_tokens<W: TokenWriter>(&self, b: &mut Builder<W>);
}

pub trait TokenWriter {
    fn write_token(&mut self, tok: Token);

    fn write_comment(&mut self, comment: &[u8]);
}

#[derive(Debug)]
pub struct Builder<'a, W> {
    w: &'a mut W,
    zero_format: ZeroFormat,
    label_format: LabelFormat,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum ZeroFormat {
    Empty,
    #[default]
    Positive,
    PositiveZero,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum LabelFormat {
    IncrementByDef,
    IncrementByUse,
    #[default]
    Utf8,
}

impl<'a, W: TokenWriter> Builder<'a, W> {
    pub fn new(token_writer: &'a mut W) -> Self {
        Builder {
            w: token_writer,
            zero_format: ZeroFormat::default(),
            label_format: LabelFormat::default(),
        }
    }

    pub fn zero_format(mut self, zero_format: ZeroFormat) -> Self {
        self.zero_format = zero_format;
        self
    }

    pub fn label_format(mut self, label_format: LabelFormat) -> Self {
        self.label_format = label_format;
        self
    }

    pub fn push(&mut self, tok: Token) {
        self.w.write_token(tok);
    }

    pub fn append(&mut self, toks: &[Token]) {
        for &tok in toks {
            self.push(tok);
        }
    }

    pub fn write_bits(&mut self, bits: &BitSlice) {
        for bit in bits {
            self.push(if *bit { Token::T } else { Token::S });
        }
        self.push(Token::L);
    }

    pub fn write_integer(&mut self, n: Integer) {
        _ = n;
        todo!()
    }
}

impl TokenWriter for Vec<Token> {
    fn write_token(&mut self, tok: Token) {
        self.push(tok);
    }

    fn write_comment(&mut self, _comment: &[u8]) {}
}

pub struct MappingWriter<'a> {
    buf: Vec<u8>,
    map: &'a Mapping,
}

impl<'a> MappingWriter<'a> {
    pub fn new(map: &'a Mapping) -> Self {
        MappingWriter {
            buf: Vec::new(),
            map,
        }
    }

    pub fn finish(self) -> Vec<u8> {
        self.buf
    }
}

impl TokenWriter for MappingWriter<'_> {
    fn write_token(&mut self, tok: Token) {
        self.buf.extend_from_slice(match tok {
            Token::S => &self.map.s,
            Token::T => &self.map.t,
            Token::L => &self.map.l,
        });
    }

    fn write_comment(&mut self, comment: &[u8]) {
        // TODO: Handle conflicts with mapping
        self.buf.extend_from_slice(comment);
    }
}

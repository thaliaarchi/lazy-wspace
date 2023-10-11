use bitvec::slice::BitSlice;
use rug::Integer;

use crate::ws::{
    ast::{Inst, IntegerLit, LabelLit},
    lex::Token,
};

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

impl FormatTokens for Inst {
    fn fmt_tokens<W: TokenWriter>(&self, b: &mut Builder<W>) {
        use Token::*;
        let opcode: &[Token] = match self {
            Inst::Push(_) => &[S, S],
            Inst::Dup => &[S, L, S],
            Inst::Copy(_) => &[S, T, S],
            Inst::Swap => &[S, L, T],
            Inst::Drop => &[S, L, L],
            Inst::Slide(_) => &[S, T, L],
            Inst::Add => &[T, S, S, S],
            Inst::Sub => &[T, S, S, T],
            Inst::Mul => &[T, S, S, L],
            Inst::Div => &[T, S, T, S],
            Inst::Mod => &[T, S, T, T],
            Inst::Store => &[T, T, S],
            Inst::Retrieve => &[T, T, T],
            Inst::Label(_) => &[L, S, S],
            Inst::Call(_) => &[L, S, T],
            Inst::Jmp(_) => &[L, S, L],
            Inst::Jz(_) => &[L, T, S],
            Inst::Jn(_) => &[L, T, T],
            Inst::Ret => &[L, T, L],
            Inst::End => &[L, L, L],
            Inst::Printc => &[T, L, S, S],
            Inst::Printi => &[T, L, S, T],
            Inst::Readc => &[T, L, T, S],
            Inst::Readi => &[T, L, T, T],
            Inst::DebugPrintStack => &[L, L, S, S, S],
            Inst::DebugPrintHeap => &[L, L, S, S, T],
            Inst::Trace => &[L, L, T],
            Inst::Shuffle => &[S, T, T, S],
            Inst::Invert => &[S, T, T],
            Inst::ParseError(err) => todo!("format parse error: {err:?}"),
        };
        b.append(opcode);
        match self {
            Inst::Push(n) | Inst::Copy(n) | Inst::Slide(n) => {
                n.fmt_tokens(b);
            }
            Inst::Label(l) | Inst::Call(l) | Inst::Jmp(l) | Inst::Jz(l) | Inst::Jn(l) => {
                l.fmt_tokens(b);
            }
            _ => {}
        }
    }
}

impl FormatTokens for IntegerLit {
    fn fmt_tokens<W: TokenWriter>(&self, b: &mut Builder<W>) {
        match self {
            IntegerLit::Pos(bits) => {
                b.push(Token::S);
                b.write_bits(bits);
            }
            IntegerLit::Neg(bits) => {
                b.push(Token::T);
                b.write_bits(bits);
            }
            IntegerLit::Empty => b.push(Token::L),
        }
    }
}

impl FormatTokens for LabelLit {
    fn fmt_tokens<W: TokenWriter>(&self, b: &mut Builder<W>) {
        b.write_bits(&self.0);
    }
}

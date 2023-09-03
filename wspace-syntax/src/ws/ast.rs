use bitvec::vec::BitVec;
use lazy_wspace_macros::Opcode;

use crate::ws::{FormatTokens, Token, TokenWriter};

/// Whitespace instruction.
///
/// See [`Extension`] for documentation on the non-standard extension
/// instructions.
#[derive(Clone, Debug, Opcode, PartialEq, Eq, Hash)]
pub enum Inst {
    // Standard instructions
    Push(IntegerLit),
    Dup,
    Copy(IntegerLit),
    Swap,
    Drop,
    Slide(IntegerLit),
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Store,
    Retrieve,
    Label(LabelLit),
    Call(LabelLit),
    Jmp(LabelLit),
    Jz(LabelLit),
    Jn(LabelLit),
    Ret,
    End,
    Printc,
    Printi,
    Readc,
    Readi,
    // Extension instructions
    DebugPrintStack,
    DebugPrintHeap,
    Trace,
    Shuffle,
    Invert,
}

impl Opcode {
    pub fn tokens(&self) -> &[Token] {
        use Token::*;
        match self {
            Opcode::Push => &[S, S],
            Opcode::Dup => &[S, L, S],
            Opcode::Copy => &[S, T, S],
            Opcode::Swap => &[S, L, T],
            Opcode::Drop => &[S, L, L],
            Opcode::Slide => &[S, T, L],
            Opcode::Add => &[T, S, S, S],
            Opcode::Sub => &[T, S, S, T],
            Opcode::Mul => &[T, S, S, L],
            Opcode::Div => &[T, S, T, S],
            Opcode::Mod => &[T, S, T, T],
            Opcode::Store => &[T, T, S],
            Opcode::Retrieve => &[T, T, T],
            Opcode::Label => &[L, S, S],
            Opcode::Call => &[L, S, T],
            Opcode::Jmp => &[L, S, L],
            Opcode::Jz => &[L, T, S],
            Opcode::Jn => &[L, T, T],
            Opcode::Ret => &[L, T, L],
            Opcode::End => &[L, L, L],
            Opcode::Printc => &[T, L, S, S],
            Opcode::Printi => &[T, L, S, T],
            Opcode::Readc => &[T, L, T, S],
            Opcode::Readi => &[T, L, T, T],
            Opcode::DebugPrintStack => &[L, L, S, S, S],
            Opcode::DebugPrintHeap => &[L, L, S, S, T],
            Opcode::Trace => &[L, L, T],
            Opcode::Shuffle => &[S, T, T, S],
            Opcode::Invert => &[S, T, T],
        }
    }
}

/// Whitespace integer literal.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntegerLit {
    Pos(BitVec),
    Neg(BitVec),
    Empty,
}

/// Whitespace label literal.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LabelLit(pub BitVec);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Extension {
    /// Enables `debug_printstack` and `debug_printheap` extension instructions
    /// from [wsintercpp](https://github.com/wspace/burghard-wsintercpp),
    /// [wsinterws](https://github.com/wspace/burghard-wsinterws), and [wsa](https://github.com/wspace/burghard-wsa)
    /// by Oliver Burghard.
    DebugPrint,
    /// Enables `Trace` extension instruction from [pywhitespace](https://github.com/wspace/phlip-pywhitespace)
    /// by Phillip Bradbury.
    Trace,
    /// Enables `Shuffle` extension instruction from [whitespace-nd](https://github.com/haroldl/whitespace-nd)
    /// by Harold Lee.
    Shuffle,
    /// Enables `Invert` extension instruction from [whitespacesdk](https://github.com/wspace/mash-whitespacesdk)
    /// by MArtin SHerratt.
    Invert,
}

impl FormatTokens for Inst {
    fn fmt_tokens<W: TokenWriter>(&self, b: &mut super::Builder<W>) {
        self.opcode().fmt_tokens(b);
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

impl FormatTokens for Opcode {
    fn fmt_tokens<W: TokenWriter>(&self, b: &mut super::Builder<W>) {
        b.append(self.tokens());
    }
}

impl FormatTokens for IntegerLit {
    fn fmt_tokens<W: TokenWriter>(&self, b: &mut super::Builder<W>) {
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
    fn fmt_tokens<W: TokenWriter>(&self, b: &mut super::Builder<W>) {
        b.write_bits(&self.0);
    }
}

use bitvec::vec::BitVec;

use crate::ws::{
    FormatTokens,
    Token::{self, *},
    TokenWriter,
};

macro_rules! map(
    ( , $then:tt) => {};
    ($optional:tt, $then:tt) => { $then };
);

macro_rules! insts {
    (
        $(#[$inst_attr:meta])* $inst_vis:vis enum $Inst:ident
        $(#[$opcode_attr:meta])* $opcode_vis:vis enum $Opcode:ident
        $([$($tok:expr)+] =>
            $Op:ident $(($($param:ty),+))?),* $(,)?
    ) => {
        $(#[$inst_attr])*
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        $inst_vis enum $Inst {
            $($Op $(($($param),+))?),*
        }

        $(#[$opcode_attr])*
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        $opcode_vis enum $Opcode {
            $($Op),*
        }

        impl Inst {
            pub fn opcode(&self) -> Opcode {
                match self {
                    $($Inst::$Op $(($(map!($param, _)),+))? => $Opcode::$Op,)*
                }
            }
        }

        impl Opcode {
            pub fn tokens(&self) -> &[Token] {
                match self {
                    $($Opcode::$Op => &[$($tok),+],)*
                }
            }
        }
    };
}

insts! {
    /// Whitespace instruction.
    ///
    /// See [`Extension`] for documentation on the non-standard extension
    /// instructions.
    pub enum Inst

    /// Whitespace instruction opcode.
    ///
    /// This is [`Inst`] without arguments.
    pub enum Opcode

    // Standard instructions
    [S S] => Push(IntegerLit),
    [S L S] => Dup,
    [S T S] => Copy(IntegerLit),
    [S L T] => Swap,
    [S L L] => Drop,
    [S T L] => Slide(IntegerLit),
    [T S S S] => Add,
    [T S S T] => Sub,
    [T S S L] => Mul,
    [T S T S] => Div,
    [T S T T] => Mod,
    [T T S] => Store,
    [T T T] => Retrieve,
    [L S S] => Label(LabelLit),
    [L S T] => Call(LabelLit),
    [L S L] => Jmp(LabelLit),
    [L T S] => Jz(LabelLit),
    [L T T] => Jn(LabelLit),
    [L T L] => Ret,
    [L L L] => End,
    [T L S S] => Printc,
    [T L S T] => Printi,
    [T L T S] => Readc,
    [T L T T] => Readi,
    // Extension instructions
    [L L S S S] => DebugPrintStack,
    [L L S S T] => DebugPrintHeap,
    [L L T] => Trace,
    [S T T S] => Shuffle,
    [S T T] => Invert,
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

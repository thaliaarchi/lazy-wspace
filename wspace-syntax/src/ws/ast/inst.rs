use bitvec::vec::BitVec;
use lazy_wspace_macros::Opcode;

use crate::ws::ast::ParseError;

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
    // Errors
    ParseError(ParseError),
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

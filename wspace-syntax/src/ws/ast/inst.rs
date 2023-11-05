use std::fmt::{self, Display, Formatter};

use crate::hs;
use crate::ws::ast::{IntegerLit, LabelLit, ParseError};

/// Whitespace instruction.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Inst {
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
    ParseError(ParseError),
}

// TODO: Implement extension instructions.
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

impl Inst {
    #[inline]
    pub fn integer(&self) -> Option<&IntegerLit> {
        match self {
            Inst::Push(n) | Inst::Copy(n) | Inst::Slide(n) => Some(n),
            _ => None,
        }
    }

    #[inline]
    pub fn label(&self) -> Option<&LabelLit> {
        match self {
            Inst::Label(l) | Inst::Call(l) | Inst::Jmp(l) | Inst::Jz(l) | Inst::Jn(l) => Some(l),
            _ => None,
        }
    }

    #[inline]
    pub fn is_control_flow(&self) -> bool {
        matches!(
            self,
            Inst::Label(_)
                | Inst::Call(_)
                | Inst::Jmp(_)
                | Inst::Jz(_)
                | Inst::Jn(_)
                | Inst::Ret
                | Inst::End
                | Inst::ParseError(_),
        )
    }

    #[inline]
    pub fn can_end_program(&self) -> bool {
        matches!(
            self,
            Inst::Jmp(_) | Inst::Ret | Inst::End | Inst::ParseError(_),
        )
    }
}

impl Display for Inst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Inst::Push(n) => write!(f, "push {n}"),
            Inst::Dup => write!(f, "dup"),
            Inst::Copy(n) => write!(f, "copy {n}"),
            Inst::Swap => write!(f, "swap"),
            Inst::Drop => write!(f, "drop"),
            Inst::Slide(n) => write!(f, "slide {n}"),
            Inst::Add => write!(f, "add"),
            Inst::Sub => write!(f, "sub"),
            Inst::Mul => write!(f, "mul"),
            Inst::Div => write!(f, "div"),
            Inst::Mod => write!(f, "mod"),
            Inst::Store => write!(f, "store"),
            Inst::Retrieve => write!(f, "retrieve"),
            Inst::Label(l) => write!(f, "label {l}"),
            Inst::Call(l) => write!(f, "call {l}"),
            Inst::Jmp(l) => write!(f, "jmp {l}"),
            Inst::Jz(l) => write!(f, "jz {l}"),
            Inst::Jn(l) => write!(f, "jn {l}"),
            Inst::Ret => write!(f, "ret"),
            Inst::End => write!(f, "end"),
            Inst::Printc => write!(f, "printc"),
            Inst::Printi => write!(f, "printi"),
            Inst::Readc => write!(f, "readc"),
            Inst::Readi => write!(f, "readi"),
            Inst::ParseError(err) => write!(f, "error {err:?}"),
        }
    }
}

impl hs::Show for Inst {
    fn show(&self) -> String {
        fn show_integer(n: &IntegerLit) -> String {
            assert!(!n.is_empty(), "cannot show empty integer");
            n.value().unwrap().show()
        }
        match self {
            Inst::Push(n) => format!("Push {}", show_integer(n)),
            Inst::Dup => "Dup".into(),
            Inst::Copy(n) => format!("Ref {}", show_integer(n)),
            Inst::Swap => "Swap".into(),
            Inst::Drop => "Discard".into(),
            Inst::Slide(n) => format!("Slide {}", show_integer(n)),
            Inst::Add => "Infix Plus".into(),
            Inst::Sub => "Infix Minus".into(),
            Inst::Mul => "Infix Times".into(),
            Inst::Div => "Infix Divide".into(),
            Inst::Mod => "Infix Modulo".into(),
            Inst::Store => "Store".into(),
            Inst::Retrieve => "Retrieve".into(),
            Inst::Label(l) => format!("Label {}", l.show()),
            Inst::Call(l) => format!("Call {}", l.show()),
            Inst::Jmp(l) => format!("Jump {}", l.show()),
            Inst::Jz(l) => format!("If Zero {}", l.show()),
            Inst::Jn(l) => format!("If Negative {}", l.show()),
            Inst::Ret => "Return".into(),
            Inst::End => "End".into(),
            Inst::Printc => "OutputChar".into(),
            Inst::Printi => "OutputNum".into(),
            Inst::Readc => "ReadChar".into(),
            Inst::Readi => "ReadNum".into(),
            _ => panic!("cannot show {self:?}"),
        }
    }
}

impl From<ParseError> for Inst {
    #[inline]
    fn from(err: ParseError) -> Self {
        Inst::ParseError(err)
    }
}

impl From<Result<Inst, ParseError>> for Inst {
    #[inline]
    fn from(res: Result<Inst, ParseError>) -> Self {
        res.unwrap_or_else(|err| err.into())
    }
}

#[cfg(test)]
mod tests {
    use bitvec::prelude::*;

    use super::*;
    use crate::hs::Show;

    #[test]
    fn show_inst() {
        let l = LabelLit::from(bitvec![0, 1, 0, 0, 1, 1, 0, 0]);
        assert_eq!("Push 0", Inst::Push(IntegerLit::new(0)).show());
        assert_eq!("Push 1234", Inst::Push(IntegerLit::new(1234)).show());
        assert_eq!("Push (-1234)", Inst::Push(IntegerLit::new(-1234)).show());
        assert_eq!("Dup", Inst::Dup.show());
        assert_eq!("Ref 0", Inst::Copy(IntegerLit::new(0)).show());
        assert_eq!("Ref 1234", Inst::Copy(IntegerLit::new(1234)).show());
        assert_eq!("Ref (-1234)", Inst::Copy(IntegerLit::new(-1234)).show());
        assert_eq!("Swap", Inst::Swap.show());
        assert_eq!("Discard", Inst::Drop.show());
        assert_eq!("Slide 0", Inst::Slide(IntegerLit::new(0)).show());
        assert_eq!("Slide 1234", Inst::Slide(IntegerLit::new(1234)).show());
        assert_eq!("Slide (-1234)", Inst::Slide(IntegerLit::new(-1234)).show());
        assert_eq!("Infix Plus", Inst::Add.show());
        assert_eq!("Infix Minus", Inst::Sub.show());
        assert_eq!("Infix Times", Inst::Mul.show());
        assert_eq!("Infix Divide", Inst::Div.show());
        assert_eq!("Infix Modulo", Inst::Mod.show());
        assert_eq!("Store", Inst::Store.show());
        assert_eq!("Retrieve", Inst::Retrieve.show());
        assert_eq!(r#"Label "  \t\t  \t ""#, Inst::Label(l.clone()).show());
        assert_eq!(r#"Call "  \t\t  \t ""#, Inst::Call(l.clone()).show());
        assert_eq!(r#"Jump "  \t\t  \t ""#, Inst::Jmp(l.clone()).show());
        assert_eq!(r#"If Zero "  \t\t  \t ""#, Inst::Jz(l.clone()).show());
        assert_eq!(r#"If Negative "  \t\t  \t ""#, Inst::Jn(l).show());
        assert_eq!("Return", Inst::Ret.show());
        assert_eq!("End", Inst::End.show());
        assert_eq!("OutputChar", Inst::Printc.show());
        assert_eq!("OutputNum", Inst::Printi.show());
        assert_eq!("ReadChar", Inst::Readc.show());
        assert_eq!("ReadNum", Inst::Readi.show());
    }
}

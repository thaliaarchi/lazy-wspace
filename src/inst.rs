use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

use bitvec::vec::BitVec;
use rug::Integer;

use crate::error::ParseError;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Inst {
    Push(NumberLit),
    Dup,
    Copy(NumberLit),
    Swap,
    Drop,
    Slide(NumberLit),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ArgKind {
    Number,
    Label,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NumberLit {
    Number(Rc<Integer>),
    Empty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LabelLit(BitVec);

impl<T: Into<Integer>> From<T> for NumberLit {
    #[inline]
    fn from(n: T) -> Self {
        NumberLit::Number(Rc::new(n.into()))
    }
}

impl From<BitVec> for LabelLit {
    #[inline]
    fn from(bits: BitVec) -> Self {
        LabelLit(bits)
    }
}

impl Display for Inst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Inst::Push(n) => write!(f, "Push {n}"),
            Inst::Dup => write!(f, "Dup"),
            Inst::Copy(n) => write!(f, "Ref {n}"),
            Inst::Swap => write!(f, "Swap"),
            Inst::Drop => write!(f, "Discard"),
            Inst::Slide(n) => write!(f, "Slide {n}"),
            Inst::Add => write!(f, "Infix Plus"),
            Inst::Sub => write!(f, "Infix Minus"),
            Inst::Mul => write!(f, "Infix Times"),
            Inst::Div => write!(f, "Infix Divide"),
            Inst::Mod => write!(f, "Infix Modulo"),
            Inst::Store => write!(f, "Store"),
            Inst::Retrieve => write!(f, "Retrieve"),
            Inst::Label(l) => write!(f, "Label {l}"),
            Inst::Call(l) => write!(f, "Call {l}"),
            Inst::Jmp(l) => write!(f, "Jump {l}"),
            Inst::Jz(l) => write!(f, "If Zero {l}"),
            Inst::Jn(l) => write!(f, "If Negative {l}"),
            Inst::Ret => write!(f, "Return"),
            Inst::End => write!(f, "End"),
            Inst::Printc => write!(f, "OutputChar"),
            Inst::Printi => write!(f, "OutputNum"),
            Inst::Readc => write!(f, "ReadChar"),
            Inst::Readi => write!(f, "ReadNum"),
            Inst::ParseError(_) => panic!("BUG: cannot display parse error"),
        }
    }
}

impl Display for NumberLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            NumberLit::Number(n) => {
                if n.cmp0() == Ordering::Less {
                    write!(f, "({n})") // Parenthesize negatives
                } else {
                    write!(f, "{n}")
                }
            }
            NumberLit::Empty => panic!("BUG: cannot display empty literal"),
        }
    }
}

impl Display for LabelLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "\"")?;
        for bit in self.0.iter().rev() {
            f.write_str(if *bit { "\\t" } else { " " })?;
        }
        write!(f, "\"")
    }
}

#[cfg(test)]
mod tests {
    use bitvec::prelude::*;

    use super::*;

    #[test]
    fn display_inst() {
        let pos: NumberLit = Integer::from(1234).into();
        let neg: NumberLit = Integer::from(-1234).into();
        let l = LabelLit(bitvec![0, 1, 0, 0, 1, 1, 0, 0]);
        assert_eq!(Inst::Push(Integer::ZERO.into()).to_string(), "Push 0");
        assert_eq!(Inst::Push(pos.clone()).to_string(), "Push 1234");
        assert_eq!(Inst::Push(neg.clone()).to_string(), "Push (-1234)");
        assert_eq!(Inst::Dup.to_string(), "Dup");
        assert_eq!(Inst::Copy(Integer::ZERO.into()).to_string(), "Ref 0");
        assert_eq!(Inst::Copy(pos.clone()).to_string(), "Ref 1234");
        assert_eq!(Inst::Copy(neg.clone()).to_string(), "Ref (-1234)");
        assert_eq!(Inst::Swap.to_string(), "Swap");
        assert_eq!(Inst::Drop.to_string(), "Discard");
        assert_eq!(Inst::Slide(Integer::ZERO.into()).to_string(), "Slide 0");
        assert_eq!(Inst::Slide(pos).to_string(), "Slide 1234");
        assert_eq!(Inst::Slide(neg).to_string(), "Slide (-1234)");
        assert_eq!(Inst::Add.to_string(), "Infix Plus");
        assert_eq!(Inst::Sub.to_string(), "Infix Minus");
        assert_eq!(Inst::Mul.to_string(), "Infix Times");
        assert_eq!(Inst::Div.to_string(), "Infix Divide");
        assert_eq!(Inst::Mod.to_string(), "Infix Modulo");
        assert_eq!(Inst::Store.to_string(), "Store");
        assert_eq!(Inst::Retrieve.to_string(), "Retrieve");
        assert_eq!(Inst::Label(l.clone()).to_string(), r#"Label "  \t\t  \t ""#);
        assert_eq!(Inst::Call(l.clone()).to_string(), r#"Call "  \t\t  \t ""#);
        assert_eq!(Inst::Jmp(l.clone()).to_string(), r#"Jump "  \t\t  \t ""#);
        assert_eq!(Inst::Jz(l.clone()).to_string(), r#"If Zero "  \t\t  \t ""#);
        assert_eq!(Inst::Jn(l).to_string(), r#"If Negative "  \t\t  \t ""#);
        assert_eq!(Inst::Ret.to_string(), "Return");
        assert_eq!(Inst::End.to_string(), "End");
        assert_eq!(Inst::Printc.to_string(), "OutputChar");
        assert_eq!(Inst::Printi.to_string(), "OutputNum");
        assert_eq!(Inst::Readc.to_string(), "ReadChar");
        assert_eq!(Inst::Readi.to_string(), "ReadNum");
    }
}

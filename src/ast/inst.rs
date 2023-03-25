use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

use bitvec::prelude::*;
use rug::integer::Order;
use rug::ops::NegAssign;
use rug::Integer;
use strum::Display;

use crate::error::{Error, NumberError, ParseError};
use crate::number::IntegerExt;

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PrintableInst {
    Push(Rc<Integer>),
    Dup,
    Copy(Rc<Integer>),
    Swap,
    Drop,
    Slide(Rc<Integer>),
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
}

#[derive(Display, Clone, Copy, Debug, PartialEq, Eq)]
#[strum(serialize_all = "snake_case")]
pub enum ArgKind {
    Number,
    Label,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NumberLit {
    Number(Rc<Integer>),
    Empty,
}

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LabelLit(pub BitVec);

impl From<ParseError> for Inst {
    #[inline]
    fn from(err: ParseError) -> Self {
        Inst::ParseError(err)
    }
}

impl NumberLit {
    #[inline]
    pub fn new<T: Into<Integer>>(n: T) -> Self {
        NumberLit::Number(Rc::new(n.into()))
    }
}

impl From<Integer> for NumberLit {
    #[inline]
    fn from(n: Integer) -> Self {
        NumberLit::Number(Rc::new(n))
    }
}

impl From<BitVec> for NumberLit {
    fn from(bits: BitVec) -> Self {
        match bits.split_first() {
            Some((neg, bits)) => {
                let mut bits = BitBox::<usize, Lsb0>::from(bits);
                bits.force_align();
                bits.fill_uninitialized(false);
                bits.reverse();
                let mut n = Integer::from_digits(bits.as_raw_slice(), Order::LsfLe);
                if *neg {
                    n.neg_assign();
                }
                NumberLit::from(n)
            }
            None => NumberLit::Empty,
        }
    }
}

impl From<BitVec> for LabelLit {
    #[inline]
    fn from(bits: BitVec) -> Self {
        LabelLit(bits)
    }
}

impl Inst {
    #[inline]
    pub fn get_number(&self) -> Option<&NumberLit> {
        match self {
            Inst::Push(n) | Inst::Copy(n) | Inst::Slide(n) => Some(n),
            _ => None,
        }
    }

    #[inline]
    pub fn get_label(&self) -> Option<&LabelLit> {
        match self {
            Inst::Label(l) | Inst::Call(l) | Inst::Jmp(l) | Inst::Jz(l) | Inst::Jn(l) => Some(l),
            _ => None,
        }
    }

    #[inline]
    pub fn is_terminator(&self) -> bool {
        match self {
            Inst::Call(_)
            | Inst::Jmp(_)
            | Inst::Jz(_)
            | Inst::Jn(_)
            | Inst::Ret
            | Inst::End
            | Inst::ParseError(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn can_end_program(&self) -> bool {
        match self {
            Inst::Jmp(_) | Inst::Ret | Inst::End | Inst::ParseError(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn to_printable(&self) -> Result<PrintableInst, Error> {
        match self {
            Inst::Push(n) => Ok(PrintableInst::Push(n.unwrap()?.clone())),
            Inst::Dup => Ok(PrintableInst::Dup),
            Inst::Copy(n) => Ok(PrintableInst::Copy(n.unwrap()?.clone())),
            Inst::Swap => Ok(PrintableInst::Swap),
            Inst::Drop => Ok(PrintableInst::Drop),
            Inst::Slide(n) => Ok(PrintableInst::Slide(n.unwrap()?.clone())),
            Inst::Add => Ok(PrintableInst::Add),
            Inst::Sub => Ok(PrintableInst::Sub),
            Inst::Mul => Ok(PrintableInst::Mul),
            Inst::Div => Ok(PrintableInst::Div),
            Inst::Mod => Ok(PrintableInst::Mod),
            Inst::Store => Ok(PrintableInst::Store),
            Inst::Retrieve => Ok(PrintableInst::Retrieve),
            Inst::Label(l) => Ok(PrintableInst::Label(l.clone())),
            Inst::Call(l) => Ok(PrintableInst::Call(l.clone())),
            Inst::Jmp(l) => Ok(PrintableInst::Jmp(l.clone())),
            Inst::Jz(l) => Ok(PrintableInst::Jz(l.clone())),
            Inst::Jn(l) => Ok(PrintableInst::Jn(l.clone())),
            Inst::Ret => Ok(PrintableInst::Ret),
            Inst::End => Ok(PrintableInst::End),
            Inst::Printc => Ok(PrintableInst::Printc),
            Inst::Printi => Ok(PrintableInst::Printi),
            Inst::Readc => Ok(PrintableInst::Readc),
            Inst::Readi => Ok(PrintableInst::Readi),
            Inst::ParseError(err) => Err(err.clone().into()),
        }
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

impl Display for PrintableInst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PrintableInst::Push(n) => write!(f, "Push {}", n.to_haskell_show()),
            PrintableInst::Dup => f.write_str("Dup"),
            PrintableInst::Copy(n) => write!(f, "Ref {}", n.to_haskell_show()),
            PrintableInst::Swap => f.write_str("Swap"),
            PrintableInst::Drop => f.write_str("Discard"),
            PrintableInst::Slide(n) => write!(f, "Slide {}", n.to_haskell_show()),
            PrintableInst::Add => f.write_str("Infix Plus"),
            PrintableInst::Sub => f.write_str("Infix Minus"),
            PrintableInst::Mul => f.write_str("Infix Times"),
            PrintableInst::Div => f.write_str("Infix Divide"),
            PrintableInst::Mod => f.write_str("Infix Modulo"),
            PrintableInst::Store => f.write_str("Store"),
            PrintableInst::Retrieve => f.write_str("Retrieve"),
            PrintableInst::Label(l) => write!(f, "Label {}", l.to_haskell_show()),
            PrintableInst::Call(l) => write!(f, "Call {}", l.to_haskell_show()),
            PrintableInst::Jmp(l) => write!(f, "Jump {}", l.to_haskell_show()),
            PrintableInst::Jz(l) => write!(f, "If Zero {}", l.to_haskell_show()),
            PrintableInst::Jn(l) => write!(f, "If Negative {}", l.to_haskell_show()),
            PrintableInst::Ret => f.write_str("Return"),
            PrintableInst::End => f.write_str("End"),
            PrintableInst::Printc => f.write_str("OutputChar"),
            PrintableInst::Printi => f.write_str("OutputNum"),
            PrintableInst::Readc => f.write_str("ReadChar"),
            PrintableInst::Readi => f.write_str("ReadNum"),
        }
    }
}

impl Display for NumberLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            NumberLit::Number(n) => write!(f, "{n}"),
            NumberLit::Empty => write!(f, "<empty>"),
        }
    }
}

impl Display for LabelLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("l")?;
        for bit in &self.0 {
            f.write_str(if *bit { "1" } else { "0" })?;
        }
        Ok(())
    }
}

impl NumberLit {
    #[inline]
    pub fn unwrap(&self) -> Result<&Rc<Integer>, NumberError> {
        match self {
            NumberLit::Number(n) => Ok(n),
            NumberLit::Empty => Err(NumberError::EmptyLit),
        }
    }
}

impl LabelLit {
    pub fn to_haskell_string(&self) -> String {
        let mut s = String::with_capacity(self.0.len());
        for bit in self.0.iter().rev() {
            s.push(if *bit { '\t' } else { ' ' });
        }
        s
    }

    pub fn to_haskell_show(&self) -> String {
        let mut s = String::with_capacity(self.0.len() + self.0.count_ones() + 2);
        s.push('"');
        for bit in self.0.iter().rev() {
            s.push_str(if *bit { "\\t" } else { " " });
        }
        s.push('"');
        s
    }
}

#[cfg(test)]
mod tests {
    use bitvec::prelude::*;

    use super::*;

    #[test]
    fn display_inst() {
        macro_rules! test(($str:literal, $inst:expr) => {
            let inst = $inst.to_printable().unwrap();
            assert_eq!($str.to_owned(), inst.to_string())
        });
        macro_rules! test_empty(($inst:expr) => {
            assert_eq!(Err(NumberError::EmptyLit.into()), $inst.to_printable())
        });
        let l = LabelLit(bitvec![0, 1, 0, 0, 1, 1, 0, 0]);
        test_empty!(Inst::Push(NumberLit::Empty));
        test!("Push 0", Inst::Push(NumberLit::new(0)));
        test!("Push 1234", Inst::Push(NumberLit::new(1234)));
        test!("Push (-1234)", Inst::Push(NumberLit::new(-1234)));
        test!("Dup", Inst::Dup);
        test_empty!(Inst::Copy(NumberLit::Empty));
        test!("Ref 0", Inst::Copy(NumberLit::new(0)));
        test!("Ref 1234", Inst::Copy(NumberLit::new(1234)));
        test!("Ref (-1234)", Inst::Copy(NumberLit::new(-1234)));
        test!("Swap", Inst::Swap);
        test!("Discard", Inst::Drop);
        test_empty!(Inst::Slide(NumberLit::Empty));
        test!("Slide 0", Inst::Slide(NumberLit::new(0)));
        test!("Slide 1234", Inst::Slide(NumberLit::new(1234)));
        test!("Slide (-1234)", Inst::Slide(NumberLit::new(-1234)));
        test!("Infix Plus", Inst::Add);
        test!("Infix Minus", Inst::Sub);
        test!("Infix Times", Inst::Mul);
        test!("Infix Divide", Inst::Div);
        test!("Infix Modulo", Inst::Mod);
        test!("Store", Inst::Store);
        test!("Retrieve", Inst::Retrieve);
        test!(r#"Label "  \t\t  \t ""#, Inst::Label(l.clone()));
        test!(r#"Call "  \t\t  \t ""#, Inst::Call(l.clone()));
        test!(r#"Jump "  \t\t  \t ""#, Inst::Jmp(l.clone()));
        test!(r#"If Zero "  \t\t  \t ""#, Inst::Jz(l.clone()));
        test!(r#"If Negative "  \t\t  \t ""#, Inst::Jn(l));
        test!("Return", Inst::Ret);
        test!("End", Inst::End);
        test!("OutputChar", Inst::Printc);
        test!("OutputNum", Inst::Printi);
        test!("ReadChar", Inst::Readc);
        test!("ReadNum", Inst::Readi);
    }
}

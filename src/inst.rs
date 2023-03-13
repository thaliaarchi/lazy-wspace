use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

use bitvec::prelude::*;
use rug::integer::Order;
use rug::ops::NegAssign;
use rug::Integer;

use crate::error::{Error, NumberError, ParseError};

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

impl Display for PrintableInst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        macro_rules! write_number(($f:expr, $name:literal, $n:expr) => {{
            $f.write_str(concat!($name, " ")).and_then(|_| fmt_integer_haskell($f, $n))
        }});
        match self {
            PrintableInst::Push(n) => write_number!(f, "Push", n),
            PrintableInst::Dup => f.write_str("Dup"),
            PrintableInst::Copy(n) => write_number!(f, "Ref", n),
            PrintableInst::Swap => f.write_str("Swap"),
            PrintableInst::Drop => f.write_str("Discard"),
            PrintableInst::Slide(n) => write_number!(f, "Slide", n),
            PrintableInst::Add => f.write_str("Infix Plus"),
            PrintableInst::Sub => f.write_str("Infix Minus"),
            PrintableInst::Mul => f.write_str("Infix Times"),
            PrintableInst::Div => f.write_str("Infix Divide"),
            PrintableInst::Mod => f.write_str("Infix Modulo"),
            PrintableInst::Store => f.write_str("Store"),
            PrintableInst::Retrieve => f.write_str("Retrieve"),
            PrintableInst::Label(l) => write!(f, "Label {l}"),
            PrintableInst::Call(l) => write!(f, "Call {l}"),
            PrintableInst::Jmp(l) => write!(f, "Jump {l}"),
            PrintableInst::Jz(l) => write!(f, "If Zero {l}"),
            PrintableInst::Jn(l) => write!(f, "If Negative {l}"),
            PrintableInst::Ret => f.write_str("Return"),
            PrintableInst::End => f.write_str("End"),
            PrintableInst::Printc => f.write_str("OutputChar"),
            PrintableInst::Printi => f.write_str("OutputNum"),
            PrintableInst::Readc => f.write_str("ReadChar"),
            PrintableInst::Readi => f.write_str("ReadNum"),
        }
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

pub fn fmt_integer_haskell(f: &mut dyn fmt::Write, n: &Integer) -> fmt::Result {
    if n.cmp0() == Ordering::Less {
        write!(f, "({n})") // Parenthesize negatives
    } else {
        write!(f, "{n}")
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

use std::fmt::{self, Display, Formatter};
use std::rc::Rc;
use std::str;

use bitvec::prelude::*;
use rug::integer::Order;
use rug::ops::NegAssign;
use rug::Integer;
use wspace_syntax::hs;

use crate::error::{Error, ParseError, ValueError};

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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntegerLit {
    Integer(Rc<Integer>),
    Empty,
}

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LabelLit(pub BitVec);

impl Inst {
    #[inline]
    pub fn get_integer(&self) -> Option<&IntegerLit> {
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
    pub fn is_control_flow(&self) -> bool {
        match self {
            Inst::Label(_)
            | Inst::Call(_)
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
    pub fn err(&self) -> Option<Error> {
        match self {
            Inst::Push(IntegerLit::Empty)
            | Inst::Copy(IntegerLit::Empty)
            | Inst::Slide(IntegerLit::Empty) => Some(ValueError::EmptyLit.into()),
            Inst::ParseError(err) => Some(err.clone().into()),
            _ => None,
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

impl hs::Show for Inst {
    fn show(&self) -> String {
        match self {
            Inst::Push(IntegerLit::Integer(n)) => format!("Push {}", n.show()),
            Inst::Dup => "Dup".into(),
            Inst::Copy(IntegerLit::Integer(n)) => format!("Ref {}", n.show()),
            Inst::Swap => "Swap".into(),
            Inst::Drop => "Discard".into(),
            Inst::Slide(IntegerLit::Integer(n)) => format!("Slide {}", n.show()),
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

impl IntegerLit {
    #[inline]
    pub fn new<T: Into<Integer>>(n: T) -> Self {
        IntegerLit::Integer(Rc::new(n.into()))
    }

    #[inline]
    pub fn ok(&self) -> Result<&Rc<Integer>, ValueError> {
        match self {
            IntegerLit::Integer(n) => Ok(n),
            IntegerLit::Empty => Err(ValueError::EmptyLit),
        }
    }
}

impl Display for IntegerLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            IntegerLit::Integer(n) => write!(f, "{n}"),
            IntegerLit::Empty => write!(f, "<empty>"),
        }
    }
}

impl From<Integer> for IntegerLit {
    #[inline]
    fn from(n: Integer) -> Self {
        IntegerLit::Integer(Rc::new(n))
    }
}

impl From<BitVec> for IntegerLit {
    fn from(bits: BitVec) -> Self {
        match bits.split_first() {
            Some((sign, bits)) => IntegerLit::from(integer_from_bits(bits, *sign)),
            None => IntegerLit::Empty,
        }
    }
}

fn integer_from_bits(bits: &BitSlice<usize, Lsb0>, is_negative: bool) -> Integer {
    let mut bits = BitBox::<usize, Lsb0>::from(bits);
    bits.force_align();
    bits.fill_uninitialized(false);
    bits.reverse();
    let mut n = Integer::from_digits(bits.as_raw_slice(), Order::LsfLe);
    if is_negative {
        n.neg_assign();
    }
    n
}

impl Display for LabelLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let bits = &self.0;
        write!(f, "@")?;

        // Try to decode it as UTF-8
        if bits.len() % 8 == 0 {
            let mut bytes = Vec::with_capacity(bits.len() / 8);
            for byte in bits.chunks(8) {
                bytes.push(byte.load_le::<u8>().reverse_bits());
            }
            if let Ok(s) = str::from_utf8(&bytes) {
                if s.len() == 0
                    || s.starts_with(|c| char::is_ascii_digit(&c))
                    || s.contains(char::is_whitespace)
                {
                    return write!(f, "\"{s}\"");
                } else {
                    return write!(f, "{s}");
                }
            }
        }

        // Print in binary, if it has leading zeros or is empty
        if bits.first().as_deref() != Some(&true) {
            write!(f, "0b")?;
            for bit in bits {
                f.write_str(if *bit { "1" } else { "0" })?;
            }
            return Ok(());
        }

        // Otherwise, print it in decimal
        let n = integer_from_bits(bits, false);
        write!(f, "{n}")
    }
}

impl LabelLit {
    /// Converts to the string representation for labels used in the reference
    /// interpreter.
    pub fn to_haskell_string(&self) -> String {
        let mut s = String::with_capacity(self.0.len());
        for bit in self.0.iter().rev() {
            s.push(if *bit { '\t' } else { ' ' });
        }
        s
    }
}

/// A quoted version of [`LabelLit::to_haskell_string`].
impl hs::Show for LabelLit {
    fn show(&self) -> String {
        let mut s = String::with_capacity(self.0.len() + self.0.count_ones() + 2);
        s.push('"');
        for bit in self.0.iter().rev() {
            s.push_str(if *bit { "\\t" } else { " " });
        }
        s.push('"');
        s
    }
}

impl From<BitVec> for LabelLit {
    #[inline]
    fn from(bits: BitVec) -> Self {
        LabelLit(bits)
    }
}

#[cfg(test)]
mod tests {
    use bitvec::prelude::*;
    use wspace_syntax::hs::Show;

    use super::*;

    #[test]
    fn show_inst() {
        let l = LabelLit(bitvec![0, 1, 0, 0, 1, 1, 0, 0]);
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

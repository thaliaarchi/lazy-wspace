use std::cell::{Ref, RefCell, RefMut};
use std::cmp::Ordering;
use std::fmt::{self, Debug, Formatter};
use std::ops::{Add, AddAssign, Mul, MulAssign, Sub, SubAssign};
use std::rc::Rc;

use rug::ops::{
    AddFrom, DivRounding, DivRoundingAssign, DivRoundingFrom, MulFrom, RemRounding,
    RemRoundingAssign, RemRoundingFrom, SubFrom,
};
use rug::Integer;

use crate::ast::NumberLit;
use crate::error::{EagerError, Error, NumberError};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Number {
    Value(Rc<Integer>),
    Op(Op, NumberRef, NumberRef),
    Error(NumberError),
}

#[repr(transparent)]
#[derive(Clone, PartialEq, Eq)]
pub struct NumberRef(Rc<RefCell<Number>>);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl Number {
    #[inline]
    pub fn zero() -> Self {
        Integer::ZERO.into()
    }

    /// Parses a line of input, that contains a number of the form
    /// `/\s*-?\s*([0-9]+|0[xX][0-9A-Fa-f]+|0[oO][0-7])\s*\n?/`, where `\s`
    /// represents any characters in the Unicode property White_Space, except
    /// for line breaks U+000A, U+0085, U+2028, and U+2029.
    pub fn parse(s: String) -> Result<Self, Error> {
        if s.len() == 0 {
            return Err(EagerError::ReadEof.into());
        }

        fn is_space(ch: char) -> bool {
            match ch {
                '\u{0009}' | '\u{000B}' | '\u{000C}' | '\u{000D}' | '\u{0020}' | '\u{00A0}'
                | '\u{1680}' | '\u{2000}' | '\u{2001}' | '\u{2002}' | '\u{2003}' | '\u{2004}'
                | '\u{2005}' | '\u{2006}' | '\u{2007}' | '\u{2008}' | '\u{2009}' | '\u{200A}'
                | '\u{202F}' | '\u{205F}' | '\u{3000}' => true,
                _ => false,
            }
        }

        let number = s.strip_suffix('\n').unwrap_or(&s).trim_matches(is_space);
        let (number, is_negative) = match number.strip_prefix('-') {
            Some(number) => (number.trim_start_matches(is_space), true),
            None => (number, false),
        };

        // The digits are ASCII, so convert it to bytes and reuse the buffer
        // in-place for the Integer digits.
        let start = unsafe {
            // FIXME: replace with byte_offset_from, once stabilized
            (number.as_ptr().cast::<u8>()).offset_from(s.as_ptr().cast::<u8>()) as usize
        };
        let end = start + number.len();
        let mut b = s.into_bytes();
        let mut digits = &mut b[start..end];
        if digits.len() == 0 {
            return Ok(NumberError::ReadiParse.into());
        }

        let radix = if digits[0] == b'0' {
            match digits.get(1) {
                Some(b'x' | b'X') => {
                    digits = &mut digits[2..];
                    16
                }
                Some(b'o' | b'O') => {
                    digits = &mut digits[2..];
                    8
                }
                _ => 10,
            }
        } else {
            10
        };

        if radix == 16 {
            for i in 0..digits.len() {
                let digit = &mut digits[i];
                if b'0' <= *digit && *digit <= b'9' {
                    *digit -= b'0';
                } else if b'A' <= *digit && *digit <= b'F' {
                    *digit -= b'A' - 10;
                } else if b'a' <= *digit && *digit <= b'f' {
                    *digit -= b'a' - 10;
                } else {
                    return Ok(NumberError::ReadiParse.into());
                }
            }
        } else {
            for i in 0..digits.len() {
                let digit = &mut digits[i];
                if b'0' <= *digit && *digit < b'0' + radix as u8 {
                    *digit -= b'0';
                } else {
                    return Ok(NumberError::ReadiParse.into());
                }
            }
        }

        let mut n = Integer::new();
        unsafe { n.assign_bytes_radix_unchecked(&digits, radix, is_negative) }
        Ok(n.into())
    }

    #[inline]
    fn eval_op(op: Op, x: NumberRef, y: NumberRef) -> Result<Rc<Integer>, NumberError> {
        let x = x.eval()?;
        let y = y.eval()?;
        match op.eval(x, y) {
            Some(z) => Ok(Rc::new(z)),
            None => Err(NumberError::DivModZero),
        }
    }
}

impl NumberRef {
    pub fn eval(self) -> Result<Rc<Integer>, NumberError> {
        match &*self.0.borrow() {
            Number::Value(n) => return Ok(n.clone()),
            Number::Op(_, _, _) => {}
            Number::Error(err) => return Err(err.clone()),
        }

        let cell = self.0;
        let n = cell.replace(Number::Error(NumberError::Internal));
        let res = match n {
            Number::Op(op, x, y) => Number::eval_op(op, x, y),
            _ => unreachable!(),
        };
        let inner = cell.replace(match &res {
            Ok(n) => Number::Value(n.clone()),
            Err(err) => Number::Error(err.clone()),
        });
        debug_assert_eq!(Number::Error(NumberError::Internal), inner);
        res
    }

    #[inline]
    pub fn borrow(&self) -> Ref<'_, Number> {
        self.0.borrow()
    }

    #[inline]
    pub fn borrow_mut(&self) -> RefMut<'_, Number> {
        self.0.borrow_mut()
    }
}

impl Op {
    pub fn eval(self, lhs: Rc<Integer>, rhs: Rc<Integer>) -> Option<Integer> {
        match self {
            Op::Add => Some(lhs.add_rc(rhs)),
            Op::Sub => Some(lhs.sub_rc(rhs)),
            Op::Mul => Some(lhs.mul_rc(rhs)),
            Op::Div => lhs.div_floor_rc(rhs),
            Op::Mod => lhs.rem_floor_rc(rhs),
        }
    }
}

pub trait IntegerExt {
    fn add_rc(self: Rc<Self>, rhs: Rc<Self>) -> Integer;
    fn sub_rc(self: Rc<Self>, rhs: Rc<Self>) -> Integer;
    fn mul_rc(self: Rc<Self>, rhs: Rc<Self>) -> Integer;
    fn div_floor_rc(self: Rc<Self>, rhs: Rc<Self>) -> Option<Integer>;
    fn rem_floor_rc(self: Rc<Self>, rhs: Rc<Self>) -> Option<Integer>;
    fn to_haskell_show(&self) -> String;
}

macro_rules! arith_op(($lhs:expr, $rhs:expr, $op:ident, $op_assign:ident, $op_from:ident) => {
    match (Rc::try_unwrap($lhs), Rc::try_unwrap($rhs)) {
        (Ok(mut lhs), Ok(mut rhs)) => {
            if rhs_has_more_alloc(&lhs, &rhs) {
                rhs.$op_from(lhs);
                rhs
            } else {
                lhs.$op_assign(rhs);
                lhs
            }
        }
        (Ok(mut lhs), Err(rhs)) => {
            lhs.$op_assign(&*rhs);
            lhs
        }
        (Err(lhs), Ok(mut rhs)) => {
            rhs.$op_from(&*lhs);
            rhs
        }
        (Err(lhs), Err(rhs)) => (&*lhs).$op(&*rhs).into(),
    }
});

// A copy of rhs_has_more_alloc(x, y) from Rug
#[inline]
fn rhs_has_more_alloc(lhs: &Integer, rhs: &Integer) -> bool {
    unsafe { (*lhs.as_raw()).alloc < (*rhs.as_raw()).alloc }
}

impl IntegerExt for Integer {
    fn add_rc(self: Rc<Self>, rhs: Rc<Self>) -> Integer {
        arith_op!(self, rhs, add, add_assign, add_from)
    }

    fn sub_rc(self: Rc<Self>, rhs: Rc<Self>) -> Integer {
        arith_op!(self, rhs, sub, sub_assign, sub_from)
    }

    fn mul_rc(self: Rc<Self>, rhs: Rc<Self>) -> Integer {
        arith_op!(self, rhs, mul, mul_assign, mul_from)
    }

    fn div_floor_rc(self: Rc<Self>, rhs: Rc<Self>) -> Option<Integer> {
        if rhs.cmp0() == Ordering::Equal {
            None
        } else {
            Some(arith_op!(
                self,
                rhs,
                div_floor,
                div_floor_assign,
                div_floor_from
            ))
        }
    }

    fn rem_floor_rc(self: Rc<Self>, rhs: Rc<Self>) -> Option<Integer> {
        if rhs.cmp0() == Ordering::Equal {
            None
        } else {
            Some(arith_op!(
                self,
                rhs,
                rem_floor,
                rem_floor_assign,
                rem_floor_from
            ))
        }
    }

    fn to_haskell_show(&self) -> String {
        if self.cmp0() == Ordering::Less {
            format!("({self})")
        } else {
            self.to_string()
        }
    }
}

impl From<&NumberLit> for Number {
    #[inline]
    fn from(n: &NumberLit) -> Self {
        match n {
            NumberLit::Number(n) => Number::Value(n.clone()),
            NumberLit::Empty => Number::Error(NumberError::EmptyLit),
        }
    }
}

impl<T: Into<Integer>> From<T> for Number {
    #[inline]
    fn from(v: T) -> Self {
        Number::Value(Rc::new(v.into()))
    }
}

impl From<NumberError> for Number {
    #[inline]
    fn from(err: NumberError) -> Self {
        Number::Error(err)
    }
}

impl<T: Into<Number>> From<T> for NumberRef {
    #[inline]
    fn from(v: T) -> Self {
        NumberRef(Rc::new(RefCell::new(v.into())))
    }
}

impl Debug for NumberRef {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&*self.borrow(), f)
    }
}

impl PartialEq<NumberRef> for Number {
    fn eq(&self, other: &NumberRef) -> bool {
        self == &*other.borrow()
    }
}

impl PartialEq<Number> for NumberRef {
    fn eq(&self, other: &Number) -> bool {
        &*self.borrow() == other
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_update_refs() {
        let x = NumberRef::from(1);
        let y = NumberRef::from(2);
        let z = NumberRef::from(Number::Op(Op::Add, x, y));
        let z1 = z.clone();
        let z2 = z.eval().unwrap();
        assert_eq!(Integer::from(3), *z2);
        assert_eq!(NumberRef::from(3), z1);
    }

    #[test]
    fn eval_error_order() {
        let x = NumberError::CopyLarge.into();
        let y = NumberError::EmptyLit.into();
        let z = NumberRef::from(Number::Op(Op::Add, x, y));
        let z1 = z.clone();
        let err = z.eval().unwrap_err();
        assert_eq!(NumberError::CopyLarge, err);
        assert_eq!(NumberRef::from(NumberError::CopyLarge), z1);
    }
}

use std::cell::{Ref, RefCell, RefMut};
use std::cmp::Ordering;
use std::fmt::{self, Debug, Formatter};
use std::ops::{Add, AddAssign, Mul, MulAssign, Sub, SubAssign};
use std::rc::Rc;
use std::str::FromStr;

use rug::ops::{
    AddFrom, DivRounding, DivRoundingAssign, DivRoundingFrom, MulFrom, RemRounding,
    RemRoundingAssign, RemRoundingFrom, SubFrom,
};
use rug::Integer;
use strum::Display;
use wspace_syntax::hs::ReadIntegerLit;

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

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "snake_case")]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

/// Parses a line of input as a number for `readi`.
impl FromStr for Number {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Err(EagerError::ReadEof.into());
        }
        match s.parse::<ReadIntegerLit>() {
            Ok(lit) => Ok(Number::Value(Rc::new(lit.value))),
            Err(_) => Ok(NumberError::ReadiParse.into()),
        }
    }
}

impl Number {
    #[inline]
    pub fn zero() -> Self {
        Integer::ZERO.into()
    }

    fn eval_op(op: Op, lhs: NumberRef, rhs: NumberRef) -> Result<Rc<Integer>, NumberError> {
        // Matches the evaluation order of the reference interpreter.
        let v = match op {
            Op::Add => {
                let rhs = rhs.eval()?;
                let lhs = lhs.eval()?;
                add_rc(lhs, rhs)
            }
            Op::Sub => {
                let rhs = rhs.eval()?;
                let lhs = lhs.eval()?;
                sub_rc(lhs, rhs)
            }
            Op::Mul => {
                let lhs = lhs.eval()?;
                let rhs = rhs.eval()?;
                mul_rc(lhs, rhs)
            }
            Op::Div => {
                let rhs = rhs.eval()?;
                if rhs.cmp0() == Ordering::Equal {
                    return Err(NumberError::DivModZero);
                }
                let lhs = lhs.eval()?;
                div_floor_rc(lhs, rhs)
            }
            Op::Mod => {
                let rhs = rhs.eval()?;
                if rhs.cmp0() == Ordering::Equal {
                    return Err(NumberError::DivModZero);
                }
                let lhs = lhs.eval()?;
                rem_floor_rc(lhs, rhs)
            }
        };
        Ok(Rc::new(v))
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
            Number::Op(op, lhs, rhs) => Number::eval_op(op, lhs, rhs),
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

fn add_rc(lhs: Rc<Integer>, rhs: Rc<Integer>) -> Integer {
    arith_op!(lhs, rhs, add, add_assign, add_from)
}

fn sub_rc(lhs: Rc<Integer>, rhs: Rc<Integer>) -> Integer {
    arith_op!(lhs, rhs, sub, sub_assign, sub_from)
}

fn mul_rc(lhs: Rc<Integer>, rhs: Rc<Integer>) -> Integer {
    arith_op!(lhs, rhs, mul, mul_assign, mul_from)
}

fn div_floor_rc(lhs: Rc<Integer>, rhs: Rc<Integer>) -> Integer {
    arith_op!(lhs, rhs, div_floor, div_floor_assign, div_floor_from)
}

fn rem_floor_rc(lhs: Rc<Integer>, rhs: Rc<Integer>) -> Integer {
    arith_op!(lhs, rhs, rem_floor, rem_floor_assign, rem_floor_from)
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
    #[inline]
    fn eq(&self, other: &NumberRef) -> bool {
        self == &*other.borrow()
    }
}

impl PartialEq<Number> for NumberRef {
    #[inline]
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
        let z1 = z.clone().eval().unwrap();
        assert_eq!(Integer::from(3), *z1);
        assert_eq!(NumberRef::from(3), z);
    }

    #[test]
    fn eval_error_order() {
        let x = NumberError::CopyLarge.into();
        let y = NumberError::EmptyLit.into();
        let z = NumberRef::from(Number::Op(Op::Add, x, y));
        let err = z.clone().eval().unwrap_err();
        assert_eq!(NumberError::EmptyLit, err);
        assert_eq!(NumberRef::from(NumberError::EmptyLit), z);
    }
}

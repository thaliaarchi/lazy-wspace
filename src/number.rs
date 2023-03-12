use std::cell::RefCell;
use std::cmp::Ordering;
use std::ops::{Add, AddAssign, Mul, MulAssign, Sub, SubAssign};
use std::rc::Rc;

use rug::ops::{
    AddFrom, DivRounding, DivRoundingAssign, DivRoundingFrom, MulFrom, RemRounding,
    RemRoundingAssign, RemRoundingFrom, SubFrom,
};
use rug::Integer;

use crate::error::NumberError;
use crate::inst::NumberLit;

pub type NumberRef = Rc<RefCell<Number>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Number {
    Value(Rc<Integer>),
    Op(Op, NumberRef, NumberRef),
    Error(NumberError),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

    pub fn eval(n: NumberRef) -> Result<Rc<Integer>, NumberError> {
        match &*n.borrow() {
            Number::Value(n) => return Ok(n.clone()),
            Number::Op(_, _, _) => {}
            Number::Error(err) => return Err(*err),
        }

        let cell = n;
        let n = cell.replace(Number::Error(NumberError::Internal));
        let res = match n {
            Number::Op(op, x, y) => Number::eval_op(op, x, y),
            _ => unreachable!(),
        };
        let inner = cell.replace(match &res {
            Ok(n) => Number::Value(n.clone()),
            Err(err) => Number::Error(*err),
        });
        debug_assert_eq!(Number::Error(NumberError::Internal), inner);
        res
    }

    #[inline]
    fn eval_op(op: Op, x: NumberRef, y: NumberRef) -> Result<Rc<Integer>, NumberError> {
        let x = Number::eval(x)?;
        let y = Number::eval(y)?;
        match op.eval(x, y) {
            Some(z) => Ok(Rc::new(z)),
            None => Err(NumberError::DivModZero),
        }
    }
}

impl Op {
    pub fn eval(self, x: Rc<Integer>, y: Rc<Integer>) -> Option<Integer> {
        match self {
            Op::Add => Some(add_rc(x, y)),
            Op::Sub => Some(sub_rc(x, y)),
            Op::Mul => Some(mul_rc(x, y)),
            Op::Div => div_rc(x, y),
            Op::Mod => mod_rc(x, y),
        }
    }
}

macro_rules! arith_op(($x:expr, $y:expr, $op:ident, $op_assign:ident, $op_from:ident) => {
    match (Rc::try_unwrap($x), Rc::try_unwrap($y)) {
        (Ok(mut x), Ok(mut y)) => {
            if rhs_has_more_alloc(&x, &y) {
                y.$op_from(x);
                y
            } else {
                x.$op_assign(y);
                x
            }
        }
        (Ok(mut x), Err(y)) => {
            x.$op_assign(&*y);
            x
        }
        (Err(x), Ok(mut y)) => {
            y.$op_from(&*x);
            y
        }
        (Err(x), Err(y)) => (&*x).$op(&*y).into(),
    }
});

// A copy of rhs_has_more_alloc(x, y) from Rug
#[inline]
fn rhs_has_more_alloc(lhs: &Integer, rhs: &Integer) -> bool {
    unsafe { (*lhs.as_raw()).alloc < (*rhs.as_raw()).alloc }
}

fn add_rc(x: Rc<Integer>, y: Rc<Integer>) -> Integer {
    arith_op!(x, y, add, add_assign, add_from)
}
fn sub_rc(x: Rc<Integer>, y: Rc<Integer>) -> Integer {
    arith_op!(x, y, sub, sub_assign, sub_from)
}
fn mul_rc(x: Rc<Integer>, y: Rc<Integer>) -> Integer {
    arith_op!(x, y, mul, mul_assign, mul_from)
}
fn div_rc(x: Rc<Integer>, y: Rc<Integer>) -> Option<Integer> {
    if y.cmp0() == Ordering::Equal {
        None
    } else {
        Some(arith_op!(x, y, div_floor, div_floor_assign, div_floor_from))
    }
}
fn mod_rc(x: Rc<Integer>, y: Rc<Integer>) -> Option<Integer> {
    if y.cmp0() == Ordering::Equal {
        None
    } else {
        Some(arith_op!(x, y, rem_floor, rem_floor_assign, rem_floor_from))
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
    fn from(n: T) -> Self {
        Number::Value(Rc::new(n.into()))
    }
}

impl From<NumberError> for Number {
    #[inline]
    fn from(err: NumberError) -> Self {
        Number::Error(err)
    }
}

impl From<Number> for NumberRef {
    #[inline]
    fn from(n: Number) -> Self {
        Rc::new(RefCell::new(n))
    }
}

impl From<&NumberLit> for NumberRef {
    #[inline]
    fn from(n: &NumberLit) -> Self {
        Rc::new(RefCell::new(n.into()))
    }
}

impl From<NumberError> for NumberRef {
    #[inline]
    fn from(err: NumberError) -> Self {
        Rc::new(RefCell::new(err.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_update_refs() {
        let x = Number::from(1).into();
        let y = Number::from(2).into();
        let z = NumberRef::from(Number::Op(Op::Add, x, y));
        let z1 = z.clone();
        let z2 = Number::eval(z).unwrap();
        assert_eq!(Integer::from(3), *z2);
        assert_eq!(NumberRef::from(Number::from(3)), z1);
    }

    #[test]
    fn eval_error_order() {
        let x = NumberRef::from(NumberError::CopyLarge);
        let y = NumberRef::from(NumberError::EmptyLit);
        let z = NumberRef::from(Number::Op(Op::Add, x, y));
        let z1 = z.clone();
        let err = Number::eval(z).unwrap_err();
        assert_eq!(NumberError::CopyLarge, err);
        assert_eq!(NumberRef::from(NumberError::CopyLarge), z1);
    }
}

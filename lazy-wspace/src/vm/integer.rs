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
use wspace_syntax::ws::ast::IntegerLit;

use crate::error::{EagerError, Error, ValueError};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Integer(Rc<Integer>),
    Op(Op, ValueRef, ValueRef),
    Error(ValueError),
}

#[repr(transparent)]
#[derive(Clone, PartialEq, Eq)]
pub struct ValueRef(Rc<RefCell<Value>>);

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "snake_case")]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

/// Parses a line of input as an integer for `readi`.
impl FromStr for Value {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Err(EagerError::ReadiEof.into());
        }
        match s.parse::<ReadIntegerLit>() {
            Ok(lit) => Ok(Value::Integer(Rc::new(lit.value))),
            Err(_) => Ok(ValueError::ReadiParse.into()),
        }
    }
}

impl Value {
    #[inline]
    pub fn integer<T: Into<Integer>>(n: T) -> Self {
        Value::Integer(Rc::new(n.into()))
    }

    #[inline]
    pub fn zero() -> Self {
        Integer::ZERO.into()
    }

    fn eval_op(op: Op, lhs: ValueRef, rhs: ValueRef) -> Result<Rc<Integer>, ValueError> {
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
                    return Err(ValueError::DivModZero);
                }
                let lhs = lhs.eval()?;
                div_floor_rc(lhs, rhs)
            }
            Op::Mod => {
                let rhs = rhs.eval()?;
                if rhs.cmp0() == Ordering::Equal {
                    return Err(ValueError::DivModZero);
                }
                let lhs = lhs.eval()?;
                rem_floor_rc(lhs, rhs)
            }
        };
        Ok(Rc::new(v))
    }
}

impl ValueRef {
    pub fn eval(self) -> Result<Rc<Integer>, ValueError> {
        match &*self.0.borrow() {
            Value::Integer(n) => return Ok(n.clone()),
            Value::Op(_, _, _) => {}
            Value::Error(err) => return Err(err.clone()),
        }

        let cell = self.0;
        let n = cell.replace(Value::Error(ValueError::EmptyLit));
        let res = match n {
            Value::Op(op, lhs, rhs) => Value::eval_op(op, lhs, rhs),
            _ => unreachable!(),
        };
        let inner = cell.replace(match &res {
            Ok(n) => Value::Integer(n.clone()),
            Err(err) => Value::Error(err.clone()),
        });
        debug_assert_eq!(Value::Error(ValueError::EmptyLit), inner);
        res
    }

    #[inline]
    pub fn borrow(&self) -> Ref<'_, Value> {
        self.0.borrow()
    }

    #[inline]
    pub fn borrow_mut(&self) -> RefMut<'_, Value> {
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

impl From<&IntegerLit> for Value {
    #[inline]
    fn from(n: &IntegerLit) -> Self {
        match n.value() {
            Some(n) => Value::Integer(Rc::new(n.clone())),
            None => Value::Error(ValueError::EmptyLit),
        }
    }
}

impl From<Integer> for Value {
    #[inline]
    fn from(n: Integer) -> Self {
        Value::Integer(Rc::new(n))
    }
}

impl From<ValueError> for Value {
    #[inline]
    fn from(err: ValueError) -> Self {
        Value::Error(err)
    }
}

impl<T: Into<Value>> From<T> for ValueRef {
    #[inline]
    fn from(v: T) -> Self {
        ValueRef(Rc::new(RefCell::new(v.into())))
    }
}

impl Debug for ValueRef {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&*self.borrow(), f)
    }
}

impl PartialEq<ValueRef> for Value {
    #[inline]
    fn eq(&self, other: &ValueRef) -> bool {
        self == &*other.borrow()
    }
}

impl PartialEq<Value> for ValueRef {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        &*self.borrow() == other
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_update_refs() {
        let x = ValueRef::from(Value::integer(1));
        let y = ValueRef::from(Value::integer(2));
        let z = ValueRef::from(Value::Op(Op::Add, x, y));
        let z1 = z.clone().eval().unwrap();
        assert_eq!(Integer::from(3), *z1);
        assert_eq!(ValueRef::from(Value::integer(3)), z);
    }

    #[test]
    fn eval_error_order() {
        let x = ValueError::CopyLarge.into();
        let y = ValueError::EmptyLit.into();
        let z = ValueRef::from(Value::Op(Op::Add, x, y));
        let err = z.clone().eval().unwrap_err();
        assert_eq!(ValueError::EmptyLit, err);
        assert_eq!(ValueRef::from(ValueError::EmptyLit), z);
    }
}

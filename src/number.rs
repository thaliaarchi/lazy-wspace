use std::cell::RefCell;
use std::rc::Rc;

use rug::Integer;

use crate::error::LazyError;
use crate::inst::NumberLit;

pub type NumberRef = Rc<RefCell<Number>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Number {
    Value(Integer),
    Op(ArithOp, NumberRef, NumberRef),
    Error(LazyError),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl From<&NumberLit> for Number {
    #[inline]
    fn from(n: &NumberLit) -> Self {
        match n {
            NumberLit::Number(n) => Number::Value(n.as_ref().clone()),
            NumberLit::Empty => Number::Error(LazyError::EmptyLit),
        }
    }
}

impl From<LazyError> for Number {
    #[inline]
    fn from(err: LazyError) -> Self {
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

impl From<LazyError> for NumberRef {
    #[inline]
    fn from(err: LazyError) -> Self {
        Rc::new(RefCell::new(err.into()))
    }
}

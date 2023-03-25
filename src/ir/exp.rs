use std::cell::{Ref, RefCell, RefMut};
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use rug::Integer;

use crate::ast::NumberLit;
use crate::error::NumberError;
use crate::number::Op;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Exp {
    Value(Rc<Integer>),
    Op(Op, ExpRef, ExpRef),
    StackRef(usize),
    LazyStackRef(usize),
    HeapRef(ExpRef),
    Error(NumberError),
}

#[repr(transparent)]
#[derive(Clone, PartialEq, Eq)]
pub struct ExpRef(Rc<RefCell<Exp>>);

impl ExpRef {
    #[inline]
    pub fn is_unique(&self) -> bool {
        Rc::strong_count(&self.0) == 1
    }

    #[inline]
    pub fn same_ref(&self, other: &ExpRef) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    #[inline]
    pub fn as_ptr(&self) -> *const RefCell<Exp> {
        Rc::as_ptr(&self.0)
    }

    #[inline]
    pub fn borrow(&self) -> Ref<'_, Exp> {
        self.0.borrow()
    }

    #[inline]
    pub fn borrow_mut(&self) -> RefMut<'_, Exp> {
        self.0.borrow_mut()
    }
}

impl From<&NumberLit> for Exp {
    #[inline]
    fn from(n: &NumberLit) -> Self {
        match n {
            NumberLit::Number(n) => Exp::Value(n.clone()),
            NumberLit::Empty => Exp::Error(NumberError::EmptyLit),
        }
    }
}

impl<T: Into<Integer>> From<T> for Exp {
    #[inline]
    fn from(v: T) -> Self {
        Exp::Value(Rc::new(v.into()))
    }
}

impl From<NumberError> for Exp {
    #[inline]
    fn from(err: NumberError) -> Self {
        Exp::Error(err)
    }
}

impl<T: Into<Exp>> From<T> for ExpRef {
    #[inline]
    fn from(v: T) -> Self {
        ExpRef(Rc::new(RefCell::new(v.into())))
    }
}

impl Debug for ExpRef {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&*self.borrow(), f)
    }
}

impl PartialEq<ExpRef> for Exp {
    #[inline]
    fn eq(&self, other: &ExpRef) -> bool {
        self == &*other.borrow()
    }
}

impl PartialEq<Exp> for ExpRef {
    #[inline]
    fn eq(&self, other: &Exp) -> bool {
        &*self.borrow() == other
    }
}

impl Hash for ExpRef {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.borrow().hash(state);
    }
}

impl Display for ExpRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &*self.borrow() {
            Exp::Value(n) => write!(f, "value {n}"),
            Exp::Op(op, l, r) => write!(f, "{op} ({l}) ({r})"),
            Exp::StackRef(n) => write!(f, "stack_ref {n}"),
            Exp::LazyStackRef(n) => write!(f, "lazy_stack_ref {n}"),
            Exp::HeapRef(addr) => write!(f, "heap_ref {addr}"),
            Exp::Error(err) => write!(f, "error {err:?}"),
        }
    }
}

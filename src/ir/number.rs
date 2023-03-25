use std::cell::{Ref, RefCell, RefMut};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use rug::Integer;

use crate::ast::NumberLit;
use crate::error::NumberError;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AbstractNumber {
    Value(Rc<Integer>),
    StackRef(usize),
    LazyStackRef(usize),
    HeapRef(AbstractNumberRef),
    Error(NumberError),
}

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbstractNumberRef(Rc<RefCell<AbstractNumber>>);

impl AbstractNumberRef {
    #[inline]
    pub fn is_unique(&self) -> bool {
        Rc::strong_count(&self.0) == 1
    }

    #[inline]
    pub fn same_ref(&self, other: &AbstractNumberRef) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    #[inline]
    pub fn borrow(&self) -> Ref<'_, AbstractNumber> {
        self.0.borrow()
    }

    #[inline]
    pub fn borrow_mut(&self) -> RefMut<'_, AbstractNumber> {
        self.0.borrow_mut()
    }
}

impl From<&NumberLit> for AbstractNumber {
    #[inline]
    fn from(n: &NumberLit) -> Self {
        match n {
            NumberLit::Number(n) => AbstractNumber::Value(n.clone()),
            NumberLit::Empty => AbstractNumber::Error(NumberError::EmptyLit),
        }
    }
}

impl<T: Into<Integer>> From<T> for AbstractNumber {
    #[inline]
    fn from(v: T) -> Self {
        AbstractNumber::Value(Rc::new(v.into()))
    }
}

impl From<NumberError> for AbstractNumber {
    #[inline]
    fn from(err: NumberError) -> Self {
        AbstractNumber::Error(err)
    }
}

impl<T: Into<AbstractNumber>> From<T> for AbstractNumberRef {
    #[inline]
    fn from(v: T) -> Self {
        AbstractNumberRef(Rc::new(RefCell::new(v.into())))
    }
}

impl PartialEq<AbstractNumberRef> for AbstractNumber {
    #[inline]
    fn eq(&self, other: &AbstractNumberRef) -> bool {
        self == &*other.0.borrow()
    }
}

impl PartialEq<AbstractNumber> for AbstractNumberRef {
    #[inline]
    fn eq(&self, other: &AbstractNumber) -> bool {
        &*self.0.borrow() == other
    }
}

impl Hash for AbstractNumberRef {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.borrow().hash(state);
    }
}

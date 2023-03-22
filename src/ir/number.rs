use std::cell::RefCell;
use std::rc::Rc;

use crate::number::Number;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AbstractNumber {
    Number(Number),
    StackRef(usize),
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
}

impl From<AbstractNumber> for AbstractNumberRef {
    #[inline]
    fn from(n: AbstractNumber) -> Self {
        AbstractNumberRef(Rc::new(RefCell::new(n)))
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

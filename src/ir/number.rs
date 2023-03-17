use std::cell::RefCell;
use std::rc::Rc;

use crate::number::Number;

pub type AbstractNumberRef = Rc<RefCell<AbstractNumber>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AbstractNumber {
    Number(Number),
    StackRef(usize),
}

impl From<AbstractNumber> for AbstractNumberRef {
    #[inline]
    fn from(n: AbstractNumber) -> Self {
        Rc::new(RefCell::new(n))
    }
}

impl PartialEq<AbstractNumberRef> for AbstractNumber {
    #[inline]
    fn eq(&self, other: &AbstractNumberRef) -> bool {
        self == &*other.borrow()
    }
}

impl PartialEq<AbstractNumber> for AbstractNumberRef {
    #[inline]
    fn eq(&self, other: &AbstractNumber) -> bool {
        &*self.borrow() == other
    }
}

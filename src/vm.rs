use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

use rug::Integer;

use crate::error::{Error, LazyError};
use crate::inst::{Inst, NumberLit};
use crate::number::NumberRef;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VM<'a> {
    src: &'a [u8],
    prog: Vec<Inst>,
    stack: Stack,
    heap: Heap,
    pc: usize,
    call_stack: Vec<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Stack {
    s: Vec<NumberRef>,
    bad_slide: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Heap {
    small: HashMap<i64, NumberRef>,
    big: HashMap<Rc<Integer>, NumberRef>,
}

impl Stack {
    #[inline]
    pub fn new() -> Self {
        Stack {
            s: Vec::new(),
            bad_slide: false,
        }
    }

    #[inline]
    pub fn stack(&self) -> &[NumberRef] {
        &self.s
    }

    #[inline]
    pub fn push(&mut self, n: &NumberLit) {
        self.s.push(n.into());
    }

    #[inline]
    pub fn dup(&mut self) -> Result<(), Error> {
        self.s.push(self.top(|| Inst::Dup)?.clone());
        Ok(())
    }

    #[inline]
    pub fn copy(&mut self, i: &NumberLit) {
        let x = match i {
            NumberLit::Number(i) => {
                if i.cmp0() == Ordering::Less {
                    LazyError::CopyNegative.into()
                } else {
                    let i = i.to_usize().unwrap_or(usize::MAX);
                    match self.s.get(self.s.len().wrapping_sub(i)) {
                        Some(n) => n.clone(),
                        None => LazyError::CopyLarge.into(),
                    }
                }
            }
            NumberLit::Empty => LazyError::EmptyLit.into(),
        };
        self.s.push(x);
    }

    #[inline]
    pub fn swap(&mut self) -> Result<(), Error> {
        let n = self.pop(|| Inst::Swap)?;
        let m = self.pop(|| Inst::Swap)?;
        self.s.push(n);
        self.s.push(m);
        Ok(())
    }

    #[inline]
    pub fn drop(&mut self) -> Result<(), Error> {
        self.pop(|| Inst::Drop)?;
        Ok(())
    }

    #[inline]
    pub fn slide(&mut self, n: &NumberLit) -> Result<(), Error> {
        // When `n` is positive, `n` values are popped off the stack under
        // the top. When `n` is negative or zero, the stack under the top is
        // unchanged. When `n` is an empty literal, accessing the stack under
        // the top is a lazily evaluated error.

        let top = self.pop(|| Inst::Slide(n.clone()))?;
        match n {
            NumberLit::Number(n) => {
                if n.cmp0() == Ordering::Greater {
                    let n = n.to_usize().unwrap_or(usize::MAX);
                    self.s.truncate(self.s.len().saturating_sub(n));
                }
            }
            NumberLit::Empty => {
                self.bad_slide = true;
                self.s.clear();
            }
        }
        self.s.push(top);
        Ok(())
    }

    #[inline]
    fn top<F: FnOnce() -> Inst>(&self, get_inst: F) -> Result<&NumberRef, Error> {
        self.s.get(self.s.len().wrapping_sub(1)).ok_or_else(|| {
            if self.bad_slide {
                Error::Lazy(LazyError::EmptyLit)
            } else {
                Error::Underflow(get_inst())
            }
        })
    }

    #[inline]
    fn pop<F: FnOnce() -> Inst>(&mut self, get_inst: F) -> Result<NumberRef, Error> {
        self.s.pop().ok_or_else(|| {
            if self.bad_slide {
                Error::Lazy(LazyError::EmptyLit)
            } else {
                Error::Underflow(get_inst())
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn copy_empty() {
        let mut s = Stack::new();
        s.copy(&NumberLit::Empty);
        assert_eq!(&[NumberRef::from(LazyError::EmptyLit)], s.stack());
    }

    #[test]
    fn copy_negative() {
        let mut s = Stack::new();
        s.copy(&NumberLit::from(-1));
        assert_eq!(&[NumberRef::from(LazyError::CopyNegative)], s.stack());
    }

    #[test]
    fn copy_large() {
        let mut s = Stack::new();
        s.copy(&NumberLit::from(1));
        assert_eq!(&[NumberRef::from(LazyError::CopyLarge)], s.stack());
    }

    #[test]
    fn slide_empty() {
        let mut s = Stack::new();
        s.push(&NumberLit::from(1));
        s.slide(&NumberLit::Empty).unwrap();
        s.drop().unwrap();
        assert_eq!(Err(LazyError::EmptyLit.into()), s.drop());
    }

    #[test]
    fn slide_negative() {
        let mut s = Stack::new();
        s.push(&NumberLit::from(1));
        s.slide(&NumberLit::from(-2)).unwrap();
        s.drop().unwrap();
        assert_eq!(Err(Error::Underflow(Inst::Drop)), s.drop());
    }
}

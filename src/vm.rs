use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

use rug::Integer;

use crate::error::{Error, LazyError};
use crate::inst::{Inst, NumberLit};
use crate::number::{ArithOp, Number, NumberRef};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VM<'a> {
    src: &'a [u8],
    prog: Vec<Inst>,
    stack: Vec<NumberRef>,
    heap: Heap,
    pc: usize,
    call_stack: Vec<usize>,
    on_underflow: UnderflowError,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnderflowError {
    Pop,
    SlideEmpty,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Heap {
    small: HashMap<i64, NumberRef>,
    big: HashMap<Rc<Integer>, NumberRef>,
}

impl<'a> VM<'a> {
    #[inline]
    pub fn new(src: &'a [u8]) -> Self {
        VM {
            src,
            prog: Vec::new(),
            stack: Vec::new(),
            heap: Heap::new(),
            pc: 0,
            call_stack: Vec::new(),
            on_underflow: UnderflowError::Pop,
        }
    }

    #[inline]
    pub fn stack(&self) -> &[NumberRef] {
        &self.stack
    }

    pub fn step(&mut self) -> Result<(), Error> {
        let inst = &self.prog[self.pc];
        macro_rules! pop(() => {
            self.stack.pop().ok_or_else(|| self.underflow(inst))
        });
        macro_rules! top(() => {
            self.stack
                .get(self.stack.len().wrapping_sub(1))
                .ok_or_else(|| self.underflow(inst))
        });
        macro_rules! arith(($op:expr) => {{
            let y = pop!()?;
            let x = pop!()?;
            self.stack.push(Number::Op($op, x, y).into());
        }});
        match inst {
            Inst::Push(n) => {
                self.stack.push(n.into());
            }
            Inst::Dup => {
                self.stack.push(top!()?.clone());
            }
            Inst::Copy(i) => {
                let x = match i {
                    NumberLit::Number(i) => {
                        if i.cmp0() == Ordering::Less {
                            LazyError::CopyNegative.into()
                        } else {
                            let i = i.to_usize().unwrap_or(usize::MAX);
                            match self.stack.get(self.stack.len().wrapping_sub(i)) {
                                Some(n) => n.clone(),
                                None => LazyError::CopyLarge.into(),
                            }
                        }
                    }
                    NumberLit::Empty => LazyError::EmptyLit.into(),
                };
                self.stack.push(x);
            }
            Inst::Swap => {
                let n = pop!()?;
                let m = pop!()?;
                self.stack.push(n);
                self.stack.push(m);
            }
            Inst::Drop => pop!().map(|_| ())?,
            Inst::Slide(n) => {
                // When `n` is positive, `n` values are popped off the stack under
                // the top. When `n` is negative or zero, the stack under the top is
                // unchanged. When `n` is an empty literal, accessing the stack under
                // the top is a lazily evaluated error.

                let top = pop!()?;
                match n {
                    NumberLit::Number(n) => {
                        if n.cmp0() == Ordering::Greater {
                            let n = n.to_usize().unwrap_or(usize::MAX);
                            self.stack.truncate(self.stack.len().saturating_sub(n));
                        }
                    }
                    NumberLit::Empty => {
                        self.on_underflow = UnderflowError::SlideEmpty;
                        self.stack.clear();
                    }
                }
                self.stack.push(top);
            }
            Inst::Add => arith!(ArithOp::Add),
            Inst::Sub => arith!(ArithOp::Sub),
            Inst::Mul => arith!(ArithOp::Mul),
            Inst::Div => arith!(ArithOp::Div),
            Inst::Mod => arith!(ArithOp::Mod),
            Inst::Store => todo!(),
            Inst::Retrieve => todo!(),
            Inst::Label(_) => todo!(),
            Inst::Call(_) => todo!(),
            Inst::Jmp(_) => todo!(),
            Inst::Jz(_) => todo!(),
            Inst::Jn(_) => todo!(),
            Inst::Ret => todo!(),
            Inst::End => todo!(),
            Inst::Printc => todo!(),
            Inst::Printi => todo!(),
            Inst::Readc => todo!(),
            Inst::Readi => todo!(),
        }
        self.pc += 1;
        Ok(())
    }

    #[inline]
    fn underflow(&self, inst: &Inst) -> Error {
        match self.on_underflow {
            UnderflowError::Pop => Error::Underflow(inst.clone()),
            UnderflowError::SlideEmpty => Error::Lazy(LazyError::EmptyLit),
        }
    }
}

impl Heap {
    #[inline]
    pub fn new() -> Self {
        Heap::default()
    }
}

impl From<Vec<Inst>> for VM<'static> {
    fn from(prog: Vec<Inst>) -> Self {
        VM {
            src: b"",
            prog,
            stack: Vec::new(),
            heap: Heap::new(),
            pc: 0,
            call_stack: Vec::new(),
            on_underflow: UnderflowError::Pop,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn copy_empty() {
        let mut vm = VM::from(vec![Inst::Copy(NumberLit::Empty)]);
        vm.step().unwrap();
        assert_eq!(&[NumberRef::from(LazyError::EmptyLit)], vm.stack());
    }

    #[test]
    fn copy_negative() {
        let mut vm = VM::from(vec![Inst::Copy(NumberLit::from(-1))]);
        vm.step().unwrap();
        assert_eq!(&[NumberRef::from(LazyError::CopyNegative)], vm.stack());
    }

    #[test]
    fn copy_large() {
        let mut vm = VM::from(vec![Inst::Copy(NumberLit::from(1))]);
        vm.step().unwrap();
        assert_eq!(&[NumberRef::from(LazyError::CopyLarge)], vm.stack());
    }

    #[test]
    fn slide_empty() {
        let mut vm = VM::from(vec![
            Inst::Push(NumberLit::from(1)),
            Inst::Slide(NumberLit::Empty),
            Inst::Drop,
            Inst::Drop,
        ]);
        vm.step().unwrap();
        vm.step().unwrap();
        vm.step().unwrap();
        assert_eq!(Err(LazyError::EmptyLit.into()), vm.step());
    }

    #[test]
    fn slide_negative() {
        let mut vm = VM::from(vec![
            Inst::Push(NumberLit::from(1)),
            Inst::Slide(NumberLit::from(-2)),
            Inst::Drop,
            Inst::Drop,
        ]);
        vm.step().unwrap();
        vm.step().unwrap();
        vm.step().unwrap();
        assert_eq!(Err(Error::Underflow(Inst::Drop)), vm.step());
    }
}

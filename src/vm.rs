use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

use crate::error::{Error, NumberError};
use crate::inst::{Inst, NumberLit};
use crate::lex::Lexer;
use crate::number::{Number, NumberRef, Op};
use crate::parse::Parser;

#[derive(Debug, PartialEq, Eq)]
pub struct VM<'a, I: ?Sized, O: ?Sized> {
    prog: Vec<Inst>,
    stack: Vec<NumberRef>,
    heap: Heap,
    pc: usize,
    call_stack: Vec<usize>,
    stdin: &'a mut I,
    stdout: &'a mut O,
    on_underflow: UnderflowError,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnderflowError {
    Pop,
    SlideEmpty,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Heap {
    heap: HashMap<u32, NumberRef>,
    len: u32,
}

pub fn execute_file<P: AsRef<Path>>(path: P, mut stdin: &[u8]) -> (Result<(), Error>, Vec<u8>) {
    let mut f = File::open(&path).unwrap();
    let mut src = Vec::<u8>::new();
    f.read_to_end(&mut src).unwrap();

    let prog = Parser::new(Lexer::new(&src)).collect();
    let mut stdout = Vec::new();
    let mut vm = VM::new(prog, &mut stdin, &mut stdout);
    (vm.execute(), stdout)
}

impl<'a, I: Read + ?Sized, O: Write + ?Sized> VM<'a, I, O> {
    #[inline]
    pub fn new(prog: Vec<Inst>, stdin: &'a mut I, stdout: &'a mut O) -> Self {
        VM {
            prog,
            stack: Vec::new(),
            heap: Heap::new(),
            pc: 0,
            call_stack: Vec::new(),
            stdin,
            stdout,
            on_underflow: UnderflowError::Pop,
        }
    }

    #[inline]
    pub fn stack(&self) -> &[NumberRef] {
        &self.stack
    }

    pub fn execute(&mut self) -> Result<(), Error> {
        while self.pc < self.prog.len() {
            self.step()?;
        }
        Ok(())
    }

    pub fn step(&mut self) -> Result<(), Error> {
        let inst = &self.prog[self.pc];
        self.pc += 1;

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
                            NumberError::CopyNegative.into()
                        } else {
                            let i = i.to_usize().unwrap_or(usize::MAX);
                            match self.stack.get(self.stack.len().wrapping_sub(i)) {
                                Some(n) => n.clone(),
                                None => NumberError::CopyLarge.into(),
                            }
                        }
                    }
                    NumberLit::Empty => NumberError::EmptyLit.into(),
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
                let top = pop!()?;
                match n {
                    NumberLit::Number(n) => {
                        // Negative values slide nothing.
                        if n.cmp0() == Ordering::Greater {
                            let n = n.to_usize().unwrap_or(usize::MAX);
                            self.stack.truncate(self.stack.len().saturating_sub(n));
                        }
                    }
                    NumberLit::Empty => {
                        // If the stack later underflows, the empty argument
                        // from this slide is evaluated and the resulting error
                        // takes precedence over the underflow error.
                        self.on_underflow = UnderflowError::SlideEmpty;
                        self.stack.clear();
                    }
                }
                self.stack.push(top);
            }
            Inst::Add => arith!(Op::Add),
            Inst::Sub => arith!(Op::Sub),
            Inst::Mul => arith!(Op::Mul),
            Inst::Div => arith!(Op::Div),
            Inst::Mod => arith!(Op::Mod),
            Inst::Store => {
                let n = pop!()?;
                let addr = pop!()?;
                self.heap.store(addr, n)?;
            }
            Inst::Retrieve => {
                let addr = pop!()?;
                let n = self.heap.retrieve(addr)?;
                self.stack.push(n);
            }
            Inst::Label(_) => todo!(),
            Inst::Call(_) => todo!(),
            Inst::Jmp(_) => todo!(),
            Inst::Jz(_) => todo!(),
            Inst::Jn(_) => todo!(),
            Inst::Ret => {
                self.pc = self.call_stack.pop().ok_or(Error::RetUnderflow)?;
            }
            Inst::End => self.pc = self.prog.len(),
            Inst::Printc => {
                let n = Number::eval(pop!()?)?;
                let ch = n
                    .to_u32()
                    .and_then(char::from_u32)
                    .ok_or(Error::PrintcInvalid(n))?;
                write!(self.stdout, "{ch}").unwrap();
                self.stdout.flush().unwrap();
            }
            Inst::Printi => {
                let n = Number::eval(pop!()?)?;
                writeln!(self.stdout, "{n}").unwrap();
            }
            Inst::Readc => todo!(),
            Inst::Readi => todo!(),
            Inst::ParseError(err) => return Err(Error::Parse(err.clone())),
        }
        Ok(())
    }

    #[inline]
    fn underflow(&self, inst: &Inst) -> Error {
        match self.on_underflow {
            UnderflowError::Pop => match inst.to_printable() {
                Ok(inst) => Error::Underflow(inst),
                Err(err) => err,
            },
            UnderflowError::SlideEmpty => NumberError::EmptyLit.into(),
        }
    }
}

impl Heap {
    /// The approximate maximum address at which a value can be stored before
    /// wspace exits with a Haskell stack space overflow error.
    pub const MAX_ADDRESS: u32 = 3311732932;

    #[inline]
    pub fn new() -> Self {
        Heap::default()
    }

    pub fn store(&mut self, addr: NumberRef, n: NumberRef) -> Result<(), Error> {
        let addr = Number::eval(addr)?;
        if addr.cmp0() == Ordering::Less {
            return Err(NumberError::StoreNegative.into());
        }
        if let Some(addr) = addr.to_u32() {
            if addr <= Self::MAX_ADDRESS {
                self.heap.insert(addr, n);
                self.len = self.len.max(addr + 1);
                return Ok(());
            }
        }
        Err(Error::StoreOverflow)
    }

    pub fn retrieve(&mut self, addr: NumberRef) -> Result<NumberRef, Error> {
        let addr = Number::eval(addr)?;
        if addr.cmp0() == Ordering::Less {
            return Ok(NumberError::RetrieveNegative.into());
        }
        if let Some(addr) = addr.to_u32() {
            if addr < self.len {
                let n = self
                    .heap
                    .entry(addr)
                    .or_insert(Number::zero().into())
                    .clone();
                return Ok(n);
            }
        }
        Ok(NumberError::RetrieveLarge.into())
    }
}

#[cfg(test)]
mod tests {
    use crate::inst::PrintableInst;

    use super::*;

    #[test]
    fn copy_empty() {
        let prog = vec![Inst::Copy(NumberLit::Empty)];
        let mut stdin = &b""[..];
        let mut stdout = Vec::<u8>::new();
        let mut vm = VM::new(prog, &mut stdin, &mut stdout);
        vm.step().unwrap();
        assert_eq!(&[NumberRef::from(NumberError::EmptyLit)], vm.stack());
        assert_eq!(Vec::<u8>::new(), stdout);
    }

    #[test]
    fn copy_negative() {
        let prog = vec![Inst::Copy(NumberLit::new(-1))];
        let mut stdin = &b""[..];
        let mut stdout = Vec::<u8>::new();
        let mut vm = VM::new(prog, &mut stdin, &mut stdout);
        vm.step().unwrap();
        assert_eq!(&[NumberRef::from(NumberError::CopyNegative)], vm.stack());
        assert_eq!(Vec::<u8>::new(), stdout);
    }

    #[test]
    fn copy_large() {
        let prog = vec![Inst::Copy(NumberLit::new(1))];
        let mut stdin = &b""[..];
        let mut stdout = Vec::<u8>::new();
        let mut vm = VM::new(prog, &mut stdin, &mut stdout);
        vm.step().unwrap();
        assert_eq!(&[NumberRef::from(NumberError::CopyLarge)], vm.stack());
        assert_eq!(Vec::<u8>::new(), stdout);
    }

    #[test]
    fn slide_empty() {
        let prog = vec![
            Inst::Push(NumberLit::new(1)),
            Inst::Slide(NumberLit::Empty),
            Inst::Drop,
            Inst::Drop,
        ];
        let mut stdin = &b""[..];
        let mut stdout = Vec::<u8>::new();
        let mut vm = VM::new(prog, &mut stdin, &mut stdout);
        vm.step().unwrap();
        vm.step().unwrap();
        vm.step().unwrap();
        assert_eq!(Err(NumberError::EmptyLit.into()), vm.step());
        assert_eq!(Vec::<u8>::new(), stdout);
    }

    #[test]
    fn slide_negative() {
        let prog = vec![
            Inst::Push(NumberLit::new(1)),
            Inst::Slide(NumberLit::new(-2)),
            Inst::Drop,
            Inst::Drop,
        ];
        let mut stdin = &b""[..];
        let mut stdout = Vec::<u8>::new();
        let mut vm = VM::new(prog, &mut stdin, &mut stdout);
        vm.step().unwrap();
        vm.step().unwrap();
        vm.step().unwrap();
        assert_eq!(Err(Error::Underflow(PrintableInst::Drop)), vm.step());
        assert_eq!(Vec::<u8>::new(), stdout);
    }
}

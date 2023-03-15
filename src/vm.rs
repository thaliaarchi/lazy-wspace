use std::cmp::Ordering;
use std::collections::HashMap;
use std::io::{BufRead, Write};

use crate::error::{Error, NumberError, ParseError};
use crate::inst::{Inst, LabelLit, NumberLit};
use crate::io::CharReader;
use crate::number::{Number, NumberRef, Op};

#[derive(Debug, PartialEq, Eq)]
pub struct VM<'a, I, O: ?Sized> {
    prog: Vec<Inst>,
    stack: Vec<NumberRef>,
    heap: Heap,
    pc: usize,
    call_stack: Vec<usize>,
    labels: HashMap<LabelLit, usize>,
    stdin: CharReader<I>,
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

impl<'a, I: BufRead, O: Write + ?Sized> VM<'a, I, O> {
    #[inline]
    pub fn new(prog: Vec<Inst>, stdin: I, stdout: &'a mut O) -> Self {
        let mut labels = HashMap::new();
        for (pc, inst) in prog.iter().enumerate() {
            if let Inst::Label(l) = inst {
                labels.entry(l.clone()).or_insert(pc);
            }
        }
        VM {
            prog,
            stack: Vec::new(),
            heap: Heap::new(),
            pc: 0,
            call_stack: Vec::new(),
            labels,
            stdin: CharReader::new(stdin),
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
        macro_rules! arith(($op:expr) => {{
            let y = pop!()?;
            let x = pop!()?;
            self.stack.push(Number::Op($op, x, y).into());
        }});
        macro_rules! jump(($l:expr) => {{
            self.pc = *self
                .labels
                .get($l)
                .ok_or_else(|| ParseError::UndefinedLabel($l.clone()))?;
        }});

        match inst {
            Inst::Push(n) => {
                self.stack.push(n.into());
            }
            Inst::Dup => {
                let i = self.stack.len().wrapping_sub(1);
                let top = self.stack.get(i).ok_or_else(|| self.underflow(inst))?;
                self.stack.push(top.clone());
            }
            Inst::Copy(n) => {
                let x = match n {
                    NumberLit::Number(n) => {
                        if n.cmp0() == Ordering::Less {
                            NumberError::CopyNegative.into()
                        } else {
                            let n = n.to_usize().unwrap_or(usize::MAX);
                            let i = self.stack.len().wrapping_sub(n.wrapping_add(1));
                            match self.stack.get(i) {
                                Some(x) => x.clone(),
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
                let n = self.heap.retrieve(addr);
                self.stack.push(n);
            }
            Inst::Label(_) => {}
            Inst::Call(l) => {
                self.call_stack.push(self.pc);
                jump!(l);
            }
            Inst::Jmp(l) => jump!(l),
            Inst::Jz(l) => {
                let n = Number::eval(pop!()?)?;
                if n.cmp0() == Ordering::Equal {
                    jump!(l);
                }
            }
            Inst::Jn(l) => {
                let n = Number::eval(pop!()?)?;
                if n.cmp0() == Ordering::Less {
                    jump!(l);
                }
            }
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
                write!(self.stdout, "{n}").unwrap();
                self.stdout.flush().unwrap();
            }
            Inst::Readc => {
                let addr = pop!()?;
                let ch = self.stdin.read_char()?;
                self.heap.store(addr, Number::from(ch as u32).into())?;
            }
            Inst::Readi => {
                let addr = pop!()?;
                let line = self.stdin.read_line()?;
                let n = Number::parse(line)?;
                self.heap.store(addr, n.into())?;
            }
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

    pub fn retrieve(&mut self, addr: NumberRef) -> NumberRef {
        let addr = match Number::eval(addr) {
            Ok(n) => n,
            Err(err) => return err.into(),
        };
        if addr.cmp0() == Ordering::Less {
            return NumberError::RetrieveNegative.into();
        }
        if let Some(addr) = addr.to_u32() {
            if addr < self.len {
                let n = self
                    .heap
                    .entry(addr)
                    .or_insert(Number::zero().into())
                    .clone();
                return n;
            }
        }
        NumberError::RetrieveLarge.into()
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

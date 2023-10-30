use std::collections::HashMap;
use std::fmt::Display;
use std::io::{self, BufWriter, Write};

use utf8_chars::BufReadCharsExt;
use wspace_syntax::ws::ast::{Inst, LabelLit};

use crate::{
    error::{EagerError, Error, ParseError},
    vm::{Heap, Op, Stack, Value},
};

#[derive(Debug)]
pub struct Vm<'a, I: BufReadCharsExt, O: Write + ?Sized> {
    prog: Vec<Inst>,
    stack: Stack,
    heap: Heap,
    pc: usize,
    call_stack: Vec<usize>,
    labels: HashMap<LabelLit, usize>,
    stdin: I,
    stdout: BufWriter<&'a mut O>,
    exited: bool,
}

impl<'a, I: BufReadCharsExt, O: Write + ?Sized> Vm<'a, I, O> {
    #[inline]
    pub fn new(prog: Vec<Inst>, stdin: I, stdout: &'a mut O) -> Self {
        let mut labels = HashMap::new();
        for (pc, inst) in prog.iter().enumerate() {
            if let Inst::Label(l) = inst {
                labels.entry(l.clone()).or_insert(pc);
            }
        }
        Vm {
            prog,
            stack: Stack::new(),
            heap: Heap::new(),
            pc: 0,
            call_stack: Vec::new(),
            labels,
            stdin,
            stdout: BufWriter::with_capacity(8192, stdout),
            exited: false,
        }
    }

    #[inline]
    pub fn stack(&self) -> &Stack {
        &self.stack
    }

    #[inline]
    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    pub fn execute(&mut self) -> Result<(), Error> {
        while self.pc < self.prog.len() {
            self.step()?;
        }
        if !self.exited {
            return Err(ParseError::ImplicitEnd.into());
        }
        Ok(())
    }

    pub fn step(&mut self) -> Result<(), Error> {
        let inst = &self.prog[self.pc];
        self.pc += 1;

        macro_rules! arith(($op:expr) => ({
            let y = self.stack.pop(inst)?;
            let x = self.stack.pop(inst)?;
            self.stack.push(Value::Op($op, x, y).into());
        }));
        macro_rules! jump(($l:expr) => ({
            self.pc = *self
                .labels
                .get($l)
                .ok_or_else(|| ParseError::UndefinedLabel($l.clone()))?;
        }));

        match inst {
            Inst::Push(n) => {
                self.stack.push(n.into());
            }
            Inst::Dup => {
                let top = self.stack.pop(inst)?;
                self.stack.push(top.clone());
                self.stack.push(top);
            }
            Inst::Copy(n) => {
                let x = self.stack.copy(n);
                self.stack.push(x);
            }
            Inst::Swap => {
                let n = self.stack.pop(inst)?;
                let m = self.stack.pop(inst)?;
                self.stack.push(n);
                self.stack.push(m);
            }
            Inst::Drop => {
                self.stack.pop(inst)?;
            }
            Inst::Slide(n) => {
                let top = self.stack.pop(inst)?;
                self.stack.drop_lazy(n)?;
                self.stack.push(top);
            }
            Inst::Add => arith!(Op::Add),
            Inst::Sub => arith!(Op::Sub),
            Inst::Mul => arith!(Op::Mul),
            Inst::Div => arith!(Op::Div),
            Inst::Mod => arith!(Op::Mod),
            Inst::Store => {
                let n = self.stack.pop(inst)?;
                let addr = self.stack.pop(inst)?;
                self.heap.store(addr, n)?;
            }
            Inst::Retrieve => {
                let addr = self.stack.pop(inst)?;
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
                let n = self.stack.pop(inst)?.eval()?;
                if n.is_zero() {
                    jump!(l);
                }
            }
            Inst::Jn(l) => {
                let n = self.stack.pop(inst)?.eval()?;
                if n.is_negative() {
                    jump!(l);
                }
            }
            Inst::Ret => {
                self.pc = self.call_stack.pop().ok_or(EagerError::RetUnderflow)?;
            }
            Inst::End => {
                self.pc = self.prog.len();
                self.exited = true;
            }
            Inst::Printc => {
                let n = self.stack.pop(inst)?.eval()?;
                let ch = n
                    .to_u32()
                    .and_then(char::from_u32)
                    .ok_or(EagerError::PrintcInvalid(n))?;
                self.print(ch)?;
            }
            Inst::Printi => {
                let n = self.stack.pop(inst)?.eval()?;
                self.print(n)?;
            }
            Inst::Readc => {
                let addr = self.stack.pop(inst)?;
                let ch = match self.stdin.read_char() {
                    Ok(Some(ch)) => Ok(ch),
                    Ok(None) => Err(EagerError::ReadcEof),
                    Err(err) => match err.kind() {
                        io::ErrorKind::InvalidData | io::ErrorKind::UnexpectedEof => {
                            Err(EagerError::ReadcInvalidUtf8)
                        }
                        _ => panic!("unhandled IO error: {err}"),
                    },
                }?;
                self.heap.store(addr, Value::integer(ch as u32).into())?;
            }
            Inst::Readi => {
                let addr = self.stack.pop(inst)?;
                let mut line = String::new();
                match self.stdin.read_line(&mut line) {
                    Ok(0) => Err(EagerError::ReadiEof),
                    Ok(_) => Ok(()),
                    Err(err) => match err.kind() {
                        io::ErrorKind::InvalidData | io::ErrorKind::UnexpectedEof => {
                            Err(EagerError::ReadiInvalidUtf8)
                        }
                        _ => panic!("unhandled IO error: {err}"),
                    },
                }?;
                let n: Value = line.parse()?;
                self.heap.store(addr, n.into())?;
            }
            Inst::ParseError(err) => return Err((*err).into()),
        }
        Ok(())
    }

    fn print<T: Display>(&mut self, v: T) -> Result<(), Error> {
        write!(self.stdout, "{v}").map_err(|err| match err.kind() {
            io::ErrorKind::BrokenPipe => EagerError::BrokenPipe,
            // The stdout buffer is always empty before printing, so permission
            // denied can only be encountered when printi fills the buffer in a
            // single write.
            io::ErrorKind::PermissionDenied => EagerError::PrintPermissionDenied,
            _ => panic!("unhandled IO error: {err}"),
        })?;
        self.stdout.flush().map_err(|err| match err.kind() {
            io::ErrorKind::BrokenPipe => EagerError::BrokenPipe,
            io::ErrorKind::PermissionDenied => EagerError::FlushPermissionDenied,
            _ => panic!("unhandled IO error: {err}"),
        })?;
        Ok(())
    }
}

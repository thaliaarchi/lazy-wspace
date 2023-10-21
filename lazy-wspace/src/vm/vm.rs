use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Display;
use std::io::{self, Write};

use utf8_chars::BufReadCharsExt;
use wspace_syntax::ws::ast::{Inst, LabelLit};

use crate::error::{EagerError, Error, ParseError, UnderflowError, ValueError};
use crate::vm::{Op, Value, ValueRef};

#[derive(Debug, PartialEq, Eq)]
pub struct VM<'a, I, O: ?Sized> {
    prog: Vec<Inst>,
    stack: Vec<ValueRef>,
    heap: Heap,
    pc: usize,
    call_stack: Vec<usize>,
    labels: HashMap<LabelLit, usize>,
    stdin: I,
    stdout: &'a mut O,
    on_underflow: UnderflowError,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Heap {
    heap: HashMap<u32, ValueRef>,
    len: u32,
}

impl<'a, I: BufReadCharsExt, O: Write + ?Sized> VM<'a, I, O> {
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
            stdin,
            stdout,
            on_underflow: UnderflowError::Normal,
        }
    }

    #[inline]
    pub fn stack(&self) -> &[ValueRef] {
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
            self.stack.pop().ok_or_else(|| self.on_underflow.to_error(inst))
        });
        macro_rules! arith(($op:expr) => {{
            let y = pop!()?;
            let x = pop!()?;
            self.stack.push(Value::Op($op, x, y).into());
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
                let top = (self.stack.get(i)).ok_or_else(|| self.on_underflow.to_error(inst))?;
                self.stack.push(top.clone());
            }
            Inst::Copy(n) => {
                let x = match n.value() {
                    Some(n) => {
                        if n.cmp0() == Ordering::Less {
                            ValueError::CopyNegative.into()
                        } else {
                            // TODO: Make wrapping.
                            let n = n.to_usize().unwrap_or(usize::MAX);
                            let i = self.stack.len().wrapping_sub(n.wrapping_add(1));
                            match self.stack.get(i) {
                                Some(x) => x.clone(),
                                None => ValueError::CopyLarge.into(),
                            }
                        }
                    }
                    None => ValueError::EmptyLit.into(),
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
                match n.value() {
                    Some(n) => {
                        // Negative values slide nothing.
                        if n.cmp0() == Ordering::Greater {
                            // TODO: Make wrapping.
                            let n = n.to_usize().unwrap_or(usize::MAX);
                            self.stack.truncate(self.stack.len().saturating_sub(n));
                        }
                    }
                    None => {
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
                let n = pop!()?.eval()?;
                if n.cmp0() == Ordering::Equal {
                    jump!(l);
                }
            }
            Inst::Jn(l) => {
                let n = pop!()?.eval()?;
                if n.cmp0() == Ordering::Less {
                    jump!(l);
                }
            }
            Inst::Ret => {
                self.pc = self.call_stack.pop().ok_or(EagerError::RetUnderflow)?;
            }
            Inst::End => self.pc = self.prog.len(),
            Inst::Printc => {
                let n = pop!()?.eval()?;
                let ch = n
                    .to_u32()
                    .and_then(char::from_u32)
                    .ok_or(EagerError::PrintcInvalid(n))?;
                self.print(ch)?;
            }
            Inst::Printi => {
                let n = pop!()?.eval()?;
                self.print(n)?;
            }
            Inst::Readc => {
                let addr = pop!()?;
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
                let addr = pop!()?;
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
        // TODO: Use buffer size of 8192.
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

impl Heap {
    #[inline]
    pub fn new() -> Self {
        Heap::default()
    }

    /// Store inserts value `n` into the heap at address `addr`.
    ///
    /// In the reference interpreter, `store` runs in *O(addr)* time and
    /// allocates *O(addr)* cons cells, by constructing a new list prefix of
    /// length addr and reusing the tail of the original list. This leads to
    /// space leaks, whenever references to the prefix of the original list are
    /// retained in unevaluated `retrieve` expressions, and runtime stack
    /// overflows, for large addresses. Experimentally, it overflows when
    /// storing at addresses greater than or equal to 3311732933 (approx.
    /// 2^31.6), starting with an empty heap, but this varies by heap size.
    ///
    /// This implementation mimics the upper address bound, by only allowing
    /// addresses up to 2^32, but does not have the linear performance issues.
    pub fn store(&mut self, addr: ValueRef, n: ValueRef) -> Result<(), Error> {
        let addr = addr.eval()?;
        if addr.cmp0() == Ordering::Less {
            return Err(EagerError::StoreNegative.into());
        }
        if let Some(addr) = addr.to_u32() {
            self.heap.insert(addr, n);
            self.len = self.len.max(addr + 1);
            return Ok(());
        }
        Err(EagerError::StoreOverflow.into())
    }

    pub fn retrieve(&mut self, addr: ValueRef) -> ValueRef {
        let addr = match addr.eval() {
            Ok(n) => n,
            Err(err) => return err.into(),
        };
        if addr.cmp0() == Ordering::Less {
            return ValueError::RetrieveNegative.into();
        }
        if let Some(addr) = addr.to_u32() {
            if addr < self.len {
                return self
                    .heap
                    .entry(addr)
                    .or_insert(Value::zero().into())
                    .clone();
            }
        }
        ValueError::RetrieveLarge.into()
    }
}

#[cfg(test)]
mod tests {
    use wspace_syntax::ws::ast::IntegerLit;

    use super::*;

    #[test]
    fn copy_empty() {
        let prog = vec![Inst::Copy(IntegerLit::empty())];
        let mut stdin = &b""[..];
        let mut stdout = Vec::<u8>::new();
        let mut vm = VM::new(prog, &mut stdin, &mut stdout);
        vm.step().unwrap();
        assert_eq!(&[ValueRef::from(ValueError::EmptyLit)], vm.stack());
        assert_eq!(Vec::<u8>::new(), stdout);
    }

    #[test]
    fn copy_negative() {
        let prog = vec![Inst::Copy(IntegerLit::new(-1))];
        let mut stdin = &b""[..];
        let mut stdout = Vec::<u8>::new();
        let mut vm = VM::new(prog, &mut stdin, &mut stdout);
        vm.step().unwrap();
        assert_eq!(&[ValueRef::from(ValueError::CopyNegative)], vm.stack());
        assert_eq!(Vec::<u8>::new(), stdout);
    }

    #[test]
    fn copy_large() {
        let prog = vec![Inst::Copy(IntegerLit::new(1))];
        let mut stdin = &b""[..];
        let mut stdout = Vec::<u8>::new();
        let mut vm = VM::new(prog, &mut stdin, &mut stdout);
        vm.step().unwrap();
        assert_eq!(&[ValueRef::from(ValueError::CopyLarge)], vm.stack());
        assert_eq!(Vec::<u8>::new(), stdout);
    }

    #[test]
    fn slide_empty() {
        let prog = vec![
            Inst::Push(IntegerLit::new(1)),
            Inst::Slide(IntegerLit::empty()),
            Inst::Drop,
            Inst::Drop,
        ];
        let mut stdin = &b""[..];
        let mut stdout = Vec::<u8>::new();
        let mut vm = VM::new(prog, &mut stdin, &mut stdout);
        vm.step().unwrap();
        vm.step().unwrap();
        vm.step().unwrap();
        assert_eq!(Err(ValueError::EmptyLit.into()), vm.step());
        assert_eq!(Vec::<u8>::new(), stdout);
    }

    #[test]
    fn slide_negative() {
        let prog = vec![
            Inst::Push(IntegerLit::new(1)),
            Inst::Slide(IntegerLit::new(-2)),
            Inst::Drop,
            Inst::Drop,
        ];
        let mut stdin = &b""[..];
        let mut stdout = Vec::<u8>::new();
        let mut vm = VM::new(prog, &mut stdin, &mut stdout);
        vm.step().unwrap();
        vm.step().unwrap();
        vm.step().unwrap();
        assert_eq!(Err(EagerError::Underflow(Inst::Drop).into()), vm.step());
        assert_eq!(Vec::<u8>::new(), stdout);
    }
}

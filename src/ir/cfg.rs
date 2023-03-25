use std::fmt::{self, Display, Formatter};

use crate::ast::{self, ExitInst, Inst, LabelLit};
use crate::error::{Error, UnderflowError};
use crate::ir::{AbstractHeap, AbstractStack, ExpRef, LazySize};
use crate::number::Op;

/// Control-flow graph of IR basic blocks.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cfg {
    bbs: Vec<Option<BasicBlock>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BasicBlock {
    id: usize,
    label: Option<LabelLit>,
    stmts: Vec<Stmt>,
    exit: ExitStmt,
    stack: AbstractStack,
    heap: AbstractHeap,
}

// TODO move
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    GuardStack(usize),
    Eval(ExpRef),
    Store(ExpRef, ExpRef),
    Print(IoKind, ExpRef),
    Read(IoKind, ExpRef),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExitStmt {
    Call(usize, usize),
    Jmp(usize),
    Br(Cond, ExpRef, usize, usize),
    Ret,
    End,
    Error(Error),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Cond {
    Zero,
    Neg,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IoKind {
    Char,
    Int,
}

impl Cfg {
    pub fn new(ast_cfg: &ast::Cfg<'_>) -> Self {
        let mut bbs = Vec::with_capacity(ast_cfg.bbs().len());
        for bb in ast_cfg.bbs() {
            bbs.push(bb.as_ref().map(|bb| BasicBlock::new(bb)));
        }
        Cfg { bbs }
    }
}

impl BasicBlock {
    pub fn new(ast_bb: &ast::BasicBlock<'_>) -> Self {
        let mut bb = BasicBlock {
            id: ast_bb.id(),
            label: ast_bb.label().cloned(),
            stmts: Vec::new(),
            exit: ExitStmt::Jmp(usize::MAX), // Placeholder
            stack: AbstractStack::new(),
            heap: AbstractHeap::new(),
        };

        for inst in ast_bb.insts() {
            if let Err(err) = bb.push_inst(inst) {
                bb.exit = ExitStmt::Error(err);
                return bb;
            }
        }

        bb.exit = match ast_bb.exit() {
            ExitInst::Call(l1, l2) => ExitStmt::Call(*l1, *l2),
            ExitInst::Jmp(l) => ExitStmt::Jmp(*l),
            ExitInst::Jz(l1, l2, inst) => match bb.stack.pop() {
                Ok(top) => ExitStmt::Br(Cond::Zero, top, *l1, *l2),
                Err(err) => ExitStmt::Error(err.to_error(inst)),
            },
            ExitInst::Jn(l1, l2, inst) => match bb.stack.pop() {
                Ok(top) => ExitStmt::Br(Cond::Neg, top, *l1, *l2),
                Err(err) => ExitStmt::Error(err.to_error(inst)),
            },
            ExitInst::Ret => ExitStmt::Ret,
            ExitInst::End => ExitStmt::End,
            ExitInst::Error(err) => ExitStmt::Error(err.clone().into()),
        };
        bb
    }

    fn push_inst(&mut self, inst: &Inst) -> Result<(), Error> {
        let pre = self.stack.drop_count().max(self.stack.values_under().len());
        match self.push_inst_underflow(inst) {
            Ok(Ok(())) => {
                let post = self.stack.drop_count().max(self.stack.values_under().len());
                if post > pre {
                    self.stmts.push(Stmt::GuardStack(post));
                }
                Ok(())
            }
            Ok(Err(err)) => Err(err),
            Err(err) => Err(err.to_error(inst)),
        }
    }

    #[inline]
    fn push_inst_underflow(&mut self, inst: &Inst) -> Result<Result<(), Error>, UnderflowError> {
        match inst {
            Inst::Push(n) => self.stack.push(n.into()),
            Inst::Dup => self.stack.dup()?,
            Inst::Copy(n) => self.stack.copy(n.into()),
            Inst::Swap => self.stack.swap()?,
            Inst::Drop => self.stack.drop_eager(1)?,
            Inst::Slide(n) => self.stack.slide(n.into())?,
            Inst::Add => self.stack.apply_op(Op::Add)?,
            Inst::Sub => self.stack.apply_op(Op::Sub)?,
            Inst::Mul => self.stack.apply_op(Op::Mul)?,
            Inst::Div => self.stack.apply_op(Op::Div)?,
            Inst::Mod => self.stack.apply_op(Op::Mod)?,
            Inst::Store => {
                let (addr, val) = self.stack.pop2()?;
                return Ok(self.heap.store(addr, val));
            }
            Inst::Retrieve => {
                let addr = self.stack.pop()?;
                self.stack.push(self.heap.retrieve(addr));
            }
            Inst::Printc => {
                let val = self.stack.pop()?;
                self.stmts.push(Stmt::Print(IoKind::Char, val));
            }
            Inst::Printi => {
                let val = self.stack.pop()?;
                self.stmts.push(Stmt::Print(IoKind::Int, val));
            }
            Inst::Readc => {
                let addr = self.stack.pop()?;
                self.stmts.push(Stmt::Read(IoKind::Char, addr));
            }
            Inst::Readi => {
                let addr = self.stack.pop()?;
                self.stmts.push(Stmt::Read(IoKind::Int, addr));
            }
            Inst::Label(_)
            | Inst::Call(_)
            | Inst::Jmp(_)
            | Inst::Jz(_)
            | Inst::Jn(_)
            | Inst::Ret
            | Inst::End
            | Inst::ParseError(_) => panic!("terminator in block body"),
        }
        Ok(Ok(()))
    }
}

impl Display for Cfg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for bb in &self.bbs {
            if let Some(bb) = bb {
                if !first {
                    writeln!(f)?;
                }
                first = false;
                writeln!(f, "{bb}:")?;
                for stmt in &bb.stmts {
                    writeln!(f, "    {stmt}")?;
                }
                if bb.stack.drop_count() != 0 {
                    writeln!(f, "    drop_eager {}", bb.stack.drop_count())?;
                }
                if bb.stack.slide_count() != LazySize::Finite(0) {
                    writeln!(f, "    drop_lazy {:?}", bb.stack.slide_count())?;
                }
                for val in bb.stack.values_pushed() {
                    writeln!(f, "    push {val:?}")?;
                }
                macro_rules! bb(($l:ident) => {
                    self.bbs[*$l].as_ref().expect("undefined label")
                });
                write!(f, "    ")?;
                match &bb.exit {
                    ExitStmt::Call(l1, l2) => write!(f, "call {} {}", bb!(l1), bb!(l2)),
                    ExitStmt::Jmp(l) => write!(f, "jmp {}", bb!(l)),
                    ExitStmt::Br(cond, val, l1, l2) => {
                        write!(f, "br {cond:?} {val:?} {} {}", bb!(l1), bb!(l2))
                    }
                    ExitStmt::Ret => write!(f, "ret"),
                    ExitStmt::End => write!(f, "end"),
                    ExitStmt::Error(err) => write!(f, "error {err:?}"),
                }?;
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.label {
            Some(l) => write!(f, "{l}"),
            None => write!(f, "bb{}", self.id),
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::GuardStack(n) => write!(f, "guard_stack {n}"),
            Stmt::Eval(v) => write!(f, "eval {v:?}"),
            Stmt::Store(addr, val) => write!(f, "store {addr:?} {val:?}"),
            Stmt::Print(kind, val) => write!(f, "print {kind:?} {val:?}"),
            Stmt::Read(kind, addr) => write!(f, "read {kind:?} {addr:?}"),
        }
    }
}

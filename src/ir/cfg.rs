use std::collections::VecDeque;
use std::fmt::{self, Display, Formatter};
use std::ops::{Index, IndexMut};

use bitvec::prelude::*;

use crate::ast::{self, ExitInst, Inst, LabelLit};
use crate::error::{Error, UnderflowError};
use crate::ir::{AbstractHeap, AbstractStack, Exp, ExpPool, ExpRef, LazySize};
use crate::number::Op;

/// Control-flow graph of IR basic blocks.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cfg {
    bbs: Vec<BBlock>,
}

/// A basic block, i.e., a sequence of non-branching instructions, followed by a
/// branch.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BBlock {
    id: BBlockId,
    label: Option<LabelLit>,
    stmts: Vec<Stmt>,
    exit: ExitStmt,
    exps: ExpPool,
    stack: AbstractStack,
    heap: AbstractHeap,
}

#[derive(Clone, Debug)]
pub struct BBlockBuilder {
    id: BBlockId,
    label: Option<LabelLit>,
    stmts: Vec<Stmt>,
    exit: Option<ExitStmt>,
    exps: ExpPool,
    stack: AbstractStack,
    heap: AbstractHeap,
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BBlockId(u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    GuardStack(usize),
    Eval(ExpRef),
    Store(ExpRef, ExpRef),
    // Retrieve(u32), // TODO add for proper scheduling
    Print(IoKind, ExpRef),
    Read(IoKind, ExpRef),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExitStmt {
    Call(BBlockId, BBlockId),
    Jmp(BBlockId),
    Br(Cond, ExpRef, BBlockId, BBlockId),
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
    pub fn from_ast(ast_cfg: &ast::Cfg<'_>) -> Self {
        let mut bbs = Vec::with_capacity(ast_cfg.bbs().len());
        for bb in ast_cfg.bbs() {
            bbs.push(BBlock::from_ast(bb));
        }
        Cfg { bbs }
    }

    pub fn reachable(&self) -> BitBox {
        let mut visited = bitbox![0; self.bbs.len()];
        let mut queue = VecDeque::new();
        queue.push_back(self.bbs[0].id);
        while let Some(id) = queue.pop_front() {
            visited.set(id.as_usize(), true);
            let bb = &self[id];
            // TODO: Call and ret are not paired
            match bb.exit {
                ExitStmt::Jmp(l) => {
                    if !visited[l.as_usize()] {
                        queue.push_back(l)
                    }
                }
                ExitStmt::Call(l1, l2) | ExitStmt::Br(_, _, l1, l2) => {
                    if !visited[l1.as_usize()] {
                        queue.push_back(l1);
                    }
                    if !visited[l2.as_usize()] {
                        queue.push_back(l2);
                    }
                }
                _ => {}
            }
        }
        visited
    }
}

impl BBlock {
    pub fn from_ast(bb: &ast::BBlock<'_>) -> Self {
        let mut b = BBlockBuilder::new(BBlockId(bb.id() as u32), bb.label().cloned());
        for inst in bb.insts() {
            if let Err(err) = b.push_inst(inst) {
                b.exit = Some(ExitStmt::Error(err));
                break;
            }
        }
        if b.exit.is_none() {
            b.set_exit(bb.exit());
        }
        b.finish()
    }

    #[inline]
    pub fn id(&self) -> BBlockId {
        self.id
    }

    #[inline]
    pub fn label(&self) -> Option<&LabelLit> {
        self.label.as_ref()
    }

    #[inline]
    pub fn stmts(&self) -> &[Stmt] {
        &self.stmts
    }

    #[inline]
    pub fn exit(&self) -> &ExitStmt {
        &self.exit
    }

    #[inline]
    pub fn stack(&self) -> &AbstractStack {
        &self.stack
    }

    #[inline]
    pub fn heap(&self) -> &AbstractHeap {
        &self.heap
    }
}

impl BBlockBuilder {
    pub fn new(id: BBlockId, label: Option<LabelLit>) -> Self {
        BBlockBuilder {
            id,
            label,
            stmts: Vec::new(),
            exit: None,
            exps: ExpPool::new(),
            stack: AbstractStack::new(),
            heap: AbstractHeap::new(),
        }
    }

    pub fn push_inst(&mut self, inst: &Inst) -> Result<(), Error> {
        match inst {
            Inst::Push(n) => self.do_stack(inst, |s, exps| Ok(s.push(exps.insert(n.into()))))?,
            Inst::Dup => self.do_stack(inst, |s, exps| s.dup(exps))?,
            Inst::Copy(n) => self.do_stack(inst, |s, exps| Ok(s.copy(n.into(), exps)))?,
            Inst::Swap => self.do_stack(inst, |s, exps| s.swap(exps))?,
            Inst::Drop => self.do_stack(inst, |s, _| s.drop_eager(1))?,
            Inst::Slide(n) => self.do_stack(inst, |s, exps| s.slide(n.into(), exps))?,
            Inst::Add => self.do_stack(inst, |s, exps| s.apply_op(Op::Add, exps))?,
            Inst::Sub => self.do_stack(inst, |s, exps| s.apply_op(Op::Sub, exps))?,
            Inst::Mul => self.do_stack(inst, |s, exps| s.apply_op(Op::Mul, exps))?,
            Inst::Div => self.do_stack(inst, |s, exps| s.apply_op(Op::Div, exps))?,
            Inst::Mod => self.do_stack(inst, |s, exps| s.apply_op(Op::Mod, exps))?,
            Inst::Store => {
                let (addr, val) = self.do_stack(inst, |s, exps| s.pop2(exps))?;
                self.stmts.push(Stmt::Store(addr, val)); // TODO: cache
                self.heap.store(addr, val, &mut self.exps)?;
            }
            Inst::Retrieve => {
                let addr = self.do_stack(inst, |s, exps| s.pop(exps))?;
                self.stack.push(self.heap.retrieve(addr, &mut self.exps));
            }
            Inst::Printc => {
                let val = self.do_stack(inst, |s, exps| s.pop(exps))?;
                self.stmts.push(Stmt::Eval(val));
                self.stmts.push(Stmt::Print(IoKind::Char, val));
            }
            Inst::Printi => {
                let val = self.do_stack(inst, |s, exps| s.pop(exps))?;
                self.stmts.push(Stmt::Eval(val));
                self.stmts.push(Stmt::Print(IoKind::Int, val));
            }
            Inst::Readc => {
                let addr = self.do_stack(inst, |s, exps| s.pop(exps))?;
                self.stmts.push(Stmt::Eval(addr));
                self.stmts.push(Stmt::Read(IoKind::Char, addr));
            }
            Inst::Readi => {
                let addr = self.do_stack(inst, |s, exps| s.pop(exps))?;
                self.stmts.push(Stmt::Eval(addr));
                self.stmts.push(Stmt::Read(IoKind::Int, addr));
            }
            Inst::Label(_)
            | Inst::Call(_)
            | Inst::Jmp(_)
            | Inst::Jz(_)
            | Inst::Jn(_)
            | Inst::Ret
            | Inst::End
            | Inst::ParseError(_) => panic!("terminator in basic block body"),
        }
        Ok(())
    }

    pub fn set_exit(&mut self, exit: &ExitInst) {
        assert!(self.exit.is_none());
        let res = (|| {
            let exit: ExitStmt = match exit {
                ExitInst::Call(l1, l2) => ExitStmt::Call(BBlockId::new(*l1), BBlockId::new(*l2)),
                ExitInst::Jmp(l) => ExitStmt::Jmp(BBlockId::new(*l)),
                ExitInst::Jz(l1, l2, inst) => {
                    let top = self.do_stack(inst, |s, exps| s.pop(exps))?;
                    self.stmts.push(Stmt::Eval(top));
                    ExitStmt::Br(Cond::Zero, top, BBlockId::new(*l1), BBlockId::new(*l2))
                }
                ExitInst::Jn(l1, l2, inst) => {
                    let top = self.do_stack(inst, |s, exps| s.pop(exps))?;
                    self.stmts.push(Stmt::Eval(top));
                    ExitStmt::Br(Cond::Neg, top, BBlockId::new(*l1), BBlockId::new(*l2))
                }
                ExitInst::Ret => ExitStmt::Ret,
                ExitInst::End => ExitStmt::End,
                ExitInst::Error(err) => ExitStmt::Error(err.clone().into()),
            };
            Ok(exit)
        })();
        self.exit = Some(match res {
            Ok(exit) => exit,
            Err(err) => ExitStmt::Error(err),
        });
    }

    fn do_stack<T, F>(&mut self, inst: &Inst, f: F) -> Result<T, Error>
    where
        F: FnOnce(&mut AbstractStack, &mut ExpPool) -> Result<T, UnderflowError>,
    {
        let pre = self.stack.accessed();
        let res = f(&mut self.stack, &mut self.exps);
        let post = self.stack.accessed();
        if post > pre {
            self.stmts.push(Stmt::GuardStack(post));
        }
        res.map_err(|err| err.to_error(inst))
    }

    pub fn finish(mut self) -> BBlock {
        self.stack.simplify(&self.exps);
        BBlock {
            id: self.id,
            label: self.label,
            stmts: self.stmts,
            exit: self.exit.expect("missing exit"),
            exps: self.exps,
            stack: self.stack,
            heap: self.heap,
        }
    }
}

impl BBlockId {
    const fn new(id: usize) -> Self {
        BBlockId(id as u32)
    }

    pub const fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

impl Index<BBlockId> for Cfg {
    type Output = BBlock;

    fn index(&self, index: BBlockId) -> &BBlock {
        &self.bbs[index.0 as usize]
    }
}

impl IndexMut<BBlockId> for Cfg {
    fn index_mut(&mut self, index: BBlockId) -> &mut BBlock {
        &mut self.bbs[index.0 as usize]
    }
}

impl Display for Cfg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fn visit_exp(root: ExpRef, exps: &ExpPool, visited: &mut BitBox, new_visited: &mut BitBox) {
            if visited[root.as_usize()] {
                return;
            }
            visited.set(root.as_usize(), true);
            new_visited.set(root.as_usize(), true);
            match &exps[root] {
                Exp::Op(_, lhs, rhs) => {
                    visit_exp(*lhs, exps, visited, new_visited);
                    visit_exp(*rhs, exps, visited, new_visited);
                }
                Exp::HeapRef(addr) => {
                    visit_exp(*addr, exps, visited, new_visited);
                }
                _ => {}
            }
        }

        let reachable = self.reachable();

        let mut first = true;
        for bb in &self.bbs {
            if !first {
                writeln!(f)?;
            }
            first = false;
            if !reachable[bb.id.as_usize()] {
                writeln!(f, "# unreachable")?;
            }
            writeln!(f, "{bb}:")?;

            let mut visited = bitbox![0; bb.exps.len()];

            for stmt in &bb.stmts {
                let mut new_visited = bitbox![0; bb.exps.len()];
                match stmt {
                    Stmt::GuardStack(_) => {}
                    Stmt::Eval(val) | Stmt::Print(_, val) | Stmt::Read(_, val) => {
                        visit_exp(*val, &bb.exps, &mut visited, &mut new_visited);
                    }
                    Stmt::Store(addr, val) => {
                        visit_exp(*addr, &bb.exps, &mut visited, &mut new_visited);
                        visit_exp(*val, &bb.exps, &mut visited, &mut new_visited);
                    }
                }
                for i in new_visited.iter_ones() {
                    let i = ExpRef::new(i);
                    writeln!(f, "    {i} = {}", bb.exps[i])?;
                }
                writeln!(f, "    {stmt}")?;
            }

            if bb.stack.dropped() != 0 {
                writeln!(f, "    drop_eager {}", bb.stack.dropped())?;
            }
            if bb.stack.lazy_dropped() != LazySize::Finite(0) {
                writeln!(f, "    drop_lazy {:?}", bb.stack.lazy_dropped())?;
            }

            for &val in bb.stack.values() {
                let mut new_visited = bitbox![0; bb.exps.len()];
                visit_exp(val, &bb.exps, &mut visited, &mut new_visited);
                for i in new_visited.iter_ones() {
                    let i = ExpRef::new(i);
                    writeln!(f, "    {i} = {}", bb.exps[i])?;
                }
                writeln!(f, "    push {val}")?;
            }

            write!(f, "    ")?;
            match &bb.exit {
                ExitStmt::Call(l1, l2) => write!(f, "call {} {}", self[*l1], self[*l2]),
                ExitStmt::Jmp(l) => write!(f, "jmp {}", self[*l]),
                ExitStmt::Br(cond, val, l1, l2) => {
                    let mut new_visited = bitbox![0; bb.exps.len()];
                    visit_exp(*val, &bb.exps, &mut visited, &mut new_visited);
                    for i in new_visited.iter_ones() {
                        let i = ExpRef::new(i);
                        writeln!(f, "    {i} = {}", bb.exps[i])?;
                    }
                    write!(f, "br {cond:?} {val} {} {}", self[*l1], self[*l2])
                }
                ExitStmt::Ret => write!(f, "ret"),
                ExitStmt::End => write!(f, "end"),
                ExitStmt::Error(err) => write!(f, "error {err:?}"),
            }?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Display for BBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.label {
            Some(l) => Display::fmt(l, f),
            None => Display::fmt(&self.id, f),
        }
    }
}

impl Display for BBlockId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::GuardStack(n) => write!(f, "guard_stack {n}"),
            Stmt::Eval(val) => write!(f, "eval {val}"),
            Stmt::Store(addr, val) => write!(f, "store {addr} {val}"),
            Stmt::Print(kind, val) => write!(f, "print {kind:?} {val}"),
            Stmt::Read(kind, addr) => write!(f, "read {kind:?} {addr}"),
        }
    }
}

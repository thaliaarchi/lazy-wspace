use std::collections::{HashMap, VecDeque};
use std::fmt::{self, Display, Formatter};
use std::ops::{Index, IndexMut};

use bitvec::prelude::*;

use crate::ast::{Inst, LabelLit};
use crate::error::{Error, ParseError, UnderflowError};
use crate::ir::{AbstractHeap, AbstractStack, Graph, LazySize, NodeRef, NodeTable};
use crate::number::Op;

/// Control-flow graph of IR basic blocks.
#[derive(Clone, Debug)]
pub struct Cfg<'g> {
    bbs: Vec<BBlock<'g>>,
    graph: &'g Graph,
    globals: NodeTable<'g>,
}

/// A basic block, i.e., a sequence of non-branching instructions, followed by a
/// branch.
#[derive(Clone, Debug)]
pub struct BBlock<'g> {
    id: BBlockId,
    label: Option<LabelLit>,
    table: NodeTable<'g>,
    stmts: Vec<Stmt>,
    exit: ExitStmt,
    stack: AbstractStack,
    heap: AbstractHeap,
}

#[derive(Clone, Debug)]
struct BBlockBuilder<'g> {
    id: BBlockId,
    label: Option<LabelLit>,
    table: NodeTable<'g>,
    stmts: Vec<Stmt>,
    exit: Option<ExitStmt>,
    exit_pc: Option<usize>,
    stack: AbstractStack,
    heap: AbstractHeap,
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BBlockId(u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    GuardStack(usize),
    Eval(NodeRef),
    Store(NodeRef, NodeRef),
    // Retrieve(u32), // TODO add for proper scheduling
    Print(IoKind, NodeRef),
    Read(IoKind, NodeRef),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExitStmt {
    Call(BBlockId, BBlockId),
    Jmp(BBlockId),
    Br(Cond, NodeRef, BBlockId, BBlockId),
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

impl<'g> Cfg<'g> {
    pub fn new(prog: &[Inst], graph: &'g Graph) -> Self {
        let mut bbs = Vec::new();
        let mut globals = NodeTable::new(graph);
        let mut labels = HashMap::new();
        let mut first_parse_error = None;

        // Build basic block bodies and collect labels:
        let mut pc = 0;
        while pc < prog.len() {
            let id = BBlockId::new(bbs.len());
            let mut bb = BBlockBuilder::new(id, graph);
            if let Inst::Label(l) = &prog[pc] {
                if !first_parse_error.is_some_and(|err_id| id >= err_id) {
                    labels.insert(l.clone(), id);
                }
                bb.label = Some(l.clone());
                pc += 1;
            }
            while pc < prog.len() {
                let curr_pc = pc;
                let inst = &prog[curr_pc];
                pc += 1;
                if inst.is_control_flow() {
                    bb.exit_pc = Some(curr_pc);
                    match inst {
                        Inst::Label(_) => pc -= 1,
                        Inst::ParseError(_) => first_parse_error = first_parse_error.or(Some(id)),
                        _ => {}
                    }
                    break;
                } else if let Err(err) = bb.push_inst(inst, &mut globals) {
                    bb.exit = Some(ExitStmt::Error(err));
                    break;
                } else if pc >= prog.len() {
                    bb.exit = Some(ExitStmt::Error(ParseError::ImplicitEnd.into()));
                }
            }
            bbs.push(bb);
        }

        // Add block for implicit end:
        if first_parse_error.is_none() {
            let id = BBlockId::new(bbs.len());
            let mut bb = BBlockBuilder::new(id, graph);
            bb.exit = Some(ExitStmt::Error(ParseError::ImplicitEnd.into()));
            bbs.push(bb);
            first_parse_error = Some(id);
        }
        let first_parse_error = first_parse_error.unwrap();

        // Connect exits:
        for (id, bb) in bbs.iter_mut().enumerate() {
            if let Some(exit_pc) = bb.exit_pc {
                macro_rules! get_label(($l:expr) => {
                    labels.get($l).copied().unwrap_or(first_parse_error)
                });
                let next = BBlockId::new(id + 1);
                let inst = &prog[exit_pc];
                let exit = match &inst {
                    Inst::Label(_) => ExitStmt::Jmp(next),
                    Inst::Call(l) => ExitStmt::Call(get_label!(l), next),
                    Inst::Jmp(l) => ExitStmt::Jmp(get_label!(l)),
                    Inst::Jz(l) | Inst::Jn(l) => match bb.do_stack(inst, |s, table| s.pop(table)) {
                        Ok(top) => {
                            bb.stmts.push(Stmt::Eval(top));
                            let cond = match inst {
                                Inst::Jz(_) => Cond::Zero,
                                _ => Cond::Neg,
                            };
                            ExitStmt::Br(cond, top, get_label!(l), next)
                        }
                        Err(err) => ExitStmt::Error(err),
                    },
                    Inst::Ret => ExitStmt::Ret,
                    Inst::End => ExitStmt::End,
                    Inst::ParseError(err) => ExitStmt::Error(err.clone().into()),
                    _ => panic!("not a terminator"),
                };
                bb.exit = Some(exit);
            }
        }

        Cfg {
            bbs: bbs.into_iter().map(|bb| bb.finish()).collect(),
            graph,
            globals,
        }
    }

    #[inline]
    pub fn bbs(&self) -> &[BBlock<'g>] {
        &self.bbs
    }

    #[inline]
    pub fn graph(&self) -> &'g Graph {
        self.graph
    }

    #[inline]
    pub fn globals(&self) -> &NodeTable<'g> {
        &self.globals
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

impl BBlock<'_> {
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

impl<'g> BBlockBuilder<'g> {
    fn new(id: BBlockId, graph: &'g Graph) -> Self {
        BBlockBuilder {
            id,
            label: None,
            table: NodeTable::new(graph),
            stmts: Vec::new(),
            exit: None,
            exit_pc: None,
            stack: AbstractStack::new(),
            heap: AbstractHeap::new(),
        }
    }

    fn push_inst(&mut self, inst: &Inst, globals: &mut NodeTable<'g>) -> Result<(), Error> {
        match inst {
            Inst::Push(n) => self.do_stack(inst, |s, _| Ok(_ = s.push_number(n, globals)))?,
            Inst::Dup => self.do_stack(inst, |s, t| s.dup(t))?,
            Inst::Copy(n) => self.do_stack(inst, |s, t| Ok(s.copy(n.into(), t)))?,
            Inst::Swap => self.do_stack(inst, |s, t| s.swap(t))?,
            Inst::Drop => self.do_stack(inst, |s, _| s.drop_eager(1))?,
            Inst::Slide(n) => self.do_stack(inst, |s, t| s.slide(n.into(), t))?,
            Inst::Add => self.do_stack(inst, |s, t| s.apply_op(Op::Add, t))?,
            Inst::Sub => self.do_stack(inst, |s, t| s.apply_op(Op::Sub, t))?,
            Inst::Mul => self.do_stack(inst, |s, t| s.apply_op(Op::Mul, t))?,
            Inst::Div => self.do_stack(inst, |s, t| s.apply_op(Op::Div, t))?,
            Inst::Mod => self.do_stack(inst, |s, t| s.apply_op(Op::Mod, t))?,
            Inst::Store => {
                let (addr, val) = self.do_stack(inst, |s, t| s.pop2(t))?;
                self.stmts.push(Stmt::Store(addr, val)); // TODO: cache
                self.heap.store(addr, val, &mut self.table)?;
            }
            Inst::Retrieve => {
                let addr = self.do_stack(inst, |s, t| s.pop(t))?;
                self.stack.push(self.heap.retrieve(addr, &mut self.table));
            }
            Inst::Printc => {
                let val = self.do_stack(inst, |s, t| s.pop(t))?;
                self.stmts.push(Stmt::Eval(val));
                self.stmts.push(Stmt::Print(IoKind::Char, val));
            }
            Inst::Printi => {
                let val = self.do_stack(inst, |s, t| s.pop(t))?;
                self.stmts.push(Stmt::Eval(val));
                self.stmts.push(Stmt::Print(IoKind::Int, val));
            }
            Inst::Readc => {
                let addr = self.do_stack(inst, |s, t| s.pop(t))?;
                self.stmts.push(Stmt::Eval(addr));
                self.stmts.push(Stmt::Read(IoKind::Char, addr));
            }
            Inst::Readi => {
                let addr = self.do_stack(inst, |s, t| s.pop(t))?;
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

    fn do_stack<T, F>(&mut self, inst: &Inst, f: F) -> Result<T, Error>
    where
        F: FnOnce(&mut AbstractStack, &mut NodeTable) -> Result<T, UnderflowError>,
    {
        let pre = self.stack.accessed();
        let res = f(&mut self.stack, &mut self.table);
        let post = self.stack.accessed();
        if post > pre {
            self.stmts.push(Stmt::GuardStack(post));
        }
        res.map_err(|err| err.to_error(inst))
    }

    fn finish(mut self) -> BBlock<'g> {
        self.stack.simplify(&self.table);
        BBlock {
            id: self.id,
            label: self.label,
            stmts: self.stmts,
            table: self.table,
            exit: self.exit.expect("missing exit"),
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

impl<'g> Index<BBlockId> for Cfg<'g> {
    type Output = BBlock<'g>;

    fn index(&self, index: BBlockId) -> &BBlock<'g> {
        &self.bbs[index.0 as usize]
    }
}

impl<'g> IndexMut<BBlockId> for Cfg<'g> {
    fn index_mut(&mut self, index: BBlockId) -> &mut BBlock<'g> {
        &mut self.bbs[index.0 as usize]
    }
}

impl Display for Cfg<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fn visit_exp(root: NodeRef, graph: &Graph, visited: &mut BitBox, new_visited: &mut BitBox) {
            use crate::ir::{Inst, InstNoRef, InstOp1, InstOp2, InstOp2U32};
            if visited[root.index()] {
                return;
            }
            visited.set(root.index(), true);
            new_visited.set(root.index(), true);
            match graph[root].inst() {
                InstOp2!(lhs, rhs) => {
                    visit_exp(*lhs, graph, visited, new_visited);
                    visit_exp(*rhs, graph, visited, new_visited);
                }
                InstOp2U32!(v, _) | InstOp1!(v) | Inst::HeapRef(v) => {
                    visit_exp(*v, graph, visited, new_visited);
                }
                InstNoRef!() => {}
            }
        }

        let reachable = self.reachable();

        let mut visited = bitbox![0; self.graph.len()];
        let mut new_visited = bitbox![0; self.graph.len()];
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

            visited.fill(false);
            for stmt in &bb.stmts {
                new_visited.fill(false);
                match stmt {
                    Stmt::GuardStack(_) => {}
                    Stmt::Eval(val) | Stmt::Print(_, val) | Stmt::Read(_, val) => {
                        visit_exp(*val, self.graph, &mut visited, &mut new_visited);
                    }
                    Stmt::Store(addr, val) => {
                        visit_exp(*addr, self.graph, &mut visited, &mut new_visited);
                        visit_exp(*val, self.graph, &mut visited, &mut new_visited);
                    }
                }
                for i in new_visited.iter_ones() {
                    let node = NodeRef::new(i);
                    writeln!(f, "    {node} = {}", self.graph[node].inst())?;
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
                new_visited.fill(false);
                visit_exp(val, self.graph, &mut visited, &mut new_visited);
                for i in new_visited.iter_ones() {
                    let node = NodeRef::new(i);
                    writeln!(f, "    {node} = {}", self.graph[node].inst())?;
                }
                writeln!(f, "    push {val}")?;
            }

            write!(f, "    ")?;
            match &bb.exit {
                ExitStmt::Call(l1, l2) => write!(f, "call {} {}", self[*l1], self[*l2]),
                ExitStmt::Jmp(l) => write!(f, "jmp {}", self[*l]),
                ExitStmt::Br(cond, val, l1, l2) => {
                    let mut new_visited = bitbox![0; bb.table.len()];
                    visit_exp(*val, self.graph, &mut visited, &mut new_visited);
                    for i in new_visited.iter_ones() {
                        let node = NodeRef::new(i);
                        writeln!(f, "    {node} = {}", self.graph[node].inst())?;
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

impl Display for BBlock<'_> {
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

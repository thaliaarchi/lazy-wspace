use std::collections::{HashMap, VecDeque};
use std::fmt::{self, Display, Formatter};
use std::ops::{Index, IndexMut};

use bitvec::prelude::*;

use crate::ast::{Inst as Ast, LabelLit};
use crate::error::{Error, ParseError, UnderflowError};
use crate::ir::{
    AbstractHeap, AbstractStack, Cond, Graph, Inst, InstUses0, InstUses1, InstUses2, IoKind,
    LazySize, NodeRef, NodeTable,
};
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
    stmts: Vec<NodeRef>,
    exit: NodeRef,
    stack: AbstractStack,
    heap: AbstractHeap,
}

#[derive(Clone, Debug)]
struct BBlockBuilder<'g> {
    id: BBlockId,
    label: Option<LabelLit>,
    table: NodeTable<'g>,
    stmts: Vec<NodeRef>,
    exit: Option<NodeRef>,
    exit_pc: Option<usize>,
    stack: AbstractStack,
    heap: AbstractHeap,
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BBlockId {
    index: u32,
}

impl<'g> Cfg<'g> {
    pub fn new(prog: &[Ast], graph: &'g Graph) -> Self {
        let mut bbs = Vec::new();
        let mut globals = NodeTable::new(graph);
        let mut labels = HashMap::new();
        let mut first_parse_error = None;

        // Build basic block bodies and collect labels:
        let mut pc = 0;
        while pc < prog.len() {
            let id = BBlockId::new(bbs.len());
            let mut bb = BBlockBuilder::new(id, graph);
            if let Ast::Label(l) = &prog[pc] {
                if !first_parse_error.is_some_and(|err_id| id >= err_id) {
                    labels.insert(l, id);
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
                        Ast::Label(_) => pc -= 1,
                        Ast::ParseError(_) => first_parse_error = first_parse_error.or(Some(id)),
                        _ => {}
                    }
                    break;
                } else if let Err(err) = bb.push_inst(inst, &mut globals) {
                    bb.exit = Some(graph.insert(Inst::Panic(Box::new(err))));
                    break;
                } else if pc >= prog.len() {
                    let inst = Inst::Panic(Box::new(ParseError::ImplicitEnd.into()));
                    bb.exit = Some(graph.insert(inst));
                }
            }
            bbs.push(bb);
        }

        // Add block for implicit end:
        if first_parse_error.is_none() {
            let id = BBlockId::new(bbs.len());
            let mut bb = BBlockBuilder::new(id, graph);
            bb.exit = Some(graph.insert(Inst::Panic(Box::new(ParseError::ImplicitEnd.into()))));
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
                    Ast::Label(_) => Inst::Jmp(next),
                    Ast::Call(l) => Inst::Call(get_label!(l), next),
                    Ast::Jmp(l) => Inst::Jmp(get_label!(l)),
                    Ast::Jz(l) | Ast::Jn(l) => match bb.do_stack(inst, |s, table| s.pop(table)) {
                        Ok(top) => {
                            bb.stmts.push(graph.insert(Inst::Eval(top)));
                            let cond = match inst {
                                Ast::Jz(_) => Cond::Zero,
                                _ => Cond::Neg,
                            };
                            Inst::Br(cond, top, get_label!(l), next)
                        }
                        Err(err) => Inst::Panic(Box::new(err)),
                    },
                    Ast::Ret => Inst::Ret,
                    Ast::End => Inst::Exit,
                    Ast::ParseError(err) => Inst::Panic(Box::new(err.clone().into())),
                    _ => panic!("not a terminator: {inst}"),
                };
                bb.exit = Some(graph.insert(exit));
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
            visited.set(id.index(), true);
            let bb = &self[id];
            // TODO: Call and ret are not paired
            match &*self.graph[bb.exit] {
                Inst::Jmp(l) => {
                    if !visited[l.index()] {
                        queue.push_back(*l)
                    }
                }
                Inst::Call(l1, l2) | Inst::Br(_, _, l1, l2) => {
                    if !visited[l1.index()] {
                        queue.push_back(*l1);
                    }
                    if !visited[l2.index()] {
                        queue.push_back(*l2);
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
    pub fn stmts(&self) -> &[NodeRef] {
        &self.stmts
    }

    #[inline]
    pub fn exit(&self) -> &NodeRef {
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

    fn push_inst(&mut self, inst: &Ast, globals: &mut NodeTable<'g>) -> Result<(), Error> {
        match inst {
            Ast::Push(n) => self.do_stack(inst, |s, _| Ok(_ = s.push_number(n, globals)))?,
            Ast::Dup => self.do_stack(inst, |s, t| s.dup(t))?,
            Ast::Copy(n) => self.do_stack(inst, |s, t| Ok(s.copy(n.into(), t)))?,
            Ast::Swap => self.do_stack(inst, |s, t| s.swap(t))?,
            Ast::Drop => self.do_stack(inst, |s, t| s.drop_eager(1, t))?,
            Ast::Slide(n) => self.do_stack(inst, |s, t| s.slide(n.into(), t))?,
            Ast::Add => self.do_stack(inst, |s, t| s.apply_op(Op::Add, t))?,
            Ast::Sub => self.do_stack(inst, |s, t| s.apply_op(Op::Sub, t))?,
            Ast::Mul => self.do_stack(inst, |s, t| s.apply_op(Op::Mul, t))?,
            Ast::Div => self.do_stack(inst, |s, t| s.apply_op(Op::Div, t))?,
            Ast::Mod => self.do_stack(inst, |s, t| s.apply_op(Op::Mod, t))?,
            Ast::Store => {
                let (addr, val) = self.do_stack(inst, |s, t| s.pop2(t))?;
                self.stmts.push(self.table.insert_unique(Inst::Eval(addr)));
                self.heap.store(addr, val, &mut self.table)?;
                self.stmts
                    .push(self.table.insert_unique(Inst::Store(addr, val)));
            }
            Ast::Retrieve => {
                let addr = self.do_stack(inst, |s, t| s.pop(t))?;
                self.stack.push(self.heap.retrieve(addr, &mut self.table));
            }
            Ast::Printc => {
                let val = self.do_stack(inst, |s, t| s.pop(t))?;
                self.stmts.push(self.table.insert_unique(Inst::Eval(val)));
                self.stmts
                    .push(self.table.insert_unique(Inst::Print(IoKind::Char, val)));
            }
            Ast::Printi => {
                let val = self.do_stack(inst, |s, t| s.pop(t))?;
                self.stmts.push(self.table.insert_unique(Inst::Eval(val)));
                self.stmts
                    .push(self.table.insert_unique(Inst::Print(IoKind::Int, val)));
            }
            Ast::Readc => {
                let addr = self.do_stack(inst, |s, t| s.pop(t))?;
                let read = self.table.insert_unique(Inst::Read(IoKind::Char));
                self.stmts.push(read);
                self.stmts.push(self.table.insert_unique(Inst::Eval(addr)));
                self.stmts
                    .push(self.table.insert_unique(Inst::Store(addr, read)));
            }
            Ast::Readi => {
                let addr = self.do_stack(inst, |s, t| s.pop(t))?;
                let read = self.table.insert_unique(Inst::Read(IoKind::Int));
                self.stmts.push(read);
                self.stmts.push(self.table.insert_unique(Inst::Eval(addr)));
                self.stmts
                    .push(self.table.insert_unique(Inst::Store(addr, read)));
            }
            Ast::Label(_)
            | Ast::Call(_)
            | Ast::Jmp(_)
            | Ast::Jz(_)
            | Ast::Jn(_)
            | Ast::Ret
            | Ast::End
            | Ast::ParseError(_) => panic!("terminator in basic block body"),
        }
        Ok(())
    }

    fn do_stack<T, F>(&mut self, inst: &Ast, f: F) -> Result<T, Error>
    where
        F: FnOnce(&mut AbstractStack, &mut NodeTable) -> Result<T, UnderflowError>,
    {
        f(&mut self.stack, &mut self.table).map_err(|err| err.to_error(inst))
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
        BBlockId { index: id as u32 }
    }

    pub const fn index(&self) -> usize {
        self.index as usize
    }
}

impl<'g> Index<BBlockId> for Cfg<'g> {
    type Output = BBlock<'g>;

    fn index(&self, index: BBlockId) -> &BBlock<'g> {
        &self.bbs[index.index()]
    }
}

impl<'g> IndexMut<BBlockId> for Cfg<'g> {
    fn index_mut(&mut self, index: BBlockId) -> &mut BBlock<'g> {
        &mut self.bbs[index.index()]
    }
}

impl Display for Cfg<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fn visit_exp(root: NodeRef, graph: &Graph, visited: &mut BitBox, new_visited: &mut BitBox) {
            if visited[root.index()] {
                return;
            }
            visited.set(root.index(), true);
            new_visited.set(root.index(), true);
            match &*graph[root] {
                InstUses0!() => {}
                InstUses1!(v) => {
                    visit_exp(*v, graph, visited, new_visited);
                }
                InstUses2!(lhs, rhs) => {
                    visit_exp(*lhs, graph, visited, new_visited);
                    visit_exp(*rhs, graph, visited, new_visited);
                }
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

            if !reachable[bb.id.index()] {
                writeln!(f, "# unreachable")?;
            }
            writeln!(f, "{bb}:")?;

            visited.fill(false);
            for &stmt in &bb.stmts {
                new_visited.fill(false);
                let stmt = &*self.graph[stmt];
                match stmt {
                    InstUses0!() => {}
                    InstUses1!(v) => {
                        visit_exp(*v, self.graph, &mut visited, &mut new_visited);
                    }
                    InstUses2!(lhs, rhs) => {
                        visit_exp(*lhs, self.graph, &mut visited, &mut new_visited);
                        visit_exp(*rhs, self.graph, &mut visited, &mut new_visited);
                    }
                }
                for i in new_visited.iter_ones() {
                    let node = NodeRef::new(i);
                    writeln!(f, "    {node} = {}", *self.graph[node])?;
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
                    writeln!(f, "    {node} = {}", *self.graph[node])?;
                }
                writeln!(f, "    push {val}")?;
            }

            match &*self.graph[bb.exit] {
                Inst::Call(l1, l2) => write!(f, "    call {}, {}", self[*l1], self[*l2]),
                Inst::Jmp(l) => write!(f, "    jmp {}", self[*l]),
                Inst::Br(cond, val, l1, l2) => {
                    let mut new_visited = bitbox![0; bb.table.len()];
                    visit_exp(*val, self.graph, &mut visited, &mut new_visited);
                    for i in new_visited.iter_ones() {
                        let node = NodeRef::new(i);
                        writeln!(f, "    {node} = {}", *self.graph[node])?;
                    }
                    write!(f, "    br {cond}, {val}, {}, {}", self[*l1], self[*l2])
                }
                Inst::Ret => write!(f, "    ret"),
                Inst::Exit => write!(f, "    exit"),
                Inst::Panic(err) => write!(f, "    panic {err:?}"),
                inst => panic!("not a terminator: {inst}"),
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
        write!(f, "bb{}", self.index)
    }
}

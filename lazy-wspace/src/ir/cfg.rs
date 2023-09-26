use std::collections::{HashMap, VecDeque};
use std::fmt::{self, Display, Formatter};
use std::ops::{Index, IndexMut};

use bitvec::prelude::*;

use crate::ast::{Inst as Ast, LabelLit};
use crate::error::{Error, ParseError, UnderflowError};
use crate::ir::instructions::{Cond, Inst, IoFormat, Opcode, Value};
use crate::ir::{AbstractHeap, AbstractStack, Graph, LazySize, NodeRef, NodeTable};
use crate::vm::Op;

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
                    bb.exit = Some(graph.insert(Inst::trap(err)));
                    break;
                } else if pc >= prog.len() {
                    let inst = Inst::trap(ParseError::ImplicitEnd);
                    bb.exit = Some(graph.insert(inst));
                }
            }
            bbs.push(bb);
        }

        // Add block for implicit end:
        if first_parse_error.is_none() {
            let id = BBlockId::new(bbs.len());
            let mut bb = BBlockBuilder::new(id, graph);
            bb.exit = Some(graph.insert(Inst::trap(ParseError::ImplicitEnd)));
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
                    Ast::Label(_) => Inst::jmp(next),
                    Ast::Call(l) => Inst::call(get_label!(l), next),
                    Ast::Jmp(l) => Inst::jmp(get_label!(l)),
                    Ast::Jz(l) | Ast::Jn(l) => match bb.do_stack(inst, |s, table| s.pop(table)) {
                        Ok(top) => {
                            bb.stmts.push(graph.insert(Inst::eval(top)));
                            let cond = match inst {
                                Ast::Jz(_) => Cond::Zero,
                                _ => Cond::Neg,
                            };
                            Inst::br(cond, top, get_label!(l), next)
                        }
                        Err(err) => Inst::trap(err),
                    },
                    Ast::Ret => Inst::ret(),
                    Ast::End => Inst::exit(),
                    Ast::ParseError(err) => Inst::trap(err.clone()),
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
            match *self.graph[bb.exit] {
                Inst::Jmp { target: l, .. } => {
                    if !visited[l.index()] {
                        queue.push_back(l)
                    }
                }
                Inst::Call {
                    target: l1,
                    next: l2,
                    ..
                }
                | Inst::Br {
                    if_true: l1,
                    if_false: l2,
                    ..
                } => {
                    if !visited[l1.index()] {
                        queue.push_back(l1);
                    }
                    if !visited[l2.index()] {
                        queue.push_back(l2);
                    }
                }
                _ => {}
            }
        }
        visited
    }
}

impl<'g> BBlock<'g> {
    #[inline]
    pub fn id(&self) -> BBlockId {
        self.id
    }

    #[inline]
    pub fn label(&self) -> Option<&LabelLit> {
        self.label.as_ref()
    }

    #[inline]
    pub fn table(&self) -> &NodeTable<'g> {
        &self.table
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
                self.stmts.push(self.table.insert_unique(Inst::eval(addr)));
                self.heap.store(addr, val, &mut self.table)?;
                self.stmts
                    .push(self.table.insert_unique(Inst::store(addr, val)));
            }
            Ast::Retrieve => {
                let addr = self.do_stack(inst, |s, t| s.pop(t))?;
                self.stack.push(self.heap.retrieve(addr, &mut self.table));
            }
            Ast::Printc => {
                let val = self.do_stack(inst, |s, t| s.pop(t))?;
                self.stmts.push(self.table.insert_unique(Inst::eval(val)));
                self.stmts
                    .push(self.table.insert_unique(Inst::print(IoFormat::Char, val)));
            }
            Ast::Printi => {
                let val = self.do_stack(inst, |s, t| s.pop(t))?;
                self.stmts.push(self.table.insert_unique(Inst::eval(val)));
                self.stmts
                    .push(self.table.insert_unique(Inst::print(IoFormat::Int, val)));
            }
            Ast::Readc => {
                let addr = self.do_stack(inst, |s, t| s.pop(t))?;
                let read = self.table.insert_unique(Inst::read(IoFormat::Char));
                self.stmts.push(read);
                self.stmts.push(self.table.insert_unique(Inst::eval(addr)));
                self.stmts.push(
                    self.table
                        .insert_unique(Inst::store(addr, Value::new(read))),
                );
            }
            Ast::Readi => {
                let addr = self.do_stack(inst, |s, t| s.pop(t))?;
                let read = self.table.insert_unique(Inst::read(IoFormat::Int));
                self.stmts.push(read);
                self.stmts.push(self.table.insert_unique(Inst::eval(addr)));
                self.stmts.push(
                    self.table
                        .insert_unique(Inst::store(addr, Value::new(read))),
                );
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
        let mut visited = bitbox![0; self.graph.len()];
        let mut new_visited = bitbox![0; self.graph.len()];
        fn visit_node(
            root: NodeRef,
            graph: &Graph,
            visited: &mut BitBox,
            new_visited: &mut BitBox,
        ) {
            if visited[root.index()] {
                return;
            }
            visited.set(root.index(), true);
            new_visited.set(root.index(), true);
            for &v in graph[root].inst().uses() {
                visit_node(v, graph, visited, new_visited);
            }
        }
        fn print_visit(
            f: &mut Formatter<'_>,
            root: NodeRef,
            cfg: &Cfg<'_>,
            visited: &mut BitBox,
            new_visited: &mut BitBox,
        ) -> fmt::Result {
            new_visited.fill(false);
            visit_node(root, cfg.graph, visited, new_visited);
            for i in new_visited.iter_ones() {
                let i = NodeRef::new(i);
                let inst = &*cfg.graph[i];
                write!(f, "    ")?;
                if inst.is_value() || inst.opcode() == Opcode::GuardStack {
                    write!(f, "{i} = ")?;
                }
                writeln!(f, "{}", inst.as_display(cfg))?;
            }
            Ok(())
        }

        let reachable = self.reachable();

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
                print_visit(f, stmt, self, &mut visited, &mut new_visited)?;
            }

            if bb.stack.dropped() != 0 {
                writeln!(f, "    drop_eager {}", bb.stack.dropped())?;
            }
            if bb.stack.lazy_dropped() != LazySize::Finite(0) {
                writeln!(f, "    drop_lazy {:?}", bb.stack.lazy_dropped())?;
            }

            for &val in bb.stack.values() {
                print_visit(f, *val, self, &mut visited, &mut new_visited)?;
                writeln!(f, "    push {val}")?;
            }

            let exit = &*self.graph[bb.exit];
            if let Inst::Br { arg, .. } = exit {
                print_visit(f, **arg, self, &mut visited, &mut new_visited)?;
            }
            writeln!(f, "    {}", exit.as_display(self))?;
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

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

use rug::Integer;

use crate::error::NumberError;
use crate::ir::{BasicBlock, ExitStmt, Exp, ExpRef};
use crate::number::Op;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dag {
    values: Vec<DagExp>,
    matches: HashMap<*const RefCell<Exp>, usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DagExp {
    Value(Rc<Integer>),
    Op(Op, usize, usize),
    StackRef(usize),
    // LazyStackRef(usize), // TODO handle lazily
    HeapRef(usize),
    Error(NumberError),
}

impl Dag {
    #[inline]
    pub fn new() -> Self {
        Dag {
            values: Vec::new(),
            matches: HashMap::new(),
        }
    }

    pub fn from_block(bb: &BasicBlock) -> Self {
        let mut dag = Dag::new();
        for val in bb.stack().values_pushed() {
            dag.insert(val);
        }
        if let ExitStmt::Br(_, val, _, _) = bb.exit() {
            dag.insert(val);
        }
        dag
    }

    #[inline]
    pub fn values(&self) -> &[DagExp] {
        &self.values
    }

    #[inline]
    pub fn get(&self, i: usize) -> &DagExp {
        &self.values[i]
    }

    #[inline]
    pub fn lookup(&self, e: &ExpRef) -> Option<usize> {
        self.matches.get(&e.as_ptr()).copied()
    }

    pub fn insert(&mut self, e: &ExpRef) -> usize {
        if let Some(&e) = self.matches.get(&e.as_ptr()) {
            return e;
        }
        let de = match &*e.borrow() {
            Exp::Value(n) => DagExp::Value(n.clone()),
            Exp::Op(op, l, r) => {
                let l = self.insert(l);
                let r = self.insert(r);
                DagExp::Op(*op, l, r)
            }
            Exp::StackRef(n) => DagExp::StackRef(*n),
            Exp::LazyStackRef(n) => DagExp::StackRef(*n), // TODO
            Exp::HeapRef(addr) => {
                let addr = self.insert(addr);
                DagExp::HeapRef(addr)
            }
            Exp::Error(err) => DagExp::Error(err.clone()),
        };
        if let Some(i) = self
            .values
            .iter()
            .enumerate()
            .find_map(|(i, de1)| (de1 == &de).then_some(i))
        {
            self.matches.insert(e.as_ptr(), i);
            return i;
        }
        let i = self.values.len();
        self.values.push(de);
        self.matches.insert(e.as_ptr(), i);
        i
    }
}

impl Display for DagExp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DagExp::Value(n) => write!(f, "value {n}"),
            DagExp::Op(op, l, r) => write!(f, "{op} %{l} %{r}"),
            DagExp::StackRef(n) => write!(f, "stack_ref {n}"),
            DagExp::HeapRef(addr) => write!(f, "heap_ref {addr}"),
            DagExp::Error(err) => write!(f, "error {err:?}"),
        }
    }
}

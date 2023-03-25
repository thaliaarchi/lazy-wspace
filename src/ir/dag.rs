use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use rug::Integer;

use crate::error::NumberError;
use crate::ir::{Exp, ExpRef};
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
    pub fn get(&self, i: usize) -> &DagExp {
        &self.values[i]
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

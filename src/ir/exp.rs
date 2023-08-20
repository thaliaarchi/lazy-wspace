use std::fmt::{self, Display, Formatter};
use std::ops::{Index, IndexMut};
use std::rc::Rc;

use rug::Integer;

use crate::ast::NumberLit;
use crate::error::NumberError;
use crate::number::Op;

/// Pool of IR expressions.
///
/// Expressions are uniquely numbered by [`ExpRef`] and [flattened](https://www.cs.cornell.edu/~asampson/blog/flattening.html).
/// This enables easy common subexpression elimination and local value
/// numbering. All `ExpRef`s within must be indices for this pool.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExpPool {
    values: Vec<Exp>,
}

/// Expression in an [`ExpPool`].
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Exp {
    Value(Rc<Integer>),
    Op(Op, ExpRef, ExpRef),
    StackRef(usize),
    LazyStackRef(usize),
    HeapRef(ExpRef),
    Error(NumberError),
}

/// Reference to an [`Exp`] in an [`ExpPool`].
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ExpRef(u32);

impl ExpPool {
    #[inline]
    pub fn new() -> Self {
        ExpPool { values: Vec::new() }
    }

    pub fn lookup(&self, e: &Exp) -> Option<ExpRef> {
        let start = match e {
            Exp::Op(_, l, r) => (l.0.max(r.0) + 1) as usize,
            Exp::HeapRef(addr) => (addr.0 + 1) as usize,
            _ => 0,
        };
        for (i, e2) in self.values[start..].iter().enumerate() {
            if e2 == e {
                return Some(ExpRef::new(i));
            }
        }
        None
    }

    pub fn insert(&mut self, e: Exp) -> ExpRef {
        if let Some(e) = self.lookup(&e) {
            e
        } else {
            let index = ExpRef::new(self.values.len());
            self.values.push(e);
            index
        }
    }

    #[inline]
    pub fn values(&self) -> &[Exp] {
        &self.values
    }

    #[inline]
    pub fn iter_refs(&self) -> impl Iterator<Item = ExpRef> {
        (0..self.values.len() as u32).map(ExpRef)
    }

    #[inline]
    pub fn iter_entries(&self) -> impl Iterator<Item = (ExpRef, &Exp)> {
        self.values
            .iter()
            .enumerate()
            .map(|(i, e)| (ExpRef::new(i), e))
    }
}

impl Exp {
    #[inline]
    pub fn value<T: Into<Integer>>(v: T) -> Self {
        Exp::Value(Rc::new(v.into()))
    }
}

impl From<&NumberLit> for Exp {
    #[inline]
    fn from(n: &NumberLit) -> Self {
        match n {
            NumberLit::Number(n) => Exp::Value(n.clone()),
            NumberLit::Empty => Exp::Error(NumberError::EmptyLit),
        }
    }
}

impl From<NumberError> for Exp {
    #[inline]
    fn from(err: NumberError) -> Self {
        Exp::Error(err)
    }
}

impl ExpRef {
    #[inline]
    pub(crate) fn new(n: usize) -> Self {
        ExpRef(n as u32)
    }
}

impl Index<ExpRef> for ExpPool {
    type Output = Exp;

    #[inline]
    fn index(&self, index: ExpRef) -> &Exp {
        // SAFETY: The pool length is monotonically increasing, so the index
        // will always be in bounds, as long as the index was created by this
        // pool. Branding `ExpRef` with a lifetime like [`BrandedVec`](https://matyama.github.io/rust-examples/rust_examples/brands/struct.BrandedVec.html)
        // in the MPI-SWS `GhostCell` paper does not seem worth it.
        unsafe { self.values.get_unchecked(index.0 as usize) }
    }
}

impl IndexMut<ExpRef> for ExpPool {
    #[inline]
    fn index_mut(&mut self, index: ExpRef) -> &mut Self::Output {
        unsafe { self.values.get_unchecked_mut(index.0 as usize) }
    }
}

impl Display for Exp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Exp::Value(n) => write!(f, "value {n}"),
            Exp::Op(op, l, r) => write!(f, "{op} {l} {r}"),
            Exp::StackRef(n) => write!(f, "stack_ref {n}"),
            Exp::LazyStackRef(n) => write!(f, "lazy_stack_ref {n}"),
            Exp::HeapRef(addr) => write!(f, "heap_ref {addr}"),
            Exp::Error(err) => write!(f, "error {err:?}"),
        }
    }
}

impl Display for ExpRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

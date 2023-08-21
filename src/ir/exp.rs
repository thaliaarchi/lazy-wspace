use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::{BuildHasher, Hash, Hasher};
use std::ops::{Add, Index, Mul, Sub};
use std::rc::Rc;

use hashbrown::{hash_map::DefaultHashBuilder, raw::RawTable};
use rug::ops::{DivRounding, RemRounding};
use rug::Integer;

use crate::ast::NumberLit;
use crate::error::NumberError;
use crate::number::Op;

/// Pool of IR expressions.
///
/// Expressions are uniquely numbered by [`ExpRef`] and [flattened](https://www.cs.cornell.edu/~asampson/blog/flattening.html).
/// This enables easy common subexpression elimination and local value
/// numbering. All `ExpRef`s within must be indices for this pool.
#[derive(Clone)]
pub struct ExpPool {
    exps: Vec<Exp>,
    // Essentially a `HashMap<Exp, ExpRef>`, that doesn't store a redundant
    // `Exp` key, instead referencing it in `exps` using the `ExpRef` value.
    table: RawTable<ExpRef>,
}

/// Expression in an [`ExpPool`].
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Exp {
    /// A constant number.
    ///
    /// ```ir
    /// %val = value {number}
    /// ```
    Number(Rc<Integer>),
    /// A lazy unevaluated error.
    ///
    /// ```ir
    /// %val = error {kind}
    /// ```
    Error(NumberError),
    /// A lazy binary arithmetic operation.
    ///
    /// ```ir
    /// %val = {op} %lhs %rhs
    /// ```
    Op(Op, ExpRef, ExpRef),
    /// Unchecked stack reference, that must be first guarded with
    /// `guard_stack`.
    ///
    /// ```ir
    /// %val = stack_ref {index}
    /// ```
    StackRef(usize),
    /// Checked stack reference.
    ///
    /// ```ir
    /// %val = checked_stack_ref {index}
    /// ```
    CheckedStackRef(usize),
    /// Heap reference.
    ///
    /// ```ir
    /// %val = heap_ref %addr
    /// ```
    HeapRef(ExpRef),
}

/// Reference to an [`Exp`] in an [`ExpPool`].
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ExpRef(u32);

impl ExpPool {
    #[inline]
    pub const fn new() -> Self {
        ExpPool {
            exps: Vec::new(),
            table: RawTable::new(),
        }
    }

    pub fn lookup(&self, e: &Exp) -> Option<ExpRef> {
        let hash = e.make_hash();
        match self.table.find(hash, |key| &self.exps[key.as_usize()] == e) {
            Some(bucket) => Some(*unsafe { bucket.as_ref() }),
            None => None,
        }
    }

    pub fn insert(&mut self, e: Exp) -> ExpRef {
        let hash = e.make_hash();
        match self.table.find_or_find_insert_slot(
            hash,
            |key| &self.exps[key.as_usize()] == &e,
            |key| self.exps[key.as_usize()].make_hash(),
        ) {
            Ok(bucket) => *unsafe { bucket.as_ref() },
            Err(slot) => {
                let i = ExpRef::new(self.exps.len());
                self.exps.push(e);
                unsafe {
                    self.table.insert_in_slot(hash, slot, i);
                }
                i
            }
        }
    }

    pub fn insert_op(&mut self, op: Op, lhs: ExpRef, rhs: ExpRef) -> ExpRef {
        // Replacing, for example, `x * 0` with `0` is unsound, because `x`
        // could evaluate to an error.

        match (op, &self[lhs], &self[rhs]) {
            (_, Exp::Number(lhs), Exp::Number(rhs)) => {
                let (lhs, rhs) = (lhs.as_ref(), rhs.as_ref());
                let res = match op {
                    Op::Add => Ok(lhs.add(rhs).into()),
                    Op::Sub => Ok(lhs.sub(rhs).into()),
                    Op::Mul => Ok(lhs.mul(rhs).into()),
                    Op::Div if rhs.cmp0() == Ordering::Equal => Err(NumberError::DivModZero),
                    Op::Div => Ok(lhs.div_floor(rhs).into()),
                    Op::Mod if rhs.cmp0() == Ordering::Equal => Err(NumberError::DivModZero),
                    Op::Mod => Ok(lhs.rem_floor(rhs).into()),
                };
                let e = match res {
                    Ok(v) => Exp::Number(Rc::new(v)),
                    Err(err) => Exp::Error(err),
                };
                self.insert(e)
            }

            (Op::Add | Op::Sub | Op::Div | Op::Mod, _, Exp::Error(_)) => rhs,
            (Op::Add | Op::Sub | Op::Div | Op::Mod, Exp::Error(_), Exp::Number(_)) => lhs,
            (Op::Mul, Exp::Error(_), _) => lhs,
            (Op::Mul, Exp::Number(_), Exp::Error(_)) => rhs,

            (Op::Add, Exp::Number(lhs), _) if lhs.cmp0() == Ordering::Equal => rhs,
            (Op::Add | Op::Sub, _, Exp::Number(rhs)) if rhs.cmp0() == Ordering::Equal => lhs,
            (Op::Mul | Op::Div, Exp::Number(lhs), _) if **lhs == 1 => rhs,
            (Op::Mul | Op::Div, _, Exp::Number(rhs)) if **rhs == 1 => lhs,

            (Op::Div | Op::Mod, _, Exp::Number(rhs)) if rhs.cmp0() == Ordering::Equal => {
                self.insert(Exp::Error(NumberError::DivModZero))
            }

            _ => self.insert(Exp::Op(op, lhs, rhs)),
        }
    }

    #[inline]
    pub fn exps(&self) -> &[Exp] {
        &self.exps
    }

    #[inline]
    pub fn iter_refs(&self) -> impl Iterator<Item = ExpRef> {
        (0..self.exps.len() as u32).map(ExpRef)
    }

    #[inline]
    pub fn iter_entries(&self) -> impl Iterator<Item = (ExpRef, &Exp)> {
        self.exps
            .iter()
            .enumerate()
            .map(|(i, e)| (ExpRef::new(i), e))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.exps.len()
    }
}

impl Exp {
    #[inline]
    pub fn number<T: Into<Integer>>(v: T) -> Self {
        Exp::Number(Rc::new(v.into()))
    }

    fn make_hash(&self) -> u64 {
        let mut state = DefaultHashBuilder::default().build_hasher();
        self.hash(&mut state);
        state.finish()
    }
}

impl From<&NumberLit> for Exp {
    #[inline]
    fn from(n: &NumberLit) -> Self {
        match n {
            NumberLit::Number(n) => Exp::Number(n.clone()),
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
    pub(crate) const fn new(n: usize) -> Self {
        ExpRef(n as u32)
    }

    #[inline]
    pub const fn as_usize(&self) -> usize {
        self.0 as usize
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
        unsafe { self.exps.get_unchecked(index.as_usize()) }
    }
}

impl PartialEq for ExpPool {
    fn eq(&self, other: &Self) -> bool {
        self.exps == other.exps
    }
}

impl Eq for ExpPool {}

impl Debug for ExpPool {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.exps.iter().enumerate()).finish()
    }
}

impl Display for Exp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Exp::Number(n) => write!(f, "number {n}"),
            Exp::Error(err) => write!(f, "error {err:?}"),
            Exp::Op(op, l, r) => write!(f, "{op} {l} {r}"),
            Exp::StackRef(n) => write!(f, "stack_ref {n}"),
            Exp::CheckedStackRef(n) => write!(f, "checked_stack_ref {n}"),
            Exp::HeapRef(addr) => write!(f, "heap_ref {addr}"),
        }
    }
}

impl Display for ExpRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unique() {
        let mut pool = ExpPool::new();
        let x = pool.insert(Exp::number(1));
        let y = pool.insert(Exp::number(2));
        let z = pool.insert(Exp::Op(Op::Add, x, y));
        let y2 = pool.insert(Exp::number(2));
        let x2 = pool.insert(Exp::number(1));
        let z2 = pool.insert(Exp::Op(Op::Add, x2, y2));
        assert_eq!(3, pool.len());
        assert_eq!(x, x2);
        assert_eq!(y, y2);
        assert_eq!(z, z2);
    }
}

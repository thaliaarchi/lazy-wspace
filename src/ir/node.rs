use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;

use rug::Integer;
use strum::Display;

use crate::ast::NumberLit;
use crate::error::NumberError;
use crate::ir::NodeRef;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Node {
    /// A constant number.
    ///
    /// ```ir
    /// %r = value {number}
    /// ```
    Number(Rc<Integer>),
    /// A lazy unevaluated error.
    ///
    /// ```ir
    /// %r = error {kind}
    /// ```
    Error(NumberError),
    /// A lazy binary arithmetic operation.
    ///
    /// ```ir
    /// %r = {op} %lhs %rhs
    /// ```
    Op2(Op2, NodeRef, NodeRef),
    /// A lazy unary arithmetic operation.
    ///
    /// ```ir
    /// %r = {op} %v
    /// ```
    Op1(Op1, NodeRef),
    /// Unchecked stack reference, that must be first guarded with
    /// `guard_stack`.
    ///
    /// ```ir
    /// %r = stack_ref {index}
    /// ```
    StackRef(usize),
    /// Checked stack reference.
    ///
    /// ```ir
    /// %r = checked_stack_ref {index}
    /// ```
    CheckedStackRef(usize),
    /// Heap reference.
    ///
    /// ```ir
    /// %r = heap_ref %addr
    /// ```
    HeapRef(NodeRef),
}

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "lowercase")]
pub enum Op2 {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    AndNot,
    Nand,
    Nor,
    Xnor,
    NandNot,
    Shl,
    Shr,
}

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "lowercase")]
pub enum Op1 {
    Neg,
    Popcnt,
    Lsb,
}

impl Node {
    #[inline]
    pub fn number<T: Into<Integer>>(v: T) -> Self {
        Node::Number(Rc::new(v.into()))
    }
}

impl From<&NumberLit> for Node {
    #[inline]
    fn from(n: &NumberLit) -> Self {
        match n {
            NumberLit::Number(n) => Node::Number(n.clone()),
            NumberLit::Empty => Node::Error(NumberError::EmptyLit),
        }
    }
}

impl From<NumberError> for Node {
    #[inline]
    fn from(err: NumberError) -> Self {
        Node::Error(err)
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Node::Number(n) => write!(f, "number {n}"),
            Node::Error(err) => write!(f, "error {err:?}"),
            Node::Op2(op, l, r) => write!(f, "{op} {l} {r}"),
            Node::Op1(op, v) => write!(f, "{op} {v}"),
            Node::StackRef(n) => write!(f, "stack_ref {n}"),
            Node::CheckedStackRef(n) => write!(f, "checked_stack_ref {n}"),
            Node::HeapRef(addr) => write!(f, "heap_ref {addr}"),
        }
    }
}

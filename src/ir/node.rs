use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;

use rug::Integer;

use crate::ast::NumberLit;
use crate::error::NumberError;
use crate::ir::NodeRef;
use crate::number::Op;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Node {
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
    Op(Op, NodeRef, NodeRef),
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
    HeapRef(NodeRef),
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
            Node::Op(op, l, r) => write!(f, "{op} {l} {r}"),
            Node::StackRef(n) => write!(f, "stack_ref {n}"),
            Node::CheckedStackRef(n) => write!(f, "checked_stack_ref {n}"),
            Node::HeapRef(addr) => write!(f, "heap_ref {addr}"),
        }
    }
}

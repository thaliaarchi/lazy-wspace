use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;

use rug::Integer;

use crate::ast::NumberLit;
use crate::error::NumberError;
use crate::ir::NodeRef;

/// A node in a graph.
///
/// All operations evaluate the RHS first, then the LHS. In Whitespace, `mul`
/// evaluates the LHS first, but its operands are swapped when constructing the
/// IR, so it is also right-first. Synthetic operations have right-first
/// evaluation order for consistency.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Node {
    // Values
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

    // Binary operations
    /// ```ir
    /// %r = add %lhs %rhs
    /// ```
    Add(NodeRef, NodeRef),
    /// ```ir
    /// %r = sub %lhs %rhs
    /// ```
    Sub(NodeRef, NodeRef),
    /// ```ir
    /// %r = mul %lhs %rhs
    /// ```
    Mul(NodeRef, NodeRef),
    /// ```ir
    /// %r = div %lhs %rhs
    /// ```
    Div(NodeRef, NodeRef),
    /// ```ir
    /// %r = mod %lhs %rhs
    /// ```
    Mod(NodeRef, NodeRef),
    /// ```ir
    /// %r = and %lhs %rhs
    /// ```
    And(NodeRef, NodeRef),
    /// ```ir
    /// %r = or %lhs %rhs
    /// ```
    Or(NodeRef, NodeRef),
    /// ```ir
    /// %r = xor %lhs %rhs
    /// ```
    Xor(NodeRef, NodeRef),
    /// ```ir
    /// %r = andnot %lhs %rhs
    /// ```
    AndNot(NodeRef, NodeRef),
    /// ```ir
    /// %r = notand %lhs %rhs
    /// ```
    NotAnd(NodeRef, NodeRef),
    /// ```ir
    /// %r = nand %lhs %rhs
    /// ```
    Nand(NodeRef, NodeRef),
    /// ```ir
    /// %r = nor %lhs %rhs
    /// ```
    Nor(NodeRef, NodeRef),
    /// ```ir
    /// %r = xnor %lhs %rhs
    /// ```
    Xnor(NodeRef, NodeRef),
    /// ```ir
    /// %r = nandnot %lhs %rhs
    /// ```
    NandNot(NodeRef, NodeRef),
    /// ```ir
    /// %r = nnotand %lhs %rhs
    /// ```
    NNotAnd(NodeRef, NodeRef),
    /// ```ir
    /// %r = shl %lhs {rhs}
    /// ```
    ///
    /// GMP `mpz_mul_2exp`
    Shl(NodeRef, u32),
    /// ```ir
    /// %r = shr %lhs {rhs}
    /// ```
    ///
    /// GMP `mpz_fdiv_q_2exp`
    Shr(NodeRef, u32),
    /// ```ir
    /// %r = getbit %v {bit}
    /// ```
    ///
    /// GMP `mpz_tstbit`
    GetBit(NodeRef, u32),
    /// ```ir
    /// %r = ngetbit %v {bit}
    /// ```
    NGetBit(NodeRef, u32),

    // Unary operations
    /// ```ir
    /// %r = neg %v
    /// ```
    Neg(NodeRef),
    /// ```ir
    /// %r = popcnt %v
    /// ```
    Popcnt(NodeRef),

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
            Node::Add(lhs, rhs) => write!(f, "add {lhs}, {rhs}"),
            Node::Sub(lhs, rhs) => write!(f, "sub {lhs}, {rhs}"),
            Node::Mul(lhs, rhs) => write!(f, "mul {lhs}, {rhs}"),
            Node::Div(lhs, rhs) => write!(f, "div {lhs}, {rhs}"),
            Node::Mod(lhs, rhs) => write!(f, "mod {lhs}, {rhs}"),
            Node::And(lhs, rhs) => write!(f, "and {lhs}, {rhs}"),
            Node::Or(lhs, rhs) => write!(f, "or {lhs}, {rhs}"),
            Node::Xor(lhs, rhs) => write!(f, "xor {lhs}, {rhs}"),
            Node::AndNot(lhs, rhs) => write!(f, "andnot {lhs}, {rhs}"),
            Node::NotAnd(lhs, rhs) => write!(f, "notand {lhs}, {rhs}"),
            Node::Nand(lhs, rhs) => write!(f, "nand {lhs}, {rhs}"),
            Node::Nor(lhs, rhs) => write!(f, "nor {lhs}, {rhs}"),
            Node::Xnor(lhs, rhs) => write!(f, "xnor {lhs}, {rhs}"),
            Node::NandNot(lhs, rhs) => write!(f, "nandnot {lhs}, {rhs}"),
            Node::NNotAnd(lhs, rhs) => write!(f, "nnotand {lhs}, {rhs}"),
            Node::Shl(lhs, rhs) => write!(f, "shl {lhs}, {rhs}"),
            Node::Shr(lhs, rhs) => write!(f, "shr {lhs}, {rhs}"),
            Node::Neg(v) => write!(f, "neg {v}"),
            Node::GetBit(v, bit) => write!(f, "getbit {v}, {bit}"),
            Node::NGetBit(v, bit) => write!(f, "ngetbit {v}, {bit}"),
            Node::Popcnt(v) => write!(f, "popcnt {v}"),
            Node::StackRef(n) => write!(f, "stack_ref {n}"),
            Node::CheckedStackRef(n) => write!(f, "checked_stack_ref {n}"),
            Node::HeapRef(addr) => write!(f, "heap_ref {addr}"),
        }
    }
}

macro_rules! NodeOp2(($lhs:pat, $rhs:pat) => {
    Node::Add($lhs, $rhs)
    | Node::Sub($lhs, $rhs)
    | Node::Mul($lhs, $rhs)
    | Node::Div($lhs, $rhs)
    | Node::Mod($lhs, $rhs)
    | Node::And($lhs, $rhs)
    | Node::Or($lhs, $rhs)
    | Node::Xor($lhs, $rhs)
    | Node::AndNot($lhs, $rhs)
    | Node::NotAnd($lhs, $rhs)
    | Node::Nand($lhs, $rhs)
    | Node::Nor($lhs, $rhs)
    | Node::Xnor($lhs, $rhs)
    | Node::NandNot($lhs, $rhs)
    | Node::NNotAnd($lhs, $rhs)
});
pub(crate) use NodeOp2;

macro_rules! NodeOp2U32(($lhs:pat, $rhs:pat) => {
    Node::Shl($lhs, $rhs)
    | Node::Shr($lhs, $rhs)
    | Node::GetBit($lhs, $rhs)
    | Node::NGetBit($lhs, $rhs)
});
pub(crate) use NodeOp2U32;

macro_rules! NodeOp1(($v:pat) => {
    Node::Neg($v)
    | Node::Popcnt($v)
});
pub(crate) use NodeOp1;

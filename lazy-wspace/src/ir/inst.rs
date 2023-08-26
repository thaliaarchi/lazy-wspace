use std::fmt::{self, Debug, Display, Formatter};

use rug::Integer;

use crate::ast::NumberLit;
use crate::error::NumberError;
use crate::ir::NodeRef;

/// An IR instruction.
///
/// All operations evaluate the RHS first, then the LHS. In Whitespace, `mul`
/// evaluates the LHS first, but its operands are swapped when constructing the
/// IR, so it is also right-first. Synthetic operations have right-first
/// evaluation order for consistency.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Inst {
    // Values
    /// A constant number.
    ///
    /// ```ir
    /// %r = value {number}
    /// ```
    // Boxed to keep the size smaller.
    Number(Box<Integer>),
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
    ///
    /// GMP [`mpz_popcount`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fpopcount)
    /// returns the maximum `mp_bitcnt_t` for v < 0. This instead returns 0 for
    /// v < 0.
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

impl Inst {
    #[inline]
    pub fn number<T: Into<Integer>>(v: T) -> Self {
        Inst::Number(Box::new(v.into()))
    }
}

impl From<&NumberLit> for Inst {
    #[inline]
    fn from(n: &NumberLit) -> Self {
        match n {
            NumberLit::Number(n) => Inst::Number(Box::new(n.as_ref().clone())),
            NumberLit::Empty => Inst::Error(NumberError::EmptyLit),
        }
    }
}

impl From<NumberError> for Inst {
    #[inline]
    fn from(err: NumberError) -> Self {
        Inst::Error(err)
    }
}

impl Display for Inst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Inst::Number(n) => write!(f, "number {n}"),
            Inst::Error(err) => write!(f, "error {err:?}"),
            Inst::Add(lhs, rhs) => write!(f, "add {lhs}, {rhs}"),
            Inst::Sub(lhs, rhs) => write!(f, "sub {lhs}, {rhs}"),
            Inst::Mul(lhs, rhs) => write!(f, "mul {lhs}, {rhs}"),
            Inst::Div(lhs, rhs) => write!(f, "div {lhs}, {rhs}"),
            Inst::Mod(lhs, rhs) => write!(f, "mod {lhs}, {rhs}"),
            Inst::And(lhs, rhs) => write!(f, "and {lhs}, {rhs}"),
            Inst::Or(lhs, rhs) => write!(f, "or {lhs}, {rhs}"),
            Inst::Xor(lhs, rhs) => write!(f, "xor {lhs}, {rhs}"),
            Inst::AndNot(lhs, rhs) => write!(f, "andnot {lhs}, {rhs}"),
            Inst::NotAnd(lhs, rhs) => write!(f, "notand {lhs}, {rhs}"),
            Inst::Nand(lhs, rhs) => write!(f, "nand {lhs}, {rhs}"),
            Inst::Nor(lhs, rhs) => write!(f, "nor {lhs}, {rhs}"),
            Inst::Xnor(lhs, rhs) => write!(f, "xnor {lhs}, {rhs}"),
            Inst::NandNot(lhs, rhs) => write!(f, "nandnot {lhs}, {rhs}"),
            Inst::NNotAnd(lhs, rhs) => write!(f, "nnotand {lhs}, {rhs}"),
            Inst::Shl(lhs, rhs) => write!(f, "shl {lhs}, {rhs}"),
            Inst::Shr(lhs, rhs) => write!(f, "shr {lhs}, {rhs}"),
            Inst::Neg(v) => write!(f, "neg {v}"),
            Inst::GetBit(v, bit) => write!(f, "getbit {v}, {bit}"),
            Inst::NGetBit(v, bit) => write!(f, "ngetbit {v}, {bit}"),
            Inst::Popcnt(v) => write!(f, "popcnt {v}"),
            Inst::StackRef(n) => write!(f, "stack_ref {n}"),
            Inst::CheckedStackRef(n) => write!(f, "checked_stack_ref {n}"),
            Inst::HeapRef(addr) => write!(f, "heap_ref {addr}"),
        }
    }
}

impl PartialEq<Integer> for Inst {
    #[inline]
    fn eq(&self, other: &Integer) -> bool {
        if let Inst::Number(n) = self {
            &**n == other
        } else {
            false
        }
    }
}

impl PartialEq<NumberError> for Inst {
    #[inline]
    fn eq(&self, other: &NumberError) -> bool {
        if let Inst::Error(err) = self {
            err == other
        } else {
            false
        }
    }
}

macro_rules! InstOp2(($lhs:pat, $rhs:pat) => {
    Inst::Add($lhs, $rhs)
    | Inst::Sub($lhs, $rhs)
    | Inst::Mul($lhs, $rhs)
    | Inst::Div($lhs, $rhs)
    | Inst::Mod($lhs, $rhs)
    | Inst::And($lhs, $rhs)
    | Inst::Or($lhs, $rhs)
    | Inst::Xor($lhs, $rhs)
    | Inst::AndNot($lhs, $rhs)
    | Inst::NotAnd($lhs, $rhs)
    | Inst::Nand($lhs, $rhs)
    | Inst::Nor($lhs, $rhs)
    | Inst::Xnor($lhs, $rhs)
    | Inst::NandNot($lhs, $rhs)
    | Inst::NNotAnd($lhs, $rhs)
});
pub(crate) use InstOp2;

macro_rules! InstOp2U32(($lhs:pat, $rhs:pat) => {
    Inst::Shl($lhs, $rhs)
    | Inst::Shr($lhs, $rhs)
    | Inst::GetBit($lhs, $rhs)
    | Inst::NGetBit($lhs, $rhs)
});
pub(crate) use InstOp2U32;

macro_rules! InstOp1(($v:pat) => {
    Inst::Neg($v)
    | Inst::Popcnt($v)
});
pub(crate) use InstOp1;

macro_rules! InstNoRef(() => {
    Inst::Number(_)
    | Inst::Error(_)
    | Inst::StackRef(_)
    | Inst::CheckedStackRef(_)
});
pub(crate) use InstNoRef;
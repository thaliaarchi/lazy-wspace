use std::fmt::{self, Debug, Display, Formatter};

use rug::Integer;
use strum::Display;

use crate::ast::NumberLit;
use crate::error::{Error, NumberError};
use crate::ir::{BBlockId, NodeRef};

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

    /// ```ir
    /// eval %v
    /// ```
    Eval(NodeRef),

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
    /// %r = stack_ref {index} %guard
    /// ```
    StackRef(usize, NodeRef),
    /// Checked stack reference.
    ///
    /// ```ir
    /// %r = checked_stack_ref {index}
    /// ```
    CheckedStackRef(usize),
    /// ```ir
    /// %guard = guard_stack {len}
    /// ```
    GuardStack(usize),
    /// ```ir
    /// push %v
    /// ```
    Push(NodeRef),
    /// ```ir
    /// drop {count}
    /// ```
    Drop(usize),
    /// ```ir
    /// drop_lazy {count}
    /// ```
    DropLazy(usize),

    /// Heap reference.
    ///
    /// ```ir
    /// %r = heap_ref %addr
    /// ```
    HeapRef(NodeRef),
    /// ```ir
    /// store %addr, %v
    /// ```
    Store(NodeRef, NodeRef),

    /// ```ir
    /// print {kind}, %v
    /// ```
    Print(IoKind, NodeRef),
    /// ```ir
    /// %r = read {kind}
    /// ```
    Read(IoKind),

    /// ```ir
    /// call @target, @next
    /// ```
    Call(BBlockId, BBlockId),
    /// ```ir
    /// jmp @target
    /// ```
    Jmp(BBlockId),
    /// ```ir
    /// br {cond}, %v, @then, @else
    /// ```
    Br(Cond, NodeRef, BBlockId, BBlockId),
    /// ```ir
    /// ret
    /// ```
    Ret,
    /// ```ir
    /// exit
    /// ```
    Exit,
    /// ```ir
    /// panic {err}
    /// ```
    // Boxed to keep the size smaller.
    Panic(Box<Error>),
}

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "snake_case")]
pub enum Cond {
    Zero,
    Neg,
}

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "snake_case")]
pub enum IoKind {
    Char,
    Int,
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
            Inst::Eval(v) => write!(f, "eval {v}"),
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
            Inst::StackRef(n, guard) => write!(f, "stack_ref {n}, {guard}"),
            Inst::CheckedStackRef(n) => write!(f, "checked_stack_ref {n}"),
            Inst::GuardStack(len) => write!(f, "guard_stack {len}"),
            Inst::Push(v) => write!(f, "push {v}"),
            Inst::Drop(count) => write!(f, "drop {count}"),
            Inst::DropLazy(count) => write!(f, "drop_lazy {count}"),
            Inst::HeapRef(addr) => write!(f, "heap_ref {addr}"),
            Inst::Store(addr, v) => write!(f, "store {addr}, {v}"),
            Inst::Print(kind, v) => write!(f, "print {kind}, {v}"),
            Inst::Read(kind) => write!(f, "read {kind}"),
            Inst::Call(target, next) => write!(f, "call {target}, {next}"),
            Inst::Jmp(target) => write!(f, "jmp {target}"),
            Inst::Br(cond, v, t, e) => write!(f, "br {cond}, {v}, {t}, {e}"),
            Inst::Ret => write!(f, "ret"),
            Inst::Exit => write!(f, "exit"),
            Inst::Panic(err) => write!(f, "panic {err:?}"),
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

macro_rules! InstUses0(() => {
    Inst::Number(_)
    | Inst::Error(_)
    | Inst::CheckedStackRef(_)
    | Inst::GuardStack(_)
    | Inst::Drop(_)
    | Inst::DropLazy(_)
    | Inst::Read(_)
    | Inst::Call(_, _)
    | Inst::Jmp(_)
    | Inst::Ret
    | Inst::Exit
    | Inst::Panic(_)
});
pub(crate) use InstUses0;

macro_rules! InstUses1(($v:pat) => {
    Inst::Eval($v)
    | Inst::Neg($v)
    | Inst::Popcnt($v)
    | Inst::StackRef(_, $v)
    | Inst::Shl($v, _)
    | Inst::Shr($v, _)
    | Inst::GetBit($v, _)
    | Inst::NGetBit($v, _)
    | Inst::Push($v)
    | Inst::HeapRef($v)
    | Inst::Print(_, $v)
    | Inst::Br(_, $v, _, _)
});
pub(crate) use InstUses1;

macro_rules! InstUses2(($lhs:pat, $rhs:pat) => {
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
    | Inst::Store($lhs, $rhs)
});
pub(crate) use InstUses2;

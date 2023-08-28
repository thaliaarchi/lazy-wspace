use rug::Integer;

use crate::error::NumberError;
use crate::ir::{nodes::*, NodeRef};

pub trait Node {
    const MIN_INPUTS: usize;

    fn inputs(&self) -> &[NodeRef];

    fn as_exp(&self) -> Exp<'_>;
}

pub enum Exp<'a> {
    Number(&'a Integer),
    Error(NumberError),
    ErrorOr(NodeRef, NodeRef),
    Add(NodeRef, NodeRef),
    Sub(NodeRef, NodeRef),
    Mul(NodeRef, NodeRef),
    Div(NodeRef, NodeRef),
    Mod(NodeRef, NodeRef),
    And(NodeRef, NodeRef),
    Or(NodeRef, NodeRef),
    Xor(NodeRef, NodeRef),
    AndNot(NodeRef, NodeRef),
    NotAnd(NodeRef, NodeRef),
    Nand(NodeRef, NodeRef),
    Nor(NodeRef, NodeRef),
    Xnor(NodeRef, NodeRef),
    NandNot(NodeRef, NodeRef),
    NNotAnd(NodeRef, NodeRef),
    Shl(NodeRef, u32),
    Shr(NodeRef, u32),
    TestBit(NodeRef, u32),
    NTestBit(NodeRef, u32),
    Neg(NodeRef),
    Popcnt(NodeRef),
    StackRef(usize, NodeRef),
    CheckedStackRef(usize),
    HeapRef(NodeRef),
    Read(IoKind),
    Stmt,
}

impl<'a> From<&'a NumberNode> for Exp<'a> {
    #[inline]
    fn from(node: &'a NumberNode) -> Self {
        Exp::Number(node.number())
    }
}

impl<'a> From<&'a ErrorNode> for Exp<'a> {
    #[inline]
    fn from(node: &ErrorNode) -> Self {
        Exp::Error(node.error())
    }
}

impl<'a> From<&'a ErrorOrNode> for Exp<'a> {
    #[inline]
    fn from(node: &ErrorOrNode) -> Self {
        Exp::ErrorOr(node.maybe_error(), node.or_value())
    }
}

impl<'a> From<&'a EvalNode> for Exp<'a> {
    #[inline]
    fn from(_node: &EvalNode) -> Self {
        Exp::Stmt
    }
}

impl<'a> From<&'a AddNode> for Exp<'a> {
    #[inline]
    fn from(node: &AddNode) -> Self {
        Exp::Add(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a SubNode> for Exp<'a> {
    #[inline]
    fn from(node: &SubNode) -> Self {
        Exp::Sub(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a MulNode> for Exp<'a> {
    #[inline]
    fn from(node: &MulNode) -> Self {
        Exp::Mul(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a DivNode> for Exp<'a> {
    #[inline]
    fn from(node: &DivNode) -> Self {
        Exp::Div(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a ModNode> for Exp<'a> {
    #[inline]
    fn from(node: &ModNode) -> Self {
        Exp::Mod(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a AndNode> for Exp<'a> {
    #[inline]
    fn from(node: &AndNode) -> Self {
        Exp::And(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a OrNode> for Exp<'a> {
    #[inline]
    fn from(node: &OrNode) -> Self {
        Exp::Or(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a XorNode> for Exp<'a> {
    #[inline]
    fn from(node: &XorNode) -> Self {
        Exp::Xor(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a AndNotNode> for Exp<'a> {
    #[inline]
    fn from(node: &AndNotNode) -> Self {
        Exp::AndNot(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a NotAndNode> for Exp<'a> {
    #[inline]
    fn from(node: &NotAndNode) -> Self {
        Exp::NotAnd(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a NandNode> for Exp<'a> {
    #[inline]
    fn from(node: &NandNode) -> Self {
        Exp::Nand(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a NorNode> for Exp<'a> {
    #[inline]
    fn from(node: &NorNode) -> Self {
        Exp::Nor(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a XnorNode> for Exp<'a> {
    #[inline]
    fn from(node: &XnorNode) -> Self {
        Exp::Xnor(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a NandNotNode> for Exp<'a> {
    #[inline]
    fn from(node: &NandNotNode) -> Self {
        Exp::NandNot(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a NNotAndNode> for Exp<'a> {
    #[inline]
    fn from(node: &NNotAndNode) -> Self {
        Exp::NNotAnd(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a ShlNode> for Exp<'a> {
    #[inline]
    fn from(node: &ShlNode) -> Self {
        Exp::Shl(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a ShrNode> for Exp<'a> {
    #[inline]
    fn from(node: &ShrNode) -> Self {
        Exp::Shr(node.lhs(), node.rhs())
    }
}

impl<'a> From<&'a TestBitNode> for Exp<'a> {
    #[inline]
    fn from(node: &TestBitNode) -> Self {
        Exp::TestBit(node.value(), node.bit())
    }
}

impl<'a> From<&'a NTestBitNode> for Exp<'a> {
    #[inline]
    fn from(node: &NTestBitNode) -> Self {
        Exp::NTestBit(node.value(), node.bit())
    }
}

impl<'a> From<&'a NegNode> for Exp<'a> {
    #[inline]
    fn from(node: &NegNode) -> Self {
        Exp::Neg(node.value())
    }
}

impl<'a> From<&'a PopcntNode> for Exp<'a> {
    #[inline]
    fn from(node: &PopcntNode) -> Self {
        Exp::Popcnt(node.value())
    }
}

impl<'a> From<&'a StackRefNode> for Exp<'a> {
    #[inline]
    fn from(node: &StackRefNode) -> Self {
        Exp::StackRef(node.index(), node.guard())
    }
}

impl<'a> From<&'a CheckedStackRefNode> for Exp<'a> {
    #[inline]
    fn from(node: &CheckedStackRefNode) -> Self {
        Exp::CheckedStackRef(node.index())
    }
}

impl<'a> From<&'a GuardStackNode> for Exp<'a> {
    #[inline]
    fn from(_node: &GuardStackNode) -> Self {
        Exp::Stmt
    }
}

impl<'a> From<&'a PushNode> for Exp<'a> {
    #[inline]
    fn from(_node: &PushNode) -> Self {
        Exp::Stmt
    }
}

impl<'a> From<&'a DropNode> for Exp<'a> {
    #[inline]
    fn from(_node: &DropNode) -> Self {
        Exp::Stmt
    }
}

impl<'a> From<&'a DropLazyNode> for Exp<'a> {
    #[inline]
    fn from(_node: &DropLazyNode) -> Self {
        Exp::Stmt
    }
}

impl<'a> From<&'a HeapRefNode> for Exp<'a> {
    #[inline]
    fn from(node: &HeapRefNode) -> Self {
        Exp::HeapRef(node.address())
    }
}

impl<'a> From<&'a StoreNode> for Exp<'a> {
    #[inline]
    fn from(_node: &StoreNode) -> Self {
        Exp::Stmt
    }
}

impl<'a> From<&'a PrintNode> for Exp<'a> {
    #[inline]
    fn from(_node: &PrintNode) -> Self {
        Exp::Stmt
    }
}

impl<'a> From<&'a ReadNode> for Exp<'a> {
    #[inline]
    fn from(node: &ReadNode) -> Self {
        Exp::Read(node.kind())
    }
}

impl<'a> From<&'a CallNode> for Exp<'a> {
    #[inline]
    fn from(_node: &CallNode) -> Self {
        Exp::Stmt
    }
}

impl<'a> From<&'a JmpNode> for Exp<'a> {
    #[inline]
    fn from(_node: &JmpNode) -> Self {
        Exp::Stmt
    }
}

impl<'a> From<&'a BrNode> for Exp<'a> {
    #[inline]
    fn from(_node: &BrNode) -> Self {
        Exp::Stmt
    }
}

impl<'a> From<&'a RetNode> for Exp<'a> {
    #[inline]
    fn from(_node: &RetNode) -> Self {
        Exp::Stmt
    }
}

impl<'a> From<&'a ExitNode> for Exp<'a> {
    #[inline]
    fn from(_node: &ExitNode) -> Self {
        Exp::Stmt
    }
}

impl<'a> From<&'a PanicNode> for Exp<'a> {
    #[inline]
    fn from(_node: &PanicNode) -> Self {
        Exp::Stmt
    }
}

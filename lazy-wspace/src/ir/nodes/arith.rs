use lazy_wspace_macros::ir_node;

use crate::ir::NodeRef;

// Binary operations

/// ```ir
/// %r = add %lhs %rhs
/// ```
#[ir_node]
pub struct AddNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = sub %lhs %rhs
/// ```
#[ir_node]
pub struct SubNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = mul %lhs %rhs
/// ```
#[ir_node]
pub struct MulNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = div %lhs %rhs
/// ```
#[ir_node]
pub struct DivNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = mod %lhs %rhs
/// ```
#[ir_node]
pub struct ModNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = and %lhs %rhs
/// ```
#[ir_node]
pub struct AndNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = or %lhs %rhs
/// ```
#[ir_node]
pub struct OrNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = xor %lhs %rhs
/// ```
#[ir_node]
pub struct XorNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = andnot %lhs %rhs
/// ```
#[ir_node]
pub struct AndNotNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = notand %lhs %rhs
/// ```
#[ir_node]
pub struct NotAndNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = nand %lhs %rhs
/// ```
#[ir_node]
pub struct NandNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = nor %lhs %rhs
/// ```
#[ir_node]
pub struct NorNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = xnor %lhs %rhs
/// ```
#[ir_node]
pub struct XnorNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = nandnot %lhs %rhs
/// ```
#[ir_node]
pub struct NandNotNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = nnotand %lhs %rhs
/// ```
#[ir_node]
pub struct NNotAndNode {
    #[input]
    lhs: NodeRef,
    #[input]
    rhs: NodeRef,
}

/// ```ir
/// %r = shl %lhs {rhs}
/// ```
///
/// GMP `mpz_mul_2exp`
#[ir_node]
pub struct ShlNode {
    #[input]
    lhs: NodeRef,
    rhs: u32,
}

/// ```ir
/// %r = shr %lhs {rhs}
/// ```
///
/// GMP `mpz_fdiv_q_2exp`
#[ir_node]
pub struct ShrNode {
    #[input]
    lhs: NodeRef,
    rhs: u32,
}

/// ```ir
/// %r = getbit %value {bit}
/// ```
///
/// GMP `mpz_tstbit`
#[ir_node]
pub struct GetBitNode {
    #[input]
    value: NodeRef,
    bit: u32,
}

/// ```ir
/// %r = ngetbit %value {bit}
/// ```
#[ir_node]
pub struct NGetBitNode {
    #[input]
    value: NodeRef,
    bit: u32,
}

// Unary operations

/// ```ir
/// %r = neg %value
/// ```
#[ir_node]
pub struct NegNode {
    #[input]
    value: NodeRef,
}

/// ```ir
/// %r = popcnt %value
/// ```
///
/// GMP [`mpz_popcount`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fpopcount)
/// returns the maximum `mp_bitcnt_t` for v < 0. This instead returns 0 for
/// v < 0.
#[ir_node]
pub struct PopcntNode {
    #[input]
    value: NodeRef,
}

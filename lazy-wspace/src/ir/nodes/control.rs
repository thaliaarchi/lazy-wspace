use lazy_wspace_macros::ir_node;
use strum::Display;

use crate::error::Error;
use crate::ir::{BBlockId, NodeRef};

/// ```ir
/// call @target, @next
/// ```
#[ir_node]
pub struct CallNode {
    target: BBlockId,
    next: BBlockId,
}

/// ```ir
/// jmp @target
/// ```
#[ir_node]
pub struct JmpNode {
    target: BBlockId,
}

/// ```ir
/// br {cond}, %value, @if_true, @if_false
/// ```
#[ir_node]
pub struct BrNode {
    cond: Cond,
    #[input]
    value: NodeRef,
    if_true: BBlockId,
    if_false: BBlockId,
}

/// ```ir
/// ret
/// ```
#[ir_node]
pub struct RetNode;

/// ```ir
/// exit
/// ```
#[ir_node]
pub struct ExitNode;

/// ```ir
/// panic {error}
/// ```
// Boxed to keep the size smaller.
#[ir_node]
pub struct PanicNode {
    error: Box<Error>,
}

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "snake_case")]
pub enum Cond {
    Zero,
    Neg,
}

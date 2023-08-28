use lazy_wspace_macros::ir_node;

use crate::ir::NodeRef;

/// Unchecked stack reference, that must be first guarded with
/// `guard_stack`.
///
/// ```ir
/// %r = stack_ref {index} %guard
/// ```
#[ir_node]
#[display("stack_ref {index} {guard}")]
pub struct StackRefNode {
    index: usize,
    #[input]
    guard: NodeRef,
}

/// Checked stack reference.
///
/// ```ir
/// %r = checked_stack_ref {index}
/// ```
#[ir_node]
#[display("checked_stack_ref {index}")]
pub struct CheckedStackRefNode {
    index: usize,
}

/// ```ir
/// %guard = guard_stack {len}
/// ```
#[ir_node]
#[display("guard_stack {len}")]
pub struct GuardStackNode {
    len: usize,
}

/// ```ir
/// push %value
/// ```
#[ir_node]
#[display("push {value}")]
pub struct PushNode {
    #[input]
    value: NodeRef,
}

/// ```ir
/// drop {count}
/// ```
#[ir_node]
#[display("drop {count}")]
pub struct DropNode {
    count: usize,
}

/// ```ir
/// drop_lazy {count}
/// ```
#[ir_node]
#[display("drop_lazy {count}")]
pub struct DropLazyNode {
    count: usize,
}

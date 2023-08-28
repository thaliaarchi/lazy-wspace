use lazy_wspace_macros::ir_node;

use crate::ir::NodeRef;

/// Heap reference.
///
/// ```ir
/// %r = heap_ref %address
/// ```
#[ir_node]
#[display("heap_ref {address}")]
pub struct HeapRefNode {
    #[input]
    address: NodeRef,
}

/// ```ir
/// store %address, %value
/// ```
#[ir_node]
#[display("store {address}, {value}")]
pub struct StoreNode {
    #[input]
    address: NodeRef,
    #[input]
    value: NodeRef,
}

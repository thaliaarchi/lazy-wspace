use lazy_wspace_macros::ir_node;
use strum::Display;

use crate::ir::NodeRef;

/// ```ir
/// print {kind}, %value
/// ```
#[ir_node]
#[display("print {kind}, {value}")]
pub struct PrintNode {
    kind: IoKind,
    #[input]
    value: NodeRef,
}

/// ```ir
/// %r = read {kind}
/// ```
#[ir_node]
#[display("read {kind}")]
pub struct ReadNode {
    kind: IoKind,
}

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "snake_case")]
pub enum IoKind {
    Char,
    Int,
}

impl PrintNode {
    #[inline]
    pub fn kind(&self) -> IoKind {
        self.kind
    }
}

impl ReadNode {
    #[inline]
    pub fn kind(&self) -> IoKind {
        self.kind
    }
}

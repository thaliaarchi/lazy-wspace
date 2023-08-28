use lazy_wspace_macros::ir_node;
use rug::Integer;
use strum::Display;

use crate::error::NumberError;
use crate::ir::NodeRef;

// Values

/// A constant number.
///
/// ```ir
/// %r = value {number}
/// ```
#[ir_node]
#[display("value {number}")]
pub struct NumberNode {
    number: Integer,
}

/// A lazy unevaluated error.
///
/// ```ir
/// %r = error {kind}
/// ```
#[ir_node]
#[display("error {kind:?}")]
pub struct ErrorNode {
    kind: NumberError,
}

/// ```ir
/// eval %value
/// ```
#[ir_node]
#[display("eval {value}")]
pub struct EvalNode {
    #[input]
    value: NodeRef,
}

/// ```ir
/// %v = error_or %maybe_error, %or_value
/// ```
#[ir_node]
#[display("error_or {maybe_error}, {or_value}")]
pub struct ErrorOrNode {
    #[input]
    maybe_error: NodeRef,
    #[input]
    or_value: NodeRef,
}

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

impl PartialEq<Integer> for NumberNode {
    #[inline]
    fn eq(&self, other: &Integer) -> bool {
        &self.number == other
    }
}

impl PartialEq<NumberError> for ErrorNode {
    #[inline]
    fn eq(&self, other: &NumberError) -> bool {
        &self.kind == other
    }
}

impl From<NumberError> for ErrorNode {
    #[inline]
    fn from(err: NumberError) -> Self {
        ErrorNode::new(err)
    }
}

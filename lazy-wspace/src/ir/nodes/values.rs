use lazy_wspace_macros::ir_node;
use rug::Integer;

use crate::error::NumberError;
use crate::ir::NodeRef;

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
/// %r = error {error}
/// ```
#[ir_node]
#[display("error {error:?}")]
pub struct ErrorNode {
    error: NumberError,
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

impl NumberNode {
    #[inline]
    pub fn number(&self) -> &Integer {
        &self.number
    }
}

impl<T: Into<Integer>> From<T> for NumberNode {
    fn from(number: T) -> Self {
        NumberNode::new(number.into())
    }
}

impl PartialEq<Integer> for NumberNode {
    #[inline]
    fn eq(&self, other: &Integer) -> bool {
        self.number() == other
    }
}

impl PartialEq<NumberNode> for Integer {
    #[inline]
    fn eq(&self, other: &NumberNode) -> bool {
        self == other.number()
    }
}

impl PartialOrd<Integer> for NumberNode {
    #[inline]
    fn partial_cmp(&self, other: &Integer) -> Option<std::cmp::Ordering> {
        self.number().partial_cmp(other)
    }
}

impl PartialOrd<NumberNode> for Integer {
    #[inline]
    fn partial_cmp(&self, other: &NumberNode) -> Option<std::cmp::Ordering> {
        self.partial_cmp(other.number())
    }
}

impl ErrorNode {
    #[inline]
    pub fn error(&self) -> NumberError {
        self.error
    }
}

impl From<NumberError> for ErrorNode {
    #[inline]
    fn from(err: NumberError) -> Self {
        ErrorNode::new(err)
    }
}

impl PartialEq<NumberError> for ErrorNode {
    #[inline]
    fn eq(&self, other: &NumberError) -> bool {
        &self.error == other
    }
}

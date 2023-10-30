use std::collections::HashMap;

use crate::{
    error::{EagerError, Error, ValueError},
    vm::{Value, ValueRef},
};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Heap {
    heap: HashMap<u32, ValueRef>,
    len: u32,
}

impl Heap {
    #[inline]
    pub fn new() -> Self {
        Heap::default()
    }

    /// Store inserts value `n` into the heap at address `addr`.
    ///
    /// In the reference interpreter, `store` runs in *O(addr)* time and
    /// allocates *O(addr)* cons cells, by constructing a new list prefix of
    /// length addr and reusing the tail of the original list. This leads to
    /// space leaks, whenever references to the prefix of the original list are
    /// retained in unevaluated `retrieve` expressions, and runtime stack
    /// overflows, for large addresses. Experimentally, it overflows when
    /// storing at addresses greater than or equal to 3311732933 (approx.
    /// 2^31.6), starting with an empty heap, but this varies by heap size.
    ///
    /// This implementation mimics the upper address bound, by only allowing
    /// addresses up to 2^32, but does not have the linear performance issues.
    pub fn store(&mut self, addr: ValueRef, n: ValueRef) -> Result<(), Error> {
        let addr = addr.eval()?;
        if addr.is_negative() {
            return Err(EagerError::StoreNegative.into());
        }
        if let Some(addr) = addr.to_u32() {
            self.heap.insert(addr, n);
            self.len = self.len.max(addr + 1);
            return Ok(());
        }
        Err(EagerError::StoreOverflow.into())
    }

    pub fn retrieve(&mut self, addr: ValueRef) -> ValueRef {
        let addr = match addr.eval() {
            Ok(n) => n,
            Err(err) => return err.into(),
        };
        if addr.is_negative() {
            return ValueError::RetrieveNegative.into();
        }
        if let Some(addr) = addr.to_u32() {
            if addr < self.len {
                return self
                    .heap
                    .entry(addr)
                    .or_insert(Value::zero().into())
                    .clone();
            }
        }
        ValueError::RetrieveLarge.into()
    }
}

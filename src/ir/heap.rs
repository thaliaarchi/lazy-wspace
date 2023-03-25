use std::collections::HashMap;

use crate::error::{EagerError, Error, NumberError};
use crate::ir::{AbstractNumber, AbstractNumberRef};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbstractHeap {
    consts: HashMap<u32, AbstractNumberRef>,
    vars: HashMap<AbstractNumberRef, AbstractNumberRef>,
}

impl AbstractHeap {
    #[inline]
    pub fn new() -> Self {
        AbstractHeap {
            consts: HashMap::new(),
            vars: HashMap::new(),
        }
    }

    /// Get the value at address `addr` in the heap. It will return the same
    /// value as previous calls to `retrieve` with the same value for `addr`,
    /// unless a potentially-aliasing address has been written to.
    pub fn retrieve(&mut self, addr: AbstractNumberRef) -> AbstractNumberRef {
        match &*addr.borrow() {
            AbstractNumber::Value(n) => {
                return if let Some(n) = n.to_u32() {
                    self.consts
                        .entry(n)
                        .or_insert_with(|| AbstractNumber::HeapRef(addr.clone()).into())
                        .clone()
                } else {
                    AbstractNumber::Error(NumberError::RetrieveLarge).into()
                };
            }
            AbstractNumber::Error(_) => return addr.clone(),
            _ => {}
        }

        self.vars
            .entry(addr.clone())
            .or_insert_with(|| AbstractNumber::HeapRef(addr).into())
            .clone()
    }

    /// Write a value to address `addr` in the heap. It invalidates any known
    /// values at addresses that may alias with `addr`.
    pub fn store(&mut self, addr: AbstractNumberRef, val: AbstractNumberRef) -> Result<(), Error> {
        match &*addr.borrow() {
            AbstractNumber::Value(n) => {
                return if let Some(n) = n.to_u32() {
                    // A constant address may alias computed addresses, but not
                    // other constant addresses
                    self.vars.clear();

                    self.consts.insert(n, val);
                    Ok(())
                } else {
                    Err(EagerError::StoreOverflow.into())
                };
            }
            AbstractNumber::Error(err) => return Err(err.clone().into()),
            _ => {}
        }

        // A non-constant address may alias any other address
        self.vars.clear();
        self.consts.clear();

        self.vars.insert(addr, val);
        Ok(())
    }
}

use std::collections::HashMap;

use crate::error::{EagerError, Error, NumberError};
use crate::ir::{Exp, ExpPool, ExpRef};

/// Abstract heap for heap operations in a basic block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbstractHeap {
    consts: HashMap<u32, ExpRef>,
    vars: HashMap<ExpRef, ExpRef>,
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
    pub fn retrieve(&mut self, addr: ExpRef, pool: &mut ExpPool) -> ExpRef {
        match &pool[addr] {
            Exp::Number(n) => {
                if let Some(n) = n.to_u32() {
                    *self
                        .consts
                        .entry(n)
                        .or_insert_with(|| pool.insert_unique(Exp::HeapRef(addr)))
                } else {
                    pool.insert(Exp::Error(NumberError::RetrieveLarge))
                }
            }
            Exp::Error(_) => addr,
            _ => *self
                .vars
                .entry(addr)
                .or_insert_with(|| pool.insert_unique(Exp::HeapRef(addr))),
        }
    }

    /// Write a value to address `addr` in the heap. It invalidates any known
    /// values at addresses that may alias with `addr`.
    pub fn store(&mut self, addr: ExpRef, val: ExpRef, pool: &mut ExpPool) -> Result<(), Error> {
        match &pool[addr] {
            Exp::Number(n) => {
                if let Some(n) = n.to_u32() {
                    // A constant address may alias computed addresses, but not
                    // other constant addresses.
                    self.vars.clear();

                    self.consts.insert(n, val);
                    Ok(())
                } else {
                    Err(EagerError::StoreOverflow.into())
                }
            }
            Exp::Error(err) => Err(err.clone().into()),
            _ => {
                // A non-constant address may alias any other address.
                self.vars.clear();
                self.consts.clear();

                self.vars.insert(addr, val);
                Ok(())
            }
        }
    }
}

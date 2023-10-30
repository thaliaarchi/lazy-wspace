use wspace_syntax::ws::ast::{Inst, IntegerLit};

use crate::{
    error::{Error, UnderflowError, ValueError},
    vm::ValueRef,
};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Stack {
    values: Vec<ValueRef>,
    on_underflow: UnderflowError,
}

impl Stack {
    #[inline]
    pub fn new() -> Self {
        Stack::default()
    }

    #[inline]
    pub fn values(&self) -> &[ValueRef] {
        &self.values
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.values.len()
    }

    #[inline]
    pub fn push(&mut self, n: ValueRef) {
        self.values.push(n);
    }

    #[inline]
    pub fn pop(&mut self, inst: &Inst) -> Result<ValueRef, Error> {
        self.values
            .pop()
            .ok_or_else(|| self.on_underflow.to_error(inst))
    }

    pub fn copy(&mut self, n: &IntegerLit) -> ValueRef {
        match n.value() {
            Some(n) if n.is_negative() => ValueError::CopyNegative.into(),
            Some(n) => {
                // TODO: Make index wrap like Haskell `Int`.
                let n = n.to_usize().unwrap_or(usize::MAX);
                let i = self.len().wrapping_sub(n.wrapping_add(1));
                match self.values.get(i) {
                    Some(v) => v.clone(),
                    None => match self.on_underflow {
                        UnderflowError::Normal => ValueError::CopyLarge.into(),
                        UnderflowError::SlideEmpty => ValueError::EmptyLit.into(),
                    },
                }
            }
            None => ValueError::EmptyLit.into(),
        }
    }

    pub fn drop_lazy(&mut self, n: &IntegerLit) -> Result<(), Error> {
        match n.value() {
            Some(n) => {
                // Negative values slide nothing.
                if n.is_positive() {
                    // TODO: Make index wrap like Haskell `Int`.
                    let n = n.to_usize().unwrap_or(usize::MAX);
                    self.values.truncate(self.values.len().saturating_sub(n));
                }
            }
            None => {
                // If the stack later underflows, the empty argument
                // from this slide is evaluated and the resulting error
                // takes precedence over the underflow error.
                self.on_underflow = UnderflowError::SlideEmpty;
                self.values.clear();
            }
        }
        Ok(())
    }
}

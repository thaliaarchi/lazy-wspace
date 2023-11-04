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
                let n = n.to_isize_wrapping();
                if n < 0 {
                    return ValueError::CopyNegative.into();
                }
                let i = self.len().wrapping_sub((n as usize).wrapping_add(1));
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
                let n = n.to_isize_wrapping();
                if n > 0 {
                    self.values
                        .truncate(self.values.len().saturating_sub(n as usize));
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

    /// Forces evaluation of the stack up to a depth of `len`. If a `slide` with
    /// a signless argument (i.e., a parse error) was called within that depth,
    /// its error is emitted. This corresponds to pattern matching on the stack.
    #[inline]
    pub fn force(&mut self, len: usize) -> Result<(), Error> {
        if len > self.len() && self.on_underflow == UnderflowError::SlideEmpty {
            Err(ValueError::EmptyLit.into())
        } else {
            Ok(())
        }
    }
}

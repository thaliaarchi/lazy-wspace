use std::rc::Rc;

use crate::error::UnderflowError;
use crate::ir::{AbstractNumber, AbstractNumberRef};

/// An abstract stack for stack operations in a basic block
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbstractStack {
    under: Vec<Option<AbstractNumberRef>>,
    on_underflow: UnderflowError,
    drop_eager: usize,
    drop_lazy: usize,
    values: Vec<AbstractNumberRef>,
}

impl AbstractStack {
    #[inline]
    pub fn new() -> Self {
        AbstractStack {
            under: Vec::new(),
            on_underflow: UnderflowError::Normal,
            drop_eager: 0,
            drop_lazy: 0,
            values: Vec::new(),
        }
    }

    #[inline]
    pub fn push(&mut self, n: AbstractNumberRef) {
        self.values.push(n);
    }

    #[inline]
    pub fn dup(&mut self) -> AbstractNumberRef {
        let top = self.top();
        self.push(top.clone());
        top
    }

    #[inline]
    pub fn copy(&mut self, n: usize) -> AbstractNumberRef {
        let nth = self.at(n);
        self.push(nth.clone());
        nth
    }

    #[inline]
    pub fn swap(&mut self) {
        let x = self.pop();
        let y = self.pop();
        self.push(x);
        self.push(y);
    }

    #[inline]
    pub fn top(&mut self) -> AbstractNumberRef {
        self.at(0)
    }

    pub fn at(&mut self, n: usize) -> AbstractNumberRef {
        if n < self.values.len() {
            return self.values[self.values.len() - n - 1].clone();
        }
        let i = self.drop_eager.checked_add(n - self.values.len()).unwrap();
        if n >= self.under.len() {
            self.under.resize(n + 1, None);
        }
        self.under[i]
            .get_or_insert_with(|| AbstractNumber::StackRef(n).into())
            .clone()
    }

    #[inline]
    pub fn pop(&mut self) -> AbstractNumberRef {
        let top = self.top();
        self.drop_eager(1);
        top
    }

    pub fn drop_eager(&mut self, n: usize) {
        if n < self.values.len() {
            self.values.truncate(self.values.len() - n);
        } else {
            self.values.clear();
            let drops = self.drop_lazy.checked_add(n - self.values.len()).unwrap();
            self.drop_eager = self.drop_eager.checked_add(drops).unwrap();
            self.drop_lazy = 0;
        }
    }

    pub fn drop_lazy(&mut self, n: usize) {
        if n < self.values.len() {
            self.values.truncate(self.values.len() - n);
        } else {
            self.values.clear();
            let eager_drops = self.under.len().saturating_sub(self.drop_eager);
            self.drop_eager = self.drop_eager.max(self.under.len());
            let _n = n.saturating_sub(eager_drops);
            todo!();
        }
    }

    #[inline]
    pub fn slide(&mut self, n: usize) {
        let top = self.pop();
        self.drop_lazy(n);
        self.push(top);
    }

    #[inline]
    pub fn slide_error(&mut self) {
        let top = self.pop();
        self.values.clear();
        self.drop_eager = self.drop_eager.max(self.under.len());
        self.drop_lazy = 0;
        self.on_underflow = UnderflowError::SlideEmpty;
        self.push(top);
    }

    pub fn simplify(&mut self) {
        let dropped = self.drop_eager.min(self.under.len()).min(self.values.len());
        let mut shift = 0;
        for i in 0..dropped {
            if let Some(u) = self.under[dropped - i - 1].as_ref() {
                if Rc::ptr_eq(u, &self.values[i]) {
                    shift += 1;
                    continue;
                }
            }
            break;
        }
        if shift != 0 {
            self.values.drain(0..shift);
            self.drop_eager -= shift;
        }
        for i in 0..self.under.len() {
            if let Some(u) = &self.under[i] {
                if Rc::strong_count(u) == 1 && u == &AbstractNumber::StackRef(i) {
                    self.under[i] = None;
                }
            }
        }
    }
}

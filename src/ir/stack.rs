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
    pub fn dup(&mut self) -> Result<AbstractNumberRef, UnderflowError> {
        let top = self.top()?;
        self.push(top.clone());
        Ok(top)
    }

    #[inline]
    pub fn copy(&mut self, n: usize) -> Result<AbstractNumberRef, UnderflowError> {
        let nth = self.at(n)?;
        self.push(nth.clone());
        Ok(nth)
    }

    #[inline]
    pub fn swap(&mut self) -> Result<(), UnderflowError> {
        let x = self.pop()?;
        let y = self.pop()?;
        self.push(x);
        self.push(y);
        Ok(())
    }

    #[inline]
    pub fn top(&mut self) -> Result<AbstractNumberRef, UnderflowError> {
        self.at(0)
    }

    pub fn at(&mut self, n: usize) -> Result<AbstractNumberRef, UnderflowError> {
        if n < self.values.len() {
            Ok(self.values[self.values.len() - n - 1].clone())
        } else {
            let i = (self.drop_eager.checked_add(self.drop_lazy))
                .and_then(|d| d.checked_add(n - self.values.len()))
                .ok_or(self.on_underflow)?;
            if i >= self.under.len() {
                self.under.resize(i + 1, None);
            }
            Ok(self.under[i]
                .get_or_insert_with(|| AbstractNumber::StackRef(i).into())
                .clone())
        }
    }

    #[inline]
    pub fn pop(&mut self) -> Result<AbstractNumberRef, UnderflowError> {
        let top = self.top()?;
        self.drop(1)?;
        Ok(top)
    }

    pub fn drop(&mut self, n: usize) -> Result<(), UnderflowError> {
        if n < self.values.len() {
            self.values.truncate(self.values.len() - n);
        } else {
            // Forces evaluation of lazy drops
            self.drop_eager = (self.drop_eager.checked_add(self.drop_lazy))
                .and_then(|d| d.checked_add(n - self.values.len()))
                .ok_or(self.on_underflow)?;
            self.drop_lazy = 0;
            self.values.clear();
        }
        Ok(())
    }

    pub fn drop_lazy(&mut self, n: usize) {
        if n < self.values.len() {
            self.values.truncate(self.values.len() - n);
        } else {
            self.drop_lazy = self.drop_lazy.saturating_add(n - self.values.len());
            self.values.clear();
        }
    }

    #[inline]
    pub fn slide(&mut self, n: usize) -> Result<(), UnderflowError> {
        let top = self.pop()?;
        self.drop_lazy(n);
        self.push(top);
        Ok(())
    }

    #[inline]
    pub fn slide_empty(&mut self) -> Result<(), UnderflowError> {
        let top = self.pop()?;
        self.drop_eager = self.drop_eager.max(self.under.len());
        self.drop_lazy = usize::MAX;
        self.on_underflow = UnderflowError::SlideEmpty;
        self.values.clear();
        self.push(top);
        Ok(())
    }

    pub fn simplify(&mut self) {
        // Eagerly evaluate lazy drops for any already-accessed values
        if self.drop_eager < self.under.len() && self.drop_lazy != 0 {
            let n = self.drop_lazy.min(self.under.len() - self.drop_eager);
            self.drop_eager += n;
            self.drop_lazy -= n;
        }
        // Simplify pop-push identities
        let dropped = self.drop_eager.min(self.under.len()).min(self.values.len());
        let mut shift = 0;
        for i in 0..dropped {
            if let Some(u) = self.under[dropped - i - 1].as_ref() {
                if u.same_ref(&self.values[i]) {
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
        // Clear unused stack references
        for i in 0..self.under.len() {
            if let Some(u) = &self.under[i] {
                if u.is_unique() && u == &AbstractNumber::StackRef(i) {
                    self.under[i] = None;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! num(($n:expr) => {
        AbstractNumberRef::from(AbstractNumber::Number($n.into()))
    });
    macro_rules! stack_ref(($n:expr) => {
        AbstractNumberRef::from(AbstractNumber::StackRef($n))
    });
    macro_rules! stack(([$($under:expr),*] => $on_underflow:ident,
            eager $drop_eager:expr, lazy $drop_lazy:expr, [$($value:expr),*] $(,)?) => {
        AbstractStack {
            under: vec![$($under),*],
            on_underflow: UnderflowError::$on_underflow,
            drop_eager: $drop_eager,
            drop_lazy: $drop_lazy,
            values: vec![$($value),*],
        }
    });

    #[test]
    fn push() {
        let v0 = num!(0);
        let mut s = stack!([] => Normal, eager 0, lazy 0, []);
        let s1 = stack!([] => Normal, eager 0, lazy 0, [v0.clone()]);
        s.push(v0);
        assert_eq!(s1, s);
    }

    #[test]
    fn pop() {
        let v0 = num!(0);
        let v1 = num!(1);
        let u0 = stack_ref!(0);
        let u1 = stack_ref!(1);
        let u3 = stack_ref!(3);

        let mut s = stack!([] => Normal, eager 0, lazy 0, []);
        let s1 = stack!([Some(u0.clone())] => Normal, eager 1, lazy 0, []);
        assert_eq!(u0, s.pop().unwrap());
        assert_eq!(s1, s);

        let mut s = stack!([] => Normal, eager 0, lazy 0, [v0.clone(), v1.clone()]);
        let s1 = stack!([] => Normal, eager 0, lazy 0, [v0.clone()]);
        assert_eq!(v1, s.pop().unwrap());
        assert_eq!(s1, s);

        let mut s = stack!([None, Some(u1.clone()), None] => Normal, eager 3, lazy 0, []);
        let s1 =
            stack!([None, Some(u1.clone()), None, Some(u3.clone())] => Normal, eager 4, lazy 0, []);
        assert_eq!(u3, s.pop().unwrap());
        assert_eq!(s1, s);
    }
}

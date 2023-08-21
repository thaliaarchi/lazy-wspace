use crate::ast::NumberLit;
use crate::error::{NumberError, UnderflowError};
use crate::ir::{Exp, ExpPool, ExpRef};
use crate::number::Op;

/// Abstract stack for stack operations in a basic block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbstractStack {
    values: Vec<ExpRef>,
    under: Vec<Option<ExpRef>>,
    drops: usize,
    slide: LazySize,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LazySize {
    Finite(usize),
    /// A stack index (for `copy`) or size (for `slide`) larger than
    /// `usize::MAX`, that always underflows when evaluated.
    Overflow,
    /// An empty number literal, that errors when evaluated.
    EmptyLit,
}

impl AbstractStack {
    #[inline]
    pub fn new() -> Self {
        AbstractStack {
            values: Vec::new(),
            under: Vec::new(),
            drops: 0,
            slide: LazySize::Finite(0),
        }
    }

    #[inline]
    pub fn values_pushed(&self) -> &[ExpRef] {
        &self.values
    }

    #[inline]
    pub fn values_under(&self) -> &[Option<ExpRef>] {
        &self.under
    }

    #[inline]
    pub fn drop_count(&self) -> usize {
        self.drops
    }

    #[inline]
    pub fn slide_count(&self) -> LazySize {
        self.slide
    }

    #[inline]
    pub fn accessed(&self) -> usize {
        self.drops.max(self.under.len())
    }

    /// Pushes a value to the stack.
    #[inline]
    pub fn push(&mut self, n: ExpRef) {
        self.values.push(n);
    }

    /// Eagerly pushes a reference to the top element on the stack.
    #[inline]
    pub fn dup(&mut self, exps: &mut ExpPool) -> Result<(), UnderflowError> {
        let top = self.top(exps)?;
        self.push(top);
        Ok(())
    }

    /// Eagerly pushes a lazy reference to the nth element on the stack.
    #[inline]
    pub fn copy(&mut self, n: LazySize, exps: &mut ExpPool) {
        let res = match n {
            LazySize::Finite(n) => match self.at_lazy(n, exps) {
                Ok(nth) => Ok(nth),
                Err(UnderflowError::Normal) => Err(NumberError::CopyLarge),
                Err(UnderflowError::SlideEmpty) => Err(NumberError::EmptyLit),
            },
            LazySize::Overflow => Err(NumberError::CopyLarge),
            LazySize::EmptyLit => Err(NumberError::EmptyLit),
        };
        let nth = match res {
            Ok(nth) => nth,
            Err(err) => exps.insert(err.into()),
        };
        self.push(nth);
    }

    /// Eagerly swaps the top two elements on the stack.
    #[inline]
    pub fn swap(&mut self, exps: &mut ExpPool) -> Result<(), UnderflowError> {
        let x = self.pop(exps)?;
        let y = self.pop(exps)?;
        self.push(x);
        self.push(y);
        Ok(())
    }

    /// Eagerly accesses the top element of the stack and returns it.
    #[inline]
    pub fn top(&mut self, exps: &mut ExpPool) -> Result<ExpRef, UnderflowError> {
        self.at_eager(0, exps)
    }

    /// Eagerly accesses the nth element from the top of the stack and returns
    /// it.
    pub fn at_eager(&mut self, n: usize, exps: &mut ExpPool) -> Result<ExpRef, UnderflowError> {
        if n < self.values.len() {
            Ok(self.values[self.values.len() - n - 1])
        } else {
            let n = n - self.values.len();
            self.eval_slide()?;
            let i = self.drops.checked_add(n).ok_or(UnderflowError::Normal)?;
            if i >= self.under.len() {
                self.under.resize(i + 1, None);
            }
            Ok(*self.under[i].get_or_insert_with(|| exps.insert(Exp::StackRef(i))))
        }
    }

    /// Lazily accesses the nth element from the top of the stack and returns
    /// it.
    pub fn at_lazy(&mut self, n: usize, exps: &mut ExpPool) -> Result<ExpRef, UnderflowError> {
        if n < self.values.len() {
            Ok(self.values[self.values.len() - n - 1])
        } else {
            let n = n - self.values.len();
            let slide_and_drops = add_or_underflow(self.slide.as_usize()?, self.drops)?;
            let i = add_or_underflow(slide_and_drops, n)?;
            if i < self.under.len() {
                Ok(*self.under[i].get_or_insert_with(|| exps.insert(Exp::StackRef(n))))
            } else {
                Ok(exps.insert(Exp::CheckedStackRef(i)))
            }
        }
    }

    /// Eagerly removes the top element from the stack and returns it.
    #[inline]
    pub fn pop(&mut self, exps: &mut ExpPool) -> Result<ExpRef, UnderflowError> {
        let top = self.top(exps)?;
        self.drop_eager(1)?;
        Ok(top)
    }

    /// Eagerly removes the top two elements from the stack and returns them.
    #[inline]
    pub fn pop2(&mut self, exps: &mut ExpPool) -> Result<(ExpRef, ExpRef), UnderflowError> {
        let v1 = self.at_eager(1, exps)?;
        let v0 = self.at_eager(0, exps)?;
        self.drop_eager(2)?;
        Ok((v1, v0))
    }

    /// Eagerly applies an arithmetic operation to the the top two elements on
    /// the stack.
    #[inline]
    pub fn apply_op(&mut self, op: Op, exps: &mut ExpPool) -> Result<(), UnderflowError> {
        let (x, y) = self.pop2(exps)?;
        self.push(exps.insert_op(op, x, y));
        Ok(())
    }

    /// Eagerly removes the top `n` elements from the stack.
    #[inline]
    pub fn drop_eager(&mut self, n: usize) -> Result<(), UnderflowError> {
        if n <= self.values.len() {
            self.values.truncate(self.values.len() - n);
        } else {
            let n = n - self.values.len();
            self.values.clear();
            self.eval_slide()?;
            self.drops = add_or_underflow(self.drops, n)?;
        }
        Ok(())
    }

    /// Lazily removes the top `n` elements from the stack
    #[inline]
    pub fn drop_lazy(&mut self, n: LazySize) {
        match n {
            LazySize::Finite(n) => {
                if n <= self.values.len() {
                    // Eagerly drop pushed values
                    self.values.truncate(self.values.len() - n);
                } else {
                    let n = n - self.values.len();
                    self.values.clear();

                    // Eagerly drop already-accessed values
                    let nu = n.min(self.under.len().saturating_sub(self.drops));
                    self.drops -= nu;
                    let n = n - nu;

                    // Lazily drop the rest
                    self.slide = self.slide.combine(LazySize::Finite(n));
                }
            }
            LazySize::Overflow | LazySize::EmptyLit => {
                // Eagerly drop all values and underflow on further accesses
                self.values.clear();
                self.drops = self.drops.max(self.under.len());
                self.slide = self.slide.combine(n);
            }
        }
    }

    /// Lazily removes `n` elements from the top of the stack, keeping the
    /// topmost element.
    pub fn slide(&mut self, n: LazySize, exps: &mut ExpPool) -> Result<(), UnderflowError> {
        let top = self.pop(exps)?;
        self.drop_lazy(n);
        self.push(top);
        Ok(())
    }

    /// Forces evaluation of slide, to perform an eager operation.
    fn eval_slide(&mut self) -> Result<(), UnderflowError> {
        self.drops = add_or_underflow(self.slide.as_usize()?, self.drops)?;
        self.slide = LazySize::Finite(0);
        Ok(())
    }

    /// Simplifies pushed values that do not change the contents of the stack.
    pub fn simplify(&mut self) {
        // Simplify pop-push identities
        if let Ok(drops_and_slide) =
            (self.slide.as_usize()).and_then(|s| add_or_underflow(s, self.drops))
        {
            if drops_and_slide <= self.under.len() {
                let mut shift = 0;
                for i in 0..drops_and_slide.min(self.values.len()) {
                    if let Some(u) = self.under[drops_and_slide - i - 1] {
                        if u == self.values[i] {
                            shift += 1;
                            continue;
                        }
                    }
                    break;
                }
                if shift != 0 {
                    self.values.drain(0..shift);
                    self.drops -= shift;
                }
            }
        }
    }
}

#[inline]
fn add_or_underflow(n: usize, m: usize) -> Result<usize, UnderflowError> {
    n.checked_add(m).ok_or(UnderflowError::Normal)
}

impl LazySize {
    #[inline]
    pub fn combine(self, rhs: LazySize) -> LazySize {
        match (self, rhs) {
            (LazySize::Finite(n), LazySize::Finite(m)) => n
                .checked_add(m)
                .map(LazySize::Finite)
                .unwrap_or(LazySize::Overflow),
            (LazySize::Overflow | LazySize::EmptyLit, _) => self,
            (_, LazySize::Overflow | LazySize::EmptyLit) => rhs,
        }
    }

    #[inline]
    pub fn as_usize(&self) -> Result<usize, UnderflowError> {
        match self {
            LazySize::Finite(n) => Ok(*n),
            LazySize::Overflow => Err(UnderflowError::Normal),
            LazySize::EmptyLit => Err(UnderflowError::SlideEmpty),
        }
    }
}

impl From<&NumberLit> for LazySize {
    #[inline]
    fn from(n: &NumberLit) -> Self {
        match n {
            NumberLit::Number(n) => {
                if let Some(n) = n.to_usize() {
                    LazySize::Finite(n)
                } else {
                    LazySize::Overflow
                }
            }
            NumberLit::Empty => LazySize::EmptyLit,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use LazySize::*;

    macro_rules! stack(([$($value:expr),*], [$($under:expr),*], $drops:expr, $slide:expr$(,)?) => {
        AbstractStack {
            values: vec![$($value),*],
            under: vec![$($under),*],
            drops: $drops,
            slide: $slide,
        }
    });

    #[test]
    fn push() {
        let v0 = ExpRef::new(0);

        let mut s = stack!([], [], 0, Finite(0));
        s.push(v0);
        let s1 = stack!([v0], [], 0, Finite(0));
        assert_eq!(s1, s);
    }

    #[test]
    fn pop() {
        {
            let mut exps = ExpPool::new();
            let mut exps1 = ExpPool::new();
            let u0 = exps1.insert(Exp::StackRef(0));

            let mut s = stack!([], [], 0, Finite(0));
            let top = s.pop(&mut exps).unwrap();
            let s1 = stack!([], [Some(u0)], 1, Finite(0));
            let top1 = u0;

            assert_eq!(s1, s);
            assert_eq!(top1, top);
            assert_eq!(exps1, exps);
        }
        {
            let mut exps = ExpPool::new();
            let v0 = exps.insert(Exp::number(1));
            let v1 = exps.insert(Exp::number(2));
            let exps1 = exps.clone();

            let mut s = stack!([v0, v1], [], 0, Finite(0));
            let top = s.pop(&mut exps).unwrap();
            let s1 = stack!([v0], [], 0, Finite(0));
            let top1 = v1;

            assert_eq!(s1, s);
            assert_eq!(top1, top);
            assert_eq!(exps1, exps);
        }
        {
            let mut exps = ExpPool::new();
            let u1 = exps.insert(Exp::StackRef(1));
            let mut exps1 = exps.clone();
            let u3 = exps1.insert(Exp::StackRef(3));

            let mut s = stack!([], [None, Some(u1), None], 3, Finite(0));
            let top = s.pop(&mut exps).unwrap();
            let s1 = stack!([], [None, Some(u1), None, Some(u3)], 4, Finite(0));
            let top1 = u3;

            assert_eq!(s1, s);
            assert_eq!(top1, top);
            assert_eq!(exps1, exps);
        }
    }

    #[test]
    fn drop_eager() {
        let v0 = ExpRef::new(0);
        let v1 = ExpRef::new(1);
        let v2 = ExpRef::new(2);
        let v3 = ExpRef::new(3);

        let mut s = stack!([v0, v1, v2], [], 5, Finite(3));
        s.drop_eager(2).unwrap();
        let s1 = stack!([v0], [], 5, Finite(3));
        assert_eq!(s1, s);

        let mut s = stack!([v0, v1, v2], [], 5, Finite(3));
        s.drop_eager(5).unwrap();
        let s1 = stack!([], [], 10, Finite(0));
        assert_eq!(s1, s);

        let mut s = stack!([], [None, Some(v0), None], 5, Finite(3));
        s.drop_eager(1).unwrap();
        let s1 = stack!([], [None, Some(v0), None], 9, Finite(0));
        assert_eq!(s1, s);

        let mut s = stack!([], [], 5, Finite(3));
        s.drop_eager(2).unwrap();
        let s1 = stack!([], [], 10, Finite(0));
        assert_eq!(s1, s);

        let mut s = stack!([v0, v1, v2], [Some(v3)], 5, Overflow);
        assert_eq!(Err(UnderflowError::Normal), s.drop_eager(5));

        let mut s = stack!([v0, v1, v2], [Some(v3)], 5, EmptyLit);
        assert_eq!(Err(UnderflowError::SlideEmpty), s.drop_eager(5));

        let mut s = stack!([v0], [], 3, Finite(usize::MAX - 3));
        assert_eq!(Err(UnderflowError::Normal), s.drop_eager(5));
    }

    #[test]
    fn drop_lazy() {
        let v0 = ExpRef::new(0);
        let v1 = ExpRef::new(1);
        let v2 = ExpRef::new(2);
        let v3 = ExpRef::new(3);

        let mut s = stack!([v0, v1, v2], [], 5, Finite(3));
        s.drop_lazy(Finite(2));
        let s1 = stack!([v0], [], 5, Finite(3));
        assert_eq!(s1, s);

        let mut s = stack!([v0, v1, v2], [], 5, Finite(3));
        s.drop_lazy(Finite(5));
        let s1 = stack!([], [], 5, Finite(5));
        assert_eq!(s1, s);

        let mut s = stack!([], [None, Some(v0), None], 5, Finite(3));
        s.drop_lazy(Finite(1));
        let s1 = stack!([], [None, Some(v0), None], 5, Finite(4));
        assert_eq!(s1, s);

        let mut s = stack!([], [], 5, Finite(3));
        s.drop_lazy(Finite(2));
        let s1 = stack!([], [], 5, Finite(5));
        assert_eq!(s1, s);

        let mut s = stack!([v0, v1, v2], [Some(v3)], 5, Overflow);
        s.drop_lazy(Finite(5));
        let s1 = stack!([], [Some(v3)], 5, Overflow);
        assert_eq!(s1, s);

        let mut s = stack!([v0, v1, v2], [Some(v3)], 5, EmptyLit);
        s.drop_lazy(Finite(5));
        let s1 = stack!([], [Some(v3)], 5, EmptyLit);
        assert_eq!(s1, s);

        let mut s = stack!([v0], [], 3, Finite(usize::MAX - 3));
        s.drop_lazy(Finite(5));
        let s1 = stack!([], [], 3, Overflow);
        assert_eq!(s1, s);
    }
}

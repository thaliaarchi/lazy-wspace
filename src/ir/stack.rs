use crate::ast::NumberLit;
use crate::error::{NumberError, UnderflowError};
use crate::ir::{AbstractNumber, AbstractNumberRef};

/// An abstract stack for stack operations in a basic block
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbstractStack {
    values: Vec<AbstractNumberRef>,
    under: Vec<Option<AbstractNumberRef>>,
    drops: usize,
    slide: LazySize,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LazySize {
    Finite(usize),
    /// A slide amount larger than `usize::MAX`, that always underflows when
    /// evaluated.
    Overflow,
    /// Represents `Slide(NumberLit::Empty)`. When it is evaluated, it produces
    /// `UnderflowError::SlideEmpty`.
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

    /// Pushes a value to the stack.
    #[inline]
    pub fn push(&mut self, n: AbstractNumberRef) {
        self.values.push(n);
    }

    /// Eagerly pushes a reference to the top element on the stack.
    #[inline]
    pub fn dup(&mut self) -> Result<AbstractNumberRef, UnderflowError> {
        let top = self.top()?;
        self.push(top.clone());
        Ok(top)
    }

    /// Eagerly pushes a lazy reference to the nth element on the stack.
    #[inline]
    pub fn copy(&mut self, n: LazySize) -> AbstractNumberRef {
        let nth = match n {
            LazySize::Finite(n) => match self.at_lazy(n) {
                Ok(nth) => nth,
                Err(UnderflowError::Normal) => NumberError::CopyLarge.into(),
                Err(UnderflowError::SlideEmpty) => NumberError::EmptyLit.into(),
            },
            LazySize::Overflow => NumberError::CopyLarge.into(),
            LazySize::EmptyLit => NumberError::EmptyLit.into(),
        };
        self.push(nth.clone());
        nth
    }

    /// Eagerly swaps the top two elements on the stack.
    #[inline]
    pub fn swap(&mut self) -> Result<(), UnderflowError> {
        let x = self.pop()?;
        let y = self.pop()?;
        self.push(x);
        self.push(y);
        Ok(())
    }

    /// Eagerly accesses the top element of the stack and returns it.
    #[inline]
    pub fn top(&mut self) -> Result<AbstractNumberRef, UnderflowError> {
        self.at_eager(0)
    }

    /// Eagerly accesses the nth element from the top of the stack and returns
    /// it.
    pub fn at_eager(&mut self, n: usize) -> Result<AbstractNumberRef, UnderflowError> {
        if n < self.values.len() {
            Ok(self.values[self.values.len() - n - 1].clone())
        } else {
            let n = n - self.values.len();
            self.eval_slide()?;
            let i = self.drops.checked_add(n).ok_or(UnderflowError::Normal)?;
            if i >= self.under.len() {
                self.under.resize(i + 1, None);
            }
            Ok(self.under[i]
                .get_or_insert_with(|| AbstractNumber::StackRef(i).into())
                .clone())
        }
    }

    /// Lazily accesses the nth element from the top of the stack and returns
    /// it.
    pub fn at_lazy(&mut self, n: usize) -> Result<AbstractNumberRef, UnderflowError> {
        if n < self.values.len() {
            Ok(self.values[self.values.len() - n - 1].clone())
        } else {
            let n = n - self.values.len();
            let slide_and_drops = add_or_underflow(self.slide.as_usize()?, self.drops)?;
            let i = add_or_underflow(slide_and_drops, n)?;
            if i < self.under.len() {
                Ok(self.under[i]
                    .get_or_insert_with(|| AbstractNumber::StackRef(n).into())
                    .clone())
            } else {
                Ok(AbstractNumber::LazyStackRef(i).into())
            }
        }
    }

    /// Eagerly removes the top element from the stack and returns it.
    #[inline]
    pub fn pop(&mut self) -> Result<AbstractNumberRef, UnderflowError> {
        let top = self.top()?;
        self.drop_eager(1)?;
        Ok(top)
    }

    /// Eagerly removes the top two elements from the stack and returns them.
    #[inline]
    pub fn pop2(&mut self) -> Result<(AbstractNumberRef, AbstractNumberRef), UnderflowError> {
        let vals = (self.at_eager(1)?, self.at_eager(0)?);
        self.drop_eager(2)?;
        Ok(vals)
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
    pub fn slide(&mut self, n: LazySize) -> Result<(), UnderflowError> {
        let top = self.pop()?;
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

    /// Simplifies pushed values that do not change the contents of the stack
    /// and clears unused stack references.
    pub fn simplify(&mut self) {
        // Simplify pop-push identities
        if let Ok(drops_and_slide) =
            (self.slide.as_usize()).and_then(|s| add_or_underflow(s, self.drops))
        {
            if drops_and_slide <= self.under.len() {
                let mut shift = 0;
                for i in 0..drops_and_slide.min(self.values.len()) {
                    if let Some(u) = self.under[drops_and_slide - i - 1].as_ref() {
                        if u.same_ref(&self.values[i]) {
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
            (LazySize::EmptyLit, _) | (_, LazySize::EmptyLit) => LazySize::EmptyLit,
            (LazySize::Overflow, _) | (_, LazySize::Overflow) => LazySize::Overflow,
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

impl From<NumberLit> for LazySize {
    #[inline]
    fn from(n: NumberLit) -> Self {
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

    macro_rules! num(($n:expr) => {
        AbstractNumberRef::from($n)
    });
    macro_rules! stk(($n:expr) => {
        AbstractNumberRef::from(AbstractNumber::StackRef($n))
    });
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
        let mut s = stack!([], [], 0, Finite(0));
        let s1 = stack!([num!(0)], [], 0, Finite(0));
        s.push(num!(0));
        assert_eq!(s1, s);
    }

    #[test]
    fn pop() {
        let mut s = stack!([], [], 0, Finite(0));
        let s1 = stack!([], [Some(stk!(0))], 1, Finite(0));
        assert_eq!(stk!(0), s.pop().unwrap());
        assert_eq!(s1, s);

        let mut s = stack!([num!(0), num!(1)], [], 0, Finite(0));
        let s1 = stack!([num!(0)], [], 0, Finite(0));
        assert_eq!(num!(1), s.pop().unwrap());
        assert_eq!(s1, s);

        let mut s = stack!([], [None, Some(stk!(1)), None], 3, Finite(0));
        let s1 = stack!([], [None, Some(stk!(1)), None, Some(stk!(3))], 4, Finite(0));
        assert_eq!(stk!(3), s.pop().unwrap());
        assert_eq!(s1, s);
    }

    #[test]
    fn drop_eager() {
        let mut s = stack!([num!(1), num!(2), num!(3)], [], 5, Finite(3));
        let s1 = stack!([num!(1)], [], 5, Finite(3));
        s.drop_eager(2).unwrap();
        assert_eq!(s1, s);

        let mut s = stack!([num!(1), num!(2), num!(3)], [], 5, Finite(3));
        let s1 = stack!([], [], 10, Finite(0));
        s.drop_eager(5).unwrap();
        assert_eq!(s1, s);

        let mut s = stack!([], [None, Some(stk!(1)), None], 5, Finite(3));
        let s1 = stack!([], [None, Some(stk!(1)), None], 9, Finite(0));
        s.drop_eager(1).unwrap();
        assert_eq!(s1, s);

        let mut s = stack!([], [], 5, Finite(3));
        let s1 = stack!([], [], 10, Finite(0));
        s.drop_eager(2).unwrap();
        assert_eq!(s1, s);

        let mut s = stack!([num!(1), num!(2), num!(3)], [Some(stk!(0))], 5, Overflow);
        assert_eq!(Err(UnderflowError::Normal), s.drop_eager(5));

        let mut s = stack!([num!(1), num!(2), num!(3)], [Some(stk!(0))], 5, EmptyLit);
        assert_eq!(Err(UnderflowError::SlideEmpty), s.drop_eager(5));

        let mut s = stack!([num!(1)], [], 3, Finite(usize::MAX - 3));
        assert_eq!(Err(UnderflowError::Normal), s.drop_eager(5));
    }

    #[test]
    fn drop_lazy() {
        let mut s = stack!([num!(1), num!(2), num!(3)], [], 5, Finite(3));
        let s1 = stack!([num!(1)], [], 5, Finite(3));
        s.drop_lazy(Finite(2));
        assert_eq!(s1, s);

        let mut s = stack!([num!(1), num!(2), num!(3)], [], 5, Finite(3));
        let s1 = stack!([], [], 5, Finite(5));
        s.drop_lazy(Finite(5));
        assert_eq!(s1, s);

        let mut s = stack!([], [None, Some(stk!(1)), None], 5, Finite(3));
        let s1 = stack!([], [None, Some(stk!(1)), None], 5, Finite(4));
        s.drop_lazy(Finite(1));
        assert_eq!(s1, s);

        let mut s = stack!([], [], 5, Finite(3));
        let s1 = stack!([], [], 5, Finite(5));
        s.drop_lazy(Finite(2));
        assert_eq!(s1, s);

        let mut s = stack!([num!(1), num!(2), num!(3)], [Some(stk!(0))], 5, Overflow);
        let s1 = stack!([], [Some(stk!(0))], 5, Overflow);
        s.drop_lazy(Finite(5));
        assert_eq!(s1, s);

        let mut s = stack!([num!(1), num!(2), num!(3)], [Some(stk!(0))], 5, EmptyLit);
        let s1 = stack!([], [Some(stk!(0))], 5, EmptyLit);
        s.drop_lazy(Finite(5));
        assert_eq!(s1, s);

        let mut s = stack!([num!(1)], [], 3, Finite(usize::MAX - 3));
        let s1 = stack!([], [], 3, Overflow);
        s.drop_lazy(Finite(5));
        assert_eq!(s1, s);
    }
}

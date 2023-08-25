use crate::ast::NumberLit;
use crate::error::{NumberError, UnderflowError};
use crate::ir::{Node, NodeRef, NodeTable};
use crate::number::Op;

/// Abstract stack for stack operations in a basic block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbstractStack {
    values: Vec<NodeRef>,
    accessed: usize, // accessed >= dropped
    dropped: usize,
    lazy_dropped: LazySize,
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
            accessed: 0,
            dropped: 0,
            lazy_dropped: LazySize::Finite(0),
        }
    }

    #[inline]
    pub fn values(&self) -> &[NodeRef] {
        &self.values
    }

    #[inline]
    pub fn accessed(&self) -> usize {
        self.accessed
    }

    #[inline]
    pub fn dropped(&self) -> usize {
        self.dropped
    }

    #[inline]
    pub fn lazy_dropped(&self) -> LazySize {
        self.lazy_dropped
    }

    /// Pushes a value to the stack.
    #[inline]
    pub fn push(&mut self, n: NodeRef) {
        self.values.push(n);
    }

    /// Eagerly pushes a reference to the top element on the stack.
    #[inline]
    pub fn dup(&mut self, table: &mut NodeTable<'_>) -> Result<(), UnderflowError> {
        let top = self.top(table)?;
        self.push(top);
        Ok(())
    }

    /// Eagerly pushes a lazy reference to the nth element on the stack.
    #[inline]
    pub fn copy(&mut self, n: LazySize, table: &mut NodeTable<'_>) {
        let res = match n {
            LazySize::Finite(n) => match self.at_lazy(n, table) {
                Ok(nth) => Ok(nth),
                Err(UnderflowError::Normal) => Err(NumberError::CopyLarge),
                Err(UnderflowError::SlideEmpty) => Err(NumberError::EmptyLit),
            },
            LazySize::Overflow => Err(NumberError::CopyLarge),
            LazySize::EmptyLit => Err(NumberError::EmptyLit),
        };
        let nth = match res {
            Ok(nth) => nth,
            Err(err) => table.insert(err.into()),
        };
        self.push(nth);
    }

    /// Eagerly swaps the top two elements on the stack.
    #[inline]
    pub fn swap(&mut self, table: &mut NodeTable<'_>) -> Result<(), UnderflowError> {
        let x = self.pop(table)?;
        let y = self.pop(table)?;
        self.push(x);
        self.push(y);
        Ok(())
    }

    /// Eagerly accesses the top element of the stack and returns it.
    #[inline]
    pub fn top(&mut self, table: &mut NodeTable<'_>) -> Result<NodeRef, UnderflowError> {
        self.at_eager(0, table)
    }

    /// Eagerly accesses the nth element from the top of the stack and returns
    /// it.
    pub fn at_eager(
        &mut self,
        n: usize,
        table: &mut NodeTable<'_>,
    ) -> Result<NodeRef, UnderflowError> {
        self.at(n, false, table)
    }

    /// Lazily accesses the nth element from the top of the stack and returns
    /// it.
    pub fn at_lazy(
        &mut self,
        n: usize,
        table: &mut NodeTable<'_>,
    ) -> Result<NodeRef, UnderflowError> {
        self.at(n, true, table)
    }

    #[inline]
    fn at(
        &mut self,
        n: usize,
        lazy: bool,
        table: &mut NodeTable<'_>,
    ) -> Result<NodeRef, UnderflowError> {
        if n < self.values.len() {
            Ok(self.values[self.values.len() - n - 1])
        } else {
            let n = n - self.values.len();
            let drops = if lazy {
                add_or_underflow(self.dropped, self.lazy_dropped.as_usize()?)?
            } else {
                self.eval_slide(0)?;
                self.dropped
            };
            let i = add_or_underflow(drops, n)?;
            let size = add_or_underflow(i, 1)?;
            let node = if i >= self.accessed && lazy {
                Node::CheckedStackRef(i)
            } else {
                self.accessed = self.accessed.max(size);
                Node::StackRef(i)
            };
            Ok(table.insert(node))
        }
    }

    /// Eagerly removes the top element from the stack and returns it.
    #[inline]
    pub fn pop(&mut self, table: &mut NodeTable<'_>) -> Result<NodeRef, UnderflowError> {
        let top = self.top(table)?;
        self.drop_eager(1)?;
        Ok(top)
    }

    /// Eagerly removes the top two elements from the stack and returns them.
    #[inline]
    pub fn pop2(
        &mut self,
        table: &mut NodeTable<'_>,
    ) -> Result<(NodeRef, NodeRef), UnderflowError> {
        let v1 = self.at_eager(1, table)?;
        let v0 = self.at_eager(0, table)?;
        self.drop_eager(2)?;
        Ok((v1, v0))
    }

    /// Eagerly applies an arithmetic operation to the the top two elements on
    /// the stack.
    #[inline]
    pub fn apply_op(&mut self, op: Op, table: &mut NodeTable<'_>) -> Result<(), UnderflowError> {
        let (x, y) = self.pop2(table)?;
        let node = match op {
            Op::Add => Node::Add(x, y),
            Op::Sub => Node::Sub(x, y),
            // Mul has left-first evaluation order in Whitespace, unlike the
            // others, so the operands are swapped to make the IR consistent.
            Op::Mul => Node::Mul(y, x),
            Op::Div => Node::Div(x, y),
            Op::Mod => Node::Mod(x, y),
        };
        self.push(table.insert_peephole(node));
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
            self.eval_slide(n)?;
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
                    let accessed = (self.accessed - self.dropped).min(n);
                    self.dropped += accessed;
                    let n = n - accessed;

                    // Lazily drop the rest
                    self.lazy_dropped = self.lazy_dropped.combine(LazySize::Finite(n));
                }
            }
            LazySize::Overflow | LazySize::EmptyLit => {
                // Eagerly drop all values and underflow on further accesses
                self.values.clear();
                self.dropped = self.accessed;
                self.lazy_dropped = self.lazy_dropped.combine(n);
            }
        }
    }

    /// Lazily removes `n` elements from the top of the stack, keeping the
    /// topmost element.
    pub fn slide(&mut self, n: LazySize, table: &mut NodeTable<'_>) -> Result<(), UnderflowError> {
        let top = self.pop(table)?;
        self.drop_lazy(n);
        self.push(top);
        Ok(())
    }

    /// Forces evaluation of slide, to perform an eager operation.
    fn eval_slide(&mut self, eager_drops: usize) -> Result<(), UnderflowError> {
        self.dropped = add_or_underflow(self.dropped, self.lazy_dropped.as_usize()?)?;
        self.dropped = add_or_underflow(self.dropped, eager_drops)?;
        self.accessed = self.accessed.max(self.dropped);
        self.lazy_dropped = LazySize::Finite(0);
        Ok(())
    }

    /// Simplifies pushed values, that do not change the contents of the stack.
    pub fn simplify(&mut self, table: &NodeTable<'_>) {
        // Simplify pop-push identities
        if let Ok(drops) = (self.lazy_dropped.as_usize())
            .and_then(|lazy_drops| add_or_underflow(self.dropped, lazy_drops))
        {
            if drops <= self.accessed {
                let mut shift = 0;
                for &v in &self.values[0..drops.min(self.values.len())] {
                    let i = drops - (shift + 1);
                    if table[v] == Node::StackRef(i)
                        || (i < self.accessed && table[v] == Node::CheckedStackRef(i))
                    {
                        shift += 1;
                    } else {
                        break;
                    }
                }
                if shift != 0 {
                    self.values.drain(0..shift);
                    self.dropped -= shift;
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
    use crate::ir::Graph;

    use super::*;
    use LazySize::*;

    macro_rules! stack(([$($value:expr),*], $accessed:expr, $dropped:expr, $lazy_dropped:expr$(,)?) => {
        AbstractStack {
            values: vec![$($value),*],
            accessed: $accessed,
            dropped: $dropped,
            lazy_dropped: $lazy_dropped,
        }
    });

    #[test]
    fn push() {
        let v0 = NodeRef::new(0);

        let mut s = stack!([], 0, 0, Finite(0));
        s.push(v0);
        let s1 = stack!([v0], 0, 0, Finite(0));
        assert_eq!(s1, s);
    }

    #[test]
    fn pop() {
        let graph = unsafe { Graph::new() };
        let (s, top) = {
            let mut table = NodeTable::new(&graph);
            let mut s = stack!([], 0, 0, Finite(0));
            let top = s.pop(&mut table).unwrap();
            (s, top)
        };
        let graph1 = unsafe { Graph::new() };
        let (s1, top1) = {
            let r1 = graph1.push(Node::StackRef(0));
            let s1 = stack!([], 1, 1, Finite(0));
            (s1, r1)
        };
        assert_eq!(s1, s);
        assert_eq!(top1, top);
        assert_eq!(graph1, graph);

        let graph = unsafe { Graph::new() };
        let (s, top) = {
            let mut table = NodeTable::new(&graph);
            let v0 = table.insert(Node::number(1));
            let v1 = table.insert(Node::number(2));
            let mut s = stack!([v0, v1], 0, 0, Finite(0));
            let top = s.pop(&mut table).unwrap();
            (s, top)
        };
        let graph1 = unsafe { Graph::new() };
        let (s1, top1) = {
            let v0 = graph1.push(Node::number(1));
            let v1 = graph1.push(Node::number(2));
            let s1 = stack!([v0], 0, 0, Finite(0));
            (s1, v1)
        };
        assert_eq!(s1, s);
        assert_eq!(top1, top);
        assert_eq!(graph1, graph);

        let graph = unsafe { Graph::new() };
        let (s, top) = {
            let mut table = NodeTable::new(&graph);
            table.insert(Node::StackRef(1));
            let mut s = stack!([], 3, 3, Finite(0));
            let top = s.pop(&mut table).unwrap();
            (s, top)
        };
        let graph1 = unsafe { Graph::new() };
        let (s1, top1) = {
            graph1.push(Node::StackRef(1));
            let r1 = graph1.push(Node::StackRef(3));
            let s1 = stack!([], 4, 4, Finite(0));
            (s1, r1)
        };
        assert_eq!(s1, s);
        assert_eq!(top1, top);
        assert_eq!(graph1, graph);
    }

    #[test]
    fn drop_eager() {
        let v0 = NodeRef::new(0);
        let v1 = NodeRef::new(1);
        let v2 = NodeRef::new(2);

        let mut s = stack!([v0, v1, v2], 6, 5, Finite(3));
        s.drop_eager(2).unwrap();
        let s1 = stack!([v0], 6, 5, Finite(3));
        assert_eq!(s1, s);

        let mut s = stack!([v0, v1, v2], 6, 5, Finite(3));
        s.drop_eager(5).unwrap();
        let s1 = stack!([], 10, 10, Finite(0));
        assert_eq!(s1, s);

        let mut s = stack!([], 6, 5, Finite(3));
        s.drop_eager(1).unwrap();
        let s1 = stack!([], 9, 9, Finite(0));
        assert_eq!(s1, s);

        let mut s = stack!([], 6, 5, Finite(3));
        s.drop_eager(2).unwrap();
        let s1 = stack!([], 10, 10, Finite(0));
        assert_eq!(s1, s);

        let mut s = stack!([v0, v1, v2], 6, 5, Overflow);
        assert_eq!(Err(UnderflowError::Normal), s.drop_eager(5));

        let mut s = stack!([v0, v1, v2], 6, 5, EmptyLit);
        assert_eq!(Err(UnderflowError::SlideEmpty), s.drop_eager(5));

        let mut s = stack!([v0], 5, 3, Finite(usize::MAX - 3));
        assert_eq!(Err(UnderflowError::Normal), s.drop_eager(5));
    }

    #[test]
    fn drop_lazy() {
        let v0 = NodeRef::new(0);
        let v1 = NodeRef::new(1);
        let v2 = NodeRef::new(2);

        let mut s = stack!([v0, v1, v2], 6, 5, Finite(3));
        s.drop_lazy(Finite(2));
        let s1 = stack!([v0], 6, 5, Finite(3));
        assert_eq!(s1, s);

        let mut s = stack!([v0, v1, v2], 6, 5, Finite(3));
        s.drop_lazy(Finite(5));
        let s1 = stack!([], 6, 6, Finite(4));
        assert_eq!(s1, s);

        let mut s = stack!([v0, v1, v2], 6, 5, Overflow);
        s.drop_lazy(Finite(5));
        let s1 = stack!([], 6, 6, Overflow);
        assert_eq!(s1, s);

        let mut s = stack!([v0, v1, v2], 6, 5, EmptyLit);
        s.drop_lazy(Finite(5));
        let s1 = stack!([], 6, 6, EmptyLit);
        assert_eq!(s1, s);

        let mut s = stack!([v0], 5, 3, Finite(usize::MAX - 2));
        s.drop_lazy(Finite(5));
        let s1 = stack!([], 5, 5, Finite(usize::MAX));
        assert_eq!(s1, s);

        let mut s = stack!([v0], 4, 3, Finite(usize::MAX - 2));
        s.drop_lazy(Finite(5));
        let s1 = stack!([], 4, 4, Overflow);
        assert_eq!(s1, s);
    }

    #[test]
    fn simplify_swap_swap() {
        let graph = unsafe { Graph::new() };
        let s = {
            let mut table = NodeTable::new(&graph);
            let mut s = stack!([], 0, 0, Finite(0));
            s.swap(&mut table).unwrap();
            s.swap(&mut table).unwrap();
            s.simplify(&table);
            s
        };
        let graph1 = unsafe { Graph::new() };
        let s1 = {
            graph1.push(Node::StackRef(0));
            graph1.push(Node::StackRef(1));
            stack!([], 2, 0, Finite(0))
        };
        assert_eq!(s1, s);
        assert_eq!(graph1, graph);
    }

    #[test]
    fn simplify_copy() {
        let graph = unsafe { Graph::new() };
        let s = {
            let mut table = NodeTable::new(&graph);
            let mut s = stack!([], 0, 0, Finite(0));
            let r1 = s.at_lazy(1, &mut table).unwrap();
            let r2 = s.at_lazy(2, &mut table).unwrap();
            let r0 = s.at_eager(0, &mut table).unwrap();
            s.drop_eager(3).unwrap();
            s.push(r2);
            s.push(r1);
            s.push(r0);
            s.simplify(&table);
            s
        };
        let graph1 = unsafe { Graph::new() };
        let s1 = {
            graph1.push(Node::CheckedStackRef(1));
            graph1.push(Node::CheckedStackRef(2));
            graph1.push(Node::StackRef(0));
            stack!([], 3, 0, Finite(0))
        };
        assert_eq!(s1, s);
        assert_eq!(graph1, graph);
    }
}

use crate::ast::NumberLit;
use crate::error::{NumberError, UnderflowError};
use crate::ir::{Inst, NodeRef, NodeTable};
use crate::number::Op;

/// Abstract stack for stack operations in a scope.
///
/// The Whitespace stack is manipulated with a mixture of strict and lazy
/// evaluation: operands are popped and results are pushed strictly; however,
/// `copy` accesses the value at its index lazily and `slide` drops under the
/// top lazily (but accesses the top eagerly). This means, that any errors from
/// `copy` or `slide`, such as an out-of-bounds index or a parse error, when the
/// AST operand is an empty integer literal, are encountered lazily.
/// `AbstractStack` models these semantics abstractly, so the rest of the
/// compiler can ignore stack invariants.
///
/// It is used in IR generation to convert all stack operations in a basic block
/// to operations on abstract registers. A basic block, then, loads and operates
/// on values from the incoming stack using fixed offsets. Stack underflow only
/// needs to be checked when lower values from the incoming stack are accessed.
/// At the end of the basic block, values are dropped and pushed in a batch.
#[derive(Debug, PartialEq, Eq)]
pub struct AbstractStack {
    values: Vec<NodeRef>,
    // guards[guards.len()-1].0 <= accessed, and is == until an underflow
    // occurs.
    guards: Vec<(usize, NodeRef)>,
    accessed: usize, // accessed >= dropped
    dropped: usize,
    lazy_dropped: LazySize,
}

/// Stack index or length.
///
// TODO: Haskell `Int` is not arbitrary precision, so the `Overflow` variant is
// not possible.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LazySize {
    Finite(usize),
    /// A stack index (for `copy`) or length (for `slide`) larger than
    /// `usize::MAX`, that always underflows when evaluated.
    Overflow,
    /// An empty number literal, that errors when evaluated.
    EmptyLit,
}

impl AbstractStack {
    /// Constructs an empty `AbstractStack`.
    #[inline]
    pub fn new() -> Self {
        AbstractStack {
            values: Vec::new(),
            guards: Vec::new(),
            accessed: 0,
            dropped: 0,
            lazy_dropped: LazySize::Finite(0),
        }
    }

    /// The values pushed to this stack frame.
    #[inline]
    pub fn values(&self) -> &[NodeRef] {
        &self.values
    }

    /// Guards for any values accessed from under this stack frame.
    #[inline]
    pub fn guards(&self) -> &[(usize, NodeRef)] {
        &self.guards
    }

    /// The depth of the values accessed from under this stack frame. Any
    /// access, excepting unconditional underflows, is guarded. `accessed` is
    /// always greater than or equal to `dropped`.
    #[inline]
    pub fn accessed(&self) -> usize {
        self.accessed
    }

    /// The number of values popped from under this stack frame.
    #[inline]
    pub fn dropped(&self) -> usize {
        self.dropped
    }

    /// The number of values, that will be popped from under this stack frame
    /// when the stack is next eagerly evaluated, or an error from the first
    /// erroneous `slide`.
    #[inline]
    pub fn lazy_dropped(&self) -> LazySize {
        self.lazy_dropped
    }

    /// Pushes a value to the stack.
    #[inline]
    pub fn push(&mut self, n: NodeRef) {
        self.values.push(n);
    }

    /// Pushes a number to the stack, only cloning it, if it does not already
    /// exist in the table. This corresponds to Whitespace `push`.
    #[inline]
    pub fn push_number(&mut self, n: &NumberLit, table: &mut NodeTable<'_>) -> NodeRef {
        let node = match n {
            NumberLit::Number(n) => table.insert_number(n),
            NumberLit::Empty => table.insert(NumberError::EmptyLit.into()),
        };
        self.push(node);
        node
    }

    /// Eagerly pushes a reference to the top element on the stack. This
    /// corresponds to Whitespace `dup`.
    #[inline]
    pub fn dup(&mut self, table: &mut NodeTable<'_>) -> Result<(), UnderflowError> {
        let top = self.top(table)?;
        self.push(top);
        Ok(())
    }

    /// Eagerly pushes a lazy reference to the nth element on the stack. This
    /// corresponds to Whitespace `copy`.
    #[inline]
    pub fn copy(&mut self, n: LazySize, table: &mut NodeTable<'_>) {
        let res = match n {
            LazySize::Finite(n) => match self.at_lazy(n, table) {
                Ok(node) => Ok(node),
                Err(UnderflowError::Normal) => Err(NumberError::CopyLarge),
                Err(UnderflowError::SlideEmpty) => Err(NumberError::EmptyLit),
            },
            LazySize::Overflow => Err(NumberError::CopyLarge),
            LazySize::EmptyLit => Err(NumberError::EmptyLit),
        };
        let node = match res {
            Ok(node) => node,
            Err(err) => table.insert(err.into()),
        };
        self.push(node);
    }

    /// Eagerly swaps the top two elements on the stack. This corresponds to
    /// Whitespace `swap`.
    #[inline]
    pub fn swap(&mut self, table: &mut NodeTable<'_>) -> Result<(), UnderflowError> {
        let (x, y) = self.pop2(table)?;
        self.push(y);
        self.push(x);
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
        self.at(n, true, table).map_err(|err| {
            self.accessed = usize::MAX;
            self.dropped = usize::MAX;
            self.lazy_dropped = LazySize::Finite(0);
            err
        })
    }

    /// Lazily accesses the nth element from the top of the stack and returns
    /// it.
    pub fn at_lazy(
        &mut self,
        n: usize,
        table: &mut NodeTable<'_>,
    ) -> Result<NodeRef, UnderflowError> {
        self.at(n, false, table)
    }

    #[inline]
    fn at(
        &mut self,
        n: usize,
        eager: bool,
        table: &mut NodeTable<'_>,
    ) -> Result<NodeRef, UnderflowError> {
        if n < self.values.len() {
            Ok(self.values[self.values.len() - n - 1])
        } else {
            let n = n - self.values.len();

            let lazy_dropped = self.lazy_dropped.finite()?;
            let dropped = add_or_underflow(self.dropped, lazy_dropped)?;
            let i = add_or_underflow(dropped, n)?;
            let len = add_or_underflow(i, 1)?;

            let inst = if eager {
                self.dropped = dropped;
                self.lazy_dropped = LazySize::Finite(0);
                let guard = self.get_or_insert_guard(len, table);
                Inst::StackRef(i, guard)
            } else {
                match self.get_guard(len) {
                    Some(guard) => Inst::StackRef(i, guard),
                    None => Inst::CheckedStackRef(i),
                }
            };
            Ok(table.insert(inst))
        }
    }

    /// Eagerly removes the top element from the stack and returns it. This
    /// corresponds to Whitespace `drop`, but with the dropped node returned.
    #[inline]
    pub fn pop(&mut self, table: &mut NodeTable<'_>) -> Result<NodeRef, UnderflowError> {
        let top = self.top(table)?;
        self.drop_eager(1, table)?;
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
        self.drop_eager(2, table)?;
        Ok((v1, v0))
    }

    /// Eagerly applies an arithmetic operation to the the top two elements on
    /// the stack. This corresponds to Whitespace `add`, `sub`, `mul`, `div`,
    /// and `mod`.
    #[inline]
    pub fn apply_op(&mut self, op: Op, table: &mut NodeTable<'_>) -> Result<(), UnderflowError> {
        let (x, y) = self.pop2(table)?;
        let inst = match op {
            Op::Add => Inst::Add(x, y),
            Op::Sub => Inst::Sub(x, y),
            // Mul has left-first evaluation order in Whitespace, unlike the
            // others, so the operands are swapped to make the IR consistent.
            Op::Mul => Inst::Mul(y, x),
            Op::Div => Inst::Div(x, y),
            Op::Mod => Inst::Mod(x, y),
        };
        self.push(table.insert_peephole(inst));
        Ok(())
    }

    /// Eagerly removes the top `n` elements from the stack. This corresponds to
    /// repeated Whitespace `drop`.
    #[inline]
    pub fn drop_eager(
        &mut self,
        n: usize,
        table: &mut NodeTable<'_>,
    ) -> Result<(), UnderflowError> {
        if n <= self.values.len() {
            self.values.truncate(self.values.len() - n);
        } else {
            let n = n - self.values.len();

            let lazy_dropped = self.lazy_dropped.finite()?;
            let dropped = add_or_underflow(self.dropped, lazy_dropped)?;
            let dropped = add_or_underflow(dropped, n)?;

            self.values.clear();
            self.dropped = dropped;
            self.lazy_dropped = LazySize::Finite(0);
            if self.accessed < dropped {
                self.accessed = dropped;
                let guard = table.insert_unique(Inst::GuardStack(dropped));
                self.guards.push((dropped, guard));
            }
        }
        Ok(())
    }

    /// Lazily removes the top `n` elements from the stack.
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
    /// topmost element. This corresponds to Whitespace `slide`.
    pub fn slide(&mut self, n: LazySize, table: &mut NodeTable<'_>) -> Result<(), UnderflowError> {
        let top = self.pop(table)?;
        self.drop_lazy(n);
        self.push(top);
        Ok(())
    }

    /// Simplifies pushed values, that do not change the contents of the stack.
    /// It should not be called in a loop, while modifying the stack, as that
    /// could potentially cause quadratic complexity; instead, call it after a
    /// batch of modifications, for linear complexity.
    pub fn simplify(&mut self, table: &NodeTable<'_>) {
        // Simplify pop-push identities
        if let Ok(drops) = (self.lazy_dropped.finite())
            .and_then(|lazy_drops| add_or_underflow(self.dropped, lazy_drops))
        {
            if drops <= self.accessed {
                let mut shift = 0;
                for &v in &self.values[0..drops.min(self.values.len())] {
                    let i = drops - (shift + 1);
                    match &*table[v] {
                        Inst::StackRef(j, _) | Inst::CheckedStackRef(j) if *j == i => {
                            shift += 1;
                        }
                        _ => break,
                    }
                }
                if shift != 0 {
                    self.values.drain(0..shift);
                    self.dropped -= shift;
                }
            }
        }
    }

    /// Gets the least [`Inst::GuardStack`] node, that guards at least a stack
    /// length of `len`, if that length has already been guarded.
    #[inline]
    pub fn get_guard(&self, len: usize) -> Option<NodeRef> {
        if len > self.accessed {
            // Short-circuit common case
            return None;
        }
        let i = match self.guards.binary_search_by(|(l, _)| l.cmp(&len)) {
            Ok(i) => i,
            Err(i) => i,
        };
        self.guards.get(i).map(|&(_, guard)| guard)
    }

    /// Gets the least [`Inst::GuardStack`] node, that guards at least a stack
    /// length of `len`, or insert a new guard for that length.
    #[inline]
    pub fn get_or_insert_guard(&mut self, len: usize, table: &mut NodeTable<'_>) -> NodeRef {
        match self.get_guard(len) {
            Some(guard) => guard,
            None => {
                self.accessed = self.accessed.max(len);
                let guard = table.insert_unique(Inst::GuardStack(len));
                self.guards.push((len, guard));
                guard
            }
        }
    }

    /// Resets all state of the stack, reusing memory.
    #[inline]
    pub fn clear(&mut self) {
        self.values.clear();
        self.guards.clear();
        self.accessed = 0;
        self.dropped = 0;
        self.lazy_dropped = LazySize::Finite(0);
    }
}

impl Clone for AbstractStack {
    #[inline]
    fn clone(&self) -> Self {
        AbstractStack {
            values: self.values.clone(),
            guards: self.guards.clone(),
            accessed: self.accessed,
            dropped: self.dropped,
            lazy_dropped: self.lazy_dropped,
        }
    }

    #[inline]
    fn clone_from(&mut self, source: &Self) {
        self.values.clone_from(&source.values);
        self.guards.clone_from(&source.guards);
        self.accessed = source.accessed;
        self.dropped = source.dropped;
        self.lazy_dropped = source.lazy_dropped;
    }
}

#[inline]
fn add_or_underflow(n: usize, m: usize) -> Result<usize, UnderflowError> {
    n.checked_add(m).ok_or(UnderflowError::Normal)
}

impl LazySize {
    /// Converts the number to a finite size, or underflows.
    #[inline]
    pub fn finite(&self) -> Result<usize, UnderflowError> {
        match self {
            LazySize::Finite(n) => Ok(*n),
            LazySize::Overflow => Err(UnderflowError::Normal),
            LazySize::EmptyLit => Err(UnderflowError::SlideEmpty),
        }
    }

    /// Adds this size to another size.
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

    macro_rules! stack(([$($value:expr),*], [$($guard:expr),*], $accessed:expr, $dropped:expr, $lazy_dropped:expr$(,)?) => {
        AbstractStack {
            values: vec![$($value),*],
            guards: vec![$($guard),*],
            accessed: $accessed,
            dropped: $dropped,
            lazy_dropped: $lazy_dropped,
        }
    });

    #[test]
    fn push() {
        let v0 = NodeRef::new(0);

        let mut s = stack!([], [], 0, 0, Finite(0));
        s.push(v0);
        let s1 = stack!([v0], [], 0, 0, Finite(0));
        assert_eq!(s1, s);
    }

    #[test]
    fn pop() {
        let graph = unsafe { Graph::new() };
        let (s, top) = {
            let mut table = NodeTable::new(&graph);
            let mut s = stack!([], [], 0, 0, Finite(0));
            let top = s.pop(&mut table).unwrap();
            (s, top)
        };
        let graph1 = unsafe { Graph::new() };
        let (s1, top1) = {
            let g1 = graph1.insert(Inst::GuardStack(1));
            let r1 = graph1.insert(Inst::StackRef(0, g1));
            let s1 = stack!([], [(1, g1)], 1, 1, Finite(0));
            (s1, r1)
        };
        assert_eq!(s1, s);
        assert_eq!(top1, top);
        assert_eq!(graph1, graph);

        let graph = unsafe { Graph::new() };
        let (s, top) = {
            let mut table = NodeTable::new(&graph);
            let v1 = table.insert(Inst::number(1));
            let v2 = table.insert(Inst::number(2));
            let mut s = stack!([v1, v2], [], 0, 0, Finite(0));
            let top = s.pop(&mut table).unwrap();
            (s, top)
        };
        let graph1 = unsafe { Graph::new() };
        let (s1, top1) = {
            let v1 = graph1.insert(Inst::number(1));
            let v2 = graph1.insert(Inst::number(2));
            let s1 = stack!([v1], [], 0, 0, Finite(0));
            (s1, v2)
        };
        assert_eq!(s1, s);
        assert_eq!(top1, top);
        assert_eq!(graph1, graph);

        let graph = unsafe { Graph::new() };
        let (s, top) = {
            let mut table = NodeTable::new(&graph);
            let g3 = table.insert(Inst::GuardStack(3));
            let mut s = stack!([], [(3, g3)], 3, 3, Finite(0));
            let top = s.pop(&mut table).unwrap();
            (s, top)
        };
        let graph1 = unsafe { Graph::new() };
        let (s1, top1) = {
            let g3 = graph1.insert(Inst::GuardStack(3));
            let g4 = graph1.insert(Inst::GuardStack(4));
            let r1 = graph1.insert(Inst::StackRef(3, g4));
            let s1 = stack!([], [(3, g3), (4, g4)], 4, 4, Finite(0));
            (s1, r1)
        };
        assert_eq!(s1, s);
        assert_eq!(top1, top);
        assert_eq!(graph1, graph);
    }

    #[test]
    fn drop_eager() {
        let graph_init = unsafe { Graph::new() };
        let mut table_init = NodeTable::new(&graph_init);
        let v1 = table_init.insert(Inst::number(1));
        let v2 = table_init.insert(Inst::number(2));
        let v3 = table_init.insert(Inst::number(3));
        let g2 = table_init.insert(Inst::GuardStack(2));

        {
            let graph = graph_init.clone();
            let mut table = unsafe { table_init.clone_with_graph(&graph) };
            let mut s = stack!([v1, v2, v3], [(2, g2)], 2, 1, Finite(3));
            s.drop_eager(2, &mut table).unwrap();

            let s1 = stack!([v1], [(2, g2)], 2, 1, Finite(3));

            assert_eq!(s1, s);
            assert_eq!(graph_init, graph);
        }
        {
            let graph = graph_init.clone();
            let mut table = unsafe { table_init.clone_with_graph(&graph) };
            let mut s = stack!([v1, v2, v3], [(2, g2)], 2, 1, Finite(3));
            s.drop_eager(5, &mut table).unwrap();

            let graph1 = graph_init.clone();
            let g6 = graph1.insert(Inst::GuardStack(6));
            let s1 = stack!([], [(2, g2), (6, g6)], 6, 6, Finite(0));

            assert_eq!(s1, s);
            assert_eq!(graph1, graph);
        }
        {
            let graph = graph_init.clone();
            let mut table = unsafe { table_init.clone_with_graph(&graph) };
            let mut s = stack!([], [(2, g2)], 2, 1, Finite(3));
            s.drop_eager(1, &mut table).unwrap();

            let graph1 = graph_init.clone();
            let g5 = graph1.insert(Inst::GuardStack(5));
            let s1 = stack!([], [(2, g2), (5, g5)], 5, 5, Finite(0));

            assert_eq!(s1, s);
            assert_eq!(graph1, graph);
        }
        {
            let graph = graph_init.clone();
            let mut table = unsafe { table_init.clone_with_graph(&graph) };
            let mut s = stack!([], [(2, g2)], 2, 1, Finite(3));
            s.drop_eager(2, &mut table).unwrap();

            let graph1 = graph_init.clone();
            let g6 = graph1.insert(Inst::GuardStack(6));
            let s1 = stack!([], [(2, g2), (6, g6)], 6, 6, Finite(0));

            assert_eq!(s1, s);
            assert_eq!(graph1, graph);
        }
        {
            let graph = graph_init.clone();
            let mut table = unsafe { table_init.clone_with_graph(&graph) };
            let mut s = stack!([v1, v2, v3], [(2, g2)], 2, 1, Overflow);
            let s1 = s.clone();

            assert_eq!(Err(UnderflowError::Normal), s.drop_eager(5, &mut table));
            assert_eq!(s1, s);
            assert_eq!(graph_init, graph);
        }
        {
            let graph = graph_init.clone();
            let mut table = unsafe { table_init.clone_with_graph(&graph) };
            let mut s = stack!([v1, v2, v3], [(2, g2)], 2, 1, EmptyLit);
            let s1 = s.clone();

            assert_eq!(Err(UnderflowError::SlideEmpty), s.drop_eager(5, &mut table));
            assert_eq!(s1, s);
            assert_eq!(graph_init, graph);
        }
        {
            let graph = graph_init.clone();
            let mut table = unsafe { table_init.clone_with_graph(&graph) };
            let mut s = stack!([v1, v2, v3], [(2, g2)], 2, 1, Finite(usize::MAX - 3));
            s.drop_eager(5, &mut table).unwrap();

            let graph1 = graph_init.clone();
            let gmax = graph1.insert(Inst::GuardStack(usize::MAX));
            let s1 = stack!(
                [],
                [(2, g2), (usize::MAX, gmax)],
                usize::MAX,
                usize::MAX,
                Finite(0)
            );

            assert_eq!(s1, s);
            assert_eq!(graph1, graph);
        }
        {
            let graph = graph_init.clone();
            let mut table = unsafe { table_init.clone_with_graph(&graph) };
            let mut s = stack!([v1, v2], [(2, g2)], 2, 1, Finite(usize::MAX - 3));
            let s1 = s.clone();

            assert_eq!(Err(UnderflowError::Normal), s.drop_eager(5, &mut table));
            assert_eq!(s1, s);
            assert_eq!(graph_init, graph);
        }
    }

    #[test]
    fn drop_lazy() {
        let graph = unsafe { Graph::new() };
        let v1 = graph.insert(Inst::number(1));
        let v2 = graph.insert(Inst::number(2));
        let v3 = graph.insert(Inst::number(3));
        let g2 = graph.insert(Inst::GuardStack(2));

        {
            let mut s = stack!([v1, v2, v3], [(2, g2)], 2, 1, Finite(3));
            s.drop_lazy(Finite(2));
            let s1 = stack!([v1], [(2, g2)], 2, 1, Finite(3));
            assert_eq!(s1, s);
        }
        {
            let mut s = stack!([v1, v2, v3], [(2, g2)], 2, 1, Finite(3));
            s.drop_lazy(Finite(5));
            let s1 = stack!([], [(2, g2)], 2, 2, Finite(4));
            assert_eq!(s1, s);
        }
        {
            let mut s = stack!([v1, v2, v3], [(2, g2)], 2, 1, Overflow);
            s.drop_lazy(Finite(5));
            let s1 = stack!([], [(2, g2)], 2, 2, Overflow);
            assert_eq!(s1, s);
        }
        {
            let mut s = stack!([v1, v2, v3], [(2, g2)], 2, 1, EmptyLit);
            s.drop_lazy(Finite(5));
            let s1 = stack!([], [(2, g2)], 2, 2, EmptyLit);
            assert_eq!(s1, s);
        }
        {
            let mut s = stack!([v1], [(2, g2)], 2, 1, Finite(usize::MAX - 3));
            s.drop_lazy(Finite(5));
            let s1 = stack!([], [(2, g2)], 2, 2, Finite(usize::MAX));
            assert_eq!(s1, s);
        }
        {
            let mut s = stack!([], [(2, g2)], 2, 1, Finite(usize::MAX - 3));
            s.drop_lazy(Finite(5));
            let s1 = stack!([], [(2, g2)], 2, 2, Overflow);
            assert_eq!(s1, s);
        }
    }

    #[test]
    fn simplify_swap_swap() {
        let graph = unsafe { Graph::new() };
        let s = {
            let mut table = NodeTable::new(&graph);
            let mut s = stack!([], [], 0, 0, Finite(0));
            s.swap(&mut table).unwrap();
            s.swap(&mut table).unwrap();
            s.simplify(&table);
            s
        };
        let graph1 = unsafe { Graph::new() };
        let s1 = {
            let g2 = graph1.insert(Inst::GuardStack(2));
            graph1.insert(Inst::StackRef(1, g2));
            graph1.insert(Inst::StackRef(0, g2));
            stack!([], [(2, g2)], 2, 0, Finite(0))
        };
        assert_eq!(s1, s);
        assert_eq!(graph1, graph);
    }

    #[test]
    fn simplify_copy() {
        let graph = unsafe { Graph::new() };
        let s = {
            let mut table = NodeTable::new(&graph);
            let mut s = stack!([], [], 0, 0, Finite(0));
            let r1 = s.at_lazy(1, &mut table).unwrap();
            let r2 = s.at_lazy(2, &mut table).unwrap();
            let r0 = s.at_eager(0, &mut table).unwrap();
            s.drop_eager(3, &mut table).unwrap();
            s.push(r2);
            s.push(r1);
            s.push(r0);
            s.simplify(&table);
            s
        };
        let graph1 = unsafe { Graph::new() };
        let s1 = {
            graph1.insert(Inst::CheckedStackRef(1));
            graph1.insert(Inst::CheckedStackRef(2));
            let g1 = graph1.insert(Inst::GuardStack(1));
            graph1.insert(Inst::StackRef(0, g1));
            let g3 = graph1.insert(Inst::GuardStack(3));
            stack!([], [(1, g1), (3, g3)], 3, 0, Finite(0))
        };
        assert_eq!(s1, s);
        assert_eq!(graph1, graph);
    }
}

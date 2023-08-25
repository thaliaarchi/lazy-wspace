use std::cell::UnsafeCell;
use std::fmt::{self, Debug, Display, Formatter};
use std::ops::Index;
use std::vec;

use static_assertions::assert_not_impl_any;

use crate::ir::Node;

/// Graph of IR nodes, indexed by [`NodeRef`].
#[repr(transparent)]
pub struct Graph {
    nodes: UnsafeCell<Vec<Node>>,
}

/// Reference to a [`Node`] in a [`Graph`].
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeRef {
    index: u32,
}

assert_not_impl_any!(Graph: Send, Sync);

impl Graph {
    /// Construct a graph.
    ///
    /// # Safety
    ///
    /// Any `NodeRef` stored in or used to index this graph must belong to this
    /// same graph. It uses uses unchecked indexing and has undefined behavior
    /// when passed a `NodeRef` from another graph.
    #[inline]
    pub const unsafe fn new() -> Self {
        Graph {
            nodes: UnsafeCell::new(Vec::new()),
        }
    }

    #[inline]
    pub fn push(&self, node: Node) -> NodeRef {
        // SAFETY: Graph is !Sync and any references are by slice.
        let nodes = unsafe { &mut *self.nodes.get() };
        let i = nodes.len();
        assert!(i as u32 != u32::MAX, "number of nodes exceeds u32");
        nodes.push(node);
        NodeRef::new(i)
    }

    #[inline]
    pub fn nodes(&self) -> &[Node] {
        // SAFETY: The length is monotonically increasing and nodes cannot be
        // modified once pushed.
        unsafe { &**self.nodes.get() }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.nodes().len()
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &Node> {
        self.nodes().iter()
    }

    #[inline]
    pub fn iter_refs(&self) -> impl Iterator<Item = NodeRef> {
        (0..self.len()).map(NodeRef::new)
    }

    #[inline]
    pub fn iter_entries(&self) -> impl Iterator<Item = (NodeRef, &Node)> {
        self.nodes()
            .iter()
            .enumerate()
            .map(|(i, node)| (NodeRef::new(i), node))
    }
}

impl Index<NodeRef> for Graph {
    type Output = Node;

    #[inline]
    fn index(&self, index: NodeRef) -> &Node {
        debug_assert!(index.index() < self.len());

        // SAFETY: The pool length is monotonically increasing, so the index
        // will always be in bounds, as long as the index was created by this
        // pool.
        //
        // Branding `NodeRef` with a lifetime like the [`BrandedVec`](https://matyama.github.io/rust-examples/rust_examples/brands/index.html)
        // technique from [“GhostCell: Separating Permissions from Data in Rust”](https://plv.mpi-sws.org/rustbelt/ghostcell/)
        // (Yanovski et al., 2021) imposes heavy API restrictions, and, since
        // only one `Graph` is constructed per program, this cost is not worth
        // it.
        unsafe { self.nodes().get_unchecked(index.index()) }
    }
}

impl Clone for Graph {
    #[inline]
    fn clone(&self) -> Self {
        // SAFETY: Graph is !Sync.
        let nodes = unsafe { &*self.nodes.get() };
        Graph {
            nodes: UnsafeCell::new(nodes.clone()),
        }
    }

    #[inline]
    fn clone_from(&mut self, source: &Self) {
        // SAFETY: Graph is !Sync.
        let other = unsafe { &*source.nodes.get() };
        self.nodes.get_mut().clone_from(other);
    }
}

impl PartialEq for Graph {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.nodes() == other.nodes()
    }
}

impl Eq for Graph {}

impl IntoIterator for Graph {
    type Item = Node;
    type IntoIter = vec::IntoIter<Node>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.nodes.into_inner().into_iter()
    }
}

impl Debug for Graph {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("Graph ")?;
        if f.alternate() {
            struct DisplayDebug<T: Display>(T);
            impl<T: Display> Debug for DisplayDebug<T> {
                fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                    Display::fmt(&self.0, f)
                }
            }

            let entries = self
                .iter_entries()
                .map(|(i, node)| (DisplayDebug(i), DisplayDebug(node)));
            f.debug_map().entries(entries).finish()
        } else {
            f.debug_map().entries(self.iter().enumerate()).finish()
        }
    }
}

impl NodeRef {
    #[inline]
    pub(crate) const fn new(index: usize) -> Self {
        NodeRef {
            index: index as u32,
        }
    }

    #[inline]
    pub const fn index(&self) -> usize {
        self.index as usize
    }
}

impl Debug for NodeRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("NodeRef").field(&self.index).finish()
    }
}

impl Display for NodeRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.index)
    }
}
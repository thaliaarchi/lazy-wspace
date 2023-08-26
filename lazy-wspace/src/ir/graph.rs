use std::cell::UnsafeCell;
use std::fmt::{self, Debug, Display, Formatter};
use std::marker::PhantomData;
use std::ops::{Deref, Index};
use std::rc::Rc;
use std::vec;

use smallvec::SmallVec;
use static_assertions::{assert_eq_size, assert_not_impl_any};

use crate::ir::{Inst, InstNoRef, InstOp1, InstOp2, InstOp2U32};

/// Graph of IR nodes, indexed by [`NodeRef`].
#[repr(transparent)]
pub struct Graph {
    nodes: UnsafeCell<Vec<Node>>,
    // Mark as !Sync
    marker: PhantomData<Rc<Node>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Node {
    inst: Inst,
    def_uses: SmallVec<[NodeRef; 4]>,
}

/// Reference to a [`Node`] in a [`Graph`].
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeRef {
    index: u32,
}

assert_not_impl_any!(Graph: Send, Sync);
assert_eq_size!(SmallVec<[NodeRef; 4]>, Vec<NodeRef>);

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
            marker: PhantomData,
        }
    }

    #[inline]
    pub fn insert(&self, inst: Inst) -> NodeRef {
        // SAFETY: Graph is !Sync and any references are by slice.
        let nodes = unsafe { &mut *self.nodes.get() };
        let i = nodes.len();
        let node = NodeRef::new(i);
        assert!(i as u32 != u32::MAX, "number of nodes exceeds u32");

        match &inst {
            InstOp2!(lhs, rhs) => {
                nodes[lhs.index()].def_uses.push(node);
                nodes[rhs.index()].def_uses.push(node);
            }
            InstOp2U32!(v, _) | InstOp1!(v) | Inst::HeapRef(v) => {
                nodes[v.index()].def_uses.push(node);
            }
            InstNoRef!() => {}
        }

        nodes.push(Node::new(inst));
        node
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
    pub fn iter_insts(&self) -> impl Iterator<Item = &Inst> {
        self.nodes().iter().map(Node::inst)
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
            marker: PhantomData,
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
                .map(|(i, node)| (DisplayDebug(i), DisplayDebug(node.inst())));
            f.debug_map().entries(entries).finish()
        } else {
            let entries = self.iter_insts().enumerate();
            f.debug_map().entries(entries).finish()
        }
    }
}

impl Node {
    #[inline]
    fn new(inst: Inst) -> Self {
        Node {
            inst,
            def_uses: SmallVec::new(),
        }
    }

    #[inline]
    pub fn inst(&self) -> &Inst {
        &self.inst
    }

    #[inline]
    pub fn def_uses(&self) -> &[NodeRef] {
        &self.def_uses
    }
}

impl Deref for Node {
    type Target = Inst;

    #[inline]
    fn deref(&self) -> &Inst {
        &self.inst
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

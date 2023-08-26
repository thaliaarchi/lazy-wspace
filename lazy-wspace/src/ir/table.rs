use std::fmt::{self, Debug, Formatter};
use std::hash::{BuildHasher, Hash, Hasher};
use std::iter::FusedIterator;
use std::marker::PhantomData;
use std::mem;
use std::ops::Index;
use std::ptr::NonNull;

use hashbrown::hash_map::DefaultHashBuilder;
use hashbrown::raw::{RawIter, RawTable};
use rug::Integer;

use crate::ir::{Graph, Inst, Node, NodeRef};

pub struct NodeTable<'g> {
    // Essentially a `HashMap<Node, NodeRef>`, that doesn't store a redundant
    // `Node` key, instead referencing it in `graph` using the `NodeRef` value.
    table: RawTable<NodeRef>,
    graph: &'g Graph,
}

#[derive(Clone)]
pub struct NodeTableIter<'a> {
    inner: RawIter<NodeRef>,
    marker: PhantomData<&'a NodeRef>,
}

impl<'g> NodeTable<'g> {
    #[inline]
    pub fn new(graph: &'g Graph) -> Self {
        NodeTable {
            table: RawTable::new(),
            graph,
        }
    }

    #[inline]
    pub fn get(&self, inst: &Inst) -> Option<NodeRef> {
        let hash = make_hash(inst);
        match self.table.find(hash, |&key| &*self.graph[key] == inst) {
            Some(bucket) => Some(*unsafe { bucket.as_ref() }),
            None => None,
        }
    }

    #[inline]
    pub fn insert(&mut self, inst: Inst) -> NodeRef {
        let hash = make_hash(&inst);
        match self.table.find_or_find_insert_slot(
            hash,
            |&key| *self.graph[key] == inst,
            |&key| make_hash(&*self.graph[key]),
        ) {
            Ok(bucket) => *unsafe { bucket.as_ref() },
            Err(slot) => {
                let node = self.graph.insert(inst);
                unsafe { self.table.insert_in_slot(hash, slot, node) };
                node
            }
        }
    }

    /// A specialization of `insert`, that avoids cloning `n` and constructing
    /// an `Inst::Number`, when an equivalent number has already been inserted.
    #[inline]
    pub fn insert_number(&mut self, n: &Integer) -> NodeRef {
        struct NodeNumberRef<'a>(&'a Integer);
        impl Hash for NodeNumberRef<'_> {
            #[inline]
            fn hash<H: Hasher>(&self, state: &mut H) {
                let inst = unsafe { Inst::Number(Box::from_raw(NonNull::dangling().as_mut())) };
                mem::discriminant(&inst).hash(state);
                mem::forget(inst);
                self.0.hash(state);
            }
        }

        let hash = make_hash(&NodeNumberRef(n));
        match self.table.find_or_find_insert_slot(
            hash,
            |&key| &*self.graph[key] == n,
            |&key| make_hash(&*self.graph[key]),
        ) {
            Ok(bucket) => *unsafe { bucket.as_ref() },
            Err(slot) => {
                let node = self.graph.insert(Inst::Number(Box::new(n.clone())));
                unsafe { self.table.insert_in_slot(hash, slot, node) };
                node
            }
        }
    }

    #[inline]
    pub fn insert_unique(&mut self, inst: Inst) -> NodeRef {
        self.graph.insert(inst)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.table.len()
    }

    #[inline]
    pub fn iter(&self) -> NodeTableIter<'_> {
        NodeTableIter {
            inner: unsafe { self.table.iter() },
            marker: PhantomData,
        }
    }

    #[inline]
    pub fn graph(&self) -> &'g Graph {
        self.graph
    }
}

#[inline]
fn make_hash<T: Hash>(v: &T) -> u64 {
    let mut state = DefaultHashBuilder::default().build_hasher();
    v.hash(&mut state);
    state.finish()
}

impl Index<NodeRef> for NodeTable<'_> {
    type Output = Node;

    #[inline]
    fn index(&self, index: NodeRef) -> &Node {
        &self.graph[index]
    }
}

impl Clone for NodeTable<'_> {
    #[inline]
    fn clone(&self) -> Self {
        NodeTable {
            table: self.table.clone(),
            graph: self.graph,
        }
    }

    #[inline]
    fn clone_from(&mut self, source: &Self) {
        self.table.clone_from(&source.table);
        self.graph = source.graph;
    }
}

impl Debug for NodeTable<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("NodeTable ")?;
        f.debug_set().entries(self.iter()).finish()
    }
}

impl Iterator for NodeTableIter<'_> {
    type Item = NodeRef;

    #[inline]
    fn next(&mut self) -> Option<NodeRef> {
        self.inner.next().map(|bucket| *unsafe { bucket.as_ref() })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl ExactSizeIterator for NodeTableIter<'_> {
    #[inline]
    fn len(&self) -> usize {
        self.inner.len()
    }
}

impl FusedIterator for NodeTableIter<'_> {}

impl Debug for NodeTableIter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unique() {
        let graph = unsafe { Graph::new() };
        let mut table = NodeTable::new(&graph);
        let x = table.insert(Inst::number(1));
        let y = table.insert(Inst::number(2));
        let z = table.insert(Inst::Add(x, y));
        let y2 = table.insert(Inst::number(2));
        let x2 = table.insert(Inst::number(1));
        let z2 = table.insert(Inst::Add(x2, y2));
        assert_eq!(3, table.len());
        assert_eq!(x, x2);
        assert_eq!(y, y2);
        assert_eq!(z, z2);
    }
}

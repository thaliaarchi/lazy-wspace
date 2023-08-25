use std::fmt::{self, Debug, Formatter};
use std::hash::{BuildHasher, Hash, Hasher};
use std::iter::FusedIterator;
use std::marker::PhantomData;
use std::ops::Index;

use hashbrown::hash_map::DefaultHashBuilder;
use hashbrown::raw::{RawIter, RawTable};

use crate::ir::{Graph, Node, NodeRef};

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
    pub fn get(&self, node: &Node) -> Option<NodeRef> {
        let hash = hash_node(node);
        match self.table.find(hash, |&key| &self.graph[key] == node) {
            Some(bucket) => Some(*unsafe { bucket.as_ref() }),
            None => None,
        }
    }

    #[inline]
    pub fn insert(&mut self, node: Node) -> NodeRef {
        let hash = hash_node(&node);
        match self.table.find_or_find_insert_slot(
            hash,
            |&key| self.graph[key] == node,
            |&key| hash_node(&self.graph[key]),
        ) {
            Ok(bucket) => *unsafe { bucket.as_ref() },
            Err(slot) => {
                let i = self.graph.push(node);
                unsafe { self.table.insert_in_slot(hash, slot, i) };
                i
            }
        }
    }

    #[inline]
    pub fn insert_unique(&mut self, node: Node) -> NodeRef {
        self.graph.push(node)
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
fn hash_node(node: &Node) -> u64 {
    let mut state = DefaultHashBuilder::default().build_hasher();
    node.hash(&mut state);
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
        let x = table.insert(Node::number(1));
        let y = table.insert(Node::number(2));
        let z = table.insert(Node::Add(x, y));
        let y2 = table.insert(Node::number(2));
        let x2 = table.insert(Node::number(1));
        let z2 = table.insert(Node::Add(x2, y2));
        assert_eq!(3, table.len());
        assert_eq!(x, x2);
        assert_eq!(y, y2);
        assert_eq!(z, z2);
    }
}

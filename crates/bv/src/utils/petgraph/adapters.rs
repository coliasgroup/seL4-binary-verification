use std::collections::HashSet;
use std::fmt;
use std::iter;

use arrayvec::ArrayVec;
use petgraph::visit::{
    Data, EdgeRef, GraphBase, GraphProp, IntoEdgeReferences, IntoNeighbors, IntoNodeIdentifiers,
    IntoNodeReferences, NodeIndexable, NodeRef, Visitable,
};
use petgraph::Directed;

use crate::abstract_syntax::{Node, NodeAddr, NodeId};
use crate::graph::HasNodeGraph;
use crate::graph::HasNodeGraphWithNodeAddrBound;
use crate::graph::NodeGraph;

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct EdgeId {
    source: NodeId,
    target: NodeId,
    condition: EdgeCondition,
}

impl EdgeId {
    pub(crate) fn new(source: NodeId, target: NodeId, condition: EdgeCondition) -> Self {
        Self {
            source,
            target,
            condition,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct EdgeCondition(Option<bool>);

impl<T: HasNodeGraph> GraphBase for NodeGraph<T> {
    type EdgeId = EdgeId;
    type NodeId = NodeId;
}

pub(crate) struct GraphNodeRef<'a, T> {
    id: NodeId,
    inner: &'a NodeGraph<T>,
}

impl<'a, T> Clone for GraphNodeRef<'a, T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            inner: self.inner,
        }
    }
}

impl<'a, T> Copy for GraphNodeRef<'a, T> {}

impl<'a, T: HasNodeGraph> NodeRef for GraphNodeRef<'a, T> {
    type NodeId = NodeId;
    type Weight = Self;

    fn id(&self) -> Self::NodeId {
        self.id
    }

    fn weight(&self) -> &Self::Weight {
        self
    }
}

impl<'a, T: HasNodeFormatter<T>> fmt::Display for GraphNodeRef<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.inner().fmt_node(self, f)
    }
}

pub(crate) struct GraphEdgeRef<'a, T> {
    id: EdgeId,
    inner: &'a NodeGraph<T>,
}

impl<'a, T> Clone for GraphEdgeRef<'a, T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            inner: self.inner,
        }
    }
}

impl<'a, T> Copy for GraphEdgeRef<'a, T> {}

impl<'a, T> EdgeRef for GraphEdgeRef<'a, T> {
    type NodeId = NodeId;
    type EdgeId = EdgeId;
    type Weight = Self;

    fn source(&self) -> Self::NodeId {
        self.id.source
    }

    fn target(&self) -> Self::NodeId {
        self.id.target
    }

    fn weight(&self) -> &Self::Weight {
        self
    }

    fn id(&self) -> Self::EdgeId {
        self.id
    }
}

impl<'a, T: HasEdgeFormatter<T>> fmt::Display for GraphEdgeRef<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.inner().fmt_edge(self, f)
    }
}

impl<'a, T: HasNodeGraph> Data for &'a NodeGraph<T> {
    type NodeWeight = GraphNodeRef<'a, T>;
    type EdgeWeight = GraphEdgeRef<'a, T>;
}

impl<'a, T: HasNodeGraph> IntoNodeIdentifiers for &'a NodeGraph<T> {
    type NodeIdentifiers = iter::Chain<
        std::array::IntoIter<NodeId, 2>,
        iter::Map<T::NodesForGraph<'a>, fn((NodeAddr, &'a Node)) -> NodeId>,
    >;

    fn node_identifiers(self) -> Self::NodeIdentifiers {
        [NodeId::Ret, NodeId::Err].into_iter().chain(
            self.inner()
                .graph_nodes()
                .map((|(addr, _node)| NodeId::Addr(addr)) as _),
        )
    }
}

impl<'a, T: HasNodeGraph> IntoNodeReferences for &'a NodeGraph<T> {
    type NodeRef = GraphNodeRef<'a, T>;
    type NodeReferences = NodeReferences<'a, T, Self::NodeIdentifiers>;

    fn node_references(self) -> Self::NodeReferences {
        NodeReferences {
            inner: self,
            it: self.node_identifiers(),
        }
    }
}

pub(crate) struct NodeReferences<'a, T, I> {
    inner: &'a NodeGraph<T>,
    it: I,
}

impl<'a, T, I: Iterator<Item = NodeId>> Iterator for NodeReferences<'a, T, I> {
    type Item = GraphNodeRef<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|id| GraphNodeRef {
            id,
            inner: self.inner,
        })
    }
}

impl<'a, T: HasNodeGraph> IntoEdgeReferences for &'a NodeGraph<T> {
    type EdgeRef = GraphEdgeRef<'a, T>;
    type EdgeReferences = EdgeReferences<
        'a,
        T,
        iter::FlatMap<
            T::NodesForGraph<'a>,
            ArrayVec<EdgeId, 2>,
            fn((NodeAddr, &Node)) -> ArrayVec<EdgeId, 2>,
        >,
    >;

    fn edge_references(self) -> Self::EdgeReferences {
        EdgeReferences {
            inner: self,
            it: self.graph_nodes().flat_map(|(addr, node)| {
                let node_id = NodeId::Addr(addr);
                let mut ret = ArrayVec::new();
                match node {
                    Node::Basic(basic) => {
                        ret.push(EdgeId::new(node_id, basic.next, EdgeCondition(None)))
                    }
                    Node::Cond(cond) => {
                        ret.push(EdgeId::new(node_id, cond.left, EdgeCondition(Some(true))));
                        ret.push(EdgeId::new(node_id, cond.right, EdgeCondition(Some(false))));
                    }
                    Node::Call(call) => {
                        ret.push(EdgeId::new(node_id, call.next, EdgeCondition(None)))
                    }
                }
                ret
            }),
        }
    }
}

pub(crate) struct EdgeReferences<'a, T, I> {
    inner: &'a NodeGraph<T>,
    it: I,
}

impl<'a, T, I: Iterator<Item = EdgeId>> Iterator for EdgeReferences<'a, T, I> {
    type Item = GraphEdgeRef<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|id| GraphEdgeRef {
            id,
            inner: self.inner,
        })
    }
}

impl<'a, T: HasNodeGraph> IntoNeighbors for &'a NodeGraph<T> {
    type Neighbors = arrayvec::IntoIter<NodeId, 2>;

    fn neighbors(self, a: Self::NodeId) -> Self::Neighbors {
        if let NodeId::Addr(addr) = a {
            self.graph_node(addr).conts()
        } else {
            ArrayVec::new()
        }
        .into_iter()
    }
}

impl<T: HasNodeGraphWithNodeAddrBound> NodeIndexable for NodeGraph<T> {
    fn node_bound(&self) -> usize {
        usize::try_from(self.node_addr_bound()).unwrap() + 2
    }

    fn to_index(&self, a: Self::NodeId) -> usize {
        match a {
            NodeId::Ret => 0,
            NodeId::Err => 1,
            NodeId::Addr(addr) => usize::try_from(addr + 2).unwrap(),
        }
    }

    fn from_index(&self, i: usize) -> Self::NodeId {
        match i {
            0 => NodeId::Ret,
            1 => NodeId::Err,
            _ => NodeId::Addr((i - 2).try_into().unwrap()),
        }
    }
}

// TODO use FixedBitSet for NodeMap case?
impl<T: HasNodeGraph> Visitable for NodeGraph<T> {
    type Map = HashSet<NodeId>;

    fn visit_map(&self) -> Self::Map {
        HashSet::new()
    }

    fn reset_map(&self, map: &mut Self::Map) {
        map.clear();
    }
}

impl<T: HasNodeGraph> GraphProp for NodeGraph<T> {
    type EdgeType = Directed;
}

// // //

pub(crate) trait HasNodeFormatter<T> {
    fn fmt_node<'a>(&self, node: &GraphNodeRef<'a, T>, f: &mut fmt::Formatter) -> fmt::Result;
}

pub(crate) trait HasEdgeFormatter<T> {
    fn fmt_edge<'a>(&self, edge: &GraphEdgeRef<'a, T>, f: &mut fmt::Formatter) -> fmt::Result;
}

impl<T, D: HasNodeFormatter<T>> HasNodeFormatter<T> for &D {
    fn fmt_node<'a>(&self, node: &GraphNodeRef<'a, T>, f: &mut fmt::Formatter) -> fmt::Result {
        D::fmt_node(self, node, f)
    }
}

impl<T, D: HasEdgeFormatter<T>> HasEdgeFormatter<T> for &D {
    fn fmt_edge<'a>(&self, node: &GraphEdgeRef<'a, T>, f: &mut fmt::Formatter) -> fmt::Result {
        D::fmt_edge(self, node, f)
    }
}

pub(crate) struct NodeGraphFormatterWrapper<T, D = SimpleFormatter> {
    inner: T,
    formatter: D,
}

impl<T> NodeGraphFormatterWrapper<T> {
    pub(crate) fn new(inner: T) -> Self {
        Self {
            inner,
            formatter: Default::default(),
        }
    }
}

impl<T, D> NodeGraphFormatterWrapper<T, D> {
    pub(crate) fn new_with_formatter(inner: T, formatter: D) -> Self {
        Self { inner, formatter }
    }
}

impl<T, U, D: HasNodeFormatter<T>> HasNodeFormatter<T> for NodeGraphFormatterWrapper<U, D> {
    fn fmt_node<'a>(&self, node: &GraphNodeRef<'a, T>, f: &mut fmt::Formatter) -> fmt::Result {
        self.formatter.fmt_node(node, f)
    }
}

impl<T, U, D: HasEdgeFormatter<T>> HasEdgeFormatter<T> for NodeGraphFormatterWrapper<U, D> {
    fn fmt_edge<'a>(&self, node: &GraphEdgeRef<'a, T>, f: &mut fmt::Formatter) -> fmt::Result {
        self.formatter.fmt_edge(node, f)
    }
}

impl<T: HasNodeGraph, D> HasNodeGraph for NodeGraphFormatterWrapper<T, D> {
    type NodesForGraph<'b> = T::NodesForGraph<'b> where Self: 'b;

    fn graph_node(&self, addr: NodeAddr) -> &Node {
        T::graph_node(&self.inner, addr)
    }

    fn graph_nodes(&self) -> Self::NodesForGraph<'_> {
        T::graph_nodes(&self.inner)
    }
}

impl<'a, T: HasNodeGraphWithNodeAddrBound, D> HasNodeGraphWithNodeAddrBound
    for NodeGraphFormatterWrapper<T, D>
{
    fn node_addr_bound(&self) -> NodeAddr {
        T::node_addr_bound(&self.inner)
    }
}

#[derive(Default, Copy, Clone)]
pub(crate) struct SimpleFormatter(());

impl<T> HasNodeFormatter<T> for SimpleFormatter {
    fn fmt_node<'a>(&self, node: &GraphNodeRef<'a, T>, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", node.id)
    }
}

impl<T> HasEdgeFormatter<T> for SimpleFormatter {
    fn fmt_edge<'a>(&self, edge: &GraphEdgeRef<'a, T>, f: &mut fmt::Formatter) -> fmt::Result {
        match edge.id.condition.0 {
            None => write!(f, ""),
            Some(b) => match b {
                true => write!(f, "T"),
                false => write!(f, "F"),
            },
        }
    }
}

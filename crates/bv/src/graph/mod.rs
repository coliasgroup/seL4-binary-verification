use std::collections::HashSet;
use std::fmt;
use std::iter;

use arrayvec::ArrayVec;
use petgraph::visit::{
    Data, EdgeRef, GraphBase, GraphProp, IntoEdgeReferences, IntoNeighbors, IntoNodeIdentifiers,
    IntoNodeReferences, NodeIndexable, NodeRef, Visitable,
};
use petgraph::Directed;

use crate::syntax::{Node, NodeAddr, NodeId};

pub(crate) mod algo;

#[derive(Copy, Clone)]
pub(crate) struct NodeGraph<T, D = SimpleFormatter> {
    inner: T,
    formatter: D,
}

impl<T> NodeGraph<T> {
    pub(crate) fn new(inner: T) -> Self {
        Self {
            inner,
            formatter: Default::default(),
        }
    }
}

impl<T, D> NodeGraph<T, D> {
    pub(crate) fn new_with_formatter(inner: T, formatter: D) -> Self {
        Self { inner, formatter }
    }

    pub(crate) fn inner(&self) -> &T {
        &self.inner
    }

    pub(crate) fn formatter(&self) -> &D {
        &self.formatter
    }
}

pub(crate) trait HasNodeGraph {
    type NodesForGraph<'a>: Iterator<Item = (NodeAddr, &'a Node)>
    where
        Self: 'a;

    fn graph_node(&self, addr: NodeAddr) -> &Node;

    fn graph_nodes(&self) -> Self::NodesForGraph<'_>;

    fn node_graph(&self) -> NodeGraph<&Self> {
        NodeGraph::new(self)
    }
}

impl<'a, T: HasNodeGraph> HasNodeGraph for &'a T {
    type NodesForGraph<'b> = T::NodesForGraph<'b> where Self: 'b;

    fn graph_node(&self, addr: NodeAddr) -> &Node {
        T::graph_node(self, addr)
    }

    fn graph_nodes(&self) -> Self::NodesForGraph<'_> {
        T::graph_nodes(self)
    }
}

impl<T: HasNodeGraph, D> HasNodeGraph for NodeGraph<T, D> {
    type NodesForGraph<'a> = T::NodesForGraph<'a> where Self: 'a;

    fn graph_node(&self, addr: NodeAddr) -> &Node {
        self.inner().graph_node(addr)
    }

    fn graph_nodes(&self) -> Self::NodesForGraph<'_> {
        self.inner().graph_nodes()
    }
}

pub(crate) trait HasNodeGraphWithNodeAddrBound: HasNodeGraph {
    fn node_addr_bound(&self) -> NodeAddr;
}

impl<'a, T: HasNodeGraphWithNodeAddrBound> HasNodeGraphWithNodeAddrBound for &'a T {
    fn node_addr_bound(&self) -> NodeAddr {
        T::node_addr_bound(self)
    }
}

impl<T: HasNodeGraphWithNodeAddrBound, D> HasNodeGraphWithNodeAddrBound for NodeGraph<T, D> {
    fn node_addr_bound(&self) -> NodeAddr {
        self.inner.node_addr_bound()
    }
}

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

impl<T: HasNodeGraph, D> GraphBase for NodeGraph<T, D> {
    type EdgeId = EdgeId;
    type NodeId = NodeId;
}

pub(crate) struct GraphNodeRef<'a, T, D> {
    id: NodeId,
    inner: &'a NodeGraph<T, D>,
}

impl<'a, T, D> Clone for GraphNodeRef<'a, T, D> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            inner: self.inner,
        }
    }
}

impl<'a, T, D> Copy for GraphNodeRef<'a, T, D> {}

impl<'a, T: HasNodeGraph, D> NodeRef for GraphNodeRef<'a, T, D> {
    type NodeId = NodeId;
    type Weight = Self;

    fn id(&self) -> Self::NodeId {
        self.id
    }

    fn weight(&self) -> &Self::Weight {
        self
    }
}

impl<'a, T, D: NodeFormatter<T>> fmt::Display for GraphNodeRef<'a, T, D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        D::fmt_node(self, f)
    }
}

pub(crate) struct GraphEdgeRef<'a, T, D> {
    id: EdgeId,
    inner: &'a NodeGraph<T, D>,
}

impl<'a, T, D> Clone for GraphEdgeRef<'a, T, D> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            inner: self.inner,
        }
    }
}

impl<'a, T, D> Copy for GraphEdgeRef<'a, T, D> {}

impl<'a, T, D> EdgeRef for GraphEdgeRef<'a, T, D> {
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

impl<'a, T, D: EdgeFormatter<T>> fmt::Display for GraphEdgeRef<'a, T, D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        D::fmt_edge(self, f)
    }
}

impl<'a, T: HasNodeGraph, D> Data for &'a NodeGraph<T, D> {
    type NodeWeight = GraphNodeRef<'a, T, D>;
    type EdgeWeight = GraphEdgeRef<'a, T, D>;
}

impl<'a, T: HasNodeGraph, D> IntoNodeIdentifiers for &'a NodeGraph<T, D> {
    type NodeIdentifiers = iter::Chain<
        std::array::IntoIter<NodeId, 2>,
        iter::Map<T::NodesForGraph<'a>, fn((NodeAddr, &'a Node)) -> NodeId>,
    >;

    fn node_identifiers(self) -> Self::NodeIdentifiers {
        [NodeId::Ret, NodeId::Err].into_iter().chain(
            self.inner
                .graph_nodes()
                .map((|(addr, _node)| NodeId::Addr(addr)) as _),
        )
    }
}

impl<'a, T: HasNodeGraph, D: 'a> IntoNodeReferences for &'a NodeGraph<T, D> {
    type NodeRef = GraphNodeRef<'a, T, D>;
    type NodeReferences = NodeReferences<'a, T, D, Self::NodeIdentifiers>;

    fn node_references(self) -> Self::NodeReferences {
        NodeReferences {
            inner: self,
            it: self.node_identifiers(),
        }
    }
}

pub(crate) struct NodeReferences<'a, T, D, I> {
    inner: &'a NodeGraph<T, D>,
    it: I,
}

impl<'a, T, D, I: Iterator<Item = NodeId>> Iterator for NodeReferences<'a, T, D, I> {
    type Item = GraphNodeRef<'a, T, D>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|id| GraphNodeRef {
            id,
            inner: self.inner,
        })
    }
}

impl<'a, T: HasNodeGraph, D> IntoEdgeReferences for &'a NodeGraph<T, D> {
    type EdgeRef = GraphEdgeRef<'a, T, D>;
    type EdgeReferences = EdgeReferences<
        'a,
        T,
        D,
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

pub(crate) struct EdgeReferences<'a, T, D, I> {
    inner: &'a NodeGraph<T, D>,
    it: I,
}

impl<'a, T, D, I: Iterator<Item = EdgeId>> Iterator for EdgeReferences<'a, T, D, I> {
    type Item = GraphEdgeRef<'a, T, D>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|id| GraphEdgeRef {
            id,
            inner: self.inner,
        })
    }
}

impl<'a, T: HasNodeGraph, D> IntoNeighbors for &'a NodeGraph<T, D> {
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

impl<T: HasNodeGraphWithNodeAddrBound, D> NodeIndexable for NodeGraph<T, D> {
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
impl<T: HasNodeGraph, D> Visitable for NodeGraph<T, D> {
    type Map = HashSet<NodeId>;

    fn visit_map(&self) -> Self::Map {
        HashSet::new()
    }

    fn reset_map(&self, map: &mut Self::Map) {
        map.clear();
    }
}

impl<T: HasNodeGraph, D> GraphProp for NodeGraph<T, D> {
    type EdgeType = Directed;
}

pub(crate) trait NodeFormatter<T>: Sized {
    fn fmt_node<'a>(node: &GraphNodeRef<'a, T, Self>, f: &mut fmt::Formatter) -> fmt::Result;
}

pub(crate) trait EdgeFormatter<T>: Sized {
    fn fmt_edge<'a>(edge: &GraphEdgeRef<'a, T, Self>, f: &mut fmt::Formatter) -> fmt::Result;
}

#[derive(Default, Copy, Clone)]
pub(crate) struct SimpleFormatter(());

impl<T> NodeFormatter<T> for SimpleFormatter {
    fn fmt_node<'a>(node: &GraphNodeRef<'a, T, Self>, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", node.id)
    }
}

impl<T> EdgeFormatter<T> for SimpleFormatter {
    fn fmt_edge<'a>(edge: &GraphEdgeRef<'a, T, Self>, f: &mut fmt::Formatter) -> fmt::Result {
        match edge.id.condition.0 {
            None => write!(f, ""),
            Some(b) => match b {
                true => write!(f, "T"),
                false => write!(f, "F"),
            },
        }
    }
}

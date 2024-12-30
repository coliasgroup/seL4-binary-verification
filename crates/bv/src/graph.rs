use crate::abstract_syntax::Argument;
use crate::abstract_syntax::{Node, NodeAddr, NodeId};

#[derive(Copy, Clone)]
pub(crate) struct NodeGraph<T> {
    inner: T,
}

impl<T> NodeGraph<T> {
    pub(crate) fn new(inner: T) -> Self {
        Self { inner }
    }

    pub(crate) fn inner(&self) -> &T {
        &self.inner
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

impl<T: HasNodeGraph> HasNodeGraph for NodeGraph<T> {
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

impl<T: HasNodeGraphWithNodeAddrBound> HasNodeGraphWithNodeAddrBound for NodeGraph<T> {
    fn node_addr_bound(&self) -> NodeAddr {
        self.inner.node_addr_bound()
    }
}

// TODO
// - HasNodeGraphWithEntrypoint
// - HasFunctionSignature
// - HasFunction (HasFunctionSignature + MaybeHasNodeGraphWithEntrypoint)

pub(crate) trait HasNodeGraphWithEntry: HasNodeGraph {
    fn graph_entry(&self) -> NodeId;
}

impl<T: HasNodeGraphWithEntry> HasNodeGraphWithEntry for NodeGraph<T> {
    fn graph_entry(&self) -> NodeId {
        self.inner.graph_entry()
    }
}

pub(crate) trait HasFunction {
    type FunctionBody: HasNodeGraphWithEntry;

    fn function_input(&self) -> &[Argument];

    fn function_output(&self) -> &[Argument];

    fn function_body_if_present(&self) -> Option<&Self::FunctionBody>;
}

pub(crate) trait HasFunctionWithBody: HasFunction {
    fn function_body(&self) -> &Self::FunctionBody;
}

pub(crate) trait MightHaveNodeGraphWithEntry {
    type NodeGraph<'a>: HasNodeGraph
    where
        Self: 'a;

    fn node_graph_option(&self) -> Option<Self::NodeGraph<'_>>;
}

impl<T: HasNodeGraphWithEntry> MightHaveNodeGraphWithEntry for T {
    type NodeGraph<'a> = &'a T where Self: 'a;

    fn node_graph_option(&self) -> Option<Self::NodeGraph<'_>> {
        Some(self)
    }
}

pub(crate) trait HasFunctionSignature {
    fn graph_input(&self) -> &[Argument];

    fn graph_output(&self) -> &[Argument];
}

impl<T: HasFunctionSignature> HasFunctionSignature for NodeGraph<T> {
    fn graph_input(&self) -> &[Argument] {
        self.inner.graph_input()
    }

    fn graph_output(&self) -> &[Argument] {
        self.inner.graph_output()
    }
}

impl<T: HasNodeGraphWithEntry> NodeGraph<T> {
    pub(crate) fn entry(&self) -> NodeId {
        self.graph_entry()
    }
}

impl<T: HasFunctionSignature> NodeGraph<T> {
    pub(crate) fn inputs(&self) -> &[Argument] {
        self.graph_input()
    }

    pub(crate) fn outputs(&self) -> &[Argument] {
        self.graph_output()
    }
}

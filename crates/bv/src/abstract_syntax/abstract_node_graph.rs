use super::{Argument, Node, NodeAddr, NodeId};

// TODO
// - AbstractFunctions
// - HasFunctions
// - HasFunctionsMut

#[derive(Copy, Clone)]
pub(crate) struct AbstractNodeGraph<T> {
    inner: T,
}

impl<T> AbstractNodeGraph<T> {
    pub(crate) fn new(inner: T) -> Self {
        Self { inner }
    }

    pub(crate) fn inner(&self) -> &T {
        &self.inner
    }

    pub(crate) fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    pub(crate) fn into_inner(self) -> T {
        self.inner
    }
}

#[derive(Copy, Clone)]
pub(crate) struct AbstractFunction<T> {
    inner: T,
}

impl<T> AbstractFunction<T> {
    pub(crate) fn new(inner: T) -> Self {
        Self { inner }
    }

    pub(crate) fn inner(&self) -> &T {
        &self.inner
    }

    pub(crate) fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    pub(crate) fn into_inner(self) -> T {
        self.inner
    }
}

impl<T: HasNodeGraph> AbstractNodeGraph<T> {
    pub(crate) fn node(&self, addr: NodeAddr) -> &Node {
        self.node_graph_node(addr)
    }

    pub(crate) fn nodes(&self) -> T::NodesForGraph<'_> {
        self.node_graph_nodes()
    }

    pub(crate) fn node_values(&self) -> impl Iterator<Item = &Node> {
        self.nodes().map(|(_, node)| node)
    }
}

impl<T: HasNodeGraphMut> AbstractNodeGraph<T> {
    pub(crate) fn node_mut(&mut self, addr: NodeAddr) -> &mut Node {
        self.node_graph_node_mut(addr)
    }

    pub(crate) fn nodes_mut(&mut self) -> T::NodesForGraphMut<'_> {
        self.node_graph_nodes_mut()
    }

    pub(crate) fn node_values_mut(&mut self) -> impl Iterator<Item = &mut Node> {
        self.nodes_mut().map(|(_, node)| node)
    }
}

impl<T: HasNodeGraphWithEntry> AbstractNodeGraph<T> {
    pub(crate) fn entry(&self) -> NodeId {
        self.node_graph_entry()
    }
}

impl<T: HasFunction> AbstractFunction<T> {
    pub(crate) fn input(&self) -> &[Argument] {
        self.function_input()
    }

    pub(crate) fn output(&self) -> &[Argument] {
        self.function_output()
    }

    pub(crate) fn body_if_present(&self) -> Option<AbstractNodeGraph<&T::FunctionBody>> {
        self.function_body_if_present()
    }
}

impl<T: HasFunctionMut> AbstractFunction<T> {
    pub(crate) fn input_mut(&mut self) -> &mut [Argument] {
        self.function_input_mut()
    }

    pub(crate) fn output_mut(&mut self) -> &mut [Argument] {
        self.function_output_mut()
    }

    pub(crate) fn body_if_present_mut(
        &mut self,
    ) -> Option<AbstractNodeGraph<&mut T::FunctionBodyMut>> {
        self.function_body_if_present_mut()
    }
}

impl<T: HasFunctionWithBody> AbstractFunction<T> {
    pub(crate) fn body(&self) -> AbstractNodeGraph<&T::FunctionBody> {
        self.function_body()
    }
}

impl<T: HasFunctionWithBodyMut> AbstractFunction<T> {
    pub(crate) fn body_mut(&mut self) -> AbstractNodeGraph<&T::FunctionBodyMut> {
        self.function_body_mut()
    }
}

pub(crate) trait HasNodeGraph {
    type NodesForGraph<'a>: Iterator<Item = (NodeAddr, &'a Node)>
    where
        Self: 'a;

    fn node_graph_node(&self, addr: NodeAddr) -> &Node;

    fn node_graph_nodes(&self) -> Self::NodesForGraph<'_>;

    fn abstract_node_graph(&self) -> AbstractNodeGraph<&Self> {
        AbstractNodeGraph::new(self)
    }
}

impl<T: HasNodeGraph> HasNodeGraph for &T {
    type NodesForGraph<'a> = T::NodesForGraph<'a> where Self: 'a;

    fn node_graph_node(&self, addr: NodeAddr) -> &Node {
        T::node_graph_node(self, addr)
    }

    fn node_graph_nodes(&self) -> Self::NodesForGraph<'_> {
        T::node_graph_nodes(self)
    }
}

impl<T: HasNodeGraph> HasNodeGraph for &mut T {
    type NodesForGraph<'a> = T::NodesForGraph<'a> where Self: 'a;

    fn node_graph_node(&self, addr: NodeAddr) -> &Node {
        T::node_graph_node(self, addr)
    }

    fn node_graph_nodes(&self) -> Self::NodesForGraph<'_> {
        T::node_graph_nodes(self)
    }
}

impl<T: HasNodeGraph> HasNodeGraph for AbstractNodeGraph<T> {
    type NodesForGraph<'a> = T::NodesForGraph<'a> where Self: 'a;

    fn node_graph_node(&self, addr: NodeAddr) -> &Node {
        self.inner().node_graph_node(addr)
    }

    fn node_graph_nodes(&self) -> Self::NodesForGraph<'_> {
        self.inner().node_graph_nodes()
    }
}

pub(crate) trait HasNodeGraphMut: HasNodeGraph {
    type NodesForGraphMut<'a>: Iterator<Item = (NodeAddr, &'a mut Node)>
    where
        Self: 'a;

    fn node_graph_node_mut(&mut self, addr: NodeAddr) -> &mut Node;

    fn node_graph_nodes_mut(&mut self) -> Self::NodesForGraphMut<'_>;

    fn abstract_node_graph_mut(&mut self) -> AbstractNodeGraph<&mut Self> {
        AbstractNodeGraph::new(self)
    }
}

impl<T: HasNodeGraphMut> HasNodeGraphMut for &mut T {
    type NodesForGraphMut<'a> = T::NodesForGraphMut<'a> where Self: 'a;

    fn node_graph_node_mut(&mut self, addr: NodeAddr) -> &mut Node {
        T::node_graph_node_mut(self, addr)
    }

    fn node_graph_nodes_mut(&mut self) -> Self::NodesForGraphMut<'_> {
        T::node_graph_nodes_mut(self)
    }
}

impl<T: HasNodeGraphMut> HasNodeGraphMut for AbstractNodeGraph<T> {
    type NodesForGraphMut<'a> = T::NodesForGraphMut<'a> where Self: 'a;

    fn node_graph_node_mut(&mut self, addr: NodeAddr) -> &mut Node {
        self.inner_mut().node_graph_node_mut(addr)
    }

    fn node_graph_nodes_mut(&mut self) -> Self::NodesForGraphMut<'_> {
        self.inner_mut().node_graph_nodes_mut()
    }
}

pub(crate) trait HasNodeGraphWithNodeAddrBound: HasNodeGraph {
    fn node_addr_bound(&self) -> NodeAddr;
}

impl<T: HasNodeGraphWithNodeAddrBound> HasNodeGraphWithNodeAddrBound for &T {
    fn node_addr_bound(&self) -> NodeAddr {
        T::node_addr_bound(self)
    }
}

impl<T: HasNodeGraphWithNodeAddrBound> HasNodeGraphWithNodeAddrBound for &mut T {
    fn node_addr_bound(&self) -> NodeAddr {
        T::node_addr_bound(self)
    }
}

impl<T: HasNodeGraphWithNodeAddrBound> HasNodeGraphWithNodeAddrBound for AbstractNodeGraph<T> {
    fn node_addr_bound(&self) -> NodeAddr {
        self.inner.node_addr_bound()
    }
}

pub(crate) trait HasNodeGraphWithEntry: HasNodeGraph {
    fn node_graph_entry(&self) -> NodeId;
}

impl<T: HasNodeGraphWithEntry> HasNodeGraphWithEntry for &T {
    fn node_graph_entry(&self) -> NodeId {
        T::node_graph_entry(self)
    }
}

impl<T: HasNodeGraphWithEntry> HasNodeGraphWithEntry for &mut T {
    fn node_graph_entry(&self) -> NodeId {
        T::node_graph_entry(self)
    }
}

impl<T: HasNodeGraphWithEntry> HasNodeGraphWithEntry for AbstractNodeGraph<T> {
    fn node_graph_entry(&self) -> NodeId {
        self.inner.node_graph_entry()
    }
}

pub(crate) trait HasFunction {
    type FunctionBody: HasNodeGraphWithEntry;

    fn function_input(&self) -> &[Argument];

    fn function_output(&self) -> &[Argument];

    fn function_body_if_present(&self) -> Option<AbstractNodeGraph<&Self::FunctionBody>>;

    fn abstract_function(&self) -> AbstractFunction<&Self> {
        AbstractFunction::new(self)
    }
}

impl<T: HasFunction> HasFunction for &T {
    type FunctionBody = T::FunctionBody;

    fn function_input(&self) -> &[Argument] {
        T::function_input(self)
    }

    fn function_output(&self) -> &[Argument] {
        T::function_output(self)
    }

    fn function_body_if_present(&self) -> Option<AbstractNodeGraph<&Self::FunctionBody>> {
        T::function_body_if_present(self)
    }
}

impl<T: HasFunction> HasFunction for &mut T {
    type FunctionBody = T::FunctionBody;

    fn function_input(&self) -> &[Argument] {
        T::function_input(self)
    }

    fn function_output(&self) -> &[Argument] {
        T::function_output(self)
    }

    fn function_body_if_present(&self) -> Option<AbstractNodeGraph<&Self::FunctionBody>> {
        T::function_body_if_present(self)
    }
}

impl<T: HasFunction> HasFunction for AbstractFunction<T> {
    type FunctionBody = T::FunctionBody;

    fn function_input(&self) -> &[Argument] {
        self.inner().function_input()
    }

    fn function_output(&self) -> &[Argument] {
        self.inner().function_output()
    }

    fn function_body_if_present(&self) -> Option<AbstractNodeGraph<&Self::FunctionBody>> {
        self.inner().function_body_if_present()
    }
}

pub(crate) trait HasFunctionMut: HasFunction {
    type FunctionBodyMut: HasNodeGraphWithEntry + HasNodeGraphMut;

    fn function_input_mut(&mut self) -> &mut [Argument];

    fn function_output_mut(&mut self) -> &mut [Argument];

    fn function_body_if_present_mut(
        &mut self,
    ) -> Option<AbstractNodeGraph<&mut Self::FunctionBodyMut>>;

    fn abstract_function_mut(&mut self) -> AbstractFunction<&mut Self> {
        AbstractFunction::new(self)
    }
}

impl<T: HasFunctionMut> HasFunctionMut for &mut T {
    type FunctionBodyMut = T::FunctionBodyMut;

    fn function_input_mut(&mut self) -> &mut [Argument] {
        T::function_input_mut(self)
    }

    fn function_output_mut(&mut self) -> &mut [Argument] {
        T::function_output_mut(self)
    }

    fn function_body_if_present_mut(
        &mut self,
    ) -> Option<AbstractNodeGraph<&mut Self::FunctionBodyMut>> {
        T::function_body_if_present_mut(self)
    }
}

impl<T: HasFunctionMut> HasFunctionMut for AbstractFunction<T> {
    type FunctionBodyMut = T::FunctionBodyMut;

    fn function_input_mut(&mut self) -> &mut [Argument] {
        self.inner_mut().function_input_mut()
    }

    fn function_output_mut(&mut self) -> &mut [Argument] {
        self.inner_mut().function_output_mut()
    }

    fn function_body_if_present_mut(
        &mut self,
    ) -> Option<AbstractNodeGraph<&mut Self::FunctionBodyMut>> {
        self.inner_mut().function_body_if_present_mut()
    }
}

pub(crate) trait HasFunctionWithBody: HasFunction {
    fn function_body(&self) -> AbstractNodeGraph<&Self::FunctionBody>;
}

impl<T: HasFunctionWithBody> HasFunctionWithBody for &T {
    fn function_body(&self) -> AbstractNodeGraph<&Self::FunctionBody> {
        T::function_body(self)
    }
}

impl<T: HasFunctionWithBody> HasFunctionWithBody for &mut T {
    fn function_body(&self) -> AbstractNodeGraph<&Self::FunctionBody> {
        T::function_body(self)
    }
}

impl<T: HasFunctionWithBody> HasFunctionWithBody for AbstractFunction<T> {
    fn function_body(&self) -> AbstractNodeGraph<&Self::FunctionBody> {
        self.inner().function_body()
    }
}

pub(crate) trait HasFunctionWithBodyMut: HasFunctionWithBody + HasFunctionMut {
    fn function_body_mut(&mut self) -> AbstractNodeGraph<&Self::FunctionBodyMut>;
}

impl<T: HasFunctionWithBodyMut> HasFunctionWithBodyMut for AbstractFunction<T> {
    fn function_body_mut(&mut self) -> AbstractNodeGraph<&Self::FunctionBodyMut> {
        self.inner_mut().function_body_mut()
    }
}

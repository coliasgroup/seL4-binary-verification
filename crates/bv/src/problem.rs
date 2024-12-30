use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::convert::Infallible;
use std::iter;
use std::slice;
use std::vec;

use crate::abstract_syntax::{
    Argument, BasicNode, CallNode, Expr, ExprValue, Function, Ident, Node, NodeAddr, NodeId, Type,
    VarUpdate,
};
use crate::concrete_syntax::parse::{
    LineBuffer, LinesBuffer, ParseError, ParseFromLine, ParseFromLines,
};
use crate::concrete_syntax::print::{BlockBuf, LineBuf, ToTokens};
use crate::graph::{
    HasFunctionSignature, HasNodeGraph, HasNodeGraphWithEntry, HasNodeGraphWithNodeAddrBound,
};
use crate::pairing::Tag;
use crate::utils::petgraph::algorithms::{reachable_nodes, tarjan_scc_variant};

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct Problem<M = ()> {
    pub(crate) c: ProblemSide,
    pub(crate) asm: ProblemSide,
    pub(crate) nodes: NodeMap<M>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ProblemSide {
    pub(crate) name: Ident,
    pub(crate) input: Vec<Argument>,
    pub(crate) output: Vec<Argument>,
    pub(crate) entry: NodeId,
}

impl<M> Problem<M> {
    pub(crate) fn strip_meta(self) -> Problem<()> {
        self.map_meta(|_| ())
    }

    pub(crate) fn map_meta<M1>(self, f: impl FnMut(M) -> M1) -> Problem<M1> {
        Problem {
            c: self.c,
            asm: self.asm,
            nodes: self.nodes.map_meta(f),
        }
    }

    pub(crate) fn problem_side(&self, tag: Tag) -> &ProblemSide {
        match tag {
            Tag::C => &self.c,
            Tag::Asm => &self.asm,
        }
    }

    pub(crate) fn problem_side_mut(&mut self, tag: Tag) -> &mut ProblemSide {
        match tag {
            Tag::C => &mut self.c,
            Tag::Asm => &mut self.asm,
        }
    }

    pub(crate) fn at<T>(&self, tag_selector: T) -> ProblemAtSide<'_, T, M> {
        ProblemAtSide {
            inner: self,
            selector: tag_selector,
        }
    }

    pub(crate) fn at_c(&self) -> ProblemAtSide<'_, C, M> {
        self.at(C)
    }

    pub(crate) fn at_asm(&self) -> ProblemAtSide<'_, Asm, M> {
        self.at(Asm)
    }
}

impl Problem {
    pub(crate) fn pretty_print_into_block(&self) -> BlockBuf {
        let mut block = BlockBuf::new();
        block.push_line_with(|line| line.to_tokens("Problem"));
        for (tag, side) in [(Tag::C, &self.c), (Tag::Asm, &self.asm)] {
            block.push_line_with(|line| {
                line.to_tokens("Entry");
                line.to_tokens(&side.entry);
                line.to_tokens(&tag);
                line.to_tokens(&side.name);
                line.to_tokens(&side.input);
                line.to_tokens(&side.output);
            });
        }
        for (addr, node) in self.nodes.nodes() {
            block.push_line_with(|line| {
                line.lower_hex_to_tokens(&addr);
                line.to_tokens(node);
            });
        }
        block.push_line_with(|line| line.to_tokens("EndProblem"));
        block
    }
}

impl ParseFromLines for Problem {
    fn parse(lines: &mut LinesBuffer) -> Result<Self, ParseError> {
        lines.parse_next_line_with(|toks| toks.match_("Problem"))?;
        let (tag_a, side_a) = lines.parse_next_line_with(parse_problem_side_line)?;
        let (tag_b, side_b) = lines.parse_next_line_with(parse_problem_side_line)?;
        let (c, asm) = match (tag_a, tag_b) {
            (Tag::C, Tag::Asm) => (side_a, side_b),
            (Tag::Asm, Tag::C) => (side_b, side_a),
            _ => panic!(),
        };

        let mut nodes = BTreeMap::new();
        loop {
            if lines.peek_token().unwrap().as_str() == "EndProblem" {
                lines.parse_next_line_with(|toks| toks.advance())?;
                break;
            } else {
                let (addr, node) = lines.parse_next_line_with(|toks| {
                    let addr = toks.parse_prim_int()?;
                    let node = toks.parse()?;
                    Ok((addr, NodeWithMeta::new(node, ())))
                })?;
                nodes.insert(addr, node);
            }
        }

        Ok(Self {
            c,
            asm,
            nodes: NodeMap::from_btree_map(nodes),
        })
    }
}

fn parse_problem_side_line(toks: &mut LineBuffer) -> Result<(Tag, ProblemSide), ParseError> {
    toks.match_("Entry")?;
    let entry = toks.parse()?;
    let tag = toks.parse()?;
    let name = toks.parse()?;
    let input = toks.parse()?;
    let output = toks.parse()?;
    let side = ProblemSide {
        name,
        input,
        output,
        entry,
    };
    Ok((tag, side))
}

impl<M> HasNodeGraph for Problem<M> {
    type NodesForGraph<'a> = <NodeMap<M> as HasNodeGraph>::NodesForGraph<'a> where M: 'a;

    fn graph_node(&self, addr: NodeAddr) -> &Node {
        self.nodes.graph_node(addr)
    }

    fn graph_nodes(&self) -> Self::NodesForGraph<'_> {
        self.nodes.graph_nodes()
    }
}

impl<M> HasNodeGraphWithNodeAddrBound for Problem<M> {
    fn node_addr_bound(&self) -> NodeAddr {
        self.nodes.node_addr_bound()
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct NodeWithMeta<M> {
    node: Node,
    meta: M,
}

impl<M> NodeWithMeta<M> {
    pub(crate) fn new(node: Node, meta: M) -> Self {
        Self { node, meta }
    }

    pub(crate) fn node(&self) -> &Node {
        &self.node
    }

    pub(crate) fn node_mut(&mut self) -> &mut Node {
        &mut self.node
    }

    pub(crate) fn into_node(self) -> Node {
        self.node
    }

    pub(crate) fn meta(&self) -> &M {
        &self.meta
    }

    pub(crate) fn meta_mut(&mut self) -> &mut M {
        &mut self.meta
    }

    pub(crate) fn into_meta(self) -> M {
        self.meta
    }

    pub(crate) fn strip_meta(self) -> NodeWithMeta<()> {
        self.with_meta(())
    }

    pub(crate) fn with_meta<M1>(self, meta: M1) -> NodeWithMeta<M1> {
        self.map_meta(|_| meta)
    }

    pub(crate) fn map_meta<M1>(self, f: impl FnOnce(M) -> M1) -> NodeWithMeta<M1> {
        self.try_map_meta(|meta| Result::<_, Infallible>::Ok(f(meta)))
            .unwrap_or_else(|never| match never {})
    }

    pub(crate) fn try_map_meta<M1, E>(
        self,
        f: impl FnOnce(M) -> Result<M1, E>,
    ) -> Result<NodeWithMeta<M1>, E> {
        Ok(NodeWithMeta::new(self.node, f(self.meta)?))
    }
}

impl From<Node> for NodeWithMeta<()> {
    fn from(node: Node) -> Self {
        Self::new(node, ())
    }
}

impl From<NodeWithMeta<()>> for Node {
    fn from(node_with_meta: NodeWithMeta<()>) -> Self {
        node_with_meta.into_node()
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct NodeMap<M> {
    nodes: Vec<Option<NodeWithMeta<M>>>,
}

impl<M> NodeMap<M> {
    pub(crate) fn empty() -> Self {
        Self { nodes: vec![] }
    }

    pub(crate) fn footprint(&self) -> usize {
        self.nodes.len()
    }

    pub(crate) fn from_assocs(
        backing_size: usize,
        assocs: impl IntoIterator<Item = (NodeAddr, NodeWithMeta<M>)>,
    ) -> Self {
        let mut v = Vec::new();
        v.resize_with(backing_size, || None);
        for (node_addr, node_with_meta) in assocs {
            v[usize::try_from(node_addr).unwrap()] = Some(node_with_meta);
        }
        Self { nodes: v }
    }

    pub(crate) fn from_btree_map(map: BTreeMap<NodeAddr, NodeWithMeta<M>>) -> Self {
        let backing_size = map
            .last_key_value()
            .map(|(k, _v)| k.checked_add(1).unwrap().try_into().unwrap())
            .unwrap_or(0);
        assert!(map.len() * 2 >= backing_size);
        Self::from_assocs(backing_size, map)
    }

    fn node_with_meta_slot(&self, addr: NodeAddr) -> &Option<NodeWithMeta<M>> {
        &self.nodes[usize::try_from(addr).unwrap()]
    }

    fn node_with_meta_slot_mut(&mut self, addr: NodeAddr) -> &mut Option<NodeWithMeta<M>> {
        &mut self.nodes[usize::try_from(addr).unwrap()]
    }

    pub(crate) fn node_with_meta(&self, addr: NodeAddr) -> &NodeWithMeta<M> {
        self.node_with_meta_slot(addr).as_ref().unwrap()
    }

    pub(crate) fn node_with_meta_mut(&mut self, addr: NodeAddr) -> &mut NodeWithMeta<M> {
        self.node_with_meta_slot_mut(addr).as_mut().unwrap()
    }

    pub(crate) fn node(&self, addr: NodeAddr) -> &Node {
        self.node_with_meta(addr).node()
    }

    pub(crate) fn node_mut(&mut self, addr: NodeAddr) -> &mut Node {
        self.node_with_meta_mut(addr).node_mut()
    }

    pub(crate) fn node_meta(&self, addr: NodeAddr) -> &M {
        self.node_with_meta(addr).meta()
    }

    pub(crate) fn node_meta_mut(&mut self, addr: NodeAddr) -> &mut M {
        self.node_with_meta_mut(addr).meta_mut()
    }

    pub(crate) fn nodes_with_meta(&self) -> NodeMapNodesWithMetaIter<'_, M> {
        self.nodes
            .iter()
            .enumerate()
            .filter_map(|(i, maybe_node_with_meta)| {
                maybe_node_with_meta
                    .as_ref()
                    .map(|node_with_meta| (i.try_into().unwrap(), node_with_meta))
            })
    }

    pub(crate) fn nodes_with_meta_mut(
        &mut self,
    ) -> impl Iterator<Item = (NodeAddr, &mut NodeWithMeta<M>)> {
        self.nodes
            .iter_mut()
            .enumerate()
            .filter_map(|(i, maybe_node_with_meta)| {
                maybe_node_with_meta
                    .as_mut()
                    .map(|node_with_meta| (i.try_into().unwrap(), node_with_meta))
            })
    }

    pub(crate) fn nodes(&self) -> NodeMapNodesIter<'_, M> {
        self.nodes_with_meta()
            .map(|(addr, node_with_meta)| (addr, node_with_meta.node()))
    }

    pub(crate) fn nodes_mut(&mut self) -> impl Iterator<Item = (NodeAddr, &mut Node)> {
        self.nodes_with_meta_mut()
            .map(|(addr, node_with_meta)| (addr, node_with_meta.node_mut()))
    }

    pub(crate) fn node_addrs(&self) -> NodeMapNodeAddrsIter<'_, M> {
        self.nodes_with_meta().map(|(addr, _node_with_meta)| addr)
    }

    pub(crate) fn into_nodes_with_meta(self) -> impl Iterator<Item = (NodeAddr, NodeWithMeta<M>)> {
        self.nodes
            .into_iter()
            .enumerate()
            .filter_map(|(i, maybe_node_with_meta)| {
                maybe_node_with_meta.map(|node_with_meta| (i.try_into().unwrap(), node_with_meta))
            })
    }

    pub(crate) fn strip_meta(self) -> NodeMap<()> {
        self.map_meta(|_| ())
    }

    pub(crate) fn map_meta<M1>(self, mut f: impl FnMut(M) -> M1) -> NodeMap<M1> {
        self.try_map_meta(|meta| Result::<_, Infallible>::Ok(f(meta)))
            .unwrap_or_else(|never| match never {})
    }

    pub(crate) fn try_map_meta<M1, E>(
        self,
        mut f: impl FnMut(M) -> Result<M1, E>,
    ) -> Result<NodeMap<M1>, E> {
        Ok(NodeMap {
            nodes: self
                .nodes
                .into_iter()
                .map(|maybe_node_with_meta| {
                    maybe_node_with_meta
                        .map(|node_with_meta| node_with_meta.try_map_meta(&mut f))
                        .transpose()
                })
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    pub(crate) fn append_option(
        &mut self,
        maybe_node_with_meta: Option<NodeWithMeta<M>>,
    ) -> NodeAddr {
        let addr = self.nodes.len().try_into().unwrap();
        self.nodes.push(maybe_node_with_meta);
        addr
    }

    pub(crate) fn append(&mut self, node_with_meta: NodeWithMeta<M>) -> NodeAddr {
        self.append_option(Some(node_with_meta))
    }

    pub(crate) fn reserve(&mut self) -> NodeAddr {
        self.append_option(None)
    }

    pub(crate) fn insert(&mut self, addr: NodeAddr, node_with_meta: NodeWithMeta<M>) {
        let slot = self.node_with_meta_slot_mut(addr);
        assert!(slot.is_none());
        *slot = Some(node_with_meta);
    }

    pub(crate) fn remove(&mut self, addr: NodeAddr) {
        let slot = self.node_with_meta_slot_mut(addr);
        assert!(slot.is_some());
        *slot = None;
    }
}

pub(crate) type NodeMapNodesWithMetaIter<'a, M> = iter::FilterMap<
    iter::Enumerate<slice::Iter<'a, Option<NodeWithMeta<M>>>>,
    fn((usize, &'a Option<NodeWithMeta<M>>)) -> Option<(NodeAddr, &'a NodeWithMeta<M>)>,
>;

pub(crate) type NodeMapNodesIter<'a, M> = iter::Map<
    NodeMapNodesWithMetaIter<'a, M>,
    fn((NodeAddr, &'a NodeWithMeta<M>)) -> (NodeAddr, &'a Node),
>;

pub(crate) type NodeMapNodeAddrsIter<'a, M> =
    iter::Map<NodeMapNodesWithMetaIter<'a, M>, fn((NodeAddr, &'a NodeWithMeta<M>)) -> NodeAddr>;

impl<M> HasNodeGraph for NodeMap<M> {
    type NodesForGraph<'a> = NodeMapNodesIter<'a, M> where M: 'a;

    fn graph_node(&self, addr: NodeAddr) -> &Node {
        self.node(addr)
    }

    fn graph_nodes(&self) -> Self::NodesForGraph<'_> {
        self.nodes()
    }
}

impl<M> HasNodeGraphWithNodeAddrBound for NodeMap<M> {
    fn node_addr_bound(&self) -> NodeAddr {
        self.footprint().try_into().unwrap()
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct NodeSource {
    pub(crate) tag: Tag,
    pub(crate) function_name: Ident,
    pub(crate) node_addr: NodeAddr,
}

impl ParseFromLine for NodeSource {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            tag: toks.parse()?,
            function_name: toks.parse()?,
            node_addr: toks.parse_prim_int()?,
        })
    }
}

impl ToTokens for NodeSource {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.tag);
        line.to_tokens(&self.function_name);
        line.lower_hex_to_tokens(&self.node_addr);
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct NodeBySource {
    pub(crate) node_source: NodeSource,
    pub(crate) index_in_problem: usize,
}

impl ToTokens for NodeBySource {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.node_source);
        line.display_to_tokens(&self.index_in_problem);
    }
}

impl ParseFromLine for NodeBySource {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            node_source: toks.parse()?,
            index_in_problem: toks.parse_prim_int()?,
        })
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ProblemNodeMeta {
    by_source: Option<NodeBySource>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NodeMapBuilder {
    nodes: NodeMap<ProblemNodeMeta>,
    nodes_by_source: HashMap<NodeSource, Vec<NodeAddr>>,
    vars: HashSet<Ident>,
}

impl NodeMapBuilder {
    fn new() -> Self {
        Self {
            nodes: NodeMap::empty(),
            nodes_by_source: Default::default(),
            vars: Default::default(),
        }
    }

    pub(crate) fn build(self) -> NodeMap<ProblemNodeMeta> {
        self.nodes
    }

    fn insert(&mut self, addr: NodeAddr, node: Node, node_source: Option<NodeSource>) {
        let by_source = node_source.map(|node_source| {
            let v = self
                .nodes_by_source
                .entry(node_source.clone())
                .or_insert_with(|| vec![]);
            let index_in_problem = v.len();
            v.push(addr);
            NodeBySource {
                node_source,
                index_in_problem,
            }
        });
        self.nodes
            .insert(addr, NodeWithMeta::new(node, ProblemNodeMeta { by_source }))
    }

    fn fresh_name(&mut self, n: &Ident) -> Ident {
        // implementation matches graph_refine.syntax.fresh_name

        if !self.vars.contains(n) {
            self.vars.insert(n.clone());
            return n.clone();
        }

        let mut x = 1;
        let mut y = 1;
        while self.vars.contains(&format!("{n}.{x:?}")) {
            y = x;
            x = x * 2;
        }
        while y < x {
            let z = (y + x) / 2;
            if self.vars.contains(&format!("{n}.{z:?}")) {
                y = z + 1;
            } else {
                x = z;
            }
        }

        let n = format!("{n}.{x:?}");
        assert!(!self.vars.contains(&n));
        self.vars.insert(n.clone());
        n
    }

    fn add_function(
        &mut self,
        tag: Tag,
        f_name: &Ident,
        f: &Function,
        mut node_renames: BTreeMap<NodeId, NodeId>,
    ) -> AddFunctionRenames {
        let body = f.body.as_ref().unwrap();
        if let Entry::Vacant(entry) = node_renames.entry(NodeId::Ret) {
            entry.insert(NodeId::Ret);
        }
        if let Entry::Vacant(entry) = node_renames.entry(NodeId::Err) {
            entry.insert(NodeId::Err);
        }
        let orig_vars = {
            let mut this = BTreeMap::new();
            f.visit_var_decls(&mut |name: &Ident, ty: &Type| {
                this.insert(name.clone(), ty.clone());
            });
            this
        };
        let var_renames: BTreeMap<Ident, Ident> = orig_vars
            .iter()
            .map(|(name, _ty)| (name.clone(), self.fresh_name(name)))
            .collect();
        let mut orig_node_addrs = reachable_nodes(
            &f.body().unwrap().node_graph(),
            Some(f.body().unwrap().entry_point),
        );
        orig_node_addrs.sort();
        let mut new_node_renames = BTreeMap::new();
        for node_id in &orig_node_addrs {
            if let NodeId::Addr(addr) = node_id {
                assert!(!node_renames.contains_key(&NodeId::Addr(*addr)));
                let new_addr = self.nodes.reserve();
                node_renames.insert(NodeId::Addr(*addr), NodeId::Addr(new_addr));
                new_node_renames.insert(NodeId::Addr(*addr), NodeId::Addr(new_addr));
            }
        }
        for node_id in &orig_node_addrs {
            if let NodeId::Addr(addr) = node_id {
                let node = body.nodes[addr].clone();
                let mut node = node.clone();
                node.visit_conts_mut(|n_id| *n_id = node_renames[n_id].clone());
                node.visit_var_decls_mut(&mut |name, _ty| *name = var_renames[name].clone());
                node.visit_exprs_mut(&mut |expr| {
                    if let ExprValue::Var(n) = &mut expr.value {
                        *n = var_renames[n].clone()
                    }
                });
                self.insert(
                    node_renames[&NodeId::Addr(*addr)].addr().unwrap(),
                    node,
                    Some(NodeSource {
                        tag,
                        function_name: f_name.clone(),
                        node_addr: *addr,
                    }),
                );
            }
        }
        // log::debug!("{:?}", var_renames);
        AddFunctionRenames {
            var: var_renames,
            node_addr: new_node_renames,
        }
    }

    pub(crate) fn inline<'a>(
        &mut self,
        node_by_source: &NodeBySource,
        lookup_function: impl FnOnce(Tag, &Ident) -> &'a Function,
    ) {
        let tag = node_by_source.node_source.tag;
        let node_addr =
            self.nodes_by_source[&node_by_source.node_source][node_by_source.index_in_problem];
        let f_name = match &self.nodes.node(node_addr) {
            Node::Call(CallNode { function_name, .. }) => function_name.clone(),
            _ => panic!(),
        };
        self.inline_at_point(node_addr, lookup_function(tag, &f_name));
    }

    pub(crate) fn inline_at_point(&mut self, node_addr: NodeAddr, f: &Function) {
        let node_with_meta = self.nodes.node_with_meta(node_addr);
        let tag = node_with_meta
            .meta()
            .by_source
            .as_ref()
            .unwrap()
            .node_source
            .tag;
        let call_node = match node_with_meta.node() {
            Node::Call(call_node) => call_node.clone(),
            _ => panic!(),
        };
        let f_name = call_node.function_name.clone();
        let exit_node_addr = self.nodes.reserve();
        let renames = self.add_function(
            tag,
            &f_name,
            f,
            BTreeMap::from_iter([(NodeId::Ret, NodeId::Addr(exit_node_addr))]),
        );
        let entry_node_addr = renames.node_addr[&f.body().unwrap().entry_point];
        *self.nodes.node_with_meta_mut(node_addr) = NodeWithMeta::new(
            Node::Basic(BasicNode {
                next: entry_node_addr,
                var_updates: f
                    .input
                    .iter()
                    .zip(call_node.input.iter())
                    .map(|(arg, call_input)| VarUpdate {
                        var_name: renames.var[&arg.name].clone(),
                        ty: arg.ty.clone(),
                        expr: call_input.clone(),
                    })
                    .collect(),
            }),
            ProblemNodeMeta { by_source: None },
        );
        self.nodes.insert(
            exit_node_addr,
            NodeWithMeta::new(
                Node::Basic(BasicNode {
                    next: call_node.next,
                    var_updates: f
                        .output
                        .iter()
                        .zip(call_node.output.iter())
                        .map(|(arg, call_output)| VarUpdate {
                            var_name: call_output.name.clone(),
                            ty: arg.ty.clone(),
                            expr: Expr::mk_var(renames.var[&arg.name].clone(), arg.ty.clone()),
                        })
                        .collect(),
                }),
                ProblemNodeMeta { by_source: None },
            ),
        );
    }

    fn compute_preds(&self) -> BTreeMap<NodeId, BTreeSet<NodeAddr>> {
        let mut preds = BTreeMap::new();
        preds.insert(NodeId::Ret, BTreeSet::new());
        preds.insert(NodeId::Err, BTreeSet::new());
        for (addr, node) in self.nodes.nodes() {
            for cont in node.conts() {
                preds
                    .entry(cont)
                    .or_insert_with(|| BTreeSet::new())
                    .insert(addr);
            }
        }
        preds
    }

    fn pad_merge_points(&mut self) {
        let mut edges_to_merge_points = vec![];
        for (node_id, node_preds) in self.compute_preds().iter() {
            if node_preds.len() > 1 {
                if let NodeId::Addr(node_addr) = node_id {
                    for pred_node_addr in node_preds {
                        match &self.nodes.node(*pred_node_addr) {
                            Node::Basic(basic) if basic.var_updates.is_empty() => {}
                            _ => edges_to_merge_points.push((*pred_node_addr, *node_addr)),
                        }
                    }
                }
            }
        }

        for (pred_node_addr, node_addr) in &edges_to_merge_points {
            let padding_node_addr = self.nodes.append(NodeWithMeta::new(
                Node::Basic(BasicNode {
                    next: NodeId::Addr(*node_addr),
                    var_updates: vec![],
                }),
                ProblemNodeMeta { by_source: None },
            ));
            self.nodes
                .node_mut(*pred_node_addr)
                .visit_conts_mut(|cont_node_addr| {
                    if cont_node_addr == &NodeId::Addr(*node_addr) {
                        *cont_node_addr = NodeId::Addr(padding_node_addr);
                    }
                });
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ProblemBuilder {
    c: ProblemSide,
    asm: ProblemSide,
    builder: NodeMapBuilder,
}

impl ProblemBuilder {
    pub(crate) fn new(
        f_c_name: &Ident,
        f_c: &Function,
        f_asm_name: &Ident,
        f_asm: &Function,
    ) -> Self {
        let mut builder = NodeMapBuilder::new();
        builder.nodes.reserve(); // HACK graph_refine.problem stats at 1
        let asm = {
            let renames = builder.add_function(Tag::Asm, f_asm_name, f_asm, BTreeMap::new());
            ProblemSide {
                name: f_asm_name.clone(),
                input: f_asm
                    .input
                    .iter()
                    .map(|arg| Argument {
                        ty: arg.ty.clone(),
                        name: renames.var[&arg.name].clone(),
                    })
                    .collect(),
                output: f_asm
                    .output
                    .iter()
                    .map(|arg| Argument {
                        ty: arg.ty.clone(),
                        name: renames.var[&arg.name].clone(),
                    })
                    .collect(),
                entry: renames.node_addr[&f_asm.body.as_ref().unwrap().entry_point],
            }
        };
        let c = {
            let renames = builder.add_function(Tag::C, f_c_name, f_c, BTreeMap::new());
            ProblemSide {
                name: f_c_name.clone(),
                input: f_c
                    .input
                    .iter()
                    .map(|arg| Argument {
                        ty: arg.ty.clone(),
                        name: renames.var[&arg.name].clone(),
                    })
                    .collect(),
                output: f_c
                    .output
                    .iter()
                    .map(|arg| Argument {
                        ty: arg.ty.clone(),
                        name: renames.var[&arg.name].clone(),
                    })
                    .collect(),
                entry: renames.node_addr[&f_c.body.as_ref().unwrap().entry_point],
            }
        };
        let mut this = Self { c, asm, builder };

        this.force_simple_loop_returns();

        this
    }

    pub(crate) fn build(mut self) -> Problem<ProblemNodeMeta> {
        self.force_simple_loop_returns();
        self.builder.pad_merge_points();
        self.force_simple_loop_returns();
        Problem {
            c: self.c,
            asm: self.asm,
            nodes: self.builder.build(),
        }
    }

    pub(crate) fn inline<'a>(
        &mut self,
        node_by_source: &NodeBySource,
        lookup_function: impl FnOnce(Tag, &Ident) -> &'a Function,
    ) {
        self.builder.inline(node_by_source, lookup_function);
        self.force_simple_loop_returns();
    }

    fn force_simple_loop_returns(&mut self) {
        let preds = self.builder.compute_preds();
        let sccs_with_heads = tarjan_scc_variant(&self.builder.nodes.node_graph(), || {
            [self.c.entry, self.asm.entry]
        });
        for (head, scc) in sccs_with_heads {
            let rets = preds[&head]
                .iter()
                .filter(|n| scc.contains(&NodeId::Addr(**n)))
                .collect::<Vec<_>>();
            let ret_is_simple = rets.len() == 1
                && NodeId::Addr(*rets[0]) != head
                && self.builder.nodes.node(*rets[0]).is_noop();
            if !ret_is_simple {
                let simple_ret = self.builder.nodes.append(NodeWithMeta::new(
                    Node::Basic(BasicNode {
                        next: head.clone(),
                        var_updates: vec![],
                    }),
                    ProblemNodeMeta { by_source: None },
                ));
                for ret in rets {
                    self.builder.nodes.node_mut(*ret).visit_conts_mut(|cont| {
                        if cont == &head {
                            *cont = NodeId::Addr(simple_ret);
                        }
                    });
                }
            }
        }
    }
}

struct AddFunctionRenames {
    node_addr: BTreeMap<NodeId, NodeId>,
    var: BTreeMap<Ident, Ident>,
}

pub(crate) struct ProblemAtSide<'a, T, M = ()> {
    inner: &'a Problem<M>,
    selector: T,
}

pub(crate) trait TagSelector {
    fn select(&self) -> Tag;
}

impl TagSelector for Tag {
    fn select(&self) -> Tag {
        *self
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct C;

#[derive(Copy, Clone, Debug)]
pub(crate) struct Asm;

impl TagSelector for C {
    fn select(&self) -> Tag {
        Tag::C
    }
}

impl TagSelector for Asm {
    fn select(&self) -> Tag {
        Tag::Asm
    }
}

impl<'a, T, M> HasNodeGraph for ProblemAtSide<'a, T, M> {
    type NodesForGraph<'b> = <Problem<M> as HasNodeGraph>::NodesForGraph<'b> where Self: 'b;

    fn graph_node(&self, addr: NodeAddr) -> &Node {
        self.inner.graph_node(addr)
    }

    fn graph_nodes(&self) -> Self::NodesForGraph<'_> {
        self.inner.graph_nodes()
    }
}

impl<'a, T: TagSelector, M> HasNodeGraphWithEntry for ProblemAtSide<'a, T, M> {
    fn graph_entry(&self) -> NodeId {
        self.inner.problem_side(self.selector.select()).entry
    }
}

impl<'a, T: TagSelector, M> HasFunctionSignature for ProblemAtSide<'a, T, M> {
    fn graph_input(&self) -> &[Argument] {
        &self.inner.problem_side(self.selector.select()).input
    }

    fn graph_output(&self) -> &[Argument] {
        &self.inner.problem_side(self.selector.select()).output
    }
}

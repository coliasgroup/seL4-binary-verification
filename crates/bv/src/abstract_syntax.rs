use std::collections::{btree_map, BTreeMap};
use std::convert::Infallible;
use std::ops::{Add, BitAnd, BitOr, Neg, Not, Sub};
use std::slice::SliceIndex;
use std::{fmt, iter};

use arrayvec::ArrayVec;
use num::BigInt;

use crate::arch::WORD_SIZE_BITS;
use crate::expr::{Expr, Type};
use crate::graph::{
    HasFunctionSignature, HasNodeGraph, HasNodeGraphWithNodeAddrBound, MightHaveNodeGraphWithEntry,
};

pub(crate) type Ident = String;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Default)]
pub(crate) struct File {
    pub(crate) structs: BTreeMap<Ident, Struct>,
    pub(crate) const_globals: BTreeMap<Ident, ConstGlobal>,
    pub(crate) functions: BTreeMap<Ident, Function>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct Struct {
    pub(crate) size: u64,
    pub(crate) align: u64,
    pub(crate) fields: BTreeMap<Ident, StructField>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct StructField {
    pub(crate) ty: Type,
    pub(crate) offset: u64,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ConstGlobal {
    pub(crate) value: Expr,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct Function {
    pub(crate) input: Vec<Argument>,
    pub(crate) output: Vec<Argument>,
    pub(crate) body: Option<FunctionBody>,
}

impl Function {
    pub(crate) fn input(&self) -> &[Argument] {
        &self.input
    }

    pub(crate) fn output(&self) -> &[Argument] {
        &self.output
    }

    pub(crate) fn body(&self) -> Option<&FunctionBody> {
        self.body.as_ref()
    }

    pub(crate) fn body_mut(&mut self) -> Option<&mut FunctionBody> {
        self.body.as_mut()
    }

    pub(crate) fn visit_exprs(&mut self, f: &mut impl FnMut(&Expr)) {
        if let Some(body) = self.body() {
            for node in body.nodes.values() {
                node.visit_exprs(f);
            }
        }
    }

    pub(crate) fn visit_exprs_mut(&mut self, f: &mut impl FnMut(&mut Expr)) {
        if let Some(body) = self.body_mut() {
            for node in body.nodes.values_mut() {
                node.visit_exprs_mut(f);
            }
        }
    }

    pub(crate) fn visit_var_decls(&self, f: &mut impl FnMut(&Ident, &Type)) {
        for arg in &self.input {
            arg.visit_var_decls(f);
        }
        for arg in &self.output {
            arg.visit_var_decls(f);
        }
        if let Some(body) = &self.body {
            for node in body.nodes.values() {
                node.visit_var_decls(f);
            }
        }
    }
}

impl MightHaveNodeGraphWithEntry for Function {
    type NodeGraph<'a> = &'a FunctionBody;

    fn node_graph_option(&self) -> Option<Self::NodeGraph<'_>> {
        self.body()
    }
}

impl HasFunctionSignature for Function {
    fn graph_input(&self) -> &[Argument] {
        self.input()
    }

    fn graph_output(&self) -> &[Argument] {
        self.output()
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct FunctionBody {
    pub(crate) entry_point: NodeId,
    pub(crate) nodes: BTreeMap<NodeAddr, Node>,
}

impl HasNodeGraph for FunctionBody {
    type NodesForGraph<'a> = iter::Map<
        btree_map::Iter<'a, NodeAddr, Node>,
        fn((&'a NodeAddr, &'a Node)) -> (NodeAddr, &'a Node),
    >;

    fn graph_node(&self, addr: NodeAddr) -> &Node {
        &self.nodes[&addr]
    }

    fn graph_nodes(&self) -> Self::NodesForGraph<'_> {
        self.nodes.iter().map(|(addr, node)| (*addr, node))
    }
}

impl HasNodeGraphWithNodeAddrBound for FunctionBody {
    fn node_addr_bound(&self) -> NodeAddr {
        self.nodes
            .last_key_value()
            .map(|(k, _v)| k + 1)
            .unwrap_or(0)
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct Argument {
    pub(crate) name: Ident,
    pub(crate) ty: Type,
}

impl Argument {
    pub(crate) fn visit_var_decls(&self, f: &mut impl FnMut(&Ident, &Type)) {
        self.try_visit_var_decls(&mut |x, y| Ok::<(), Infallible>(f(x, y)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_var_decls<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Ident, &'a Type) -> Result<(), E>,
    ) -> Result<(), E> {
        f(&self.name, &self.ty)
    }

    pub(crate) fn visit_var_decls_mut(&mut self, f: &mut impl FnMut(&mut Ident, &mut Type)) {
        self.try_visit_var_decls_mut(&mut |x, y| Ok::<(), Infallible>(f(x, y)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_var_decls_mut<E>(
        &mut self,
        f: &mut impl FnMut(&mut Ident, &mut Type) -> Result<(), E>,
    ) -> Result<(), E> {
        f(&mut self.name, &mut self.ty)
    }
}

pub(crate) type NodeAddr = u64;

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum NodeId {
    Addr(NodeAddr),
    Ret,
    Err,
}

impl NodeId {
    pub(crate) fn addr(&self) -> Option<NodeAddr> {
        match self {
            Self::Addr(addr) => Some(*addr),
            _ => None,
        }
    }
}

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ret => write!(f, "Ret"),
            Self::Err => write!(f, "Err"),
            Self::Addr(addr) => write!(f, "{:#x?}", addr),
        }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum Node {
    Basic(BasicNode),
    Cond(CondNode),
    Call(CallNode),
}

impl Node {
    pub(crate) fn visit_exprs(&self, f: &mut impl FnMut(&Expr)) {
        self.try_visit_exprs(&mut |x| Ok::<(), Infallible>(f(x)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_exprs<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Expr) -> Result<(), E>,
    ) -> Result<(), E> {
        match self {
            Self::Basic(basic) => basic.try_visit_exprs(f),
            Self::Cond(cond) => cond.try_visit_exprs(f),
            Self::Call(call) => call.try_visit_exprs(f),
        }
    }

    pub(crate) fn visit_exprs_mut(&mut self, f: &mut impl FnMut(&mut Expr)) {
        self.try_visit_exprs_mut(&mut |x| Ok::<(), Infallible>(f(x)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_exprs_mut<E>(
        &mut self,
        f: &mut impl FnMut(&mut Expr) -> Result<(), E>,
    ) -> Result<(), E> {
        match self {
            Self::Basic(basic) => basic.try_visit_exprs_mut(f),
            Self::Cond(cond) => cond.try_visit_exprs_mut(f),
            Self::Call(call) => call.try_visit_exprs_mut(f),
        }
    }

    pub(crate) fn visit_var_decls(&self, f: &mut impl FnMut(&Ident, &Type)) {
        self.try_visit_var_decls(&mut |x, y| Ok::<(), Infallible>(f(x, y)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_var_decls<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Ident, &'a Type) -> Result<(), E>,
    ) -> Result<(), E> {
        match self {
            Self::Basic(basic) => basic.try_visit_var_decls(f),
            Self::Cond(cond) => cond.try_visit_var_decls(f),
            Self::Call(call) => call.try_visit_var_decls(f),
        }
    }

    pub(crate) fn visit_var_decls_mut(&mut self, f: &mut impl FnMut(&mut Ident, &mut Type)) {
        self.try_visit_var_decls_mut(&mut |x, y| Ok::<(), Infallible>(f(x, y)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_var_decls_mut<E>(
        &mut self,
        f: &mut impl FnMut(&mut Ident, &mut Type) -> Result<(), E>,
    ) -> Result<(), E> {
        match self {
            Self::Basic(basic) => basic.try_visit_var_decls_mut(f),
            Self::Cond(cond) => cond.try_visit_var_decls_mut(f),
            Self::Call(call) => call.try_visit_var_decls_mut(f),
        }
    }

    pub(crate) fn visit_conts(&self, mut f: impl FnMut(&NodeId)) {
        self.try_visit_conts(|x| Ok::<(), Infallible>(f(x)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_conts<E>(
        &self,
        mut f: impl FnMut(&NodeId) -> Result<(), E>,
    ) -> Result<(), E> {
        match self {
            Self::Basic(inner) => {
                f(&inner.next)?;
            }
            Self::Cond(inner) => {
                f(&inner.left)?;
                f(&inner.right)?;
            }
            Self::Call(inner) => {
                f(&inner.next)?;
            }
        }
        Ok(())
    }

    pub(crate) fn visit_conts_mut(&mut self, mut f: impl FnMut(&mut NodeId)) {
        self.try_visit_conts_mut(|x| Ok::<(), Infallible>(f(x)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_conts_mut<E>(
        &mut self,
        mut f: impl FnMut(&mut NodeId) -> Result<(), E>,
    ) -> Result<(), E> {
        match self {
            Self::Basic(inner) => {
                f(&mut inner.next)?;
            }
            Self::Cond(inner) => {
                f(&mut inner.left)?;
                f(&mut inner.right)?;
            }
            Self::Call(inner) => {
                f(&mut inner.next)?;
            }
        }
        Ok(())
    }

    pub(crate) fn conts(&self) -> ArrayVec<NodeId, 2> {
        let mut v = ArrayVec::new();
        self.visit_conts(|n| v.push(*n));
        v
    }

    pub(crate) fn is_noop(&self) -> bool {
        match self {
            Self::Basic(inner) => inner.var_updates.is_empty(),
            Self::Cond(inner) => inner.left == inner.right,
            Self::Call(_inner) => false,
        }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct BasicNode {
    pub(crate) next: NodeId,
    pub(crate) var_updates: Vec<VarUpdate>,
}

impl BasicNode {
    pub(crate) fn visit_exprs(&self, f: &mut impl FnMut(&Expr)) {
        self.try_visit_exprs(&mut |x| Ok::<(), Infallible>(f(x)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_exprs<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Expr) -> Result<(), E>,
    ) -> Result<(), E> {
        for var_update in &self.var_updates {
            var_update.expr.try_visit_exprs(f)?;
        }
        Ok(())
    }

    pub(crate) fn visit_exprs_mut(&mut self, f: &mut impl FnMut(&mut Expr)) {
        self.try_visit_exprs_mut(&mut |x| Ok::<(), Infallible>(f(x)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_exprs_mut<E>(
        &mut self,
        f: &mut impl FnMut(&mut Expr) -> Result<(), E>,
    ) -> Result<(), E> {
        for var_update in &mut self.var_updates {
            var_update.expr.try_visit_exprs_mut(f)?;
        }
        Ok(())
    }

    pub(crate) fn visit_var_decls(&self, f: &mut impl FnMut(&Ident, &Type)) {
        self.try_visit_var_decls(&mut |x, y| Ok::<(), Infallible>(f(x, y)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_var_decls<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Ident, &'a Type) -> Result<(), E>,
    ) -> Result<(), E> {
        for var_update in &self.var_updates {
            var_update.try_visit_var_decls(f)?;
        }
        Ok(())
    }

    pub(crate) fn visit_var_decls_mut(&mut self, f: &mut impl FnMut(&mut Ident, &mut Type)) {
        self.try_visit_var_decls_mut(&mut |x, y| Ok::<(), Infallible>(f(x, y)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_var_decls_mut<E>(
        &mut self,
        f: &mut impl FnMut(&mut Ident, &mut Type) -> Result<(), E>,
    ) -> Result<(), E> {
        for var_update in &mut self.var_updates {
            var_update.try_visit_var_decls_mut(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct CondNode {
    pub(crate) left: NodeId,
    pub(crate) right: NodeId,
    pub(crate) expr: Expr,
}

impl CondNode {
    pub(crate) fn visit_exprs(&self, f: &mut impl FnMut(&Expr)) {
        self.try_visit_exprs(&mut |x| Ok::<(), Infallible>(f(x)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_exprs<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Expr) -> Result<(), E>,
    ) -> Result<(), E> {
        self.expr.try_visit_exprs(f)
    }

    pub(crate) fn visit_exprs_mut(&mut self, f: &mut impl FnMut(&mut Expr)) {
        self.try_visit_exprs_mut(&mut |x| Ok::<(), Infallible>(f(x)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_exprs_mut<E>(
        &mut self,
        f: &mut impl FnMut(&mut Expr) -> Result<(), E>,
    ) -> Result<(), E> {
        self.expr.try_visit_exprs_mut(f)
    }

    pub(crate) fn visit_var_decls(&self, f: &mut impl FnMut(&Ident, &Type)) {
        self.try_visit_var_decls(&mut |x, y| Ok::<(), Infallible>(f(x, y)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_var_decls<'a, E>(
        &'a self,
        _f: &mut impl FnMut(&'a Ident, &'a Type) -> Result<(), E>,
    ) -> Result<(), E> {
        Ok(())
    }

    pub(crate) fn visit_var_decls_mut(&mut self, f: &mut impl FnMut(&mut Ident, &mut Type)) {
        self.try_visit_var_decls_mut(&mut |x, y| Ok::<(), Infallible>(f(x, y)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_var_decls_mut<E>(
        &mut self,
        _f: &mut impl FnMut(&mut Ident, &mut Type) -> Result<(), E>,
    ) -> Result<(), E> {
        Ok(())
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct CallNode {
    pub(crate) next: NodeId,
    pub(crate) function_name: Ident,
    pub(crate) input: Vec<Expr>,
    pub(crate) output: Vec<Argument>,
}

impl CallNode {
    pub(crate) fn visit_exprs(&self, f: &mut impl FnMut(&Expr)) {
        self.try_visit_exprs(&mut |x| Ok::<(), Infallible>(f(x)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_exprs<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Expr) -> Result<(), E>,
    ) -> Result<(), E> {
        for expr in &self.input {
            expr.try_visit_exprs(f)?;
        }
        Ok(())
    }

    pub(crate) fn visit_exprs_mut(&mut self, f: &mut impl FnMut(&mut Expr)) {
        self.try_visit_exprs_mut(&mut |x| Ok::<(), Infallible>(f(x)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_exprs_mut<E>(
        &mut self,
        f: &mut impl FnMut(&mut Expr) -> Result<(), E>,
    ) -> Result<(), E> {
        for expr in &mut self.input {
            expr.try_visit_exprs_mut(f)?;
        }
        Ok(())
    }

    pub(crate) fn visit_var_decls(&self, f: &mut impl FnMut(&Ident, &Type)) {
        self.try_visit_var_decls(&mut |x, y| Ok::<(), Infallible>(f(x, y)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_var_decls<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Ident, &'a Type) -> Result<(), E>,
    ) -> Result<(), E> {
        for arg in &self.output {
            arg.try_visit_var_decls(f)?;
        }
        Ok(())
    }

    pub(crate) fn visit_var_decls_mut(&mut self, f: &mut impl FnMut(&mut Ident, &mut Type)) {
        self.try_visit_var_decls_mut(&mut |x, y| Ok::<(), Infallible>(f(x, y)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_var_decls_mut<E>(
        &mut self,
        f: &mut impl FnMut(&mut Ident, &mut Type) -> Result<(), E>,
    ) -> Result<(), E> {
        for arg in &mut self.output {
            arg.try_visit_var_decls_mut(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct VarUpdate {
    pub(crate) var_name: Ident,
    pub(crate) ty: Type,
    pub(crate) expr: Expr,
}

impl VarUpdate {
    pub(crate) fn visit_var_decls(&self, f: &mut impl FnMut(&Ident, &Type)) {
        self.try_visit_var_decls(&mut |x, y| Ok::<(), Infallible>(f(x, y)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_var_decls<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Ident, &'a Type) -> Result<(), E>,
    ) -> Result<(), E> {
        f(&self.var_name, &self.ty)
    }

    pub(crate) fn visit_var_decls_mut(&mut self, f: &mut impl FnMut(&mut Ident, &mut Type)) {
        self.try_visit_var_decls_mut(&mut |x, y| Ok::<(), Infallible>(f(x, y)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_var_decls_mut<E>(
        &mut self,
        f: &mut impl FnMut(&mut Ident, &mut Type) -> Result<(), E>,
    ) -> Result<(), E> {
        f(&mut self.var_name, &mut self.ty)
    }
}

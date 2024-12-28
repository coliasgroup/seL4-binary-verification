use std::collections::{btree_map, BTreeMap};
use std::convert::Infallible;
use std::ops::{Add, BitAnd, BitOr, Neg, Not, Sub};
use std::slice::SliceIndex;
use std::{fmt, iter};

use arrayvec::ArrayVec;
use num::BigInt;

use crate::arch::WORD_SIZE_BITS;
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

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct Expr {
    pub(crate) ty: Type,
    pub(crate) value: ExprValue,
}

impl Expr {
    pub(crate) fn new(ty: Type, value: ExprValue) -> Self {
        Self { ty, value }
    }

    pub(crate) fn visit_exprs(&self, f: &mut impl FnMut(&Expr)) {
        self.try_visit_exprs(&mut |x| Ok::<(), Infallible>(f(x)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_exprs<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Expr) -> Result<(), E>,
    ) -> Result<(), E> {
        if let ExprValue::Op(_, exprs) = &self.value {
            for expr in exprs {
                expr.try_visit_exprs(f)?;
            }
        }
        f(self)
    }

    pub(crate) fn visit_exprs_mut(&mut self, f: &mut impl FnMut(&mut Expr)) {
        self.try_visit_exprs_mut(&mut |x| Ok::<(), Infallible>(f(x)))
            .unwrap_or_else(|err| match err {})
    }

    pub(crate) fn try_visit_exprs_mut<E>(
        &mut self,
        f: &mut impl FnMut(&mut Expr) -> Result<(), E>,
    ) -> Result<(), E> {
        if let ExprValue::Op(_, exprs) = &mut self.value {
            for expr in exprs {
                expr.try_visit_exprs_mut(f)?;
            }
        }
        f(self)
    }

    pub(crate) fn mk_true() -> Self {
        Self::new(Type::Bool, Op::True.mk(&[]))
    }

    pub(crate) fn mk_false() -> Self {
        Self::new(Type::Bool, Op::False.mk(&[]))
    }

    pub(crate) fn mk_word(val: Num, bits: u64) -> Self {
        Self::new(Type::Word(bits), ExprValue::Num(val))
    }

    pub(crate) fn mk_eq(self, rhs: Self) -> Self {
        assert_eq!(self.ty, rhs.ty);
        Self::new(Type::Bool, Op::Equals.mk(&[self, rhs]))
    }

    pub(crate) fn mk_aligned(self, n: u64) -> Self {
        assert!(self.ty.is_word());
        let bits = self.ty.as_word().unwrap();
        let mask = Self::mk_word((Num::from(1) << n) - 1, bits);
        self.mk_bitwise_and(mask)
            .mk_eq(Self::mk_word(0.into(), bits))
    }

    pub(crate) fn mk_and(self, rhs: Self) -> Self {
        assert_eq!(self.ty, Type::Bool);
        assert_eq!(rhs.ty, Type::Bool);
        Self::new(Type::Bool, Op::And.mk(&[self, rhs]))
    }

    pub(crate) fn mk_or(self, rhs: Self) -> Self {
        assert_eq!(self.ty, Type::Bool);
        assert_eq!(rhs.ty, Type::Bool);
        Self::new(Type::Bool, Op::Or.mk(&[self, rhs]))
    }

    pub(crate) fn mk_implies(self, rhs: Self) -> Self {
        assert_eq!(self.ty, Type::Bool);
        assert_eq!(rhs.ty, Type::Bool);
        Self::new(Type::Bool, Op::Implies.mk(&[self, rhs]))
    }

    pub(crate) fn mk_plus(self, rhs: Self) -> Self {
        assert!(self.ty.is_word());
        assert_eq!(self.ty, rhs.ty);
        Self::new(self.ty.clone(), Op::Plus.mk(&[self, rhs]))
    }

    pub(crate) fn mk_less(self, rhs: Self) -> Self {
        self.mk_less_with_signedness(rhs, false)
    }

    pub(crate) fn mk_less_signed(self, rhs: Self) -> Self {
        self.mk_less_with_signedness(rhs, true)
    }

    pub(crate) fn mk_less_with_signedness(self, rhs: Self, signed: bool) -> Self {
        assert!(self.ty.is_word());
        assert_eq!(self.ty, rhs.ty);
        let op = if signed { Op::SignedLess } else { Op::Less };
        Self::new(Type::Bool, op.mk(&[self, rhs]))
    }

    pub(crate) fn mk_less_eq(self, rhs: Self) -> Self {
        self.mk_less_eq_with_signedness(rhs, false)
    }

    pub(crate) fn mk_less_eq_signed(self, rhs: Self) -> Self {
        self.mk_less_eq_with_signedness(rhs, true)
    }

    pub(crate) fn mk_less_eq_with_signedness(self, rhs: Self, signed: bool) -> Self {
        assert!(self.ty.is_word());
        assert_eq!(self.ty, rhs.ty);
        let op = if signed {
            Op::SignedLessEquals
        } else {
            Op::LessEquals
        };
        Self::new(Type::Bool, op.mk(&[self, rhs]))
    }

    pub(crate) fn mk_bitwise_and(self, rhs: Self) -> Self {
        assert!(self.ty.is_word());
        assert_eq!(self.ty, rhs.ty);
        Self::new(self.ty.clone(), Op::BWAnd.mk(&[self, rhs]))
    }

    pub(crate) fn mk_token(name: Ident) -> Self {
        Self::new(Type::Token, ExprValue::Token(name))
    }

    pub(crate) fn mk_var(name: Ident, ty: Type) -> Self {
        Self::new(ty, ExprValue::Var(name))
    }

    pub(crate) fn mk_var_from_arg(arg: &Argument) -> Self {
        Self::mk_var(arg.name.clone(), arg.ty.clone())
    }

    pub(crate) fn mk_word_var(name: Ident, bits: u64) -> Self {
        Self::mk_var(name, Type::Word(bits))
    }

    pub(crate) fn mk_machine_word_var(name: Ident) -> Self {
        Self::mk_var(name, Type::mk_machine_word())
    }

    pub(crate) fn mk_memacc(self, addr: Self, ty: Type) -> Self {
        assert!(self.ty.is_mem());
        assert!(addr.ty.is_word_with_size(WORD_SIZE_BITS));
        assert!(ty.is_word());
        Self::new(ty, Op::MemAcc.mk([self, addr]))
    }

    pub(crate) fn mk_rodata(self) -> Self {
        assert_eq!(self.ty, Type::Mem);
        Self::new(Type::Bool, Op::ROData.mk(&[self]))
    }

    pub(crate) fn mk_if(self, true_: Self, false_: Self) -> Self {
        assert!(self.ty.is_bool());
        assert_eq!(true_.ty, false_.ty);
        Self::new(true_.ty.clone(), Op::IfThenElse.mk([true_, false_]))
    }

    pub(crate) fn mk_cast(self, ty: Type) -> Self {
        if self.ty == ty {
            self
        } else {
            assert!(self.ty.is_word());
            assert!(ty.is_word());
            Self::new(ty, Op::WordCast.mk([self]))
        }
    }

    pub(crate) fn cast_c_val(self, ty: Type) -> Self {
        assert!(ty.is_word());
        if self.ty.is_bool() {
            self.mk_if(Self::new(ty.clone(), ExprValue::Num(1.into())), Self::new(ty.clone(), ExprValue::Num(0.into())))
        } else {
            self.mk_cast(ty)
        }
    }
}

impl BitAnd for Expr {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        self.mk_and(rhs)
    }
}

impl BitOr for Expr {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.mk_or(rhs)
    }
}

impl Not for Expr {
    type Output = Self;

    fn not(self) -> Self::Output {
        assert_eq!(self.ty, Type::Bool);
        Self::new(self.ty.clone(), Op::Not.mk(&[self]))
    }
}

impl Add for Expr {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        assert_eq!(self.ty, rhs.ty);
        Self::new(self.ty.clone(), Op::Plus.mk(&[self, rhs]))
    }
}

impl Sub for Expr {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        assert_eq!(self.ty, rhs.ty);
        Self::new(self.ty.clone(), Op::Minus.mk(&[self, rhs]))
    }
}

impl Neg for Expr {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Expr::new(self.ty.clone(), ExprValue::Num(0.into())) - self
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum Type {
    Bool,
    Mem,
    Dom,
    Htd,
    Pms,
    Unit,
    Type,
    Token,
    RelWrapper,
    Word(u64),
    WordArray { length: u64, bits: u64 },
    Array(Box<Self>, u64),
    Struct(Ident),
    Ptr(Box<Self>),
}

impl Type {
    pub(crate) fn mk_machine_word() -> Self {
        Self::Word(WORD_SIZE_BITS)
    }

    pub(crate) fn as_word(&self) -> Option<u64> {
        match self {
            Self::Word(bits) => Some(*bits),
            _ => None,
        }
    }

    pub(crate) fn is_word(&self) -> bool {
        self.as_word().is_some()
    }

    pub(crate) fn is_word_with_size(&self, bits: u64) -> bool {
        matches!(self, Self::Word(actual_bits) if &bits == actual_bits)
    }

    pub(crate) fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }

    pub(crate) fn is_mem(&self) -> bool {
        matches!(self, Self::Mem)
    }

    pub(crate) fn is_dom(&self) -> bool {
        matches!(self, Self::Dom)
    }

    pub(crate) fn is_htd(&self) -> bool {
        matches!(self, Self::Htd)
    }

    pub(crate) fn is_type(&self) -> bool {
        matches!(self, Self::Type)
    }

    pub(crate) fn as_ptr(&self) -> Option<&Self> {
        match self {
            Self::Ptr(ty) => Some(ty),
            _ => None,
        }
    }

    pub(crate) fn is_ptr(&self) -> bool {
        self.as_ptr().is_some()
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum ExprValue {
    Var(Ident),
    Op(Op, Vec<Expr>),
    Num(Num),
    Type(Type),
    Symbol(Ident),
    Token(Ident),
}

pub(crate) type Num = BigInt;

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum Op {
    Plus,
    Minus,
    Times,
    Modulus,
    DividedBy,
    BWAnd,
    BWOr,
    BWXOR,
    And,
    Or,
    Implies,
    Equals,
    Less,
    LessEquals,
    SignedLess,
    SignedLessEquals,
    ShiftLeft,
    ShiftRight,
    CountLeadingZeroes,
    CountTrailingZeroes,
    WordReverse,
    SignedShiftRight,
    Not,
    BWNot,
    WordCast,
    WordCastSigned,
    True,
    False,
    UnspecifiedPrecond,
    MemUpdate,
    MemAcc,
    IfThenElse,
    ArrayIndex,
    ArrayUpdate,
    MemDom,
    PValid,
    PWeakValid,
    PAlignValid,
    PGlobalValid,
    PArrayValid,
    HTDUpdate,
    WordArrayAccess,
    WordArrayUpdate,
    TokenWordsAccess,
    TokenWordsUpdate,
    ROData,
    StackWrapper,
    ToFloatingPoint,
    ToFloatingPointSigned,
    ToFloatingPointUnsigned,
    FloatingPointCast,
}

impl Op {
    pub(crate) fn mk(self, operands: impl AsRef<[Expr]>) -> ExprValue {
        ExprValue::Op(self, operands.as_ref().to_owned())
    }

    pub(crate) fn num_operands(&self) -> usize {
        match self {
            Self::Plus => 2,
            Self::Minus => 2,
            Self::Times => 2,
            Self::Modulus => 2,
            Self::DividedBy => 2,
            Self::BWAnd => 2,
            Self::BWOr => 2,
            Self::BWXOR => 2,
            Self::And => 2,
            Self::Or => 2,
            Self::Implies => 2,
            Self::Equals => 2,
            Self::Less => 2,
            Self::LessEquals => 2,
            Self::SignedLess => 2,
            Self::SignedLessEquals => 2,
            Self::ShiftLeft => 2,
            Self::ShiftRight => 2,
            Self::CountLeadingZeroes => 1,
            Self::CountTrailingZeroes => 1,
            Self::WordReverse => 1,
            Self::SignedShiftRight => 2,
            Self::Not => 1,
            Self::BWNot => 1,
            Self::WordCast => 1,
            Self::WordCastSigned => 1,
            Self::True => 0,
            Self::False => 0,
            Self::UnspecifiedPrecond => 0,
            Self::MemUpdate => 3,
            Self::MemAcc => 2,
            Self::IfThenElse => 3,
            Self::ArrayIndex => 2,
            Self::ArrayUpdate => 3,
            Self::MemDom => 2,
            Self::PValid => 3,
            Self::PWeakValid => 3,
            Self::PAlignValid => 2,
            Self::PGlobalValid => 3,
            Self::PArrayValid => 4,
            Self::HTDUpdate => 5,
            Self::WordArrayAccess => 2,
            Self::WordArrayUpdate => 3,
            Self::TokenWordsAccess => 2,
            Self::TokenWordsUpdate => 3,
            Self::ROData => 1,
            Self::StackWrapper => 2,
            Self::ToFloatingPoint => 1,
            Self::ToFloatingPointSigned => 2,
            Self::ToFloatingPointUnsigned => 2,
            Self::FloatingPointCast => 1,
        }
    }

    pub(crate) fn typecheck(&self, ty: &Type, operands: &[Expr]) -> Result<(), OpTypeError> {
        ExprChecker {
            op: self,
            ty,
            operands,
        }
        .check_all()
    }
}

struct ExprChecker<'a> {
    op: &'a Op,
    ty: &'a Type,
    operands: &'a [Expr],
}

impl<'a> ExprChecker<'a> {
    fn check_op(&self, f: impl FnOnce(&Type) -> bool) -> Result<(), OpTypeError> {
        let ty = self.ty;
        if f(ty) {
            Ok(())
        } else {
            Err(OpTypeError::OperationTypeMismatch { op: *self.op })
        }
    }

    fn check(&self, i: usize, f: impl FnOnce(&Type) -> bool) -> Result<(), OpTypeError> {
        let ty = &self.operands[i].ty;
        if f(ty) {
            Ok(())
        } else {
            Err(OpTypeError::IncorrectTypeOfOperand {
                op: *self.op,
                operand_type: ty.clone(),
            })
        }
    }

    fn ensure_equal<Ix>(&self, indices: Ix) -> Result<&Type, OpTypeError>
    where
        Ix: SliceIndex<[Expr], Output = [Expr]>,
    {
        let mut it = self.operands[indices].as_ref().iter().map(|expr| &expr.ty);
        let ty = it.next().unwrap();
        for ty_j in it {
            if ty_j != ty {
                return Err(OpTypeError::OperandTypeMismatch {
                    op: *self.op,
                    operand_type_1: ty.clone(),
                    operand_type_2: ty_j.clone(),
                });
            }
        }
        Ok(ty)
    }

    fn ensure_equal_and<Ix>(
        &self,
        indices: Ix,
        f: impl FnOnce(&Type) -> bool,
    ) -> Result<(), OpTypeError>
    where
        Ix: SliceIndex<[Expr], Output = [Expr]>,
    {
        let ty = self.ensure_equal(indices)?;
        if f(ty) {
            Ok(())
        } else {
            Err(OpTypeError::IncorrectTypeOfOperand {
                op: *self.op,
                operand_type: ty.clone(),
            })
        }
    }

    fn ensure_equal_to_op<Ix>(&self, indices: Ix) -> Result<(), OpTypeError>
    where
        Ix: SliceIndex<[Expr], Output = [Expr]>,
    {
        self.ensure_equal_to_op_and(indices, |_| true)
    }

    fn ensure_equal_to_op_and<Ix>(
        &self,
        indices: Ix,
        f: impl FnOnce(&Type) -> bool,
    ) -> Result<(), OpTypeError>
    where
        Ix: SliceIndex<[Expr], Output = [Expr]>,
    {
        let ty = self.ensure_equal(indices)?;
        if ty == self.ty && f(ty) {
            Ok(())
        } else {
            Err(OpTypeError::IncorrectTypeOfOperand {
                op: *self.op,
                operand_type: ty.clone(),
            })
        }
    }

    // TODO mark args and op as having been checked and then ensure all have been checked
    fn check_all(&self) -> Result<(), OpTypeError> {
        if self.op.num_operands() != self.operands.len() {
            return Err(OpTypeError::IncorrectNumberOfOperands {
                op: *self.op,
                num_operands: self.operands.len(),
            });
        }
        match self.op {
            Op::Plus => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::Minus => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::Times => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::Modulus => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::DividedBy => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::BWAnd => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::BWOr => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::BWXOR => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::And => {
                self.ensure_equal_to_op_and(.., Type::is_bool)?;
            }
            Op::Or => {
                self.ensure_equal_to_op_and(.., Type::is_bool)?;
            }
            Op::Implies => {
                self.ensure_equal_to_op_and(.., Type::is_bool)?;
            }
            Op::Equals => {
                self.check_op(Type::is_bool)?;
                self.ensure_equal(..)?;
            }
            Op::Less => {
                self.check_op(Type::is_bool)?;
                self.ensure_equal_and(.., Type::is_word)?;
            }
            Op::LessEquals => {
                self.check_op(Type::is_bool)?;
                self.ensure_equal_and(.., Type::is_word)?;
            }
            Op::SignedLess => {
                self.check_op(Type::is_bool)?;
                self.ensure_equal_and(.., Type::is_word)?;
            }
            Op::SignedLessEquals => {
                self.check_op(Type::is_bool)?;
                self.ensure_equal_and(.., Type::is_word)?;
            }
            Op::ShiftLeft => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::ShiftRight => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::CountLeadingZeroes => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::CountTrailingZeroes => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::WordReverse => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::SignedShiftRight => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::Not => {
                self.ensure_equal_to_op_and(.., Type::is_bool)?;
            }
            Op::BWNot => {
                self.ensure_equal_to_op_and(.., Type::is_word)?;
            }
            Op::WordCast => {
                self.check_op(Type::is_word)?;
                self.check(0, Type::is_word)?;
            }
            Op::WordCastSigned => {
                self.check_op(Type::is_word)?;
                self.check(0, Type::is_word)?;
            }
            Op::True => {
                self.check_op(Type::is_bool)?;
            }
            Op::False => {
                self.check_op(Type::is_bool)?;
            }
            Op::UnspecifiedPrecond => {
                self.check_op(Type::is_bool)?;
            }
            Op::MemUpdate => {
                self.check(0, Type::is_mem)?;
                self.check(1, |ty| ty.is_word_with_size(32))?;
                self.check(2, Type::is_word)?;
                self.check_op(Type::is_mem)?;
            }
            Op::MemAcc => {
                self.check(0, Type::is_mem)?;
                self.check(1, |ty| ty.is_word_with_size(32))?;
                self.check_op(Type::is_word)?;
            }
            Op::IfThenElse => {
                self.check(0, Type::is_bool)?;
                self.ensure_equal_to_op(1..)?;
            }
            Op::ArrayIndex => {
                todo!()
            }
            Op::ArrayUpdate => {
                todo!()
            }
            Op::MemDom => {
                self.check(0, |ty| ty.is_word_with_size(32))?;
                self.check(1, Type::is_dom)?;
                self.check_op(Type::is_bool)?;
            }
            Op::PValid => {
                self.check(0, Type::is_htd)?;
                self.check(1, Type::is_type)?;
                self.check(2, |ty| ty.is_word_with_size(32))?;
                self.check_op(Type::is_bool)?;
            }
            Op::PWeakValid => {
                self.check(0, Type::is_htd)?;
                self.check(1, Type::is_type)?;
                self.check(2, |ty| ty.is_word_with_size(32))?;
                self.check_op(Type::is_bool)?;
            }
            Op::PAlignValid => {
                self.check(0, Type::is_type)?;
                self.check(1, |ty| ty.is_word_with_size(32))?;
                self.check_op(Type::is_bool)?;
            }
            Op::PGlobalValid => {
                self.check(0, Type::is_htd)?;
                self.check(1, Type::is_type)?;
                self.check(2, |ty| ty.is_word_with_size(32))?;
                self.check_op(Type::is_bool)?;
            }
            Op::PArrayValid => {
                self.check(0, Type::is_htd)?;
                self.check(1, Type::is_type)?;
                self.check(2, |ty| ty.is_word_with_size(32))?;
                self.check(3, |ty| ty.is_word_with_size(32))?;
                self.check_op(Type::is_bool)?;
            }
            Op::HTDUpdate => {
                self.check(0, Type::is_type)?;
                self.check(1, |ty| ty.is_word_with_size(32))?;
                self.check(2, |ty| ty.is_word_with_size(32))?;
                self.check(3, |ty| ty.is_word_with_size(32))?;
                self.check(4, Type::is_htd)?;
                self.check_op(Type::is_htd)?;
            }
            Op::WordArrayAccess => {
                let (length, bits) = {
                    let ty = &self.operands[0].ty;
                    match &self.operands[0].ty {
                        Type::WordArray { length, bits } => (length, bits),
                        _ => {
                            return Err(OpTypeError::IncorrectTypeOfOperand {
                                op: *self.op,
                                operand_type: ty.clone(),
                            })
                        }
                    }
                };
                self.check(1, |ty| ty.is_word_with_size(*length))?;
                self.check_op(|ty| ty.is_word_with_size(*bits))?;
            }
            Op::WordArrayUpdate => {
                let (length, bits) = {
                    let ty = &self.operands[0].ty;
                    match &self.operands[0].ty {
                        Type::WordArray { length, bits } => (length, bits),
                        _ => {
                            return Err(OpTypeError::IncorrectTypeOfOperand {
                                op: *self.op,
                                operand_type: ty.clone(),
                            })
                        }
                    }
                };
                self.check(1, |ty| ty.is_word_with_size(*length))?;
                self.check(2, |ty| ty.is_word_with_size(*bits))?;
                self.check_op(|ty| ty == &self.operands[0].ty)?;
            }
            Op::TokenWordsAccess => {
                todo!()
            }
            Op::TokenWordsUpdate => {
                todo!()
            }
            Op::ROData => {
                todo!()
            }
            Op::StackWrapper => {
                todo!()
            }
            Op::ToFloatingPoint => {
                todo!()
            }
            Op::ToFloatingPointSigned => {
                todo!()
            }
            Op::ToFloatingPointUnsigned => {
                todo!()
            }
            Op::FloatingPointCast => {
                todo!()
            }
        }
        Ok(())
    }
}

impl Expr {
    pub(crate) fn typecheck(&self) -> Result<(), OpTypeError> {
        match &self.value {
            ExprValue::Op(op, operands) => op.typecheck(&self.ty, operands),
            ExprValue::Num(_) => match &self.ty {
                Type::Word(_) => Ok(()),
                _ => Err(OpTypeError::MistypedNum),
            },
            ExprValue::Type(_) => match &self.ty {
                Type::Type => Ok(()),
                _ => Err(OpTypeError::MistypedType),
            },
            _ => Ok(()),
        }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum OpTypeError {
    IncorrectNumberOfOperands {
        op: Op,
        num_operands: usize,
    },
    IncorrectTypeOfOperand {
        op: Op,
        operand_type: Type,
    },
    OperandTypeMismatch {
        op: Op,
        operand_type_1: Type,
        operand_type_2: Type,
    },
    OperationTypeMismatch {
        op: Op,
    },
    MistypedNum,
    MistypedType,
}

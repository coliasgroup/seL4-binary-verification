use std::collections::{btree_map, BTreeMap};
use std::ops::{Add, BitAnd, BitOr, Neg, Not, Sub};
use std::{fmt, iter};

use arrayvec::ArrayVec;
use num::BigInt;

use crate::arch::WORD_SIZE_BITS;
use crate::graph::{HasNodeGraph, HasNodeGraphWithNodeAddrBound};

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
        f(&self.name, &self.ty)
    }

    pub(crate) fn visit_var_decls_mut(&mut self, f: &mut impl FnMut(&mut Ident, &mut Type)) {
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
        match self {
            Self::Basic(basic) => basic.visit_exprs(f),
            Self::Cond(cond) => cond.visit_exprs(f),
            Self::Call(call) => call.visit_exprs(f),
        }
    }

    pub(crate) fn visit_exprs_mut(&mut self, f: &mut impl FnMut(&mut Expr)) {
        match self {
            Self::Basic(basic) => basic.visit_exprs_mut(f),
            Self::Cond(cond) => cond.visit_exprs_mut(f),
            Self::Call(call) => call.visit_exprs_mut(f),
        }
    }

    pub(crate) fn visit_var_decls(&self, f: &mut impl FnMut(&Ident, &Type)) {
        match self {
            Self::Basic(basic) => basic.visit_var_decls(f),
            Self::Cond(cond) => cond.visit_var_decls(f),
            Self::Call(call) => call.visit_var_decls(f),
        }
    }

    pub(crate) fn visit_var_decls_mut(&mut self, f: &mut impl FnMut(&mut Ident, &mut Type)) {
        match self {
            Self::Basic(basic) => basic.visit_var_decls_mut(f),
            Self::Cond(cond) => cond.visit_var_decls_mut(f),
            Self::Call(call) => call.visit_var_decls_mut(f),
        }
    }

    pub(crate) fn visit_conts(&self, mut f: impl FnMut(&NodeId)) {
        match self {
            Self::Basic(inner) => {
                f(&inner.next);
            }
            Self::Cond(inner) => {
                f(&inner.left);
                f(&inner.right);
            }
            Self::Call(inner) => {
                f(&inner.next);
            }
        }
    }

    pub(crate) fn visit_conts_mut(&mut self, mut f: impl FnMut(&mut NodeId)) {
        match self {
            Self::Basic(inner) => {
                f(&mut inner.next);
            }
            Self::Cond(inner) => {
                f(&mut inner.left);
                f(&mut inner.right);
            }
            Self::Call(inner) => {
                f(&mut inner.next);
            }
        }
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
        for var_update in &self.var_updates {
            var_update.expr.visit_exprs(f);
        }
    }

    pub(crate) fn visit_exprs_mut(&mut self, f: &mut impl FnMut(&mut Expr)) {
        for var_update in &mut self.var_updates {
            var_update.expr.visit_exprs_mut(f);
        }
    }

    pub(crate) fn visit_var_decls(&self, f: &mut impl FnMut(&Ident, &Type)) {
        for var_update in &self.var_updates {
            var_update.visit_var_decls(f);
        }
    }

    pub(crate) fn visit_var_decls_mut(&mut self, f: &mut impl FnMut(&mut Ident, &mut Type)) {
        for var_update in &mut self.var_updates {
            var_update.visit_var_decls_mut(f);
        }
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
        self.expr.visit_exprs(f);
    }

    pub(crate) fn visit_exprs_mut(&mut self, f: &mut impl FnMut(&mut Expr)) {
        self.expr.visit_exprs_mut(f);
    }

    pub(crate) fn visit_var_decls(&self, _f: &mut impl FnMut(&Ident, &Type)) {}

    pub(crate) fn visit_var_decls_mut(&mut self, _f: &mut impl FnMut(&mut Ident, &mut Type)) {}
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
        for expr in &self.input {
            expr.visit_exprs(f);
        }
    }

    pub(crate) fn visit_exprs_mut(&mut self, f: &mut impl FnMut(&mut Expr)) {
        for expr in &mut self.input {
            expr.visit_exprs_mut(f);
        }
    }

    pub(crate) fn visit_var_decls(&self, f: &mut impl FnMut(&Ident, &Type)) {
        for arg in &self.output {
            arg.visit_var_decls(f);
        }
    }

    pub(crate) fn visit_var_decls_mut(&mut self, f: &mut impl FnMut(&mut Ident, &mut Type)) {
        for arg in &mut self.output {
            arg.visit_var_decls_mut(f);
        }
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
        f(&self.var_name, &self.ty)
    }

    pub(crate) fn visit_var_decls_mut(&mut self, f: &mut impl FnMut(&mut Ident, &mut Type)) {
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
        if let ExprValue::Op(_, exprs) = &self.value {
            for expr in exprs {
                expr.visit_exprs(f);
            }
        }
        f(self);
    }

    pub(crate) fn visit_exprs_mut(&mut self, f: &mut impl FnMut(&mut Expr)) {
        if let ExprValue::Op(_, exprs) = &mut self.value {
            for expr in exprs {
                expr.visit_exprs_mut(f);
            }
        }
        f(self);
    }

    pub(crate) fn mk_eq(self, rhs: Self) -> Self {
        assert_eq!(self.ty, rhs.ty);
        Self::new(
            Type::Bool,
            ExprValue::Op("Equals".to_owned(), vec![self, rhs]),
        )
    }

    pub(crate) fn mk_and(self, rhs: Self) -> Self {
        assert_eq!(self.ty, Type::Bool);
        assert_eq!(rhs.ty, Type::Bool);
        Self::new(Type::Bool, ExprValue::Op("And".to_owned(), vec![self, rhs]))
    }

    pub(crate) fn mk_or(self, rhs: Self) -> Self {
        assert_eq!(self.ty, Type::Bool);
        assert_eq!(rhs.ty, Type::Bool);
        Self::new(Type::Bool, ExprValue::Op("Or".to_owned(), vec![self, rhs]))
    }

    pub(crate) fn mk_implies(self, rhs: Self) -> Self {
        assert_eq!(self.ty, Type::Bool);
        assert_eq!(rhs.ty, Type::Bool);
        Self::new(
            Type::Bool,
            ExprValue::Op("Implies".to_owned(), vec![self, rhs]),
        )
    }

    pub(crate) fn mk_less(self, rhs: Self) -> Self {
        self.mk_less_with_signedness(rhs, false)
    }

    pub(crate) fn mk_less_signed(self, rhs: Self) -> Self {
        self.mk_less_with_signedness(rhs, true)
    }

    pub(crate) fn mk_less_with_signedness(self, rhs: Self, signed: bool) -> Self {
        assert_eq!(self.ty, rhs.ty);
        let name = if signed { "SignedLess" } else { "Less" };
        Self::new(Type::Bool, ExprValue::Op(name.to_owned(), vec![self, rhs]))
    }

    pub(crate) fn mk_less_eq(self, rhs: Self) -> Self {
        self.mk_less_eq_with_signedness(rhs, false)
    }

    pub(crate) fn mk_less_eq_signed(self, rhs: Self) -> Self {
        self.mk_less_eq_with_signedness(rhs, true)
    }

    pub(crate) fn mk_less_eq_with_signedness(self, rhs: Self, signed: bool) -> Self {
        assert_eq!(self.ty, rhs.ty);
        let name = if signed {
            "SignedLessEquals"
        } else {
            "LessEquals"
        };
        Self::new(Type::Bool, ExprValue::Op(name.to_owned(), vec![self, rhs]))
    }

    pub(crate) fn mk_bitwise_and(self, rhs: Self) -> Self {
        assert_eq!(self.ty, rhs.ty);
        assert!(matches!(self.ty, Type::Word(_)));
        Self::new(
            self.ty.clone(),
            ExprValue::Op("BWAnd".to_owned(), vec![self, rhs]),
        )
    }

    pub(crate) fn mk_token(name: Ident) -> Self {
        Self::new(Type::Token, ExprValue::Token(name))
    }

    pub(crate) fn mk_var(name: Ident, ty: Type) -> Self {
        Self::new(ty, ExprValue::Var(name))
    }

    pub(crate) fn mk_word_var(name: Ident, bits: u64) -> Self {
        Self::mk_var(name, Type::Word(bits))
    }

    pub(crate) fn mk_machine_word_var(name: Ident) -> Self {
        Self::mk_var(name, Type::mk_machine_word())
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
        Self::new(self.ty.clone(), ExprValue::Op("Not".to_owned(), vec![self]))
    }
}

impl Add for Expr {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        assert_eq!(self.ty, rhs.ty);
        Self::new(
            self.ty.clone(),
            ExprValue::Op("Plus".to_owned(), vec![self, rhs]),
        )
    }
}

impl Sub for Expr {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        assert_eq!(self.ty, rhs.ty);
        Self::new(
            self.ty.clone(),
            ExprValue::Op("Minus".to_owned(), vec![self, rhs]),
        )
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
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum ExprValue {
    Var(Ident),
    Op(OpName, Vec<Expr>),
    Num(Num),
    Type,
    Symbol(Ident),
    Token(Ident),
}

pub(crate) type Num = BigInt;

pub(crate) type OpName = String;

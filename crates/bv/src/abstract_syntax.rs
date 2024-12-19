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
        Self::new(Type::Bool, Op::Equals.mk(&[self, rhs]))
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

    pub(crate) fn mk_less(self, rhs: Self) -> Self {
        self.mk_less_with_signedness(rhs, false)
    }

    pub(crate) fn mk_less_signed(self, rhs: Self) -> Self {
        self.mk_less_with_signedness(rhs, true)
    }

    pub(crate) fn mk_less_with_signedness(self, rhs: Self, signed: bool) -> Self {
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
        assert_eq!(self.ty, rhs.ty);
        let op = if signed {
            Op::SignedLessEquals
        } else {
            Op::LessEquals
        };
        Self::new(Type::Bool, op.mk(&[self, rhs]))
    }

    pub(crate) fn mk_bitwise_and(self, rhs: Self) -> Self {
        assert_eq!(self.ty, rhs.ty);
        assert!(matches!(self.ty, Type::Word(_)));
        Self::new(self.ty.clone(), Op::BWAnd.mk(&[self, rhs]))
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
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum ExprValue {
    Var(Ident),
    Op(Op, Vec<Expr>),
    Num(Num),
    Type,
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

    pub(crate) fn typecheck(&self, operands: &[Expr]) -> Result<(), OpTypeError> {
        if self.num_operands() != operands.len() {
            return Err(OpTypeError::IncorrectNumberOfOperands { op: *self, num_operands: operands.len() })
        }

        let checker = OperandChecker {
            op: self,
            operands,
        };

        match self {
            Self::Plus => {
                checker.check(0, |t| true)?;
            }
            Self::Minus => {
                checker.check(0, |t| true)?;
            }
            Self::Times => {
                checker.check(0, |t| true)?;
            }
            Self::Modulus => {
                checker.check(0, |t| true)?;
            }
            Self::DividedBy => {
                checker.check(0, |t| true)?;
            }
            Self::BWAnd => {
                checker.check(0, |t| true)?;
            }
            Self::BWOr => {
                checker.check(0, |t| true)?;
            }
            Self::BWXOR => {
                checker.check(0, |t| true)?;
            }
            Self::And => {
                checker.check(0, |t| true)?;
            }
            Self::Or => {
                checker.check(0, |t| true)?;
            }
            Self::Implies => {
                checker.check(0, |t| true)?;
            }
            Self::Equals => {
                checker.check(0, |t| true)?;
            }
            Self::Less => {
                checker.check(0, |t| true)?;
            }
            Self::LessEquals => {
                checker.check(0, |t| true)?;
            }
            Self::SignedLess => {
                checker.check(0, |t| true)?;
            }
            Self::SignedLessEquals => {
                checker.check(0, |t| true)?;
            }
            Self::ShiftLeft => {
                checker.check(0, |t| true)?;
            }
            Self::ShiftRight => {
                checker.check(0, |t| true)?;
            }
            Self::CountLeadingZeroes => {
                checker.check(0, |t| true)?;
            }
            Self::CountTrailingZeroes => {
                checker.check(0, |t| true)?;
            }
            Self::WordReverse => {
                checker.check(0, |t| true)?;
            }
            Self::SignedShiftRight => {
                checker.check(0, |t| true)?;
            }
            Self::Not => {
                checker.check(0, |t| true)?;
            }
            Self::BWNot => {
                checker.check(0, |t| true)?;
            }
            Self::WordCast => {
                checker.check(0, |t| true)?;
            }
            Self::WordCastSigned => {
                checker.check(0, |t| true)?;
            }
            Self::True => {
                checker.check(0, |t| true)?;
            }
            Self::False => {
                checker.check(0, |t| true)?;
            }
            Self::UnspecifiedPrecond => {
                checker.check(0, |t| true)?;
            }
            Self::MemUpdate => {
                checker.check(0, |t| true)?;
            }
            Self::MemAcc => {
                checker.check(0, |t| true)?;
            }
            Self::IfThenElse => {
                checker.check(0, |t| true)?;
            }
            Self::ArrayIndex => {
                checker.check(0, |t| true)?;
            }
            Self::ArrayUpdate => {
                checker.check(0, |t| true)?;
            }
            Self::MemDom => {
                checker.check(0, |t| true)?;
            }
            Self::PValid => {
                checker.check(0, |t| true)?;
            }
            Self::PWeakValid => {
                checker.check(0, |t| true)?;
            }
            Self::PAlignValid => {
                checker.check(0, |t| true)?;
            }
            Self::PGlobalValid => {
                checker.check(0, |t| true)?;
            }
            Self::PArrayValid => {
                checker.check(0, |t| true)?;
            }
            Self::HTDUpdate => {
                checker.check(0, |t| true)?;
            }
            Self::WordArrayAccess => {
                checker.check(0, |t| true)?;
            }
            Self::WordArrayUpdate => {
                checker.check(0, |t| true)?;
            }
            Self::TokenWordsAccess => {
                checker.check(0, |t| true)?;
            }
            Self::TokenWordsUpdate => {
                checker.check(0, |t| true)?;
            }
            Self::ROData => {
                checker.check(0, |t| true)?;
            }
            Self::StackWrapper => {
                checker.check(0, |t| true)?;
            }
            Self::ToFloatingPoint => {
                checker.check(0, |t| true)?;
            }
            Self::ToFloatingPointSigned => {
                checker.check(0, |t| true)?;
            }
            Self::ToFloatingPointUnsigned => {
                checker.check(0, |t| true)?;
            }
            Self::FloatingPointCast => {
                checker.check(0, |t| true)?;
            }
        }
        Ok(())
    }
}

struct OperandChecker<'a> {
    op: &'a Op,
    operands: &'a [Expr],
}

impl<'a> OperandChecker<'a> {
    fn check(&self, i: usize, f: impl FnOnce(&Type) -> bool) -> Result<(), OpTypeError> {
        let ty = &self.operands[i].ty;
        if f(ty) {
            Ok(())
        } else {
            Err(OpTypeError::IncorrectTypeOfOperand { op: *self.op, operand_index: i, operand_type: ty.clone() })
        }
    }
}

impl Expr {
    pub(crate) fn typecheck(&self) -> Result<(), OpTypeError> {
        match &self.value {
            ExprValue::Op(op, operands) => op.typecheck(operands),
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
        operand_index: usize,
        operand_type: Type,
    },
}

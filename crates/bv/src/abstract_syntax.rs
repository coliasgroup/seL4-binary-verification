use std::collections::{btree_map, BTreeMap};
use std::ops::{Add, BitAnd, BitOr, Neg, Not, Sub};
use std::{fmt, iter};

use arrayvec::ArrayVec;
use num::BigInt;

use crate::arch::WORD_SIZE_BITS;
use crate::graph::{HasNodeGraph, HasNodeGraphWithNodeAddrBound};
use crate::concrete_syntax::parse::{LineBuffer, Lines, LinesBuffer, ParseError, ParseFromLine, ParseFromLines};
use crate::concrete_syntax::pretty_print::{BlockBuf, FileBuf, LineBuf, ToTokens};

pub(crate) type Ident = String;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Default)]
pub(crate) struct File {
    pub(crate) structs: BTreeMap<Ident, Struct>,
    pub(crate) const_globals: BTreeMap<Ident, ConstGlobal>,
    pub(crate) functions: BTreeMap<Ident, Function>,
}

impl File {
    pub(crate) fn parse_from_str(s: &str) -> Result<Self, ParseError> {
        Lines::tokenize(s).parse()
    }

    pub(crate) fn pretty_print(&self) -> String {
        let mut file = FileBuf::new();
        for (name, struct_) in &self.structs {
            file.push(struct_.pretty_print_into_block(name));
        }
        for (name, const_global) in &self.const_globals {
            file.push(const_global.pretty_print_into_block(name));
        }
        for (name, function) in &self.functions {
            file.push(function.pretty_print_into_block(name));
        }
        file.pretty_print_into_string()
    }
}

impl ParseFromLines for File {
    fn parse(lines: &mut LinesBuffer) -> Result<Self, ParseError> {
        let mut structs = BTreeMap::new();
        let mut const_globals = BTreeMap::new();
        let mut functions = BTreeMap::new();

        while let Ok(tok) = lines.peek_token() {
            match tok.as_str() {
                "Struct" => {
                    let (name, struct_) = Struct::parse_from_lines(lines)?;
                    structs.insert(name, struct_);
                }
                "ConstGlobalDef" => {
                    let (name, const_global) = ConstGlobal::parse_from_lines(lines)?;
                    const_globals.insert(name, const_global);
                }
                "Function" => {
                    let (name, f) = Function::parse_from_lines(lines)?;
                    functions.insert(name, f);
                }
                _ => return Err(ParseError::UnexpectedToken(tok.location())),
            }
        }

        Ok(Self {
            structs,
            const_globals,
            functions,
        })
    }
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

impl Struct {
    fn parse_from_lines(lines: &mut LinesBuffer) -> Result<(Ident, Self), ParseError> {
        let (name, size, align) = lines.parse_next_line_with(|toks| {
            toks.match_("Struct")?;
            Ok((
                toks.parse()?,
                toks.parse_prim_int()?,
                toks.parse_prim_int()?,
            ))
        })?;

        let mut fields = BTreeMap::new();

        while lines
            .peek_token()
            .map(|tok| tok.as_str() == "StructField")
            .unwrap_or(false)
        {
            let (field_name, ty, offset) = lines.parse_next_line_with(|toks| {
                toks.advance().unwrap();
                Ok((toks.parse()?, toks.parse()?, toks.parse_prim_int()?))
            })?;
            fields.insert(field_name, StructField { ty, offset });
        }

        let struct_ = Struct {
            size,
            align,
            fields,
        };

        Ok((name, struct_))
    }

    fn pretty_print_into_block(&self, name: &Ident) -> BlockBuf {
        let mut block = BlockBuf::new();
        block.push_line_with(|line| {
            line.to_tokens("Struct");
            line.to_tokens(name);
            line.display_to_tokens(&self.size);
            line.display_to_tokens(&self.align);
        });
        for (name, field) in &self.fields {
            block.push_line_with(|line| {
                line.to_tokens("StructField");
                line.to_tokens(name);
                line.to_tokens(&field.ty);
                line.display_to_tokens(&field.offset);
            });
        }
        block
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ConstGlobal {
    pub(crate) value: Expr,
}

impl ConstGlobal {
    fn parse_from_lines(_lines: &mut LinesBuffer) -> Result<(Ident, Self), ParseError> {
        todo!()
    }

    fn pretty_print_into_block(&self, _name: &Ident) -> BlockBuf {
        todo!()
    }
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

    fn parse_from_lines(lines: &mut LinesBuffer) -> Result<(Ident, Self), ParseError> {
        let (name, input, output) = lines.parse_next_line_with(|toks| {
            toks.match_("Function")?;
            Ok((toks.parse()?, toks.parse()?, toks.parse()?))
        })?;

        let has_body = lines
            .peek_token()
            .map(|tok| tok.as_str() == "EntryPoint" || tok.parse_big_int().is_some())
            .unwrap_or(false);

        let body = if has_body {
            let mut nodes = BTreeMap::new();
            let entry_point = loop {
                if lines.peek_token()?.as_str() == "EntryPoint" {
                    break lines.parse_next_line_with(|toks| {
                        toks.advance().unwrap();
                        toks.parse()
                    })?;
                } else {
                    let (addr, node) = lines
                        .parse_next_line_with(|toks| Ok((toks.parse_prim_int()?, toks.parse()?)))?;
                    nodes.insert(addr, node);
                }
            };
            Some(FunctionBody { entry_point, nodes })
        } else {
            None
        };

        let f = Function {
            input,
            output,
            body,
        };

        Ok((name, f))
    }

    fn pretty_print_into_block(&self, name: &Ident) -> BlockBuf {
        let mut block = BlockBuf::new();
        block.push_line_with(|line| {
            line.to_tokens("Function");
            line.to_tokens(name);
            line.to_tokens(&self.input);
            line.to_tokens(&self.output);
        });
        if let Some(body) = self.body() {
            for (addr, node) in &body.nodes {
                block.push_line_with(|line| {
                    line.lower_hex_to_tokens(addr);
                    line.to_tokens(node);
                });
            }
            block.push_line_with(|line| {
                line.to_tokens("EntryPoint");
                line.to_tokens(&body.entry_point);
            });
        }
        block
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

impl ParseFromLine for Argument {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            name: toks.parse()?,
            ty: toks.parse()?,
        })
    }
}

impl ToTokens for Argument {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.name);
        line.to_tokens(&self.ty);
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

impl ParseFromLine for NodeId {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(match toks.peek()?.as_str() {
            "Ret" => {
                toks.advance().unwrap();
                Self::Ret
            }
            "Err" => {
                toks.advance().unwrap();
                Self::Err
            }
            _ => Self::Addr(toks.parse_prim_int()?),
        })
    }
}

impl ToTokens for NodeId {
    fn to_tokens(&self, line: &mut LineBuf) {
        match self {
            Self::Addr(addr) => {
                line.lower_hex_to_tokens(addr);
            }
            Self::Ret => {
                line.to_tokens("Ret");
            }
            Self::Err => {
                line.to_tokens("Err");
            }
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

impl ParseFromLine for Node {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        Ok(match tok.as_str() {
            "Basic" => Self::Basic(toks.parse()?),
            "Cond" => Self::Cond(toks.parse()?),
            "Call" => Self::Call(toks.parse()?),
            _ => return Err(ParseError::UnexpectedToken(tok.location())),
        })
    }
}

impl ToTokens for Node {
    fn to_tokens(&self, line: &mut LineBuf) {
        match self {
            Self::Basic(inner) => {
                line.to_tokens("Basic");
                line.to_tokens(inner);
            }
            Self::Cond(inner) => {
                line.to_tokens("Cond");
                line.to_tokens(inner);
            }
            Self::Call(inner) => {
                line.to_tokens("Call");
                line.to_tokens(inner);
            }
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

impl ParseFromLine for BasicNode {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            next: toks.parse()?,
            var_updates: toks.parse()?,
        })
    }
}

impl ToTokens for BasicNode {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.next);
        line.to_tokens(&self.var_updates);
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

impl ParseFromLine for CondNode {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            left: toks.parse()?,
            right: toks.parse()?,
            expr: toks.parse()?,
        })
    }
}

impl ToTokens for CondNode {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.left);
        line.to_tokens(&self.right);
        line.to_tokens(&self.expr);
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

impl ParseFromLine for CallNode {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            next: toks.parse()?,
            function_name: toks.parse()?,
            input: toks.parse()?,
            output: toks.parse()?,
        })
    }
}

impl ToTokens for CallNode {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.next);
        line.to_tokens(&self.function_name);
        line.to_tokens(&self.input);
        line.to_tokens(&self.output);
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct VarUpdate {
    pub(crate) var_name: Ident,
    pub(crate) ty: Type,
    pub(crate) expr: Expr,
}

impl ParseFromLine for VarUpdate {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            var_name: toks.parse()?,
            ty: toks.parse()?,
            expr: toks.parse()?,
        })
    }
}

impl VarUpdate {
    pub(crate) fn visit_var_decls(&self, f: &mut impl FnMut(&Ident, &Type)) {
        f(&self.var_name, &self.ty)
    }

    pub(crate) fn visit_var_decls_mut(&mut self, f: &mut impl FnMut(&mut Ident, &mut Type)) {
        f(&mut self.var_name, &mut self.ty)
    }
}

impl ToTokens for VarUpdate {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.var_name);
        line.to_tokens(&self.ty);
        line.to_tokens(&self.expr);
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

impl ParseFromLine for Expr {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        let ty;
        let expr_val = match tok.as_str() {
            "Var" => {
                let name = toks.parse()?;
                ty = toks.parse()?;
                ExprValue::Var(name)
            }
            "Op" => {
                let name = toks.parse()?;
                ty = toks.parse()?;
                let exprs = toks.parse()?;
                ExprValue::Op(name, exprs)
            }
            "Num" => {
                let num = toks.parse()?;
                ty = toks.parse()?;
                ExprValue::Num(num)
            }
            "Type" => {
                ty = toks.parse()?;
                ExprValue::Type
            }
            "Symbol" => {
                let name = toks.parse()?;
                ty = toks.parse()?;
                ExprValue::Symbol(name)
            }
            "Token" => {
                let name = toks.parse()?;
                ty = toks.parse()?;
                ExprValue::Token(name)
            }
            _ => return Err(ParseError::UnexpectedToken(tok.location())),
        };
        Ok(Self::new(ty, expr_val))
    }
}

impl ToTokens for Expr {
    fn to_tokens(&self, line: &mut LineBuf) {
        let ty = &self.ty;
        match &self.value {
            ExprValue::Var(name) => {
                line.to_tokens("Var");
                line.to_tokens(name);
                line.to_tokens(ty);
            }
            ExprValue::Op(name, exprs) => {
                line.to_tokens("Op");
                line.to_tokens(name);
                line.to_tokens(ty);
                line.to_tokens(exprs);
            }
            ExprValue::Num(num) => {
                line.to_tokens("Num");
                line.lower_hex_to_tokens(num);
                line.to_tokens(ty);
            }
            ExprValue::Type => {
                line.to_tokens("Type");
                line.to_tokens(ty);
            }
            ExprValue::Symbol(name) => {
                line.to_tokens("Symbol");
                line.to_tokens(name);
                line.to_tokens(ty);
            }
            ExprValue::Token(name) => {
                line.to_tokens("Token");
                line.to_tokens(name);
                line.to_tokens(ty);
            }
        }
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

impl ParseFromLine for Type {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        Ok(match tok.as_str() {
            "Bool" => Self::Bool,
            "Mem" => Self::Mem,
            "Dom" => Self::Dom,
            "HTD" => Self::Htd,
            "PMS" => Self::Pms,
            "UNIT" => Self::Unit,
            "Type" => Self::Type,
            "Token" => Self::Token,
            "RelWrapper" => Self::RelWrapper,
            "Word" => Self::Word(toks.parse_prim_int()?),
            "WordArray" => Self::WordArray {
                length: toks.parse_prim_int()?,
                bits: toks.parse_prim_int()?,
            },
            "Array" => Self::Array(toks.parse()?, toks.parse_prim_int()?),
            "Struct" => Self::Struct(toks.parse()?),
            "Ptr" => Self::Ptr(toks.parse()?),
            _ => return Err(ParseError::UnexpectedToken(tok.location())),
        })
    }
}

impl ToTokens for Type {
    fn to_tokens(&self, line: &mut LineBuf) {
        match self {
            Self::Bool => {
                line.to_tokens("Bool");
            }
            Self::Mem => {
                line.to_tokens("Mem");
            }
            Self::Dom => {
                line.to_tokens("Dom");
            }
            Self::Htd => {
                line.to_tokens("HTD");
            }
            Self::Pms => {
                line.to_tokens("PMS");
            }
            Self::Unit => {
                line.to_tokens("UNIT");
            }
            Self::Type => {
                line.to_tokens("Type");
            }
            Self::Token => {
                line.to_tokens("Token");
            }
            Self::RelWrapper => {
                line.to_tokens("RelWrapper");
            }
            Self::Word(num_bits) => {
                line.to_tokens("Word");
                line.display_to_tokens(num_bits);
            }
            Self::WordArray { length, bits } => {
                line.to_tokens("WordArray");
                line.display_to_tokens(length);
                line.display_to_tokens(bits);
            }
            Self::Array(ty, length) => {
                line.to_tokens("Array");
                line.to_tokens(&**ty);
                line.display_to_tokens(length);
            }
            Self::Struct(name) => {
                line.to_tokens("Struct");
                line.to_tokens(name);
            }
            Self::Ptr(ty) => {
                line.to_tokens("Ptr");
                line.to_tokens(&**ty);
            }
        }
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

#[cfg(test)]
mod tests {
    use crate::tests::utils::*;

    use super::*;

    #[test]
    fn round_trips() {
        let mut paths = vec![];
        paths.push(graph_refine_dir().join("example/Functions.txt"));
        paths.push(graph_refine_dir().join("loop-example/CFuns-annotated.txt"));
        for opt_level in ["O1", "O2"] {
            let d = graph_refine_dir().join("loop-example").join(opt_level);
            paths.push(d.join(format!("ASM{opt_level}Funs.txt")));
            paths.push(d.join("ASM-annotated.txt"));
            paths.push(d.join("CFunDump.txt"));
        }
        paths.push(graph_refine_dir().join("loop-example/synth/Functions.txt"));
        paths.push(sel4_target_dir().join("ASMFunctions.txt"));
        paths.push(sel4_target_dir().join("CFunctions.txt"));
        paths.push(sel4_target_dir().join("functions.txt"));

        for path in paths {
            test_round_trip_path(path, File::parse_from_str, File::pretty_print);
        }
    }
}

use std::collections::{btree_map, BTreeMap};
use std::convert::Infallible;
use std::ops::{Add, BitAnd, BitOr, Neg, Not, Sub};
use std::slice::SliceIndex;
use std::{fmt, iter};

use arrayvec::ArrayVec;
use num::BigInt;

use crate::abstract_syntax::{Argument, Ident};
use crate::arch::WORD_SIZE_BITS;
use crate::graph::{
    HasFunctionSignature, HasNodeGraph, HasNodeGraphWithNodeAddrBound, MightHaveNodeGraphWithEntry,
};

use self::typecheck::OpTypeError;

pub(crate) mod typecheck;

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

    pub(crate) fn mk_stack_wrapper(
        sp: Self,
        stack: Self,
        except: impl IntoIterator<Item = Self>,
    ) -> Self {
        assert!(sp.ty.is_word_with_size(WORD_SIZE_BITS));
        assert!(stack.ty.is_mem());
        let except = except
            .into_iter()
            .inspect(|expr| assert!(expr.ty.is_word_with_size(WORD_SIZE_BITS)));
        let args = [sp, stack].into_iter().chain(except).collect::<Vec<_>>();
        Self::new(Type::RelWrapper, Op::StackWrapper.mk(args))
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
            self.mk_if(
                Self::new(ty.clone(), ExprValue::Num(1.into())),
                Self::new(ty.clone(), ExprValue::Num(0.into())),
            )
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
    EqSelectiveWrapper,
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
            Self::StackWrapper => todo!(), // 2,
            Self::EqSelectiveWrapper => todo!(),
            Self::ToFloatingPoint => 1,
            Self::ToFloatingPointSigned => 2,
            Self::ToFloatingPointUnsigned => 2,
            Self::FloatingPointCast => 1,
        }
    }
}

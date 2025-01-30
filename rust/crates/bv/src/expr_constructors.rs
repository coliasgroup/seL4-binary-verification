use crate::abstract_syntax::{Argument, Expr, ExprValue, Ident, Num, Type};
use crate::{abstract_syntax::Op, arch::WORD_SIZE_BITS};

impl Expr {
    pub(crate) fn mk_true() -> Self {
        Self::new(Type::Bool, Op::True.mk([]))
    }

    pub(crate) fn mk_false() -> Self {
        Self::new(Type::Bool, Op::False.mk([]))
    }

    pub(crate) fn mk_bool(val: bool) -> Self {
        if val {
            Self::mk_true()
        } else {
            Self::mk_false()
        }
    }

    pub(crate) fn mk_word(val: Num, bits: u64) -> Self {
        Self::new(Type::Word(bits), ExprValue::Num(val))
    }

    pub(crate) fn mk_eq(self, rhs: Self) -> Self {
        assert_eq!(self.ty, rhs.ty);
        Self::new(Type::Bool, Op::Equals.mk([self, rhs]))
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
        Self::new(Type::Bool, Op::And.mk([self, rhs]))
    }

    pub(crate) fn mk_or(self, rhs: Self) -> Self {
        assert_eq!(self.ty, Type::Bool);
        assert_eq!(rhs.ty, Type::Bool);
        Self::new(Type::Bool, Op::Or.mk([self, rhs]))
    }

    pub(crate) fn mk_not(self) -> Self {
        assert_eq!(self.ty, Type::Bool);
        Self::new(self.ty.clone(), Op::Not.mk([self]))
    }

    pub(crate) fn mk_implies(self, rhs: Self) -> Self {
        assert_eq!(self.ty, Type::Bool);
        assert_eq!(rhs.ty, Type::Bool);
        Self::new(Type::Bool, Op::Implies.mk([self, rhs]))
    }

    pub(crate) fn mk_plus(self, rhs: Self) -> Self {
        assert!(self.ty.is_word());
        assert_eq!(self.ty, rhs.ty);
        Self::new(self.ty.clone(), Op::Plus.mk([self, rhs]))
    }

    pub(crate) fn mk_minus(self, rhs: Self) -> Self {
        assert!(self.ty.is_word());
        assert_eq!(self.ty, rhs.ty);
        Self::new(self.ty.clone(), Op::Minus.mk([self, rhs]))
    }

    pub(crate) fn mk_neg(self) -> Self {
        assert!(self.ty.is_word());
        Self::new(self.ty.clone(), ExprValue::Num(0.into())).mk_minus(self)
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
        Self::new(Type::Bool, op.mk([self, rhs]))
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
        Self::new(Type::Bool, op.mk([self, rhs]))
    }

    pub(crate) fn mk_bitwise_and(self, rhs: Self) -> Self {
        assert!(self.ty.is_word());
        assert_eq!(self.ty, rhs.ty);
        Self::new(self.ty.clone(), Op::BWAnd.mk([self, rhs]))
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

    pub(crate) fn mk_memacc(self, addr: Self, ty: Type) -> Self {
        assert!(self.ty.is_mem());
        assert!(addr.ty.is_word_with_size(WORD_SIZE_BITS));
        assert!(ty.is_word());
        Self::new(ty, Op::MemAcc.mk([self, addr]))
    }

    pub(crate) fn mk_rodata(self) -> Self {
        assert_eq!(self.ty, Type::Mem);
        Self::new(Type::Bool, Op::ROData.mk([self]))
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

    // TODO
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

    // TODO
    pub(crate) fn mk_machine_word_var(name: Ident) -> Self {
        Self::mk_var(name, Type::mk_machine_word())
    }
}

impl Type {
    // TODO
    pub(crate) fn mk_machine_word() -> Self {
        Self::Word(WORD_SIZE_BITS)
    }
}

impl Op {
    pub(crate) fn mk(self, operands: impl IntoIterator<Item = Expr>) -> ExprValue {
        ExprValue::Op(self, operands.into_iter().collect())
    }
}

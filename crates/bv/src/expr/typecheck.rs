use std::slice::SliceIndex;

use super::{Expr, ExprValue, Op, Type};


pub(super) struct ExprChecker<'a> {
    pub(super) op: &'a Op,
    pub(super) ty: &'a Type,
    pub(super) operands: &'a [Expr],
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
            Op::EqSelectiveWrapper => {
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

impl Op {
    pub(crate) fn typecheck(&self, ty: &Type, operands: &[Expr]) -> Result<(), OpTypeError> {
        ExprChecker {
            op: self,
            ty,
            operands,
        }
        .check_all()
    }
}

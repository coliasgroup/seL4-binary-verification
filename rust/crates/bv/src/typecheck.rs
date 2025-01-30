use std::{
    collections::{btree_map::Entry, BTreeMap},
    slice::SliceIndex,
};

use crate::{
    abstract_syntax::{Argument, Expr, ExprValue, File, Ident, Node, Op, Type},
    compat::ProblemsFile,
    pairing::{PairingId, Tag},
    problem::Problem,
};

use crate::abstract_syntax::{HasFunction, HasNodeGraph};

// TODO
// - typecheck symbols file-wide
// - typecheck inputs and outputs in call nodes

impl File {
    pub(crate) fn typecheck(&self) -> Result<(), FileTypeError> {
        for (f_name, f) in &self.functions {
            typecheck_function(f).map_err(|err| FileTypeError {
                function: f_name.clone(),
                error: err,
            })?;
        }
        Ok(())
    }
}

impl ProblemsFile {
    pub(crate) fn typecheck(&self) -> Result<(), ProblemsFileTypeError> {
        for (pairing, problem) in &self.problems {
            problem.typecheck().map_err(|err| ProblemsFileTypeError {
                pairing: pairing.clone(),
                error: err,
            })?;
        }
        Ok(())
    }
}

impl Problem {
    pub(crate) fn typecheck(&self) -> Result<(), ProblemTypeError> {
        typecheck_function(&self.at_c()).map_err(|err| ProblemTypeError {
            tag: Tag::C,
            error: err,
        })?;
        typecheck_function(&self.at_asm()).map_err(|err| ProblemTypeError {
            tag: Tag::Asm,
            error: err,
        })?;
        Ok(())
    }
}

fn typecheck_function<T: HasFunction>(f: &T) -> Result<(), FunctionTypeError> {
    if let Some(graph) = f.function_body_if_present() {
        for (_, node) in graph.node_graph_nodes() {
            node.try_visit_exprs(&mut |expr| expr.typecheck())
                .map_err(FunctionTypeError::OpTypeError)?;
            if let Node::Cond(node) = node {
                if !matches!(&node.expr.ty, Type::Bool) {
                    return Err(FunctionTypeError::CondExprNotBool);
                }
            }
        }
    }
    let mut var_types = VariableTypes::new();
    var_types.admit_args(f.function_input())?;
    var_types.admit_args(f.function_output())?;
    if let Some(graph) = f.function_body_if_present() {
        for (_, node) in graph.node_graph_nodes() {
            var_types.admit_node(node)?;
        }
    }
    Ok(())
}

struct VariableTypes<'a> {
    map: BTreeMap<&'a Ident, &'a Type>,
}

impl<'a> VariableTypes<'a> {
    fn new() -> Self {
        Self {
            map: BTreeMap::new(),
        }
    }

    fn admit(&mut self, ident: &'a Ident, ty: &'a Type) -> Result<(), FunctionTypeError> {
        match self.map.entry(ident) {
            Entry::Occupied(entry) => {
                if &ty != entry.get() {
                    return Err(FunctionTypeError::InconsistentVariableType(ident.clone()));
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(ty);
            }
        }
        Ok(())
    }

    fn admit_arg(&mut self, arg: &'a Argument) -> Result<(), FunctionTypeError> {
        self.admit(&arg.name, &arg.ty)
    }

    fn admit_args(&mut self, args: &'a [Argument]) -> Result<(), FunctionTypeError> {
        for arg in args {
            self.admit_arg(arg)?;
        }
        Ok(())
    }

    fn admit_node(&mut self, node: &'a Node) -> Result<(), FunctionTypeError> {
        node.try_visit_exprs(&mut |expr| self.admit_expr(expr))?;
        node.try_visit_var_decls(&mut |ident, ty| self.admit(ident, ty))?;
        Ok(())
    }

    fn admit_expr(&mut self, expr: &'a Expr) -> Result<(), FunctionTypeError> {
        if let ExprValue::Var(ident) = &expr.value {
            self.admit(ident, &expr.ty)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum FunctionTypeError {
    OpTypeError(OpTypeError),
    InconsistentVariableType(Ident),
    CondExprNotBool,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct FileTypeError {
    function: Ident,
    error: FunctionTypeError,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ProblemsFileTypeError {
    pairing: PairingId,
    error: ProblemTypeError,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ProblemTypeError {
    tag: Tag,
    error: FunctionTypeError,
}

impl Op {
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

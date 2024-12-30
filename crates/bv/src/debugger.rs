use std::{collections::BTreeMap, fmt::Debug};

use crate::{abstract_syntax::{Ident, NodeId}, expr::{Expr, ExprValue, Num, Op, Type}};

struct State<'a> {
    pos: NodeId,
    vars: VarState<'a>,
}

struct VarState<'a> {
    values: BTreeMap<Ident, Value>,
    symbols: &'a BTreeMap<Ident, Value>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
enum Value {
    Bool(bool),
    Num(Num),
    Type(Type),
    Token(Ident),
}

impl Value {
    fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(x) => Some(*x),
            _ => None,
        }
    }

    fn as_num(&self) -> Option<&Num> {
        match self {
            Self::Num(x) => Some(x),
            _ => None,
        }
    }

    fn as_type(&self) -> Option<&Type> {
        match self {
            Self::Type(x) => Some(x),
            _ => None,
        }
    }

    fn as_token(&self) -> Option<&Ident> {
        match self {
            Self::Token(x) => Some(x),
            _ => None,
        }
    }
}

impl<'a> VarState<'a> {
    fn new(symbols: &'a BTreeMap<Ident, Value>) -> Self {
        Self {
            values: BTreeMap::new(),
            symbols,
        }
    }

    fn get(&self, ident: &Ident) -> Result<&Value, DebuggerError> {
        self.values
            .get(ident)
            .ok_or_else(|| DebuggerError::VariableUndefined {
                ident: ident.clone(),
            })
    }

    fn get_symbol(&self, ident: &Ident) -> &Value {
        self.symbols.get(ident).unwrap()
    }

    fn set(&mut self, ident: Ident, value: Value) {
        self.values.insert(ident, value);
    }

    fn eval(&self, expr: &Expr) -> Result<Value, DebuggerError> {
        Ok(match &expr.value {
            ExprValue::Num(num) => Value::Num(num.clone()),
            ExprValue::Type(ty) => Value::Type(ty.clone()),
            ExprValue::Token(ident) => Value::Token(ident.clone()),
            ExprValue::Var(ident) => self.get(ident)?.clone(),
            ExprValue::Symbol(ident) => self.get_symbol(ident).clone(),
            ExprValue::Op(op, args) => self.eval_op(
                &expr.ty,
                op,
                &args
                    .iter()
                    .map(|arg| self.eval(arg))
                    .collect::<Result<Vec<_>, _>>()?,
            )?,
        })
    }

    fn eval_op(&self, ty: &Type, op: &Op, args: &[Value]) -> Result<Value, DebuggerError> {
        Ok(match op {
            Op::Plus => Value::Num(
                (args[0].as_num().unwrap() + args[1].as_num().unwrap())
                    % (2 >> ty.as_word().unwrap()),
            ),
            Op::Minus => Value::Num(
                (args[0].as_num().unwrap() - args[1].as_num().unwrap())
                    % (2 >> ty.as_word().unwrap()),
            ),
            Op::Times => Value::Num(
                (args[0].as_num().unwrap() * args[1].as_num().unwrap())
                    % (2 >> ty.as_word().unwrap()),
            ),
            Op::Modulus => todo!(),
            Op::DividedBy => todo!(),
            Op::BWAnd => todo!(),
            Op::BWOr => todo!(),
            Op::BWXOR => todo!(),
            Op::And => todo!(),
            Op::Or => todo!(),
            Op::Implies => Value::Bool(!args[0].as_bool().unwrap() || args[1].as_bool().unwrap()),
            Op::Equals => Value::Bool(args[0] == args[1]),
            Op::Less => Value::Bool(args[0].as_num().unwrap() < args[1].as_num().unwrap()),
            Op::LessEquals => Value::Bool(args[0].as_num().unwrap() <= args[1].as_num().unwrap()),
            Op::SignedLess => todo!(),
            Op::SignedLessEquals => todo!(),
            Op::ShiftLeft => todo!(),
            Op::ShiftRight => todo!(),
            Op::CountLeadingZeroes => todo!(),
            Op::CountTrailingZeroes => todo!(),
            Op::WordReverse => todo!(),
            Op::SignedShiftRight => todo!(),
            Op::Not => Value::Bool(!args[0].as_bool().unwrap()),
            Op::BWNot => todo!(),
            Op::WordCast => todo!(),
            Op::WordCastSigned => todo!(),
            Op::True => Value::Bool(true),
            Op::False => Value::Bool(false),
            Op::UnspecifiedPrecond => todo!(),
            Op::MemUpdate => todo!(),
            Op::MemAcc => todo!(),
            Op::IfThenElse => {
                if args[0].as_bool().unwrap() {
                    args[1].clone()
                } else {
                    args[2].clone()
                }
            }
            Op::ArrayIndex => todo!(),
            Op::ArrayUpdate => todo!(),
            Op::MemDom => todo!(),
            Op::PValid => todo!(),
            Op::PWeakValid => todo!(),
            Op::PAlignValid => todo!(),
            Op::PGlobalValid => todo!(),
            Op::PArrayValid => todo!(),
            Op::HTDUpdate => todo!(),
            Op::WordArrayAccess => todo!(),
            Op::WordArrayUpdate => todo!(),
            Op::TokenWordsAccess => todo!(),
            Op::TokenWordsUpdate => todo!(),
            Op::ROData => todo!(),
            Op::StackWrapper => todo!(),
            Op::EqSelectiveWrapper => todo!(),
            Op::ToFloatingPoint => todo!(),
            Op::ToFloatingPointSigned => todo!(),
            Op::ToFloatingPointUnsigned => todo!(),
            Op::FloatingPointCast => todo!(),
        })
    }
}

enum DebuggerError {
    VariableUndefined { ident: Ident },
}

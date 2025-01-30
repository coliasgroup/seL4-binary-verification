use crate::abstract_syntax::{Expr, ExprValue, Function, HasFunctionMut, Num, Op, Type};
use crate::arch::WORD_SIZE_BITS;
use crate::objdump::ObjdumpInfo;
use crate::utils::DoubleEndedIteratorExt;

impl Function {
    pub(crate) fn pseudo_compile(&mut self, objdump_info: &ObjdumpInfo) {
        self.compile_symbol_references(objdump_info);
        self.compile_p_align_valid_exprs();
    }

    fn compile_symbol_references(&mut self, objdump_info: &ObjdumpInfo) {
        self.abstract_function_mut().visit_exprs_mut(&mut |expr| {
            if let ExprValue::Symbol(name) = &expr.value {
                // pseudo_compile.subst_expr: #FIXME: dubious assumption of native word size here
                expr.value = ExprValue::Num(Num::from(objdump_info.symbols[name].addr));
            }
        })
    }

    fn compile_p_align_valid_exprs(&mut self) {
        self.abstract_function_mut().visit_exprs_mut(&mut |expr| {
            if let ExprValue::Op(Op::PAlignValid, exprs) = &expr.value {
                let (ty, p) = match exprs.as_slice() {
                    [Expr {
                        ty: Type::Type,
                        value: ExprValue::Type(ty_val),
                    }, p @ Expr {
                        ty: Type::Word(ptr_bits),
                        value: ExprValue::Var(_),
                    }] if *ptr_bits == WORD_SIZE_BITS => (ty_val, p),
                    _ => panic!(),
                };
                *expr = mk_align_valid_ineq(ty, p);
            }
        })
    }
}

fn mk_align_valid_ineq(ty: &Type, p: &Expr) -> Expr {
    let size = Expr::new(Type::Word(WORD_SIZE_BITS), ExprValue::Num(ty.size().into()));
    let align = ty.align();
    assert!([1, 4, 8].contains(&align));
    let w0 = Expr::new(Type::Word(WORD_SIZE_BITS), ExprValue::Num(0.into()));
    let mut conj = vec![];
    if align > 1 {
        conj.push(
            p.clone()
                .mk_bitwise_and(Expr::new(
                    Type::Word(WORD_SIZE_BITS),
                    ExprValue::Num((align - 1).into()),
                ))
                .mk_eq(w0.clone()),
        );
    }
    conj.push(p.clone().mk_eq(w0.clone()).mk_not());
    conj.push(
        w0.mk_less(size.clone())
            .mk_implies(p.clone().mk_less_eq(size.mk_neg())),
    );
    conj.into_iter().foldr1_like_gr(Expr::mk_and).unwrap()
}

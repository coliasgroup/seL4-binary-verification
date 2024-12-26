use std::collections::{BTreeMap, BTreeSet};

use regex::Regex;

use crate::abstract_syntax::{
    Argument, CallNode, Expr, Function, FunctionBody, Ident, Node, NodeId, Type,
};
use crate::arch::WORD_SIZE_BITS;
use crate::logic::split_scalar_pairs;
use crate::pairing::{EqSideQuadrant, Pairing, PairingId};

pub(crate) fn add_asm_inst_spec(
    asm_functions: &mut BTreeMap<Ident, Function>,
    c_functions: &mut BTreeMap<Ident, Function>,
    pairings: &mut BTreeMap<PairingId, Pairing>,
) {
    let mut new_idents = BTreeSet::new();
    let mut unhandled = BTreeSet::new();
    for (f_name, f) in c_functions.iter_mut() {
        if let Some(ident) = f_name.strip_prefix("asm_instruction'") {
            assert!(f.body.is_none());
            let (body, new_ident) = mk_asm_inst_spec(ident, f);
            if body.is_none() {
                unhandled.insert(ident.to_owned());
            } else {
                new_idents.insert(new_ident);
            }
            f.body = body;
        }
    }
    for (f_name, f) in asm_functions.iter_mut() {
        if let Some(ident) = f_name.strip_prefix("instruction'") {
            assert!(f.body.is_none());
            let (body, new_ident) = mk_bin_inst_spec(ident);
            if body.is_none() {
                unhandled.insert(ident.to_owned());
            } else {
                new_idents.insert(new_ident);
            }
            f.body = body;
        }
    }
    for new_ident in &new_idents {
        let (impl_name, reg_spec) = reg_spec(new_ident).unwrap();
        let mut input = vec![];
        for i in 0..reg_spec.iter().filter(|p| matches!(p, RegRole::In)).count() {
            input.push(Argument {
                ty: Type::Word(WORD_SIZE_BITS),
                name: format!("reg_val{:?}", i + 1),
            });
        }
        input.push(Argument {
            ty: Type::Token,
            name: "inst_ident".to_owned(),
        });
        input.push(Argument {
            ty: Type::Mem,
            name: "mem".to_owned(),
        });
        let mut output = vec![];
        for i in 0..reg_spec
            .iter()
            .filter(|p| matches!(p, RegRole::Out))
            .count()
        {
            output.push(Argument {
                ty: Type::Word(WORD_SIZE_BITS),
                name: format!("ret_val{:?}", i + 1),
            });
        }
        output.push(Argument {
            ty: Type::Mem,
            name: "mem".to_owned(),
        });
        let new_f = Function {
            input,
            output,
            body: None,
        };
        let new_asm_f_name = format!("l_{impl_name}");
        let new_c_f_name = format!("r_{impl_name}");
        let pairing_id = PairingId {
            asm: new_asm_f_name.clone(),
            c: new_c_f_name.clone(),
        };

        asm_functions.insert(new_asm_f_name, new_f.clone());
        c_functions.insert(new_c_f_name, new_f.clone());

        let mut in_eqs = new_f.input.iter().map(|arg| {
            let expr = Expr::mk_var(arg.name.clone(), arg.ty.clone());
            EqSideQuadrant::ASM_IN.side(expr.clone()).mk_eq(EqSideQuadrant::C_IN.side(expr.clone()))
        }).collect::<Vec<_>>();
        in_eqs.push(EqSideQuadrant::ASM_IN.side(Expr::mk_var("mem".to_owned(), Type::Mem).mk_rodata()).mk_eq(EqSideQuadrant::C_IN.side(Expr::mk_true())));
        let mut out_eqs = new_f.output.iter().map(|arg| {
            let expr = Expr::mk_var(arg.name.clone(), arg.ty.clone());
            EqSideQuadrant::ASM_OUT.side(expr.clone()).mk_eq(EqSideQuadrant::C_OUT.side(expr.clone()))
        }).collect::<Vec<_>>();
        out_eqs.push(EqSideQuadrant::ASM_OUT.side(Expr::mk_var("mem".to_owned(), Type::Mem).mk_rodata()).mk_eq(EqSideQuadrant::C_OUT.side(Expr::mk_true())));
        let pairing = Pairing { in_eqs, out_eqs };
        pairings.insert(pairing_id, pairing);
    }
    // TODO
    // log::warn!("unhandled: {unhandled:#?}");
}

fn is_reg(s: &str) -> bool {
    Regex::new(r"^r(?<i>[0-9]+)$")
        .unwrap()
        .captures(s)
        .and_then(|caps| u8::from_str_radix(&caps["i"], 10).ok())
        .map(|i| i < 16)
        .unwrap_or(false)
}

fn split_inst_name_regs(ident: &str) -> (Vec<String>, String) {
    let mut bits = Regex::new(r"[_,]+")
        .unwrap()
        .split(&ident)
        .collect::<Vec<_>>();
    let mut fin_bits = Vec::new();
    let mut regs = Vec::new();
    if bits.len() > 1 && Regex::new(r"^p[0-9]+$").unwrap().is_match(bits[1]) {
        bits[1] = &bits[1][1..];
    }
    for bit in &bits {
        let mut bit2 = bit.to_lowercase();
        bit2 = unalias_reg(&bit2).unwrap_or(&bit2).to_owned();
        if Regex::new(r"^cr[0-9]+$").unwrap().is_match(&bit2) {
            bit2 = format!("c{}", &bit2[2..]);
        }
        if is_reg(&bit2) || bit2.starts_with('%') {
            regs.push(bit2);
            fin_bits.push(format!("argv{:?}", regs.len()));
        } else {
            fin_bits.push(bit2);
        }
    }
    (regs, fin_bits.join("_"))
}

fn mk_asm_inst_spec(f_name: &str, f: &Function) -> (Option<FunctionBody>, Ident) {
    let ident = f_name;
    let (args, ident) = split_inst_name_regs(ident);
    if !args.iter().all(|arg| arg.starts_with('%')) {
        return (None, f_name.to_owned());
    }
    let base_ident = ident.split('_').next().unwrap();
    let body = if let Some((impl_name, _reg_spec)) = reg_spec(base_ident) {
        let (iscs, imems, _) = split_scalar_pairs(&f.input);
        let (oscs, omems, _) = split_scalar_pairs(&f.output);
        let mut input = iscs
            .into_iter()
            .map(|arg| Expr::mk_var(arg.name, arg.ty))
            .collect::<Vec<_>>();
        input.push(Expr::mk_token(ident.to_owned()));
        input.extend(imems.into_iter().map(|arg| Expr::mk_var(arg.name, arg.ty)));
        let mut output = oscs;
        output.extend(omems);
        Some(FunctionBody {
            entry_point: NodeId::Addr(1),
            nodes: {
                let mut this = BTreeMap::new();
                this.insert(
                    1,
                    Node::Call(CallNode {
                        next: NodeId::Ret,
                        function_name: format!("r_{}", impl_name),
                        input,
                        output,
                    }),
                );
                this
            },
        })
    } else {
        None
    };
    (body, base_ident.to_owned())
}

fn mk_bin_inst_spec(f_name: &str) -> (Option<FunctionBody>, Ident) {
    let (ident, addr_s) = f_name.rsplit_once('_').unwrap();
    let (regs, ident) = split_inst_name_regs(ident);
    let ident = unalias_insn(&ident).unwrap_or(&ident);
    let _addr = u64::from_str_radix(addr_s, 16).unwrap();
    let base_ident = ident.split('_').next().unwrap();
    let body = if let Some((impl_name, reg_spec)) = reg_spec(base_ident) {
        let mut input = vec![];
        for (reg, d) in regs.iter().zip(reg_spec.iter()) {
            if let RegRole::In = d {
                input.push(Expr::mk_var(reg.to_owned(), Type::Word(WORD_SIZE_BITS)));
            }
        }
        input.push(Expr::mk_token(ident.to_owned()));
        input.push(Expr::mk_var("mem".to_owned(), Type::Mem));
        let mut output = vec![];
        for (reg, d) in regs.iter().zip(reg_spec.iter()) {
            if let RegRole::Out = d {
                output.push(Argument {
                    ty: Type::Word(WORD_SIZE_BITS),
                    name: reg.to_owned(),
                });
            }
        }
        output.push(Argument {
            ty: Type::Mem,
            name: "mem".to_owned(),
        });
        Some(FunctionBody {
            entry_point: NodeId::Addr(1),
            nodes: {
                let mut this = BTreeMap::new();
                this.insert(
                    1,
                    Node::Call(CallNode {
                        next: NodeId::Ret,
                        function_name: format!("l_{}", impl_name),
                        input,
                        output,
                    }),
                );
                this
            },
        })
    } else {
        None
    };
    (body, base_ident.to_owned())
}

fn unalias_reg(alias: &str) -> Option<&str> {
    Some(match alias {
        "sb" => "r9",
        "sl" => "r10",
        "fp" => "r11",
        "ip" => "r12",
        "sp" => "r13",
        "lr" => "r14",
        "pc" => "r15",
        _ => return None,
    })
}

fn unalias_insn(alias: &str) -> Option<&str> {
    Some(match alias {
        "isb_sy" => "isb",
        "dmb_sy" => "dmb",
        "dsb_sy" => "dsb",
        _ => return None,
    })
}

enum RegRole {
    In,
    Out,
}

type RegSpec = &'static [RegRole];

fn reg_spec(insn: &str) -> Option<(&str, RegSpec)> {
    Some(match insn {
        "mcr" => ("impl'mcr", &[RegRole::In]),
        "mcr2" => ("impl'mcr", &[RegRole::In]),
        "mcrr" => ("impl'mcrr", &[RegRole::In, RegRole::In]),
        "mcrr2" => ("impl'mcrr", &[RegRole::In, RegRole::In]),
        "mrc" => ("impl'mrc", &[RegRole::Out]),
        "mrc2" => ("impl'mrc", &[RegRole::Out]),
        "mrrc" => ("impl'mrrc", &[RegRole::Out, RegRole::Out]),
        "mrrc2" => ("impl'mrrc", &[RegRole::Out, RegRole::Out]),
        "dsb" => ("impl'dsb", &[]),
        "dmb" => ("impl'dmb", &[]),
        "isb" => ("impl'isb", &[]),
        "wfi" => ("impl'wfi", &[]),
        _ => return None,
    })
}

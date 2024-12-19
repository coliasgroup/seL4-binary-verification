use std::str::FromStr;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

use crate::abstract_syntax::{
    Argument, Expr, ExprValue, File, Function, Ident, Node, NodeId, Op, Type, VarUpdate,
};
use crate::compat::{PairingsFile, ProblemProof, ProblemsFile, ProofsFile};
use crate::pairing::{Eq, EqSide, Pairing, PairingId, Tag};
use crate::problem::{Problem, ProblemSide};
use crate::proof_script::{ProofNode, RestrProofNodeKind};

pub(crate) fn pp(x: impl ToTokens) -> String {
    try_pp(x).unwrap_or_else(|e| panic!("failed to format: {}: {:?}", e, e.span().start()))
}

pub(crate) fn try_pp(x: impl ToTokens) -> Result<String, syn::Error> {
    try_pp_keeping_failure(x).map_err(|(e, _)| e)
}

pub(crate) fn try_pp_keeping_failure(x: impl ToTokens) -> Result<String, (syn::Error, String)> {
    let ugly = x.to_token_stream().to_string();
    let parsed = syn::parse_file(&ugly).map_err(|err| (err, ugly))?;
    Ok(prettyplease::unparse(&parsed))
}

impl ToTokens for ProofsFile {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let problems = self.problems.iter().map(|(id, problem_proof)| {
            let asm = escape_ident(&id.asm);
            let c = escape_ident(&id.c);
            quote! {
                const #asm: #c = #problem_proof;
            }
        });
        toks.extend(quote! {
            #(#problems)*
        });
    }
}

impl ToTokens for ProblemProof {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let proof = &self.proof;
        let c = &self.problem.c;
        let asm = &self.problem.asm;
        let nodes = self.problem.nodes.nodes().map(|(addr, node)| {
            let addr = parse(format!("{:#x?}", addr));
            quote! {
                #addr => #node
            }
        });
        toks.extend(quote! {
            Problem {
                c: #c,
                asm: #asm,
                proof: #proof,
                nodes: match _ {
                    #(#nodes,)*
                },
            }
        });
    }
}

impl ToTokens for ProofNode {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(match self {
            Self::Leaf => {
                quote! {
                    Leaf
                }
            }
            Self::Restr(inner) => {
                let addr = parse(format!("{:#x?}", inner.addr));
                let tag = &inner.tag;
                let kind = &inner.kind;
                let x = parse(format!("{:#x?}", inner.x));
                let y = parse(format!("{:#x?}", inner.y));
                let child = &inner.child;
                quote! {
                    RestrProofNode {
                        addr: #addr,
                        tag: #tag,
                        kind: #kind,
                        x: #x,
                        y: #y,
                        child: #child,
                    }
                }
            }
            Self::CaseSplit(_inner) => {
                quote! {
                    CaseSplitProofNode
                }
            }
            Self::Split(_inner) => {
                quote! {
                    SplitProofNode {

                    }
                }
            }
            Self::SingleRevInduct(_inner) => {
                quote! {
                    SingleRevInductProofNode
                }
            }
        })
    }
}

impl ToTokens for RestrProofNodeKind {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(parse(format!("{:?}", self)));
    }
}

impl ToTokens for ProblemsFile {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let problems = self.problems.iter().map(|(id, problem)| {
            let asm = escape_ident(&id.asm);
            let c = escape_ident(&id.c);
            quote! {
                const #asm: #c = #problem;
            }
        });
        toks.extend(quote! {
            #(#problems)*
        });
    }
}

impl ToTokens for PairingsFile {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let pairings = self.pairings.iter().map(|(id, pairing)| {
            let asm = escape_ident(&id.asm);
            let c = escape_ident(&id.c);
            quote! {
                const #asm: #c = #pairing;
            }
        });
        toks.extend(quote! {
            #(#pairings)*
        });
    }
}

impl ToTokens for Tag {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(parse(format!("{:?}", self)));
    }
}

impl ToTokens for PairingId {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let asm = escape_ident(&self.asm);
        let c = escape_ident(&self.c);
        toks.extend(quote! {
            Asm(#asm) <= C(#c)
        });
    }
}

impl ToTokens for Pairing {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let in_eqs = &self.in_eqs;
        let out_eqs = &self.out_eqs;
        toks.extend(quote! {
            [
                #(IN #in_eqs,)*
                #(OUT #out_eqs,)*
            ]
        });
    }
}

impl ToTokens for Eq {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let lhs = &self.lhs;
        let rhs = &self.rhs;
        toks.extend(quote! {
            {
                #lhs,
                #rhs,
            }
        });
    }
}

impl ToTokens for EqSide {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let q = parse(format!("{}", self.quadrant));
        let expr = &self.expr;
        toks.extend(quote! {
            #q: #expr
        });
    }
}

impl ToTokens for Problem {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let c = &self.c;
        let asm = &self.asm;
        let nodes = self.nodes.nodes().map(|(addr, node)| {
            let addr = parse(format!("{:#x?}", addr));
            quote! {
                #addr => #node
            }
        });
        toks.extend(quote! {
            Problem {
                c: #c,
                asm: #asm,
                nodes: match _ {
                    #(#nodes,)*
                },
            }
        });
    }
}

impl ToTokens for ProblemSide {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let name = escape_ident(&self.name);
        let input = &self.input;
        let output = &self.output;
        toks.extend(quote! {
            Problem {
                name: #name,
                input: Arguments {
                    #(#input,)*
                },
                output: Arguments {
                    #(#output,)*
                },
            }
        });
    }
}

impl ToTokens for File {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let functions = self.functions.iter().map(|(name, function)| {
            let name = escape_ident(name);
            quote! {
                const #name: () = #function;
            }
        });
        toks.extend(quote! {
            #(#functions)*
        });
    }
}

impl ToTokens for Function {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let input = &self.input;
        let output = &self.output;
        let body = if let Some(body) = self.body() {
            let entry_point = &body.entry_point;
            let nodes = body.nodes.iter().map(|(addr, node)| {
                let addr = parse(format!("{:#x?}", addr));
                quote! {
                    #addr => #node
                }
            });
            quote! {
                entry_point: #entry_point,
                nodes: match _ {
                    #(#nodes,)*
                },
            }
        } else {
            quote!()
        };
        toks.extend(quote! {
            Function {
                input: Arguments {
                    #(#input,)*
                },
                output: Arguments {
                    #(#output,)*
                },
                #body
            }
        });
    }
}

impl ToTokens for NodeId {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(parse(format!("{}", self)));
    }
}

impl ToTokens for Node {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(match self {
            Self::Basic(inner) => {
                let next = &inner.next;
                let var_updates = &inner.var_updates;
                quote! {
                    Basic {
                        next: #next,
                        var_updates: O {
                            #(#var_updates,)*
                        }
                    }
                }
            }
            Self::Cond(inner) => {
                let left = &inner.left;
                let right = &inner.right;
                let expr = &inner.expr;
                quote! {
                    Cond {
                        left: #left,
                        right: #right,
                        expr: #expr,
                    }
                }
            }
            Self::Call(inner) => {
                let next = &inner.next;
                let function_name = &inner.function_name;
                let input = &inner.input;
                let output = &inner.output;
                quote! {
                    Call {
                        next: #next,
                        function_name: #function_name,
                        input: [
                            #(#input,)*
                        ],
                        output: O {
                            #(#output,)*
                        }
                    }
                }
            }
        });
    }
}

impl ToTokens for VarUpdate {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let name = escape_ident(&self.var_name);
        let expr = &self.expr;
        toks.extend(quote! {
            #name: #expr
        });
    }
}

impl ToTokens for Argument {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let name = escape_ident(&self.name);
        let ty = &self.ty;
        toks.extend(quote! {
            #name: #ty
        });
    }
}

impl ToTokens for Type {
    fn to_tokens(&self, toks: &mut TokenStream) {
        toks.extend(match self {
            Self::Bool => quote!(Bool),
            Self::Mem => quote!(Mem),
            Self::Dom => quote!(Dom),
            Self::Htd => quote!(Htd),
            Self::Pms => quote!(Pms),
            Self::Unit => quote!(Unit),
            Self::Type => quote!(Type),
            Self::Token => quote!(Token),
            Self::RelWrapper => quote!(RelWrapper),
            Self::Word(num_bits) => {
                let num_bits = parse(format!("{num_bits}"));
                quote!(Word(#num_bits))
            }
            Self::WordArray { length, bits } => {
                let inner = Self::Array(Box::new(Self::Word(*bits)), *length);
                quote!(WordArray(#inner))
            }
            Self::Array(ty, length) => {
                let length = parse(format!("{length:#x?}"));
                quote!([#ty; #length])
            }
            Self::Struct(name) => {
                let name = escape_ident(name);
                quote!(Struct(#name))
            }
            Self::Ptr(ty) => quote!(*#ty),
        });
    }
}

impl Expr {
    fn to_tokens_sub(&self) -> TokenStream {
        let (mut toks, is_compound) = self.to_tokens_is_compound();
        if is_compound {
            toks = quote! {
                (#toks)
            }
        }
        toks
    }

    fn to_tokens_is_compound(&self) -> (TokenStream, bool) {
        let ty = &self.ty;
        let mut is_compound = false;
        let toks = match &self.value {
            ExprValue::Var(name) => {
                let name = escape_ident(name);
                quote! {
                    #name
                }
            }
            ExprValue::Op(op, exprs) => {
                if let Some(op) = binop(&op) {
                    is_compound = true;
                    let op = parse(op);
                    let (x, y) = match exprs.as_slice() {
                        [x, y] => (x, y),
                        _ => panic!(),
                    };
                    let x = x.to_tokens_sub();
                    let y = y.to_tokens_sub();
                    quote! {
                        #x #op #y
                    }
                } else {
                    match op {
                        Op::True => quote!(true),
                        Op::False => quote!(false),
                        Op::Not | Op::BWNot => {
                            let x = match exprs.as_slice() {
                                [x] => x,
                                _ => panic!(),
                            };
                            let x = x.to_tokens_sub();
                            quote!(!#x)
                        }
                        Op::WordCast => {
                            let bits = match ty {
                                Type::Word(bits) => bits,
                                _ => panic!(),
                            };
                            let t = parse(format!("u{:?}", bits));
                            let x = match exprs.as_slice() {
                                [x] => x,
                                _ => panic!(),
                            };
                            let x = x.to_tokens_sub();
                            is_compound = true;
                            quote! {
                                #x as #t
                            }
                        }
                        Op::WordCastSigned => {
                            let bits = match ty {
                                Type::Word(bits) => bits,
                                _ => panic!(),
                            };
                            let t = parse(format!("u{:?}", bits));
                            let x = match exprs.as_slice() {
                                [x] => x,
                                _ => panic!(),
                            };
                            let x = x.to_tokens_sub();
                            is_compound = true;
                            quote! {
                                #x as Signed<#t>
                            }
                        }
                        Op::IfThenElse => {
                            let (x, y, z) = match exprs.as_slice() {
                                [x, y, z] => (x, y, z),
                                _ => panic!(),
                            };
                            is_compound = true;
                            quote! {
                                if #x { #y } else { #z }
                            }
                        }
                        _ => {
                            let op_name = escape_ident(&op.to_op_name());
                            quote! {
                                #op_name(#(#exprs),*)
                            }
                        }
                    }
                }
            }
            ExprValue::Num(val) => {
                let _bits = match ty {
                    Type::Word(bits) => bits,
                    _ => panic!(),
                };
                let s = format!("{:#x}", val);
                let val = parse(&s);
                quote! {
                    #val
                }
            }
            ExprValue::Type => {
                quote! {
                    Type
                }
            }
            ExprValue::Symbol(name) => {
                let name = escape_ident(name);
                quote! {
                    Symbol(#name)
                }
            }
            ExprValue::Token(name) => {
                let name = escape_ident(name);
                quote! {
                    Token(#name)
                }
            }
        };
        (toks, is_compound)
    }
}

impl ToTokens for Expr {
    fn to_tokens(&self, toks: &mut TokenStream) {
        let (t, _) = self.to_tokens_is_compound();
        toks.extend(quote! {
            #t
        });
    }
}

fn binop(op: &Op) -> Option<&str> {
    Some(match op {
        Op::Plus => "+",
        Op::Minus => "-",
        Op::Times => "*",
        Op::Modulus => "%",
        Op::DividedBy => "/",
        Op::BWAnd => "&",
        Op::BWOr => "|",
        Op::BWXOR => "^",
        Op::And => "&&",
        Op::Or => "||",
        Op::Equals => "==",
        Op::Less => "<",
        Op::LessEquals => "<=",
        Op::ShiftLeft => "<<",
        Op::ShiftRight => ">>",
        _ => return None,
    })
}

fn escape_ident(ident: &Ident) -> proc_macro2::Ident {
    let s = ident
        .clone()
        .replace(",", "_")
        .replace("%", "_")
        .replace("\\", "_")
        .replace(".", "·")
        .replace("'", "_")
        .replace("#", "_");
    // 𐌟 ˆ
    syn::parse_str(&s).unwrap_or_else(|_| panic!("bad identifier: {:?}", ident))
}

fn parse(s: impl AsRef<str>) -> TokenStream {
    TokenStream::from_str(s.as_ref()).unwrap()
}

#[cfg(test)]
mod tests {
    use crate::tests::utils::*;

    use super::*;

    use crate::sel4::TargetDir;

    fn z<T: ToTokens>(v: T) -> String {
        match try_pp_keeping_failure(&v) {
            Ok(s) => s,
            Err((e, s)) => {
                write("bad.rs", &s);
                panic!("{}: {:?}", e, e.span().start());
            }
        }
    }

    #[test]
    fn x() {
        let t = TargetDir::new(sel4_target_dir(), Default::default());
        z(t.read_c_functions_file());
        z(t.read_asm_functions_file());
        z(t.read_pairings_file());
        z(t.read_problems_file());
        z(t.read_proofs_file());
    }
}

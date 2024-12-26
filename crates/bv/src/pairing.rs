#![allow(warnings)]

use std::{fmt, iter};

use regex::Regex;

use crate::abstract_syntax::{Argument, Expr, Ident, Type};
use crate::concrete_syntax::parse::{
    LineBuffer, LinesBuffer, ParseError, ParseFromLine, ParseFromLines,
};
use crate::concrete_syntax::print::{BlockBuf, LineBuf, ToTokens};
use crate::logic::split_scalar_pairs;

// TODO ordering throughout
#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum Tag {
    C,
    Asm,
}

impl ParseFromLine for Tag {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        Ok(match tok.as_str() {
            "C" => Self::C,
            "ASM" => Self::Asm,
            _ => return Err(ParseError::UnexpectedToken(tok.location())),
        })
    }
}

impl ToTokens for Tag {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(match self {
            Self::C => "C",
            Self::Asm => "ASM",
        });
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct PairingId {
    pub(crate) asm: Ident,
    pub(crate) c: Ident,
}

impl PairingId {
    pub(crate) fn pretty_print(&self) -> String {
        format!("{} (ASM) <= {} (C)", self.asm, self.c)
    }

    pub(crate) fn parse_from_str(s: &str) -> Self {
        let re = Regex::new(r"^(?<asm>\S+) \(ASM\) <= (?<c>\S+) \(C\)$").unwrap();
        let caps = re.captures(s).unwrap();
        Self {
            c: caps["c"].to_owned(),
            asm: caps["asm"].to_owned(),
        }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Default)]
pub(crate) struct Pairing {
    pub(crate) in_eqs: Vec<Eq>,
    pub(crate) out_eqs: Vec<Eq>,
}

impl Pairing {
    pub(crate) fn formulate(
        min_stack_size: &Expr,
        c_input: &[Argument],
        c_output: &[Argument],
    ) -> Self {
        let (var_c_args, c_imem, _glob_c_args) = split_scalar_pairs(c_input);
        let (var_c_rets, c_omem, _glob_c_rets) = split_scalar_pairs(c_output);
        Self::formulate_arm_none_eabi_gnu(
            min_stack_size,
            &var_c_rets,
            &var_c_args,
            &c_imem,
            &c_omem,
        )
    }

    pub fn formulate_arm_none_eabi_gnu(
        min_stack_size: &Expr,
        var_c_args: &[Argument],
        var_c_rets: &[Argument],
        c_imem: &[Argument],
        o_omem: &[Argument],
    ) -> Self {
        let mut in_eqs = vec![];
        let mut out_eqs = vec![];
        let r = (0..=14)
            .map(|i| Expr::mk_machine_word_var(format!("r{i}")))
            .collect::<Vec<_>>();
        let stack_pointer = &r[13];
        let stack = Expr::mk_var("stack".to_owned(), Type::Mem);
        let r0_input = Expr::mk_machine_word_var("ret_addr_input".to_owned());
        let asm_mem = Expr::mk_var("mem".to_owned(), Type::Mem);

        let ret = Expr::mk_machine_word_var("ret".to_owned());

        let mut preconds = vec![];
        let mut post_eqs = vec![];

        preconds.push(stack_pointer.clone().mk_aligned(2));
        preconds.push(ret.clone().mk_eq(r[14].clone()));
        preconds.push(ret.clone().mk_aligned(2));
        preconds.push(r0_input.clone().mk_eq(r[0].clone()));
        preconds.push(min_stack_size.clone().mk_less_eq(stack_pointer.clone()));

        for i in (4..=11).chain(iter::once(13)) {
            post_eqs.push((r[i].clone(), r[i].clone()));
        }

        in_eqs.extend(preconds.into_iter().map(|expr| ASM_IN.side(expr).mk_eq(ASM_IN.side(Expr::mk_true()))));

        let asm_invs = post_eqs.into_iter().map(|(vin, vout)| ASM_IN.side(vin).mk_eq(ASM_OUT.side(vout)));
        out_eqs.extend(asm_invs);

        Self { in_eqs, out_eqs }
    }

    pub(crate) fn pretty_print_into_block(&self) -> BlockBuf {
        let mut block = BlockBuf::new();
        block.push_line_with(|line| line.to_tokens("Pairing"));
        for (dir, eqs) in [
            (EqDirection::In, &self.in_eqs),
            (EqDirection::Out, &self.out_eqs),
        ] {
            for eq in eqs {
                block.push_line_with(|line| {
                    line.to_tokens(&dir);
                    line.to_tokens(eq);
                });
            }
        }
        block.push_line_with(|line| line.to_tokens("EndPairing"));
        block
    }
}

impl ParseFromLines for Pairing {
    fn parse(lines: &mut LinesBuffer) -> Result<Self, ParseError> {
        let mut pairing = Pairing::default();
        lines.parse_next_line_with(|toks| toks.match_("Pairing"))?;
        loop {
            if lines.peek_token().unwrap().as_str() == "EndPairing" {
                lines.parse_next_line_with(|toks| toks.advance())?;
                break;
            } else {
                let (dir, eq) = lines.parse_next_line()?;
                let v = match dir {
                    EqDirection::In => &mut pairing.in_eqs,
                    EqDirection::Out => &mut pairing.out_eqs,
                };
                v.push(eq);
            }
        }
        Ok(pairing)
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct Eq {
    pub(crate) lhs: EqSide,
    pub(crate) rhs: EqSide,
}

impl Eq {
    pub(crate) fn new(lhs: EqSide, rhs: EqSide) -> Self {
        Self { lhs, rhs }
    }
}

impl ParseFromLine for Eq {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            lhs: toks.parse()?,
            rhs: toks.parse()?,
        })
    }
}

impl ToTokens for Eq {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.lhs);
        line.to_tokens(&self.rhs);
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct EqSide {
    pub(crate) quadrant: EqSideQuadrant,
    pub(crate) expr: Expr,
}

impl EqSide {
    pub(crate) fn new(quadrant: EqSideQuadrant, expr: Expr) -> Self {
        Self { quadrant, expr }
    }

    pub(crate) fn mk_eq(self, rhs: Self) -> Eq {
        Eq::new(self, rhs)
    }
}

impl ParseFromLine for EqSide {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            quadrant: toks.parse()?,
            expr: toks.parse()?,
        })
    }
}

impl ToTokens for EqSide {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.quadrant);
        line.to_tokens(&self.expr);
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct EqSideQuadrant {
    pub(crate) tag: Tag,
    pub(crate) direction: EqDirection,
}

impl EqSideQuadrant {
    pub(crate) const fn new(tag: Tag, direction: EqDirection) -> Self {
        Self { tag, direction }
    }

    pub(crate) fn side(self, expr: Expr) -> EqSide {
        EqSide::new(self, expr)
    }
}

pub(crate) const ASM_IN: EqSideQuadrant = EqSideQuadrant::new(Tag::Asm, EqDirection::In);
pub(crate) const ASM_OUT: EqSideQuadrant = EqSideQuadrant::new(Tag::Asm, EqDirection::Out);
pub(crate) const C_IN: EqSideQuadrant = EqSideQuadrant::new(Tag::C, EqDirection::In);
pub(crate) const C_OUT: EqSideQuadrant = EqSideQuadrant::new(Tag::C, EqDirection::Out);

impl ParseFromLine for EqSideQuadrant {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        let (s_tag, s_dir) = tok
            .as_str()
            .split_once('_')
            .ok_or_else(|| ParseError::UnexpectedToken(tok.location()))?;
        Ok(Self {
            tag: match s_tag {
                "C" => Tag::C,
                "ASM" => Tag::Asm,
                _ => return Err(ParseError::UnexpectedToken(tok.location())),
            },
            direction: match s_dir {
                "IN" => EqDirection::In,
                "OUT" => EqDirection::Out,
                _ => return Err(ParseError::UnexpectedToken(tok.location())),
            },
        })
    }
}

impl fmt::Display for EqSideQuadrant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match &self.tag {
            Tag::C => "C",
            Tag::Asm => "ASM",
        })?;
        f.write_str("_")?;
        f.write_str(match &self.direction {
            EqDirection::In => "IN",
            EqDirection::Out => "OUT",
        })?;
        Ok(())
    }
}

impl ToTokens for EqSideQuadrant {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.display_to_tokens(self);
    }
}

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum EqDirection {
    In,
    Out,
}

impl ParseFromLine for EqDirection {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        Ok(match tok.as_str() {
            "IN" => Self::In,
            "OUT" => Self::Out,
            _ => return Err(ParseError::UnexpectedToken(tok.location())),
        })
    }
}

impl ToTokens for EqDirection {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(match self {
            Self::In => "IN",
            Self::Out => "OUT",
        });
    }
}

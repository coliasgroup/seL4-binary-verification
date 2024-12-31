use std::collections::BTreeMap;

use crate::{
    abstract_syntax::{Expr, NodeId, Num},
    concrete_syntax::{
        parse::{LineBuffer, ParseError, ParseFromLine},
        print::{LineBuf, ToTokens},
    },
    pairing::{Pairing, PairingId, Tag},
    problem::Problem,
    proof_script::ProofNode,
};

pub(crate) fn proof_checks(
    pairing_id: &PairingId,
    pairings: &BTreeMap<PairingId, Pairing>,
    problem: &Problem,
    proof: &ProofNode,
) -> ProofChecks<()> {
    let pairing = &pairings[pairing_id];
    todo!()
}

fn inst_eqs(
    problem: &Problem,
    pairing: &Pairing,
    restrs: &[Restr],
    eqs: &[Eq],
    tag_map: &BTreeMap<Tag, Tag>,
) -> Vec<Hyp> {
    todo!()
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ProofChecks<M> {
    pub(crate) checks: Vec<ProofCheck<M>>,
}

impl<M> ProofChecks<M> {
    pub(crate) fn map_meta<M1>(self, mut f: impl FnMut(M) -> M1) -> ProofChecks<M1> {
        ProofChecks {
            checks: self
                .checks
                .into_iter()
                .map(|check| check.map_meta(&mut f))
                .collect(),
        }
    }

    pub(crate) fn strip_meta(self) -> ProofChecks<()> {
        self.map_meta(|_| ())
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ProofCheck<M> {
    pub(crate) meta: M,
    pub(crate) hyps: Vec<Hyp>,
    pub(crate) hyp: Hyp,
}

impl<M> ProofCheck<M> {
    pub(crate) fn map_meta<M1>(self, f: impl FnOnce(M) -> M1) -> ProofCheck<M1> {
        ProofCheck {
            meta: f(self.meta),
            hyps: self.hyps,
            hyp: self.hyp,
        }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum Hyp {
    PcImp(PcImpHyp),
    Eq { eq: EqHyp, if_at: bool },
}

impl Hyp {
    pub(crate) fn mk_eq_with_if_at(
        lhs: EqHypSide,
        rhs: EqHypSide,
        induct: Option<EqHypInduct>,
        if_at: bool,
    ) -> Self {
        Self::Eq {
            eq: EqHyp { lhs, rhs, induct },
            if_at,
        }
    }

    pub(crate) fn mk_eq(lhs: EqHypSide, rhs: EqHypSide, induct: Option<EqHypInduct>) -> Self {
        Self::mk_eq_with_if_at(lhs, rhs, induct, false)
    }

    pub(crate) fn mk_eq_if_at(lhs: EqHypSide, rhs: EqHypSide, induct: Option<EqHypInduct>) -> Self {
        Self::mk_eq_with_if_at(lhs, rhs, induct, false)
    }

    pub(crate) fn mk_true_if_at(
        expr: Expr,
        vis: &VisitWithTag,
        induct: Option<EqHypInduct>,
    ) -> Self {
        Self::mk_eq_if_at(
            EqHypSide::new(expr, vis.clone()),
            EqHypSide::new(Expr::mk_true(), vis.clone()),
            induct,
        )
    }

    pub(crate) fn pc_true(vis: VisitWithTag) -> Self {
        Self::PcImp(PcImpHyp {
            lhs: PcImpHypSide::Bool(true),
            rhs: PcImpHypSide::Pc(vis),
        })
    }

    pub(crate) fn pc_false(vis: VisitWithTag) -> Self {
        Self::PcImp(PcImpHyp {
            lhs: PcImpHypSide::Pc(vis),
            rhs: PcImpHypSide::Bool(false),
        })
    }

    pub(crate) fn pc_triv(vis: &VisitWithTag) -> Self {
        Self::PcImp(PcImpHyp {
            lhs: PcImpHypSide::Pc(vis.clone()),
            rhs: PcImpHypSide::Pc(vis.clone()),
        })
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct PcImpHyp {
    lhs: PcImpHypSide,
    rhs: PcImpHypSide,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum PcImpHypSide {
    Bool(bool),
    Pc(VisitWithTag),
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct EqHyp {
    lhs: EqHypSide,
    rhs: EqHypSide,
    induct: Option<EqHypInduct>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct EqHypSide {
    expr: Expr,
    visit: VisitWithTag,
}

impl EqHypSide {
    pub(crate) fn new(expr: Expr, visit: VisitWithTag) -> Self {
        Self { expr, visit }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct VisitWithTag {
    visit: Visit,
    tag: Tag,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct Visit {
    node_id: NodeId,
    restrs: Vec<Restr>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct Restr {
    node_id: NodeId,
    visit_count: VisitCount,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct VisitCount {
    numbers: Vec<Num>,
    offsets: Vec<Num>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct EqHypInduct {
    a: Num,
    b: Num,
}

impl ParseFromLine for Hyp {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        Ok(match tok.as_str() {
            "PCImp" => Self::PcImp(toks.parse()?),
            "Eq" => Self::Eq {
                eq: toks.parse()?,
                if_at: false,
            },
            "EqIfAt" => Self::Eq {
                eq: toks.parse()?,
                if_at: true,
            },
            _ => return Err(ParseError::UnexpectedToken(tok.location())),
        })
    }
}

impl ToTokens for Hyp {
    fn to_tokens(&self, line: &mut LineBuf) {
        match self {
            Self::PcImp(inner) => {
                line.to_tokens("PCImp");
                line.to_tokens(inner);
            }
            Self::Eq { eq, if_at } => {
                line.to_tokens(if *if_at { "EqIfAt" } else { "Eq" });
                line.to_tokens(eq);
            }
        }
    }
}

impl ParseFromLine for PcImpHyp {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            lhs: toks.parse()?,
            rhs: toks.parse()?,
        })
    }
}

impl ToTokens for PcImpHyp {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.lhs);
        line.to_tokens(&self.rhs);
    }
}

impl ParseFromLine for PcImpHypSide {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        Ok(match tok.as_str() {
            "True" => Self::Bool(true),
            "False" => Self::Bool(false),
            "PC" => Self::Pc(toks.parse()?),
            _ => return Err(ParseError::UnexpectedToken(tok.location())),
        })
    }
}

impl ToTokens for PcImpHypSide {
    fn to_tokens(&self, line: &mut LineBuf) {
        match self {
            Self::Bool(inner) => {
                line.to_tokens(if *inner { "True" } else { "False" });
            }
            Self::Pc(visit) => {
                line.to_tokens("PC");
                line.to_tokens(visit);
            }
        }
    }
}

impl ParseFromLine for EqHyp {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            lhs: toks.parse()?,
            rhs: toks.parse()?,
            induct: {
                let a = parse_induct(toks, None)?;
                let b = parse_induct(toks, Some(a.is_some()))?;
                match (a, b) {
                    (Some(a), Some(b)) => Some(EqHypInduct { a, b }),
                    (None, None) => None,
                    _ => panic!(),
                }
            },
        })
    }
}

fn parse_induct(
    toks: &mut LineBuffer,
    expectation: Option<bool>,
) -> Result<Option<Num>, ParseError> {
    let peek = toks.peek()?;
    if peek.as_str() == "None" && !matches!(expectation, Some(true)) {
        toks.advance().unwrap();
        Ok(None)
    } else if !matches!(expectation, Some(false)) {
        Ok(Some(toks.parse()?))
    } else {
        Err(ParseError::UnexpectedToken(peek.location()))
    }
}

impl ToTokens for EqHyp {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.lhs);
        line.to_tokens(&self.rhs);
        match &self.induct {
            Some(induct) => {
                line.display_to_tokens(&induct.a);
                line.display_to_tokens(&induct.b);
            }
            None => {
                line.to_tokens("None");
                line.to_tokens("None");
            }
        }
    }
}

impl ParseFromLine for EqHypSide {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            expr: toks.parse()?,
            visit: toks.parse()?,
        })
    }
}

impl ToTokens for EqHypSide {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.expr);
        line.to_tokens(&self.visit);
    }
}

impl ParseFromLine for VisitWithTag {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            visit: toks.parse()?,
            tag: toks.parse()?,
        })
    }
}

impl ToTokens for VisitWithTag {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.visit);
        line.to_tokens(&self.tag);
    }
}

impl ParseFromLine for Visit {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            node_id: toks.parse()?,
            restrs: toks.parse()?,
        })
    }
}

impl ToTokens for Visit {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.node_id);
        line.to_tokens(&*self.restrs);
    }
}

impl ParseFromLine for Restr {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            node_id: toks.parse()?,
            visit_count: toks.parse()?,
        })
    }
}

impl ToTokens for Restr {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.node_id);
        line.to_tokens(&self.visit_count);
    }
}

impl ParseFromLine for VisitCount {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        if tok.as_str() != "VC" {
            return Err(ParseError::UnexpectedToken(tok.location()));
        }
        Ok(Self {
            numbers: toks.parse()?,
            offsets: toks.parse()?,
        })
    }
}

impl ToTokens for VisitCount {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens("VC");
        line.iter_to_tokens_with(self.numbers.iter(), |x, line| line.display_to_tokens(x));
        line.iter_to_tokens_with(self.offsets.iter(), |x, line| line.display_to_tokens(x));
    }
}

#[cfg(test)]
mod tests {
    use crate::sel4;
    use crate::tests::utils::*;

    use super::*;

    fn t() -> sel4::TargetDir {
        sel4::TargetDir::usual(sel4_target_dir())
    }

    #[test]
    #[ignore]
    fn x() {
        let t = t();
        let pairings = t.build_pairings();
        let their_pairings = t.read_pairings_file();
        assert_eq!(pairings, their_pairings.pairings);
        let problems = t.build_problems();
        let their_proofs = t.read_proofs_file();
        let their_proof_checks = t.read_proof_checks_file();
        let mut failure = false;
        for (pairing_id, their_problem_proof) in &their_proofs.problems {
            let their_problem = &their_problem_proof.problem;
            let their_proof = &their_problem_proof.proof;
            let proof = their_proof;
            let problem = problems[pairing_id].clone().strip_meta();
            println!("{pairing_id:?}");
            assert_eq!(&problem, their_problem);
            // println!("{proof:?}");
            println!("checking...");
            let checks = proof_checks(pairing_id, &pairings, &problem, proof);
            let their_checks = their_proof_checks.problems[pairing_id].clone().strip_meta();
            if &checks != &their_checks {
                failure = true;
                println!("FAILURE: {:?}", pairing_id.pretty_print());
            }
            println!("done");
        }
        assert!(!failure);
    }
}

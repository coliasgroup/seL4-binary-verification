use std::collections::{BTreeMap, BTreeSet};

use env_logger::init;

use crate::{
    abstract_syntax::{Expr, File, HasNodeGraph, Ident, NodeAddr, NodeId, Num},
    concrete_syntax::{
        parse::{LineBuffer, ParseError, ParseFromLine},
        print::{LineBuf, ToTokens},
    },
    pairing::{Pairing, PairingEq, PairingEqDirection, PairingEqSideQuadrant, PairingId, Tag},
    problem::Problem,
    proof_script::{ProofNode, RestrProofNodeKind, RestrProofNodeRange},
    utils::petgraph::algorithms::{is_reachable_from, reachable_nodes, tarjan_scc_variant},
};

struct LoopData {
    x: BTreeMap<NodeId, LoopDataEntry>,
}

enum LoopDataEntry {
    Head(BTreeSet<NodeId>),
    Member(NodeId),
}

impl LoopData {
    fn new(problem: &Problem) -> Self {
        let sccs_with_heads = tarjan_scc_variant(&problem.nodes.abstract_node_graph(), || {
            [problem.c.entry, problem.asm.entry]
        });
        let mut x = BTreeMap::new();
        for (head, scc) in sccs_with_heads {
            for addr in &scc {
                x.insert(*addr, LoopDataEntry::Member(head));
            }
            x.insert(head, LoopDataEntry::Head(BTreeSet::from_iter(scc)));
        }
        Self { x }
    }

    fn get(&self, node: NodeId) -> Option<&LoopDataEntry> {
        self.x.get(&node)
    }

    fn loop_id(&self, node: NodeId) -> Option<NodeId> {
        self.get(node).map(|entry| match entry {
            LoopDataEntry::Head(_) => node,
            LoopDataEntry::Member(node_) => *node_,
        })
    }

    fn loop_heads(&self) -> Vec<NodeId> {
        self.x
            .iter()
            .filter_map(|(n, e)| match e {
                LoopDataEntry::Head(_) => Some(*n),
                LoopDataEntry::Member(_) => None,
            })
            .collect()
    }
}

pub(crate) fn proof_checks(
    pairing_id: &PairingId,
    pairings: &BTreeMap<PairingId, Pairing>,
    functions: &File,
    problem: &Problem,
    proof: &ProofNode,
) -> ProofChecks<String> {
    let pairing = &pairings[pairing_id];
    let loop_data = LoopData::new(problem);
    let mut node_tags = BTreeMap::new();
    for n in reachable_nodes(&problem.nodes.abstract_node_graph(), [problem.asm.entry]) {
        if let NodeId::Addr(addr) = n {
            node_tags.insert(addr, Tag::Asm);
        }
    }
    for n in reachable_nodes(&problem.nodes.abstract_node_graph(), [problem.c.entry]) {
        if let NodeId::Addr(addr) = n {
            node_tags.insert(addr, Tag::C);
        }
    }
    let builder = ProofChecksBuilder {
        ctxt: ProofChecksBuilderContext {
            pairings,
            functions,
            pairing_id,
            pairing,
            problem,
            loop_data,
            node_tags,
        },
    };
    let hyps = init_point_hyps(functions, problem, pairing);
    let checks = builder.proof_checks_rec(vec![], hyps, proof, "root".to_string());
    ProofChecks { checks }
}

struct ProofChecksBuilderContext<'a> {
    pairings: &'a BTreeMap<PairingId, Pairing>,
    functions: &'a File,
    pairing_id: &'a PairingId,
    pairing: &'a Pairing,
    problem: &'a Problem,
    loop_data: LoopData,
    node_tags: BTreeMap<NodeAddr, Tag>,
}

struct ProofChecksBuilder<'a> {
    ctxt: ProofChecksBuilderContext<'a>,
}

const RESTR_BUMP: u64 = 0;

fn get_proof_restr(point: NodeAddr, range: &RestrProofNodeRange) -> Restr {
    Restr {
        node_id: NodeId::Addr(point),
        visit_count: VisitCount::mk_options(
            (range.x..(range.y + RESTR_BUMP))
                .map(|n| VisitCount::mk_from_restr_kind(range.kind, n)),
        ),
    }
}

impl<'a> ProofChecksBuilder<'a> {
    fn proof_checks_rec(
        &self,
        restrs: Vec<Restr>,
        hyps: Vec<Hyp>,
        proof: &ProofNode,
        path: String,
    ) -> Vec<ProofCheck<String>> {
        match proof {
            ProofNode::Leaf => self
                .leaf_condition_checks(restrs, hyps)
                .into_iter()
                .map(|check| check.map_meta(|name| format!("{name} on {path}")))
                .collect(),
            ProofNode::CaseSplit(proof) => {
                todo!()
            }
            ProofNode::Restr(proof) => {
                let mut checks = self
                    .restr_checks(&proof.point, &proof.range, restrs.clone(), hyps.clone())
                    .into_iter()
                    .map(|check| check.map_meta(|name| format!("{name} on {path}")))
                    .collect::<Vec<_>>();
                let restr = get_proof_restr(proof.point, &proof.range);
                let mut next_restrs = vec![restr];
                next_restrs.extend(restrs.iter().cloned());
                let mut next_hyps = hyps.clone();
                next_hyps.push(Hyp::pc_triv(&VisitWithTag {
                    visit: Visit::new(NodeId::Addr(proof.point), {
                        let mut this = vec![Restr {
                            node_id: NodeId::Addr(proof.point),
                            visit_count: VisitCount::mk_from_restr_kind(
                                proof.range.kind,
                                proof.range.y - 1,
                            ),
                        }];
                        this.extend(restrs.clone());
                        this
                    }),
                    tag: proof.tag,
                }));
                let next_path = format!("{} ({:?} limited)", path, proof.point);
                let next_checks =
                    self.proof_checks_rec(next_restrs, next_hyps, &proof.child, next_path);
                checks.extend(next_checks);
                checks
            }
            ProofNode::Split(proof) => {
                todo!()
            }
            ProofNode::SingleRevInduct(proof) => {
                todo!()
            }
        }
    }

    fn leaf_condition_checks(
        &self,
        restrs: Vec<Restr>,
        orig_hyps: Vec<Hyp>,
    ) -> Vec<ProofCheck<String>> {
        let nrerr_pc_hyp = non_r_err_pc_hyp(restrs.clone());
        let mut hyps = vec![nrerr_pc_hyp];
        hyps.extend(orig_hyps);
        let nlerr_pc = Hyp::pc_false(VisitWithTag::new(
            Visit::new(NodeId::Err, restrs.clone()),
            Tag::Asm,
        ));
        let ret_eq = Hyp::mk_eq(
            EqHypSide::new(
                Expr::mk_true(),
                VisitWithTag::new(Visit::new(NodeId::Ret, restrs.clone()), Tag::Asm),
            ),
            EqHypSide::new(
                Expr::mk_true(),
                VisitWithTag::new(Visit::new(NodeId::Ret, restrs.clone()), Tag::C),
            ),
            None,
        );
        let out_eqs = &self.ctxt.pairing.out_eqs;
        let mut checks = vec![ProofCheck {
            hyps: {
                let mut this = hyps.clone();
                this.push(ret_eq.clone());
                this
            },
            hyp: nlerr_pc.clone(),
            meta: "Leaf path-cond imp".to_owned(),
        }];
        for hyp in inst_eqs(
            &self.ctxt.functions,
            &self.ctxt.problem,
            &restrs,
            out_eqs,
            None,
        ) {
            checks.push(ProofCheck {
                meta: "Leaf eq check".to_owned(),
                hyps: {
                    let mut this = hyps.clone();
                    this.push(nlerr_pc.clone());
                    this.push(ret_eq.clone());
                    this
                },
                hyp,
            })
        }
        checks
    }

    fn restr_checks(
        &self,
        point: &NodeAddr,
        range: &RestrProofNodeRange,
        restrs: Vec<Restr>,
        orig_hyps: Vec<Hyp>,
    ) -> Vec<ProofCheck<String>> {
        let restr = get_proof_restr(*point, range);
        let nrerr_pc_hyp = non_r_err_pc_hyp(self.restr_others(
            {
                let mut this = vec![restr];
                this.extend(restrs.clone());
                this
            },
            2,
        ));
        let mut new_hyps = vec![nrerr_pc_hyp];
        new_hyps.extend(orig_hyps.clone());

        let min_vc = match range.kind {
            RestrProofNodeKind::Offset => Some(VisitCount::mk_offset(range.x.saturating_sub(1))),
            _ if range.x > 1 => Some(VisitCount::mk_number(range.x - 1)),
            _ => None,
        };
        let init_check = match min_vc {
            Some(min_vc) => vec![ProofCheck {
                meta: format!(
                    "Check of restr min {:?} {:?} for {:?}",
                    range.x, range.kind, point
                ),
                hyp: Hyp::pc_true(VisitWithTag {
                    tag: self.ctxt.node_tags[&point],
                    visit: Visit {
                        node_id: NodeId::Addr(*point),
                        restrs: {
                            let mut this = vec![Restr {
                                node_id: NodeId::Addr(*point),
                                visit_count: min_vc,
                            }];
                            this.extend(restrs.clone());
                            this
                        },
                    },
                }),
                hyps: new_hyps.clone(),
            }],
            None => vec![],
        };
        let top_vc = match range.kind {
            RestrProofNodeKind::Offset => VisitCount::mk_offset(range.y - 1),
            RestrProofNodeKind::Number => VisitCount::mk_number(range.y - 1),
        };
        let top_check = ProofCheck {
            meta: format!(
                "Check of restr max {:?} {:?} for {:?}",
                range.y, range.kind, point
            ),
            hyp: Hyp::pc_false(VisitWithTag {
                tag: self.ctxt.node_tags[&point],
                visit: Visit {
                    node_id: NodeId::Addr(*point),
                    restrs: {
                        let mut this = vec![Restr {
                            node_id: NodeId::Addr(*point),
                            visit_count: top_vc,
                        }];
                        this.extend(restrs.clone());
                        this
                    },
                },
            }),
            hyps: new_hyps.clone(),
        };

        let mut ret = init_check;
        ret.push(top_check);
        ret
    }

    fn restr_others(&self, mut restrs: Vec<Restr>, n: u64) -> Vec<Restr> {
        let extras = self.loops_to_split(&restrs).into_iter().map(|sp| Restr {
            node_id: sp,
            visit_count: VisitCount::mk_up_to(n),
        });
        restrs.extend(extras);
        restrs
    }

    fn loops_to_split(&self, restrs: &Vec<Restr>) -> BTreeSet<NodeId> {
        let loop_heads_with_split = BTreeSet::from_iter(
            restrs
                .iter()
                .map(|restr| self.ctxt.loop_data.loop_id(restr.node_id)),
        );
        let mut rem_loop_heads = BTreeSet::from_iter(self.ctxt.loop_data.loop_heads());
        rem_loop_heads.retain(|h| !loop_heads_with_split.contains(&Some(*h)));
        for restr in restrs {
            if !restr.visit_count.has_zero() {
                rem_loop_heads.retain(|h| {
                    is_reachable_from(
                        &self.ctxt.problem.nodes.abstract_node_graph(),
                        restr.node_id,
                        [*h],
                    ) || self.ctxt.node_tags[&restr.node_id.addr().unwrap()]
                        != self.ctxt.node_tags[&h.addr().unwrap()]
                });
            }
        }
        rem_loop_heads
    }
}

fn non_r_err_pc_hyp(restrs: Vec<Restr>) -> Hyp {
    Hyp::pc_false(VisitWithTag::new(Visit::new(NodeId::Err, restrs), Tag::C))
}

fn inst_eqs(
    functions: &File,
    problem: &Problem,
    restrs: &[Restr],
    eqs: &[PairingEq],
    tag_map: Option<&BTreeMap<Tag, Tag>>,
) -> Vec<Hyp> {
    let mut addr_map = BTreeMap::new();
    let default_tag_map = BTreeMap::from_iter(Tag::iter().map(|tag| (tag.clone(), tag)));
    let tag_map = tag_map.unwrap_or(&default_tag_map);
    for (pair_tag, p_tag) in tag_map.iter() {
        addr_map.insert(
            pair_tag.with_direction(PairingEqDirection::In),
            VisitWithTag::new(
                Visit::new(problem.problem_side(*p_tag).entry, vec![]),
                *p_tag,
            ),
        );
        addr_map.insert(
            pair_tag.with_direction(PairingEqDirection::Out),
            VisitWithTag::new(Visit::new(NodeId::Ret, restrs.into()), *p_tag),
        );
    }
    let mut renames = problem_entry_exit_renames(functions, problem, tag_map.values().copied());
    for (pair_tag, p_tag) in tag_map.iter() {
        renames.insert(
            pair_tag.with_direction(PairingEqDirection::In),
            renames[&p_tag.with_direction(PairingEqDirection::In)].clone(),
        );
        renames.insert(
            pair_tag.with_direction(PairingEqDirection::Out),
            renames[&p_tag.with_direction(PairingEqDirection::Out)].clone(),
        );
    }
    let mut hyps = vec![];
    for eq in eqs {
        let lhs = EqHypSide::new(
            {
                let mut expr = eq.lhs.expr.clone();
                expr.rename_vars(|ident| renames[&eq.lhs.quadrant].get(ident).cloned());
                expr
            },
            addr_map[&eq.lhs.quadrant].clone(),
        );
        let rhs = EqHypSide::new(
            {
                let mut expr = eq.rhs.expr.clone();
                expr.rename_vars(|ident| renames[&eq.rhs.quadrant].get(ident).cloned());
                expr
            },
            addr_map[&eq.rhs.quadrant].clone(),
        );
        let hyp = Hyp::mk_eq(lhs, rhs, None);
        hyps.push(hyp);
    }
    hyps
}

fn problem_entry_exit_renames(
    functions: &File,
    problem: &Problem,
    tags: impl Iterator<Item = Tag>,
) -> BTreeMap<PairingEqSideQuadrant, BTreeMap<Ident, Ident>> {
    let mut renames = BTreeMap::new();
    for tag in tags {
        let f = &functions.functions[&problem.problem_side(tag).name];
        let input = &problem.problem_side(tag).input;
        let output = &problem.problem_side(tag).output;
        renames.insert(
            tag.with_direction(PairingEqDirection::In),
            BTreeMap::from_iter(
                f.input()
                    .iter()
                    .map(|arg| arg.name.clone())
                    .zip(input.iter().map(|arg| arg.name.clone())),
            ),
        );
        renames.insert(
            tag.with_direction(PairingEqDirection::Out),
            BTreeMap::from_iter(
                f.output()
                    .iter()
                    .map(|arg| arg.name.clone())
                    .zip(output.iter().map(|arg| arg.name.clone())),
            ),
        );
    }
    renames
}

fn init_point_hyps(functions: &File, problem: &Problem, pairing: &Pairing) -> Vec<Hyp> {
    inst_eqs(functions, problem, &[], &pairing.in_eqs, None)
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

impl VisitWithTag {
    pub(crate) fn new(visit: Visit, tag: Tag) -> Self {
        Self { visit, tag }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct Visit {
    node_id: NodeId,
    restrs: Vec<Restr>,
}

impl Visit {
    pub(crate) fn new(node_id: NodeId, restrs: Vec<Restr>) -> Self {
        Self { node_id, restrs }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct Restr {
    node_id: NodeId,
    visit_count: VisitCount,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct VisitCount {
    numbers: Vec<u64>,
    offsets: Vec<u64>,
}

impl VisitCount {
    fn mk_number(n: u64) -> Self {
        Self {
            numbers: vec![n],
            offsets: vec![],
        }
    }

    fn mk_offset(n: u64) -> Self {
        Self {
            numbers: vec![],
            offsets: vec![n],
        }
    }

    fn mk_options(it: impl Iterator<Item = Self>) -> Self {
        let mut this = Self {
            numbers: vec![],
            offsets: vec![],
        };
        for vc in it {
            this.numbers.extend(vc.numbers);
            this.offsets.extend(vc.offsets);
        }
        this
    }

    fn mk_from_restr_kind(kind: RestrProofNodeKind, n: u64) -> Self {
        match kind {
            RestrProofNodeKind::Number => Self::mk_number(n),
            RestrProofNodeKind::Offset => Self::mk_offset(n),
        }
    }

    fn mk_up_to(n: u64) -> Self {
        Self::mk_options((0..n).map(Self::mk_number))
    }

    fn has_zero(&self) -> bool {
        self.numbers.contains(&0)
    }
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
            numbers: toks.parse_vec_with(|toks| toks.parse_prim_int())?,
            offsets: toks.parse_vec_with(|toks| toks.parse_prim_int())?,
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
    use crate::compat::ProofChecksFile;
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
        let functions = t.build_functions_file();
        let pairings = t.build_pairings();
        let their_pairings = t.read_pairings_file();
        assert_eq!(pairings, their_pairings.pairings);
        let problems = t.build_problems();
        let their_proofs = t.read_proofs_file();
        let their_proof_checks = t.read_proof_checks_file();
        let mut failure = false;
        let mut compat = ProofChecksFile {
            problems: Default::default(),
        };
        for (pairing_id, their_problem_proof) in &their_proofs.problems {
            let their_problem = &their_problem_proof.problem;
            let their_proof = &their_problem_proof.proof;
            let proof = their_proof;
            let problem = problems[pairing_id].clone().strip_meta();
            println!("{pairing_id:?}");
            assert_eq!(&problem, their_problem);
            // println!("{proof:?}");
            println!("checking...");
            let checks = proof_checks(pairing_id, &pairings, &functions, &problem, proof);
            let their_checks = their_proof_checks.problems[pairing_id].clone();
            compat.problems.insert(pairing_id.clone(), checks.clone());
            if &checks != &their_checks {
                failure = true;
                println!("FAILURE: {:?}", pairing_id.pretty_print());
            }
            println!("done");
        }
        if failure {
            dump_juxt(
                "t/proof-checks.txt",
                "json",
                compat.pretty_print(),
                their_proof_checks.pretty_print(),
            );
        }
        assert!(!failure);
    }
}

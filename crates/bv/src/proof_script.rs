use crate::abstract_syntax::{Expr, Ident, NodeAddr, Type};
use crate::concrete_syntax::parse::{LineBuffer, Lines, ParseError, ParseFromLine};
use crate::concrete_syntax::print::{LineBuf, ToTokens};
use crate::pairing::Tag;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum ProofNode {
    Leaf,
    Restr(RestrProofNode),
    CaseSplit(CaseSplitProofNode),
    Split(SplitProofNode),
    SingleRevInduct(SingleRevInductProofNode),
}

impl ProofNode {
    pub(crate) fn parse_from_str(s: &str) -> Result<Self, ParseError> {
        Lines::tokenize(s).parse_with(|lines| lines.parse_next_line())
    }
}

impl ParseFromLine for ProofNode {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        Ok(match tok.as_str() {
            "Leaf" => Self::Leaf,
            "Restr" => Self::Restr(toks.parse()?),
            "CaseSplit" => Self::CaseSplit(toks.parse()?),
            "Split" => Self::Split(toks.parse()?),
            "SingleRevInduct" => Self::SingleRevInduct(toks.parse()?),
            _ => return Err(ParseError::UnexpectedToken(tok.location())),
        })
    }
}

impl ToTokens for ProofNode {
    fn to_tokens(&self, line: &mut LineBuf) {
        match self {
            Self::Leaf => {
                line.to_tokens("Leaf");
            }
            Self::Restr(inner) => {
                line.to_tokens("Restr");
                line.to_tokens(inner);
            }
            Self::CaseSplit(inner) => {
                line.to_tokens("CaseSplit");
                line.to_tokens(inner);
            }
            Self::Split(inner) => {
                line.to_tokens("Split");
                line.to_tokens(inner);
            }
            Self::SingleRevInduct(inner) => {
                line.to_tokens("SingleRevInduct");
                line.to_tokens(inner);
            }
        }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct RestrProofNode {
    pub(crate) addr: NodeAddr,
    pub(crate) tag: Tag,
    pub(crate) kind: RestrProofNodeKind,
    pub(crate) x: u64,
    pub(crate) y: u64,
    pub(crate) child: Box<ProofNode>,
}

impl ParseFromLine for RestrProofNode {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let addr = toks.parse_prim_int()?;
        let tag = toks.parse()?;
        let kind = toks.parse()?;
        let x = toks.parse_prim_int()?;
        let y = toks.parse_prim_int()?;
        let child = toks.parse()?;
        Ok(Self {
            addr,
            tag,
            kind,
            x,
            y,
            child,
        })
    }
}

impl ToTokens for RestrProofNode {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.display_to_tokens(&self.addr);
        line.to_tokens(&self.tag);
        line.to_tokens(&self.kind);
        line.display_to_tokens(&self.x);
        line.display_to_tokens(&self.y);
        line.to_tokens(&*self.child);
    }
}

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum RestrProofNodeKind {
    Number,
    Offset,
}

impl ParseFromLine for RestrProofNodeKind {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        Ok(match tok.as_str() {
            "Number" => Self::Number,
            "Offset" => Self::Offset,
            _ => return Err(ParseError::UnexpectedToken(tok.location())),
        })
    }
}

impl ToTokens for RestrProofNodeKind {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(match self {
            Self::Number => "Number",
            Self::Offset => "Offset",
        });
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct CaseSplitProofNode {
    pub(crate) addr: NodeAddr,
    pub(crate) tag: Tag,
    pub(crate) left: Box<ProofNode>,
    pub(crate) right: Box<ProofNode>,
}

impl ParseFromLine for CaseSplitProofNode {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let addr = toks.parse_prim_int()?;
        let tag = toks.parse()?;
        let left = toks.parse()?;
        let right = toks.parse()?;
        Ok(Self {
            addr,
            tag,
            left,
            right,
        })
    }
}

impl ToTokens for CaseSplitProofNode {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.display_to_tokens(&self.addr);
        line.to_tokens(&self.tag);
        line.to_tokens(&*self.left);
        line.to_tokens(&*self.right);
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct SplitProofNode {
    pub(crate) addr: NodeAddr,
    pub(crate) loop_r_max: usize,
    pub(crate) l_details: SplitProofNodeDetails,
    pub(crate) r_details: SplitProofNodeDetails,
    pub(crate) eqs: Vec<(Lambda, Lambda)>,
    pub(crate) p1: Box<ProofNode>,
    pub(crate) p2: Box<ProofNode>,
}

impl ParseFromLine for SplitProofNode {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let addr = toks.parse_prim_int()?;
        let loop_r_max = toks.parse_prim_int()?;
        let l_details = toks.parse()?;
        let r_details = toks.parse()?;
        let eqs = toks.parse()?;
        let p1 = toks.parse()?;
        let p2 = toks.parse()?;
        Ok(Self {
            addr,
            loop_r_max,
            l_details,
            r_details,
            eqs,
            p1,
            p2,
        })
    }
}

impl ToTokens for SplitProofNode {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.display_to_tokens(&self.addr);
        line.display_to_tokens(&self.loop_r_max);
        line.to_tokens(&self.l_details);
        line.to_tokens(&self.r_details);
        line.to_tokens(&self.eqs);
        line.to_tokens(&*self.p1);
        line.to_tokens(&*self.p2);
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct SplitProofNodeDetails {
    pub(crate) split: usize,
    pub(crate) seq_start: usize,
    pub(crate) step: usize,
    pub(crate) eqs: Vec<Lambda>,
}

impl ParseFromLine for SplitProofNodeDetails {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let split = toks.parse_prim_int()?;
        let seq_start = toks.parse_prim_int()?;
        let step = toks.parse_prim_int()?;
        let eqs = toks.parse()?;
        Ok(Self {
            split,
            seq_start,
            step,
            eqs,
        })
    }
}

impl ToTokens for SplitProofNodeDetails {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.display_to_tokens(&self.split);
        line.display_to_tokens(&self.seq_start);
        line.display_to_tokens(&self.step);
        line.to_tokens(&self.eqs);
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct SingleRevInductProofNode {
    pub(crate) point: NodeAddr,
    pub(crate) tag: Tag,
    pub(crate) n: usize,
    pub(crate) eqs: Vec<Lambda>,
    pub(crate) pred: Expr,
    pub(crate) n_bound: usize,
    pub(crate) child: Box<ProofNode>,
}

impl ParseFromLine for SingleRevInductProofNode {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let point = toks.parse_prim_int()?;
        let tag = toks.parse()?;
        let n = toks.parse_prim_int()?;
        let eqs = toks.parse()?;
        let pred = toks.parse()?;
        let n_bound = toks.parse_prim_int()?;
        let child = toks.parse()?;
        Ok(Self {
            point,
            tag,
            n,
            eqs,
            pred,
            n_bound,
            child,
        })
    }
}

impl ToTokens for SingleRevInductProofNode {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.display_to_tokens(&self.point);
        line.to_tokens(&self.tag);
        line.display_to_tokens(&self.n);
        line.to_tokens(&self.eqs);
        line.to_tokens(&self.pred);
        line.display_to_tokens(&self.n_bound);
        line.to_tokens(&*self.child);
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct Lambda {
    pub(crate) free_var: Ident,
    pub(crate) free_var_ty: Type,
    pub(crate) expr: Expr,
}

impl ParseFromLine for Lambda {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        toks.match_("Lambda")?;
        let free_var = toks.parse()?;
        let free_var_ty = toks.parse()?;
        let expr = toks.parse()?;
        Ok(Self {
            free_var,
            free_var_ty,
            expr,
        })
    }
}

impl ToTokens for Lambda {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens("Lambda");
        line.to_tokens(&self.free_var);
        line.to_tokens(&self.free_var_ty);
        line.to_tokens(&self.expr);
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::utils::*;

    use super::*;

    #[test]
    fn round_trip() {
        test_round_trip_path(
            graph_refine_dir().join("loop-example/O2/proof"),
            ProofNode::parse_from_str,
            ProofNode::pretty_print,
        );
    }
}

use std::collections::BTreeMap;
use std::fmt;
use std::fmt::Write;

use regex::Regex;

use crate::abstract_syntax::{Expr, Ident, NodeAddr, Num};
use crate::concrete_syntax::parse::{
    LineBuffer, Lines, LinesBuffer, ParseError, ParseFileAsLines, ParseFromLine, ParseFromLines,
};
use crate::concrete_syntax::print::{BlockBuf, FileBuf, LineBuf, ToTokens};
use crate::pairing::{Pairing, PairingId};
use crate::problem::{NodeBySource, Problem};
use crate::proof_script::ProofNode;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct StackBoundsFile {
    pub(crate) function_hash: Option<Num>,
    pub(crate) by_function: BTreeMap<Ident, Expr>,
}

impl StackBoundsFile {
    pub(crate) fn pretty_print(&self) -> String {
        let mut block = BlockBuf::new();

        if let Some(function_hash) = &self.function_hash {
            block.push_line_with(|line| {
                line.to_tokens("FunctionHash");
                line.display_to_tokens(function_hash);
            })
        }

        for (f_name, bound) in &self.by_function {
            block.push_line_with(|line| {
                line.to_tokens("StackBound");
                line.to_tokens(f_name);
                line.to_tokens(bound);
            })
        }

        block.pretty_print_into_string()
    }
}

impl ParseFileAsLines for StackBoundsFile {}

impl ParseFromLines for StackBoundsFile {
    fn parse(lines: &mut LinesBuffer) -> Result<Self, ParseError> {
        let function_hash = if lines
            .peek_token()
            .map(|tok| tok.as_str() == "FunctionHash")
            .unwrap_or(false)
        {
            Some(lines.parse_next_line_with(|toks| {
                toks.advance().unwrap();
                toks.parse()
            })?)
        } else {
            None
        };

        let mut by_function = BTreeMap::new();

        while !lines.is_empty() {
            let mut toks = lines.next().unwrap();
            toks.match_("StackBound")?;
            let f_name = toks.parse()?;
            let bound = toks.parse()?;
            by_function.insert(f_name, bound);
        }

        Ok(Self {
            function_hash,
            by_function,
        })
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ProofsFile {
    pub(crate) problems: BTreeMap<PairingId, ProblemProof>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ProblemProof {
    pub(crate) problem: Problem,
    pub(crate) proof: ProofNode,
}

impl ProofsFile {
    pub(crate) fn parse_from_str(s: &str) -> Result<Self, ParseError> {
        let mut problems = BTreeMap::new();
        for (pairing_id, body) in parse_compat_file(["ProblemProof", "Problem", "Paring"], s) {
            let problem_proof = body.parse_with(|lines| {
                let problem = lines.parse()?;
                let proof = lines.parse_next_line()?;
                Ok(ProblemProof { problem, proof })
            })?;
            problems.insert(pairing_id, problem_proof);
        }
        Ok(Self { problems })
    }

    pub(crate) fn pretty_print(&self) -> String {
        pretty_print_compat_file(self.problems.iter().map(|(pairing_id, problem_proof)| {
            let name = format!(
                "ProblemProof (Problem (Pairing ({})))",
                pairing_id.pretty_print()
            );
            let mut body = FileBuf::new();
            body.push(problem_proof.problem.pretty_print_into_block());
            body.push(
                problem_proof
                    .proof
                    .pretty_print_into_line()
                    .into_block_buf(),
            );
            (name, body)
        }))
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ProblemsFile {
    pub(crate) problems: BTreeMap<PairingId, Problem>,
}

impl ProblemsFile {
    pub(crate) fn parse_from_str(s: &str) -> Result<Self, ParseError> {
        let mut problems = BTreeMap::new();
        for (pairing_id, body) in parse_compat_file(["Problem", "Pairing"], s) {
            let problem = body.parse()?;
            problems.insert(pairing_id, problem);
        }
        Ok(Self { problems })
    }

    pub(crate) fn pretty_print(&self) -> String {
        pretty_print_compat_file(self.problems.iter().map(|(pairing_id, problem)| {
            let name = format!("Problem (Pairing ({}))", pairing_id.pretty_print());
            let body = problem.pretty_print_into_block().into_file_buf();
            (name, body)
        }))
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct InlineScriptsFile {
    pub(crate) inline_scripts: BTreeMap<PairingId, InlineScript>,
}

impl InlineScriptsFile {
    pub(crate) fn strip(&self) -> BTreeMap<PairingId, Vec<NodeBySource>> {
        BTreeMap::from_iter(self.inline_scripts.iter().map(|(id, script)| {
            (
                id.clone(),
                script
                    .commands
                    .iter()
                    .map(|command| command.node_by_source.clone())
                    .collect(),
            )
        }))
    }

    pub(crate) fn parse_from_str(s: &str) -> Result<Self, ParseError> {
        let mut inline_scripts = BTreeMap::new();
        for (pairing_id, body) in parse_compat_file(["Problem", "Pairing"], s) {
            inline_scripts.insert(pairing_id, body.parse()?);
        }
        Ok(Self { inline_scripts })
    }

    pub(crate) fn pretty_print(&self) -> String {
        pretty_print_compat_file(
            self.inline_scripts
                .iter()
                .map(|(pairing_id, inline_script)| {
                    let name = format!("Problem (Pairing ({}))", pairing_id.pretty_print());
                    let body = inline_script.pretty_print_into_block().into_file_buf();
                    (name, body)
                }),
        )
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct InlineScript {
    pub(crate) commands: Vec<InlineScriptComand>,
}

impl InlineScript {
    fn pretty_print_into_block(&self) -> BlockBuf {
        let mut block = BlockBuf::new();
        block.push_line_with(|line| line.to_tokens("InlineScript"));
        for command in &self.commands {
            block.push_line(command);
        }
        block.push_line_with(|line| line.to_tokens("EndInlineScript"));
        block
    }
}

impl ParseFromLines for InlineScript {
    fn parse(lines: &mut LinesBuffer) -> Result<Self, ParseError> {
        let mut commands = vec![];
        lines.parse_next_line_with(|toks| toks.match_("InlineScript"))?;
        loop {
            if lines.peek_token().unwrap().as_str() == "EndInlineScript" {
                lines.parse_next_line_with(|toks| toks.advance())?;
                break;
            } else {
                commands.push(lines.parse_next_line()?);
            }
        }
        Ok(Self { commands })
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct InlineScriptComand {
    pub(crate) node_by_source: NodeBySource,
    pub(crate) inlined_function: Ident,
}

impl ParseFromLine for InlineScriptComand {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            node_by_source: toks.parse()?,
            inlined_function: toks.parse()?,
        })
    }
}

impl ToTokens for InlineScriptComand {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.node_by_source);
        line.to_tokens(&self.inlined_function);
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct InlineScriptCommandDetail {
    pub(crate) function: Ident,
    pub(crate) node_addr: NodeAddr,
}

impl ParseFromLine for InlineScriptCommandDetail {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            function: toks.parse()?,
            node_addr: toks.parse_prim_int()?,
        })
    }
}

impl ToTokens for InlineScriptCommandDetail {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.function);
        line.lower_hex_to_tokens(&self.node_addr);
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct PairingsFile {
    pub(crate) pairings: BTreeMap<PairingId, Pairing>,
}

impl PairingsFile {
    pub(crate) fn parse_from_str(s: &str) -> Result<Self, ParseError> {
        let mut pairings = BTreeMap::new();
        for (pairing_id, body) in parse_compat_file(["Pairing"], s) {
            pairings.insert(pairing_id, body.parse()?);
        }
        Ok(Self { pairings })
    }

    pub(crate) fn pretty_print(&self) -> String {
        pretty_print_compat_file(self.pairings.iter().map(|(pairing_id, pairing)| {
            let name = format!("Pairing ({})", pairing_id.pretty_print());
            let body = pairing.pretty_print_into_block().into_file_buf();
            (name, body)
        }))
    }
}

// // //

fn parse_compat_file(
    nested_name: impl IntoIterator<Item = impl AsRef<str>>,
    s: &str,
) -> Vec<(PairingId, Lines)> {
    let line_re = Regex::new(r"^\s*(#.*)?$").unwrap();

    let open_re = {
        let mut re_s = String::new();
        re_s.push_str(r"^\s*");
        let mut num_parens = 0;
        for segment in nested_name.into_iter() {
            write!(re_s, r"{}\s*\(\s*", regex::escape(segment.as_ref())).unwrap();
            num_parens += 1;
        }
        re_s.push_str(r"(?<pairing_id>.*)\s*");
        for _ in 0..num_parens {
            re_s.push_str(r"\)\s*");
        }
        re_s.push_str(r"\{\s*$");
        Regex::new(&re_s).unwrap()
    };

    let close_re = Regex::new(r"^\s*\}\s*$").unwrap();

    let mut items = vec![];

    let mut lines = s
        .lines()
        .enumerate()
        .filter(|(_i, line)| !line_re.is_match(line))
        .peekable();

    while lines.peek().is_some() {
        let (_i, open) = lines.next().unwrap();
        let pairing_id_s = open_re
            .captures(open)
            .unwrap()
            .name("pairing_id")
            .unwrap()
            .as_str();
        let pairing_id = PairingId::parse_from_str(pairing_id_s);

        let mut body = vec![];
        loop {
            let (i, line) = lines.next().unwrap();
            if close_re.is_match(line) {
                break;
            } else {
                body.push((i, line));
            }
        }

        let lines = Lines::tokenize_indexed_syntax_lines(body.into_iter());
        items.push((pairing_id, lines));
    }

    items
}

fn pretty_print_compat_file(items: impl Iterator<Item = (impl fmt::Display, FileBuf)>) -> String {
    let mut buf = String::new();
    for (name, body) in items {
        write!(buf, "{name} {{\n").unwrap();
        buf.push_str(&body.pretty_print_into_string());
        buf.push_str("}\n");
    }
    buf
}

// // //

#[cfg(test)]
mod tests {
    use crate::concrete_syntax::parse::ParseFile;

    use crate::tests::utils::*;

    use super::*;

    #[test]
    fn stack_bounds_round_trips() {
        let mut paths = vec![];
        for opt_level in ["O1", "O2"] {
            let d = graph_refine_dir().join("loop-example").join(opt_level);
            paths.push(d.join("StackBounds.txt"));
        }
        paths.push(sel4_target_dir().join("StackBounds.txt"));

        for path in paths {
            test_round_trip_path(
                path,
                StackBoundsFile::parse_from_str,
                StackBoundsFile::pretty_print,
            );
        }
    }

    #[test]
    fn other_round_trips() {
        test_round_trip_path(
            sel4_target_dir().join("inline-scripts.txt"),
            InlineScriptsFile::parse_from_str,
            InlineScriptsFile::pretty_print,
        );
        test_round_trip_path(
            sel4_target_dir().join("pairings.txt"),
            PairingsFile::parse_from_str,
            PairingsFile::pretty_print,
        );
        test_round_trip_path(
            sel4_target_dir().join("problems.txt"),
            ProblemsFile::parse_from_str,
            ProblemsFile::pretty_print,
        );
        test_round_trip_path(
            sel4_target_dir().join("proofs.txt"),
            ProofsFile::parse_from_str,
            ProofsFile::pretty_print,
        );
    }
}

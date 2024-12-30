use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::Context;

use crate::abstract_syntax::{Expr, File, Function, Ident};
use crate::compat::{
    InlineScriptsFile, PairingsFile, ProblemsFile, ProofChecksFile, ProofsFile, StackBoundsFile,
};
use crate::concrete_syntax::parse::ParseFile;
use crate::inst_logic::add_asm_inst_spec;
use crate::objdump::ObjdumpInfo;
use crate::pairing::PairingId;
use crate::pairing::{Pairing, Tag};
use crate::pretty_hack::pp;
use crate::problem::{NodeBySource, Problem, ProblemBuilder, ProblemNodeMeta};

#[derive(Debug, Clone, Default)]
pub(crate) struct TargetDir {
    path: PathBuf,
    config: TargetDirConfig,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct TargetDirConfig {
    ignore_asm: Vec<Ident>,
}

impl TargetDirConfig {
    pub(crate) fn new(ignore_asm: Vec<Ident>) -> Self {
        Self { ignore_asm }
    }

    pub(crate) fn usual() -> Self {
        TargetDirConfig::new(vec![
            "fastpath_call".to_owned(),
            "fastpath_reply_recv".to_owned(),
            "c_handle_syscall".to_owned(),
            "arm_swi_syscall".to_owned(),
        ])
    }
}

impl TargetDir {
    pub(crate) fn new(path: impl AsRef<Path>, config: TargetDirConfig) -> Self {
        Self {
            path: path.as_ref().to_owned(),
            config,
        }
    }

    pub(crate) fn usual(path: impl AsRef<Path>) -> Self {
        Self::new(path, TargetDirConfig::usual())
    }

    pub(crate) fn path(&self, rel: impl AsRef<Path>) -> PathBuf {
        self.path.join(rel)
    }

    pub(crate) fn read(&self, rel: impl AsRef<Path>) -> String {
        let path = self.path(rel);
        fs::read_to_string(&path)
            .with_context(|| format!("Failed to read {}", path.display()))
            .unwrap()
    }

    pub(crate) fn read_objdump_info(&self) -> ObjdumpInfo {
        ObjdumpInfo::parse_from_str(&self.read("kernel.elf.symtab"))
    }

    pub(crate) fn read_asm_functions_file(&self) -> File {
        File::parse_from_str(&self.read("ASMFunctions.txt")).unwrap()
    }

    pub(crate) fn read_asm_functions(&self) -> BTreeMap<Ident, Function> {
        self.read_asm_functions_file().functions
    }

    pub(crate) fn read_c_functions_file(&self) -> File {
        File::parse_from_str(&self.read("CFunctions.txt")).unwrap()
    }

    pub(crate) fn read_c_functions(&self) -> BTreeMap<Ident, Function> {
        self.read_c_functions_file().functions
    }

    pub(crate) fn read_inline_scripts_file(&self) -> InlineScriptsFile {
        InlineScriptsFile::parse_from_str(&self.read("inline-scripts.txt")).unwrap()
    }

    pub(crate) fn read_inline_scripts(&self) -> BTreeMap<PairingId, Vec<NodeBySource>> {
        self.read_inline_scripts_file().strip()
    }

    pub(crate) fn read_stack_bounds_file(&self) -> StackBoundsFile {
        StackBoundsFile::parse_from_str(&self.read("StackBounds.txt")).unwrap()
    }

    pub(crate) fn read_stack_bounds(&self) -> BTreeMap<Ident, Expr> {
        self.read_stack_bounds_file().by_function
    }

    pub(crate) fn read_functions_file(&self) -> File {
        File::parse_from_str(&self.read("functions.txt")).unwrap()
    }

    pub(crate) fn read_pairings_file(&self) -> PairingsFile {
        PairingsFile::parse_from_str(&self.read("pairings.txt")).unwrap()
    }

    pub(crate) fn read_problems_file(&self) -> ProblemsFile {
        ProblemsFile::parse_from_str(&self.read("problems.txt")).unwrap()
    }

    pub(crate) fn read_proofs_file(&self) -> ProofsFile {
        ProofsFile::parse_from_str(&self.read("proofs.txt")).unwrap()
    }

    pub(crate) fn read_proof_checks_file(&self) -> ProofChecksFile {
        ProofChecksFile::parse_from_str(&self.read("proof-checks.txt")).unwrap()
    }

    fn altered_asm_functions(&self) -> BTreeMap<Ident, Function> {
        let mut functions = self.read_asm_functions();
        for ignore in &self.config.ignore_asm {
            functions.remove(ignore);
        }
        functions.values_mut().for_each(Function::fixup);
        functions
    }

    fn altered_c_functions(&self) -> BTreeMap<Ident, Function> {
        let objdump_info = self.read_objdump_info();
        let mut functions = self.read_c_functions();
        functions.values_mut().for_each(Function::fixup);
        functions
            .values_mut()
            .for_each(|f| f.pseudo_compile(&objdump_info));
        functions
    }

    pub(crate) fn build_functions_file(&self) -> File {
        let mut asm = self.altered_asm_functions();
        let mut c = self.altered_c_functions();
        add_asm_inst_spec(&mut asm, &mut c, &mut Default::default());

        let mut functions = asm;
        functions.extend(c.into_iter());
        File {
            functions,
            ..Default::default()
        }
    }

    pub(crate) fn build_pairings(&self) -> BTreeMap<PairingId, Pairing> {
        let stack_bounds = &self.read_stack_bounds();

        let mut asm = self.altered_asm_functions();
        let mut c = self.altered_c_functions();
        let mut pairings = BTreeMap::new();

        add_asm_inst_spec(&mut asm, &mut c, &mut pairings);

        for f_asm_name in asm.keys() {
            let f_c_name = format!("Kernel_C.{}", f_asm_name);
            if let Some(f_c) = c.get(&f_c_name) {
                let pairing_id = PairingId {
                    asm: f_asm_name.clone(),
                    c: f_c_name.clone(),
                };
                let min_stack_size = &stack_bounds[f_asm_name];
                let pairing = Pairing::formulate(min_stack_size, &f_c.input, &f_c.output);
                pairings.insert(pairing_id, pairing);
            }
        }

        pairings
    }

    pub(crate) fn build_pairings_file(&self) -> PairingsFile {
        let pairings = self.build_pairings();
        PairingsFile { pairings }
    }

    pub(crate) fn build_problems(&self) -> BTreeMap<PairingId, Problem<ProblemNodeMeta>> {
        let inline_scripts = &self.read_inline_scripts();
        let mut asm = self.altered_asm_functions();
        let mut c = self.altered_c_functions();
        let pairings = self.build_pairings();

        add_asm_inst_spec(&mut asm, &mut c, &mut Default::default());

        let mut problems = BTreeMap::new();

        for pairing_id in pairings.keys() {
            let f_c_name = &pairing_id.c;
            let f_asm_name = &pairing_id.asm;
            let f_c = &c[f_c_name];
            let f_asm = &asm[f_asm_name];
            if f_asm.body().is_some() && f_c.body().is_some() {
                let mut builder = ProblemBuilder::new(&f_c_name, f_c, f_asm_name, f_asm);
                if let Some(inlines) = inline_scripts.get(&pairing_id) {
                    for inline in inlines {
                        builder.inline(inline, |tag, f_name| {
                            &(match tag {
                                Tag::C => &c,
                                Tag::Asm => &asm,
                            })[f_name]
                        });
                    }
                }
                let problem = builder.build();
                problems.insert(pairing_id.clone(), problem);
            }
        }

        problems
    }

    pub(crate) fn build_problems_file(&self) -> ProblemsFile {
        ProblemsFile {
            problems: self
                .build_problems()
                .into_iter()
                .map(|(k, v)| (k, v.strip_meta()))
                .collect(),
        }
    }

    pub(crate) fn check_all(&self) {
        todo!()
    }

    pub(crate) fn pp_all(&self, d: impl AsRef<Path>) {
        if !d.as_ref().is_dir() {
            fs::create_dir_all(&d).unwrap();
        }
        let pp_one = |fname, s| fs::write(d.as_ref().join(fname), s).unwrap();
        pp_one("CFunctions.rs", pp(self.read_c_functions_file()));
        pp_one("ASMFunctions.rs", pp(self.read_asm_functions_file()));
        pp_one("pairings.rs", pp(self.read_pairings_file()));
        pp_one("problems.rs", pp(self.read_problems_file()));
        pp_one("proofs.rs", pp(self.read_proofs_file()));
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::utils::*;

    use super::*;

    fn t() -> TargetDir {
        TargetDir::usual(sel4_target_dir())
    }

    #[test]
    fn functions() {
        let t = t();
        let theirs = t.read_functions_file();
        let ours = t.build_functions_file();
        theirs.typecheck().unwrap();
        ours.typecheck().unwrap();
        eq_or_dump(
            "t/functions.txt",
            "txt",
            theirs.pretty_print(),
            ours.pretty_print(),
        );
    }

    #[test]
    fn pairings() {
        let t = t();
        eq_or_dump(
            "t/pairings.txt",
            "txt",
            t.read_pairings_file().pretty_print(),
            t.build_pairings_file().pretty_print(),
        );
    }

    #[test]
    fn problems() {
        let t = t();
        let theirs = t.read_problems_file();
        let mut ours = t.build_problems_file();
        theirs.typecheck().unwrap();
        ours.typecheck().unwrap();
        // graph-refine doesn't output aborted problems
        ours.problems.retain(|k, _| theirs.problems.contains_key(k));
        eq_or_dump(
            "t/problems.txt",
            "txt",
            theirs.pretty_print(),
            ours.pretty_print(),
        );
    }
}

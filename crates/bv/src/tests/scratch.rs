use std::fs;
use std::path::Path;

fn ensure_dir_and_write(path: impl AsRef<Path>, contents: impl AsRef<[u8]>) {
    let d = path.as_ref().parent().unwrap();
    if !d.is_dir() {
        fs::create_dir_all(d).unwrap();
    }
    fs::write(path, contents).unwrap();
}

mod dot {
    use crate::tests::utils::*;

    use petgraph::dot;

    use crate::graph::NodeGraph;
    use crate::sel4::TargetDir;
    use crate::utils::petgraph::NodeGraphFormatterWrapper;

    #[test]
    #[ignore]
    fn dot() {
        let f_name = "copyMRs";
        let t = TargetDir::usual(sel4_target_dir());
        let fs = t.read_asm_functions();
        let f = &fs[f_name];
        let g = NodeGraph::new(NodeGraphFormatterWrapper::new(f.body().unwrap()));
        let d = dot::Dot::with_config(&g, &[]);
        eprintln!("{}", d);
        write("f.dot", format!("{}", d));
    }
}

mod pp {
    use std::ops::RangeInclusive;
    use std::path::{Path, PathBuf};

    use crate::pretty_hack::pp;
    use crate::sel4::TargetDir;
    use crate::tests::utils::*;

    use super::ensure_dir_and_write;

    fn target_dir_for(i: usize) -> TargetDir {
        TargetDir::usual(
            super_project_root_dir()
                .join("tmp/rscratch/t")
                .join(format!("{:?}", i)),
        )
    }

    fn out_dir_for(rel: impl AsRef<Path>, i: usize) -> PathBuf {
        super_project_root_dir()
            .join("tmp/rscratch/out")
            .join(rel)
            .join(format!("{:?}", i))
    }

    fn both() -> RangeInclusive<usize> {
        1..=2
    }

    fn pp_all(i: usize) {
        target_dir_for(i).pp_all(out_dir_for("pp-all", i));
    }

    #[test]
    #[ignore]
    fn pp_all_1() {
        pp_all(1);
    }

    #[test]
    #[ignore]
    fn pp_all_2() {
        pp_all(2);
    }

    #[test]
    #[ignore]
    fn pp_all_both() {
        pp_all(1);
        pp_all(2);
    }

    fn pp_some<I: IntoIterator<Item = usize>>(
        out_dir: impl AsRef<Path>,
        ols: impl Fn() -> I,
        filter: impl Fn(&str) -> bool,
    ) {
        for ol in (ols)() {
            let t = target_dir_for(ol);
            let out = out_dir_for(&out_dir, ol);

            {
                let mut file = t.read_asm_functions_file();
                file.functions.retain(|k, _| filter(k));
                let s = pp(&file);
                ensure_dir_and_write(out.join("ASMFunctions.rs"), s);
            }
            {
                let mut file = t.read_c_functions_file();
                file.functions.retain(|k, _| {
                    k.strip_prefix("Kernel_C.")
                        .map(|k_| filter(k_))
                        .unwrap_or(false)
                });
                let s = pp(&file);
                ensure_dir_and_write(out.join("CFunctions.rs"), s);
            }
            {
                let mut file = t.read_pairings_file();
                file.pairings.retain(|k, _| filter(&k.asm));
                let s = pp(&file);
                ensure_dir_and_write(out.join("pairings.rs"), s);
            }
            {
                let mut file = t.read_problems_file();
                file.problems.retain(|k, _| filter(&k.asm));
                let s = pp(&file);
                ensure_dir_and_write(out.join("problems.rs"), s);
            }
        }
    }

    #[test]
    #[ignore]
    fn x() {
        pp_some("lct", both, |k| {
            true || [
                "loadCapTransfer",
                "lookupSourceSlot",
                "Arch_maskCapRights",
                "setupCallerCap",
            ]
            .contains(&k)
        });
    }
}

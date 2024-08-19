use std::env;
use std::ffi::OsStr;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::Context;

#[ctor::ctor]
fn init_logging() {
    let _ = env_logger::builder().is_test(true).try_init();
}

pub(crate) fn project_root_dir() -> PathBuf {
    Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap()).join("../..")
}

pub(crate) fn super_project_root_dir() -> PathBuf {
    // project_root_dir().join("../..")
    project_root_dir().join("../seL4-verification-reproducibility")
}

pub(crate) fn tmp_dir() -> PathBuf {
    project_root_dir().join("tmp")
}

pub(crate) fn graph_refine_dir() -> PathBuf {
    super_project_root_dir().join("projects/graph-refine")
}

pub(crate) fn sel4_target_dir() -> PathBuf {
    env::var("SEL4_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| tmp_dir().join("target"))
}

pub(crate) fn read_sel4_target_dir_file(rel_path: impl AsRef<Path>) -> String {
    let path = sel4_target_dir().join(rel_path);
    fs::read_to_string(&path)
        .with_context(|| format!("Failed to read {}", path.display()))
        .unwrap()
}

pub(crate) fn test_output_dir() -> PathBuf {
    tmp_dir().join("test-output")
}

pub(crate) fn create_test_output_dir() {
    let path = test_output_dir();
    if !path.is_dir() {
        fs::create_dir_all(path).unwrap();
    }
}

pub(crate) fn test_round_trip<T: Eq + fmt::Debug, E: fmt::Debug>(
    orig: &T,
    parse: impl Fn(&str) -> Result<T, E>,
    pretty_print: impl Fn(&T) -> String,
) {
    let orig_pp = pretty_print(&orig);
    let parsed = parse(&orig_pp).unwrap();
    let parsed_pp = pretty_print(&parsed);
    assert_eq!(orig, &parsed, "");
    assert_eq!(orig_pp, parsed_pp, "");
}

pub(crate) fn test_round_trip_content<T: Eq + fmt::Debug, E: fmt::Debug>(
    s: &str,
    parse: impl Fn(&str) -> Result<T, E>,
    pretty_print: impl Fn(&T) -> String,
) {
    let orig = parse(s).unwrap();
    test_round_trip(&orig, parse, pretty_print);
}

pub(crate) fn test_round_trip_path<T: Eq + fmt::Debug, E: fmt::Debug>(
    path: impl AsRef<Path>,
    parse: impl Fn(&str) -> Result<T, E>,
    pretty_print: impl Fn(&T) -> String,
) {
    log::trace!("testing round trip: {}", path.as_ref().display());
    let s = fs::read_to_string(path).unwrap();
    test_round_trip_content(&s, parse, pretty_print);
}

pub(crate) fn write(path: impl AsRef<Path>, contents: impl AsRef<[u8]>) {
    let abs = test_output_dir().join(path);
    let d = abs.parent().unwrap();
    if !d.is_dir() {
        fs::create_dir_all(d).unwrap();
    }
    fs::write(abs, contents).unwrap();
}

pub(crate) fn eq_or_dump<T: AsRef<[u8]> + Eq>(
    rel: impl AsRef<Path>,
    extension: impl AsRef<OsStr>,
    lhs: T,
    rhs: T,
) {
    if lhs != rhs {
        write(rel.as_ref().join("lhs").with_extension(&extension), lhs);
        write(rel.as_ref().join("rhs").with_extension(&extension), rhs);
        panic!("lhs != rhs. written to {}", rel.as_ref().display());
    }
}

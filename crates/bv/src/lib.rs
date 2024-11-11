#![allow(dead_code)]
#![deny(clippy::arithmetic_side_effects)]

mod arch;
mod compat;
mod fixup;
mod graph;
mod inst_logic;
mod logic;
mod objdump;
mod pairing;
mod parse;
mod pretty_hack;
mod pretty_print;
mod problem;
mod proof_script;
mod pseudo_compile;
mod sel4;
mod abstract_syntax;
mod utils;

#[cfg(test)]
mod tests;

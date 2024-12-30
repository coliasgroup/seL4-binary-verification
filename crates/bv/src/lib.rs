#![allow(dead_code)]
#![deny(clippy::arithmetic_side_effects)]

mod abstract_syntax;
mod arch;
mod check;
mod compat;
mod concrete_syntax;
mod debugger;
mod expr_constructors;
mod fixup;
mod inst_logic;
mod logic;
mod objdump;
mod pairing;
mod pretty_hack;
mod problem;
mod proof_script;
mod pseudo_compile;
mod sel4;
mod typecheck;
mod utils;

#[cfg(test)]
mod tests;

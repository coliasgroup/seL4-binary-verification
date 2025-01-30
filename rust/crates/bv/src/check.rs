use std::collections::BTreeMap;

use crate::{
    pairing::{Pairing, PairingId},
    problem::Problem,
    proof_script::ProofNode,
};

mod checks;

pub(crate) use checks::*;

pub(crate) fn check_proof(
    pairing_id: &PairingId,
    pairings: &BTreeMap<PairingId, Pairing>,
    problem: &Problem,
    proof: &ProofNode,
) -> SolverSessions {
    Default::default()
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Default)]
pub(crate) struct SolverSessions {
    pub(crate) sessions: Vec<SolverSession>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Default)]
pub(crate) struct SolverSession {
    pub(crate) commands: Vec<SolverSessionCommand>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct SolverSessionCommand {
    pub(crate) input: String,
    pub(crate) output: SolverSessionCommandOutput,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum SolverSessionCommandOutput {
    Success,
    Unsat,
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
        for (pairing_id, their_problem_proof) in &their_proofs.problems {
            let their_problem = &their_problem_proof.problem;
            let their_proof = &their_problem_proof.proof;
            let proof = their_proof;
            let problem = problems[pairing_id].clone().strip_meta();
            println!("{pairing_id:?}");
            assert_eq!(&problem, their_problem);
            println!("{proof:?}");
            println!("checking...");
            check_proof(pairing_id, &pairings, &problem, proof);
            println!("done");
        }
    }
}

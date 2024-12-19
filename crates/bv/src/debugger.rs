use std::collections::BTreeMap;

use crate::abstract_syntax::{Expr, Ident, NodeId};

struct State {
    pos: NodeId,
    vars: VarState,
}

struct VarState {
    values: BTreeMap<Ident, Expr>,
}

enum DebuggerError {
    VariableUndefined {
        ident: Ident,
    }
}

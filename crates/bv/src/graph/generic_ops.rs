use std::collections::{btree_map::Entry, BTreeMap};

use crate::{
    abstract_syntax::{Argument, Expr, ExprValue, File, Ident, Node, OpTypeError, Type},
    compat::ProblemsFile,
    pairing::{PairingId, Tag},
    problem::Problem,
};

use super::{HasNodeGraph, NodeGraph};

impl File {
    pub(crate) fn typecheck(&self) -> Result<(), FileTypeError> {
        for (f_name, f) in &self.functions {
            f.body()
                .unwrap()
                .node_graph()
                .typecheck(f.input(), f.output())
                .map_err(|err| FileTypeError {
                    function: f_name.clone(),
                    error: err,
                })?;
        }
        Ok(())
    }
}

impl ProblemsFile {
    pub(crate) fn typecheck(&self) -> Result<(), ProblemsFileTypeError> {
        for (pairing, problem) in &self.problems {
            problem.typecheck().map_err(|err| ProblemsFileTypeError {
                pairing: pairing.clone(),
                error: err,
            })?;
        }
        Ok(())
    }
}

impl Problem {
    pub(crate) fn typecheck(&self) -> Result<(), ProblemTypeError> {
        self.nodes
            .node_graph()
            .typecheck(&self.c.input, &self.c.output)
            .map_err(|err| ProblemTypeError {
                tag: Tag::C,
                error: err,
            })?;
        self.nodes
            .node_graph()
            .typecheck(&self.asm.input, &self.asm.output)
            .map_err(|err| ProblemTypeError {
                tag: Tag::Asm,
                error: err,
            })?;
        Ok(())
    }
}

impl<T: HasNodeGraph> NodeGraph<T> {
    pub(crate) fn typecheck(
        &self,
        input: &[Argument],
        output: &[Argument],
    ) -> Result<(), FunctionTypeError> {
        for (_, node) in self.graph_nodes() {
            node.try_visit_exprs(&mut |expr| expr.typecheck())
                .map_err(FunctionTypeError::OpTypeError)?;
        }
        let mut var_types = VariableTypes::new();
        var_types.admit_args(input)?;
        var_types.admit_args(output)?;
        for (_, node) in self.graph_nodes() {
            var_types.admit_node(node)?;
        }
        Ok(())
    }
}

struct VariableTypes<'a> {
    map: BTreeMap<&'a Ident, &'a Type>,
}

impl<'a> VariableTypes<'a> {
    fn new() -> Self {
        Self {
            map: BTreeMap::new(),
        }
    }

    fn admit(&mut self, ident: &'a Ident, ty: &'a Type) -> Result<(), FunctionTypeError> {
        match self.map.entry(ident) {
            Entry::Occupied(entry) => {
                if &ty != entry.get() {
                    return Err(FunctionTypeError::InconsistentVariableType(ident.clone()));
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(ty);
            }
        }
        Ok(())
    }

    fn admit_arg(&mut self, arg: &'a Argument) -> Result<(), FunctionTypeError> {
        self.admit(&arg.name, &arg.ty)
    }

    fn admit_args(&mut self, args: &'a [Argument]) -> Result<(), FunctionTypeError> {
        for arg in args {
            self.admit_arg(arg)?;
        }
        Ok(())
    }

    fn admit_node(&mut self, node: &'a Node) -> Result<(), FunctionTypeError> {
        node.try_visit_exprs(&mut |expr| self.admit_expr(expr))
    }

    fn admit_expr(&mut self, expr: &'a Expr) -> Result<(), FunctionTypeError> {
        if let ExprValue::Var(ident) = &expr.value {
            self.admit(ident, &expr.ty)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum FunctionTypeError {
    OpTypeError(OpTypeError),
    InconsistentVariableType(Ident),
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct FileTypeError {
    function: Ident,
    error: FunctionTypeError,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ProblemsFileTypeError {
    pairing: PairingId,
    error: ProblemTypeError,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ProblemTypeError {
    tag: Tag,
    error: FunctionTypeError,
}

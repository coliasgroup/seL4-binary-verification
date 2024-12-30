use std::collections::{btree_map::Entry, BTreeMap};

use crate::{
    abstract_syntax::{Argument, File, Ident, Node}, compat::ProblemsFile, expr::{typecheck::OpTypeError, Expr, ExprValue, Type}, pairing::{PairingId, Tag}, problem::Problem
};

use super::{HasFunctionSignature, HasNodeGraph, MightHaveNodeGraphWithEntry};

// TODO
// - typecheck symbols file-wide
// - typecheck inputs and outputs in call nodes

impl File {
    pub(crate) fn typecheck(&self) -> Result<(), FileTypeError> {
        for (f_name, f) in &self.functions {
            typecheck_function(f).map_err(|err| FileTypeError {
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
        typecheck_function(&self.at_c()).map_err(|err| ProblemTypeError {
            tag: Tag::C,
            error: err,
        })?;
        typecheck_function(&self.at_asm()).map_err(|err| ProblemTypeError {
            tag: Tag::Asm,
            error: err,
        })?;
        Ok(())
    }
}

fn typecheck_function<T: HasFunctionSignature + MightHaveNodeGraphWithEntry>(
    f: &T,
) -> Result<(), FunctionTypeError> {
    if let Some(graph) = f.node_graph_option() {
        for (_, node) in graph.graph_nodes() {
            node.try_visit_exprs(&mut |expr| expr.typecheck())
                .map_err(FunctionTypeError::OpTypeError)?;
            if let Node::Cond(node) = node {
                if !matches!(&node.expr.ty, Type::Bool) {
                    return Err(FunctionTypeError::CondExprNotBool);
                }
            }
        }
    }
    let mut var_types = VariableTypes::new();
    var_types.admit_args(f.graph_input())?;
    var_types.admit_args(f.graph_output())?;
    if let Some(graph) = f.node_graph_option() {
        for (_, node) in graph.graph_nodes() {
            var_types.admit_node(node)?;
        }
    }
    Ok(())
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
        node.try_visit_exprs(&mut |expr| self.admit_expr(expr))?;
        node.try_visit_var_decls(&mut |ident, ty| self.admit(ident, ty))?;
        Ok(())
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
    CondExprNotBool,
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

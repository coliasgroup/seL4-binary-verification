use std::collections::{btree_map::Entry, BTreeMap};

use crate::abstract_syntax::{Argument, Ident, OpTypeError, Type};

use super::{HasNodeGraph, NodeGraph};

impl<T: HasNodeGraph> NodeGraph<T> {
    pub(crate) fn typecheck(
        &self,
        input: &[Argument],
        output: &[Argument],
    ) -> Result<(), FunctionTypeError> {
        let mut var_types = VariableTypes::new();
        var_types.admit_args(input)?;
        var_types.admit_args(output)?;
        for (_, node) in self.graph_nodes() {
            node.try_visit_exprs(|expr| {

            })
        }
        Ok(())
    }
}

struct VariableTypes<'a> {
    map: BTreeMap<&'a Ident, &'a Type>
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

    fn admit_expr(&mut self, node: &'a Node) -> Result<(), FunctionTypeError> {
        Ok(())
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum FunctionTypeError {
    OpTypeError(OpTypeError),
    InconsistentVariableType(Ident),
}

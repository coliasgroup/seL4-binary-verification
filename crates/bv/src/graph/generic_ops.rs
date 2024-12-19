use crate::abstract_syntax::{Argument, OpTypeError};

use super::{HasNodeGraph, NodeGraph};

impl<T: HasNodeGraph> NodeGraph<T> {
    pub(crate) fn typecheck(
        &self,
        input: &[Argument],
        output: &[Argument],
    ) -> Result<(), FunctionTypeError> {
        todo!()
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) enum FunctionTypeError {
    OpTypeError(OpTypeError),
}

use crate::abstract_syntax::{Argument, Type};
use crate::arch::PTR_SIZE_BYTES;

impl Type {
    pub(crate) fn size(&self) -> u64 {
        match self {
            Self::Array(ty, len) => len * ty.size(),
            Self::Word(bits) => {
                assert_eq!(bits % 8, 0);
                bits / 8
            }
            Self::Ptr(_) => PTR_SIZE_BYTES,
            _ => {
                panic!()
            }
        }
    }

    pub(crate) fn align(&self) -> u64 {
        match self {
            Self::Array(ty, _) => ty.align(),
            Self::Word(_) | Self::Ptr(_) => self.size(),
            _ => {
                panic!()
            }
        }
    }
}

pub(crate) fn split_scalar_pairs(
    args: &[Argument],
) -> (Vec<Argument>, Vec<Argument>, Vec<Argument>) {
    let i = args
        .iter()
        .take_while(|arg| matches!(&arg.ty, Type::Word(_) | Type::Bool))
        .count();
    let (scalars, globals) = args.split_at(i);
    let mems = globals
        .iter()
        .filter(|arg| matches!(&arg.ty, Type::Mem))
        .cloned()
        .collect::<Vec<_>>();
    let others = globals
        .iter()
        .filter(|arg| !matches!(&arg.ty, Type::Mem))
        .cloned()
        .collect::<Vec<_>>();
    (scalars.to_vec(), mems, others)
}

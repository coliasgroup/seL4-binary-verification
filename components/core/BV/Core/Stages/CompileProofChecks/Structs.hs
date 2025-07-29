
module BV.Core.Stages.CompileProofChecks.Structs
    ( initStructsEnv
    ) where

import BV.Core.Types

import Data.Foldable (toList)
import qualified Data.Map as M

initStructsEnv :: Tag t => ROData -> ByTag t (M.Map Ident Struct) -> (Ident -> Struct)
initStructsEnv rodata structs = (M.!) $ M.unionsWith (error "unexpected") $
    rodataStructsOf rodata : toList structs

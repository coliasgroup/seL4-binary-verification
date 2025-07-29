
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Stages.CompileProofChecks.Structs
    ( initStructsEnv
    ) where

import BV.Core.Types

import Data.Map (Map)
import qualified Data.Map as M

initStructsEnv :: ROData -> Map Ident Struct -> (Ident -> Struct)
initStructsEnv rodata cStructs = (M.!) $ augmentStructs rodata cStructs

augmentStructs :: ROData -> Map Ident Struct -> Map Ident Struct
augmentStructs rodata cStructs = cStructs <> rodataStructsOf rodata

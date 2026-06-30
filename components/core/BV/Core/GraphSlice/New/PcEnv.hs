{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.GraphSlice.New.PcEnv
    ( ExprEnv
    , PcEnv (..)
    , exprEnvVars
    , mergePcEnvs
    ) where

import BV.Core.GraphSlice.New.SendFlatExprCommand (FlatExpr)

import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils (foldAssocBalanced)

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

type ExprEnv = Map Ident FlatExpr

exprEnvVars :: ExprEnv -> S.Set NameTy
exprEnvVars = S.fromList . M.elems . M.mapWithKey (\k v -> NameTy k v.ty)

data PcEnv
  = PcEnv
      { pc :: FlatExpr
      , env :: ExprEnv
      }
  deriving (Eq, Generic, Ord, Show)

mergePcEnvs :: [PcEnv] -> PcEnv
mergePcEnvs unfilteredPcEnvs = PcEnv pc env
  where
    pcEnvs = filter (\pcEnv -> pcEnv.pc /= falseE) unfilteredPcEnvs
    pc = case pcEnvs of
            [] -> falseE
            _ -> foldAssocBalanced orE (nub (pcEnvs ^.. folded % #pc))
    env = mergeEnvs pcEnvs

mergeEnvs :: [PcEnv] -> ExprEnv
mergeEnvs envs = fmap (mergeValPcList . M.toList) varValPcMap
  where
    varValPcList = concat $ flip map envs $ \(PcEnv pc env) ->
        [ (var, val, pc)
        | (var, val) <- M.toList env
        ]
    varValPcMap = foldr (M.unionWith (M.unionWith (<>))) M.empty $
        [ M.singleton var (M.singleton val [pc'])
        | (var, val, pc') <- varValPcList
        ]
    mergeValPcList valsByPc =
        let Just (valsByPcInit, (lastVal, _)) = unsnoc valsByPc
            f accVal (val, pcs) = ifThenElseE (foldr1 orE pcs) val accVal
         in foldl f lastVal valsByPcInit

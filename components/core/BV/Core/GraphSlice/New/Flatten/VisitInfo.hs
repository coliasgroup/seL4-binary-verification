{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.GraphSlice.New.Flatten.VisitInfo
    ( ExprEnv
    , PcEnv (..)
    , VisitInfo (..)
    , exprEnvVars
    , mergeVisitInfo
    , visitInfoPcEnv
    ) where

import BV.Core.GraphSlice.New.Flatten.CallCount
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

data VisitInfo
  = VisitInfo
      { pc :: FlatExpr
      , env :: ExprEnv
      , callCount :: CallCount
      }
  deriving (Eq, Generic, Ord, Show)

visitInfoPcEnv :: VisitInfo -> PcEnv
visitInfoPcEnv info = PcEnv
    { pc = info.pc
    , env = info.env
    }

mergeVisitInfo :: [VisitInfo] -> VisitInfo
mergeVisitInfo unfiltered = VisitInfo
    { pc = case infos of
            [] -> falseE
            _ -> foldAssocBalanced orE (nub (map (.pc) infos))
    , env = mergeEnvs infos
    , callCount = foldl mergeCallCounts emptyCallCount (map (.callCount) infos)
    }
  where
    infos = filter (\pcEnv -> pcEnv.pc /= falseE) unfiltered

mergeEnvs :: [VisitInfo] -> ExprEnv
mergeEnvs envs = mergeValPcList . M.toList <$> varValPcMap
  where
    varValPcList = flip concatMap envs $ \info ->
        [ (var, val, info.pc)
        | (var, val) <- M.toList info.env
        ]
    varValPcMap = foldr (M.unionWith (M.unionWith (<>))) M.empty $
        [ M.singleton var (M.singleton val [pc])
        | (var, val, pc) <- varValPcList
        ]
    mergeValPcList valsByPc =
        let Just (valsByPcInit, (lastVal, _)) = unsnoc valsByPc
            f accVal (val, pcs) = ifThenElseE (foldr1 orE pcs) val accVal
         in foldl f lastVal valsByPcInit

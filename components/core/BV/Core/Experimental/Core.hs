{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Experimental.Core
    ( PcEnv (..)
    , addSplitMemVar
    , mergeEnvsPcs
    ) where

import BV.Core.Experimental.Flatten
import BV.Core.Experimental.Types

import BV.Core.Types hiding (SplitMem (..))
import BV.Core.Types.Extras

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.List (nub)
import qualified Data.Map as M
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics

--

addSplitMemVar :: MonadRepGraphFlatten m => SolverExpr -> NameHint -> m SolverExpr
addSplitMemVar split nameHint = do
    top <- varFromNameTyE <$> addVar (nameHint ++ "_top") memT
    bottom <- varFromNameTyE <$> addVar (nameHint ++ "_bot") memT
    return $ splitMemE split top bottom

--

data PcEnv
  = PcEnv
      { pc :: SolverExpr
      , env :: ExprEnv
      }
  deriving (Eq, Generic, NFData, Ord, Show)

mergeEnvsPcs :: MonadRepGraphFlatten m => [PcEnv] -> m (PcEnv, Bool)
mergeEnvsPcs unfilteredPcEnvs = do
    let pcEnvs = filter (\pcEnv -> pcEnv.pc /= falseE) unfilteredPcEnvs
    let pc = case pcEnvs of
            [] -> falseE
            _ -> foldAssocBalanced orE (nub (pcEnvs ^.. folded % #pc))
    env <- mergeEnvs pcEnvs
    return (PcEnv pc env, length pcEnvs > 1)

foldAssocBalanced :: (a -> a -> a) -> [a] -> a
foldAssocBalanced f = go
  where
    go xs =
        let n = length xs
         in if n >= 4
            then
                let (lhs, rhs) = splitAt (n `div` 2) xs
                 in f (go lhs) (go rhs)
            else
                foldr1 f xs

mergeEnvs :: MonadRepGraphFlatten m => [PcEnv] -> m ExprEnv
mergeEnvs envs = do
    varValPcList <- fmap concat $ for envs $ \(PcEnv pc env) -> do
        return
            [ (var, val, pc)
            | (var, val) <- M.toList env
            ]
    let varValPcMap = foldr (M.unionWith (M.unionWith (<>))) M.empty $
            [ M.singleton var (M.singleton val [pc'])
            | (var, val, pc') <- varValPcList
            ]
    traverse (mergeValPcList . M.toList) varValPcMap
  where
    mergeValPcList valsByPc =
        let Just (valsByPcInit, (lastVal, _)) = unsnoc valsByPc
            f accVal (val, pcs) = flattenOpExpr boolT OpIfThenElse [foldr1 orE pcs, val, accVal]
         in foldM f lastVal valsByPcInit

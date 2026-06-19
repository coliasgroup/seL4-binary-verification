{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Search.Core.Solver
    ( module BV.Search.Core.Solver.Common
    , getFlatExprValue
    , testHyp
    , testHypWhyps
    , testHypWhypsWithCache
    , testHypWhypsWithCacheFast
    , withoutSendSExpr
    ) where

import BV.Search.Core.GraphSlice
import BV.Search.Core.Solver.Common

import BV.Core.Types
import BV.Core.Types.Extras.SExprToExpr (sexprToExpr)
import BV.Core.Types.Extras.SExprWithPlaceholders (showSExprWithPlaceholders)
import BV.Utils

import Control.Monad.State (StateT (..), modify)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (Writer, runWriter, tell)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Optics

-- TODO
-- Use an abstraction that allows for caching sat results
-- Add param to run*Solver* that determines whether ":produce-models" is sent

--

newtype Cache
  = Cache { unwrap :: M.Map FlatExpr Bool }
  deriving (Generic)

emptyCache :: Cache
emptyCache = Cache M.empty

withCache :: Monad m => StateT (Maybe Cache) m a -> StateT Cache m a
withCache (StateT f) = StateT $ \cache -> over _2 fromJust <$> f (Just cache)

withoutCache :: Monad m => StateT (Maybe Cache) m a -> m a
withoutCache (StateT f) = fst <$> f Nothing

--

testHyp :: (Tag t, MonadGraphSliceSolverInteract m) => FlatExpr -> GraphSliceT t m Bool
testHyp expr = do
    sexpr <- convertExpr expr
    addAccumulatedAssertions
    lift $ checkSExprHyp sexpr

testHypWhypsCommon
    :: (Tag t, MonadGraphSliceSolverInteract m)
    => Bool -> FlatExpr -> [Hyp t] -> StateT (Maybe Cache) (GraphSliceT t m) (Maybe Bool)
testHypWhypsCommon fast hyp hyps = do
    expr <- lift $ interpretHypImps hyps hyp
    cacheEntry <- preuse $ #_Just % #unwrap % at expr % #_Just
    case (cacheEntry, fast) of
        (Just v, _) -> do
            return $ Just $ v
        (Nothing, True) -> do
            return Nothing
        (Nothing, False) -> do
            v <- lift $ testHyp expr
            zoomMaybe (#_Just % #unwrap) $ modify $ M.insertWith undefined expr v
            return $ Just v

testHypWhyps
    :: (Tag t, MonadGraphSliceSolverInteract m)
    => FlatExpr -> [Hyp t] -> GraphSliceT t m Bool
testHypWhyps hyp hyps =
    fromJust <$> withoutCache (testHypWhypsCommon False hyp hyps)

testHypWhypsWithCache
    :: (Tag t, MonadGraphSliceSolverInteract m)
    => FlatExpr -> [Hyp t] -> StateT Cache (GraphSliceT t m) Bool
testHypWhypsWithCache hyp hyps =
    fromJust <$> withCache (testHypWhypsCommon False hyp hyps)

testHypWhypsWithCacheFast
    :: (Tag t, MonadGraphSliceSolverInteract m)
    => FlatExpr -> [Hyp t] -> StateT Cache (GraphSliceT t m) (Maybe Bool)
testHypWhypsWithCacheFast hyp hyps =
    withCache (testHypWhypsCommon True hyp hyps)

getFlatExprValue :: (Tag t, MonadGraphSliceGetSExprValue m) => FlatExpr -> GraphSliceT t m (Expr c)
getFlatExprValue expr = do
    sexpr <- withoutSendSExpr $ convertExpr expr
    valueSExpr <- lift $ getSExprValue sexpr
    let valueExpr = sexprToExpr valueSExpr
    ensureM $ valueExpr.ty == expr.ty
    return valueExpr

--

newtype DontSendSExpr a
  = DontSendSExpr { run :: Writer [SMTProofCheckCommand] a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadGraphSliceSendSExpr DontSendSExpr where
    sendCommand s = DontSendSExpr $ tell [s]

mapDontSendSExpr :: Monad m => DontSendSExpr a -> m a
mapDontSendSExpr m = case runWriter m.run of
    (a, []) -> return a
    (_, ss) -> error $
        "withoutSendSExpr:\n"
            ++ concat [ showSExprWithPlaceholders (commandToSExpr s) ++ "\n" | s <- ss ]

withoutSendSExpr :: Monad m => GraphSliceT t DontSendSExpr a -> GraphSliceT t m a
withoutSendSExpr = mapGraphSliceT mapDontSendSExpr

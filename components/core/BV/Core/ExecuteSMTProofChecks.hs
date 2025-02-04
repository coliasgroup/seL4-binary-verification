{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module BV.Core.ExecuteSMTProofChecks
    ( OnlineSolverAbortReason (..)
    , SolverConfig (..)
    , SolverMemoryMode (..)
    , executeSMTProofCheckGroupOffline
    , executeSMTProofCheckGroupOnline
    , executeSMTProofCheckOffline
    ) where

import BV.Core.ConfigureSMT
import BV.Core.Types
import BV.Core.Types.Extras
import BV.SMTLIB2
import BV.SMTLIB2.Command

import Control.Monad (forM_)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Writer (runWriterT)
import Control.Monad.Writer (tell)
import Data.Function (applyWhen)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((<<%=))

logic :: String
logic = "QF_AUFBV"

executeSMTProofCheckOffline
    :: MonadSolver m
    => SolverConfig -> Maybe SolverTimeout -> SMTProofCheck a -> m (Maybe SatResult)
executeSMTProofCheckOffline config timeout check = undefined

executeSMTProofCheckGroupOffline
    :: MonadSolver m
    => SolverConfig -> Maybe SolverTimeout -> SMTProofCheckGroup a -> m (Maybe SatResult)
executeSMTProofCheckGroupOffline config timeout check = undefined

data OnlineSolverAbortReason
  = OnlineSolverAbortReasonTimeout
  | OnlineSolverAbortReasonAnsweredSat
  | OnlineSolverAbortReasonAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

executeSMTProofCheckGroupOnline
    :: (MonadSolver m, MonadThrow m)
    => SolverConfig
    -> Maybe SolverTimeout
    -> SMTProofCheckGroup a
    -> m (Either (a, OnlineSolverAbortReason) (), [a])
executeSMTProofCheckGroupOnline config timeout group = do
    sendSimpleCommandExpectingSuccess $ SetOption (PrintSuccessOption True)
    sendSimpleCommandExpectingSuccess $ SetLogic logic
    mapM_ sendExpectingSuccess (smtConfigPreamble config)
    mapM_ (sendExpectingSuccess . configureSExpr config) group.setup
    (abortInfo, completed) <-
        flip evalStateT 1 {- matches graph-refine -} .
        runWriterT .
        runExceptT .
            forM_ group.imps $ \check -> do
                let meta = check.meta
                let hyps = splitHyp (notS check.term)
                sendSimpleCommandExpectingSuccess $ Push 1
                forM_ hyps $ \hyp -> do
                    labeledHyp <- labelHyp hyp
                    sendAssert labeledHyp
                result <- checkSatWithTimeout timeout >>=
                    maybe (throwError (meta, OnlineSolverAbortReasonTimeout)) return
                case result of
                    Sat -> throwError (meta, OnlineSolverAbortReasonAnsweredSat)
                    Unknown -> throwError (meta, OnlineSolverAbortReasonAnsweredUnknown)
                    Unsat -> return ()
                tell [meta]
                sendSimpleCommandExpectingSuccess $ Pop 1
                sendAssert $ notS (andNS hyps)
    return (abortInfo, completed)
  where
    sendAssert = sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr config

labelHyp :: MonadState Integer m => SExprWithPlaceholders -> m SExprWithPlaceholders
labelHyp sexpr = do
    i <- simple <<%= (+ 1)
    let label = "hyp" ++ show i
    return $ labelS label sexpr

splitHyp :: SExprWithPlaceholders -> [SExprWithPlaceholders]
splitHyp = fromJust . traverse checkSExprWithPlaceholders . go . viewSExprWithPlaceholders
  where
    notU x = ["not", x]
    isBool = (`elem` (["true", "false"] :: [_]))
    go hyp = case hyp of
        List (Atom (AtomOrPlaceholderAtom (SymbolAtom "and")) : args) ->
            concatMap go args
        List [ Atom (AtomOrPlaceholderAtom (SymbolAtom "not"))
             , List [ Atom (AtomOrPlaceholderAtom (SymbolAtom "=>"))
                    , p
                    , q
                    ]
             ] ->
            go p ++ go (notU q)
        List [ Atom (AtomOrPlaceholderAtom (SymbolAtom "not"))
             , List [ Atom (AtomOrPlaceholderAtom (SymbolAtom "or"))
                    , p
                    , q
                    ]
             ] ->
            go (notU p) ++ go (notU q)
        List [ Atom (AtomOrPlaceholderAtom (SymbolAtom "not"))
             , List [ Atom (AtomOrPlaceholderAtom (SymbolAtom "not"))
                    , p
                    ]
             ] ->
            go (notU p)
        List [ Atom (AtomOrPlaceholderAtom (SymbolAtom "=>"))
             , p
             , Atom (AtomOrPlaceholderAtom (SymbolAtom "false"))
             ] ->
            go (notU p)
        List (Atom (AtomOrPlaceholderAtom (SymbolAtom "=")) : args@[p, q])
            | any isBool args ->
                let (bool, term) = applyWhen (isBool q) swap (p, q)
                 in go (applyWhen (bool == "false") notU term)
        _ -> [hyp]

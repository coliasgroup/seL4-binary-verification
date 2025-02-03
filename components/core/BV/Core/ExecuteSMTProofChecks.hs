{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module BV.Core.ExecuteSMTProofChecks
    ( SolverConfig (..)
    , SolverMemoryMode (..)
    , executeSMTProofCheckGroupOffline
    , executeSMTProofCheckGroupOnline
    , executeSMTProofCheckOffline
    ) where

import BV.Core.ConfigureSMT
import BV.Core.Types
import BV.Core.Types.Extras
import BV.SMTLIB2.Types
import BV.SMTLIB2.Types.Command

import Control.Monad (forM_)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Writer (runWriterT)
import Control.Monad.Writer (tell)
import Data.Function (applyWhen)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Optics
import Optics.State.Operators ((<<%=))

executeSMTProofCheckOffline
    :: MonadSolver m
    => SolverConfig -> Maybe SolverTimeout -> SMTProofCheck a -> m (Maybe SatResult)
executeSMTProofCheckOffline config timeout check = undefined

executeSMTProofCheckGroupOffline
    :: MonadSolver m
    => SolverConfig -> Maybe SolverTimeout -> SMTProofCheckGroup a -> m (Maybe SatResult)
executeSMTProofCheckGroupOffline config timeout check = undefined

executeSMTProofCheckGroupOnline
    :: (MonadSolver m, MonadThrow m)
    => SolverConfig
    -> Maybe SolverTimeout
    -> SMTProofCheckGroup a
    -> m (Maybe a, [(a, SatResult)])
executeSMTProofCheckGroupOnline config timeout group = do
    sendSimpleCommandExpectingSuccess $ SetOption (PrintSuccessOption True)
    sendSimpleCommandExpectingSuccess $ SetLogic "QF_AUFBV"
    mapM_ sendExpectingSuccess (smtConfigPreamble config)
    mapM_ (sendExpectingSuccess . configureSExpr config) group.setup
    (timeoutInfo, results) <- flip evalStateT 0 . runWriterT . runExceptT . forM_ group.imps $ \check -> do
        let meta = check.meta
        let split = splitHyp check
        lift . sendSimpleCommandExpectingSuccess $ Push 1
        forM_ split $ \hyp -> do
            labeledHyp <- labelSExpr hyp
            lift . sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr config $
                labeledHyp
        result <- ExceptT $ maybe (Left meta) Right <$> checkSatWithTimeout timeout
        tell [(meta, result)]
        lift . sendSimpleCommandExpectingSuccess $ Pop 1
        lift . sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr config $
            notS (andNS split)
    return (timeoutInfo ^? _Left, results)

labelSExpr :: MonadState Integer m => SExprWithPlaceholders -> m SExprWithPlaceholders
labelSExpr sexpr = do
    i <- simple <<%= (+ 1)
    let label = "hyp" ++ show i
    return $ labelS label sexpr

splitHyp :: SMTProofCheckImp a -> [SExprWithPlaceholders]
splitHyp imp =
    splitHypSExpr imp.term
--     if split
--     then splitHypSExpr hyp
--     else [hyp]
--   where
--     hyp = imp.term
--     split = any (`matchPatternS` hyp)
--         [ ["and"]
--         , ["not", ["=>"]]
--         , ["not", ["or"]]
--         , ["not", ["not"]]
--         ]

splitHypSExpr :: SExprWithPlaceholders -> [SExprWithPlaceholders]
splitHypSExpr = fromJust . traverse checkSExprWithPlaceholders . go . viewSExprWithPlaceholders
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
             , List [ p
                    , Atom (AtomOrPlaceholderAtom (SymbolAtom "false"))
                    ]
             ] ->
            go (notU p)
        List [ Atom (AtomOrPlaceholderAtom (SymbolAtom "="))
             , List args@[p, q]
             ] | any isBool args ->
                let (bool, term) = applyWhen (isBool q) swap (p, q)
                 in go (applyWhen (bool == "false") notU term)
        _ -> [hyp]

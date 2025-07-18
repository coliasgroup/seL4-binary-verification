{-# LANGUAGE OverloadedStrings #-}

module BV.Core.ExecuteSMTProofChecks
    ( OnlineSolverFailureInfo (..)
    , OnlineSolverFailureReason (..)
    , executeSMTProofCheckGroupOffline
    , executeSMTProofCheckGroupOnline
    , executeSMTProofCheckOffline
    ) where

import BV.Core.ModelConfig
import BV.Core.Types
import BV.Core.Types.Extras
import BV.SMTLIB2
import BV.SMTLIB2.Command

import Control.Monad (forM_)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (runExceptT, throwError)
import Data.Binary (Binary)
import Data.Function (applyWhen)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import GHC.Generics (Generic)

logic :: String
logic = "QF_AUFBV"

executeSMTProofCheckOffline
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout -> ModelConfig -> SMTProofCheck a -> m (Maybe SatResult)
executeSMTProofCheckOffline timeout config check = do
    commonSetup config check.setup
    sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr config $ goal
    checkSatWithTimeout timeout
  where
    goal = notS check.imp.term

executeSMTProofCheckGroupOffline
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout -> ModelConfig -> SMTProofCheckGroup a -> m (Maybe SatResult)
executeSMTProofCheckGroupOffline timeout config group =
    executeSMTProofCheckOffline timeout config $ SMTProofCheck
        { setup = group.setup
        , imp = SMTProofCheckImp
            { meta = map (.meta) group.imps
            , term = foldr1 andS (map (.term) group.imps)
            }
        }

data OnlineSolverFailureInfo
  = OnlineSolverFailureInfo
      { index :: Integer
      , reason :: OnlineSolverFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

instance Binary OnlineSolverFailureInfo where

data OnlineSolverFailureReason
  = OnlineSolverAnsweredSat
  | OnlineSolverTimedOut
  | OnlineSolverAnsweredUnknown SExpr
  deriving (Eq, Generic, Ord, Show)

instance Binary OnlineSolverFailureReason where

-- TODO
-- - This hyp numbering doesn't match graph-refine.
--   In graph-refine, it is global, whereas here it resets for each call to this function
executeSMTProofCheckGroupOnline
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout
    -> ModelConfig
    -> SMTProofCheckGroup a
    -> m (Either OnlineSolverFailureInfo ())
executeSMTProofCheckGroupOnline timeout config group = do
    commonSetup config group.setup
    runExceptT $ do
        forM_ (zip [0..] group.imps) $ \(i, imp) -> do
            let hyps = splitHyp (notS imp.term)
            sendSimpleCommandExpectingSuccess $ Push 1
            mapM_ sendAssert hyps
            checkSatWithTimeout timeout >>=
                let
                    throwErrorWithIndex = throwError . OnlineSolverFailureInfo i
                 in \case
                    Nothing -> throwErrorWithIndex OnlineSolverTimedOut
                    Just Sat -> throwErrorWithIndex OnlineSolverAnsweredSat
                    Just (Unknown reason) -> throwErrorWithIndex (OnlineSolverAnsweredUnknown reason)
                    Just Unsat -> return ()
            sendSimpleCommandExpectingSuccess $ Pop 1
            sendAssert $ notS (andNS hyps)
  where
    sendAssert = sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr config

commonSetup
    :: (MonadSolver m, MonadThrow m)
    => ModelConfig
    -> [SExprWithPlaceholders]
    -> m ()
commonSetup config setup = do
    sendSimpleCommandExpectingSuccess $ SetOption (PrintSuccessOption True)
    sendSimpleCommandExpectingSuccess $ SetLogic logic
    mapM_ sendExpectingSuccess (modelConfigPreamble config)
    mapM_ (sendExpectingSuccess . configureSExpr config) setup

splitHyp :: SExprWithPlaceholders -> [SExprWithPlaceholders]
splitHyp = fromJust . traverse checkSExprWithPlaceholders . go . viewSExprWithPlaceholders
  where
    notU x = List ["not", x]
    isBool = (`elem` ["true", "false"])
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
            go p
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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.Command
    ( Assertion (..)
    , CommandError (..)
    , FunDeclaration (..)
    , FunDefinition (..)
    , Option (..)
    , SatResult (..)
    , SimpleCommand (..)
    , boolToSExpr
    , checkSat
    , checkSatE
    , checkSatWithTimeout
    , checkSatWithTimeoutE
    , getModel
    , getModelE
    , getValue
    , getValueE
    , sendExpectingSuccess
    , sendExpectingSuccessE
    , sendRecv
    , sendRecvE
    , sendRecvWithTimeout
    , sendRecvWithTimeoutE
    , sendSimpleCommand
    , sendSimpleCommandExpectingSuccess
    , sendSimpleCommandExpectingSuccessE
    , simpleCommandToSExpr
    ) where

import BV.SMTLIB2

import Control.DeepSeq (NFData)
import Control.Exception (Exception, toException)
import Control.Monad (guard, (>=>))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Binary (Binary)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data CommandError
  = ErrorResponse String
  | UnexpectedResponse SExpr
  deriving (Eq, Generic, NFData, Ord, Show)

instance Exception CommandError

exceptTToThrow :: MonadThrow m => ExceptT CommandError m a -> m a
exceptTToThrow = runExceptT >=> either (throwM . toException) return

withoutTimeout :: MonadSolver m => m (Maybe a) -> m a
withoutTimeout = fmap fromJust

sendRecvWithTimeoutE :: (MonadSolver m, MonadError CommandError m) => Maybe SolverTimeout -> SExpr -> m (Maybe SExpr)
sendRecvWithTimeoutE timeout req = do
    sendSExpr req
    runMaybeT $ do
        resp <- MaybeT $ recvSExprWithTimeout timeout
        case viewSExpr resp of
            List ["error", Atom (StringAtom s)] -> throwError (ErrorResponse s)
            _ -> return resp

sendRecvWithTimeout :: (MonadSolver m, MonadThrow m) => Maybe SolverTimeout -> SExpr -> m (Maybe SExpr)
sendRecvWithTimeout timeout = exceptTToThrow . sendRecvWithTimeoutE timeout

sendRecvE :: (MonadSolver m, MonadError CommandError m) => SExpr -> m SExpr
sendRecvE req = withoutTimeout $ sendRecvWithTimeoutE Nothing req

sendRecv :: (MonadSolver m, MonadThrow m) => SExpr -> m SExpr
sendRecv = exceptTToThrow . sendRecvE

sendExpectingSuccessE :: (MonadSolver m, MonadError CommandError m) => SExpr -> m ()
sendExpectingSuccessE req = do
    resp <- sendRecvE req
    case viewSExpr resp of
        "success" -> return ()
        _ -> throwError (UnexpectedResponse resp)

sendExpectingSuccess :: (MonadSolver m, MonadThrow m) => SExpr -> m ()
sendExpectingSuccess = exceptTToThrow . sendExpectingSuccessE

-- TODO Unknown { reason :: String }
data SatResult
  = Sat
  | Unsat
  | Unknown SExpr
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary SatResult

checkSatWithTimeoutE :: (MonadSolver m, MonadError CommandError m) => Maybe SolverTimeout -> m (Maybe SatResult)
checkSatWithTimeoutE timeout = runMaybeT $ do
    checkSatResp <- MaybeT $ sendRecvWithTimeoutE timeout $ List ["check-sat"]
    case checkSatResp of
        "sat" -> return Sat
        "unsat" -> return Unsat
        "unknown" -> do
            let reasonUnkownKeyword = Atom (keywordAtom "reason-unkown")
            reasonUnknownResp <- lift $ sendRecvE ["get-info", reasonUnkownKeyword]
            case reasonUnknownResp of
                [kw, reason] | kw == reasonUnkownKeyword -> return (Unknown reason)
                _ -> throwError (UnexpectedResponse reasonUnknownResp)
        _ -> throwError (UnexpectedResponse checkSatResp)

checkSatWithTimeout :: (MonadSolver m, MonadThrow m) => Maybe SolverTimeout -> m (Maybe SatResult)
checkSatWithTimeout timeout = exceptTToThrow $ checkSatWithTimeoutE timeout

checkSatE :: (MonadSolver m, MonadError CommandError m) => m SatResult
checkSatE = withoutTimeout $ checkSatWithTimeoutE Nothing

checkSat :: (MonadSolver m, MonadThrow m) => m SatResult
checkSat = exceptTToThrow checkSatE

-- TODO refine response type
getModelE :: (MonadSolver m, MonadError CommandError m) => m [SExpr]
getModelE = do
    resp <- sendRecvE $ List ["get-model"]
    case resp of
        List model -> return model
        _ -> throwError (UnexpectedResponse resp)

getModel :: (MonadSolver m, MonadThrow m) => m [SExpr]
getModel = exceptTToThrow getModelE

getValueE :: (MonadSolver m, MonadError CommandError m) => [SExpr] -> m [SExpr]
getValueE terms = do
    resp <- sendRecvE $ List ["get-value", List terms]
    let opt = do
            List assocs <- return resp
            let f term assoc = do
                    List [term', value] <- return assoc
                    guard $ term == term'
                    return value
            sequenceA $ zipWith f terms assocs
    maybe (throwError (UnexpectedResponse resp)) return opt

getValue :: (MonadSolver m, MonadThrow m) => [SExpr] -> m [SExpr]
getValue = exceptTToThrow . getValueE

data SimpleCommand
  = SetLogic String
  | SetOption Option
  | Assert Assertion
  | DefineFun FunDefinition
  | DeclareFun FunDeclaration
  | Push Natural
  | Pop Natural
  deriving (Eq, Generic, NFData, Ord, Show)

data Option
  = PrintSuccessOption Bool
  | ProduceModelsOption Bool
  deriving (Eq, Generic, NFData, Ord, Show)

data Assertion
  = Assertion SExpr
  deriving (Eq, Generic, NFData, Ord, Show)

data FunDefinition
  = FunDefinition
      { name :: String
      , args :: [(String, SExpr)]
      , ret :: SExpr
      , body :: SExpr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data FunDeclaration
  = FunDeclaration
      { name :: String
      , args :: [SExpr]
      , ret :: SExpr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

sendSimpleCommand :: MonadSolver m => SimpleCommand -> m ()
sendSimpleCommand = sendSExpr . simpleCommandToSExpr

sendSimpleCommandExpectingSuccessE :: (MonadSolver m, MonadError CommandError m) => SimpleCommand -> m ()
sendSimpleCommandExpectingSuccessE = sendExpectingSuccessE . simpleCommandToSExpr

sendSimpleCommandExpectingSuccess :: (MonadSolver m, MonadThrow m) => SimpleCommand -> m ()
sendSimpleCommandExpectingSuccess = exceptTToThrow . sendSimpleCommandExpectingSuccessE

simpleCommandToSExpr :: SimpleCommand -> SExpr
simpleCommandToSExpr = \case
    SetLogic logic -> List ["set-logic", Atom (symbolAtom logic)]
    SetOption option -> List ("set-option" : optionToSExprList option)
    Assert (Assertion body) -> List ["assert", body]
    DefineFun (FunDefinition { name, args, ret, body }) -> List
        [ "define-fun"
        , Atom (symbolAtom name)
        , List (map (\(n, ty) -> List [Atom (symbolAtom n), ty]) args)
        , ret
        , body
        ]
    DeclareFun (FunDeclaration { name, args, ret }) -> List
        [ "declare-fun"
        , Atom (symbolAtom name)
        , List args
        , ret
        ]
    Push n -> List [ "push", Atom (numeralAtom n)]
    Pop n -> List [ "pop", Atom (numeralAtom n)]

boolToSExpr :: Bool -> SExpr
boolToSExpr = \case
    True -> "true"
    False -> "false"

optionToSExprList :: Option -> [SExpr]
optionToSExprList = \case
    PrintSuccessOption doit -> [Atom (keywordAtom "print-success"), boolToSExpr doit]
    ProduceModelsOption doit -> [Atom (keywordAtom "produce-models"), boolToSExpr doit]

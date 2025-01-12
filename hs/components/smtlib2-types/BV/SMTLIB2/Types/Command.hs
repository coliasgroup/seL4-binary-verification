{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.Types.Command
    ( Assertion (..)
    , CommandError (..)
    , FunDeclaration (..)
    , FunDefinition (..)
    , Option (..)
    , SatResult (..)
    , SimpleCommand (..)
    , boolToSExpr
    , checkSat
    , getModel
    , sendExpectingSuccess
    , sendRecvE
    , sendSimpleCommand
    , sendSimpleCommandExpectingSuccess
    , simpleCommandToSExpr
    ) where

import Control.DeepSeq (NFData)
import Control.Monad.Except (MonadError (throwError))
import GHC.Generics (Generic)

import BV.SMTLIB2.Types

data CommandError
  = ErrorResponse String
  | UnexpectedResponse SExpr
  deriving (Eq, Generic, NFData, Ord, Show)

sendRecvE :: (MonadSolver m, MonadError CommandError m) => SExpr -> m SExpr
sendRecvE req = do
    send req
    resp <- recv
    case viewSExpr resp of
        List ["error", Atom (StringAtom s)] -> throwError (ErrorResponse s)
        _ -> return resp

sendExpectingSuccess :: (MonadSolver m, MonadError CommandError m) => SExpr -> m ()
sendExpectingSuccess req = do
    resp <- sendRecvE req
    case viewSExpr resp of
        "success" -> return ()
        _ -> throwError (UnexpectedResponse resp)

data SatResult
  = Sat
  | Unsat
  | Unknown
  deriving (Eq, Generic, NFData, Ord, Show)

checkSat :: (MonadSolver m, MonadError CommandError m) => m SatResult
checkSat = do
    resp <- sendRecvE $ List ["check-sat"]
    case resp of
        "sat" -> return Sat
        "unsat" -> return Unsat
        "unknown" -> return Unknown
        _ -> throwError (UnexpectedResponse resp)

-- TODO refine response type
getModel :: (MonadSolver m, MonadError CommandError m) => m [SExpr]
getModel = do
    resp <- sendRecvE $ List ["get-model"]
    case resp of
        List model -> return model
        _ -> throwError (UnexpectedResponse resp)

data SimpleCommand
  = SetLogic String
  | SetOption Option
  | Assert Assertion
  | DefineFun FunDefinition
  | DeclareFun FunDeclaration
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
sendSimpleCommand = send . simpleCommandToSExpr

sendSimpleCommandExpectingSuccess :: (MonadSolver m, MonadError CommandError m) => SimpleCommand -> m ()
sendSimpleCommandExpectingSuccess = sendExpectingSuccess . simpleCommandToSExpr

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

boolToSExpr :: Bool -> SExpr
boolToSExpr = \case
    True -> "true"
    False -> "false"

optionToSExprList :: Option -> [SExpr]
optionToSExprList = \case
    PrintSuccessOption doit -> [Atom (keywordAtom "print-success"), boolToSExpr doit]
    ProduceModelsOption doit -> [Atom (keywordAtom "produce-models"), boolToSExpr doit]

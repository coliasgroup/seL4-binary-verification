{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.Core.Types.SMTProofCheck
    ( SMTProofCheck (..)
    , SMTProofCheckAssertion (..)
    , SMTProofCheckCommand (..)
    , SMTProofCheckFunDeclaration (..)
    , SMTProofCheckFunDefinition (..)
    , SMTProofCheckGroup (..)
    , SMTProofCheckImp (..)
    , commandToSExpr
    , configureAssertion
    , configureCommand
    , configureFunDeclaration
    , configureFunDefinition
    ) where

import BV.Core.ModelConfig
import BV.Core.Types.Extras.SExprWithPlaceholders (symbolS)
import BV.Core.Types.SExprWithPlaceholders
import BV.SMTLIB2 (GenericSExpr (List))
import BV.SMTLIB2.Command

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Optics

data SMTProofCheckGroup a
  = SMTProofCheckGroup
      { setup :: [SMTProofCheckCommand]
      , imps :: [SMTProofCheckImp a]
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data SMTProofCheck a
  = SMTProofCheck
      { setup :: [SMTProofCheckCommand]
      , imp :: SMTProofCheckImp a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data SMTProofCheckImp a
  = SMTProofCheckImp
      { meta :: a
      , term :: SExprWithPlaceholders
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data SMTProofCheckCommand
  = SMTProofCheckCommandDeclareFun SMTProofCheckFunDeclaration
  | SMTProofCheckCommandDefineFun SMTProofCheckFunDefinition
  | SMTProofCheckCommandAssert SMTProofCheckAssertion
  deriving (Eq, Generic, NFData, Ord, Show)

data SMTProofCheckFunDeclaration
  = SMTProofCheckFunDeclaration
      { name :: String
      , args :: [SExprWithPlaceholders]
      , ret :: SExprWithPlaceholders
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data SMTProofCheckFunDefinition
  = SMTProofCheckFunDefinition
      { name :: String
      , args :: [(String, SExprWithPlaceholders)]
      , ret :: SExprWithPlaceholders
      , body :: SExprWithPlaceholders
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data SMTProofCheckAssertion
  = SMTProofCheckAssertion SExprWithPlaceholders
  deriving (Eq, Generic, NFData, Ord, Show)

configureCommand :: ModelConfig -> SMTProofCheckCommand -> SimpleCommand
configureCommand config = \case
    SMTProofCheckCommandDeclareFun decl -> DeclareFun $ configureFunDeclaration config decl
    SMTProofCheckCommandDefineFun def -> DefineFun $ configureFunDefinition config def
    SMTProofCheckCommandAssert as -> Assert $ configureAssertion config as

configureFunDeclaration :: ModelConfig -> SMTProofCheckFunDeclaration -> FunDeclaration
configureFunDeclaration config decl = FunDeclaration
    { name = decl.name
    , args = map f decl.args
    , ret = f decl.ret
    }
  where
    f = configureSExpr config

configureFunDefinition :: ModelConfig -> SMTProofCheckFunDefinition -> FunDefinition
configureFunDefinition config def = FunDefinition
    { name = def.name
    , args = over (traversed % _2) f def.args
    , ret = f def.ret
    , body = f def.body
    }
  where
    f = configureSExpr config

configureAssertion :: ModelConfig -> SMTProofCheckAssertion -> Assertion
configureAssertion config (SMTProofCheckAssertion v) = Assertion (f v)
  where
    f = configureSExpr config

commandToSExpr :: SMTProofCheckCommand -> SExprWithPlaceholders
commandToSExpr = \case
    SMTProofCheckCommandDefineFun (SMTProofCheckFunDefinition { name, args, ret, body }) -> List
        [ "define-fun"
        , symbolS name
        , List [ List [symbolS n, ty] | (n, ty) <- args ]
        , ret
        , body
        ]
    SMTProofCheckCommandDeclareFun (SMTProofCheckFunDeclaration { name, args, ret }) -> List
        [ "declare-fun"
        , symbolS name
        , List args
        , ret
        ]
    SMTProofCheckCommandAssert (SMTProofCheckAssertion body) -> List ["assert", body]

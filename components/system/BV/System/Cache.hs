{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.System.Cache
    (
    ) where

import BV.Core.Types
import BV.System.Throttle

import GHC.Generics (Generic)

data ProblemCheckError
  = NoSolversAnswered
  | SomeSolverAnsweredSat
  | AllSolversAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

data AcceptableSatResult
  = AcceptableSatResultSat
  | AcceptableSatResultUnsat
  deriving (Eq, Generic, Ord, Show)

class Monad m => MonadCache n m | m -> n where
    checkSMTProofCheckWithCache :: Units -> CacheWrapper n m (SMTProofCheckGroup ()) AcceptableSatResult

type CacheWrapper n m a b = (a -> n b) -> a -> m b

module BV.System.Utils
    ( SemGate
    , applySemGate
    , newSemGate
    ) where

import BV.System.Core (SolverGate)

import Control.Concurrent (QSemN)
import Control.Concurrent.QSemN (newQSemN, signalQSemN, waitQSemN)
import Control.Exception.Safe (bracket_)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIOOp)

newtype SemGate
  = SemGate QSemN

newSemGate :: Integer -> IO SemGate
newSemGate width = SemGate <$> newQSemN (fromInteger width)

applySemGate :: MonadUnliftIO m => SemGate -> SolverGate m
applySemGate (SemGate sem) n = liftIOOp $ bracket_ (waitQSemN sem n') (signalQSemN sem n')
  where
    n' = fromInteger n

module BV.System.Utils
    ( SemGate
    , applySemGate
    , newSemGate
    ) where

import BV.System.Core (SolverGate)
import BV.System.Utils.TSemN

import Control.Concurrent.STM (atomically)
import Control.Exception.Safe (bracket_)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIOOp)

newtype SemGate
  = SemGate TSemN

newSemGate :: Integer -> IO SemGate
newSemGate width = SemGate <$> atomically (newTSemN width)

applySemGate :: MonadUnliftIO m => SemGate -> SolverGate m
applySemGate (SemGate sem) n = liftIOOp $ bracket_ (atomically (waitTSemN sem n)) (atomically (signalTSemN sem n))

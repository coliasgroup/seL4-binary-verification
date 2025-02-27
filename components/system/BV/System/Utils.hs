module BV.System.Utils
    ( SemGate
    , applySemGate
    , expecting
    , newSemGate
    ) where

import BV.System.Core (SolverGate)
import BV.System.Utils.TSemN

import Control.Concurrent.STM (atomically)
import Control.Exception.Safe (bracket_)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIOOp)
import Data.Either (fromRight)
import GHC.Stack (HasCallStack)
import Optics

newtype SemGate
  = SemGate TSemN

newSemGate :: Integer -> IO SemGate
newSemGate width = SemGate <$> atomically (newTSemN width)

applySemGate :: MonadUnliftIO m => SemGate -> SolverGate m
applySemGate (SemGate sem) n = liftIOOp $ bracket_ (atomically (waitTSemN sem n)) (atomically (signalTSemN sem n))

expecting :: (Is k An_AffineTraversal, HasCallStack) => Optic k is s t a b -> Lens s t a b
expecting optic = withAffineTraversal optic $ \match -> lens (fromRight (error "!isRight") . match)

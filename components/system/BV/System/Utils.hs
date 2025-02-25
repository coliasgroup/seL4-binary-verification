module BV.System.Utils
    ( SemGate
    , applySemGate
    , expecting
    , newSemGate
    ) where

import BV.System.Core (SolverGate)

import Control.Concurrent (QSemN)
import Control.Concurrent.QSemN (newQSemN, signalQSemN, waitQSemN)
import Control.Exception.Safe (bracket_)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIOOp)
import Data.Either (fromRight)
import GHC.Stack (HasCallStack)
import Optics

newtype SemGate
  = SemGate QSemN

newSemGate :: Integer -> IO SemGate
newSemGate width = SemGate <$> newQSemN (fromInteger width)

applySemGate :: MonadUnliftIO m => SemGate -> SolverGate m
applySemGate (SemGate sem) n = liftIOOp $ bracket_ (waitQSemN sem n') (signalQSemN sem n')
  where
    n' = fromInteger n

expecting :: (Is k An_AffineTraversal, HasCallStack) => Optic k is s t a b -> Lens s t a b
expecting optic = withAffineTraversal optic $ \match -> lens (fromRight (error "!isRight") . match)

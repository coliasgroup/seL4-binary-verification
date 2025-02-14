module BV.System.Core.SolverBackend.ParallelSolvers
    ( ParallelSolvers
    , SolverResult (..)
    , liftSolver
    , mapParallelSolvers
    , mapParallelSolversResult
    , runParallelSolvers
    ) where

import BV.System.Utils.UnliftIO.Async

import Control.Monad.Except (ExceptT (ExceptT), mapExceptT, runExceptT)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import GHC.Generics (Generic)
import Optics

data ParallelSolvers c m a
  = ParallelSolvers
      { width :: Integer
      , run :: ExceptT c m a
      }
  deriving (Foldable, Functor, Generic, Traversable)

instance (MonadUnliftIO m, Semigroup a) => Semigroup (ParallelSolvers c m a) where
    m <> n = ParallelSolvers
        { width = m.width + n.width
        , run = ExceptT . runConcurrentlyUnliftIOE $
            (<>)
                <$> makeConcurrentlyUnliftIOE (runExceptT m.run)
                <*> makeConcurrentlyUnliftIOE (runExceptT n.run)
        }

instance (MonadUnliftIO m, Monoid a) => Monoid (ParallelSolvers c m a) where
    mempty = ParallelSolvers
        { width = 0
        , run = pure mempty
        }

instance Monad m => Applicative (ParallelSolvers c m) where
    pure a = ParallelSolvers
        { width = 0
        , run = pure a
        }
    m <*> n = ParallelSolvers
        { width = max m.width n.width
        , run = m.run <*> n.run
        }

data SolverResult c a
  = Conclusive c
  | Inconclusive a

solverResultEitherIso :: Iso (SolverResult c a) (SolverResult c b) (Either c a) (Either c b)
solverResultEitherIso = iso f g
  where
    f (Conclusive c) = Left c
    f (Inconclusive a) = Right a
    g (Left c) = Conclusive c
    g (Right a) = Inconclusive a

liftSolver :: Functor m => m (SolverResult c a) -> ParallelSolvers c m a
liftSolver m = ParallelSolvers
    { width = 1
    , run = ExceptT (view solverResultEitherIso <$> m)
    }

runParallelSolvers :: Functor m => ParallelSolvers c m a -> m (Either c a)
-- runParallelSolvers m = (m.width, view (re solverResultEitherIso) <$> runExceptT m.run)
runParallelSolvers m = runExceptT m.run

mapParallelSolvers :: (Functor m, Functor n) => (m (SolverResult c a) -> n (SolverResult c b)) -> ParallelSolvers c m a -> ParallelSolvers c n b
mapParallelSolvers f = #run %~ mapExceptT (auf solverResultEitherIso f)

mapParallelSolversResult :: (SolverResult c a -> SolverResult c' a') -> ParallelSolvers c m a -> ParallelSolvers c' m a'
mapParallelSolversResult f m = undefined

--

auf :: (Functor f, Functor g) => Iso s t a b -> (f t -> g s) -> f b -> g a
auf k ftgs fb = withIso k $ \sa bt -> sa <$> ftgs (bt <$> fb)

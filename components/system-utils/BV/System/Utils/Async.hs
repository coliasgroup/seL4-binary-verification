module BV.System.Utils.Async
    ( concurrentlyE_
    , forConcurrentlyE
    , forConcurrentlyE_
    ) where

import Control.Concurrent.Async (ConcurrentlyE (ConcurrentlyE), concurrentlyE,
                                 runConcurrentlyE)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Traversable (for)

forConcurrentlyE :: Traversable t => t a -> (a -> IO (Either e b)) -> IO (Either e (t b))
forConcurrentlyE t f = runConcurrentlyE $ for t (ConcurrentlyE . f)

forConcurrentlyE_ :: Traversable t => t a -> (a -> IO (Either e b)) -> IO (Either e ())
forConcurrentlyE_ t f = runConcurrentlyE $ for_ t (ConcurrentlyE . f)

concurrentlyE_ :: IO (Either e a) -> IO (Either e b) -> IO (Either e ())
concurrentlyE_ a b = void <$> concurrentlyE a b

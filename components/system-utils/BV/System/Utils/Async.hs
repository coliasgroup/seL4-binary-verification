module BV.System.Utils.Async
    ( concurrentlyE_
    , forConcurrentlyE
    , forConcurrentlyE_
    , withLinkedAsync
    ) where

import Control.Concurrent.Async (Async, ConcurrentlyE (ConcurrentlyE),
                                 concurrentlyE, link, runConcurrentlyE,
                                 withAsync)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Traversable (for)

withLinkedAsync :: IO a -> (Async a -> IO b) -> IO b
withLinkedAsync action inner = withAsync action $ \a -> do
    link a
    inner a

forConcurrentlyE :: Traversable t => t a -> (a -> IO (Either e b)) -> IO (Either e (t b))
forConcurrentlyE t f = runConcurrentlyE $ for t (ConcurrentlyE . f)

forConcurrentlyE_ :: Traversable t => t a -> (a -> IO (Either e b)) -> IO (Either e ())
forConcurrentlyE_ t f = runConcurrentlyE $ for_ t (ConcurrentlyE . f)

concurrentlyE_ :: IO (Either e a) -> IO (Either e b) -> IO (Either e ())
concurrentlyE_ a b = void <$> concurrentlyE a b

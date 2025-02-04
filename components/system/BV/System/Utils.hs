module BV.System.Utils
    ( zipTraversableWith
    ) where

import Control.Monad.State (evalState, state)
import Data.List (uncons)
import Data.Maybe (fromJust)

zipTraversableWith :: Traversable f => (a -> b -> c) -> [a] -> f b -> f c
zipTraversableWith f as bs = flip evalState as $ traverse m bs
  where
    m b = do
        a <- state (fromJust . uncons)
        return $ f a b

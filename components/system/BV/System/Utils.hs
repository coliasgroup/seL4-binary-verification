module BV.System.Utils
    ( zipTraversableWith
    , zipTraversableWithOf
    ) where

import Control.Monad.State (evalState, state)
import Data.Maybe (fromJust)
import Optics

zipTraversableWith :: Traversable f => (a -> b -> c) -> [a] -> f b -> f c
zipTraversableWith = zipTraversableWithOf traversed

zipTraversableWithOf :: Traversal s t a b -> (c -> a -> b) -> [c] -> s -> t
zipTraversableWithOf traversal f as bs = flip evalState as $ traverseOf traversal m bs
  where
    m b = do
        a <- state (fromJust . uncons)
        return $ f a b

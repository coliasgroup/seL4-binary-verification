module BV.Utils
    ( intersperse
    , adjacently
    ) where

import Optics.Core

intersperse :: Monoid a => a -> [a] -> a
intersperse _ [] = mempty
intersperse sep (x:xs) = x <> mconcat (map (sep <>) xs)

adjacently :: Lens' s a -> Lens' s a' -> Lens' s (a, a')
adjacently l r =
    withLens l $ \getl setl ->
    withLens r $ \getr setr ->
        lens (\s -> (getl s, getr s))
             (\s (b, b') -> setr (setl s b) b')

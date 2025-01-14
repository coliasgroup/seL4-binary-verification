module BV.Core.Utils
    ( adjacently
    ) where

import Optics.Core

adjacently :: Lens' s a -> Lens' s a' -> Lens' s (a, a')
adjacently l r =
    withLens l $ \getl setl ->
    withLens r $ \getr setr ->
        lens (\s -> (getl s, getr s))
             (\s (b, b') -> setr (setl s b) b')

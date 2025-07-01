module BV.System.Core.Utils
    ( expecting
    , expectingIx
    , expecting_
    , is
    , unwrapped
    ) where

import Data.Either (fromRight)
import Data.Maybe (fromJust, isJust)
import GHC.Stack (HasCallStack)
import Optics

unwrapped :: HasCallStack => Lens (Maybe a) (Maybe b) a b
unwrapped = expecting _Just

expectingIx :: HasCallStack => (Ixed m, IxKind m ~ An_AffineTraversal) => Index m -> Lens' m (IxValue m)
expectingIx i = expecting (ix i)

expecting :: HasCallStack => Is k An_AffineTraversal => Optic k is s t a b -> Lens s t a b
expecting optic = withAffineTraversal optic $ \match update ->
    lens
        (fromRight (error "!isRight") . match)
        update

expecting_ :: HasCallStack => Is k An_AffineFold => Optic' k is s a -> Getter s a
expecting_ optic = to (fromJust . preview optic)

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is k s = isJust (preview k s)

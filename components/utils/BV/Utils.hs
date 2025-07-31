module BV.Utils
    ( compose2
    , compose3
    , compose4
    , compose5
    , ensure
    , ensureM
    , expecting
    , expectingAt
    , expectingIx
    , expecting_
    , findWithCallstack
    , is
    , unwrapped
    , (!@)
    ) where

import Data.Either (fromRight)
import Data.Function (applyWhen)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import GHC.Stack (HasCallStack)
import Optics

--

compose2 :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
compose2 = (.) . (.)

compose3 :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> (a1 -> a2 -> a3 -> c)
compose3 = (.) . compose2

compose4 :: (b -> c) -> (a1 -> a2 -> a3 -> a4 -> b) -> (a1 -> a2 -> a3 -> a4 -> c)
compose4 = (.) . compose3

compose5 :: (b -> c) -> (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> (a1 -> a2 -> a3 -> a4 -> a5 -> c)
compose5 = (.) . compose4

--

ensure :: HasCallStack => Bool -> a -> a
ensure p = applyWhen (not p) (error "ensure failed")

ensureM :: HasCallStack => Applicative f => Bool -> f ()
ensureM p = ensure p $ pure ()

--

unwrapped :: HasCallStack => Lens (Maybe a) (Maybe b) a b
unwrapped = expecting _Just

expectingIx :: HasCallStack => (Ixed m, IxKind m ~ An_AffineTraversal) => Index m -> Lens' m (IxValue m)
expectingIx i = expecting (ix i)

expectingAt :: HasCallStack => At m => Index m -> Lens' m (IxValue m)
expectingAt i = at i % unwrapped

expecting :: HasCallStack => Is k An_AffineTraversal => Optic k is s t a b -> Lens s t a b
expecting optic = withAffineTraversal optic $ \match update ->
    lens
        (fromRight (error "!isRight") . match)
        update

expecting_ :: HasCallStack => Is k An_AffineFold => Optic' k is s a -> Getter s a
expecting_ optic = to (fromJust . preview optic)

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is k s = isJust (preview k s)

--

findWithCallstack :: HasCallStack => (Show k, Ord k) => M.Map k a -> k -> a
findWithCallstack m k = if k `M.member` m then m M.! k else error ("not present: " ++ show k)

(!@) :: (HasCallStack, Show k, Ord k) => M.Map k a -> k -> a
(!@) = findWithCallstack

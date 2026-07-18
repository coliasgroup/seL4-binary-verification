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
    , formatArgSimple
    , fromIntegerChecked
    , is
    , mapFilterA
    , mapFilterWithKeyA
    , todo
    , unexpected
    , unimplemented
    , unwrapped
    , viewExpecting
    , (!@)
    ) where

import Control.Monad (filterM)
import Data.Either (fromRight)
import Data.Function (applyWhen)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import GHC.Stack (HasCallStack)
import Optics
import qualified Text.Printf as P

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

unimplemented :: HasCallStack => a
unimplemented = error "unimplemented"

todo :: HasCallStack => a
todo = error "todo"

unexpected :: HasCallStack => a
unexpected = error "unexpected"

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
expecting_ optic = to (viewExpecting optic)

viewExpecting :: Is k An_AffineFold => Optic' k is s a -> s -> a
viewExpecting optic = fromJust . preview optic

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is k s = isJust (preview k s)

--

fromIntegerChecked :: forall a. HasCallStack => (Num a, Integral a, Bounded a) => Integer -> a
fromIntegerChecked x = if lo <= x && x <= hi then fromInteger x else error "out of bounds"
  where
    lo = toInteger (minBound :: a)
    hi = toInteger (maxBound :: a)

--

findWithCallstack :: HasCallStack => (Show k, Ord k) => M.Map k a -> k -> a
findWithCallstack m k = if k `M.member` m then m M.! k else error ("not present: " ++ show k)

(!@) :: (HasCallStack, Show k, Ord k) => M.Map k a -> k -> a
(!@) = findWithCallstack

--

mapFilterA :: (Ord k, Applicative f) => (a -> f Bool) -> M.Map k a -> f (M.Map k a)
mapFilterA f m = M.fromList <$> filterM (f . snd) (M.toList m)

mapFilterWithKeyA :: (Ord k, Applicative f) => (k -> a -> f Bool) -> M.Map k a -> f (M.Map k a)
mapFilterWithKeyA f m = M.fromList <$> filterM (uncurry f) (M.toList m)

--

-- TODO add %D for concise debug
formatArgSimple :: (a -> String) -> a -> P.FieldFormatter
formatArgSimple pretty a fmt =
    if P.fmtChar (P.vFmt 'P' fmt) == 'P'
    then P.formatString (pretty a) (fmt { P.fmtChar = 's', P.fmtPrecision = Nothing })
    else P.errorBadFormat (P.fmtChar fmt)

-- TODO debugShow class, with D wrapper for printf

-- TODO (++ " " ++)

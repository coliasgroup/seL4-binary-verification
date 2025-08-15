module BV.Utils
    ( applyWhenM
    , compose2
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
    , setFilterA
    , todo
    , unimplemented
    , unwrapped
    , whileM
    , (!@)
    ) where

import Control.Monad (filterM, when)
import Data.Either (fromRight)
import Data.Function (applyWhen)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
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

whileM :: Monad m => m Bool -> m () -> m ()
whileM cond body = go
  where
    go = do
        p <- cond
        when p $ do
            body
            go

setFilterA :: (Ord a, Applicative f) => (a -> f Bool) -> S.Set a -> f (S.Set a)
setFilterA f s = S.fromList <$> filterM f (S.toList s)

mapFilterA :: (Ord k, Applicative f) => (b -> f Bool) -> M.Map k b -> f (M.Map k b)
mapFilterA f m = M.fromList <$> filterM (f . snd) (M.toList m)

applyWhenM :: Monad m => Bool -> (a -> m a) -> a -> m a
applyWhenM c f = if c then f else return

--

formatArgSimple :: (a -> String) -> a -> P.FieldFormatter
formatArgSimple pretty a fmt =
    if P.fmtChar (P.vFmt 'P' fmt) == 'P'
    then P.formatString (pretty a) (fmt { P.fmtChar = 's', P.fmtPrecision = Nothing })
    else P.errorBadFormat (P.fmtChar fmt)

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.RepGraph.Old.Flatten.MemCalls
    ( MemCalls
    , MemCallsIfKnown
    , MemCallsRange (..)
    , addMemCall
    , addUnboundedMemCalls
    , emptyMemCalls
    , memCallsRangesOverlap
    , mergeMemCalls
    , zeroMemCallsRange
    ) where

import BV.Core.Types

import Data.Function (on)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Optics

type MemCalls = M.Map Ident MemCallsRange

type MemCallsIfKnown = Maybe MemCalls

emptyMemCalls :: MemCalls
emptyMemCalls = M.empty

data MemCallsRange
  = MemCallsRange
      { min :: Integer
      , max :: Maybe Integer
      }
  deriving (Eq, Generic, Ord, Show)

zeroMemCallsRange :: MemCallsRange
zeroMemCallsRange = MemCallsRange
    { min = 0
    , max = Just 0
    }

addMemCall :: Ident -> MemCallsIfKnown -> MemCallsIfKnown
addMemCall fname = fmap $ flip M.alter fname $ Just . incr . fromMaybe zeroMemCallsRange
  where
    incr = (#min %~ (+1 )) . (#max % _Just %~ (+1 ))

addUnboundedMemCalls :: Ident -> MemCalls -> MemCalls
addUnboundedMemCalls fname = flip M.alter fname $ Just . incr . fromMaybe zeroMemCallsRange
  where
    incr = #max .~ Nothing

mergeMemCalls :: MemCalls -> MemCalls -> MemCalls
mergeMemCalls xs ys =
    if xs == ys
    then xs
    else
        let ks = M.keysSet xs <> M.keysSet ys
         in flip M.fromSet ks $ \k ->
                (mergeRanges `on` fromMaybe zeroMemCallsRange)
                    (M.lookup k xs)
                    (M.lookup k ys)
  where
    mergeRanges x y = MemCallsRange
        { min = min x.min y.min
        , max = max <$> x.max <*> y.max
        }

memCallsRangesOverlap :: MemCallsRange -> MemCallsRange -> Bool
memCallsRangesOverlap lhs rhs =
    maybe True (lhs.min <=) rhs.max && maybe True (rhs.min <=) lhs.max

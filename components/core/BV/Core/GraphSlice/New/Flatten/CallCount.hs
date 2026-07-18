module BV.Core.GraphSlice.New.Flatten.CallCount
    ( CallCount
    , addCall
    , addUnboundedCalls
    , areCallCountsCompatible
    , debugShowCallCount
    , emptyCallCount
    , mergeCallCounts
    ) where

import BV.Core.Types

import Data.Function (on)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

newtype CallCount
  = CallCount { unwrap :: M.Map Ident CallCountRange }
  deriving (Eq, Generic, Ord, Show)

emptyCallCount :: CallCount
emptyCallCount = CallCount M.empty

data CallCountRange
  = CallCountRange
      { min :: Integer
      , max :: Maybe Integer
      }
  deriving (Eq, Generic, Ord, Show)

zeroCallCountRange :: CallCountRange
zeroCallCountRange = CallCountRange
    { min = 0
    , max = Just 0
    }

addCall :: Ident -> CallCount -> CallCount
addCall = modifyCallCount $ (#min %~ (+1 )) . (#max % _Just %~ (+1 ))

addUnboundedCalls :: Ident -> CallCount -> CallCount
addUnboundedCalls = modifyCallCount $ #max .~ Nothing

modifyCallCount :: (CallCountRange -> CallCountRange) -> Ident -> (CallCount -> CallCount)
modifyCallCount f fname = #unwrap %~ (flip M.alter fname $ Just . f . fromMaybe zeroCallCountRange)

mergeCallCounts :: CallCount -> CallCount -> CallCount
mergeCallCounts xs ys =
    if xs == ys
    then xs
    else
        let ks = ((<>) `on` (M.keysSet . (.unwrap))) xs ys
         in CallCount $ flip M.fromSet ks $ \k ->
                (mergeRanges `on` (fromMaybe zeroCallCountRange . M.lookup k . (.unwrap))) xs ys
  where
    mergeRanges x y = CallCountRange
        { min = min x.min y.min
        , max = max <$> x.max <*> y.max
        }

areCallCountsCompatible
    :: RefineTag t
    => (WithTag t Ident -> Maybe (PairingId t))
    -> ByTag t CallCount
    -> Bool
areCallCountsCompatible lookupPairingIdOpt callsByTag =
    all compat int
  where
    int =
        let f (WithTag tag calls) =
                S.fromList $
                    mapMaybe (lookupPairingIdOpt . WithTag tag) $
                        M.keys calls.unwrap
         in foldMap f (withTags callsByTag)
    compat pid =
        let f tag =
                M.findWithDefault
                    zeroCallCountRange
                    (viewAtTag tag pid)
                    (viewAtTag tag callsByTag).unwrap
         in callCountRangesOverlap (f leftTag) (f rightTag)

callCountRangesOverlap :: CallCountRange -> CallCountRange -> Bool
callCountRangesOverlap lhs rhs =
    maybe True (lhs.min <=) rhs.max && maybe True (rhs.min <=) lhs.max

debugShowCallCount :: CallCount -> String
debugShowCallCount calls = intercalate ", "
    [ fname.unwrap ++ ": " ++ debugShowCallCountRange range
    | (fname, range) <- M.toList calls.unwrap
    ]

debugShowCallCountRange :: CallCountRange -> String
debugShowCallCountRange range = show range.min ++ ".." ++ (case range.max of
    Nothing -> ""
    Just n -> "=" ++ show n)

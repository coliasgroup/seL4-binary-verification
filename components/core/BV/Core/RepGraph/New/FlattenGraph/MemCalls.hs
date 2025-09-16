{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.RepGraph.New.FlattenGraph.MemCalls
    ( MemCalls
    , addMemCall
    , addUnboundedMemCalls
    , areMemCallsCompatible
    , mergeMemCalls
    ) where

import BV.Core.Types
import BV.Core.Types.Extras

import Data.Function (on)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

type MemCalls = Map Ident MemCallsRange

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

addMemCall :: Ident -> MemCalls -> MemCalls
addMemCall fname = flip M.alter fname $ Just . incr . fromMaybe zeroMemCallsRange
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

areMemCallsCompatible
    :: LookupFunctionSignature AsmRefineTag
    -> (WithTag' Ident -> PairingId')
    -> ByTag' (Maybe MemCalls) -> Bool
areMemCallsCompatible lookupSig lookupPairingId callsOpt = case sequenceA callsOpt of
    Nothing -> True
    Just calls ->
        let cCastCalls = M.fromList $ catMaybes
                [ let pairingId = lookupPairingId (WithTag Asm asmName)
                      cName = pairingId.c
                      cSig = lookupSig (WithTag C cName)
                  in if any (\arg -> isMemT arg.ty) cSig.output
                     then Just (cName, asmCallsForFun)
                     else Nothing
                | (asmName, asmCallsForFun) <- M.toList calls.asm
                ]
            compat rname =
                let rcast = fromMaybe zeroMemCallsRange $ cCastCalls !? rname
                    ractual = fromMaybe zeroMemCallsRange $ calls.c !? rname
                in maybe True (ractual.min <=) rcast.max && maybe True (rcast.min <=) ractual.max
         in all compat $ S.toList $ M.keysSet calls.c <> M.keysSet cCastCalls

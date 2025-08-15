{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.AsmRefineTag
    ( AsmRefineTag (..)
    , ByAsmRefineTag (..)
    , ByTag'
    , InlineScript'
    , Pairing'
    , PairingId'
    , Problem'
    , WithTag'
    , byAsmRefineTag
    , getAsm
    , getC
    ) where

import BV.Core.Types.Pairing
import BV.Core.Types.Problem
import BV.Core.Types.Tag
import BV.Utils (formatArgSimple)

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Vector.Binary ()
import GHC.Generics (Generic)
import GHC.IsList (fromList)
import GHC.Records (HasField (getField))
import Text.Printf (PrintfArg (formatArg))

data AsmRefineTag
  = Asm
  | C
  deriving (Bounded, Enum, Eq, Generic, NFData, Ord, Show)

instance Binary AsmRefineTag

instance PrintfArg AsmRefineTag where
    formatArg = formatArgSimple prettyTag

instance Tag AsmRefineTag where
    prettyTag = \case
        Asm -> "ASM"
        C -> "C"
    parsePrettyTag = \case
        "ASM" -> Just Asm
        "C" -> Just C
        _ -> Nothing

instance StaticTag AsmRefineTag

instance RefineTag AsmRefineTag

getAsm :: ByTag AsmRefineTag a -> a
getAsm = getLeft

getC :: ByTag AsmRefineTag a -> a
getC = getRight

instance HasField "asm" (ByTag AsmRefineTag a) a where
  getField = getAsm

instance HasField "c" (ByTag AsmRefineTag a) a where
  getField = getC

-- TODO
-- {-# LANGUAGE PatternSynonyms #-}
-- pattern ByAsmRefineTag :: a -> a -> ByTag AsmRefineTag a
-- pattern ByAsmRefineTag { asm, c } <- TODO

data ByAsmRefineTag a
  = ByAsmRefineTag
      { asm :: a
      , c :: a
      }
  deriving (Eq, Generic, NFData, Ord, Show)

byAsmRefineTag :: ByAsmRefineTag a -> ByTag AsmRefineTag a
byAsmRefineTag (ByAsmRefineTag { asm, c }) = fromList [asm, c]

-- TODO

type ByTag' = ByTag AsmRefineTag

type WithTag' = WithTag AsmRefineTag

type PairingId' = PairingId AsmRefineTag

type Pairing' = Pairing AsmRefineTag

type Problem' = Problem AsmRefineTag

type InlineScript' = InlineScript AsmRefineTag

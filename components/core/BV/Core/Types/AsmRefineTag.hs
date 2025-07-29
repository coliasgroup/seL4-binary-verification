{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.AsmRefineTag
    ( AsmRefineTag (..)
    , ByAsmRefineTag (..)
    , ByTag'
    , InlineScript'
    , Pairing'
    , PairingId'
    , Problem'
    , Problems' (..)
    , WithTag'
    , byAsmRefineTag
    , getAsm
    , getC
    ) where

import BV.Core.Types.Pairing
import BV.Core.Types.Problem
import BV.Core.Types.SearchOutputs
import BV.Core.Types.Tag

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import qualified Data.Map as M
import Data.Vector.Binary ()
import GHC.Generics (Generic)
import GHC.IsList (fromList)

data AsmRefineTag
  = Asm
  | C
  deriving (Bounded, Enum, Eq, Generic, NFData, Ord, Show)

instance Binary AsmRefineTag where

instance Tag AsmRefineTag where
    prettyTag = \case
        Asm -> "ASM"
        C -> "C"
    parsePrettyTag = \case
        "ASM" -> Just Asm
        "C" -> Just C
        _ -> Nothing

instance RefineTag AsmRefineTag

getAsm :: ByTag AsmRefineTag a -> a
getAsm = getLeft

getC :: ByTag AsmRefineTag a -> a
getC = getRight

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

newtype Problems'
  = Problems { unwrap :: M.Map PairingId' Problem' }
  deriving (Eq, Generic)
  deriving newtype (NFData)

type InlineScript' = InlineScript AsmRefineTag

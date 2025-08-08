{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.ObjDump
    ( ObjDumpInfo (..)
    , ROData (..)
    , RODataInputRangeType (..)
    , RODataInputRanges
    , RODataRange (..)
    , Section (..)
    , Symbol (..)
    , rodataStructNamesOf
    , rodataStructsOf
    , sectionEnd
    , symbolEnd
    ) where

import BV.Core.Types.Program (Ident (..), Struct (..))

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Map (Map)
import qualified Data.Map.Lazy as M
import GHC.Generics (Generic)
import Optics

data ObjDumpInfo
  = ObjDumpInfo
      { symbols :: Map String Symbol
      , sections :: Map String Section
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary ObjDumpInfo

data Symbol
  = Symbol
      { addr :: Integer
      , size :: Integer
      , section :: String
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary Symbol

data Section
  = Section
      { addr :: Integer
      , size :: Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary Section

symbolEnd :: Symbol -> Integer
symbolEnd = (+) <$> view #addr <*> view #size

sectionEnd :: Section -> Integer
sectionEnd = (+) <$> view #addr <*> view #size

data ROData
  = ROData
      { ranges :: [RODataRange]
      , rodata :: Map Integer Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary ROData

data RODataRange
  = RODataRange
      { addr :: Integer
      , size :: Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary RODataRange

data RODataInputRangeType
  = RODataInputRangeTypeSection
  | RODataInputRangeTypeSymbol
  deriving (Eq, Generic, NFData, Ord, Show)

type RODataInputRanges = [(RODataInputRangeType, String)]

rodataStructsOf :: ROData -> Map Ident Struct
rodataStructsOf rodata =
    M.fromList
        [ let struct = Struct
                { size = range.size
                , align = 1
                , fields = M.empty
                }
           in (structName, struct)
        | (structName, range) <- rodataStructNamesOf rodata
        ]

rodataStructNamesOf :: ROData -> [(Ident, RODataRange)]
rodataStructNamesOf rodata =
    [ let name = Ident $ case rodata.ranges of
            [_] -> "rodata_struct"
            _ -> "rodata_struct_" ++ show i
       in (name, range)
    | (i :: Integer, range) <- zip [1..] rodata.ranges
    ]

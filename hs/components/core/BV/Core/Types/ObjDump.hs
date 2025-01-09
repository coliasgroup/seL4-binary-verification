module BV.Core.Types.ObjDump
    ( ObjDumpInfo (..)
    , Section (..)
    , Symbol (..)
    , sectionEnd
    , symbolEnd
    ) where

import Data.Map (Map)
import GHC.Generics (Generic)
import Optics.Core

data ObjDumpInfo
  = ObjDumpInfo
      { symbols :: Map String Symbol
      , sections :: Map String Section
      }
  deriving (Eq, Generic, Ord, Show)

data Symbol
  = Symbol
      { addr :: Integer
      , size :: Integer
      , section :: String
      }
  deriving (Eq, Generic, Ord, Show)

data Section
  = Section
      { addr :: Integer
      , size :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

symbolEnd :: Symbol -> Integer
symbolEnd = (+) <$> view #addr <*> view #size

sectionEnd :: Section -> Integer
sectionEnd = (+) <$> view #addr <*> view #size

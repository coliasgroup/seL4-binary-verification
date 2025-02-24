{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.ObjDump
    ( ObjDumpInfo (..)
    , Section (..)
    , Symbol (..)
    , sectionEnd
    , symbolEnd
    ) where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Map (Map)
import GHC.Generics (Generic)
import Optics

data ObjDumpInfo
  = ObjDumpInfo
      { symbols :: Map String Symbol
      , sections :: Map String Section
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary ObjDumpInfo where

data Symbol
  = Symbol
      { addr :: Integer
      , size :: Integer
      , section :: String
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary Symbol where

data Section
  = Section
      { addr :: Integer
      , size :: Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary Section where

symbolEnd :: Symbol -> Integer
symbolEnd = (+) <$> view #addr <*> view #size

sectionEnd :: Section -> Integer
sectionEnd = (+) <$> view #addr <*> view #size

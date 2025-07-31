module BV.Search.Core.StackBounds.All
    ( FullDiscoverStackBoundsInput (..)
    , prepareDiscoverStackBoundsInput
    ) where

import BV.Core.Stages
import BV.Core.Types
import BV.Core.Utils.IncludeExcludeFilter

import BV.Search.Core.StackBounds

import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

data FullDiscoverStackBoundsInput
  = DiscoverAllStacFullDiscoverStackBoundsInputkBoundsInput
      { program :: Program
      , rodata :: ROData
      , earlyAsmFunctionFilter :: AsmFunctionFilter
      , include :: S.Set Ident
      }
  deriving (Generic)

prepareDiscoverStackBoundsInput
    :: FullDiscoverStackBoundsInput
    -> DiscoverStackBoundsInput
prepareDiscoverStackBoundsInput input = DiscoverStackBoundsInput
    { rodata = input.rodata
    , functions = lookupFunction
    , include = input.include
    }

  where

    alteredProgram = fixupProgram $ input.program & #functions %~ M.filterWithKey (\k _v ->
        applyIncludeExcludeFilter input.earlyAsmFunctionFilter k)

    finalProgram = alteredProgram

    lookupFunction funName = finalProgram.functions M.! funName

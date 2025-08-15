module BV.Search.Core.StackBounds.All
    ( FullDiscoverStackBoundsInput (..)
    , prepareDiscoverStackBoundsInput
    ) where

import BV.Core.Stages
import BV.Core.Types
import BV.Search.Core.StackBounds

import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

data FullDiscoverStackBoundsInput
  = DiscoverAllStacFullDiscoverStackBoundsInputkBoundsInput
      { programs :: ByTag' Program
      , objDumpInfo :: ObjDumpInfo
      , rodata :: ROData
      , cFunctionPrefix :: String
      , earlyAsmFunctionFilter :: FunctionFilter
      , includeAsmFrom :: S.Set Ident
      }
  deriving (Generic)

prepareDiscoverStackBoundsInput
    :: FullDiscoverStackBoundsInput
    -> DiscoverStackBoundsInput
prepareDiscoverStackBoundsInput input = DiscoverStackBoundsInput
    { structs = (.structs) <$> input.programs
    , rodata = input.rodata
    , lookupFunction
    , pairingIds
    , includeAsmFrom = input.includeAsmFrom
    }

  where

    finalPrograms =
          over (atTag C) (pseudoCompile input.objDumpInfo)
        . over (atTag Asm) (applyFunctionFilter input.earlyAsmFunctionFilter)
        . over mapped fixupProgram
        $ input.programs

    lookupFunction (WithTag tag funName) = (viewAtTag tag finalPrograms).functions M.! funName

    pairingIds = S.fromList $ do
        asm <- M.keys finalPrograms.asm.functions
        let c = asm & #unwrap %~ (input.cFunctionPrefix ++)
        guard $ c `M.member` finalPrograms.c.functions
        return $ byAsmRefineTag (ByAsmRefineTag { asm, c })

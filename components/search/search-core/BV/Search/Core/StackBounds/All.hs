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
    , functions = lookupFunction
    , pairings
    , includeAsmFrom = input.includeAsmFrom
    }

  where

    alterProgramByTag = byAsmRefineTag (ByAsmRefineTag
        { asm = applyFunctionFilter input.earlyAsmFunctionFilter
        , c = pseudoCompile input.objDumpInfo
        })

    alteredPrograms = alterProgramByTag <*> (fixupProgram <$> input.programs)

    finalPrograms = alteredPrograms

    lookupFunction (WithTag tag funName) = (viewAtTag tag finalPrograms).functions M.! funName

    pairings = S.fromList $ do
        asm <- M.keys finalPrograms.asm.functions
        let c = asm & #unwrap %~ (input.cFunctionPrefix ++)
        guard $ c `M.member` finalPrograms.c.functions
        return $ byAsmRefineTag (ByAsmRefineTag { asm, c })

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
      { programs :: ByTag' Program
      , objDumpInfo :: ObjDumpInfo
      , rodata :: ROData
      , cFunctionPrefix :: String
      , earlyAsmFunctionFilter :: AsmFunctionFilter
      , includeAsmFrom :: S.Set Ident
      }
  deriving (Generic)

prepareDiscoverStackBoundsInput
    :: FullDiscoverStackBoundsInput
    -> DiscoverStackBoundsInput
prepareDiscoverStackBoundsInput input = DiscoverStackBoundsInput
    { rodata = input.rodata
    , functions = lookupFunction
    , include
    }

  where

    alterProgramByTag = byAsmRefineTag (ByAsmRefineTag
        { asm = #functions %~ M.filterWithKey (\k _v ->
            applyIncludeExcludeFilter input.earlyAsmFunctionFilter k)
        , c = pseudoCompile input.objDumpInfo
        })

    alteredPrograms = fixupProgram <$> (alterProgramByTag <*> input.programs)

    finalPrograms = alteredPrograms

    lookupFunction (WithTag tag funName) = (viewAtTag tag finalPrograms).functions M.! funName

    includeAsm = go S.empty input.includeAsmFrom
      where
        go visited toVisit = case S.minView toVisit of
            Nothing -> visited
            Just (cur, rest) ->
                let neighbors = lookupFunction (WithTag Asm cur)
                        ^.. #body % _Just % #nodes % folded % #_NodeCall % #functionName
                 in go (S.insert cur visited) (rest <> S.fromList neighbors)

    include = flip S.map includeAsm $ \asm ->
        let c = asm & #unwrap %~ (input.cFunctionPrefix ++)
         in byAsmRefineTag (ByAsmRefineTag { asm, c })

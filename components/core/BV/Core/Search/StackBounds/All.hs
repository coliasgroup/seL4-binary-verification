module BV.Core.Search.StackBounds.All
    ( DiscoverAllStackBoundsInput (..)
    , discoverAllStackBounds
    ) where

import BV.Core.ModelConfig (ModelConfig)
import BV.Core.Search.StackBounds
import BV.Core.Stages
import BV.Core.Types
import BV.Core.Utils
import qualified BV.SMTLIB2.Monad as S

import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

data DiscoverAllStackBoundsInput
  = DiscoverAllStackBoundsInput
      { program :: Program
      , rodata :: ROData
      , earlyAsmFunctionFilter :: IncludeExcludeFilter Ident
      , include :: S.Set Ident
      }
  deriving (Generic)

discoverAllStackBounds
    :: (Monad m, S.MonadSolver n)
    => ((ModelConfig -> n a) -> m a)
    -> DiscoverAllStackBoundsInput
    -> m StackBounds
discoverAllStackBounds run input = discoverStackBounds run $ DiscoverStackBoundsInput
    { rodata = input.rodata
    , functions = lookupFunction
    , include = input.include
    }

  where

    alteredProgram = fixupProgram $ input.program & #functions %~ M.filterWithKey (\k _v ->
        applyIncludeExcludeFilter input.earlyAsmFunctionFilter k)

    finalProgram = alteredProgram

    lookupFunction funName = finalProgram.functions M.! funName

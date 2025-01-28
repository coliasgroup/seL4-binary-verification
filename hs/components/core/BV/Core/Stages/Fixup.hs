module BV.Core.Stages.Fixup
    ( fixupProgram
    ) where

import Control.Monad (forM_)
import Control.Monad.State
import Data.Foldable (find)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Optics

import BV.Core.Types
import BV.Core.Types.Extras

fixupProgram :: Program -> Program
fixupProgram = #functions % traversed % #body % traversed %~ fixupFunctionBody

fixupFunctionBody :: FunctionBody -> FunctionBody
fixupFunctionBody = execState $ do
    ensureSimpleEntrypoint
    ensureClosed

ensureSimpleEntrypoint :: State FunctionBody ()
ensureSimpleEntrypoint = do
    origEntryPointId <- use #entryPoint
    newEntryPointAddr <- use $ #nodes % to freshNodeAddr
    modify $ #nodes % at newEntryPointAddr ?~ trivialNode origEntryPointId
    assign #entryPoint (Addr newEntryPointAddr)

ensureClosed :: State FunctionBody ()
ensureClosed = do
    allContAddrs <- gets $ S.fromList . toListOf (#nodes % traversed % nodeConts % #_Addr)
    liveAddrs <- use $ #nodes % to M.keysSet
    let deadAddrs = allContAddrs `S.difference` liveAddrs
    forM_ deadAddrs $ \deadAddr -> do
        modify $ #nodes % at deadAddr ?~ trivialNode Err

-- implementation matches graph_refine.syntax.fresh_node
freshNodeAddr :: NodeMap -> NodeAddr
freshNodeAddr nodeMap = fromJust . find (`M.notMember` nodeMap) $
    iterate (+ 16) 17

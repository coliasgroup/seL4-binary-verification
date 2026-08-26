{-# LANGUAGE OverloadedStrings #-}

module BV.Core.Stages.FixupProgram
    ( fixupProgram
    ) where

import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad.State (State, execState, gets, modify)
import Data.Bits (shiftL)
import Data.Foldable (find, for_)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Optics

fixupProgram :: Program -> Program
fixupProgram = #functions % traversed % #body % traversed %~ fixupFunctionBody

fixupFunctionBody :: FunctionBody -> FunctionBody
fixupFunctionBody = execState $ do
    ensureSimpleEntrypoint
    cutNoreturnCallConts
    cutUnreachableCalls
    ensureClosed

ensureSimpleEntrypoint :: State FunctionBody ()
ensureSimpleEntrypoint = do
    origEntryPointId <- use #entryPoint
    newEntryPointAddr <- use $ #nodes % to freshNodeAddr
    modify $ #nodes % at newEntryPointAddr ?~ trivialNode origEntryPointId
    assign #entryPoint (Addr newEntryPointAddr)

noreturnFunctionNames :: S.Set Ident
noreturnFunctionNames = S.fromList
    [ "halt"
    , "idle_thread"
    , "restore_user_context"
    , "c_handle_vm_fault"
    , "fastpath_restore"
    , "slowpath"
    ]

cutNoreturnCallConts :: State FunctionBody ()
cutNoreturnCallConts = modify $ #nodes % traversed % #_NodeCall %~ \callNode ->
    if callNode.functionName `S.member` noreturnFunctionNames
        then callNode & #next .~ Err
        else callNode

unreachableFunctionName :: Ident
unreachableFunctionName = "Kernel_C.StrictC'__builtin_unreachable"

cutUnreachableCalls :: State FunctionBody ()
cutUnreachableCalls = modify $ #nodes % traversed %~ \node -> case node of
    NodeCall callNode | callNode.functionName == unreachableFunctionName ->
        trivialNode Err
    _ -> node

ensureClosed :: State FunctionBody ()
ensureClosed = do
    allContAddrs <- gets $ S.fromList . toListOf (#nodes % traversed % nodeConts % #_Addr)
    liveAddrs <- use $ #nodes % to M.keysSet
    let deadAddrs = allContAddrs `S.difference` liveAddrs
    for_ deadAddrs $ \deadAddr -> do
        modify $ #nodes % at deadAddr ?~ trivialNode Err

-- implementation matches graph_refine.syntax.fresh_node
freshNodeAddr :: NodeMap -> NodeAddr
freshNodeAddr nodeMap = fromJust . find (`M.notMember` nodeMap) $
    [ NodeAddr (hint + (i `shiftL` 4))
    | i <- [1..]
    ]
  where
    hint = 1

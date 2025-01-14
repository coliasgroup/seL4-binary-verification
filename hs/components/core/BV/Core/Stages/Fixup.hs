module BV.Core.Stages.Fixup
    ( fixupProgram
    ) where

import Control.Monad.State
import Optics

import BV.Core.Types

fixupProgram :: Program -> Program
fixupProgram = #functions % traversed % #body % traversed %~ fixupFunctionBody

fixupFunctionBody :: FunctionBody -> FunctionBody
fixupFunctionBody = execState $ do
    return ()

freshNodeAddr :: NodeMap -> NodeAddr
freshNodeAddr = undefined

module Main
    ( main
    ) where

import BV.System.Core
import BV.System.EvalStages
import BV.TargetDir
import BV.Test.Utils
import BV.Test.Utils.Paths

import Control.Monad.Logger (runStderrLoggingT)

main :: IO ()
main = do
    input <- seL4DefaultReadStagesInput $ TargetDir $ defaultTestTargetDir "big"
    checks <- runStderrLoggingT $ evalStages defaultEvalStagesContext input
    let check = lookupCheck path checks
    return ()
  where

path :: CheckPath
path = CheckPath
    { checkGroupPath = CheckGroupPath
        { pairingId = undefined
        , proofScriptEdgePath = undefined
        , checkIndices = undefined
        , fingerprint = undefined
        }
    , indexInGroup = undefined
    , fingerprint = undefined
    }

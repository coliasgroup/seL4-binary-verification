module BV.Core.RepGraph.New
    ( module BV.Core.RepGraph.New.FlattenGraph
    , module BV.Core.RepGraph.New.InterpretHyp
    , module BV.Core.RepGraph.New.SendFlatExprCommand
    , module BV.Core.RepGraph.New.SendSolverExprCommand
    , addPValidDomAssertions
    , isUnreachable
    ) where

import BV.Core.RepGraph.New.FlattenGraph
import BV.Core.RepGraph.New.InterpretHyp
import BV.Core.RepGraph.New.SendFlatExprCommand
import BV.Core.RepGraph.New.SendSolverExprCommand

import BV.Core.Structs (MonadStructs)
import BV.Core.Types
import BV.Core.Types.Extras

import Data.Maybe (fromJust)

-- TODO
addPValidDomAssertions :: Monad m => m ()
addPValidDomAssertions = do
    return ()

isUnreachable :: (Tag t, MonadStructs m, MonadRepGraphSendSExpr m) => Visit -> RepGraphFlattenGraphTaggedT t m SExprWithPlaceholders
isUnreachable visit = do
    pcEnv <- fromJust <$> getNodePcEnv visit
    -- convertFlatExpr (notE pcEnv.pc) >>= convertSolverExpr
    undefined

type Name = Ident

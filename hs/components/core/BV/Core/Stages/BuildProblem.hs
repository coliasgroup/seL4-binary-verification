module BV.Core.Stages.BuildProblem
    ( buildProblem
    ) where

import BV.Core.Types

buildProblem :: (Tag -> Ident -> Function) -> InlineScript -> PairingOf (Named Function) -> Problem
buildProblem = undefined

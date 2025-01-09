module BV.Core.BuildProblem
    ( buildProblem
    ) where

import Optics.Core

import BV.Core.Types

buildProblem :: PairingOf (Named Function) -> InlineScript -> (Ident -> Function) -> Problem
buildProblem = undefined

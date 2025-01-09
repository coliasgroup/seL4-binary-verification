module BV.Core.BuildProblem
    ( buildProblem
    ) where

import Optics.Core

import BV.Core.Inputs
import BV.Core.Pairing
import BV.Core.Problem
import BV.Core.Program

buildProblem :: PairingOf (Named Function) -> InlineScript -> (Ident -> Function) -> Problem
buildProblem = undefined

module BV.BuildProblem
    ( buildProblem
    ) where

import Optics.Core

import BV.Inputs
import BV.Pairing
import BV.Problem
import BV.Program

buildProblem :: PairingOf (Named Function) -> InlineScript -> (Ident -> Function) -> Problem
buildProblem = undefined

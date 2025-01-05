module BV.Inputs where

import qualified Data.Map as M

import BV.Program

type StackBounds = M.Map Ident Expr

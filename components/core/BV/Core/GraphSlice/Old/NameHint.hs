module BV.Core.GraphSlice.Old.NameHint
    ( NameHint
    , condName
    , inductVarName
    , localName
    , localNameBefore
    , nodeCountName
    , pathCondName
    , successName
    ) where

import BV.Core.Types

import Data.Char (isAlpha)
import Data.List (intercalate, tails)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Optics
import Text.Printf (printf)

type NameHint = String

localNameBefore :: Visit t -> Ident -> NameHint
localNameBefore visit var = printf "%P_v_at_%s" var (nodeCountName visit)

localName ::Visit t ->  Ident -> NameHint
localName visit var = printf "%P_after_%s" var (nodeCountName visit)

condName :: Visit t -> NameHint
condName visit = printf "cond_at_%s" (nodeCountName visit)

pathCondName :: Tag t => Visit t -> NameHint
pathCondName visit = printf "path_cond_to_%s_%P" (nodeCountName visit) visit.tag

successName :: Visit t -> Ident -> NameHint
successName visit fname =
    printf "%s_success_at_%s" name (nodeCountName visit)
  where
    name = case unsnoc names of
        Nothing -> "fun"
        Just (_, name') -> name'
    names =
        [ intercalate "_" suffix
        | suffix@(bit:_) <- filter (not . null) $ tails bits
        , all isAlpha bit
        ]
    bits = splitOn "." fname.unwrap

nodeCountName :: Visit t -> NameHint
nodeCountName visit = intercalate "_" $
    [ prettyNodeId visit.nodeId
    ] ++
    [ printf "%P=%s" addr (visitCountName vc)
    | (addr, vc) <- M.toList visit.restrs
    ]

visitCountName :: VisitCount -> String
visitCountName (VisitCount { numbers, offsets }) =
    intercalate "_" $ map showNumber numbers ++ map showOffset offsets
  where
    showNumber = show
    showOffset n = "i+" ++ show n

inductVarName :: EqHypInduct -> NameHint
inductVarName induct = printf "induct_i_%d_%d" induct.n1 induct.n2

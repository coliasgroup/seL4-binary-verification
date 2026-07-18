module BV.Core.GraphSlice.New.Flatten.NameHint
    ( condName
    , inductVarName
    , initName
    , localName
    , localNameAfterLoop
    , localNameBefore
    , pathCondName
    , pathCondNameAfterLoop
    , successName
    ) where

import BV.Core.GraphSlice.New.Flat (NameHint)
import BV.Core.GraphSlice.New.Flatten.Visit (NormalizedVisit,
                                             unwrapNormalizedVisit)

import BV.Core.Types

import Data.Char (isAlpha)
import Data.List (intercalate, tails)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Optics
import Text.Printf (printf)

initName :: Tag t => t -> Ident -> NameHint
initName tag var = printf "%P_init_%P" var tag

localNameBefore :: Tag t => NormalizedVisit t -> Ident -> NameHint
localNameBefore norm var = printf "%P_v_at_%s" var (visitName norm)

localName :: Tag t => NormalizedVisit t -> Ident -> NameHint
localName norm var = printf "%P_after_%s" var (visitName norm)

localNameAfterLoop :: Tag t => NormalizedVisit t -> Ident -> NameHint
localNameAfterLoop norm var = printf "%P_after_loop_at_%s" var (visitName norm)

condName :: Tag t => NormalizedVisit t -> NameHint
condName norm = printf "cond_at_%s" (visitName norm)

pathCondName :: Tag t => NormalizedVisit t -> NameHint
pathCondName norm = printf "path_cond_to_%s" (visitName norm)

pathCondNameAfterLoop :: Tag t => NormalizedVisit t -> NameHint
pathCondNameAfterLoop norm = printf "pc_of_loop_at_%s" (visitName norm)

successName :: Tag t => NormalizedVisit t -> Ident -> NameHint
successName norm fname = printf "%s_success_at_%s" (sanitizeFunName fname) (visitName norm)

-- TODO clean up
sanitizeFunName :: Ident -> String
sanitizeFunName fname = case unsnoc names of
    Nothing -> "fun"
    Just (_, name') -> name'
  where
    names =
        [ intercalate "_" suffix
        | suffix@(bit:_) <- filter (not . null) $ tails bits
        , all isAlpha bit
        ]
    bits = splitOn "." fname.unwrap

visitName :: Tag t => NormalizedVisit t -> NameHint
visitName norm = intercalate "_" $
    [ prettyNodeId visit.nodeId
    ] ++
    [ printf "%P=%s" addr (visitCountName vc)
    | (addr, vc) <- M.toList visit.restrs
    ] ++
    [ prettyTag visit.tag
    ]
  where
    visit = unwrapNormalizedVisit norm

visitCountName :: VisitCount -> String
visitCountName (VisitCount { numbers, offsets }) =
    intercalate "_" $ map showNumber numbers ++ map showOffset offsets
  where
    showNumber = show
    showOffset n = "i+" ++ show n

inductVarName :: EqHypInduct -> NameHint
inductVarName induct = printf "induct_i_%d_%d" induct.n1 induct.n2

module BV.Core.Stages.CompileProofChecks.Grouping
    ( ProofCheckGroup
    , proofCheckGroups
    ) where

import BV.Core.Types

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

type ProofCheckGroup a = [ProofCheck a]

proofCheckGroups :: [ProofCheck a] -> [ProofCheckGroup a]
proofCheckGroups = toList . proofCheckGroupsWithKeys

proofCheckGroupsWithKeys :: [ProofCheck a] -> Map CheckGroupKey (ProofCheckGroup a)
proofCheckGroupsWithKeys checks = M.unionsWith (<>)
    [ M.singleton (compatOrdKey (groupKeyOf check)) [check]
    | check <- checks
    ]

newtype CheckGroupKey
  = CheckGroupKey { unwrap :: [((String, [(Integer, ([Integer], [Integer]))]), String)] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

groupKeyOf :: ProofCheck a -> Set VisitWithTag
groupKeyOf check = S.fromList (check ^.. checkVisits)

compatOrdKey :: Set VisitWithTag -> CheckGroupKey
compatOrdKey visits = CheckGroupKey $ sort
    [ ((prettyNodeId visit.nodeId, []), prettyTag tag)
    | VisitWithTag visit tag <- S.toList visits
    ]

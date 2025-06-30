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
    [ M.singleton (compatKey (groupKey check)) [check]
    | check <- checks
    ]

newtype CheckGroupKey
  = CheckGroupKey { unwrap :: [((String, [(Integer, ([Integer], [Integer]))]), String)] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

groupKey :: ProofCheck a -> Set VisitWithTag
groupKey check = S.fromList (check ^.. checkVisits)

compatKey :: Set VisitWithTag -> CheckGroupKey
compatKey visits = CheckGroupKey $ sort
    [ ((prettyNodeId visit.nodeId, map compatRestr visit.restrs), prettyTag tag)
    | VisitWithTag visit tag <- S.toList visits
    ]

compatRestr :: Restr -> (Integer, ([Integer], [Integer]))
compatRestr restr = (restr.nodeAddr.unwrap, compatVisitCount restr.visitCount)

compatVisitCount :: VisitCount -> ([Integer], [Integer])
compatVisitCount vc = (vc.numbers, vc.offsets)

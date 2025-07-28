{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.DecorateProofScript
    ( ProofScriptNodePath (..)
    , decorateProofScriptWithProofScriptNodePathsWith
    , prettyProofScriptNodePath
    , proofScriptEdgePath
    ) where

import BV.Core.Types
import BV.Core.Utils

import Control.DeepSeq (NFData)
import Data.Foldable (fold)
import Data.Functor (void)
import GHC.Generics (Generic)
import Optics
import Text.Printf (printf)

data ProofScriptNodePath t
  = ProofScriptNodePath
      { unwrap :: ProofNodeWith t (Maybe (ProofScriptNodePath t, ProofNodeEdge))
      }
  deriving (Eq, Generic, NFData, Ord, Show)

newtype ProofNodeEdge
  = ProofNodeEdge { unwrap :: Integer }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

proofScriptNodeEdgePath :: ProofScriptNodePath t -> [(ProofScriptNodePath t, ProofNodeEdge)]
proofScriptNodeEdgePath = reverse . go
  where
    go path = case path.unwrap.meta of
        Nothing -> []
        Just (path', edge) -> (path', edge) : go path'

proofScriptEdgePath :: ProofScriptNodePath t -> [ProofNodeEdge]
proofScriptEdgePath = map snd . proofScriptNodeEdgePath

traverseChildren :: IxTraversal ProofNodeEdge (ProofNode t a) (ProofNode t b) (ProofNodeWith t a) (ProofNodeWith t b)
traverseChildren = itraversalVL $ \f ->
    let g = f . ProofNodeEdge
     in \case
            ProofNodeLeaf -> pure ProofNodeLeaf
            ProofNodeRestr node -> ProofNodeRestr <$> traverseRestrProofNodeChild (g 0) node
            ProofNodeCaseSplit node -> ProofNodeCaseSplit <$> traverseCaseSplitProofNodeChildren (g 0) (g 1) node
            ProofNodeSplit node -> ProofNodeSplit <$> traverseSplitProofNodeChildren (g 0) (g 1) node
            ProofNodeSingleRevInduct node -> ProofNodeSingleRevInduct <$> traverseSingleRevInductProofNodeChild (g 0) node

getChild :: ProofNodeEdge -> ProofNode t a -> ProofNodeWith t a
getChild (ProofNodeEdge i) = \case
    ProofNodeLeaf -> case i of
    ProofNodeRestr node -> case i of
        0 -> node.child
    ProofNodeCaseSplit node -> case i of
        0 -> node.left
        1 -> node.right
    ProofNodeSplit node -> case i of
        0 -> node.p1
        1 -> node.p2
    ProofNodeSingleRevInduct node -> case i of
        0 -> node.child

zipProofNodes :: RefineTag t => ProofNodeWith t (a -> b) -> ProofNodeWith t a -> ProofNodeWith t b
zipProofNodes pf pa = ensure (void pf == void pa) go pf pa
  where
    go (ProofNodeWith f nf) (ProofNodeWith a na) =
        ProofNodeWith (f a) $ iover traverseChildren (\edge child -> zipProofNodes child (getChild edge na)) nf

zipProofScripts :: RefineTag t => ProofScript t (a -> b) -> ProofScript t a -> ProofScript t b
zipProofScripts (ProofScript f) (ProofScript a) = ProofScript (zipProofNodes f a)

selfInMeta :: ProofScript t a -> ProofScript t (ProofNodeWith t a)
selfInMeta = #root %~ go
  where
    go nodeWith = ProofNodeWith nodeWith $ over traverseChildren go nodeWith.node

pathInMeta :: ProofScript t a -> ProofScript t (ProofScriptNodePath t)
pathInMeta script = ProofScriptNodePath <$> selfInMeta (pathInMetaHelper script)

pathInMetaHelper :: ProofScript t a -> ProofScript t (Maybe (ProofScriptNodePath t, ProofNodeEdge))
pathInMetaHelper = #root %~ go Nothing
  where
    go :: Maybe (ProofScriptNodePath t, ProofNodeEdge) -> ProofNodeWith t a -> ProofNodeWith t (Maybe (ProofScriptNodePath t, ProofNodeEdge))
    go toHere (ProofNodeWith _ node) =
        let here = ProofNodeWith toHere $ iover traverseChildren (\edge -> go (Just (ProofScriptNodePath here, edge))) node
         in here

decorateProofScriptWithProofScriptNodePathsWith
    :: RefineTag t => (ProofScriptNodePath t -> a -> b) -> ProofScript t a -> ProofScript t b
decorateProofScriptWithProofScriptNodePathsWith f script =
    zipProofScripts (f <$> pathInMeta script) script

--

prettyProofScriptNodePath :: RefineTag t => ProofScriptNodePath t -> String
prettyProofScriptNodePath path =
    fold
        [ let arrow = case briefEdge node edge of
                Just label -> "-" ++ label ++ "->"
                Nothing -> "->"
           in briefNode node ++ " " ++ arrow ++ " "
        | (ProofScriptNodePath (ProofNodeWith _ node), edge) <- proofScriptNodeEdgePath path
        ] ++ briefNode path.unwrap.node

briefNode :: RefineTag t => ProofNode t a -> String
briefNode = \case
    ProofNodeLeaf ->
        "leaf"
    ProofNodeRestr node ->
        printf "restr %s (%s)" (show node.point.unwrap) (prettyTag node.tag)
    ProofNodeCaseSplit node ->
        printf "case split %s (%s)" (show node.addr.unwrap) (prettyTag node.tag)
    ProofNodeSplit node ->
        let s = withTags node.details <&> \(WithTag tag details) ->
                    printf "%s (%s)" (show details.split.unwrap) (prettyTag tag) :: String
         in printf "split %s and %s" (getRight s) (getLeft s)
    ProofNodeSingleRevInduct node ->
        printf "restr %s (%s)" (show node.point.unwrap) (prettyTag node.tag)

briefEdge :: ProofNode t a -> ProofNodeEdge -> Maybe String
briefEdge node (ProofNodeEdge i) = case node of
    ProofNodeLeaf -> case i of
    ProofNodeRestr _ -> case i of
        0 -> Nothing
    ProofNodeCaseSplit _ -> case i of
        0 -> Just "l"
        1 -> Just "r"
    ProofNodeSplit _ -> case i of
        0 -> Just "1"
        1 -> Just "2"
    ProofNodeSingleRevInduct _ -> case i of
        0 -> Nothing

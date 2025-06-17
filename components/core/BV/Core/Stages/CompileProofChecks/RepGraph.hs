{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BV.Core.Stages.CompileProofChecks.RepGraph
    ( FunctionSignature (..)
    , FunctionSignatures
    , MonadRepGraph (..)
    , RepGraphContext
    , RepGraphEnv
    , RepGraphState
    , getInductVarM
    , getNodePcEnvM
    , getNodePcEnvM'
    , getPcM
    , getPcM'
    , initRepGraphEnv
    , initRepGraphState
    , instEqWithEnvsM
    , substInduct
    ) where

import BV.Core.Graph
import BV.Core.Logic
import BV.Core.Stages.CompileProofChecks.Solver
import BV.Core.Types

import BV.Core.Types.Extras.Expr
import BV.Core.Types.Extras.ProofCheck
import BV.Core.Utils
import Control.DeepSeq (NFData)
import Control.Monad (filterM, guard, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (MonadReader)
import Control.Monad.RWS (MonadState (get, put), MonadWriter (..),
                          RWST (runRWST), evalRWST)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (for_)
import Data.List (sort)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))
import Text.Printf (printf)

type RepGraphContext m = (MonadReader RepGraphEnv m, MonadState RepGraphState m)

class MonadSolver m => MonadRepGraph m where
    liftRepGraph :: (forall n. RepGraphContext n => n a) -> m a

instance MonadRepGraph m => MonadRepGraph (ExceptT e m) where
    liftRepGraph f = lift $ liftRepGraph f

data RepGraphEnv
  = RepGraphEnv
      { functionSigs :: FunctionSignatures
      , pairings :: Pairings
      , problem :: Problem
      , nodeTag :: NodeAddr -> Tag
      , loopData :: Map NodeAddr LoopData
      , nodeGraph :: NodeGraph
      , preds :: Map NodeId (Set NodeAddr)
      }
  deriving (Generic)

data RepGraphState
  = RepGraphState
      { inductVarEnv :: Map EqHypInduct Name
      , nodePcEnvs :: Map VisitWithTag (Maybe (Expr, SMTEnv))
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data FunctionSignature
  = FunctionSignature
      { input :: [Argument]
      , output :: [Argument]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

type FunctionSignatures = WithTag Ident -> FunctionSignature

data LoopData
  = LoopHead (Set NodeAddr)
  | LoopMember NodeAddr
  deriving (Eq, Generic, Ord, Show)

initRepGraphEnv :: FunctionSignatures -> Pairings -> Problem -> RepGraphEnv
initRepGraphEnv functionSigs pairings problem =
    RepGraphEnv
        { functionSigs
        , pairings
        , problem
        -- , nodeGraph = makeNodeGraph (map (_2 %~ view #node) (M.toAscList problem.nodes))
        , nodeGraph
        , nodeTag =
            let c = S.fromList . mapMaybe (preview #_Addr) $ reachableFrom nodeGraph problem.sides.c.entryPoint
             in \addr -> if addr `S.member` c then C else Asm
        , loopData =
            let heads = loopHeads nodeGraph [problem.sides.c.entryPoint, problem.sides.asm.entryPoint]
             in M.fromList $ flip foldMap heads $ \(loopHead, scc) ->
                    [(loopHead, LoopHead scc)] <> flip mapMaybe (S.toList scc) (\member ->
                        if member == loopHead then Nothing else Just (member, LoopMember loopHead))
        , preds = M.fromListWith (<>) $ concat
            [ [ (cont, S.singleton nodeAddr)
              | cont <- node ^.. nodeConts
              ]
            | (nodeAddr, node) <- M.toAscList problem.nodes
            ]
        }
  where
    nodeGraph = makeNodeGraph (M.toAscList problem.nodes)

loopIdR :: MonadRepGraph m => NodeAddr -> m (Maybe NodeAddr)
loopIdR addr = liftRepGraph $ do
    loopData <- gview $ #loopData % at addr
    return (loopData <&> \case
        LoopHead _ -> addr
        LoopMember addr' -> addr')

loopHeadsR :: MonadRepGraph m => m [NodeAddr]
loopHeadsR = liftRepGraph $ do
    loopData <- gview #loopData
    return (mapMaybe (\(k, v) -> case v of
        LoopHead _ -> Just k
        LoopMember _ -> Nothing) (M.toList loopData))

nodeTagR :: MonadRepGraph m => NodeAddr -> m Tag
nodeTagR n = liftRepGraph $ do
    gview (#nodeTag % to ($ n))

getReachableR :: MonadRepGraph m => NodeAddr -> NodeId -> m Bool
getReachableR split n = do
    g <- liftRepGraph $ gview #nodeGraph
    return $ isReachableFrom g (Addr split) n

predsR :: MonadRepGraph m => NodeId -> m (Set NodeAddr)
predsR n = liftRepGraph $ gview $ #preds % at n % unwrapped

prevsR :: MonadRepGraph m => Visit -> m [Visit]
prevsR visit = do
    let m = vcountToMap visit.restrs
    preds <- S.toAscList <$> predsR visit.nodeId
    let f p = Visit (Addr p) <$>
            if M.member p m
            then incrVCs visit.restrs p (-1)
            else Just visit.restrs
    return $ catMaybes $ map f preds

initRepGraphState :: RepGraphState
initRepGraphState = RepGraphState
    { inductVarEnv = M.empty
    , nodePcEnvs = M.empty
    }

--

type MonadRepGraphE m = (MonadRepGraph m, MonadError TooGeneral m)

data TooGeneral
  = TooGeneral
      { split :: NodeAddr
      }
  deriving (Eq, Generic, Ord, Show)

getPcM' :: MonadRepGraph m => Visit -> Maybe Tag -> m Expr
getPcM' visit tag = view (expecting _Right) <$> runExceptT (getPcM visit tag)

getPcM :: MonadRepGraphE m => Visit -> Maybe Tag -> m Expr
getPcM visit tag = do
    pc_env <- getNodePcEnvM visit tag
    let Just (pc, env) = pc_env
    withEnv env $ toSmtExprM pc

toSmtExprRM :: MonadRepGraphE m => Expr -> Visit -> Maybe Tag -> m Expr
toSmtExprRM expr visit tag = do
    pc_env <- getNodePcEnvM visit tag
    let Just (_pc, env) = pc_env
    withEnv env $ toSmtExprM expr

getNodePcEnvM' :: MonadRepGraph m => Visit -> Maybe Tag -> m (Maybe (Expr, SMTEnv))
getNodePcEnvM' visit tag = view (expecting _Right) <$> runExceptT (getNodePcEnvM visit tag)

getNodePcEnvM :: MonadRepGraphE m => Visit -> Maybe Tag -> m (Maybe (Expr, SMTEnv))
getNodePcEnvM visit tag = do
    (tag', vcount) <- getTagVCount visit tag
    case vcount of
        Nothing -> return Nothing
        Just vcount' -> do
            let vt = VisitWithTag
                    { visit = Visit
                        { nodeId = visit.nodeId
                        , restrs = vcount'
                        }
                    , tag = tag'
                    }
            liftRepGraph (use (#nodePcEnvs % at vt)) >>= \case
                Just ret -> return ret
                Nothing -> do
                    warmPcEnvCacheM vt
                    pc_env <- getNodePcEnvRawM vt
                    pc_env' <- for pc_env $ \pc_env'' -> do
                        applyKnownEqsPcEnvM vt pc_env''
                    present <- liftRepGraph $ use $ #nodePcEnvs % to (M.member vt)
                    ensureM $ not present
                    liftRepGraph $ #nodePcEnvs %= M.insert vt pc_env'
                    return pc_env

type VCount = [Restr]

vcountToMap :: [Restr] -> Map NodeAddr VisitCount
vcountToMap restrs = ensure check m
  where
    m = M.fromList [ (restr.nodeAddr, restr.visitCount) | restr <- restrs ]
    check = M.size m == length restrs

vcountFromMap :: Map NodeAddr VisitCount -> [Restr]
vcountFromMap = map f . M.toAscList
  where
    f (nodeAddr, visitCount) = Restr { nodeAddr, visitCount }

incrVCs :: VCount -> NodeAddr -> Integer -> Maybe VCount
incrVCs vcount n incr = if isEmptyVC vc then Nothing else Just (vcountFromMap (M.insert n vc m))
  where
    m = vcountToMap vcount
    vc = incrVC incr (m ! n)

warmPcEnvCacheM :: MonadRepGraph m => VisitWithTag -> m ()
warmPcEnvCacheM visitWithTag = do
    let go = do
            n_vc <- get
            prevs <- lift $ prevsR n_vc
            let f p = do
                    present <- liftRepGraph $ use $ #nodePcEnvs % to (M.member (VisitWithTag p visitWithTag.tag))
                    if present
                        then return False
                        else do
                            vc <- getTagVCount p Nothing
                            return $ vc == (visitWithTag.tag, Just n_vc.restrs)
            prevs' <- lift $ runExceptT $ filterM f prevs
            case prevs' of
                Left (n_vc':_) -> do
                    tell [n_vc]
                    put n_vc'
                    undefined
                _ -> undefined
    ((), prevChain :: [Visit]) <- evalRWST go () visitWithTag.visit
    undefined

getNodePcEnvRawM :: MonadRepGraphE m => VisitWithTag -> m (Maybe (Expr, SMTEnv))
getNodePcEnvRawM = undefined

applyKnownEqsPcEnvM :: MonadRepGraphE m => VisitWithTag -> (Expr, SMTEnv) -> m (Expr, SMTEnv)
applyKnownEqsPcEnvM = undefined

getTagVCount :: MonadRepGraphE m => Visit -> Maybe Tag -> m (Tag, Maybe [Restr])
getTagVCount visit mtag = do
    tag <- maybe (nodeTagR (visit.nodeId ^. expecting #_Addr)) return mtag
    vcount_r <- catMaybes <$> for visit.restrs (\restr -> runMaybeT $ do
        reachable <- lift $ getReachableR restr.nodeAddr visit.nodeId
        tag' <- lift $ nodeTagR restr.nodeAddr
        guard $ tag' == tag
        return $ (restr.nodeAddr, restr.visitCount, reachable)
        )
    let done = flip any vcount_r $ \(_split, count, r) -> not r && not (hasZeroVC count)
    if done
        then return (tag, Nothing)
        else do
            let vcount = sort [ (split, count) | (split, count, r) <- vcount_r, r ]
            maybeLoopId <- loopIdR (visit.nodeId ^. expecting #_Addr)
            case maybeLoopId of
                Nothing -> return ()
                Just loopId -> for_ vcount $ \(split, visits) -> do
                    maybeLoopId' <- loopIdR split
                    when (maybeLoopId' == Just loopId && isOptionsVC visits) $ do
                        throwError $ TooGeneral { split }
            return (tag, Just visit.restrs)

getInductVarM :: MonadRepGraph m => EqHypInduct -> m Expr
getInductVarM induct = do
    vname <- liftRepGraph (use (#inductVarEnv % at induct)) >>= \case
        Just vname -> return vname
        Nothing -> do
            vname <- addVarM (printf "induct_i_%d_%d" induct.a induct.b) word32T
            liftRepGraph $ #inductVarEnv %= M.insert induct (vname)
            return vname
    return $ smtExprE word32T (SMT $ nameS vname)

substInduct :: Expr -> Expr -> Expr
substInduct expr inductVar = flip varSubstNotMust expr $ \ident ty ->
    if ident.unwrap == "%n" && ty == word32T
    then Just inductVar
    else Nothing

toSmtExprUnderOpM :: MonadSolver m => Expr -> ReaderT SMTEnv m Expr
toSmtExprUnderOpM expr = case expr.value of
    ExprValueOp op args -> do
        args' <- mapM toSmtExprM args
        return $ expr & #value .~ ExprValueOp op args'
    _ -> toSmtExprM expr

instEqWithEnvsM :: MonadSolver m => (Expr, SMTEnv) -> (Expr, SMTEnv) -> m Expr
instEqWithEnvsM (x, env1) (y, env2) = do
    x' <- withEnv env1 $ toSmtExprUnderOpM x
    y' <- withEnv env2 $ toSmtExprUnderOpM y
    return $ case x'.ty of
        ExprTypeRelWrapper -> applyRelWrapper x' y'
        _ -> eqE x' y'

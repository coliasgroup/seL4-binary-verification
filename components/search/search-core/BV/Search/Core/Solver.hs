{-# LANGUAGE MultiWayIf #-}

module BV.Search.Core.Solver
    ( Cache
    , Model
    , MonadGraphSliceSolverInteract
    , GraphSliceSolverFailureReason (..)
    , GraphSliceSolverInteractParallel
    , GraphSliceSolverInteractParallelFailureInfo (..)
    , GraphSliceSolverInteractSimple
    , GraphSliceSolverInteractSimpleFailureInfo (..)
    , TestResultWith (..)
    , emptyCache
      -- , evalModelExpr
    , runGraphSliceSolverInteractParallel
    , runGraphSliceSolverInteractSimple
    , testHyp
    , testHypGetModel
    , testHypWhyps
    , testHypWhypsGetModel
    , testHypWhypsWithCache
    , testHypWhypsWithCacheFast
    ) where

import BV.Core.ExecuteSMTProofChecks (defaultLogic, splitHyp)
import BV.Core.ModelConfig
import BV.Core.GraphSlice.Old
import BV.Core.GraphSlice.Old.InterpretHyp
import BV.Core.Types
import BV.Core.Types.Extras.SExprWithPlaceholders (andNS, notS)
import BV.Logging
import BV.SMTLIB2.Command
import BV.SMTLIB2.Monad
import BV.SMTLIB2.SExpr
import BV.Utils

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError,
                             withExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT (StateT), evalStateT, modify)
import Control.Monad.Trans (lift)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))

-- TODO
-- Use an abstraction that allows for caching sat results

data TestResultWith a
  = TestResultWithTrue
  | TestResultWithFalse
      { whenFalse :: a
      }
  deriving (Foldable, Functor, Generic, Traversable)

isTrueResult :: TestResultWith a -> Bool
isTrueResult = \case
    TestResultWithTrue -> True
    TestResultWithFalse _ -> False

fromResult :: a -> Bool -> TestResultWith a
fromResult a = \case
    True -> TestResultWithTrue
    False -> TestResultWithFalse a

ensureNothing :: TestResultWith (Maybe a) -> TestResultWith (Maybe b)
ensureNothing = over (traversed % _Just) (error "expected Nothing")

ensureModel :: TestResultWith (Maybe a) -> TestResultWith a
ensureModel = fmap fromJust

ensureNoModel :: TestResultWith (Maybe a) -> TestResultWith ()
ensureNoModel = fmap $ \opt -> ensure (isNothing opt) ()

newtype Cache
  = Cache { unwrap :: M.Map SExprWithPlaceholders Bool }
  deriving (Generic)

emptyCache :: Cache
emptyCache = Cache  M.empty

withCache :: Monad m => StateT (Maybe Cache) m a -> StateT Cache m a
withCache (StateT f) = StateT $ \cache -> over _2 fromJust <$> f (Just cache)

withoutCache :: Monad m => StateT (Maybe Cache) m a -> m a
withoutCache (StateT f) = fst <$> f Nothing

--

newtype Model
  = Model { unwrap :: M.Map SExprWithPlaceholders GraphExpr }
  deriving (Generic, Show)

type LowLevelModelRequest = [String]

type LowLevelModel = [SExpr]

-- reconstructModel
--     :: M.Map SExprWithPlaceholders (Name, ExprType)
--     -> LowLevelModelRequest
--     -> LowLevelModel
--     -> Model
-- reconstructModel exprs req vals =
--     Model $ M.mapKeys (symbolS . (.unwrap)) assocs <> abbrevs
--   where
--     assocs = M.fromList $ catMaybes $ zipWith (\name val -> (,) name <$> smtToVal val) req vals
--     abbrevs = (assocs M.!) <$> M.filter (`M.member` assocs) (fst <$> exprs)

-- smtToVal :: SExpr -> Maybe GraphExpr
-- smtToVal sexpr = case viewSExpr sexpr of
--     Atom (SymbolAtom "true") -> Just trueE
--     Atom (SymbolAtom "false") -> Just falseE
--     Atom (HexadecimalAtom s) ->
--         Just $ numE (wordT todo) (readS readHex s)
--     Atom (BinaryAtom s) ->
--         Just $ numE (wordT todo) (readS readBin s)
--     List [Atom (SymbolAtom "_"), Atom (SymbolAtom ('b':'v':bits)), Atom (NumeralAtom n)] ->
--         Just $ numE (wordT (read bits)) (toInteger n)
--     _ -> Nothing
--   where
--     readS p s = case filter (null . snd) (p s) of
--         [(a, "")] -> a
--         _ -> error "parse failure"

-- evalModelExpr :: (Tag t, MonadGraphSliceSendSExpr m) => GraphExpr -> StateT Model (GraphSliceT t m) GraphExpr
-- evalModelExpr _expr = do
--     -- sexpr <- convertSolverExpr expr
--     evalModel undefined

-- evalModel :: Monad m => SExprWithPlaceholders -> StateT Model m GraphExpr
-- evalModel = go
--   where
--     go sexpr = maybe (complex sexpr) return (trySimple sexpr)
--     trySimple sexpr = smtToVal =<< traverse (preview #_AtomOrPlaceholderAtom) sexpr
--     complex sexpr = do
--         expr <- case sexpr of
--             List [op, cond, x, y] | op == symbolS "ite" -> do
--                 cond' <- go cond
--                 if | cond' == trueE -> go x
--                    | cond' == falseE -> go y
--                    | otherwise -> undefined
--             List (_op:args) -> do
--                 _args' <- traverse go args
--                 todo
--             _ -> undefined
--         #unwrap %= M.insert sexpr expr
--         return expr

--

class MonadGraphSliceSendSExpr m => MonadGraphSliceSolverInteract m where
    checkHyp :: Maybe LowLevelModelRequest -> SExprWithPlaceholders -> m (TestResultWith (Maybe LowLevelModel))

instance MonadGraphSliceSolverInteract m => MonadGraphSliceSolverInteract (ReaderT r m) where
    checkHyp = compose2 lift checkHyp

instance MonadGraphSliceSolverInteract m => MonadGraphSliceSolverInteract (ExceptT e m) where
    checkHyp = compose2 lift checkHyp

-- instance MonadGraphSliceSolverInteract m => MonadGraphSliceSolverInteract (GraphSliceT t m) where
--     checkHyp = compose2 lift checkHyp

-- instance MonadGraphSliceSolverInteract m => MonadGraphSliceSolverInteract (GraphSliceTaggedT t m) where
--     checkHyp = compose2 lift checkHyp

testHypCommon :: (MonadGraphSliceSolverInteract m) => Bool -> SExprWithPlaceholders -> m (TestResultWith (Maybe Model))
testHypCommon wantModel sexpr = case wantModel of
    False -> ensureNothing <$> checkHyp Nothing sexpr
    True -> do
        undefined
        -- vars <- getModelVars
        -- exprs <- getModelExprs
        -- let req = S.toList vars ++ map fst (toList exprs)
        -- r <- checkHyp (Just req) sexpr
        -- return $ over (#_TestResultWithFalse % _Just) (reconstructModel exprs req) r

testHyp :: (Tag t, MonadGraphSliceSolverInteract m) => SExprWithPlaceholders -> GraphSliceT t m Bool
testHyp hyp = lift $ isTrueResult . ensureNoModel <$> testHypCommon False hyp

testHypGetModel :: (Tag t, MonadGraphSliceSolverInteract m) => SExprWithPlaceholders -> GraphSliceT t m (TestResultWith Model)
testHypGetModel hyp = lift $ ensureModel <$> testHypCommon True hyp

testHypWhypsCommon
    :: (RefineTag t, MonadGraphSliceSolverInteract m)
    => Bool -> Bool -> FlatExpr -> [Hyp t] -> StateT (Maybe Cache) (GraphSliceT t m) (Maybe (TestResultWith (Maybe Model)))
testHypWhypsCommon fast wantModel hyp hyps = do
    sexpr <- lift $ interpretHypImps hyps hyp >>= convertExpr
    cacheEntry <- preuse $ #_Just % #unwrap % at sexpr % #_Just
    case (cacheEntry, fast) of
        (Just v, _) -> do
            ensureM $ not wantModel
            return $ Just $ fromResult Nothing v
        (Nothing, True) -> do
            return Nothing
        (Nothing, False) -> do
            lift addPValidDomAssertions
            v <- lift $ lift $ testHypCommon wantModel sexpr
            zoomMaybe (#_Just % #unwrap) $ modify $ M.insertWith undefined sexpr (isTrueResult v)
            return $ Just v

testHypWhyps
    :: (RefineTag t, MonadGraphSliceSolverInteract m)
    => FlatExpr -> [Hyp t] -> GraphSliceT t m Bool
testHypWhyps hyp hyps =
    isTrueResult . ensureNoModel . fromJust <$>
        withoutCache (testHypWhypsCommon False False hyp hyps)

testHypWhypsGetModel
    :: (RefineTag t, MonadGraphSliceSolverInteract m)
    => FlatExpr -> [Hyp t] -> GraphSliceT t m (TestResultWith Model)
testHypWhypsGetModel hyp hyps =
    ensureModel . fromJust <$>
        withoutCache (testHypWhypsCommon False True hyp hyps)

testHypWhypsWithCache
    :: (RefineTag t, MonadGraphSliceSolverInteract m)
    => FlatExpr -> [Hyp t] -> StateT Cache (GraphSliceT t m) Bool
testHypWhypsWithCache hyp hyps =
    isTrueResult . ensureNoModel . fromJust <$>
        withCache (testHypWhypsCommon False False hyp hyps)

testHypWhypsWithCacheFast
    :: (RefineTag t, MonadGraphSliceSolverInteract m)
    => FlatExpr -> [Hyp t] -> StateT Cache (GraphSliceT t m) (Maybe Bool)
testHypWhypsWithCacheFast hyp hyps =
    fmap (isTrueResult . ensureNoModel) <$>
        withCache (testHypWhypsCommon True False hyp hyps)

--

data GraphSliceSolverFailureReason
  = GraphSliceSolverTimedOut
  | GraphSliceSolverAnsweredUnknown SExpr
  deriving (Eq, Generic, Ord, Show)

checkHypInner
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout
    -> ModelConfig
    -> Maybe LowLevelModelRequest
    -> SExprWithPlaceholders
    -> m (Either GraphSliceSolverFailureReason (TestResultWith (Maybe LowLevelModel)))
checkHypInner timeout modelConfig modelRequestOpt hyp = runExceptT $ do
    let sendAssert = sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr modelConfig
    let hyps = splitHyp (notS hyp)
    sendSimpleCommandExpectingSuccess $ Push 1
    traverse_ sendAssert hyps
    r <- checkSatWithTimeout timeout >>= \case
        Nothing -> throwError GraphSliceSolverTimedOut
        Just (Unknown reason) -> throwError (GraphSliceSolverAnsweredUnknown reason)
        Just Unsat -> return TestResultWithTrue
        Just Sat -> fmap TestResultWithFalse $ for modelRequestOpt $ \modelRequest -> do
            getValue $ map (Atom . symbolAtom) modelRequest
    sendSimpleCommandExpectingSuccess $ Pop 1
    when (isTrueResult r) $ do
        sendAssert $ notS (andNS hyps)
    return r

commonSolverSetup
    :: (MonadSolver m, MonadThrow m)
    => ModelConfig
    -> m ()
commonSolverSetup modelConfig = do
    sendSimpleCommandExpectingSuccess $ SetOption (PrintSuccessOption True)
    sendSimpleCommandExpectingSuccess $ SetOption (ProduceModelsOption True)
    sendSimpleCommandExpectingSuccess $ SetLogic defaultLogic
    traverse_ sendExpectingSuccess (modelConfigPreamble modelConfig)

--

newtype GraphSliceSolverInteractSimple m a
  = GraphSliceSolverInteractSimple { run :: ExceptT GraphSliceSolverInteractSimpleFailureInfo (ReaderT SimpleEnv m) a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadLogger
    , MonadLoggerWithContext
    )

data SimpleEnv
  = SimpleEnv
      { timeout :: Maybe SolverTimeout
      , modelConfig :: ModelConfig
      }
  deriving (Generic)

instance (MonadSolver m, MonadThrow m) => MonadGraphSliceSendSExpr (GraphSliceSolverInteractSimple m) where
    sendSExpr s = GraphSliceSolverInteractSimple $ do
        modelConfig <- gview #modelConfig
        sendExpectingSuccess $ configureSExpr modelConfig s

instance (MonadSolver m, MonadThrow m) => MonadGraphSliceSolverInteract (GraphSliceSolverInteractSimple m) where
    checkHyp modelRequestOpt hyp = GraphSliceSolverInteractSimple $ do
        timeout <- gview #timeout
        modelConfig <- gview #modelConfig
        withExceptT GraphSliceSolverInteractSimpleFailureInfo $
            ExceptT $ checkHypInner timeout modelConfig modelRequestOpt hyp

data GraphSliceSolverInteractSimpleFailureInfo
  = GraphSliceSolverInteractSimpleFailureInfo
      { reason :: GraphSliceSolverFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

runGraphSliceSolverInteractSimple
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout -> ModelConfig -> GraphSliceSolverInteractSimple m a -> m (Either GraphSliceSolverInteractSimpleFailureInfo a)
runGraphSliceSolverInteractSimple timeout modelConfig m = do
    commonSolverSetup modelConfig
    runReaderT (runExceptT m.run) env
  where
    env = SimpleEnv
        { timeout
        , modelConfig
        }

--

-- TODO

type GraphSliceSolverInteractParallelInner m =
    ExceptT GraphSliceSolverInteractParallelFailureInfo (StateT ParallelState (ReaderT (ParallelEnv m) m))

newtype GraphSliceSolverInteractParallel m a
  = GraphSliceSolverInteractParallel { run :: GraphSliceSolverInteractParallelInner m a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadLogger
    , MonadLoggerWithContext
    )

data ParallelState
  = ParallelState
      { setup :: [SExprWithPlaceholders]
      }
  deriving (Generic)

data ParallelEnv m
  = ParallelEnv
      { timeout :: Maybe SolverTimeout
      , modelConfig :: ModelConfig
      , runParallel :: RunParallel m
      }
  deriving (Generic)

type RunParallel m = m ()

instance (MonadSolver m, MonadThrow m) => MonadGraphSliceSendSExpr (GraphSliceSolverInteractParallel m) where
    sendSExpr s = GraphSliceSolverInteractParallel $ do
        #setup %= (++ [s])
        modelConfig <- gview #modelConfig
        sendExpectingSuccess $ configureSExpr modelConfig s

instance (MonadSolver m, MonadThrow m) => MonadGraphSliceSolverInteract (GraphSliceSolverInteractParallel m) where
    checkHyp modelRequestOpt hyp = GraphSliceSolverInteractParallel $ do
        timeout <- gview #timeout
        modelConfig <- gview #modelConfig
        withExceptT GraphSliceSolverInteractParallelFailureInfo $
            ExceptT $ checkHypInner timeout modelConfig modelRequestOpt hyp

data GraphSliceSolverInteractParallelFailureInfo
  = GraphSliceSolverInteractParallelFailureInfo
      { reason :: GraphSliceSolverFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

runGraphSliceSolverInteractParallel
    :: (MonadSolver m, MonadThrow m)
    => RunParallel m -> Maybe SolverTimeout -> ModelConfig -> GraphSliceSolverInteractParallel m a -> m (Either GraphSliceSolverInteractParallelFailureInfo a)
runGraphSliceSolverInteractParallel runParallel timeout modelConfig m = do
    commonSolverSetup modelConfig
    runReaderT (evalStateT (runExceptT m.run) initState) env
  where
    env = ParallelEnv
        { timeout
        , modelConfig
        , runParallel
        }
    initState = ParallelState
        { setup = []
        }

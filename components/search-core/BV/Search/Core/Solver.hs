{-# LANGUAGE MultiWayIf #-}

module BV.Search.Core.Solver
    ( Cache
    , Model
    , MonadRepGraphSolverInteract
    , RepGraphSolverFailureReason (..)
    , RepGraphSolverInteractParallel
    , RepGraphSolverInteractParallelFailureInfo (..)
    , RepGraphSolverInteractSimple
    , RepGraphSolverInteractSimpleFailureInfo (..)
    , TestResultWith (..)
    , emptyCache
    , evalModelExpr
    , runRepGraphSolverInteractParallel
    , runRepGraphSolverInteractSimple
    , testHyp
    , testHypGetModel
    , testHypWhyps
    , testHypWhypsGetModel
    , testHypWhypsWithCache
    , testHypWhypsWithCacheFast
    ) where

import BV.Core.ExecuteSMTProofChecks (defaultLogic, splitHyp)
import BV.Core.ModelConfig
import BV.Core.RepGraph
import BV.Core.Types
import BV.Core.Types.Extras.Expr
import BV.Core.Types.Extras.SExprWithPlaceholders (andNS, notS, symbolS)
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
import Data.Foldable (toList, traverse_)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isNothing)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Numeric (readBin, readHex)
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
  = Model { unwrap :: M.Map SExprWithPlaceholders Expr }
  deriving (Generic, Show)

type LowLevelModelRequest = [Name]

type LowLevelModel = [SExpr]

reconstructModel
    :: M.Map SExprWithPlaceholders (Name, ExprType)
    -> LowLevelModelRequest
    -> LowLevelModel
    -> Model
reconstructModel exprs req vals =
    Model $ M.mapKeys (symbolS . (.unwrap)) assocs <> abbrevs
  where
    assocs = M.fromList $ catMaybes $ zipWith (\name val -> (,) name <$> smtToVal val) req vals
    abbrevs = (assocs M.!) <$> M.filter (`M.member` assocs) (fst <$> exprs)

smtToVal :: SExpr -> Maybe Expr
smtToVal sexpr = case viewSExpr sexpr of
    Atom (SymbolAtom "true") -> Just trueE
    Atom (SymbolAtom "false") -> Just falseE
    Atom (HexadecimalAtom s) ->
        Just $ numE (wordT (todo)) (readS readHex s)
    Atom (BinaryAtom s) ->
        Just $ numE (wordT (todo)) (readS readBin s)
    List [Atom (SymbolAtom "_"), Atom (SymbolAtom ('b':'v':bits)), Atom (NumeralAtom n)] ->
        Just $ numE (wordT (read bits)) (toInteger n)
    _ -> Nothing
  where
    readS p s = case filter (null . snd) (p s) of
        [(a, "")] -> a
        _ -> error "parse failure"

evalModelExpr :: MonadRepGraph t m => Expr -> StateT Model m Expr
evalModelExpr expr = do
    sexpr <- withoutEnv $ convertExprNoSplit expr
    evalModel sexpr

evalModel :: Monad m => SExprWithPlaceholders -> StateT Model m Expr
evalModel = go
  where
    go sexpr = maybe (complex sexpr) return (trySimple sexpr)
    trySimple sexpr = smtToVal =<< traverse (preview #_AtomOrPlaceholderAtom) sexpr
    complex sexpr = do
        expr <- case sexpr of
            List [op, cond, x, y] | op == symbolS "ite" -> do
                cond' <- go cond
                if | cond' == trueE -> go x
                   | cond' == falseE -> go y
                   | otherwise -> undefined
            List (_op:args) -> do
                _args' <- traverse go args
                todo
            _ -> undefined
        #unwrap %= M.insert sexpr expr
        return expr

--

class MonadRepGraphSolverSend m => MonadRepGraphSolverInteract m where
    checkHyp :: Maybe LowLevelModelRequest -> SExprWithPlaceholders -> m (TestResultWith (Maybe LowLevelModel))

instance MonadRepGraphSolverInteract m => MonadRepGraphSolverInteract (ReaderT r m) where
    checkHyp = compose2 lift checkHyp

instance MonadRepGraphSolverInteract m => MonadRepGraphSolverInteract (ExceptT e m) where
    checkHyp = compose2 lift checkHyp

instance MonadRepGraphSolverInteract m => MonadRepGraphSolverInteract (RepGraphBase t m) where
    checkHyp = compose2 lift checkHyp

testHypCommon :: (MonadRepGraphSolver m, MonadRepGraphSolverInteract m) => Bool -> SExprWithPlaceholders -> m (TestResultWith (Maybe Model))
testHypCommon wantModel sexpr = case wantModel of
    False -> ensureNothing <$> checkHyp Nothing sexpr
    True -> do
        vars <- askModelVars
        exprs <- askModelExprs
        let req = S.toList vars ++ map fst (toList exprs)
        r <- checkHyp (Just req) sexpr
        return $ over (#_TestResultWithFalse % _Just) (reconstructModel exprs req) r

testHyp :: (MonadRepGraph t m, MonadRepGraphSolverInteract m) => SExprWithPlaceholders -> m Bool
testHyp hyp = (isTrueResult . ensureNoModel) <$> testHypCommon False hyp

testHypGetModel :: (MonadRepGraph t m, MonadRepGraphSolverInteract m) => SExprWithPlaceholders -> m (TestResultWith Model)
testHypGetModel hyp = ensureModel <$> testHypCommon True hyp

testHypWhypsCommon
    :: (RefineTag t, MonadRepGraph t m, MonadRepGraphSolverInteract m)
    => Bool -> Bool -> Expr -> [Hyp t] -> StateT (Maybe Cache) m (Maybe (TestResultWith (Maybe Model)))
testHypWhypsCommon fast wantModel hyp hyps = do
    sexpr <- lift $ interpretHypImps hyps hyp >>= withoutEnv . convertExprNoSplit
    cacheEntry <- preuse $ #_Just % #unwrap % at sexpr % #_Just
    case (cacheEntry, fast) of
        (Just v, _) -> do
            ensureM $ not wantModel
            return $ Just $ fromResult Nothing v
        (Nothing, True) -> do
            return Nothing
        (Nothing, False) -> do
            addPValidDomAssertions
            v <- lift $ testHypCommon wantModel sexpr
            zoomMaybe (#_Just % #unwrap) $ modify $ M.insertWith undefined sexpr (isTrueResult v)
            return $ Just v

testHypWhyps
    :: (RefineTag t, MonadRepGraph t m, MonadRepGraphSolverInteract m)
    => Expr -> [Hyp t] -> m Bool
testHypWhyps hyp hyps =
    (isTrueResult . ensureNoModel . fromJust) <$>
        withoutCache (testHypWhypsCommon False False hyp hyps)

testHypWhypsGetModel
    :: (RefineTag t, MonadRepGraph t m, MonadRepGraphSolverInteract m)
    => Expr -> [Hyp t] -> m (TestResultWith Model)
testHypWhypsGetModel hyp hyps =
    (ensureModel . fromJust) <$>
        withoutCache (testHypWhypsCommon False True hyp hyps)

testHypWhypsWithCache
    :: (RefineTag t, MonadRepGraph t m, MonadRepGraphSolverInteract m)
    => Expr -> [Hyp t] -> StateT Cache m Bool
testHypWhypsWithCache hyp hyps =
    (isTrueResult . ensureNoModel . fromJust) <$>
        withCache (testHypWhypsCommon False False hyp hyps)

testHypWhypsWithCacheFast
    :: (RefineTag t, MonadRepGraph t m, MonadRepGraphSolverInteract m)
    => Expr -> [Hyp t] -> StateT Cache m (Maybe Bool)
testHypWhypsWithCacheFast hyp hyps =
    fmap (isTrueResult . ensureNoModel) <$>
        withCache (testHypWhypsCommon True False hyp hyps)

--

data RepGraphSolverFailureReason
  = RepGraphSolverTimedOut
  | RepGraphSolverAnsweredUnknown SExpr
  deriving (Eq, Generic, Ord, Show)

checkHypInner
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout
    -> ModelConfig
    -> Maybe LowLevelModelRequest
    -> SExprWithPlaceholders
    -> m (Either RepGraphSolverFailureReason (TestResultWith (Maybe LowLevelModel)))
checkHypInner timeout modelConfig modelRequestOpt hyp = runExceptT $ do
    let sendAssert = sendSimpleCommandExpectingSuccess . Assert . Assertion . configureSExpr modelConfig
    let hyps = splitHyp (notS hyp)
    sendSimpleCommandExpectingSuccess $ Push 1
    traverse_ sendAssert hyps
    r <- checkSatWithTimeout timeout >>= \case
        Nothing -> throwError RepGraphSolverTimedOut
        Just (Unknown reason) -> throwError (RepGraphSolverAnsweredUnknown reason)
        Just Unsat -> return TestResultWithTrue
        Just Sat -> fmap TestResultWithFalse $ for modelRequestOpt $ \modelRequest -> do
            getValue $ map (Atom . symbolAtom . (.unwrap)) modelRequest
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

newtype RepGraphSolverInteractSimple m a
  = RepGraphSolverInteractSimple { run :: ExceptT RepGraphSolverInteractSimpleFailureInfo (ReaderT SimpleEnv m) a }
  deriving newtype (Applicative, Functor, Monad)

data SimpleEnv
  = SimpleEnv
      { timeout :: Maybe SolverTimeout
      , modelConfig :: ModelConfig
      }
  deriving (Generic)

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverSend (RepGraphSolverInteractSimple m) where
    sendSExprWithPlaceholders s = RepGraphSolverInteractSimple $ do
        modelConfig <- gview #modelConfig
        sendExpectingSuccess $ configureSExpr modelConfig s

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverInteract (RepGraphSolverInteractSimple m) where
    checkHyp modelRequestOpt hyp = RepGraphSolverInteractSimple $ do
        timeout <- gview #timeout
        modelConfig <- gview #modelConfig
        withExceptT RepGraphSolverInteractSimpleFailureInfo $
            ExceptT $ checkHypInner timeout modelConfig modelRequestOpt hyp

data RepGraphSolverInteractSimpleFailureInfo
  = RepGraphSolverInteractSimpleFailureInfo
      { reason :: RepGraphSolverFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

runRepGraphSolverInteractSimple
    :: (MonadSolver m, MonadThrow m)
    => Maybe SolverTimeout -> ModelConfig -> RepGraphSolverInteractSimple m a -> m (Either RepGraphSolverInteractSimpleFailureInfo a)
runRepGraphSolverInteractSimple timeout modelConfig m = do
    commonSolverSetup modelConfig
    runReaderT (runExceptT m.run) env
  where
    env = SimpleEnv
        { timeout
        , modelConfig
        }

--

-- TODO

type RepGraphSolverInteractParallelInner m =
    ExceptT RepGraphSolverInteractParallelFailureInfo (StateT ParallelState (ReaderT (ParallelEnv m) m))

newtype RepGraphSolverInteractParallel m a
  = RepGraphSolverInteractParallel { run :: RepGraphSolverInteractParallelInner m a }
  deriving newtype (Applicative, Functor, Monad)

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

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverSend (RepGraphSolverInteractParallel m) where
    sendSExprWithPlaceholders s = RepGraphSolverInteractParallel $ do
        #setup %= (++ [s])
        modelConfig <- gview #modelConfig
        sendExpectingSuccess $ configureSExpr modelConfig s

instance (MonadSolver m, MonadThrow m) => MonadRepGraphSolverInteract (RepGraphSolverInteractParallel m) where
    checkHyp modelRequestOpt hyp = RepGraphSolverInteractParallel $ do
        timeout <- gview #timeout
        modelConfig <- gview #modelConfig
        withExceptT RepGraphSolverInteractParallelFailureInfo $
            ExceptT $ checkHypInner timeout modelConfig modelRequestOpt hyp

data RepGraphSolverInteractParallelFailureInfo
  = RepGraphSolverInteractParallelFailureInfo
      { reason :: RepGraphSolverFailureReason
      }
  deriving (Eq, Generic, Ord, Show)

runRepGraphSolverInteractParallel
    :: (MonadSolver m, MonadThrow m)
    => RunParallel m -> Maybe SolverTimeout -> ModelConfig -> RepGraphSolverInteractParallel m a -> m (Either RepGraphSolverInteractParallelFailureInfo a)
runRepGraphSolverInteractParallel runParallel timeout modelConfig m = do
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

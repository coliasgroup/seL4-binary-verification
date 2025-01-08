module BV.SMTSolverInteraction
    ( MonadSolver (..)
    , Result (..)
    , SolverT (..)
    , Value (..)
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import GHC.Generics (Generic)
import qualified SimpleSMT as S
import SimpleSMT (Result, SExpr, Solver, Value)

class Monad m => MonadSolver m where

    ackCommand :: SExpr -> m ()
    simpleCommand :: [String] -> m ()
    simpleCommandMaybe :: [String] -> m Bool

    loadString :: String -> m ()

    setLogicMaybe :: String -> m Bool
    setOption :: String -> String -> m ()
    setOptionMaybe :: String -> String -> m Bool
    produceUnsatCores :: m Bool
    push :: m ()
    pushMany :: Integer -> m ()
    pop :: m ()
    popMany :: Integer -> m ()
    -- inNewScope :: m a -> m a
    declare :: String -> SExpr -> m SExpr
    declareFun :: String -> [SExpr] -> SExpr -> m SExpr
    declareDatatype :: String-> [String] -> [(String, [(String, SExpr)])] -> m ()
    define :: String -> SExpr -> SExpr -> m SExpr
    defineFun :: String -> [(String, SExpr)] -> SExpr -> SExpr -> m SExpr
    defineFunRec :: String -> [(String, SExpr)] -> SExpr -> (SExpr -> SExpr) -> m SExpr
    defineFunsRec :: [(String, [(String, SExpr)], SExpr, SExpr)] -> m ()
    assert :: SExpr -> m ()
    check :: m Result
    getExprs :: [SExpr] -> m [(SExpr, Value)]
    getExpr :: SExpr -> m Value
    getConsts :: [String] -> m [(String, Value)]
    getConst :: String -> m Value
    getUnsatCore :: m [String]

--

newtype SolverT m a
  = SolverT { runSolverT :: ReaderT Solver m a }
  deriving (Applicative, Functor, Generic, Monad, MonadIO)

askSolver :: Monad m => SolverT m Solver
askSolver = SolverT ask

instance MonadIO m => MonadSolver (SolverT m) where
    ackCommand = liftHelper1 S.ackCommand
    simpleCommand = liftHelper1 S.simpleCommand
    simpleCommandMaybe = liftHelper1 S.simpleCommandMaybe
    loadString = liftHelper1 S.loadString
    setLogicMaybe = liftHelper1 S.setLogicMaybe
    setOption = liftHelper2 S.setOption
    setOptionMaybe = liftHelper2 S.setOptionMaybe
    produceUnsatCores = liftHelper0 S.produceUnsatCores
    push = liftHelper0 S.push
    pushMany = liftHelper1 S.pushMany
    pop = liftHelper0 S.pop
    popMany = liftHelper1 S.popMany
    -- inNewScope = liftHelper2 S.inNewScope
    declare = liftHelper2 S.declare
    declareFun = liftHelper3 S.declareFun
    declareDatatype = liftHelper3 S.declareDatatype
    define = liftHelper3 S.define
    defineFun = liftHelper4 S.defineFun
    defineFunRec = liftHelper4 S.defineFunRec
    defineFunsRec = liftHelper1 S.defineFunsRec
    assert = liftHelper1 S.assert
    check = liftHelper0 S.check
    getExprs = liftHelper1 S.getExprs
    getExpr = liftHelper1 S.getExpr
    getConsts = liftHelper1 S.getConsts
    getConst = liftHelper1 S.getConst
    getUnsatCore = liftHelper0 S.getUnsatCore

liftHelper0 :: MonadIO m => (Solver -> IO a) -> SolverT m a
liftHelper0 f = askSolver >>= liftIO . f

liftHelper1 :: MonadIO m => (Solver -> a1 -> IO a) -> a1 -> SolverT m a
liftHelper1 = liftHelperHelper liftHelper0

liftHelper2 :: MonadIO m => (Solver -> a1 -> a2 -> IO a) -> a1 -> a2 -> SolverT m a
liftHelper2 = liftHelperHelper liftHelper1

liftHelper3 :: MonadIO m => (Solver -> a1 -> a2 -> a3 -> IO a) -> a1 -> a2 -> a3 -> SolverT m a
liftHelper3 = liftHelperHelper liftHelper2

liftHelper4 :: MonadIO m => (Solver -> a1 -> a2 -> a3 -> a4 -> IO a) -> a1 -> a2 -> a3 -> a4 -> SolverT m a
liftHelper4 = liftHelperHelper liftHelper3

liftHelperHelper :: ((Solver -> f) -> g) -> (Solver -> a1 -> f) -> a1 -> g
liftHelperHelper prev raw = prev . flip raw

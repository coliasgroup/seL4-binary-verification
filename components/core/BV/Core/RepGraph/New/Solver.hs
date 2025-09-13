{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module BV.Core.RepGraph.New.Solver
    ( MonadRepGraphSolver (..)
    , MonadRepGraphSolverSend (..)
    , SmtName (..)
    , SolverEnv
    , SolverOutput
    , SolverState
    , convertCommand
    , initSolver
    , initSolverEnv
    , initSolverState
    , nameS
    ) where

import BV.Core.RepGraph.New.Types

import BV.Core.GenerateFreshName
import BV.Core.Structs
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Utils

import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (Reader, ReaderT)
import Control.Monad.RWS (RWST, lift, modify, tell)
import Control.Monad.State (StateT, get)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Writer (WriterT)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString (..))
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))

class Monad m => MonadRepGraphSolverSend m where
    sendSExprWithPlaceholders :: SExprWithPlaceholders -> m ()

instance Monad m => MonadRepGraphSolverSend (WriterT SolverOutput m) where
    sendSExprWithPlaceholders s = tell [s]

class (MonadStructs m, MonadRepGraphSolverSend m) => MonadRepGraphSolver m where
    liftSolver :: StateT SolverState (Reader SolverEnv) a -> m a

instance (Monoid w, MonadRepGraphSolver m) => MonadRepGraphSolver (RWST r w s m) where
    liftSolver = lift . liftSolver

instance (Monoid w, MonadRepGraphSolverSend m) => MonadRepGraphSolverSend (RWST r w s m) where
    sendSExprWithPlaceholders = lift . sendSExprWithPlaceholders

instance MonadRepGraphSolver m => MonadRepGraphSolver (ReaderT r m) where
    liftSolver = lift . liftSolver

instance MonadRepGraphSolverSend m => MonadRepGraphSolverSend (ReaderT r m) where
    sendSExprWithPlaceholders = lift . sendSExprWithPlaceholders

instance MonadRepGraphSolver m => MonadRepGraphSolver (StateT s m) where
    liftSolver = lift . liftSolver

instance MonadRepGraphSolverSend m => MonadRepGraphSolverSend (StateT s m) where
    sendSExprWithPlaceholders = lift . sendSExprWithPlaceholders

instance MonadRepGraphSolver m => MonadRepGraphSolver (MaybeT m) where
    liftSolver = lift . liftSolver

instance MonadRepGraphSolverSend m => MonadRepGraphSolverSend (MaybeT m) where
    sendSExprWithPlaceholders = lift . sendSExprWithPlaceholders

instance MonadRepGraphSolver m => MonadRepGraphSolver (ExceptT e m) where
    liftSolver = lift . liftSolver

instance MonadRepGraphSolverSend m => MonadRepGraphSolverSend (ExceptT e m) where
    sendSExprWithPlaceholders = lift . sendSExprWithPlaceholders

data SolverEnv
  = SolverEnv
      { rodata :: ROData
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data SolverState
  = SolverState
      { namesUsed :: Set SmtName
      , nameMap :: Map Ident SmtName
      , inline :: Map Ident SExprWithPlaceholders
      , smtDerivedOps :: Map (Op, Integer) SmtName
      }
  deriving (Eq, Generic, NFData, Ord, Show)

type SolverOutput = [SExprWithPlaceholders]

initSolverEnv :: ROData -> SolverEnv
initSolverEnv rodata = SolverEnv
    { rodata
    }

initSolverState :: SolverState
initSolverState = SolverState
    { namesUsed = S.empty
    , nameMap = M.empty
    , inline = M.empty
    , smtDerivedOps = M.empty
    }

--

send :: MonadRepGraphSolver m => SExprWithPlaceholders -> m ()
send = sendSExprWithPlaceholders

--

initSolver :: MonadRepGraphSolver m => m ()
initSolver = do
    addRODataDef

--

type SmtNameHint = String

newtype SmtName
  = SmtName { unwrap :: String }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

instance IsString SmtName where
    fromString = SmtName

nameS :: SmtName -> S
nameS name = symbolS name.unwrap

takeFreshName :: MonadRepGraphSolver m => SmtNameHint -> m SmtName
takeFreshName nameHint = liftSolver $ zoom #namesUsed $ do
    names <- get
    let isTaken = (`S.member` names) . SmtName
    let name = SmtName (generateFreshName isTaken sanitized)
    modify $ S.insert name
    return name
  where
    sanitized =
        [ if c `elem` ("'#\"" :: String) then '_' else c
        | c <- nameHint
        ]

--

typeToSMT :: ExprType -> S
typeToSMT = \case
    ExprTypeWord bits -> bitVecS bits
    ExprTypeWordArray { len, bits } -> ["Array", bitVecS len, bitVecS bits]
    ExprTypeBool -> boolS
    ExprTypeMem -> memSortS
    ExprTypeDom -> memDomSortS

addSmtVar :: MonadRepGraphSolver m => SmtNameHint -> ExprType -> m SmtName
addSmtVar nameHint ty = do
    name <- takeFreshName nameHint
    send $ declareFunS name.unwrap [] (typeToSMT ty)
    return name

addSmtDef :: MonadRepGraphSolver m => SmtNameHint -> SolverExpr -> m SmtName
addSmtDef nameHint val = do
    name <- takeFreshName nameHint
    s <- convertExpr val
    send $ defineFunS name.unwrap [] (typeToSMT val.ty) s
    return name

askRODataPtrs :: MonadRepGraphSolver m => m [(Expr c, ExprType)]
askRODataPtrs = do
    rodata <- liftSolver $ gview #rodata
    return
        [ (machineWordE range.addr, structT structName)
        | (structName, range) <- rodataStructNamesOf rodata
        ]

addRODataDef :: MonadRepGraphSolver m => m ()
addRODataDef = do
    roName <- takeFreshName "rodata"
    impRoName <- takeFreshName "implies-rodata"
    ensureM $ roName == "rodata"
    ensureM $ impRoName == "implies-rodata"
    rodataPtrs <- askRODataPtrs
    let memParamName = "m"
    (roDef, impRoDef) <- case rodataPtrs of
        [] -> do
            return (trueS, trueS)
        _ -> do
            roWitness <- addSmtVar "rodata-witness" word32T
            roWitnessVal <- addSmtVar "rodata-witness-val" word32T
            ensureM $ roWitness == "rodata-witness"
            ensureM $ roWitnessVal == "rodata-witness-val"
            let roWitnessS = nameS roWitness
            let roWitnessValS = nameS roWitnessVal
            rodata <- liftSolver $ gview #rodata
            let eqs =
                    [ eqS ["load-word32", symbolS memParamName, p] v
                    | (p, v) <-
                        [ (machineWordS p, machineWordS v)
                        | (p, v) <- M.toList rodata.rodata
                        ] ++
                        [ (roWitnessS, roWitnessValS)
                        ]
                    ]
            send $ assertS $ orNS
                [ andS
                    (machineWordS range.addr `bvuleS` roWitnessS)
                    (roWitnessS `bvuleS` machineWordS (range.addr + range.size - 1))
                | range <- rodata.ranges
                ]
            send $ assertS $ eqS
                (roWitnessS `bvandS` machineWordS 3)
                (machineWordS 0)
            return (andNS eqs, last eqs)
    send $ defineFunS
        roName.unwrap
        [(memParamName, typeToSMT ExprTypeMem)]
        boolS
        roDef
    send $ defineFunS
        impRoName.unwrap
        [(memParamName, typeToSMT ExprTypeMem)]
        boolS
        impRoDef

--

convertCommand :: MonadRepGraphSolver m => Command -> m ()
convertCommand = \case
    CommandDeclare var -> do
        name <- addSmtVar var.name.unwrap var.ty
        liftSolver $ #nameMap %= M.insertWith undefined var.name name
    CommandDefine inlineHint var val -> do
        s <- convertExpr val
        if inlineHint == Just InlineHintInline && length (showSExprWithPlaceholders s) < 80
            then do
                liftSolver $ #inline %= M.insertWith undefined var.name s
            else do
                name <- addSmtDef var.name.unwrap val
                liftSolver $ #nameMap %= M.insertWith undefined var.name name
    CommandAssert expr -> do
        s <- convertExpr expr
        send $ assertS s

convertExpr :: MonadRepGraphSolver m => SolverExpr -> m SExprWithPlaceholders
convertExpr = undefined

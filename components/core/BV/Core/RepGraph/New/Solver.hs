{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.RepGraph.New.Solver
    ( MonadRepGraphSolver (..)
    , MonadRepGraphSolverSend (..)
    , SolverEnv
    , SolverExpr
    , SolverExprCommand (..)
    , SolverExprCommandInlineHint (..)
    , SolverExprContext (..)
    , SolverOutput
    , SolverState
    , convertCommand
    , convertExpr
    , initSolverEnv
    , initSolverState
    ) where

import BV.Core.GenerateFreshName
import BV.Core.Structs
import BV.Core.Types
import BV.Core.Types.Extras

import BV.Core.Utils (whenNothing)
import BV.SMTLIB2.SExpr
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (Reader, ReaderT)
import Control.Monad.RWS (RWST, lift, modify, tell)
import Control.Monad.State (StateT, get)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Control.Monad.Writer (WriterT)
import Data.Binary (Binary)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString (..))
import Data.Void (Void)
import Debug.Trace (traceShowM)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))
import Text.Printf (printf)

--

newtype SolverExprContext
  = SolverExprContext Void

type SolverExpr = Expr SolverExprContext

data SolverExprCommand
  = SolverExprCommandDeclare NameTy
  | SolverExprCommandDefine SolverExprCommandInlineHint NameTy SolverExpr
  | SolverExprCommandAssert SolverExpr
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary SolverExprCommand

data SolverExprCommandInlineHint
  = SolverExprCommandInlineHintInline
  | SolverExprCommandInlineHintDontInline
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary SolverExprCommandInlineHint

--

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
      , tokens :: Map Ident SmtName
      , smtDerivedOps :: Map (Op, ExprType) SmtName
      }
  deriving (Eq, Generic, NFData, Ord, Show)

type SolverOutput = [SExprWithPlaceholders]

initSolverEnv :: ROData -> SolverEnv
initSolverEnv rodata = SolverEnv
    { rodata
    }

initSolverState :: SolverState
initSolverState = SolverState
    { namesUsed = initNamesUsed
    , nameMap = M.empty
    , inline = M.empty
    , tokens = M.empty
    , smtDerivedOps = M.empty
    }

initNamesUsed :: Set SmtName
initNamesUsed = S.fromList $ map SmtName $
    [ "mem-dom"
    , "word2-xor-scramble"
    , "unspecified-precond"
    ] ++
    [ prefix ++ "-eq"
    | prefix <- ["mem", "word32"]
    ] ++
    [ op ++ "-word" ++ bits
    | op <- ["load", "store"]
    , bits <- ["8", "32", "64"]
    ]

--

send :: MonadRepGraphSolver m => SExprWithPlaceholders -> m ()
send s = do
    -- traceShowM $ showSExprWithPlaceholders s
    sendSExprWithPlaceholders s

--

withMapSlot :: (MonadRepGraphSolver m, Ord k) => Lens' SolverState (M.Map k v) -> k -> m v -> m v
withMapSlot l k m = do
    opt <- liftSolver (use (l % at k))
    whenNothing opt $ do
        v <- m
        liftSolver $ l %= M.insert k v
        return v

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
    ExprTypeToken -> typeToSMT compiledTokenType

compiledTokenType :: ExprType
compiledTokenType = ExprTypeWord 64

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

--

convertCommand :: MonadRepGraphSolver m => SolverExprCommand -> m ()
convertCommand cmd = do
    -- traceShowM cmd
    convertCommand' cmd

convertCommand' :: MonadRepGraphSolver m => SolverExprCommand -> m ()
convertCommand' = \case
    SolverExprCommandDeclare var -> do
        name <- addSmtVar var.name.unwrap var.ty
        liftSolver $ #nameMap %= M.insertWith undefined var.name name
    SolverExprCommandDefine inlineHint var val -> do
        s <- convertExpr val
        if inlineHint == SolverExprCommandInlineHintInline && length (showSExprWithPlaceholders s) < 80
            then do
                liftSolver $ #inline %= M.insertWith undefined var.name s
            else do
                name <- addSmtDef var.name.unwrap val
                liftSolver $ #nameMap %= M.insertWith undefined var.name name
    SolverExprCommandAssert expr -> do
        s <- convertExpr expr
        send $ assertS s

convertExpr :: MonadRepGraphSolver m => SolverExpr -> m SExprWithPlaceholders
convertExpr expr = case expr.value of
    ExprValueVar var -> do
        sOpt <- runMaybeT $
            nameS <$> MaybeT (liftSolver $ use $ #nameMap % at var)
                <|> MaybeT (liftSolver $ use $ #inline % at var)
        whenNothing sOpt $ do
            error $ "could not convert var: " ++ show var
    ExprValueNum n -> do
        return $ intWithWidthS (wordTBits expr.ty) n
    ExprValueToken tok -> do
        getToken tok
    ExprValueOp OpCountTrailingZeroes [arg] -> do
        convertExpr $ clzE (wordReverseE arg)
    ExprValueOp op [arg] | op == OpWordCast || op == OpWordCastSigned -> do
        let signed = op == OpWordCastSigned
        let ExprTypeWord fromBits = arg.ty
        let ExprTypeWord toBits = expr.ty
        convertWordCast signed fromBits toBits <$> convertExpr arg
    ExprValueOp op args -> do
        let argTypes = (map (.ty) args)
        opOpt' <- runMaybeT $
            hoistMaybe (convertSimpleOpWithTypes op expr.ty argTypes)
                <|> nameS <$> MaybeT (getDerivedOp op expr.ty)
        op' <- whenNothing opOpt' $ do
            error $ "could not convert op: " ++ show op
        args' <- traverse convertExpr args
        return $ case args' of
            [] -> op'
            _ -> List $ [op'] ++ args'

getToken :: MonadRepGraphSolver m => Ident -> m SExprWithPlaceholders
getToken ident = fmap nameS $ withMapSlot #tokens ident $ do
    n <- liftSolver $ use $ #tokens % to M.size
    addSmtDef
        ("token_" ++ ident.unwrap)
        (numE compiledTokenType (toInteger n))

convertWordCast :: Bool -> Integer -> Integer -> SExprWithPlaceholders -> SExprWithPlaceholders
convertWordCast signed fromBits toBits arg = if
    | toBits == fromBits -> arg
    | toBits < fromBits -> [ixS "extract" [intS (toBits - 1), intS (0 :: Integer)], arg]
    | otherwise ->
        let op = if signed then "sign_extend" else "zero_extend"
         in [ixS op [intS (toBits - fromBits)], arg]

convertSimpleOp :: Op -> Maybe S
convertSimpleOp = \case
    OpPlus -> Just "bvadd"
    OpMinus -> Just "bvsub"
    OpTimes -> Just "bvmul"
    OpModulus -> Just "bvurem"
    OpDividedBy -> Just "bvudiv"
    OpBWAnd -> Just "bvand"
    OpBWOr -> Just "bvor"
    OpBWXOR -> Just "bvxor"
    OpAnd -> Just "and"
    OpOr -> Just "or"
    OpImplies -> Just "=>"
    OpLess -> Just "bvult"
    OpLessEquals -> Just "bvule"
    OpSignedLess -> Just "bvslt"
    OpSignedLessEquals -> Just "bvsle"
    OpShiftLeft -> Just "bvshl"
    OpShiftRight -> Just "bvlshr"
    OpSignedShiftRight -> Just "bvashr"
    OpNot -> Just "not"
    OpBWNot -> Just "bvnot"
    OpTrue -> Just "true"
    OpFalse -> Just "false"
    OpUnspecifiedPrecond -> Just "unspecified-precond"
    OpIfThenElse -> Just "ite"
    OpMemDom -> Just "mem-dom"
    OpWordArrayAccess -> Just "select"
    OpWordArrayUpdate -> Just "store"
    _ -> Nothing

convertSimpleOpWithTypes :: Op -> ExprType -> [ExprType] -> Maybe S
convertSimpleOpWithTypes op ty argTypes = convertSimpleOp op <|> case op of
    OpEquals -> Just $
        case argTypes of
            ExprTypeMem:_ -> "mem-eq"
            (ExprTypeWord 32):_ -> "word32-eq"
            _ -> "="
    OpMemAcc -> Just $
        let ExprTypeWord bits = ty
         in case bits of
                8 -> "load-word8"
                32 -> "load-word32"
                64 -> "load-word64"
    OpMemUpdate -> Just $
        let [_, _, ExprTypeWord bits] = argTypes
         in case bits of
                8 -> "store-word8"
                32 -> "store-word32"
                64 -> "store-word64"
    _ -> Nothing

getDerivedOp :: MonadRepGraphSolver m => Op -> ExprType -> m (Maybe SmtName)
getDerivedOp op ty = traverse (withMapSlot #smtDerivedOps (op, ty)) $ if
    | op == OpCountLeadingZeroes || op == OpWordReverse -> Just $ do
        let ExprTypeWord bits = ty
        getDerivedWordOp op bits
    | op == OpExt OpExtROData || op == OpExt OpExtImpliesROData -> Just $ do
        getDerivedRODataOp op
    | otherwise -> Nothing

getDerivedWordOp :: MonadRepGraphSolver m => Op -> Integer -> m SmtName
getDerivedWordOp op bits = do
    name <- takeFreshName $ case op of
        OpCountLeadingZeroes -> printf "bvclz_%d" bits
        OpWordReverse -> printf "bvrev_%d" bits
    body <- case bits of
        1 -> return $ case op of
            OpCountLeadingZeroes -> iteS ("x" `eqS` binS "0") (binS "1") (binS "0")
            OpWordReverse -> "x"
        _ -> do
            let botBits = bits `div` 2
            let topBits = bits - botBits
            topOp <- fromJust <$> getDerivedOp op (wordT topBits)
            botOp <- fromJust <$> getDerivedOp op (wordT botBits)
            let top = [ixS "extract" [intS (bits - 1), intS botBits], "x"]
                bot = [ixS "extract" [intS (botBits - 1), intS (0 :: Integer)], "x"]
                topApp = [nameS topOp, top]
                botApp = [nameS botOp, bot]
                topAppExtended = [ixS "zero_extend" [intS botBits], topApp]
                botAppExtended = [ixS "zero_extend" [intS topBits], botApp]
            return $ case op of
                OpCountLeadingZeroes ->
                    iteS
                        (top `eqS` intWithWidthS topBits 0)
                        (bvaddS botAppExtended (intWithWidthS bits botBits))
                        topAppExtended
                OpWordReverse ->
                    concatS botApp topApp
    send $ defineFunS
        name.unwrap
        [("x", bitVecS bits)]
        (bitVecS bits)
        body
    return name

getDerivedRODataOp :: MonadRepGraphSolver m => Op -> m SmtName
getDerivedRODataOp op = do
    name <- takeFreshName $ case op of
        OpExt OpExtROData -> "rodata"
        OpExt OpExtImpliesROData -> "implies-rodata"
    rodata <- liftSolver $ gview #rodata
    body <- case rodata.ranges of
        [] -> do
            return trueS
        _ -> case op of
            OpExt OpExtROData -> do
                impliesRODataOp <- fromJust <$> getDerivedOp (OpExt OpExtImpliesROData) boolT
                let eqs =
                        [ loadWord32S "m" (machineWordS p) `eqS` (machineWordS v)
                        | (p, v) <- M.toList rodata.rodata
                        ]
                return $ andNS eqs `andS` [nameS impliesRODataOp, "m"]
            OpExt OpExtImpliesROData -> do
                roWitness <- nameS <$> addSmtVar "rodata-witness" word32T
                roWitnessVal <- nameS <$> addSmtVar "rodata-witness-val" word32T
                send $ assertS $ orNS
                    [ andS
                        (machineWordS range.addr `bvuleS` roWitness)
                        (roWitness `bvuleS` machineWordS (range.addr + range.size - 1))
                    | range <- rodata.ranges
                    ]
                send $ assertS $ eqS
                    (roWitness `bvandS` machineWordS 3)
                    (machineWordS 0)
                return $ loadWord32S "m" roWitness `eqS` roWitnessVal
    send $ defineFunS
        name.unwrap
        [("m", typeToSMT ExprTypeMem)]
        boolS
        body
    return name

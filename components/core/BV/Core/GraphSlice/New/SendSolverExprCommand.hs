{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.GraphSlice.New.SendSolverExprCommand
    ( GraphSliceSendSolverExprCommandT
    , SolverExpr
    , SolverExprCommand
    , SolverExprContext (..)
    , convertSolverExpr
    , runGraphSliceSendSolverExprCommandTStep
    , sendSolverExprCommand
    ) where

import BV.Core.GraphSlice.New.Common

import BV.Core.GenerateFreshName (takeFreshNameWith)
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils (whenNothing, withMapSlotWith)
import BV.SMTLIB2.SExpr (GenericSExpr (List), isValidSymbolAtomFirstChar,
                         isValidSymbolAtomSubsequentChar)

import Control.Applicative ((<|>))
import Control.Monad (join)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (Reader, ReaderT, mapReaderT, runReaderT)
import Control.Monad.RWS (lift)
import Control.Monad.State (StateT, evalStateT, mapStateT)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString (..))
import Data.Void (Void)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Optics
import Optics.State.Operators ((%=))
import Text.Printf (printf)

-- TODO prefix all names to avoid clashes?

type T = GraphSliceSendSolverExprCommandT

type C = MonadGraphSliceSendSExpr

--

newtype SolverExprContext
  = SolverExprContext Void

type SolverExpr = Expr SolverExprContext

type SolverExprCommand = ExprCommand SolverExprContext

--

newtype GraphSliceSendSolverExprCommandT m a
  = GraphSliceSendSolverExprCommandT { run :: StateT TState (ReaderT TEnv m) a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadTrans GraphSliceSendSolverExprCommandT where
    lift = GraphSliceSendSolverExprCommandT . lift . lift

liftPure :: Monad m => StateT TState (Reader TEnv) a -> T m a
liftPure = GraphSliceSendSolverExprCommandT . mapStateT (mapReaderT (return . runIdentity))

send :: C m => SExprWithPlaceholders -> T m ()
send = lift . sendSExpr

runGraphSliceSendSolverExprCommandTStep :: Monad m => ROData -> T m a -> m a
runGraphSliceSendSolverExprCommandTStep rodata =
      flip runReaderT (initEnv rodata)
    . flip evalStateT initState
    . (.run)

data TEnv
  = TEnv
      { rodata :: ROData
      }
  deriving (Generic)

data TState
  = TState
      { names :: Set SmtName
      , nameMap :: Map Ident SmtName
      , inline :: Map Ident SExprWithPlaceholders
      , tokens :: Map Ident SmtName
      , smtDerivedOps :: Map (Op, ExprType) SmtName
      }
  deriving (Generic)

initEnv :: ROData -> TEnv
initEnv rodata = TEnv
    { rodata
    }

initState :: TState
initState = TState
    { names = initNames
    , nameMap = M.empty
    , inline = M.empty
    , tokens = M.empty
    , smtDerivedOps = M.empty
    }

--

withMapSlot :: (C m, Ord k) => Lens' TState (M.Map k v) -> k -> T m v -> T m v
withMapSlot = withMapSlotWith $ liftPure . mapStateT (return . runIdentity)

--

type SmtNameHint = String

newtype SmtName
  = SmtName { unwrap :: String }
  deriving (Eq, Generic, Ord, Show)

instance IsString SmtName where
    fromString = SmtName

nameS :: SmtName -> SExprWithPlaceholders
nameS name = symbolS name.unwrap

takeFreshName :: C m => SmtNameHint -> T m SmtName
takeFreshName nameHint = liftPure $ zoom #names $ takeFreshNameWith SmtName $ sanitizeName nameHint

sanitizeName :: String -> String
sanitizeName name =
    [ if (not isFirst || isValidSymbolAtomFirstChar c) && isValidSymbolAtomSubsequentChar c
      then c
      else '_'
    | (c, isFirst) <- zip name $ True : repeat False
    ]

initNames :: Set SmtName
initNames = S.fromList $ map SmtName $ join
    [ [ "mem-dom"
      , "word2-xor-scramble"
      , "unspecified-precond"
      ]
    , [ prefix ++ "-eq"
      | prefix <- ["mem", "word32"]
      ]
    , [ op ++ "-word" ++ bits
      | op <- ["load", "store"]
      , bits <- ["8", "32", "64"]
      ]
    ]

--

typeToSmt :: ExprType -> S
typeToSmt = \case
    ExprTypeWord bits -> bitVecS bits
    ExprTypeWordArray { len, bits } -> ["Array", bitVecS len, bitVecS bits]
    ExprTypeBool -> boolS
    ExprTypeMem -> memSortS
    ExprTypeDom -> memDomSortS
    ExprTypeToken -> typeToSmt concreteTokenType

concreteTokenType :: ExprType
concreteTokenType = ExprTypeWord 64

addSmtVar :: C m => SmtNameHint -> ExprType -> T m SmtName
addSmtVar nameHint ty = do
    name <- takeFreshName nameHint
    send $ declareFunS name.unwrap [] (typeToSmt ty)
    return name

addSmtDef :: C m => SmtNameHint -> SolverExpr -> T m SmtName
addSmtDef nameHint val = convertSolverExpr val >>= addConvertedSmtDefx nameHint val.ty

addConvertedSmtDefx :: C m => SmtNameHint -> ExprType -> SExprWithPlaceholders -> T m SmtName
addConvertedSmtDefx nameHint ty s = do
    name <- takeFreshName nameHint
    send $ defineFunS name.unwrap [] (typeToSmt ty) s
    return name

--

sendSolverExprCommand :: C m => SolverExprCommand -> T m ()
sendSolverExprCommand = \case
    ExprCommandDeclare var -> do
        name <- addSmtVar var.name.unwrap var.ty
        liftPure $ #nameMap %= M.insertWith undefined var.name name
    ExprCommandDefine inlineHint var val -> do
        s <- convertSolverExpr val
        if inlineHint == ExprCommandInlineHintSometimes && length (showSExprWithPlaceholders s) < 80
        then do
            liftPure $ #inline %= M.insertWith undefined var.name s
        else do
            name <- addConvertedSmtDefx var.name.unwrap val.ty s
            liftPure $ #nameMap %= M.insertWith undefined var.name name
    ExprCommandAssert expr -> do
        s <- convertSolverExpr expr
        send $ assertS s

convertSolverExpr :: HasCallStack => C m => SolverExpr -> T m SExprWithPlaceholders
convertSolverExpr expr = case expr.value of
    ExprValueVar var -> do
        fmap fromJust $ runMaybeT $
            nameS <$> MaybeT (liftPure $ use $ #nameMap % at var)
                <|> MaybeT (liftPure $ use $ #inline % at var)
    ExprValueNum n -> do
        return $ intWithWidthS (wordTBits expr.ty) n
    ExprValueToken tok -> do
        getToken tok
    ExprValueOp OpCountTrailingZeroes ~[arg] -> do
        convertSolverExpr $ clzE (wordReverseE arg)
    ExprValueOp op ~[arg] | op == OpWordCast || op == OpWordCastSigned -> do
        let signed = op == OpWordCastSigned
        let ExprTypeWord fromBits = arg.ty
        let ExprTypeWord toBits = expr.ty
        convertWordCast signed fromBits toBits <$> convertSolverExpr arg
    ExprValueOp op args -> do
        let argTypes = map (.ty) args
        opOpt' <- runMaybeT $
            hoistMaybe (convertSimpleOpWithTypes op expr.ty argTypes)
                <|> nameS <$> MaybeT (getDerivedOp op expr.ty)
        op' <- whenNothing opOpt' $ do
            error $ "could not convert op: " ++ show op
        args' <- traverse convertSolverExpr args
        return $ case args' of
            [] -> op'
            _ -> List $ [op'] ++ args'

getToken :: C m => Ident -> T m SExprWithPlaceholders
getToken ident = fmap nameS $ withMapSlot #tokens ident $ do
    n <- liftPure $ use $ #tokens % to M.size
    addSmtDef
        ("token_" ++ ident.unwrap)
        (numE concreteTokenType (toInteger n))

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

getDerivedOp :: C m => Op -> ExprType -> T m (Maybe SmtName)
getDerivedOp op ty = traverse (withMapSlot #smtDerivedOps (op, ty)) $ if
    | op == OpCountLeadingZeroes || op == OpWordReverse -> Just $ do
        let ExprTypeWord bits = ty
        getDerivedWordOp op bits
    | op == OpExt OpExtROData || op == OpExt OpExtImpliesROData -> Just $ do
        getDerivedRODataOp op
    | otherwise -> Nothing

getDerivedWordOp :: C m => Op -> Integer -> T m SmtName
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

getDerivedRODataOp :: C m => Op -> T m SmtName
getDerivedRODataOp op = do
    name <- takeFreshName $ case op of
        OpExt OpExtROData -> "rodata"
        OpExt OpExtImpliesROData -> "implies-rodata"
    rodata <- liftPure $ gview #rodata
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
        [("m", typeToSmt ExprTypeMem)]
        boolS
        body
    return name

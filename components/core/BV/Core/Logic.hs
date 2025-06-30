{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module BV.Core.Logic
    ( MonadStructs (..)
    , PValidKind (..)
    , PValidType (..)
    , alignOfSelfContainedType
    , alignOfType
    , alignValidIneqE
    , alignValidIneqM
    , applyRelWrapper
    , isNodeNoop
    , lookupStruct
    , pvalidAssertion1
    , pvalidAssertion2
    , pvalidKindFromOp
    , sizeOfSelfContainedType
    , sizeOfType
    , strengthenHyp
    , weakenAssert
    , withStructs
    , withoutStructs
    ) where

import BV.Core.Arch
import BV.Core.Types
import BV.Core.Types.Extras.Expr
import BV.Core.Utils

import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask), Reader, ReaderT, runReader)
import Control.Monad.Trans (lift)
import Data.Function (applyWhen)
import Data.Functor ((<&>))
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)

--

class Monad m => MonadStructs m where
    askLookupStruct :: m (Ident -> Struct)

instance MonadStructs m => MonadStructs (ReaderT r m) where
    askLookupStruct = lift askLookupStruct

instance MonadStructs m => MonadStructs (ExceptT e m) where
    askLookupStruct = lift askLookupStruct

lookupStruct :: MonadStructs m => Ident -> m Struct
lookupStruct name = ($ name) <$> askLookupStruct

newtype WithoutStructs a
  = WithoutStructs { unwrap :: Identity a }
  deriving newtype (Applicative, Functor, Monad)

instance MonadStructs WithoutStructs where
    askLookupStruct = return $ \name ->
        error $ "attempted to lookup struct '" ++ name.unwrap ++ "' without structs"

withoutStructs :: forall a. (forall m. MonadStructs m => m a) -> a
withoutStructs m = runIdentity (m :: WithoutStructs a).unwrap

newtype WithStructs a
  = WithStructs { unwrap :: Reader (Ident -> Struct) a }
  deriving newtype (Applicative, Functor, Monad)

instance MonadStructs WithStructs where
    askLookupStruct = WithStructs ask

withStructs :: forall a. (Ident -> Struct) -> (forall m. MonadStructs m => m a) -> a
withStructs f m = runReader (m :: WithStructs a).unwrap f

--

sizeOfType :: MonadStructs m => ExprType -> m Integer
sizeOfType = \case
    ExprTypeWord { bits } ->
        let (bytes, 0) = bits `divMod` 8
        in return bytes
    ExprTypeArray { ty, len } -> (* len) <$> sizeOfType ty
    ExprTypeStruct name -> (.size) <$> lookupStruct name
    ExprTypePtr _ -> return archPtrSizeBytes

sizeOfSelfContainedType :: ExprType -> Integer
sizeOfSelfContainedType ty = withoutStructs $ sizeOfType ty

alignOfType :: MonadStructs m => ExprType -> m Integer
alignOfType ty = case ty of
    ExprTypeWord { } -> sizeOfType ty
    ExprTypeArray { ty = ty' } -> alignOfType ty'
    ExprTypeStruct name -> (.align) <$> lookupStruct name
    ExprTypePtr _ -> return archPtrSizeBytes

alignOfSelfContainedType :: ExprType -> Integer
alignOfSelfContainedType ty = withoutStructs $ alignOfType ty

--

isNodeNoop :: Node -> Bool
isNodeNoop = \case
    NodeBasic (BasicNode { varUpdates }) -> null varUpdates
    NodeCond (CondNode { left, right }) -> left == right
    NodeCall (CallNode {}) -> False

--

data PValidType
  = PValidTypeArray
      { ty :: ExprType
      , len :: Expr
      }
  | PValidTypeType ExprType
  deriving (Eq, Generic, NFData, Ord, Show)

alignValidIneqE :: ExprType -> Expr -> Expr
alignValidIneqE ty p =
    ensure (align `elem` [1, 4, 8]) $
        foldr1 andE conj
  where
    size = machineWordE (sizeOfSelfContainedType ty)
    align = alignOfSelfContainedType ty
    w0 = machineWordE 0
    conj = optionals (align > 1) [bitwiseAndE p (machineWordE (align - 1)) `eqE` w0] ++
        [ notE (p `eqE` w0)
        , (w0 `lessE` size) `impliesE` (p `lessEqE` negE size)
        ]

arraySizeIneqM :: MonadStructs m => ExprType -> Expr -> Expr -> m Expr
arraySizeIneqM ty len _p = do
    elSize <- sizeOfType ty
    let limit = (((2 :: Integer) ^ (32 :: Integer)) - 4) `div` elSize
    return $ lessEqE len (machineWordE limit)

alignValidIneqM :: MonadStructs m => PValidType -> Expr -> m Expr
alignValidIneqM pvTy p = do
    (align, size, sizeReqs) <- case pvTy of
        PValidTypeType ty -> do
            align <- alignOfType ty
            size <- machineWordE <$> sizeOfType ty
            return (align, size, [])
        PValidTypeArray { ty, len } -> do
            align <- alignOfType ty
            elSize <- machineWordE <$> sizeOfType ty
            let size = timesE elSize len
            sizeReq <- arraySizeIneqM ty len p
            return (align, size, [sizeReq])
    ensureM $ align `elem` [1, 4, 8]
    let conj =
            optionals (align > 1) [bitwiseAndE p (machineWordE (align - 1)) `eqE` w0]
                ++ sizeReqs
                ++ [ notE (p `eqE` w0)
                   , (w0 `lessE` size) `impliesE` (p `lessEqE` negE size)
                   ]
    return $ foldr1 andE conj
  where
    w0 = machineWordE 0

data PValidTypeWithStrength
  = PValidTypeWithStrengthArray
      { ty :: ExprType
      , len :: Expr
      , strength :: Maybe PArrayValidStrength
      }
  | PValidTypeWithStrengthType ExprType
  deriving (Eq, Generic, NFData, Ord, Show)

pvalidTypeWithUnspecifiedStrength :: PValidType -> PValidTypeWithStrength
pvalidTypeWithUnspecifiedStrength = \case
    PValidTypeType ty -> PValidTypeWithStrengthType ty
    PValidTypeArray { ty, len } -> PValidTypeWithStrengthArray
        { ty
        , len
        , strength = Nothing
        }

data PArrayValidStrength
  = PArrayValidStrengthStrong
  | PArrayValidStrengthWeak
  deriving (Eq, Generic, NFData, Ord, Show)

data PValidKind
  = PValidKindPArrayValid
  | PValidKindPGlobalValid
  | PValidKindPValid
  | PValidKindPWeakValid
  deriving (Eq, Generic, NFData, Ord, Show)

pvalidKindFromOp :: Op -> PValidKind
pvalidKindFromOp = \case
    OpPValid -> PValidKindPValid
    OpPGlobalValid -> PValidKindPGlobalValid
    OpPArrayValid -> PValidKindPArrayValid
    OpPWeakValid -> PValidKindPWeakValid

endAddr :: MonadStructs m => Expr -> PValidType -> m Expr
endAddr p pvTy = do
    size <- case pvTy of
        PValidTypeArray { ty, len } -> do
            elemSize <- sizeOfType ty
            return $ machineWordE elemSize `timesE` len
        PValidTypeType ty -> machineWordE <$> sizeOfType ty
    return $ p `plusE` (size `minusE` machineWordE 1)

normArrayType :: PValidTypeWithStrength -> PValidTypeWithStrength
normArrayType = \case
    PValidTypeWithStrengthType (ExprTypeArray { ty, len }) -> PValidTypeWithStrengthArray
        { ty
        , len = machineWordE len
        , strength = Just PArrayValidStrengthStrong
        }
    PValidTypeWithStrengthArray { ty, len, strength = Nothing } -> PValidTypeWithStrengthArray
        { ty
        , len
        , strength = Just PArrayValidStrengthWeak
        }
    pvTy -> pvTy

pvalidAssertion1 :: MonadStructs m => (PValidType, PValidKind, Expr, Expr) -> (PValidType, PValidKind, Expr, Expr) -> m Expr
pvalidAssertion1 (pvTy1, _pvKind1, p1, pv1) (pvTy2, _pvKind2, p2, pv2) = do
    let offs1 = p1 `minusE` p2
    cond1 <- getSTypCondition offs1 pvTy1 pvTy2
    let offs2 = p2 `minusE` p1
    cond2 <- getSTypCondition offs2 pvTy2 pvTy1
    out1 <- lessE <$> endAddr p1 pvTy1 <*> pure p2
    out2 <- lessE <$> endAddr p2 pvTy2 <*> pure p1
    return $ (pv1 `andE` pv2) `impliesE` foldr1 orE [cond1, cond2, out1, out2]

getSTypCondition :: MonadStructs m =>  Expr -> PValidType -> PValidType -> m Expr
getSTypCondition offs innerTy outerTy = do
    r <- getSTypConditionInner1
        (pvalidTypeWithUnspecifiedStrength innerTy)
        (pvalidTypeWithUnspecifiedStrength outerTy)
    return $ case r of
        Nothing -> falseE
        Just f -> f offs

-- TODO evaluate performance loss from not caching (see graph-refine)
getSTypConditionInner1 :: MonadStructs m => PValidTypeWithStrength -> PValidTypeWithStrength -> m (Maybe (Expr -> Expr))
getSTypConditionInner1 innerTy outerTy = do
    let innerTyNorm = normArrayType innerTy
    let outerTyNorm = normArrayType outerTy
    getSTypConditionInner2 innerTyNorm outerTyNorm

arrayTypeSize :: MonadStructs m => PValidTypeWithStrength -> m Expr
arrayTypeSize (PValidTypeWithStrengthArray { ty, len }) = do
    size <- sizeOfType ty
    return $ timesE len (machineWordE size)

getSTypConditionInner2 :: MonadStructs m => PValidTypeWithStrength -> PValidTypeWithStrength -> m (Maybe (Expr -> Expr))
getSTypConditionInner2 innerPvTy outerPvTy = case (innerPvTy, outerPvTy) of
    (PValidTypeWithStrengthArray { ty = innerElTy }, PValidTypeWithStrengthArray { strength = outerBound }) -> do
            condOpt <- getSTypConditionInner1 (PValidTypeWithStrengthType innerElTy) outerPvTy
            innerSize <- arrayTypeSize innerPvTy
            outerSize <- arrayTypeSize outerPvTy
            return $ case (outerBound, condOpt) of
                (Just PArrayValidStrengthStrong, Just cond) ->
                    Just $ \offs -> andE (cond offs) (lessEqE (plusE innerSize offs) outerSize)
                _ -> condOpt
    _ | innerPvTy == outerPvTy -> return $ Just $ \offs -> eqE offs (machineWordE 0)
    (_, PValidTypeWithStrengthType (ExprTypeStruct outerStructName)) -> do
        outerStruct <- lookupStruct outerStructName
        conds <- fmap catMaybes $ for (M.elems outerStruct.fields) $ \field -> do
            fOpt <- getSTypConditionInner1 innerPvTy (PValidTypeWithStrengthType field.ty)
            return $ (, machineWordE field.offset) <$> fOpt
        return $ case conds of
            [] -> Nothing
            _ -> Just $ \offs -> foldr1 orE
                [ c (minusE offs offs')
                | (c, offs') <- conds
                ]
    (_, PValidTypeWithStrengthArray { ty = outerElTy, len = outerLen, strength = outerBound }) -> do
            condOpt <- getSTypConditionInner1 innerPvTy (PValidTypeWithStrengthType outerElTy)
            outerElSize <- machineWordE <$> sizeOfType outerElTy
            let outerSize = timesE outerLen outerElSize
            return $ condOpt <&> \cond -> case outerBound of
                Just PArrayValidStrengthStrong -> \offs -> andE (lessE offs outerSize) (cond (modulusE offs outerElSize))
                _ -> \offs -> cond (modulusE offs outerElSize)
    _ -> return Nothing

pvalidAssertion2 :: MonadStructs m => (PValidType, PValidKind, Expr, Expr) -> (PValidType, PValidKind, Expr, Expr) -> m Expr
pvalidAssertion2 (pvTy1, _pvKind1, p1, pv1) (pvTy2, _pvKind2, p2, pv2) = do
    case (pvTy1, pvTy2) of
        (PValidTypeArray {}, PValidTypeArray {}) -> return trueE
        _ -> do
            let offs1 = p1 `minusE` p2
            cond1 <- getSTypCondition offs1 pvTy1 pvTy2
            let imp1 = impliesE (andE cond1 pv2) pv1
            let offs2 = p2 `minusE` p1
            cond2 <- getSTypCondition offs2 pvTy2 pvTy1
            let imp2 = impliesE (andE cond2 pv1) pv2
            return $ imp1 `andE` imp2

--

applyRelWrapper :: Expr -> Expr -> Expr
applyRelWrapper lhs rhs =
    case () of
        _ | ops == S.fromList [OpStackWrapper] ->
            let sp1:st1:rest1 = argsL
                sp2:st2:rest2 = argsR
                excepts = nub $ rest1 ++ rest2
                f st0 = foldl (\st p -> memUpdE p st (word32E 0)) st0 excepts
             in boolE $ ExprValueOp OpStackEquals [sp1, f st1, sp2, f st2]
        _ | ops == S.fromList [OpMemAccWrapper, OpMemWrapper] ->
            let [[addr, val]] =
                    [ args
                    | Expr { value = ExprValueOp OpMemAccWrapper args } <- [lhs, rhs]
                    ]
                [[m]] =
                    [ args
                    | Expr { value = ExprValueOp OpMemWrapper args } <- [lhs, rhs]
                    ]
             in ensure (addr.ty == word32T && m.ty == memT) $
                    eqE (memAccE val.ty addr m) val
        _ | ops == S.fromList [OpEqSelectiveWrapper] ->
            let [lhsV, _, _] = argsL
                [rhsV, _, _] = argsR
             in if lhsV.ty == ExprTypeRelWrapper
                then applyRelWrapper lhsV rhsV
                else eqE lhs rhs
        _ -> error ""
  where
    ops = S.fromList [opL, opR]
    destructOp (Expr { ty = ExprTypeRelWrapper, value = ExprValueOp op args}) = (op, args)
    (opL, argsL) = destructOp lhs
    (opR, argsR) = destructOp rhs

strengthenHyp :: Expr -> Expr
strengthenHyp = strengthenHypInner 1

weakenAssert :: Expr -> Expr
weakenAssert = strengthenHypInner (-1)

strengthenHypInner :: Integer -> Expr -> Expr
strengthenHypInner = go
  where
    go sign expr = case expr.value of
        ExprValueOp op args -> case op of
            _ | op == OpAnd || op == OpOr ->
                Expr expr.ty (ExprValueOp op (map goWith args))
            OpImplies ->
                let [l, r] = args
                in goAgainst l `impliesE` goWith r
            OpNot ->
                let [x] = args
                in notE (goAgainst x)
            OpStackEquals -> case sign of
                1 -> boolE (ExprValueOp OpImpliesStackEquals args)
                -1 -> boolE (ExprValueOp OpStackEqualsImplies args)
            OpROData -> case sign of
                1 -> boolE (ExprValueOp OpImpliesROData args)
                -1 -> expr
            OpEquals | isBoolT (head args).ty ->
                let [_l, r] = args
                    args' = applyWhen (r `elem` [trueE, falseE]) reverse args
                    [l', r'] = args'
                in if
                    | l' == trueE -> goWith r'
                    | l' == falseE -> goWith (notE r')
                    | otherwise -> expr
            _ -> expr
        _ -> expr
      where
        goWith = go sign
        goAgainst = go (-sign)

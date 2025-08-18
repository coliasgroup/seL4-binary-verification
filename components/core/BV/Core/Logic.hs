{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module BV.Core.Logic
    ( PValidInfo (..)
    , PValidKind (..)
    , PValidType (..)
    , alignOfType
    , alignValidIneq
    , applyRelWrapper
    , instEqAtVisit
    , isNodeNoop
    , pvalidAssertion1
    , pvalidAssertion2
    , pvalidKindFromOp
    , sizeOfType
    , splitScalarPairs
    , strengthenHyp
    , weakenAssert
    ) where

import BV.Core.Arch
import BV.Core.Structs
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Utils (ensure, ensureM)

import Control.DeepSeq (NFData)
import Data.Function (applyWhen)
import Data.List (nub, partition)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics

sizeOfType :: MonadStructs m => ExprType -> m Integer
sizeOfType = \case
    ExprTypeWord { bits } ->
        let (bytes, 0) = bits `divMod` 8
         in return bytes
    ExprTypeArray { ty, len } -> (* len) <$> sizeOfType ty
    ExprTypeStruct name -> (.size) <$> askStruct name
    ExprTypePtr _ -> return archPtrSizeBytes
    ExprTypeGlobalWrapper ty' -> sizeOfType ty'

alignOfType :: MonadStructs m => ExprType -> m Integer
alignOfType ty = case ty of
    ExprTypeWord { } -> sizeOfType ty
    ExprTypeArray { ty = ty' } -> alignOfType ty'
    ExprTypeStruct name -> (.align) <$> askStruct name
    ExprTypePtr _ -> return archPtrSizeBytes
    ExprTypeGlobalWrapper ty' -> alignOfType ty'

--

data PValidType
  = PValidTypeArray
      { ty :: ExprType
      , len :: Expr
      }
  | PValidTypeType ExprType
  deriving (Eq, Generic, NFData, Ord, Show)

data PValidKind
  = PValidKindPArrayValid
  | PValidKindPGlobalValid
  | PValidKindPValid
  | PValidKindPWeakValid
  deriving (Eq, Generic, NFData, Ord, Show)

data PValidTypeWithStrength
  = PValidTypeWithStrengthArray
      { ty :: ExprType
      , len :: Expr
      , strength :: Maybe PArrayValidStrength
      }
  | PValidTypeWithStrengthType ExprType
  deriving (Eq, Generic, NFData, Ord, Show)

data PArrayValidStrength
  = PArrayValidStrengthStrong
  | PArrayValidStrengthWeak
  deriving (Eq, Generic, NFData, Ord, Show)

pvalidKindFromOp :: Op -> PValidKind
pvalidKindFromOp = \case
    OpPValid -> PValidKindPValid
    OpPGlobalValid -> PValidKindPGlobalValid
    OpPArrayValid -> PValidKindPArrayValid
    OpPWeakValid -> PValidKindPWeakValid

pvalidTypeWithoutStrength :: PValidTypeWithStrength -> PValidType
pvalidTypeWithoutStrength = \case
    PValidTypeWithStrengthType ty -> PValidTypeType ty
    PValidTypeWithStrengthArray { ty, len } -> PValidTypeArray { ty, len }

pvalidTypeWithUnspecifiedStrength :: PValidType -> PValidTypeWithStrength
pvalidTypeWithUnspecifiedStrength = \case
    PValidTypeType ty -> PValidTypeWithStrengthType ty
    PValidTypeArray { ty, len } -> PValidTypeWithStrengthArray
        { ty
        , len
        , strength = Nothing
        }

pvalidTypeSize :: MonadStructs m => PValidType -> m Expr
pvalidTypeSize = \case
    PValidTypeType ty -> machineWordE <$> sizeOfType ty
    PValidTypeArray { ty, len } -> do
        elSize <- machineWordE <$> sizeOfType ty
        return $ elSize `timesE` len

pvalidTypeAlign :: MonadStructs m => PValidType -> m Integer
pvalidTypeAlign = \case
    PValidTypeType ty -> alignOfType ty
    PValidTypeArray { ty } -> alignOfType ty

alignValidIneq :: MonadStructs m => PValidType -> Expr -> m Expr
alignValidIneq pvTy p = do
    align <- pvalidTypeAlign pvTy
    size <- pvalidTypeSize pvTy
    sizeReqs <- case pvTy of
        PValidTypeType _ -> return []
        PValidTypeArray { ty, len } -> (:[]) <$> arraySizeIneq ty len
    ensureM $ align `elem` [1, 4, 8]
    return $ foldr1 andE $
        [ bitwiseAndE p (machineWordE (align - 1)) `eqE` w0 | align > 1 ]
            ++ sizeReqs
            ++ [ notE (p `eqE` w0)
               , (w0 `lessE` size) `impliesE` (p `lessEqE` negE size)
               ]
  where
    w0 = machineWordE 0

arraySizeIneq :: MonadStructs m => ExprType -> Expr -> m Expr
arraySizeIneq ty len = do
    elSize <- sizeOfType ty
    let limit = (memSize - archPtrSizeBytes) `div` elSize
    return $ len `lessEqE` machineWordE limit
  where
    memSize = 2 ^ archWordSizeBits

-- TODO improve name
data PValidInfo
  = PValidInfo
      { pvKind :: PValidKind
      , pvTy :: PValidType
      , p :: Expr
      , pv :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

pvalidAssertion1 :: MonadStructs m => PValidInfo -> PValidInfo -> m Expr
pvalidAssertion1 a b = do
    (cond1, out1) <- f a b
    (cond2, out2) <- f b a
    return $ (a.pv `andE` b.pv) `impliesE` foldr1 orE [cond1, cond2, out1, out2]
  where
    f c d = do
        let offs = c.p `minusE` d.p
        cond <- getSTypCondition offs c.pvTy d.pvTy
        size <- pvalidTypeSize c.pvTy
        let endAddr = c.p `plusE` (size `minusE` machineWordE 1)
        let out = endAddr `lessE` d.p
        return (cond, out)

pvalidAssertion2 :: MonadStructs m => PValidInfo -> PValidInfo -> m Expr
pvalidAssertion2 a b = do
    case (a.pvTy, b.pvTy) of
        (PValidTypeArray {}, PValidTypeArray {}) -> return trueE
        _ -> do
            imp1 <- f a b
            imp2 <- f b a
            return $ imp1 `andE` imp2
          where
            f c d = do
                let offs = c.p `minusE` d.p
                cond <- getSTypCondition offs c.pvTy d.pvTy
                return $ (cond `andE` d.pv) `impliesE` c.pv

getSTypCondition :: MonadStructs m =>  Expr -> PValidType -> PValidType -> m Expr
getSTypCondition offs innerTy outerTy =
    getSTypConditionInner1
        (pvalidTypeWithUnspecifiedStrength innerTy)
        (pvalidTypeWithUnspecifiedStrength outerTy)
            <&> \case
                Nothing -> falseE
                Just f -> f offs

-- TODO evaluate performance loss from not caching (see graph-refine)
getSTypConditionInner1 :: MonadStructs m => PValidTypeWithStrength -> PValidTypeWithStrength -> m (Maybe (Expr -> Expr))
getSTypConditionInner1 innerTy outerTy =
    getSTypConditionInner2
        (normalizeArrayType innerTy)
        (normalizeArrayType outerTy)

normalizeArrayType :: PValidTypeWithStrength -> PValidTypeWithStrength
normalizeArrayType = \case
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

getSTypConditionInner2 :: MonadStructs m => PValidTypeWithStrength -> PValidTypeWithStrength -> m (Maybe (Expr -> Expr))
getSTypConditionInner2 innerPvTy outerPvTy = case (innerPvTy, outerPvTy) of
    (PValidTypeWithStrengthArray { ty = innerElTy }, PValidTypeWithStrengthArray { strength = outerBound }) -> do
            condOpt <- getSTypConditionInner1 (PValidTypeWithStrengthType innerElTy) outerPvTy
            innerSize <- pvTySizeCompat innerPvTy
            outerSize <- pvTySizeCompat outerPvTy
            return $ case (outerBound, condOpt) of
                (Just PArrayValidStrengthStrong, Just cond) ->
                    Just $ \offs -> cond offs `andE` ((innerSize `plusE` offs) `lessEqE` outerSize)
                _ -> condOpt
    _ | innerPvTy == outerPvTy -> do
            return $ Just $ \offs -> offs `eqE` machineWordE 0
    (_, PValidTypeWithStrengthType (ExprTypeStruct outerStructName)) -> do
            askStruct outerStructName >>= doOuterStruct
    (_, PValidTypeWithStrengthType (ExprTypeGlobalWrapper inner)) -> do
            dummyGlobalWrapperStruct inner >>= doOuterStruct
    (_, PValidTypeWithStrengthArray { ty = outerElTy, strength = outerBound }) -> do
            condOpt <- getSTypConditionInner1 innerPvTy (PValidTypeWithStrengthType outerElTy)
            outerSize <- pvTySizeCompat outerPvTy
            outerElSize <- machineWordE <$> sizeOfType outerElTy
            return $ condOpt <&> \cond -> case outerBound of
                Just PArrayValidStrengthStrong ->
                    \offs -> (offs `lessE` outerSize) `andE` cond (offs `modulusE` outerElSize)
                _ ->
                    \offs -> cond (offs `modulusE` outerElSize)
    _ -> return Nothing
  where
    doOuterStruct outerStruct = do
        conds <- fmap catMaybes $ for (M.elems outerStruct.fields) $ \field -> do
            condOpt <- getSTypConditionInner1 innerPvTy (PValidTypeWithStrengthType field.ty)
            return $ condOpt <&> (, machineWordE field.offset)
        return $ case conds of
            [] -> Nothing
            _ -> Just $ \offs -> foldr1 orE
                [ c (offs `minusE` offs')
                | (c, offs') <- conds
                ]
    -- HACK don't use pvalidTypeSize, instead match op order of graph-refine
    pvTySizeCompat = \case
        PValidTypeWithStrengthType ty -> machineWordE <$> sizeOfType ty
        PValidTypeWithStrengthArray { ty, len } -> do
            elSize <- machineWordE <$> sizeOfType ty
            return $ len `timesE` elSize

dummyGlobalWrapperStruct :: MonadStructs m => ExprType -> m Struct
dummyGlobalWrapperStruct ty = do
    size <- sizeOfType ty
    align <- alignOfType ty
    return $ Struct
        { size
        , align
        , fields = M.fromList
            [ ( Ident "v"
              , StructField
                    { ty
                    , offset = 0
                    }
              )
            ]
        }

--

applyRelWrapper :: Expr -> Expr -> Expr
applyRelWrapper lhs rhs = if
    | ops == S.fromList [OpStackWrapper] ->
        let spL:stackL:exceptsL = argsL
            spR:stackR:exceptsR = argsR
            excepts = nub $ exceptsL ++ exceptsR
            f stack0 = foldl (\stack p -> memUpdE p stack (word32E 0)) stack0 excepts
         in boolE $ ExprValueOp OpStackEquals [spL, f stackL, spR, f stackR]
    | ops == S.fromList [OpMemAccWrapper, OpMemWrapper] ->
        let [[addr, val]] =
                [ args
                | Expr { value = ExprValueOp OpMemAccWrapper args } <- [lhs, rhs]
                ]
            [[mem]] =
                [ args
                | Expr { value = ExprValueOp OpMemWrapper args } <- [lhs, rhs]
                ]
         in ensure (addr.ty == word32T && mem.ty == memT) $
                eqE (memAccE val.ty addr mem) val
    | ops == S.fromList [OpEqSelectiveWrapper] ->
        let [valL, _, _] = argsL
            [valR, _, _] = argsR
         in if valL.ty == ExprTypeRelWrapper
            then applyRelWrapper valL valR
            else eqE lhs rhs
  where
    ops = S.fromList [opL, opR]
    destructOp (Expr { ty = ExprTypeRelWrapper, value = ExprValueOp op args}) = (op, args)
    (opL, argsL) = destructOp lhs
    (opR, argsR) = destructOp rhs

--

splitScalarPairs :: [NameTy] -> ([NameTy], [NameTy], [NameTy])
splitScalarPairs args = (scalars, mems, others)
  where
    (scalars, globals) = span (\arg -> isWordT arg.ty || isBoolT arg.ty) args
    (mems, others) = partition (\arg -> isMemT arg.ty) globals

--

instEqAtVisit :: VisitCount -> Expr -> Bool
instEqAtVisit visit expr = case expr.value of
    ExprValueOp OpEqSelectiveWrapper [_, xs, ys] -> case fromJust (simpleVC visit) of
        SimpleVisitCountViewNumber n -> n `elem` numListFromSum xs
        SimpleVisitCountViewOffset n -> n `elem` numListFromSum ys
    _ -> True

numListFromSum :: Expr -> [Integer]
numListFromSum = go
  where
    go expr = case expr.value of
        ExprValueOp OpPlus [Expr _ (ExprValueNum n), rhs] -> n : go rhs
        ExprValueNum n -> [n]
        _ -> []

--

strengthenHyp :: Expr -> Expr
strengthenHyp = strengthenHypInner True

weakenAssert :: Expr -> Expr
weakenAssert = strengthenHypInner False

strengthenHypInner :: Bool -> Expr -> Expr
strengthenHypInner = go
  where
    go direction expr = case expr.value of
        ExprValueOp op args -> case op of
            _ | op == OpAnd || op == OpOr ->
                expr & exprOpArgs % mapped %~ goWith
            OpImplies ->
                let [l, r] = args
                 in goAgainst l `impliesE` goWith r
            OpNot ->
                let [x] = args
                 in notE (goAgainst x)
            OpStackEquals ->
                let op' = if direction then OpImpliesStackEquals else OpStackEqualsImplies
                 in expr & exprOp .~ op'
            OpROData ->
                applyWhen direction (exprOp .~ OpImpliesROData) expr
            OpEquals | isBoolT (head args).ty ->
                let [_, r] = args
                    [l', r'] = applyWhen (r `elem` [trueE, falseE]) reverse args
                 in if | l' == trueE -> goWith r'
                       | l' == falseE -> goWith (notE r')
                       | otherwise -> expr
            _ -> expr
        _ -> expr
      where
        goWith = go direction
        goAgainst = go (not direction)

--

isNodeNoop :: Node -> Bool
isNodeNoop = \case
    NodeBasic (BasicNode { varUpdates }) -> null varUpdates
    NodeCond (CondNode { left, right }) -> left == right
    NodeCall (CallNode {}) -> False

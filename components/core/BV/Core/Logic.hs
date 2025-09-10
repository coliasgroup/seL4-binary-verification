{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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
import BV.Utils (ensureM)

import Control.DeepSeq (NFData)
import Data.Function (applyWhen, on)
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
      , strength :: PArrayValidStrength
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

augmentPValidTypeWithStrength :: PArrayValidStrength -> PValidType -> PValidTypeWithStrength
augmentPValidTypeWithStrength strength = \case
    PValidTypeType ty -> PValidTypeWithStrengthType ty
    PValidTypeArray { ty, len } -> PValidTypeWithStrengthArray
        { ty
        , len
        , strength
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
        [ (p `bitwiseAndE` machineWordE (align - 1)) `eqE` w0 | align > 1 ]
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

data PValidInfo
  = PValidInfo
      { pvKind :: PValidKind
      , pvTy :: PValidType
      , p :: Expr
      , pv :: Expr
      }
  deriving (Eq, Generic, NFData, Ord, Show)

-- First pointer validity assertion: incompatibility.
-- pvalid1 & pvalid2 --> non-overlapping OR somehow-contained.
pvalidAssertion1 :: MonadStructs m => PValidInfo -> PValidInfo -> m Expr
pvalidAssertion1 x y = do
    p1 <- x `completelyPrecedes` y
    p2 <- y `completelyPrecedes` x
    p3 <- x `pvalidContains` y
    p4 <- y `pvalidContains` x
    -- NOTE order matches graph-refine
    let conj = foldr1 orE [p4, p3, p1, p2]
    return $ (x.pv `andE` y.pv) `impliesE` conj
  where
    completelyPrecedes lo hi = do
        loSize <- pvalidTypeSize lo.pvTy
        return $ (lo.p `plusE` (loSize `minusE` machineWordE 1)) `lessE` hi.p

-- Second pointer validity assertion: implication.
-- pvalid1 & strictly-contained --> pvalid2
pvalidAssertion2 :: MonadStructs m => PValidInfo -> PValidInfo -> m Expr
pvalidAssertion2 x y = do
    case (x.pvTy, y.pvTy) of
        (PValidTypeArray {}, PValidTypeArray {}) ->
            -- Ignore this case. See note in graph-refine
            return trueE
        _ -> do
            p1 <- x `wouldImply` y
            p2 <- y `wouldImply` x
            -- NOTE order matches graph-refine
            return $ p2 `andE` p1
          where
            wouldImply outer inner = do
                contained <- outer `pvalidContains` inner
                return $ (contained `andE` outer.pv) `impliesE` inner.pv

pvalidContains :: MonadStructs m => PValidInfo -> PValidInfo -> m Expr
pvalidContains outer inner =
    (getSubtypeCondition `on` augmentPValidTypeWithStrength PArrayValidStrengthWeak)
        inner.pvTy
        outer.pvTy
            <&> \case
                Nothing -> falseE
                Just cond -> cond (inner.p `minusE` outer.p)

getSubtypeCondition :: MonadStructs m => PValidTypeWithStrength -> PValidTypeWithStrength -> m (Maybe (Expr -> Expr))
getSubtypeCondition = go

  where

    go = goNorm `on` normalizeArrayType

    goNorm innerPvTy outerPvTy = case (innerPvTy, outerPvTy) of
        ( PValidTypeWithStrengthArray { ty = innerElTy }
            , PValidTypeWithStrengthArray { strength = outerBound }
            ) -> do
                condOpt <- go (PValidTypeWithStrengthType innerElTy) outerPvTy
                case (outerBound, condOpt) of
                    (PArrayValidStrengthStrong, Just cond) -> do
                        innerSize <- pvTySizeCompat innerPvTy
                        outerSize <- pvTySizeCompat outerPvTy
                        return $ Just $
                            \offs -> cond offs `andE` ((innerSize `plusE` offs) `lessEqE` outerSize)
                    _ -> return condOpt
        _ | innerPvTy == outerPvTy -> do
                return $ Just $ \offs -> offs `eqE` machineWordE 0
        (_, PValidTypeWithStrengthType (ExprTypeStruct outerStructName)) -> do
                askStruct outerStructName >>= goNormStruct innerPvTy
        (_, PValidTypeWithStrengthType (ExprTypeGlobalWrapper inner)) -> do
                dummyGlobalWrapperStruct inner >>= goNormStruct innerPvTy
        (_, PValidTypeWithStrengthArray { ty = outerElTy, strength = outerBound }) -> do
                condOpt <- go innerPvTy (PValidTypeWithStrengthType outerElTy)
                outerSize <- pvTySizeCompat outerPvTy
                outerElSize <- machineWordE <$> sizeOfType outerElTy
                return $ condOpt <&> \cond -> case outerBound of
                    PArrayValidStrengthStrong ->
                        \offs -> (offs `lessE` outerSize) `andE` cond (offs `modulusE` outerElSize)
                    _ ->
                        \offs -> cond (offs `modulusE` outerElSize)
        _ -> return Nothing

    goNormStruct innerPvTy outerStruct = do
        conds <- fmap catMaybes $ for (M.elems outerStruct.fields) $ \field -> do
            let contextualize cond offs = cond (offs `minusE` machineWordE field.offset)
            over _Just contextualize <$> go innerPvTy (PValidTypeWithStrengthType field.ty)
        return $ case conds of
            [] -> Nothing
            _ -> Just $ \offs -> foldr1 orE [ cond offs | cond <- conds]

    normalizeArrayType = \case
        PValidTypeWithStrengthType (ExprTypeArray { ty, len }) -> PValidTypeWithStrengthArray
            { ty
            , len = machineWordE len
            , strength = PArrayValidStrengthStrong
            }
        pvTy -> pvTy

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
    ExprValueOp OpEqSelectiveWrapper [_, xs, ys] -> case fromJust (toSimpleVC visit) of
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

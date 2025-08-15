{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module BV.Core.Logic
    ( PValidKind (..)
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
import Data.Functor ((<&>))
import Data.List (nub, partition)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)

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

splitScalarPairs :: [NameTy] -> ([NameTy], [NameTy], [NameTy])
splitScalarPairs args = (scalars, mems, others)
  where
    (scalars, globals) = span (\arg -> isWordT arg.ty || isBoolT arg.ty) args
    (mems, others) = partition (\arg -> isMemT arg.ty) globals

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

pvalidTypeWithUnspecifiedStrength :: PValidType -> PValidTypeWithStrength
pvalidTypeWithUnspecifiedStrength = \case
    PValidTypeType ty -> PValidTypeWithStrengthType ty
    PValidTypeArray { ty, len } -> PValidTypeWithStrengthArray
        { ty
        , len
        , strength = Nothing
        }

alignValidIneq :: MonadStructs m => PValidType -> Expr -> m Expr
alignValidIneq pvTy p = do
    (align, size, sizeReqs) <- case pvTy of
        PValidTypeType ty -> do
            align <- alignOfType ty
            size <- machineWordE <$> sizeOfType ty
            return (align, size, [])
        PValidTypeArray { ty, len } -> do
            align <- alignOfType ty
            elSize <- machineWordE <$> sizeOfType ty
            let size = timesE elSize len
            sizeReq <- arraySizeIneq ty len p
            return (align, size, [sizeReq])
    ensureM $ align `elem` [1, 4, 8]
    let conj =
            [ bitwiseAndE p (machineWordE (align - 1)) `eqE` w0 | align > 1 ]
                ++ sizeReqs
                ++ [ notE (p `eqE` w0)
                   , (w0 `lessE` size) `impliesE` (p `lessEqE` negE size)
                   ]
    return $ foldr1 andE conj
  where
    w0 = machineWordE 0

arraySizeIneq :: MonadStructs m => ExprType -> Expr -> Expr -> m Expr
arraySizeIneq ty len _p = do
    elSize <- sizeOfType ty
    let limit = (((2 :: Integer) ^ (32 :: Integer)) - 4) `div` elSize
    return $ lessEqE len (machineWordE limit)

endAddr :: MonadStructs m => Expr -> PValidType -> m Expr
endAddr p pvTy = do
    size <- case pvTy of
        PValidTypeArray { ty, len } -> do
            elSize <- sizeOfType ty
            return $ machineWordE elSize `timesE` len
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
        (normArrayType innerTy)
        (normArrayType outerTy)

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
        outerStruct <- askStruct outerStructName
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
    (_, PValidTypeWithStrengthType (ExprTypeGlobalWrapper inner)) -> do
        fOpt <- getSTypConditionInner1 innerPvTy (PValidTypeWithStrengthType inner)
        return $ fOpt <&> \f offs -> f (minusE offs (machineWordE 0)) -- HACK `- 0` to match graph-refine
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
applyRelWrapper lhs rhs = if
    | ops == S.fromList [OpStackWrapper] ->
        let sp1:st1:rest1 = argsL
            sp2:st2:rest2 = argsR
            excepts = nub $ rest1 ++ rest2
            f st0 = foldl (\st p -> memUpdE p st (word32E 0)) st0 excepts
         in boolE $ ExprValueOp OpStackEquals [sp1, f st1, sp2, f st2]
    | ops == S.fromList [OpMemAccWrapper, OpMemWrapper] ->
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
    | ops == S.fromList [OpEqSelectiveWrapper] ->
        let [lhsV, _, _] = argsL
            [rhsV, _, _] = argsR
         in if lhsV.ty == ExprTypeRelWrapper
            then applyRelWrapper lhsV rhsV
            else eqE lhs rhs
    | otherwise -> error ""
  where
    ops = S.fromList [opL, opR]
    destructOp (Expr { ty = ExprTypeRelWrapper, value = ExprValueOp op args}) = (op, args)
    (opL, argsL) = destructOp lhs
    (opR, argsR) = destructOp rhs

--

instEqAtVisit :: VisitCount -> Expr -> Bool
instEqAtVisit visit expr = case expr.value of
    ExprValueOp OpEqSelectiveWrapper [_, xs, ys] -> case fromJust (simpleVC visit) of
        SimpleVisitCountViewNumber n -> n `elem` sumElems xs
        SimpleVisitCountViewOffset n -> n `elem` sumElems ys
    _ -> True
  where
    sumElems expr' = case expr'.value of
        ExprValueNum n -> [n]
        ExprValueOp OpPlus [Expr _ (ExprValueNum n), rhs] -> n : sumElems rhs
        _ -> []

--

strengthenHypInner :: Bool -> Expr -> Expr
strengthenHypInner = go
  where
    go direction expr = case expr.value of
        ExprValueOp op args -> case op of
            _ | op == OpAnd || op == OpOr ->
                Expr expr.ty (ExprValueOp op (map goWith args))
            OpImplies ->
                let [l, r] = args
                in goAgainst l `impliesE` goWith r
            OpNot ->
                let [x] = args
                in notE (goAgainst x)
            OpStackEquals -> boolE $
                ExprValueOp (if direction then OpImpliesStackEquals else OpStackEqualsImplies) args
            OpROData -> if direction then boolE (ExprValueOp OpImpliesROData args) else expr
            OpEquals | isBoolT (head args).ty ->
                let [_, r] = args
                    [l', r'] = applyWhen (r `elem` [trueE, falseE]) reverse args
                in if
                    | l' == trueE -> goWith r'
                    | l' == falseE -> goWith (notE r')
                    | otherwise -> expr
            _ -> expr
        _ -> expr
      where
        goWith = go direction
        goAgainst = go (not direction)

strengthenHyp :: Expr -> Expr
strengthenHyp = strengthenHypInner True

weakenAssert :: Expr -> Expr
weakenAssert = strengthenHypInner False

--

isNodeNoop :: Node -> Bool
isNodeNoop = \case
    NodeBasic (BasicNode { varUpdates }) -> null varUpdates
    NodeCond (CondNode { left, right }) -> left == right
    NodeCall (CallNode {}) -> False

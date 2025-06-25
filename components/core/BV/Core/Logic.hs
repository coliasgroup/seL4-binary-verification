{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
import Control.Monad.Reader (MonadReader (ask), Reader, ReaderT (runReaderT),
                             asks, runReader)
import Control.Monad.Trans (lift)
import Data.Foldable (fold)
import Data.Foldable1 (Foldable1 (fold1))
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Traversable (for)
import Debug.Trace (trace)
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
    ExprTypeArray { ty, length } -> (* length) <$> sizeOfType ty
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

isNodeNoop :: Node -> Bool
isNodeNoop = \case
    NodeBasic (BasicNode { varUpdates }) -> null varUpdates
    NodeCond (CondNode { left, right }) -> left == right
    NodeCall (CallNode {}) -> False

--

data PValidType
  = PValidTypeType ExprType
  | PValidTypeArray
      { ty :: ExprType
      , len :: Expr
      }
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
arraySizeIneqM ty len p = do
    -- align <- alignOfType ty
    elSize <- sizeOfType ty
    -- let size = timesE (machineWordE elSize) len
    let size_lim = (((2 :: Integer) ^ (32 :: Integer)) - 4) `div` elSize
    return $ lessEqE len (machineWordE size_lim)

alignValidIneqM :: MonadStructs m => PValidType -> Expr -> m Expr
alignValidIneqM pvTy p = do
    (align, size, size_req) <- case pvTy of
        PValidTypeType ty -> do
            align <- alignOfType ty
            size <- machineWordE <$> sizeOfType ty
            let size_req = []
            return (align, size, size_req)
        PValidTypeArray { ty, len } -> do
            align <- alignOfType ty
            elSize <- machineWordE <$> sizeOfType ty
            let size = timesE elSize len
            x <- arraySizeIneqM ty len p
            let size_req = [x]
            return (align, size, size_req)
    let w0 = machineWordE 0
    let conj = optionals (align > 1) [bitwiseAndE p (machineWordE (align - 1)) `eqE` w0] ++ size_req ++
            [ notE (p `eqE` w0)
            , (w0 `lessE` size) `impliesE` (p `lessEqE` negE size)
            ]
    return $ ensure (align `elem` [1, 4, 8]) $
        foldr1 andE conj

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
  = PValidKindGlobal
  | PValidKindWeak
  | PValidKind
  | PValidKindArray
  deriving (Eq, Generic, NFData, Ord, Show)

pvalidKindFromOp :: Op -> PValidKind
pvalidKindFromOp = \case
    OpPValid -> PValidKind
    OpPGlobalValid -> PValidKindGlobal
    OpPArrayValid -> PValidKindArray
    OpPWeakValid -> PValidKindWeak

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
    PValidTypeWithStrengthType (ExprTypeArray { ty, length }) -> PValidTypeWithStrengthArray
        { ty
        , len = machineWordE length
        , strength = Just PArrayValidStrengthStrong
        }
    PValidTypeWithStrengthArray { ty, len, strength = Nothing } -> PValidTypeWithStrengthArray
        { ty
        , len
        , strength = Just PArrayValidStrengthWeak
        }
    pvTy -> pvTy

pvalidAssertion1 :: MonadStructs m => (PValidType, PValidKind, Expr, Expr) -> (PValidType, PValidKind, Expr, Expr) -> m Expr
pvalidAssertion1 (typ, k, p, pv) (typ2, k2, p2, pv2) = do
    let offs1 = p `minusE` p2
    cond1 <- getSTypCondition offs1 typ typ2
    let offs2 = p2 `minusE` p
    cond2 <- getSTypCondition offs2 typ2 typ
    out1 <- lessE <$> endAddr p typ <*> pure p2
    out2 <- lessE <$> endAddr p2 typ2 <*> pure p
    return $ (pv `andE` pv2) `impliesE` (foldr1 orE [cond1, cond2, out1, out2])

getSTypCondition :: MonadStructs m =>  Expr -> PValidType -> PValidType -> m Expr
getSTypCondition offs innerTy outerTy = getSTypConditionInner1
    (pvalidTypeWithUnspecifiedStrength innerTy)
    (pvalidTypeWithUnspecifiedStrength outerTy) <&> \case
        Nothing -> falseE
        Just f -> f offs

getSTypConditionInner1 :: MonadStructs m => PValidTypeWithStrength -> PValidTypeWithStrength -> m (Maybe (Expr -> Expr))
getSTypConditionInner1 innerTy outerTy = do
    let innerTyNorm = normArrayType innerTy
    let outerTyNorm = normArrayType outerTy
    -- TODO cache on (innerTyNorm, outerTyNorm)
    getSTypConditionInner2 innerTyNorm outerTyNorm

arrayTypeSize :: MonadStructs m => PValidTypeWithStrength -> m Expr
arrayTypeSize (PValidTypeWithStrengthArray { ty, len }) = do
    size <- sizeOfType ty
    return $ timesE len (machineWordE size)

getSTypConditionInner2 :: MonadStructs m => PValidTypeWithStrength -> PValidTypeWithStrength -> m (Maybe (Expr -> Expr))
getSTypConditionInner2 innerPvTy outerPvTy = case (innerPvTy, outerPvTy) of
    ( PValidTypeWithStrengthArray { ty = innerElTy }
        , PValidTypeWithStrengthArray { strength = outerBound }
        ) -> do
            cond <- getSTypConditionInner1 (PValidTypeWithStrengthType innerElTy) outerPvTy
            innerSize <- arrayTypeSize innerPvTy
            outerSize <- arrayTypeSize outerPvTy
            return $ case (outerBound, cond) of
                (Just PArrayValidStrengthStrong, Just cond') -> Just $ \offs -> andE (cond' offs) (lessEqE (plusE innerSize offs) outerSize)
                _ -> cond
    _ | innerPvTy == outerPvTy -> do
        return $ Just $ \offs -> eqE offs (machineWordE 0)
    (_, PValidTypeWithStrengthType (ExprTypeStruct outerStructName)) -> do
        outerStruct <- lookupStruct outerStructName
        conds <- catMaybes <$> (for (M.elems outerStruct.fields) (\field -> do
            mf <- getSTypConditionInner1 innerPvTy (PValidTypeWithStrengthType field.ty)
            return $ (, machineWordE field.offset) <$> mf
            ))
        case conds of
            [] -> return Nothing
            _ -> return $ Just $ \offs -> foldr1 orE
                [ c (minusE offs offs2)
                | (c, offs2) <- conds
                ]
    (_, PValidTypeWithStrengthArray { ty = outerElTy, len = outerLen, strength = outerBound }) -> do
            cond <- getSTypConditionInner1 innerPvTy (PValidTypeWithStrengthType outerElTy)
            outerElSize <- machineWordE <$> sizeOfType outerElTy
            let outerSize = timesE outerLen outerElSize
            return $ case cond of
                Just cond' -> Just $ case outerBound of
                    Just PArrayValidStrengthStrong -> \offs -> andE (lessE offs outerSize) (cond' (modulusE offs outerElSize))
                    _ -> \offs -> cond' (modulusE offs outerElSize)
                _ -> Nothing
    _ -> return Nothing

pvalidAssertion2 :: MonadStructs m => (PValidType, PValidKind, Expr, Expr) -> (PValidType, PValidKind, Expr, Expr) -> m Expr
pvalidAssertion2 (typ, k, p, pv) (typ2, k2, p2, pv2) = do
    case (typ, typ2) of
        (PValidTypeArray {}, PValidTypeArray {}) -> return trueE
        _ -> do
            let offs1 = p `minusE` p2
            cond1 <- getSTypCondition offs1 typ typ2
            let imp1 = impliesE (andE cond1 pv2) pv
            let offs2 = p2 `minusE` p
            cond2 <- getSTypCondition offs2 typ2 typ
            let imp2 = impliesE (andE cond2 pv) pv2
            return $ imp1 `andE` imp2

--

applyRelWrapper :: Expr -> Expr -> Expr
applyRelWrapper lhs rhs =
    case () of
        _ | ops == S.fromList [OpStackWrapper] ->
            let sp1:st1:rest1 = argsL
                sp2:st2:rest2 = argsR
                excepts = nub $ rest1 ++ rest2
                f st0 = foldr (\p st -> memUpdE st p (word32E 0)) st0 excepts
             in boolE $ ExprValueOp OpImpliesStackEquals [sp1, (f st1), sp2, (f st2)]
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

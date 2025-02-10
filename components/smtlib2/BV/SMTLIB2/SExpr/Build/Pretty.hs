{-# LANGUAGE OverloadedStrings #-}

module BV.SMTLIB2.SExpr.Build.Pretty
    ( PrettySExprConfig (..)
    , buildGenericSExprPretty
    , buildSExprPretty
    , defaultDelimsAt
    , defaultMinBreak
    , defaultPrettySExprConfig
    ) where

import BV.SMTLIB2.SExpr
import BV.SMTLIB2.SExpr.Build

import Control.Applicative (empty)
import Control.Monad (when)
import Control.Monad.State (evalStateT, get, put)
import Control.Monad.Writer (execWriterT, tell)
import Data.ByteString.Builder ()
import Data.Function (applyWhen)
import Data.List (genericReplicate, intersperse)
import Data.Maybe (isJust)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, fromLazyText, toLazyText)
import GHC.Generics (Generic)
import Optics

data PrettySExprConfig
  = PrettySExprConfig
      { preferredMaxLineWidth :: Integer
      , indentWidth :: Integer
      , minBreak :: Maybe Integer
      , delimsAt :: Integer -> (Builder, Builder)
      }
  deriving (Generic)

defaultPrettySExprConfig :: PrettySExprConfig
defaultPrettySExprConfig = PrettySExprConfig
    { preferredMaxLineWidth = 80
    , indentWidth = 2
    , minBreak = Just defaultMinBreak
    , delimsAt = defaultDelimsAt
    }

defaultMinBreak :: Integer
defaultMinBreak = 40

defaultDelimsAt :: Integer -> (Builder, Builder)
defaultDelimsAt _ = ("(", ")")

buildSExprPretty :: PrettySExprConfig -> SExpr -> Builder
buildSExprPretty = buildGenericSExprPretty buildAtom

buildGenericSExprPretty :: (a -> Builder) -> PrettySExprConfig -> GenericSExpr a -> Builder
buildGenericSExprPretty f config sexpr = buildAt config initialSpacingCtx partial <> "\n"
  where
    initialSpacingCtx = SpacingContext
        { startCol = 0
        , nestLevel = 0
        }
    partial = sexpr <&> \a ->
        let built = toLazyText (f a)
         in BuiltAtom
                { builder = fromLazyText built
                , length = toInteger (TL.length built)
                }

data BuiltAtom
  = BuiltAtom
      { builder :: Builder
      , length :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

data SpacingContext
  = SpacingContext
      { startCol :: Integer
      , nestLevel :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

buildAt :: PrettySExprConfig -> SpacingContext -> GenericSExpr BuiltAtom -> Builder
buildAt config = go
  where
    go spacingCtx sexpr =
        let computedLimit = config.preferredMaxLineWidth - spacingCtx.startCol
            limit = maybe id max config.minBreak computedLimit
         in case buildInlineWithin config.delimsAt limit spacingCtx.nestLevel sexpr of
                Just built -> built
                Nothing -> case sexpr of
                    Atom atom -> atom.builder
                    List sexprs ->
                        let (openDelim, closeDelim) = config.delimsAt spacingCtx.nestLevel
                            startCol' = spacingCtx.startCol + 1
                            startsWithList = isJust $ sexprs ^? _head % #_List
                            sepIndentCols = applyWhen (not startsWithList) (+ config.indentWidth) startCol'
                            sep = "\n" <> mconcat (genericReplicate sepIndentCols " ")
                            defaultSpacingContext = SpacingContext
                                { startCol = startCol'
                                , nestLevel = spacingCtx.nestLevel + 1
                                }
                            sexprs' = map
                                (uncurry go)
                                (map (defaultSpacingContext,) sexprs
                                    & _tail % traversed % _1 % #startCol .~ sepIndentCols)
                            in openDelim <> mconcat (intersperse sep sexprs') <> closeDelim

buildInlineWithin
    :: (Integer -> (Builder, Builder))
    -> Integer
    -> Integer
    -> GenericSExpr BuiltAtom
    -> Maybe Builder
buildInlineWithin delimsAt limit initNestLevel =
    execWriterT . flip evalStateT limit . go initNestLevel
  where
    go curNestLevel = \case
        Atom a -> do
            putAtom a
        List xs -> do
            let (openDelim, closeDelim) = delimsAt curNestLevel
            putSingle openDelim
            sequence_ $ intersperse (putSingle " ") (map (go (curNestLevel + 1)) xs)
            putSingle closeDelim
    reserve n = do
        available <- get
        when (n > available) empty
        put $ available - n
    putSingle builder = do
        reserve 1
        tell builder
    putAtom builtAtom = do
        reserve builtAtom.length
        tell builtAtom.builder

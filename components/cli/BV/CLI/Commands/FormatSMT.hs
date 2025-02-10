{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}

module BV.CLI.Commands.FormatSMT
    ( formatSMTC
    , runFormatSMT
    ) where

import BV.CLI.Opts
import BV.ConcreteSyntax
import BV.Core.Types (SExprWithPlaceholders)
import BV.Logging
import BV.SMTLIB2.SExpr.Build.Pretty
import BV.SMTLIB2.SExpr.Parse.Attoparsec

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (fromLogStr)
import Control.Monad.State (get, put)
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toChunks)
import Data.Conduit (ConduitT, Flush (Chunk, Flush), runConduit, (.|))
import Data.Conduit.Attoparsec (conduitParser)
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Lift (evalStateLC)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT (decodeUtf8)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (isJust)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Optics
import Options.Applicative (many)
import Options.Applicative.Help.Pretty (indent)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid),
                            ConsoleLayer (Foreground), SGR (Reset, SetColor),
                            setSGRCode)
import System.Exit (die)
import System.IO (stdout)

runFormatSMT :: (MonadThrow m, MonadIO m, MonadLoggerWithContext m) => FormatSMTOpts -> m ()
runFormatSMT opts = do
    runConduit $
        C.stdin
        .| CT.decodeUtf8
        .| conduitParser (Just <$> parseSExprWithPlaceholdersFaster <|> Nothing <$ consumeSomeSExprWhitespace)
        .| CL.map snd
        .| CL.concat
        .| formatSMTC opts.lineWrappingOpts
        .| C.sinkHandleFlush stdout

formatSMTC :: Monad m => Maybe LineWrappingOpts -> ConduitT SExprWithPlaceholders (Flush B.ByteString) m ()
formatSMTC maybeOpts =
       CL.map build
    .| CL.concatMap (\built -> map Chunk (toChunks (encodeUtf8 (toLazyText built))) ++ [Flush])
  where
    build = case maybeOpts of
        Nothing ->
            \sexpr -> buildSExprWithPlaceholders sexpr <> "\n"
        Just opts ->
            buildGenericSExprPretty buildAtomOrPlaceholder $ PrettySExprConfig
                { preferredMaxLineWidth = opts.preferredMaxLineWidth
                , indentWidth = opts.indentWidth
                , minBreak = opts.minBreak
                , delimsAt = if opts.color then colorDelimsAt else defaultDelimsAt
                }

colorDelimsAt :: Integer -> (Builder, Builder)
colorDelimsAt nestLevel = (wrap "(", wrap ")")
  where
    color = selectColor nestLevel
    setCope = setSGRCode [SetColor Foreground Vivid color]
    resetCode = setSGRCode [Reset]
    wrap b = fromString setCope <> b <> fromString resetCode

selectColor :: Integer -> Color
selectColor nestLevel = colorByNestLevel ! (fromInteger nestLevel `mod` V.length colorByNestLevel)

colorByNestLevel :: Vector Color
colorByNestLevel =
    [ Red
    , Green
    , Yellow
    , Blue
    , Magenta
    , Cyan
    ]

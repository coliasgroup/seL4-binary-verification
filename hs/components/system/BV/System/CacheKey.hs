{-# LANGUAGE OverloadedStrings #-}

module BV.System.CacheKey
    ( cacheKeyForCheck
    ) where

import BV.ConcreteSyntax (buildSExprWithPlaceholders)
import BV.Core.Types

import Crypto.Hash.SHA256 (hashlazy)
import Data.ByteString (ByteString)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)

cacheKeyForCheck :: SMTProofCheck () -> ByteString
cacheKeyForCheck check = hashlazy bytes
  where
    bytes = encodeUtf8 normalizedText
    normalizedText = toLazyText $
        foldMap ((<> "\n") . buildSExprWithPlaceholders) (check.imp.term : check.setup)

{-# LANGUAGE OverloadedStrings #-}

module BV.System.CacheKey
    ( cacheKeyForCheck
    ) where

import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Crypto.Hash.SHA256 (hashlazy)

import BV.Core.Types
import BV.ConcreteSyntax (buildSExprWithPlaceholders)

cacheKeyForCheck :: SMTProofCheck () -> ByteString
cacheKeyForCheck check = hashlazy bytes
  where
    bytes = encodeUtf8 normalizedText
    normalizedText = toLazyText $
        foldMap ((<> "\n") . buildSExprWithPlaceholders) (check.imp.term:check.setup)

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.System.Fingerprinting
    ( SMTProofCheckFingerprint (..)
    , SMTProofCheckGroupFingerprint (..)
    , prettySMTProofCheckFingerprint
    , prettySMTProofCheckGroupFingerprint
    , smtProofCheckFingerprint
    , smtProofCheckGroupFingerprint
    ) where

import BV.ConcreteSyntax (buildSExprWithPlaceholders)
import BV.Core.Types

import Crypto.Hash.SHA256 (hashlazy)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import GHC.IsList (fromList)
import Text.Printf (PrintfArg (formatArg), formatString)

newtype SMTProofCheckFingerprint
  = SMTProofCheckFingerprint { unwrap :: ByteString }
  deriving (Eq, Generic, Ord, Show)

smtProofCheckFingerprint :: SMTProofCheck a -> SMTProofCheckFingerprint
smtProofCheckFingerprint check =
    SMTProofCheckFingerprint . hashlazy . encodeUtf8 . toLazyText . buildSExprWithPlaceholders $
        [ "SMTProofCheck"
        , ["setup", fromList check.setup]
        , ["imp", check.imp.term]
        ]

newtype SMTProofCheckGroupFingerprint
  = SMTProofCheckGroupFingerprint { unwrap :: ByteString }
  deriving (Eq, Generic, Ord, Show)

smtProofCheckGroupFingerprint :: SMTProofCheckGroup a -> SMTProofCheckGroupFingerprint
smtProofCheckGroupFingerprint group =
    SMTProofCheckGroupFingerprint . hashlazy . encodeUtf8 . toLazyText . buildSExprWithPlaceholders $
        [ "SMTProofCheckGroup"
        , ["setup", fromList group.setup]
        , ["imps", fromList (map (.term) group.imps)]
        ]

prettyHash :: ByteString -> String
prettyHash = C.unpack . B16.encode

prettySMTProofCheckFingerprint :: SMTProofCheckFingerprint -> String
prettySMTProofCheckFingerprint = prettyHash . (.unwrap)

prettySMTProofCheckGroupFingerprint :: SMTProofCheckGroupFingerprint -> String
prettySMTProofCheckGroupFingerprint = prettyHash . (.unwrap)

instance PrintfArg SMTProofCheckFingerprint where
  formatArg = formatString . prettySMTProofCheckFingerprint

instance PrintfArg SMTProofCheckGroupFingerprint where
  formatArg = formatString . prettySMTProofCheckGroupFingerprint

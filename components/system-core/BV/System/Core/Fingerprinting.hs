{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.System.Core.Fingerprinting
    ( CheckFingerprint (..)
    , CheckFingerprintPattern (..)
    , CheckGroupFingerprint (..)
    , CheckGroupFingerprintPattern (..)
    , fingerprintCheck
    , fingerprintCheckGroup
    , matchCheckFingerprint
    , matchCheckGroupFingerprint
    , prettyCheckFingerprint
    , prettyCheckFingerprintShort
    , prettyCheckGroupFingerprint
    , prettyCheckGroupFingerprintShort
    ) where

import BV.ConcreteSyntax
import BV.Core

import Control.DeepSeq (NFData)
import Crypto.Hash.SHA256 (hashlazy)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import GHC.IsList (fromList)

newtype CheckFingerprint
  = CheckFingerprint { unwrap :: ByteString }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

fingerprintCheck :: SMTProofCheck a -> CheckFingerprint
fingerprintCheck check =
    CheckFingerprint . hashlazy . encodeUtf8 . toLazyText . buildSExprWithPlaceholders $
        [ "SMTProofCheck"
        , ["setup", fromList check.setup]
        , ["imp", check.imp.term]
        ]

newtype CheckGroupFingerprint
  = CheckGroupFingerprint { unwrap :: ByteString }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

fingerprintCheckGroup :: SMTProofCheckGroup a -> CheckGroupFingerprint
fingerprintCheckGroup group =
    CheckGroupFingerprint . hashlazy . encodeUtf8 . toLazyText . buildSExprWithPlaceholders $
        [ "SMTProofCheckGroup"
        , ["setup", fromList group.setup]
        , ["imps", fromList (map (.term) group.imps)]
        ]

prettyHash :: ByteString -> String
prettyHash = C.unpack . B16.encode

prettyCheckFingerprint :: CheckFingerprint -> String
prettyCheckFingerprint = prettyHash . (.unwrap)

prettyCheckGroupFingerprint :: CheckGroupFingerprint -> String
prettyCheckGroupFingerprint = prettyHash . (.unwrap)

shortFingerprintLength :: Int
shortFingerprintLength = 12

prettyCheckFingerprintShort :: CheckFingerprint -> String
prettyCheckFingerprintShort = take shortFingerprintLength . prettyCheckFingerprint

prettyCheckGroupFingerprintShort :: CheckGroupFingerprint -> String
prettyCheckGroupFingerprintShort = take shortFingerprintLength . prettyCheckGroupFingerprint

--

newtype CheckFingerprintPattern
  = CheckFingerprintPattern { unwrap :: ByteString }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

matchCheckFingerprint :: CheckFingerprintPattern -> CheckFingerprint -> Bool
matchCheckFingerprint pat fingerprint =
    pat.unwrap `B.isPrefixOf` fingerprint.unwrap

newtype CheckGroupFingerprintPattern
  = CheckGroupFingerprintPattern { unwrap :: ByteString }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

matchCheckGroupFingerprint :: CheckGroupFingerprintPattern -> CheckGroupFingerprint -> Bool
matchCheckGroupFingerprint pat fingerprint =
    pat.unwrap `B.isPrefixOf` fingerprint.unwrap

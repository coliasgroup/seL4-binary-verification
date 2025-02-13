{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.System.Core.Fingerprinting
    ( HasComputedFingerprint (..)
    , SMTProofCheckFingerprint (..)
    , SMTProofCheckFingerprintPattern (..)
    , SMTProofCheckGroupFingerprint (..)
    , SMTProofCheckGroupFingerprintPattern (..)
    , fingerprintSMTProofCheck
    , fingerprintSMTProofCheckGroup
    , matchSMTProofCheckFingerprint
    , matchSMTProofCheckGroupFingerprint
    , prettySMTProofCheckFingerprint
    , prettySMTProofCheckFingerprintShort
    , prettySMTProofCheckGroupFingerprint
    , prettySMTProofCheckGroupFingerprintShort
    ) where

import BV.ConcreteSyntax
import BV.Core.Types

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

newtype SMTProofCheckFingerprint
  = SMTProofCheckFingerprint { unwrap :: ByteString }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

fingerprintSMTProofCheck :: SMTProofCheck a -> SMTProofCheckFingerprint
fingerprintSMTProofCheck check =
    SMTProofCheckFingerprint . hashlazy . encodeUtf8 . toLazyText . buildSExprWithPlaceholders $
        [ "SMTProofCheck"
        , ["setup", fromList check.setup]
        , ["imp", check.imp.term]
        ]

newtype SMTProofCheckGroupFingerprint
  = SMTProofCheckGroupFingerprint { unwrap :: ByteString }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

fingerprintSMTProofCheckGroup :: SMTProofCheckGroup a -> SMTProofCheckGroupFingerprint
fingerprintSMTProofCheckGroup group =
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

shortFingerprintLength :: Int
shortFingerprintLength = 12

prettySMTProofCheckFingerprintShort :: SMTProofCheckFingerprint -> String
prettySMTProofCheckFingerprintShort = take shortFingerprintLength . prettySMTProofCheckFingerprint

prettySMTProofCheckGroupFingerprintShort :: SMTProofCheckGroupFingerprint -> String
prettySMTProofCheckGroupFingerprintShort = take shortFingerprintLength . prettySMTProofCheckGroupFingerprint

--

class HasComputedFingerprint b a | a -> b where
    computedFingerprint :: a -> b

instance HasComputedFingerprint SMTProofCheckFingerprint SMTProofCheckFingerprint where
    computedFingerprint = id

instance HasComputedFingerprint SMTProofCheckGroupFingerprint SMTProofCheckGroupFingerprint where
    computedFingerprint = id

--

newtype SMTProofCheckFingerprintPattern
  = SMTProofCheckFingerprintPattern { unwrap :: ByteString }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

matchSMTProofCheckFingerprint :: SMTProofCheckFingerprintPattern -> SMTProofCheckFingerprint -> Bool
matchSMTProofCheckFingerprint pat fingerprint =
    pat.unwrap `B.isPrefixOf` fingerprint.unwrap

newtype SMTProofCheckGroupFingerprintPattern
  = SMTProofCheckGroupFingerprintPattern { unwrap :: ByteString }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

matchSMTProofCheckGroupFingerprint :: SMTProofCheckGroupFingerprintPattern -> SMTProofCheckGroupFingerprint -> Bool
matchSMTProofCheckGroupFingerprint pat fingerprint =
    pat.unwrap `B.isPrefixOf` fingerprint.unwrap

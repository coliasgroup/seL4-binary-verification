{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.System.Fingerprinting
    ( FlattenedSMTProofChecksWithFingerprints (..)
    , SMTProofCheckFingerprint (..)
    , SMTProofCheckGroupFingerprint (..)
    , SMTProofCheckGroupWithCheckFingerprints
    , SMTProofCheckGroupWithFingerprints (..)
    , SMTProofCheckMetaWithFingerprint (..)
    , SMTProofCheckWithFingerprint
    , adornWithFingerprints
    , prettySMTProofCheckFingerprint
    , prettySMTProofCheckFingerprintShort
    , prettySMTProofCheckGroupFingerprint
    , prettySMTProofCheckGroupFingerprintShort
    , smtProofCheckFingerprint
    , smtProofCheckGroupFingerprint
    ) where

import BV.ConcreteSyntax
import BV.Core.Types

import Control.DeepSeq (NFData)
import Crypto.Hash.SHA256 (hashlazy)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import GHC.IsList (fromList)
import Optics
import Text.Printf (PrintfArg (formatArg), formatString)

newtype SMTProofCheckFingerprint
  = SMTProofCheckFingerprint { unwrap :: ByteString }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

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
  deriving newtype (NFData)

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

shortFingerprintLength :: Int
shortFingerprintLength = 12

prettySMTProofCheckFingerprintShort :: SMTProofCheckFingerprint -> String
prettySMTProofCheckFingerprintShort = take shortFingerprintLength . prettySMTProofCheckFingerprint

prettySMTProofCheckGroupFingerprintShort :: SMTProofCheckGroupFingerprint -> String
prettySMTProofCheckGroupFingerprintShort = take shortFingerprintLength . prettySMTProofCheckGroupFingerprint

instance PrintfArg SMTProofCheckFingerprint where
  formatArg = formatString . prettySMTProofCheckFingerprint

instance PrintfArg SMTProofCheckGroupFingerprint where
  formatArg = formatString . prettySMTProofCheckGroupFingerprint

--

data SMTProofCheckMetaWithFingerprint a
  = SMTProofCheckMetaWithFingerprint
      { fingerprint :: SMTProofCheckFingerprint
      , inner :: a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

type SMTProofCheckWithFingerprint a = SMTProofCheck (SMTProofCheckMetaWithFingerprint a)

type SMTProofCheckGroupWithCheckFingerprints a = SMTProofCheckGroup (SMTProofCheckMetaWithFingerprint a)

data SMTProofCheckGroupWithFingerprints a
  = SMTProofCheckGroupWithFingerprint
      { fingerprint :: SMTProofCheckGroupFingerprint
      , inner :: SMTProofCheckGroupWithCheckFingerprints a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

newtype FlattenedSMTProofChecksWithFingerprints a
  = FlattenedSMTProofChecksWithFingerprints { unwrap :: M.Map PairingId [SMTProofCheckGroupWithFingerprints a] }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving newtype (NFData)

adornWithFingerprints :: FlattenedSMTProofChecks a -> FlattenedSMTProofChecksWithFingerprints a
adornWithFingerprints (FlattenedSMTProofChecks byPairing) =
    FlattenedSMTProofChecksWithFingerprints $
        byPairing &
            traversed % traversed %~ \group ->
                SMTProofCheckGroupWithFingerprint (smtProofCheckGroupFingerprint group) $
                    group &
                        #imps % traversed %~ \imp -> imp &
                            #meta %~ \meta ->
                                let check = SMTProofCheck group.setup imp
                                    fingerprint = smtProofCheckFingerprint check
                                in SMTProofCheckMetaWithFingerprint fingerprint meta

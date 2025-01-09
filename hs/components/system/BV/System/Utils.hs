module BV.System.Utils
    ( decodeMany
    ) where

import Data.Aeson (FromJSON, Result (..), fromJSON)
import qualified Data.Aeson.Parser as P
import qualified Data.Attoparsec.ByteString as A
import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

decodeMany :: FromJSON a => T.Text -> Either String [a]
decodeMany s = A.parseOnly (A.many' P.json) (T.encodeUtf8 s) >>= traverse (fromJSON <&> \case
    Error err -> Left err
    Success a -> Right a)

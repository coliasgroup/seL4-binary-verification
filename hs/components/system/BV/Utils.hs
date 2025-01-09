module BV.Utils
    ( adjacently
    , decodeMany
    , intersperse
    ) where

import Data.Aeson (FromJSON, Result (..), fromJSON)
import qualified Data.Aeson.Parser as P
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Optics.Core

intersperse :: Monoid a => a -> [a] -> a
intersperse _ [] = mempty
intersperse sep (x:xs) = x <> mconcat (map (sep <>) xs)

adjacently :: Lens' s a -> Lens' s a' -> Lens' s (a, a')
adjacently l r =
    withLens l $ \getl setl ->
    withLens r $ \getr setr ->
        lens (\s -> (getl s, getr s))
             (\s (b, b') -> setr (setl s b) b')

decodeMany :: FromJSON a => T.Text -> Either String [a]
decodeMany s = A.parseOnly (A.many' P.json) (T.encodeUtf8 s) >>= traverse (fromJSON <&> \case
    Error err -> Left err
    Success a -> Right a)

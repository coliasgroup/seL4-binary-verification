module BV.Core.Types.Extras.Aggregate
    ( inlineScriptMapsEquivalent
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Problem (inlineScriptsEquivalent)

import Control.Monad (unless)
import Data.Foldable (for_, toList)
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Set as S

inlineScriptMapsEquivalent :: Tag t => InlineScripts t -> InlineScripts t -> Either (PairingId t) ()
inlineScriptMapsEquivalent x y = do
    case toList (S.union (f x y) (f y x)) of
        k:_ -> Left k
        _ -> return ()
    for_ (M.toList x.unwrap) $ \(k, v) -> do
        let v' = y.unwrap M.! k
        unless (inlineScriptsEquivalent v v') $ do
            Left k
  where
    f = S.difference `on` (M.keysSet . (.unwrap))

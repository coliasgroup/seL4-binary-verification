module BV.Core.GenerateFreshName
    ( generateFreshName
    ) where

import BV.Utils (ensure)

-- Implementation matches graph_refine.syntax.fresh_name
generateFreshName :: (String -> Bool) -> String -> String
generateFreshName isTaken n =
    if not (isTaken n)
    then n
    else loop1 1 (1 :: Integer)
  where
    fmt x = n ++ "." ++ show x
    loop1 x y =
        if isTaken (fmt x)
        then loop1 (x * 2) x
        else loop2 x y
    loop2 x y =
        if y < x
        then
            let z = (y + x) `div` 2
             in if isTaken (fmt z)
                then loop2 x (z + 1)
                else loop2 z y
        else
            let n' = fmt x
             in ensure (not (isTaken n')) n'

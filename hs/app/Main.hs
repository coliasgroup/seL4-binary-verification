module Main where

import qualified BV (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  BV.someFunc

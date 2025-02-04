module Main
    ( main
    ) where

import BV.SMTLIB2

main :: IO ()
main = do
    contents <- getContents
    let sexprs = readSExprs contents
    mapM_ (putStrLn . showSExpr) sexprs

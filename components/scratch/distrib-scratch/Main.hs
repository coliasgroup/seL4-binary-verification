module Main
    ( main
    ) where


import Opts

main :: IO ()
main = do
    opts <- parseOpts
    case opts of
        CommandDriver opts' -> runDriver opts'
        CommandWorker opts' -> runWorker opts'

runDriver :: DriverOpts -> IO ()
runDriver opts = do
    putStrLn "driver"
    return ()

runWorker :: WorkerOpts -> IO ()
runWorker opts = do
    putStrLn "worker"
    return ()

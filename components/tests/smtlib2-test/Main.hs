{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import BV.SMTLIB2
import BV.SMTLIB2.Command
import BV.SMTLIB2.Process
import BV.SMTLIB2.SExpr.Build
import qualified BV.SMTLIB2.SExpr.Parse.Attoparsec as SA
import qualified BV.SMTLIB2.SExpr.Parse.Megaparsec as SM
import BV.TargetDir
import BV.Test.Utils

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Attoparsec.Text.Lazy as A
import Data.Maybe (fromJust)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TL
import Optics
import System.FilePath ((</>))
import System.FilePath.Glob (compile, globDir1)
import System.Process (CreateProcess, proc)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Megaparsec as M

-- config

configCreateProc :: CreateProcess
configCreateProc = proc "yices-smt2" ["--incremental"]

configTestPairs :: CustomOpts -> IO [(FilePath, FilePath)]
configTestPairs opts = map f <$> globDir1 (compile (opts.defaultTargetDirForTestsRequiringTrace.path </> "trace" </> "*")) "."
  where
    f d = (d </> "in.smt2", d </> "out.smt2")

--

main :: IO ()
main = bvMain $ \opts -> testGroup "Tests"
    [ testCase "chosen pairs" testChosenPairs
    , testCase "trace pairs" $ testTracePairs opts
    , testCase "demo" demo
    ]

runSolverSimple :: ExceptT CommandError (SolverT IO) a -> IO (Either CommandError a)
runSolverSimple m = runSolver
    T.putStrLn
    configCreateProc
    (runExceptT m)

parsersAndBuildersAgreePath :: FilePath -> IO [SExpr]
parsersAndBuildersAgreePath path = TL.readFile path >>= parsersAndBuildersAgree path

parsersAndBuildersAgree :: String -> TL.Text -> IO [SExpr]
parsersAndBuildersAgree src s = do
    parsed <- parsersAgree src s
    let built = TB.toLazyText . foldMap ((<> "\n") . buildSExpr) $ parsed
    parsedBuilt <- parsersAgree (src ++ " (built)") built
    assertEqual "" parsed parsedBuilt
    return parsed

parsersAgree :: String -> TL.Text -> IO [SExpr]
parsersAgree src s = do
    let mega = megaP src s
    let atto = attoP s
    let rp = readSExprs (TL.unpack s)
    assertEqual "" mega atto
    assertEqual "" mega rp
    return mega
  where
    megaP path = either (error . M.errorBundlePretty) id .
        M.parse (SM.consumeAnySExprWhitespace *> M.many (SM.parseSExpr <* SM.consumeAnySExprWhitespace) <* M.eof) path
    attoP = either error id .
        A.parseOnly (SA.consumeAnySExprWhitespace *> A.many' (SA.parseSExpr <* SA.consumeAnySExprWhitespace) <* A.endOfInput)

solverAgrees :: FilePath -> [SExpr] -> [SExpr] -> IO ()
solverAgrees inPath sexprsIn sexprsOut = do
    r <- runSolverSimple $ do
            forM_ sexprsIn $ \sexprIn -> do
                sendSExpr sexprIn
            forM_ sexprsOut $ \sexprOut -> do
                sexprOut' <- recvSExpr
                liftIO $ assertEqual inPath sexprOut sexprOut'
    case r of
        Left err -> assertFailure $ show err
        Right _x -> return ()

chosenPairs :: [(TL.Text, [SExpr])]
chosenPairs = map (_2 % traversed %~ (fromJust . checkSExpr))
    [ ("", [])
    , ("\"xyz\"\"abc\"", [Atom (StringAtom "xyz\"abc")])
    -- , ("\"xyz\"\"abc", [Atom (StringAtom "xyz\"abc")])
    ]

testChosenPairs :: IO ()
testChosenPairs = forM_ chosenPairs $ \(s, ex) -> do
    ex' <- parsersAndBuildersAgree "" s
    assertEqual "" ex ex'

testTracePairs :: CustomOpts -> IO ()
testTracePairs opts = do
    pairs <- configTestPairs opts
    forM_ pairs $ \(inPath, outPath) -> do
        -- putStrLn $ "checking " ++ inPath
        sexprsIn <- parsersAndBuildersAgreePath inPath
        sexprsOut <- parsersAndBuildersAgreePath outPath
        -- putStrLn "parsers agreed"
        solverAgrees inPath sexprsIn sexprsOut
        -- putStrLn "solver agrees"

demo :: IO ()
demo = do
    r <- runSolverSimple $ do
            sendSimpleCommandExpectingSuccess $ SetOption (PrintSuccessOption True)
            sendSimpleCommandExpectingSuccess $ SetLogic "QF_UF"
            sendExpectingSuccess $ readSExpr "(declare-const p Bool)"
            sendExpectingSuccess $ readSExpr "(assert (and p (not p)))"
            checkSat
    case r of
        Left err -> assertFailure $ show err
        Right x -> assertEqual "" x Unsat

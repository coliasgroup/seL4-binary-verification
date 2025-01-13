{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import System.FilePath ((</>))
import System.FilePath.Glob (compile, globDir1)
import System.Process (CreateProcess, proc)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Megaparsec as M

import BV.SMTLIB2.Builder as SB
import BV.SMTLIB2.Parser.Attoparsec as SA
import BV.SMTLIB2.Parser.Megaparsec as SM
import BV.SMTLIB2.Process
import BV.SMTLIB2.Types
import BV.SMTLIB2.Types.Command

-- config

configCreateProc :: CreateProcess
configCreateProc = proc "yices-smt2" ["--incremental"]

configTestPairs :: IO [(FilePath, FilePath)]
configTestPairs = map f <$> globDir1 (compile ("tmp" </> "target-small-trace" </> "trace" </> "*")) "."
  where
    f d = (d </> "in.smt2", d </> "out.smt2")

--

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "trivial" $ return ()
    , testCase "trace pairs" testTracePairs
    , testCase "demo" demo
    ]

runSolverSimple :: ExceptT CommandError (SolverT IO) a -> IO (Either CommandError a)
runSolverSimple m = runSolver
    configCreateProc
    T.putStrLn
    (runExceptT m)

parsersAndBuildersAgreePath :: FilePath -> IO [SExpr]
parsersAndBuildersAgreePath path = T.readFile path >>= parsersAndBuildersAgree path

parsersAndBuildersAgree :: String -> T.Text -> IO [SExpr]
parsersAndBuildersAgree src s = do
    parsed <- parsersAgree src s
    let built = TL.toStrict . TB.toLazyText . mconcat . map ((<> "\n") . buildSExpr) $ parsed
    let shown = T.pack $ concatMap ((++ "\n") . showSExpr) parsed
    parsedBuilt <- parsersAgree (src ++ " (built)") built
    parsedShown <- parsersAgree (src ++ " (shown)") shown
    assertEqual "" parsed parsedBuilt
    assertEqual "" parsed parsedShown
    return parsed

parsersAgree :: String -> T.Text -> IO [SExpr]
parsersAgree src s = do
    let mega = megaP src s
    let atto = attoP s
    let rp = readSExprs (T.unpack s)
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
                send sexprIn
            forM_ sexprsOut $ \sexprOut -> do
                sexprOut' <- recv
                liftIO $ assertEqual inPath sexprOut sexprOut'
    case r of
        Left err -> assertFailure $ show err
        Right _x -> return ()

testTracePairs :: IO ()
testTracePairs = do
    pairs <- configTestPairs
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

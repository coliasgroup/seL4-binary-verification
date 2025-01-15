{-# LANGUAGE OverloadedStrings #-}

module BV.Core.Stages.InlineAssembly
    ( addInlineAssemblySpecs
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Optics
import Data.List (stripPrefix, groupBy, isPrefixOf, intercalate)
import qualified Text.ParserCombinators.ReadP as P
import Debug.Trace
import Data.Char (isDigit, isHexDigit, toLower)
import Optics.Internal.Optic (getOptic)
import Control.Monad.Writer (runWriter, tell)

import BV.Core.Utils
import BV.Core.Types
import BV.Core.ExprConstruction
import Control.Monad (when, unless)
import Data.Foldable (forM_)
import Control.Monad.State.Lazy (execStateT)
import Control.Monad.State (modify)
import Control.Monad.State (gets)
import Control.Monad.State.Lazy (StateT)
import Control.Monad.State.Lazy (State)
import Control.Monad.State.Lazy (execState)


addInlineAssemblySpecs :: PairingOf Program -> (Pairings, PairingOf Program)
addInlineAssemblySpecs progs =
    (pairings, progs')
    -- tt undefined
    -- undefined
  where
    -- pairings = Pairings M.empty

    g :: String -> (String -> Function -> (Maybe FunctionBody, Ident)) -> Program -> (Program, S.Set Ident, S.Set Ident)
    g prefix mkInstSpec prog = (prog', S.fromList newFunNames, S.fromList unhandledFunNames)
      where
        (prog', (newFunNames, unhandledFunNames)) =
            runWriter . iforOf (#functions % itraversed) prog $ \funName fun ->
                case stripPrefix prefix funName.unwrap of
                    Nothing -> return fun
                    Just funNameSuffix ->
                        let (body, newFunName) = mkInstSpec funNameSuffix fun
                         in do
                            case body of
                                Just _ -> tell ([newFunName], [])
                                Nothing -> tell ([], [funName])
                            return $ fun & #body .~ body

    -- f :: String -> Program -> S.Set String
    -- f prefix prog = S.fromList $
    --     prog ^.. #functions % to M.keys % folded % #unwrap % to (stripPrefix prefix) % folded

    (cProg', cNewFunNames, cUnhandledFunNames) = g "asm_instruction'" mkAsmInstSpec progs.c
    (asmProg', asmNewFunNames, asmUnhandledFunNames) = g "instruction'" mkBinInstSpec progs.asm

    allNewFunNames = S.toList (cNewFunNames `S.union` asmNewFunNames)

    progs'Init = PairingOf
        { c = cProg'
        , asm = asmProg'
        }

    -- (progs', pairings)

    (progs', pairings) = foldr
        (\(pairingId, pairOfNewFuns, pairing) (progs'', pairings') ->
            ( (\n f p -> p & #functions % at n ?~ f) <$> pairingId <*> pairOfNewFuns <*> progs''
            , pairings & #unwrap % at pairingId ?~ pairing
            ))
        (progs'Init, Pairings M.empty)
        (map (mkNew . (.unwrap)) allNewFunNames)

    mkNew :: String -> (PairingId, PairingOf Function, Pairing)
    mkNew newFunName = (pairingId, pairOfNewFuns, pairing)
      where
        Just (implName, spec) = regSpec newFunName
        pairingId = PairingOf
            { c = Ident $ "r_" ++ implName
            , asm = Ident $ "l_" ++ implName
            }
        input =
            [ Argument (Ident ("reg_val" ++ show (i + 1))) machineWordT
            | i <- [ 0 .. length (filter (== RegRoleIn) spec) - 1 ]
            ] ++
            [ Argument "inst_ident" tokenT
            , Argument "mem" memT
            ]
        output =
            [ Argument (Ident ("ret_val" ++ show (i + 1))) machineWordT
            | i <- [ 0 .. length (filter (== RegRoleOut) spec) - 1 ]
            ] ++
            [ Argument "mem" memT
            ]
        pairOfNewFuns = PairingOf
            { c = Function
                { input
                , output
                , body = Nothing
                }
            , asm = Function
                { input
                , output
                , body = Nothing
                }
            }
        pairing = undefined

mkBinInstSpec :: String -> Function -> (Maybe FunctionBody, Ident)
mkBinInstSpec funNameSuffix _ = (body, newFunName)
  where
    (regs, newFunName, token) = splitInstNameRegs Asm funNameSuffix
    body = regSpec newFunName.unwrap <&> \(implName, regSpec) ->
        FunctionBody
            { entryPoint = Addr 1
            , nodes = M.singleton 1 $ CallNode
                { next = Ret
                , functionName = Ident ("l_" ++ implName)
                , input =
                    [ machineWordVarE reg | (reg, RegRoleIn) <- zip regs regSpec ]
                    -- []
                    ++ [ tokenE (unaliasInsn token), varE memT "mem" ]
                , output =
                    [ Argument reg machineWordT | (reg, RegRoleOut) <- zip regs regSpec ]
                    -- []
                    ++ [ Argument "mem" memT ]
                }
            }

mkAsmInstSpec :: String -> Function -> (Maybe FunctionBody, Ident)
mkAsmInstSpec funNameSuffix fun = (body, newFunName)
  where
    (args, newFunName, token) = splitInstNameRegs C funNameSuffix
    body = do
        unless (all (("%" `isPrefixOf`) . (.unwrap)) args) $ Nothing
        regSpec newFunName.unwrap <&> \(implName, _regSpec) ->
            let (iscs, imems) = splitScalarPairs fun.input
                (oscs, omems) = splitScalarPairs fun.output
             in FunctionBody
                    { entryPoint = Addr 1
                    , nodes = M.singleton 1 $ CallNode
                        { next = Ret
                        , functionName = Ident ("r_" ++ implName)
                        , input =
                            map varFromArgE iscs
                            ++ [ tokenE token ]
                            ++ map varFromArgE imems
                        , output =
                            oscs ++ omems
                        }
                    }

splitScalarPairs :: [Argument] -> ([Argument], [Argument])
splitScalarPairs args = (scalars, mems)
  where
    (scalars, globals) = span (\arg -> isWordT arg.ty || isBoolT arg.ty) args
    mems = filter (\arg -> isMemT arg.ty) globals

splitInstNameRegs :: Tag -> String -> ([Ident], Ident, Ident)
splitInstNameRegs tag funNameSuffix =
    (regs, Ident (head segsOut), Ident (intercalate "_" segsOut))
  where
    allSegsIn = splitBetween (`elem` ("_," :: String)) funNameSuffix
    segsIn = case tag of
        C -> allSegsIn
        Asm -> init allSegsIn
    (regs, segsOut) = flip execState mempty $ do
        forM_ (zip [0..] segsIn) $ \(i, seg) -> do
            let seg' = unaliasReg (adjust i (map toLower seg))
            if isReg seg' || ("%" `isPrefixOf` seg')
            then do
                modify (<> ([Ident seg'], []))
                iArg <- gets (length . fst)
                modify (<> ([Ident seg'], ["argv" ++ show iArg]))
            else modify (<> ([], [seg']))
    adjust i = \case
        'p':digits | i == 1 && not (null digits) && all isDigit digits -> digits
        'c':'r':digits | not (null digits) && all isDigit digits -> 'c':digits
        s -> s

splitBetween :: (a -> Bool) -> [a] -> [[a]]
splitBetween isDelim = go
  where
    go [] = []
    go xs = let (chunk, rest) = break isDelim (dropWhile isDelim xs) in chunk : go rest

data RegRole = RegRoleIn | RegRoleOut
  deriving (Eq, Ord, Show)

type RegSpec = [RegRole]

unaliasInsn = \case
    "isb_sy" -> "isb"
    "dmb_sy" -> "dmb"
    "dsb_sy" -> "dsb"
    s -> s

isReg = \case
    'r':digits | all isDigit digits && read digits < (16 :: Integer) -> True
    _ -> False

unaliasReg = \case
    "sb" -> "r9"
    "sl" -> "r10"
    "fp" -> "r11"
    "ip" -> "r12"
    "sp" -> "r13"
    "lr" -> "r14"
    "pc" -> "r15"
    s -> s

regSpec = \case
    "mcr" -> Just ("impl'mcr", [RegRoleIn])
    "mcr2" -> Just ("impl'mcr", [RegRoleIn])
    "mcrr" -> Just ("impl'mcrr", [RegRoleIn, RegRoleIn])
    "mcrr2" -> Just ("impl'mcrr", [RegRoleIn, RegRoleIn])
    "mrc" -> Just ("impl'mrc", [RegRoleOut])
    "mrc2" -> Just ("impl'mrc", [RegRoleOut])
    "mrrc" -> Just ("impl'mrrc", [RegRoleOut, RegRoleOut])
    "mrrc2" -> Just ("impl'mrrc", [RegRoleOut, RegRoleOut])
    "dsb" -> Just ("impl'dsb", [])
    "dmb" -> Just ("impl'dmb", [])
    "isb" -> Just ("impl'isb", [])
    "wfi" -> Just ("impl'wfi", [])
    _ -> Nothing

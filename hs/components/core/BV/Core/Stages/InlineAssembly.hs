{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Fuse foldr/map" #-}

module BV.Core.Stages.InlineAssembly
    ( addInlineAssemblySpecs
    ) where

import Control.Monad (unless)
import Control.Monad.Writer (runWriter, tell)
import Data.Char (isAlphaNum, isDigit, isHexDigit)
import Data.Functor (void)
import Data.List (intercalate, stripPrefix)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Numeric (readDec)
import Optics
import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP

import BV.Core.ExprConstruction
import BV.Core.Types

addInlineAssemblySpecs :: PairingOf Program -> (Pairings, PairingOf Program)
addInlineAssemblySpecs progs =
    -- traceShow (map (.unwrap) (S.toList cUnhandledFunNames)) .
    -- traceShow (map (.unwrap) (S.toList asmUnhandledFunNames)) .
    -- id $
    (pairings, progs')
  where
    (cProg', cInstFuns, cUnhandledFunNames) = f decodeCInstFun progs.c
    (asmProg', asmInstFuns, asmUnhandledFunNames) = f decodeAsmInstFun progs.asm

    f
        :: (Ident -> Function -> Maybe (Either Unhandled (FunctionBody, InstFunction)))
        -> Program
        -> (Program, S.Set InstFunction, S.Set Ident)
    f decode prog = (prog', S.fromList requiredInstFunsForSide, S.fromList unhandledFunNames)
      where
        (prog', (requiredInstFunsForSide, unhandledFunNames)) =
            runWriter . iforOf (#functions % itraversed) prog $ \funName fun ->
                case decode funName fun of
                    Nothing -> do
                        return fun
                    Just (Left Unhandled) -> do
                        tell ([], [funName])
                        return fun
                    Just (Right (funBody, instFun)) -> do
                        tell ([instFun], [])
                        return $ fun & #body ?~ funBody

    progsInit = PairingOf
        { c = cProg'
        , asm = asmProg'
        }

    requiredInstFuns = S.toList (cInstFuns `S.union` asmInstFuns)

    (progs', pairings) = foldr
        (\(pairingId, pairOfNewFuns, pairing) (progs'', pairings') ->
            ( (\name fun p -> p & #functions % at name ?~ fun) <$> pairingId <*> pairOfNewFuns <*> progs''
            , pairings' & #unwrap % at pairingId ?~ pairing
            ))
        (progsInit, Pairings M.empty)
        (map explodeInst requiredInstFuns)

    explodeInst :: InstFunction -> (PairingId, PairingOf Function, Pairing)
    explodeInst instFun = (pairingId, pairOfNewFuns, pairing)
      where
        pairingId = instFunctionName instFun
        pairOfNewFuns = pure (elaborateInstFunction instFun)
        pairing = undefined

data InstFunction
  = MCR
  | MCRR
  | MRC
  | MRRC
  | DSB
  | DMB
  | ISB
  | WFI
  deriving (Eq, Ord, Show)

data RegRole
  = RegRoleIn
  | RegRoleOut
  deriving (Eq, Ord, Show)

data Unhandled
  = Unhandled

regSpecForInstFunction :: InstFunction -> [RegRole]
regSpecForInstFunction = \case
    MCR -> [RegRoleIn]
    MCRR -> [RegRoleIn, RegRoleIn]
    MRC -> [RegRoleOut]
    MRRC -> [RegRoleOut, RegRoleOut]
    DSB -> []
    DMB -> []
    ISB -> []
    WFI -> []

instFunctionName :: InstFunction -> PairingOf Ident
instFunctionName instFun = PairingOf { c = "r", asm = "l"} <&> \prefix -> Ident (prefix ++ "_impl'" ++ suffix)
  where
    suffix = case instFun of
        MCR -> "mcr"
        MCRR -> "mcrr"
        MRC -> "mrc"
        MRRC -> "mrrc"
        DSB -> "dsb"
        DMB -> "dmb"
        ISB -> "isb"
        WFI -> "wfi"

elaborateInstFunction :: InstFunction -> Function
elaborateInstFunction instFun =
    Function
        { input =
            [ Argument (Ident ("reg_val" ++ show (i + 1))) machineWordT
            | i <- [ 0 .. length (filter (== RegRoleIn) regSpec) - 1 ]
            ] ++
            [ Argument "inst_ident" tokenT
            , Argument "mem" memT
            ]
        , output =
            [ Argument (Ident ("ret_val" ++ show (i + 1))) machineWordT
            | i <- [ 0 .. length (filter (== RegRoleOut) regSpec) - 1 ]
            ] ++
            [ Argument "mem" memT
            ]
        , body = Nothing
        }
  where
    regSpec = regSpecForInstFunction instFun

decodeAsmInstFun :: Ident -> Function -> Maybe (Either Unhandled (FunctionBody, InstFunction))
decodeAsmInstFun funName _fun = f <$> stripPrefix "instruction'" funName.unwrap
  where
    f funNameWithoutPrefix = case tryReadP parseAsmInstFun funNameWithoutPrefix of
        Nothing -> Left Unhandled
        Just (instFun, regAssignments, token, _addr) -> Right $
            let regSpec = regSpecForInstFunction instFun
                funBody =
                    FunctionBody
                        { entryPoint = Addr 1
                        , nodes = M.singleton 1 $ CallNode
                            { next = Ret
                            , functionName = (instFunctionName instFun).asm
                            , input =
                                [ machineWordVarE reg | (reg, RegRoleIn) <- zip regAssignments regSpec ]
                                ++ [ tokenE token, varE memT "mem" ]
                            , output =
                                [ Argument reg machineWordT | (reg, RegRoleOut) <- zip regAssignments regSpec ]
                                ++ [ Argument "mem" memT ]
                            }
                        }
             in (funBody, instFun)

parseAsmInstFun :: P.ReadP (InstFunction, [Ident], Ident, String)
parseAsmInstFun = do
    (instFun, tokenHead) <- choice
        [ (,) MCR <$> string "mcr"
        , (,) MCR <$> string "mcr2"
        , (,) MCRR <$> string "mcrr"
        , (,) MCRR <$> string "mcrr2"
        , (,) MRC <$> string "mrc"
        , (,) MRC <$> string "mrc2"
        , (,) MRRC <$> string "mrrc"
        , (,) MRRC <$> string "mrrc2"
        , (,) DSB <$> string "dsb"
        , (,) DMB <$> string "dmb"
        , (,) ISB <$> string "isb"
        , (,) WFI <$> string "wfi"
        ]
    (regAssignments, tokenTail) <- case instFun of
        MCR -> do
            sep
            s1 <- munch1 isDigit
            sep
            s2 <- munch1 isDigit
            sep
            r <- reg
            sep
            s3 <- cr
            sep
            s4 <- cr
            sep
            s5 <- munch1 isDigit
            return ([r], [s1, s2, "argv1", s3, s4, s5])
        MCRR -> error "TODO"
        MRC -> do
            sep
            s1 <- munch1 isDigit
            sep
            s2 <- munch1 isDigit
            sep
            r <- reg
            sep
            s3 <- cr
            sep
            s4 <- cr
            sep
            s5 <- munch1 isDigit
            return ([r], [s1, s2, "argv1", s3, s4, s5])
        MRRC -> error "TODO"
        DSB -> barrier
        DMB -> barrier
        ISB -> barrier
        _ -> return ([], [])
    sep
    addr <- many1 (satisfy isHexDigit)
    return (instFun, map Ident regAssignments, Ident (intercalate "_" (tokenHead:tokenTail)), addr)
  where
    sep = void $ char '_'
    cr = (:) <$> (char 'c' <* char 'r') <*> munch1 isDigit
    reg = choice
        [ "r9" <$ string "sb"
        , "r10" <$ string "sl"
        , "r11" <$ string "fp"
        , "r12" <$ string "ip"
        , "r13" <$ string "sp"
        , "r14" <$ string "lr"
        , "r15" <$ string "pc"
        , do
            char 'r'
            n :: Integer <- fromJust . tryReadS readDec <$> munch1 isDigit
            unless (n < 16) pfail
            return $ "r" ++ show n
        ]
    barrier = do
        arg' <- option [] ((:[]) <$> (sep *> munch1 isAlphaNum))
        return ([], filter (/= "sy") arg')

decodeCInstFun :: Ident -> Function -> Maybe (Either Unhandled (FunctionBody, InstFunction))
decodeCInstFun funName fun = f <$> stripPrefix "asm_instruction'" funName.unwrap
  where
    f funNameWithoutPrefix = case tryReadP parseCInstFun funNameWithoutPrefix of
        Nothing -> Left Unhandled
        Just (instFun, token) -> Right $
            let (iscs, imems) = splitScalarPairs fun.input
                (oscs, omems) = splitScalarPairs fun.output
                funBody =
                    FunctionBody
                        { entryPoint = Addr 1
                        , nodes = M.singleton 1 $ CallNode
                            { next = Ret
                            , functionName = (instFunctionName instFun).c
                            , input =
                                map varFromArgE iscs
                                ++ [ tokenE token ]
                                ++ map varFromArgE imems
                            , output =
                                oscs ++ omems
                            }
                        }
             in (funBody, instFun)

splitScalarPairs :: [Argument] -> ([Argument], [Argument])
splitScalarPairs args = (scalars, mems)
  where
    (scalars, globals) = span (\arg -> isWordT arg.ty || isBoolT arg.ty) args
    mems = filter (\arg -> isMemT arg.ty) globals

parseCInstFun :: P.ReadP (InstFunction, Ident)
parseCInstFun = do
    (instFun, tokenHead) <- choice
        [ (,) MCR <$> string "mcr"
        , (,) MCR <$> string "mcr2"
        , (,) MCRR <$> string "mcrr"
        , (,) MCRR <$> string "mcrr2"
        , (,) MRC <$> string "mrc"
        , (,) MRC <$> string "mrc2"
        , (,) MRRC <$> string "mrrc"
        , (,) MRRC <$> string "mrrc2"
        , (,) DSB <$> string "dsb"
        , (,) DMB <$> string "dmb"
        , (,) ISB <$> string "isb"
        , (,) WFI <$> string "wfi"
        ]
    tokenTail <- case instFun of
        MCR -> do
            sep
            s1 <- p
            sep
            s2 <- munch1 isDigit
            sep
            arg
            sep
            s3 <- cr
            sep
            s4 <- cr
            sep
            s5 <- munch1 isDigit
            return [s1, s2, "argv1", s3, s4, s5]
        MCRR -> error "TODO"
        MRC -> do
            sep
            s1 <- p
            sep
            s2 <- munch1 isDigit
            sep
            arg
            sep
            s3 <- cr
            sep
            s4 <- cr
            sep
            s5 <- munch1 isDigit
            return [s1, s2, "argv1", s3, s4, s5]
        MRRC -> error "TODO"
        DSB -> barrier
        DMB -> barrier
        ISB -> barrier
        _ -> return []
    return (instFun, Ident (intercalate "_" (tokenHead:tokenTail)))
  where
    sep = void $ munch1 (`elem` (",_" :: String))
    p = char 'p' *> munch1 isDigit
    cr = (:) <$> char 'c' <*> munch1 isDigit
    arg = void $ char '%' *> munch1 isDigit
    barrier = do
        arg' <- option [] ((:[]) <$> (sep *> munch1 isAlphaNum))
        return $ filter (/= "sy") arg'

tryReadP :: P.ReadP a -> String -> Maybe a
tryReadP = tryReadS . P.readP_to_S

tryReadS :: P.ReadS a -> String -> Maybe a
tryReadS p s = case filter (null . snd) (p s) of
    [(a, "")] -> Just a
    _ -> Nothing

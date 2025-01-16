{-# LANGUAGE OverloadedLists #-}
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

addInlineAssemblySpecs :: PairingOf Program -> (Pairings, PairingOf Program, PairingOf [Ident])
addInlineAssemblySpecs progs =
    (pairings, finalProgs, unhandled)
  where
    (cProg', (cInstFuns, cUnhandledFunNames)) = applyDecoder decodeCInstFun progs.c
    (asmProg', (asmInstFuns, asmUnhandledFunNames)) = applyDecoder decodeAsmInstFun progs.asm

    intermediateProgs = PairingOf
        { c = cProg'
        , asm = asmProg'
        }

    requiredInstFuns = S.toList (cInstFuns `S.union` asmInstFuns)

    unhandled = S.toList <$> PairingOf
        { c = cUnhandledFunNames
        , asm = asmUnhandledFunNames
        }

    applyDecoder :: InstFunDecoder -> Program -> (Program, (S.Set InstFunction, S.Set Ident))
    applyDecoder decode prog = runWriter . iforOf (#functions % itraversed) prog $ \funName fun ->
        case decode funName fun of
            Nothing -> do
                return fun
            Just (Left Unhandled) -> do
                tell ([], [funName])
                return fun
            Just (Right (funBody, instFun)) -> do
                tell ([instFun], [])
                return $ fun & #body ?~ funBody

    explodeInst :: InstFunction -> (PairingId, PairingOf Function, Pairing)
    explodeInst instFun = (pairingId, pairOfNewFuns, pairing)
      where
        pairingId = instFunctionName instFun
        pairOfNewFuns = pure (elaborateInstFunction instFun)
        pairing = undefined

    (finalProgs, pairings) = foldr
        (\(pairingId, pairOfNewFuns, pairing) (accProg, accPairings) ->
            ( (\funName fun prg -> prg & #functions % at funName ?~ fun) <$> pairingId <*> pairOfNewFuns <*> accProg
            , accPairings & #unwrap % at pairingId ?~ pairing
            ))
        (intermediateProgs, Pairings M.empty)
        (map explodeInst requiredInstFuns)

--

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

--

type InstFunDecoder = Ident -> Function -> Maybe (Either Unhandled (FunctionBody, InstFunction))

decodeAsmInstFun :: InstFunDecoder
decodeAsmInstFun funName _fun = f <$> stripPrefix "instruction'" funName.unwrap
  where
    f funNameWithoutPrefix = case tryReadP parseAsmInstFun funNameWithoutPrefix of
        Nothing -> Left Unhandled
        Just (instFun, regAssignments, token) -> Right $
            let regSpec = regSpecForInstFunction instFun
                input =
                    [ machineWordVarE reg | (reg, RegRoleIn) <- zip regAssignments regSpec ]
                    ++ [ tokenE token, varE memT "mem" ]
                output =
                    [ Argument reg machineWordT | (reg, RegRoleOut) <- zip regAssignments regSpec ]
                    ++ [ Argument "mem" memT ]
                funBody = trivialProxyFunctionBody (instFunctionName instFun).asm input output
             in (funBody, instFun)


decodeCInstFun :: InstFunDecoder
decodeCInstFun funName fun = f <$> stripPrefix "asm_instruction'" funName.unwrap
  where
    f funNameWithoutPrefix = case tryReadP parseCInstFun funNameWithoutPrefix of
        Nothing -> Left Unhandled
        Just (instFun, token) -> Right $
            let (iscs, imems) = splitScalarPairs fun.input
                (oscs, omems) = splitScalarPairs fun.output
                input = map varFromArgE iscs ++ [ tokenE token ] ++ map varFromArgE imems
                output = oscs ++ omems
                funBody = trivialProxyFunctionBody (instFunctionName instFun).c input output
             in (funBody, instFun)

splitScalarPairs :: [Argument] -> ([Argument], [Argument])
splitScalarPairs args = (scalars, mems)
  where
    (scalars, globals) = span (\arg -> isWordT arg.ty || isBoolT arg.ty) args
    mems = filter (\arg -> isMemT arg.ty) globals

trivialProxyFunctionBody :: Ident -> [Expr] -> [Argument] -> FunctionBody
trivialProxyFunctionBody functionName input output =
    FunctionBody
        { entryPoint = Addr 1
        , nodes = M.singleton 1 $ CallNode
            { next = Ret
            , functionName
            , input
            , output
            }
        }

--

parseAsmInstFun :: P.ReadP (InstFunction, [Ident], Ident)
parseAsmInstFun = parseInstFun registerP <* char '_' <* munch1 isHexDigit

parseCInstFun :: P.ReadP (InstFunction, Ident)
parseCInstFun = do
    (instFun, _args, token) <- parseInstFun argP
    return (instFun, token)
  where
    argP = char '%' *> munch1 isDigit

parseInstFun :: ReadP a -> P.ReadP (InstFunction, [a], Ident)
parseInstFun argP = do
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
    (args, tokenTail) <- case instFun of
        MCR -> mcrP
        MCRR -> error "TODO"
        MRC -> mrcP
        MRRC -> error "TODO"
        DSB -> barrierP
        DMB -> barrierP
        ISB -> barrierP
        _ -> return ([], [])
    return (instFun, args, Ident (intercalate "_" (tokenHead:tokenTail)))
  where
    sepP = void $ munch1 (`elem` (",_" :: String))
    sepThenP = (sepP *>)
    digits1P = munch1 isDigit
    crP = (:) <$> (char 'c' <* optional (char 'r')) <*> digits1P
    mcrP = do
        s1 <- sepThenP $ optional (char 'p') *> digits1P
        s2 <- sepThenP $ digits1P
        r <- sepThenP $ argP
        s3 <- sepThenP $ crP
        s4 <- sepThenP $ crP
        s5 <- sepThenP $ digits1P
        return ([r], [s1, s2, "argv1", s3, s4, s5])
    mrcP = mcrP
    barrierP = do
        arg' <- option [] ((:[]) <$> (sepP *> munch1 isAlphaNum))
        return ([], filter (/= "sy") arg')

registerP :: ReadP Ident
registerP = Ident <$> choice
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

tryReadP :: P.ReadP a -> String -> Maybe a
tryReadP = tryReadS . P.readP_to_S

tryReadS :: P.ReadS a -> String -> Maybe a
tryReadS p s = case filter (null . snd) (p s) of
    [(a, "")] -> Just a
    _ -> Nothing

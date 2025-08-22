{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Stages.FormulatePairing
    ( formulatePairing
    ) where

import BV.Core.Arch (archPtrSizeBytes)
import BV.Core.Logic (splitScalarPairs)
import BV.Core.Types
import BV.Core.Types.Extras

import Data.Function (applyWhen, on)
import Data.Maybe (mapMaybe, maybeToList)
import Optics


formulatePairing :: Expr -> FunctionSignature -> Pairing'
formulatePairing minStackSize sig = Pairing { inEqs, outEqs }

  where

    (cArgVars, cInMems, _) = splitScalarPairs sig.input
    (cRetVars, cOutMems, _) = splitScalarPairs sig.output

    numCArgs = length cArgVars
    numCRets = length cRetVars
    cArgExprs = map varFromNameTyE cArgVars
    cRetExprs = map varFromNameTyE cRetVars

    multiRet = numCRets > 1

    r i = machineWordVarE (Ident ("r" ++ show (i :: Integer)))
    sp = r 13
    stack = varE memT "stack"
    asmMem = varE memT "mem"
    r0Input = machineWordVarE "ret_addr_input"
    ret = machineWordVarE "ret"

    (inMemEqs, outMemEqs) =
        mkMemEqs
            asmMem
            (maybeFromSingletonList (map varFromNameTyE cInMems))
            (Just asmMem)
            (maybeFromSingletonList (map varFromNameTyE cOutMems))

    asmArgSeq = take numCArgs $ concat
        [ [ (r i, Nothing)
          | i <- applyWhen multiRet (drop 1) [0..3]
          ]
        , [ (value, Just addr)
          | (value, addr) <- mkStackSequence stack sp
          ]
        ]

    asmArgExprs = map fst asmArgSeq

    asmArgAddrs = mapMaybe snd asmArgSeq

    argEqs =
        [ asmIn asm === cIn (castCToAsmE asm.ty c)
        | (c, asm) <- zip cArgExprs asmArgExprs
        ]

    asmPreconds = concat
        [ [ alignedE 2 sp
          , ret `eqE` r 14
          , alignedE 2 ret
          , r0Input `eqE` r 0
          , minStackSize `lessEqE` sp
          ]
        , retInfo.asmPreconds
        , [ sp `lessEqE` lastAsmArgAddr
          | lastAsmArgAddr <- maybeToList (lastOf folded asmArgAddrs)
          ]
        ]

    inEqs = concat
        [ argEqs
        , inMemEqs
        , [ asmIn expr === asmIn trueE | expr <- asmPreconds ]
        ]

    retEqs =
        [ asmOut asm === cOut (castCToAsmE asm.ty c)
        | (c, asm) <- retInfo.eqPairs
        ]

    asmInvariants = concat
        [ [ (r i, r i)
          | i <- [4..11] ++ [13]
          ]
        , retInfo.asmInvariants
        , [ ((,) `on` stackWrapperE sp stack) asmArgAddrs retInfo.asmRetAddrs
          ]
        ]

    outEqs = concat
        [ retEqs
        , outMemEqs
        , [ asmIn inVal === asmOut outVal | (inVal, outVal) <- asmInvariants ]
        ]

    retInfo =
        if not multiRet
        then
            RetInfo
                { asmPreconds = []
                , asmInvariants = []
                , asmRetAddrs = []
                , eqPairs = zip cRetExprs [r 0]
                }
        else
            let (eqPairs, asmRetAddrs) = unzip
                    [ ((c, castE c.ty asm), asmAddr)
                    | (c, (asm, asmAddr)) <- zip cRetExprs $ mkStackSequence stack r0Input
                    ]
                initAsmRetSeq = take numCRets $ mkStackSequence stack (r 0)
                Just ((_, initAsmRetSeqHeadAddr), _) = uncons initAsmRetSeq
                Just (_, (_, initAsmRetSeqLastAddr)) = unsnoc initAsmRetSeq
                lastArgAddrOpt = case unsnoc asmArgSeq of
                    Just (_, (_, addrOpt)) -> addrOpt
                    -- HACK (this whole branch) to match unnecessary precond in graph-refine
                    Nothing -> Just $ sp `plusE` numE sp.ty 0
             in RetInfo
                    { asmPreconds = concat
                        [ [ alignedE 2 (r 0)
                          , sp `lessEqE` r 0
                          , r 0 `lessEqE` initAsmRetSeqLastAddr
                          ]
                        , [ lastArgAddr `lessE` initAsmRetSeqHeadAddr
                          | lastArgAddr <- maybeToList lastArgAddrOpt
                          ]
                        ]
                    , asmInvariants = [(r0Input, r0Input)]
                    , asmRetAddrs
                    , eqPairs
                    }

data RetInfo
  = RetInfo
      { asmPreconds :: [Expr]
      , asmInvariants :: [(Expr, Expr)]
      , asmRetAddrs :: [Expr]
      , eqPairs :: [(Expr, Expr)]
      }

mkStackSequence :: Expr -> Expr -> [(Expr, Expr)]
mkStackSequence stack sp =
    [ let addr = sp `plusE` numE sp.ty (archPtrSizeBytes * i)
          expr = memAccE machineWordT addr stack
       in (expr, addr)
    | i <- [0..]
    ]

mkMemEqs :: Expr -> Maybe Expr -> Maybe Expr -> Maybe Expr -> ([PairingEq AsmRefineTag], [PairingEq AsmRefineTag])
mkMemEqs asmInMem cInMemOpt asmOutMemOpt cOutMemOpt =
    (inEqs, outEqs)
  where
    inEqs = case cInMemOpt of
        Nothing ->
            [ asmIn (rodataE asmInMem) === cIn trueE
            ]
        Just cInMem ->
            [ asmIn asmInMem === cIn cInMem
            , cIn (rodataE cInMem) === cIn trueE
            ]
    outEqs = case (asmOutMemOpt, cOutMemOpt) of
        (_, Nothing) ->
            [ asmOut asmOutMem === asmIn asmInMem
            | asmOutMem <- maybeToList asmOutMemOpt
            ]
        (Just asmOutMem, Just cOutMem) ->
            [ asmOut asmOutMem === cOut cOutMem
            , cOut (rodataE cOutMem) === cOut trueE
            ]

maybeFromSingletonList :: [a] -> Maybe a
maybeFromSingletonList = \case
    [] -> Nothing
    [a] -> Just a

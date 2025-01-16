{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.ConcreteSyntax.Instances
    ( parsePrettyPairingId
    ) where

import Data.Bits (shiftL, (.|.))
import Data.Char (chr, isDigit, isSpace, ord)
import Data.Either (partitionEithers)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Monoid (Endo (Endo, appEndo))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Internal.Builder (Builder)
import qualified Data.Text.Lazy.Builder.Int as B
import Data.Void (Void)
import Optics.Core (at, (%), (&), (?~))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import BV.Core.Types
import BV.SMTLIB2.Builder
import BV.SMTLIB2.Parser.Megaparsec

import BV.ConcreteSyntax.Parsing
import BV.ConcreteSyntax.Printing

--

parseLines :: Parser [(String, Symbol)]
parseLines = do
    _ <- skipManyTill anySingle (string "SYMBOL TABLE:" *> eol)
    lines <- many $ do
        addr <- L.hexadecimal
        _ <- char ' ' *> skipCount 7 anySingle *> char ' '
        section <- ident
        _ <- char '\t'
        size <- L.hexadecimal
        _ <- char ' '
        name <- ident
        _ <- eol
        let symbol = Symbol
                { addr
                , size
                , section
                }
        return (name, symbol)
    _ <- skipSome eol
    return lines
  where
    ident = some (satisfy (not . isSpace))

makeObjDumpInfo :: [(String, Symbol)] -> ObjDumpInfo
makeObjDumpInfo lines =
    ObjDumpInfo
        { symbols
        , sections
        }
  where
    symbols = M.fromList lines
    sections = appEndo (foldMap (Endo . f) (M.elems symbols)) M.empty
    f symbol = flip M.alter symbol.section $ \entry -> Just $ case entry of
        Just section ->
            let addr = min section.addr symbol.addr
                end = max (sectionEnd section) (symbolEnd symbol)
             in Section
                    { addr
                    , size = end - addr
                    }
        Nothing ->
            Section
                { addr = symbol.addr
                , size = symbol.size
                }

parseObjDumpInfo :: Parsec Void Text ObjDumpInfo
parseObjDumpInfo = makeObjDumpInfo <$> parseLines

instance ParseFile ObjDumpInfo where
    parseFile = parseObjDumpInfo

--

instance ParseInLine Hyp where
    parseInLine = word >>= \case
        "PCImp" -> HypPcImp <$> parseInLine
        "Eq" -> HypEq False <$> parseInLine
        "EqIfAt" -> HypEq True <$> parseInLine
        _ -> fail "invalid pc imp hyp side"

instance BuildInLine Hyp where
    buildInLine = \case
        HypPcImp hyp -> putWord "PCImp" <> put hyp
        HypEq False hyp -> putWord "Eq" <> put hyp
        HypEq True hyp -> putWord "EqIfAt" <> put hyp

instance ParseInLine PcImpHyp where
    parseInLine = PcImpHyp <$> parseInLine <*> parseInLine

instance BuildInLine PcImpHyp where
    buildInLine hyp = put hyp.lhs <> put hyp.rhs

instance ParseInLine PcImpHypSide where
    parseInLine = word >>= \case
        "True" -> return $ PcImpHypSideBool True
        "False" -> return $ PcImpHypSideBool False
        "PC" -> PcImpHypSidePc <$> parseInLine
        _ -> fail "invalid pc imp hyp side"

instance BuildInLine PcImpHypSide where
    buildInLine = \case
        PcImpHypSideBool val -> putWord (show val)
        PcImpHypSidePc visit -> putWord "PC" <> put visit

instance ParseInLine EqHyp where
    parseInLine = do
        lhs <- parseInLine
        rhs <- parseInLine
        induct <- try (Nothing <$ (inLineSymbol "None" *> inLineSymbol "None"))
            <|> (Just <$> (EqHypInduct <$> parseInLine <*> parseInLine))
        return $ EqHyp lhs rhs induct

instance BuildInLine EqHyp where
    buildInLine hyp = put hyp.lhs <> put hyp.rhs <> case hyp.induct of
        Just induct -> putDec induct.a <> putDec induct.b
        Nothing -> putWord "None" <> putWord "None"

instance ParseInLine EqHypSide where
    parseInLine = EqHypSide <$> parseInLine <*> parseInLine

instance BuildInLine EqHypSide where
    buildInLine side = put side.expr <> put side.visit

instance ParseInLine VisitWithTag where
    parseInLine = VisitWithTag <$> parseInLine <*> parseInLine

instance BuildInLine VisitWithTag where
    buildInLine visit = put visit.visit <> put visit.tag

instance ParseInLine Visit where
    parseInLine = Visit <$> parseInLine <*> parseInLine

instance BuildInLine Visit where
    buildInLine visit = put visit.nodeId <> put visit.restrs

instance ParseInLine Restr where
    parseInLine = Restr <$> parseInLine <*> parseInLine

instance BuildInLine Restr where
    buildInLine restr = put restr.nodeId <> put restr.visitCount

instance ParseInLine VisitCount where
    parseInLine = inLineSymbol "VC" *> (VisitCount <$> parseInLine <*> parseInLine)

instance BuildInLine VisitCount where
    buildInLine visitCount = "VC" <> putManyWith putDec visitCount.numbers <> putManyWith putDec visitCount.offsets

--

instance ParseInLine ProofNode where
    parseInLine = word >>= \case
        "Leaf" -> return ProofNodeLeaf
        "Restr" -> ProofNodeRestr <$> parseInLine
        "CaseSplit" -> ProofNodeCaseSplit <$> parseInLine
        "Split" -> ProofNodeSplit <$> parseInLine
        "SingleRevInduct" -> ProofNodeSingleRevInduct <$> parseInLine
        _ -> fail "invalid proof node"

instance ParseInLine RestrProofNode where
    parseInLine =
        RestrProofNode
            <$> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine

instance ParseInLine RestrProofNodeRange where
    parseInLine =
        RestrProofNodeRange
            <$> parseInLine
            <*> parseInLine
            <*> parseInLine

instance ParseInLine RestrProofNodeRangeKind where
    parseInLine = wordWithOr "invalid restr proof node reange kind" $ \case
        "Number" -> Just RestrProofNodeRangeKindNumber
        "Offset" -> Just RestrProofNodeRangeKindOffset
        _ -> Nothing

instance ParseInLine CaseSplitProofNode where
    parseInLine =
        CaseSplitProofNode
            <$> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine

instance ParseInLine SplitProofNode where
    parseInLine =
        SplitProofNode
            <$> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine

instance ParseInLine SplitProofNodeDetails where
    parseInLine =
        SplitProofNodeDetails
            <$> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine

instance ParseInLine SingleRevInductProofNode where
    parseInLine =
        SingleRevInductProofNode
            <$> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine
            <*> parseInLine

instance ParseInLine Lambda where
    parseInLine = do
        inLineSymbol "Lambda"
        Lambda <$> parseInLine <*> parseInLine <*> parseInLine

--

instance BuildInLine ProofNode where
    buildInLine = \case
        ProofNodeLeaf -> "Leaf"
        ProofNodeRestr node-> "Restr" <> put node
        ProofNodeCaseSplit node-> "CaseSplit" <> put node
        ProofNodeSplit node -> "Split" <> put node
        ProofNodeSingleRevInduct node -> "SingleRevInduct" <> put node

instance BuildInLine RestrProofNode where
    buildInLine range =
           put range.point
        <> put range.tag
        <> put range.range
        <> put range.child

instance BuildInLine RestrProofNodeRange where
    buildInLine range =
           put range.kind
        <> putDec range.x
        <> putDec range.y

instance BuildInLine RestrProofNodeRangeKind where
    buildInLine RestrProofNodeRangeKindNumber = "Number"
    buildInLine RestrProofNodeRangeKindOffset = "Offset"

instance BuildInLine CaseSplitProofNode where
    buildInLine node =
           put node.addr
        <> put node.tag
        <> put node.left
        <> put node.right

instance BuildInLine SplitProofNode where
    buildInLine node =
           put node.addr
        <> putDec node.loopRMax
        <> put node.rDetails
        <> put node.lDetails
        <> put node.eqs
        <> put node.p1
        <> put node.p2

instance BuildInLine SplitProofNodeDetails where
    buildInLine details =
           putDec details.split
        <> putDec details.seqStart
        <> putDec details.step
        <> put details.eqs

instance BuildInLine SingleRevInductProofNode where
    buildInLine node =
           put node.point
        <> put node.tag
        <> putDec node.n
        <> put node.egs
        <> put node.pred
        <> putDec node.nBounds
        <> put node.child

instance BuildInLine Lambda where
    buildInLine (Lambda { freeVar, freeVarTy, expr }) =
        "Lambda" <> put freeVar <> put freeVarTy <> put expr

--

instance ParseFile StackBounds where
    parseFile = do
        optional . line $ inLineSymbol "FunctionHash" *> parseInLine @Integer
        StackBounds . M.fromList <$> many p
      where
        p = line $ do
            inLineSymbol "StackBound"
            (,) <$> parseInLine <*> parseInLine

instance BuildToFile StackBounds where
    buildToFile (StackBounds stackBounds) = buildBlock $ foldMap f (M.toList stackBounds)
      where
        f (ident, expr) = lineInBlock $ "StackBound" <> put ident <> put expr

instance ParseFile ProblemsAndProofs where
    parseFile =
        ProblemsAndProofs . M.fromList
            <$> parseBlocksFileWithTypicalKeyFormat ["ProblemProof", "Problem", "Pairing"] parsePrettyPairingId parseInBlock

instance BuildToFile ProblemsAndProofs where
    buildToFile (ProblemsAndProofs problemsAndProofs) =
        buildBlocksFileWithTypicalKeyFormat
            ["ProblemProof", "Problem", "Pairing"]
            (fromString . prettyPairingId)
            buildInBlock
            (M.toList problemsAndProofs)

instance ParseInBlock ProblemAndProof where
    parseInBlock = ProblemAndProof <$> parseInBlock <*> line parseInLine

instance BuildInBlock ProblemAndProof where
    buildInBlock (ProblemAndProof { problem, proof }) = buildInBlock problem <> lineInBlock (put proof)

--

instance ParseFile InlineScripts where
    parseFile =
        InlineScripts . M.fromList
            <$> parseBlocksFileWithTypicalKeyFormat ["Problem", "Pairing"] parsePrettyPairingId parseInBlock

instance ParseInBlock [InlineScriptEntry] where
    parseInBlock = do
        _ <- line $ inLineSymbol "InlineScript"
        manyTill (line parseInLine) (try endLine)
      where
        endLine = line $ inLineSymbol "EndInlineScript"

instance ParseInLine InlineScriptEntry where
    parseInLine = InlineScriptEntry <$> parseInLine <*> parseInLine

instance ParseInLine NodeBySource where
    parseInLine = NodeBySource <$> parseInLine <*> parseInLine

instance BuildToFile InlineScripts where
    buildToFile (InlineScripts scripts) =
        buildBlocksFileWithTypicalKeyFormat
            ["Problem", "Pairing"]
            (fromString . prettyPairingId)
            buildInBlock
            (M.toList scripts)

instance BuildInBlock InlineScript where
    buildInBlock entries =
        lineInBlock "InlineScript"
            <> foldMap (lineInBlock . put) entries
            <> lineInBlock "EndInlineScript"

instance BuildInLine InlineScriptEntry where
    buildInLine (InlineScriptEntry { nodeBySource, inlinedFunctionName }) = put nodeBySource <> put inlinedFunctionName

instance BuildInLine NodeBySource where
    buildInLine (NodeBySource { nodeSource, indexInProblem }) = put nodeSource <> putDec indexInProblem

--

instance ParseFile Problems where
    parseFile =
        Problems . M.fromList
            <$> parseBlocksFileWithTypicalKeyFormat ["Problem", "Pairing"] parsePrettyPairingId parseInBlock

instance ParseInBlock Problem where
    parseInBlock = do
        _ <- line $ inLineSymbol "Problem"
        (tagA, sideA) <- problemSideLine
        (tagB, sideB) <- problemSideLine
        (c, asm) <- case (tagA, tagB) of
                (C, Asm) -> return (sideA, sideB)
                (Asm, C) -> return (sideB, sideA)
                _ -> fail "invalid problem side tags"
        nodes <- M.fromList <$> manyTill nodeLine (try endLine)
        return $ Problem
            { sides = PairingOf { c, asm }
            , nodes
            }
      where
        nodeLine = line $ (,) <$> parseInLine <*> parseInLine
        endLine = line $ inLineSymbol "EndProblem"
        problemSideLine = line $ do
            _ <- inLineSymbol "Entry"
            entryPoint <- parseInLine
            tag <- parseInLine
            name <- parseInLine
            input <- parseInLine
            output <- parseInLine
            let side = ProblemSide { name, input, output, entryPoint }
            return (tag, side)

instance ParseInLine NodeSource where
    parseInLine = NodeSource <$> parseInLine <*> parseInLine <*> parseInLine

--

instance BuildToFile Problems where
    buildToFile (Problems problems) =
        buildBlocksFileWithTypicalKeyFormat
            ["Problem", "Pairing"]
            (fromString . prettyPairingId)
            buildInBlock
            (M.toList problems)

instance BuildInBlock Problem where
    buildInBlock (Problem { sides, nodes }) =
        lineInBlock "Problem"
            <> problemSideLine C sides.c
            <> problemSideLine Asm sides.asm
            <> foldMap nodeLine (M.toList nodes)
            <> lineInBlock "EndProblem"
      where
        problemSideLine tag (ProblemSide { name, input, output, entryPoint }) = lineInBlock $
            "Entry"
                <> put entryPoint
                <> put tag
                <> put name
                <> put input
                <> put output
        nodeLine (addr, node) = lineInBlock $ put addr <> put node

instance BuildInLine NodeSource where
    buildInLine (NodeSource { tag, functionName, nodeAddr }) = put tag <> put functionName <> put nodeAddr

--

parsePrettyPairingId :: Parser PairingId
parsePrettyPairingId = do
    asm <- ident
    hspace *> "(ASM)" *> hspace *> "<=" *> hspace
    c <- ident
    hspace *> "(C)"
    return $ PairingOf { asm, c }
  where
    ident = Ident <$> some (satisfy isIdentChar)
    isIdentChar c = not (isSpace c || c == '(' || c == ')')

parsePythonPairingName :: Parser PairingId
parsePythonPairingName = do
    "Pairing" *> hspace1 *> "(" *> hspace
    pairingId <- parsePrettyPairingId
    ")"
    return pairingId

--

instance ParseFile Pairings where
    parseFile =
        Pairings . M.fromList
            <$> parseBlocksFileWithTypicalKeyFormat ["Pairing"] parsePrettyPairingId parseInBlock

instance BuildToFile Pairings where
    buildToFile (Pairings pairings) =
        buildBlocksFileWithTypicalKeyFormat
            ["Pairing"]
            (fromString . prettyPairingId)
            buildInBlock
            (M.toList pairings)

instance ParseInBlock Pairing where
    parseInBlock = do
        line $ inLineSymbol "Pairing"
        (inEqs, outEqs) <- partitionEqs <$> manyTill eqLine (try endLine)
        return $ Pairing { inEqs, outEqs }
      where
        eqLine = line $ (,) <$> parseInLine <*> parseInLine
        endLine = line $ inLineSymbol "EndPairing"
        partitionEqs :: [(PairingEqDirection, PairingEq)] -> ([PairingEq], [PairingEq])
        partitionEqs = partitionEithers . map (\(direction, eq) -> eq & (case direction of
            PairingEqDirectionIn -> Left
            PairingEqDirectionOut -> Right))

instance BuildInBlock Pairing where
    buildInBlock pairing =
           lineInBlock "Pairing"
        <> foldMap (eqLine PairingEqDirectionIn) pairing.inEqs
        <> foldMap (eqLine PairingEqDirectionOut) pairing.outEqs
        <> lineInBlock "EndPairing"
      where
        eqLine direction eq = lineInBlock $ put direction <> put eq

instance ParseInLine PairingEq where
    parseInLine = PairingEq <$> parseInLine <*> parseInLine

instance BuildInLine PairingEq where
    buildInLine eq = put eq.lhs <> put eq.rhs

instance ParseInLine PairingEqSide where
    parseInLine = PairingEqSide <$> parseInLine <*> parseInLine

instance BuildInLine PairingEqSide where
    buildInLine side = put side.quadrant <> put side.expr

instance ParseInLine Tag where
    parseInLine = wordWithOr "invalid tag" $ \case
        "C" -> Just C
        "ASM" -> Just Asm
        _ -> Nothing

instance BuildInLine Tag where
    buildInLine = putWord . prettyTag

instance ParseInLine PairingEqSideQuadrant where
    parseInLine = wordWithOr "invalid pairing eq side quadrant" $ \case
        "ASM_IN" -> Just asmIn
        "ASM_OUT" -> Just asmOut
        "C_IN" -> Just cIn
        "C_OUT" -> Just cOut
        _ -> Nothing

instance BuildInLine PairingEqSideQuadrant where
    buildInLine = putWord . prettyPairingEqSideQuadrant

instance ParseInLine PairingEqDirection where
    parseInLine = wordWithOr "invalid pairing eq direction" $ \case
        "IN" -> Just PairingEqDirectionIn
        "OUT" -> Just PairingEqDirectionOut
        _ -> Nothing

instance BuildInLine PairingEqDirection where
    buildInLine = putWord . prettyPairingEqDirection

--

instance ParseFile Program where
    parseFile = do
        ignoredLines
        go $ Program
            { structs = M.empty
            , constGlobals = M.empty
            , functions = M.empty
            }
      where
        go acc =
            ((try (parseAndUpdate #structs acc)
                <|> try (parseAndUpdate #constGlobals acc)
                <|> try (parseAndUpdate #functions acc))
                    >>= go)
                        <|> return acc
        parseAndUpdate field acc = insertItemInto field acc <$> parseInBlock
        insertItemInto field acc (Named name item) = acc & field % at name ?~ item

instance ParseInBlock (Named Struct) where
    parseInBlock = do
        (name, size, align) <- line $ do
            _ <- inLineSymbol "Struct"
            name <- parseInLine
            size <- parseInLine
            align <- parseInLine
            return (name, size, align)
        fields <- many . line $ do
            _ <- inLineSymbol "StructField"
            fieldName <- parseInLine
            ty <- parseInLine
            offset <- parseInLine
            return $ Named fieldName (StructField ty offset)
        return $ Named name (Struct { size, align, fields = fromListOfNamed fields })

instance ParseInBlock (Named ConstGlobal) where
    parseInBlock = do
            _ <- inLineSymbol "ConstGlobalDef"
            undefined

instance ParseInBlock (Named Function) where
    parseInBlock = do
        (name, input, output) <- line $ do
            _ <- inLineSymbol "Function"
            name <- parseInLine
            input <- parseInLine
            output <- parseInLine
            return (name, input, output)
        body <- optional . try $ do
            (nodes, entryPoint) <- manyTill_ nodeLine (try entryPointLine)
            return $ FunctionBody { entryPoint, nodes = M.fromList nodes }
        return $ Named name (Function { input, output, body })
      where
        nodeLine = line $ (,) <$> parseInLine <*> parseInLine
        entryPointLine = line $ inLineSymbol "EntryPoint" *> parseInLine

instance ParseInLine Ident where
    parseInLine = Ident <$> word

instance ParseInLine Argument where
    parseInLine = Argument <$> parseInLine <*> parseInLine

instance ParseInLine NodeId where
    parseInLine =
        (Addr <$> try parseInLine)
            <|> (Err <$ try (inLineSymbol "Err"))
            <|> (Ret <$ try (inLineSymbol "Ret"))

instance ParseInLine NodeAddr where
    parseInLine = NodeAddr <$> parseInLine

instance ParseInLine Node where
    parseInLine = do
        w <- word
        case w of
            "Basic" -> BasicNode <$> parseInLine <*> parseInLine
            "Cond" -> CondNode <$> parseInLine <*> parseInLine <*> parseInLine
            "Call" -> CallNode <$> parseInLine <*> parseInLine <*> parseInLine <*> parseInLine
            _ -> fail "invalid node type"

instance ParseInLine VarUpdate where
    parseInLine = VarUpdate <$> parseInLine <*> parseInLine <*> parseInLine

instance ParseInLine Expr where
    parseInLine = do
        w <- word
        case w of
            "Var" -> typical ExprValueVar
            "Op" -> do
                op <- parseInLine
                ty <- parseInLine
                args <- parseInLine
                return $ Expr { ty, value = ExprValueOp op args }
            "Num" -> typical ExprValueNum
            "Type" -> do
                ty' <- parseInLine
                return $ Expr { ty = ExprTypeType, value = ExprValueType ty' }
            "Symbol" -> typical ExprValueSymbol
            "Token" -> typical ExprValueToken
            "SMTExpr" -> typicalWith hexEncodedString ExprValueSMTExpr
            _ -> fail "invalid value"
      where
        typical :: (ParseInLine a) => (a -> ExprValue) -> Parser Expr
        typical = typicalWith parseInLine
        typicalWith p f = do
            value <- f <$> p
            ty <- parseInLine
            return $ Expr { ty, value }

hexEncodedString :: Parser String
hexEncodedString = inLineLexeme $ some (chr <$> hexByte)
  where
    hexByte = do
        d1 <- hexDigitValue
        d2 <- hexDigitValue
        return $ (d1 `shiftL` 4) .|. d2
    hexDigitValue = hexDigitToValue <$> hexDigitChar

hexDigitToValue :: Char -> Int
hexDigitToValue d = ord d - sub
  where
    sub = case d of
        _ | isDigit d -> ord '0'
        _ | 'a' <= d && d <= 'f' -> ord 'a' - 10
        _ | 'A' <= d && d <= 'F' -> ord 'A' - 10
        _ -> error "unreachable"

instance ParseInLine ExprType where
    parseInLine = do
        w <- word
        case w of
            "Bool" -> return ExprTypeBool
            "Mem" -> return ExprTypeMem
            "Dom" -> return ExprTypeDom
            "HTD" -> return ExprTypeHtd
            "PMS" -> return ExprTypePms
            "UNIT" -> return ExprTypeUnit
            "Type" -> return ExprTypeType
            "Token" -> return ExprTypeToken
            "RelWrapper" -> return ExprTypeRelWrapper
            "Word" -> ExprTypeWord <$> parseInLine
            "WordArray" -> ExprTypeWordArray <$> parseInLine <*> parseInLine
            "Array" -> ExprTypeArray <$> parseInLine <*> parseInLine
            "Struct" -> ExprTypeStruct <$> parseInLine
            "Ptr" -> ExprTypePtr <$> parseInLine
            _ -> fail "invalid type"

instance ParseInLine Op where
    parseInLine = wordWith $ \case
        "Plus" -> Right OpPlus
        "Minus" -> Right OpMinus
        "Times" -> Right OpTimes
        "Modulus" -> Right OpModulus
        "DividedBy" -> Right OpDividedBy
        "BWAnd" -> Right OpBWAnd
        "BWOr" -> Right OpBWOr
        "BWXOR" -> Right OpBWXOR
        "And" -> Right OpAnd
        "Or" -> Right OpOr
        "Implies" -> Right OpImplies
        "Equals" -> Right OpEquals
        "Less" -> Right OpLess
        "LessEquals" -> Right OpLessEquals
        "SignedLess" -> Right OpSignedLess
        "SignedLessEquals" -> Right OpSignedLessEquals
        "ShiftLeft" -> Right OpShiftLeft
        "ShiftRight" -> Right OpShiftRight
        "CountLeadingZeroes" -> Right OpCountLeadingZeroes
        "CountTrailingZeroes" -> Right OpCountTrailingZeroes
        "WordReverse" -> Right OpWordReverse
        "SignedShiftRight" -> Right OpSignedShiftRight
        "Not" -> Right OpNot
        "BWNot" -> Right OpBWNot
        "WordCast" -> Right OpWordCast
        "WordCastSigned" -> Right OpWordCastSigned
        "True" -> Right OpTrue
        "False" -> Right OpFalse
        "UnspecifiedPrecond" -> Right OpUnspecifiedPrecond
        "MemUpdate" -> Right OpMemUpdate
        "MemAcc" -> Right OpMemAcc
        "IfThenElse" -> Right OpIfThenElse
        "ArrayIndex" -> Right OpArrayIndex
        "ArrayUpdate" -> Right OpArrayUpdate
        "MemDom" -> Right OpMemDom
        "PValid" -> Right OpPValid
        "PWeakValid" -> Right OpPWeakValid
        "PAlignValid" -> Right OpPAlignValid
        "PGlobalValid" -> Right OpPGlobalValid
        "PArrayValid" -> Right OpPArrayValid
        "HTDUpdate" -> Right OpHTDUpdate
        "WordArrayAccess" -> Right OpWordArrayAccess
        "WordArrayUpdate" -> Right OpWordArrayUpdate
        "TokenWordsAccess" -> Right OpTokenWordsAccess
        "TokenWordsUpdate" -> Right OpTokenWordsUpdate
        "ROData" -> Right OpROData
        "StackWrapper" -> Right OpStackWrapper
        "EqSelectiveWrapper" -> Right OpEqSelectiveWrapper
        "ToFloatingPoint" -> Right OpToFloatingPoint
        "ToFloatingPointSigned" -> Right OpToFloatingPointSigned
        "ToFloatingPointUnsigned" -> Right OpToFloatingPointUnsigned
        "FloatingPointCast" -> Right OpFloatingPointCast
        "ImpliesROData" -> Right OpImpliesROData
        "ImpliesStackEquals" -> Right OpImpliesStackEquals
        "StackEqualsImplies" -> Right OpStackEqualsImplies
        w -> Left $ "invalid operation: " ++ w

--

instance BuildToFile Program where
    buildToFile (Program { structs, constGlobals, functions }) =
        intersperse "\n" . map buildBlock $
            map buildInBlock (toListOfNamed structs)
                <> map buildInBlock (toListOfNamed constGlobals)
                <> map buildInBlock (toListOfNamed functions)

instance BuildInBlock (Named Struct) where
    buildInBlock (Named name (Struct { size, align, fields })) =
        lineInBlock ("Struct" <> put name <> putDec size <> putDec align)
            <> foldMap buildField (M.toList fields)
      where
        buildField (fieldName, StructField { ty, offset }) = lineInBlock $
            "StructField" <> put fieldName <> put ty <> putDec offset

instance BuildInBlock (Named ConstGlobal) where
    buildInBlock = undefined

instance BuildInBlock (Named Function) where
    buildInBlock (Named name (Function { input, output, body })) =
        lineInBlock ("Function" <> put name <> put input <> put output)
            <> mconcat (maybeToList (buildBody <$> body))
      where
        buildBody (FunctionBody { entryPoint, nodes }) =
            foldMap buildNode (M.toList nodes)
                <> lineInBlock ("EntryPoint" <> put entryPoint)
        buildNode (addr, node) = lineInBlock $ put addr <> put node

instance BuildInLine Ident where
    buildInLine = fromString . (.unwrap)

instance BuildInLine Argument where
    buildInLine (Argument { name, ty }) = put name <> put ty

instance BuildInLine NodeId where
    buildInLine Ret = "Ret"
    buildInLine Err = "Err"
    buildInLine (Addr addr) = put addr

instance BuildInLine NodeAddr where
    buildInLine = putHex . (.unwrap)

instance BuildInLine Node where
    buildInLine (BasicNode { next, varUpdates }) = "Basic" <> put next <> put varUpdates
    buildInLine (CondNode { left, right, expr }) = "Cond" <> put left <> put right <> put expr
    buildInLine (CallNode { next, functionName, input, output }) = "Call" <> put next <> put functionName <> put input <> put output

instance BuildInLine VarUpdate where
    buildInLine (VarUpdate { varName, ty, expr }) = put varName <> put ty <> put expr

instance BuildInLine Expr where
    buildInLine (Expr { ty, value }) = case value of
        ExprValueVar ident -> "Var" <> put ident <> put ty
        ExprValueOp op args -> "Op" <> put op <> put ty <> put args
        ExprValueNum n -> "Num" <> putHex n <> put ty
        ExprValueType ty' -> "Type" <> put ty'
        ExprValueSymbol ident -> "Symbol" <> put ident <> put ty
        ExprValueToken ident -> "Token" <> put ident <> put ty
        ExprValueSMTExpr _s -> "SMTExpr" <> undefined <> put ty

instance BuildInLine ExprType where
    buildInLine a = case a of
        ExprTypeBool -> "Bool"
        ExprTypeMem -> "Mem"
        ExprTypeDom -> "Dom"
        ExprTypeHtd -> "HTD"
        ExprTypePms -> "PMS"
        ExprTypeUnit -> "UNIT"
        ExprTypeType -> "Type"
        ExprTypeToken -> "Token"
        ExprTypeRelWrapper -> "RelWrapper"
        ExprTypeWord { bits } -> "Word" <> putDec bits
        ExprTypeWordArray { length, bits } -> "WordArray" <> putDec length <> putDec bits
        ExprTypeArray { ty, length } -> "Array" <> put ty <> putDec length
        ExprTypeStruct ident -> "Struct" <> put ident
        ExprTypePtr ty -> "Ptr" <> put ty

instance BuildInLine Op where
    buildInLine a = case a of
        OpPlus -> "Plus"
        OpMinus -> "Minus"
        OpTimes -> "Times"
        OpModulus -> "Modulus"
        OpDividedBy -> "DividedBy"
        OpBWAnd -> "BWAnd"
        OpBWOr -> "BWOr"
        OpBWXOR -> "BWXOR"
        OpAnd -> "And"
        OpOr -> "Or"
        OpImplies -> "Implies"
        OpEquals -> "Equals"
        OpLess -> "Less"
        OpLessEquals -> "LessEquals"
        OpSignedLess -> "SignedLess"
        OpSignedLessEquals -> "SignedLessEquals"
        OpShiftLeft -> "ShiftLeft"
        OpShiftRight -> "ShiftRight"
        OpCountLeadingZeroes -> "CountLeadingZeroes"
        OpCountTrailingZeroes -> "CountTrailingZeroes"
        OpWordReverse -> "WordReverse"
        OpSignedShiftRight -> "SignedShiftRight"
        OpNot -> "Not"
        OpBWNot -> "BWNot"
        OpWordCast -> "WordCast"
        OpWordCastSigned -> "WordCastSigned"
        OpTrue -> "True"
        OpFalse -> "False"
        OpUnspecifiedPrecond -> "UnspecifiedPrecond"
        OpMemUpdate -> "MemUpdate"
        OpMemAcc -> "MemAcc"
        OpIfThenElse -> "IfThenElse"
        OpArrayIndex -> "ArrayIndex"
        OpArrayUpdate -> "ArrayUpdate"
        OpMemDom -> "MemDom"
        OpPValid -> "PValid"
        OpPWeakValid -> "PWeakValid"
        OpPAlignValid -> "PAlignValid"
        OpPGlobalValid -> "PGlobalValid"
        OpPArrayValid -> "PArrayValid"
        OpHTDUpdate -> "HTDUpdate"
        OpWordArrayAccess -> "WordArrayAccess"
        OpWordArrayUpdate -> "WordArrayUpdate"
        OpTokenWordsAccess -> "TokenWordsAccess"
        OpTokenWordsUpdate -> "TokenWordsUpdate"
        OpROData -> "ROData"
        OpStackWrapper -> "StackWrapper"
        OpEqSelectiveWrapper -> "EqSelectiveWrapper"
        OpToFloatingPoint -> "ToFloatingPoint"
        OpToFloatingPointSigned -> "ToFloatingPointSigned"
        OpToFloatingPointUnsigned -> "ToFloatingPointUnsigned"
        OpFloatingPointCast -> "FloatingPointCast"
        OpImpliesROData -> "ImpliesROData"
        OpImpliesStackEquals -> "ImpliesStackEquals"
        OpStackEqualsImplies -> "StackEqualsImplies"

--

instance ParseFile (SMTProofChecks ()) where
    parseFile = do
        blocks <- parseBlocksFileWithTypicalKeyFormat ["Problem", "Pairing"] parsePrettyPairingId $ do
            setupLen <- L.decimal <* eol
            impsLen <- L.decimal <* eol
            setup <- count setupLen (parseSExprWithPlaceholders <* ignoredLines)
            imps <- count impsLen (SMTProofCheckImp () <$> parseSExprWithPlaceholders <* ignoredLines)
            return $ SMTProofCheckGroup { setup, imps }
        let x = map (\(k, v) -> M.insertWith (++) k [v]) blocks
        return . SMTProofChecks . ($ M.empty) . appEndo . foldMap Endo $ x

instance BuildToFile (SMTProofChecks ()) where
    buildToFile checks = mconcat $ foldMap (\(k, v) -> map (buildGroup k) v) (M.toList checks.unwrap)
      where
        buildGroup pairingId (SMTProofCheckGroup { setup, imps }) =
            mconcat . (map (<> "\n")) $
                [ buildTypicalKeyFormat ["Problem", "Pairing"] (fromString (prettyPairingId pairingId)) <> " {"
                , B.decimal (length setup)
                , B.decimal (length imps)
                ] ++ f setup ++ f (map (.term) imps) ++
                [ "}"
                ]
        f ss = map buildSExprWithPlaceholders ss

parseSExprWithPlaceholders :: Parser SExprWithPlaceholders
parseSExprWithPlaceholders = parseGenericSExpr $
    Left <$> parseSExprPlaceholder <|> Right <$> parseAtom

parseSExprPlaceholder :: Parser SExprPlaceholder
parseSExprPlaceholder = between "{" "}" $
    try (SExprPlaceholderMemSort <$ "MemSort") <|> try (SExprPlaceholderMemDomSort <$ "MemDomSort")

buildSExprWithPlaceholders :: SExprWithPlaceholders -> Builder
buildSExprWithPlaceholders = buildGenericSExpr $ either buildSExprPlaceholder buildAtom

buildSExprPlaceholder :: SExprPlaceholder -> Builder
buildSExprPlaceholder placeholder = "{" <> inner <> "}"
  where
    inner = case placeholder of
        SExprPlaceholderMemSort -> "MemSort"
        SExprPlaceholderMemDomSort -> "MemDomSort"

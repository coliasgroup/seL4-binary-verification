{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BV.ConcreteSyntax.GraphLangLike.Instances
    (
    ) where

import BV.ConcreteSyntax.GraphLangLike.Building
import BV.ConcreteSyntax.GraphLangLike.Parsing
import BV.ConcreteSyntax.SExprWithPlaceholders
import BV.Core.Types

import Control.Monad (replicateM)
import Data.Bits (shiftL, (.|.))
import Data.Char (chr, isDigit, ord)
import Data.Functor ((<&>))
import Data.List (elemIndices, unsnoc)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Void (Void)
import Optics (at, (%), (&), (?~))
import Text.Megaparsec
import Text.Megaparsec.Char

--

type Parser = Parsec Void Text

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
            "Basic" -> NodeBasic <$> (BasicNode <$> parseInLine <*> parseInLine)
            "Cond" -> NodeCond <$> (CondNode <$> parseInLine <*> parseInLine <*> parseInLine)
            "Call" -> NodeCall <$> (CallNode <$> parseInLine <*> parseInLine <*> parseInLine <*> parseInLine)
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
            "SMTExpr" -> do
                s <- hexEncodedString
                let r = parse (parseSExprWithPlaceholders <* eof) "SMTExpr" (TL.pack s)
                value <- either (fail . errorBundlePretty) (pure . ExprValueSMTExpr . NotSplit) r
                ty <- parseInLine
                return $ Expr { ty, value }
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
            <> foldMap buildField (M.toAscList fields)
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
            foldMap buildNode (M.toAscList nodes)
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
    buildInLine (NodeBasic (BasicNode { next, varUpdates })) = "Basic" <> put next <> put varUpdates
    buildInLine (NodeCond (CondNode { left, right, expr })) = "Cond" <> put left <> put right <> put expr
    buildInLine (NodeCall (CallNode { next, functionName, input, output })) = "Call" <> put next <> put functionName <> put input <> put output

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
        ExprTypeWordArray { len, bits } -> "WordArray" <> putDec len <> putDec bits
        ExprTypeArray { ty, len } -> "Array" <> put ty <> putDec len
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
        OpStackEquals -> "StackEquals"
        OpImpliesStackEquals -> "ImpliesStackEquals"
        OpStackEqualsImplies -> "StackEqualsImplies"
        OpMemAccWrapper -> "MemAccWrapper"
        OpMemWrapper -> "MemWrapper"

--

instance ParseFile Problems where
    parseFile = Problems . M.fromList <$> some (parseInBlock <&> \problem -> ((.name) <$> problem.sides, problem))

instance Tag t => ParseInBlock (Problem t) where
    parseInBlock = do
        _ <- line $ inLineSymbol "Problem"
        byTagAssocs <- replicateM (numTagValues (Proxy :: Proxy t)) problemSideLine
        let sides = byTagFrom (M.fromList byTagAssocs M.!)
        nodes <- M.fromList <$> manyTill nodeLine (try endLine)
        return $ Problem
            { sides
            , nodes
            }
      where
        nodeLine = line $ (,) <$> parseInLine <*> parseInLine
        endLine = line $ inLineSymbol "EndProblem"
        problemSideLine = line $ do
            _ <- inLineSymbol "Entry"
            entryPoint <- parseInLine
            tag <- parseTag
            name <- parseInLine
            input <- parseInLine
            output <- parseInLine
            let side = ProblemSide { name, input, output, entryPoint }
            return (tag, side)

instance Tag t => ParseInLine (NodeSource t) where
    parseInLine = NodeSource <$> parseTag <*> parseInLine <*> parseInLine

--

instance BuildToFile Problems where
    buildToFile (Problems problems) = intersperse "\n" $ map (buildBlock . buildInBlock . snd) (M.toAscList problems)

instance Tag t => BuildInBlock (Problem t) where
    buildInBlock (Problem { sides, nodes }) =
        lineInBlock "Problem"
            <> foldMap (withTag problemSideLine) (withTags sides)
            <> foldMap nodeLine (M.toAscList nodes)
            <> lineInBlock "EndProblem"
      where
        problemSideLine tag (ProblemSide { name, input, output, entryPoint }) = lineInBlock $
            "Entry"
                <> put entryPoint
                <> putTag tag
                <> put name
                <> put input
                <> put output
        nodeLine (addr, node) = lineInBlock $ put addr <> put node

instance Tag t => BuildInLine (NodeSource t) where
    buildInLine (NodeSource { tag, functionName, nodeAddr }) = putTag tag <> put functionName <> put nodeAddr

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
    buildToFile (StackBounds stackBounds) = buildBlock $ foldMap f (M.toAscList stackBounds)
      where
        f (ident, expr) = lineInBlock $ "StackBound" <> put ident <> put expr

--

instance Tag t => ParseInLine (InlineScriptEntry t) where
    parseInLine = InlineScriptEntry <$> parseInLine <*> parseInLine

instance Tag t => ParseInLine (NodeBySource t) where
    parseInLine = NodeBySource <$> parseInLine <*> parseInLine

instance Tag t => BuildInLine (InlineScriptEntry t) where
    buildInLine (InlineScriptEntry { nodeBySource, inlinedFunctionName }) = put nodeBySource <> put inlinedFunctionName

instance Tag t => BuildInLine (NodeBySource t) where
    buildInLine (NodeBySource { nodeSource, indexInProblem }) = put nodeSource <> putDec indexInProblem

--

instance ParseInLine (ProofScript ()) where
    parseInLine = ProofScript <$> parseInLine

instance ParseInLine (ProofNodeWith ()) where
    parseInLine = ProofNodeWith () <$> parseInLine

instance ParseInLine (ProofNode ()) where
    parseInLine = word >>= \case
        "Leaf" -> return ProofNodeLeaf
        "Restr" -> ProofNodeRestr <$> parseInLine
        "CaseSplit" -> ProofNodeCaseSplit <$> parseInLine
        "Split" -> ProofNodeSplit <$> parseInLine
        "SingleRevInduct" -> ProofNodeSingleRevInduct <$> parseInLine
        _ -> fail "invalid proof node"

instance ParseInLine (RestrProofNode ()) where
    parseInLine =
        RestrProofNode
            <$> parseInLine
            <*> parseTag
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

instance ParseInLine (CaseSplitProofNode ()) where
    parseInLine =
        CaseSplitProofNode
            <$> parseInLine
            <*> parseTag
            <*> parseInLine
            <*> parseInLine

instance ParseInLine (SplitProofNode ()) where
    parseInLine =
        SplitProofNode
            <$> parseInLine
            <*> parseInLine
            <*> traverse (const parseInLine) (pure ())
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

instance ParseInLine (SingleRevInductProofNode ()) where
    parseInLine =
        SingleRevInductProofNode
            <$> parseInLine
            <*> parseTag
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

instance BuildInLine (ProofScript ()) where
    buildInLine proofScript = put proofScript.root

instance BuildInLine (ProofNodeWith ()) where
    buildInLine proofNodeWith = put proofNodeWith.node

instance BuildInLine (ProofNode ()) where
    buildInLine = \case
        ProofNodeLeaf -> "Leaf"
        ProofNodeRestr node-> "Restr" <> put node
        ProofNodeCaseSplit node-> "CaseSplit" <> put node
        ProofNodeSplit node -> "Split" <> put node
        ProofNodeSingleRevInduct node -> "SingleRevInduct" <> put node

instance BuildInLine (RestrProofNode ()) where
    buildInLine range =
           put range.point
        <> putTag range.tag
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

instance BuildInLine (CaseSplitProofNode ()) where
    buildInLine node =
           put node.addr
        <> putTag node.tag
        <> put node.left
        <> put node.right

instance BuildInLine (SplitProofNode ()) where
    buildInLine node =
           putDec node.n
        <> putDec node.loopRMax
        <> put node.details.asm
        <> put node.details.c
        <> put node.eqs
        <> put node.p1
        <> put node.p2

instance BuildInLine SplitProofNodeDetails where
    buildInLine details =
           put details.split
        <> putDec details.seqStart
        <> putDec details.step
        <> put details.eqs

instance BuildInLine (SingleRevInductProofNode ()) where
    buildInLine node =
           put node.point
        <> putTag node.tag
        <> putDec node.n
        <> put node.eqs
        <> put node.pred_
        <> putDec node.nBound
        <> put node.child

instance BuildInLine Lambda where
    buildInLine (Lambda { freeVar, freeVarTy, expr }) =
        "Lambda" <> put freeVar <> put freeVarTy <> put expr

--

instance RefineTag t => ParseInLine (PairingEq t) where
    parseInLine = PairingEq <$> parseInLine <*> parseInLine

instance RefineTag t => BuildInLine (PairingEq t) where
    buildInLine eq = put eq.lhs <> put eq.rhs

instance RefineTag t => ParseInLine (PairingEqSide t) where
    parseInLine = PairingEqSide <$> parseInLine <*> parseInLine

instance RefineTag t => BuildInLine (PairingEqSide t) where
    buildInLine side = put side.quadrant <> put side.expr

parseTag :: Tag t => Parser t
parseTag = wordWithOr "invalid tag" parsePrettyTag

putTag :: Tag t => t -> LineBuilder
putTag = putWord . prettyTag

instance RefineTag t => ParseInLine (PairingEqSideQuadrant t) where
    parseInLine = wordWithOr "invalid pairing eq side quadrant" $ \w -> do
        (_, i) <- unsnoc $ elemIndices '_' w
        let (tagS, '_':dirS) = splitAt i w
        PairingEqSideQuadrant <$> parsePrettyTag tagS <*> parsePairingEqDirection dirS

instance RefineTag t => BuildInLine (PairingEqSideQuadrant t) where
    buildInLine = putWord . prettyPairingEqSideQuadrant

instance ParseInLine PairingEqDirection where
    parseInLine = wordWithOr "invalid pairing eq direction" parsePairingEqDirection

parsePairingEqDirection :: String -> Maybe PairingEqDirection
parsePairingEqDirection = \case
    "IN" -> Just PairingEqDirectionIn
    "OUT" -> Just PairingEqDirectionOut
    _ -> Nothing

instance BuildInLine PairingEqDirection where
    buildInLine = putWord . prettyPairingEqDirection

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
    parseInLine = VisitWithTag <$> parseInLine <*> parseTag

instance BuildInLine VisitWithTag where
    buildInLine visit = put visit.visit <> putTag visit.tag

instance ParseInLine Visit where
    parseInLine = Visit <$> parseInLine <*> parseInLine

instance BuildInLine Visit where
    buildInLine visit = put visit.nodeId <> put visit.restrs

instance ParseInLine Restr where
    parseInLine = Restr <$> parseInLine <*> parseInLine

instance BuildInLine Restr where
    buildInLine restr = put restr.nodeAddr <> put restr.visitCount

instance ParseInLine VisitCount where
    parseInLine = inLineSymbol "VC" *> (VisitCount <$> parseInLine <*> parseInLine)

instance BuildInLine VisitCount where
    buildInLine visitCount = "VC" <> putManyWith putDec visitCount.numbers <> putManyWith putDec visitCount.offsets

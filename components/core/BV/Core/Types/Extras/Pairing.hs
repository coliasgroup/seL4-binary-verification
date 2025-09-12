module BV.Core.Types.Extras.Pairing
    ( asmIn
    , asmOut
    , cIn
    , cOut
    , leftIn
    , leftInQ
    , leftOut
    , leftOutQ
    , rightIn
    , rightInQ
    , rightOut
    , rightOutQ
    , (===)
    ) where

import BV.Core.Types

leftInQ :: RefineTag t => PairingEqSideQuadrant t
leftInQ = PairingEqSideQuadrant leftTag PairingEqDirectionIn

leftOutQ :: RefineTag t => PairingEqSideQuadrant t
leftOutQ = PairingEqSideQuadrant leftTag PairingEqDirectionOut

rightInQ :: RefineTag t => PairingEqSideQuadrant t
rightInQ = PairingEqSideQuadrant rightTag PairingEqDirectionIn

rightOutQ :: RefineTag t => PairingEqSideQuadrant t
rightOutQ = PairingEqSideQuadrant rightTag PairingEqDirectionOut

leftIn :: RefineTag t => GraphExpr -> PairingEqSide t
leftIn = PairingEqSide leftInQ

leftOut :: RefineTag t => GraphExpr -> PairingEqSide t
leftOut = PairingEqSide leftOutQ

rightIn :: RefineTag t => GraphExpr -> PairingEqSide t
rightIn = PairingEqSide rightInQ

rightOut :: RefineTag t => GraphExpr -> PairingEqSide t
rightOut = PairingEqSide rightOutQ

asmIn :: GraphExpr -> PairingEqSide AsmRefineTag
asmIn = leftIn

asmOut :: GraphExpr -> PairingEqSide AsmRefineTag
asmOut = leftOut

cIn :: GraphExpr -> PairingEqSide AsmRefineTag
cIn = rightIn

cOut :: GraphExpr -> PairingEqSide AsmRefineTag
cOut = rightOut

infix 4 ===

(===) :: PairingEqSide t -> PairingEqSide t -> PairingEq t
(===) = PairingEq

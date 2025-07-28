module BV.Core.Types.Extras.Pairing
    ( leftIn
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

leftIn :: RefineTag t => Expr -> PairingEqSide t
leftIn = PairingEqSide leftInQ

leftOut :: RefineTag t => Expr -> PairingEqSide t
leftOut = PairingEqSide leftOutQ

rightIn :: RefineTag t => Expr -> PairingEqSide t
rightIn = PairingEqSide rightInQ

rightOut :: RefineTag t => Expr -> PairingEqSide t
rightOut = PairingEqSide rightOutQ

infix 4 ===

(===) :: PairingEqSide t -> PairingEqSide t -> PairingEq t
(===) = PairingEq

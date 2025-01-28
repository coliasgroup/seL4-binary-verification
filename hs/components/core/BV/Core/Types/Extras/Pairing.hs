module BV.Core.Types.Extras.Pairing
    ( asmIn
    , asmInQ
    , asmOut
    , asmOutQ
    , cIn
    , cInQ
    , cOut
    , cOutQ
    , (===)
    ) where

import BV.Core.Types

asmInQ :: PairingEqSideQuadrant
asmInQ = PairingEqSideQuadrant Asm PairingEqDirectionIn

asmOutQ :: PairingEqSideQuadrant
asmOutQ = PairingEqSideQuadrant Asm PairingEqDirectionOut

cInQ :: PairingEqSideQuadrant
cInQ = PairingEqSideQuadrant C PairingEqDirectionIn

cOutQ :: PairingEqSideQuadrant
cOutQ = PairingEqSideQuadrant C PairingEqDirectionOut

asmIn :: Expr -> PairingEqSide
asmIn = PairingEqSide asmInQ

asmOut :: Expr -> PairingEqSide
asmOut = PairingEqSide asmOutQ

cIn :: Expr -> PairingEqSide
cIn = PairingEqSide cInQ

cOut :: Expr -> PairingEqSide
cOut = PairingEqSide cOutQ

infix 4 ===

(===) :: PairingEqSide -> PairingEqSide -> PairingEq
(===) = PairingEq

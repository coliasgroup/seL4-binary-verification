module BV.System.SeL4
    ( defaultSeL4AsmFunctionFilter
    ) where

import BV.Core.Types

defaultSeL4AsmFunctionFilter :: Ident -> Bool
defaultSeL4AsmFunctionFilter = not . (`elem` ignore)
  where
    ignore = map Ident
        [ "fastpath_call"
        , "fastpath_reply_recv"
        , "c_handle_syscall"
        , "arm_swi_syscall"
        ]

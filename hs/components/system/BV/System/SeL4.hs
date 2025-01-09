module BV.System.SeL4
    ( defaultSeL4AsmFunctionFilter
    , defaultTestSeL4TargetDir
    ) where

import BV.Core.Types
import BV.System.TargetDir

defaultSeL4AsmFunctionFilter :: Ident -> Bool
defaultSeL4AsmFunctionFilter = not . (`elem` ignore)
  where
    ignore = map Ident
        [ "fastpath_call"
        , "fastpath_reply_recv"
        , "c_handle_syscall"
        , "arm_swi_syscall"
        ]

defaultTestSeL4TargetDir :: FilePath -> TargetDir
defaultTestSeL4TargetDir path = TargetDir
    { path
    , asmFunctionFilter = defaultSeL4AsmFunctionFilter
    }

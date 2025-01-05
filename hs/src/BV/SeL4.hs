module BV.SeL4
    ( defaultSeL4AsmFunctionFilter
    , defaulttestSeL4TargetDir
    ) where

import GHC.Generics (Generic)
import System.FilePath

import BV.Program
import BV.TargetDir

defaultSeL4AsmFunctionFilter :: Ident -> Bool
defaultSeL4AsmFunctionFilter = not . (`elem` ignore)
  where
    ignore = map Ident
        [ "fastpath_call"
        , "fastpath_reply_recv"
        , "c_handle_syscall"
        , "arm_swi_syscall"
        ]

defaulttestSeL4TargetDir :: FilePath -> TargetDir
defaulttestSeL4TargetDir path = TargetDir
    { path
    , asmFunctionFilter = defaultSeL4AsmFunctionFilter
    }

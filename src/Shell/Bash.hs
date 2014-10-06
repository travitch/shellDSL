module Shell.Bash ( runBash ) where

import qualified Control.Monad.State.Strict as MS

import Shell.Internal

runBash :: ShellM a -> String
runBash (ShellM st) = walkAST ast
  where
    ast = MS.execState st emptyState

walkAST :: ShellState -> String
walkAST = undefined

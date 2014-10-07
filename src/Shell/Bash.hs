module Shell.Bash ( runBash ) where

import qualified Control.Concurrent.Supply as U
import qualified Control.Monad.State.Strict as MS

import Shell.Internal

runBash :: (Show a) => ShellM a -> IO String
runBash st = do
  s <- U.newSupply
  let shell = MS.evalStateT st (ShellState s)
  return (show shell)
--   where
--     ast = MS.execState st undefined

-- walkAST :: ShellState -> String
-- walkAST = undefined

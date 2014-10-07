module Shell.Bash ( runBash ) where

import qualified Control.Concurrent.Supply as U
import qualified Control.Monad.Free as FR
import qualified Control.Monad.State.Strict as MS
import Text.Printf ( printf )

import Shell.Internal

runBash :: ShellM () -> IO String
runBash st = do
  s <- U.newSupply
  let shell = MS.evalStateT st (ShellState s)
  walkAST shell
  return (show shell)

walkAST :: Shell -> IO ()
walkAST = FR.iterM $ \f ->
  case f of
    RunSync uid cmd next -> do
      printf "%s # (%d)\n" (show cmd) uid
      next
    RunAsync uid cmd next -> do
      printf "%s & # (%d)\n" (show cmd) uid
      next
    Wait _ (Async a) next -> do
      printf "wait # on %d\n" a
      next
    Done -> return ()

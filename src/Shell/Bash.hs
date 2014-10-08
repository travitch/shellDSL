module Shell.Bash ( runBash ) where

import qualified Control.Concurrent.Supply as U
import qualified Control.Monad.Free as FR
import qualified Control.Monad.State.Strict as MS
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Builder as BS
import Data.Monoid
import Text.Printf ( printf )

import Shell.Internal
import Shell.Formatter.Base

bashFormatter :: Formatter
bashFormatter = defaultFormatter

runBash :: ShellM () -> IO BS.ByteString
runBash st = do
  s <- U.newSupply
  let shell = MS.evalStateT st (ShellState s)
  return $ renderScript bashFormatter shell

data RenderState =
  RenderState { sBuilder :: !BS.Builder
              }

emptyRenderState :: RenderState
emptyRenderState =
  RenderState { sBuilder = mempty
              }

type Render = MS.State RenderState

renderScript :: Formatter -> Shell -> BS.ByteString
renderScript fmt s = BS.toLazyByteString $ sBuilder $ comp
  where
    comp = MS.execState (renderScriptM fmt s) emptyRenderState

-- This function will have to handle the recursive traversal of block
-- structures.  The formatters can't recurse within the Render monad,
-- so we have to do it here.  Formatters will need to be flexible to
-- return intermediate strings.
renderScriptM :: Formatter -> Shell -> Render ()
renderScriptM fmt = FR.iterM $ \f ->
  case f of
    RunSync uid _ next -> do
      writeLineUID uid (fmtAction fmt fmt (AnyShell f))
      next
    RunAsync uid _ next -> do
      writeLineUID uid (fmtAction fmt fmt (AnyShell f))
      next
    Wait _ _ next -> do
      writeLine (fmtAction fmt fmt (AnyShell f))
      next
    While _ _ _ next -> do
      writeLine (fmtAction fmt fmt (AnyShell f))
      next
    Until _ _ _ next -> do
      writeLine (fmtAction fmt fmt (AnyShell f))
      next
    If _ _ _ next -> do
      writeLine (fmtAction fmt fmt (AnyShell f))
      next
    SubBlock _ _ next -> do
      writeLine (fmtAction fmt fmt (AnyShell f))
      next
    GetExitCode _ _ next -> do
      writeLine (fmtAction fmt fmt (AnyShell f))
      next
    UnsetEnv _ _ next -> do
      writeLine (fmtAction fmt fmt (AnyShell f))
      next
    SetEnv _ _ _ next -> do
      writeLine (fmtAction fmt fmt (AnyShell f))
      next
    Done -> return ()

-- FIXME: Write a withIndentation operator to add to the current
-- indentation and then subtract when the scope is exited.  This way,
-- formatters won't need to know about it

writeLine :: String -> Render ()
writeLine line =
  MS.modify $ \s -> s { sBuilder = sBuilder s <> BS.stringUtf8 line <> BS.charUtf8 '\n' }

writeLineUID :: Int -> String -> Render ()
writeLineUID uid line =
  MS.modify $ \s -> s { sBuilder = sBuilder s <> BS.stringUtf8 line <> BS.stringUtf8 str }
  where
    str = printf " # (uid: %d)\n" uid

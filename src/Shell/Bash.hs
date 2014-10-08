module Shell.Bash ( runBash ) where

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
  shell <- flattenShell st
  return $ renderScript bashFormatter shell

data RenderState =
  RenderState { sBuilder :: !BS.Builder
              , sFormat :: Formatter
              }

emptyRenderState :: Formatter -> RenderState
emptyRenderState fmt =
  RenderState { sBuilder = mempty
              , sFormat = fmt
              }

type Render = MS.State RenderState

renderScript :: Formatter -> [Shell] -> BS.ByteString
renderScript fmt s = BS.toLazyByteString $ sBuilder comp
  where
    comp = MS.execState (mapM_ renderScriptM s) (emptyRenderState fmt)

-- This function will have to handle the recursive traversal of block
-- structures.  The formatters can't recurse within the Render monad,
-- so we have to do it here.  Formatters will need to be flexible to
-- return intermediate strings.
renderScriptM :: Shell -> Render ()
renderScriptM s = do
  fmt <- MS.gets sFormat
  case s of
    RunSync uid _ -> writeLineUID uid (fmtAction fmt fmt s)
    RunAsync uid _ -> writeLineUID uid (fmtAction fmt fmt s)
    Wait {} -> writeLine (fmtAction fmt fmt s)
    GetExitCode {} -> writeLine (fmtAction fmt fmt s)
    UnsetEnv {} -> writeLine (fmtAction fmt fmt s)
    SetEnv {} -> writeLine (fmtAction fmt fmt s)

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

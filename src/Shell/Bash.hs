module Shell.Bash ( runBash ) where

import qualified Control.Monad.State.Strict as MS
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Builder as BS
import Data.Monoid
import Text.Printf ( printf )
import qualified Text.PrettyPrint.Mainland as PP

import Shell.Internal
import Shell.Formatter.Base

bashFormatter :: Formatter
bashFormatter = defaultFormatter

runBash :: ShellM () -> IO String
runBash st = do
  shell <- flattenShell st
  return $ renderScript bashFormatter shell

data RenderState =
  RenderState { sBuilder :: !PP.Doc
              , sFormat :: Formatter
              }

emptyRenderState :: Formatter -> RenderState
emptyRenderState fmt =
  RenderState { sBuilder = mempty
              , sFormat = fmt
              }

type Render = MS.State RenderState

renderScript :: Formatter -> [Shell] -> String
renderScript fmt s = PP.pretty 0 $ sBuilder comp
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
    _ -> writeLine (fmtAction fmt fmt s)

-- FIXME: Write a withIndentation operator to add to the current
-- indentation and then subtract when the scope is exited.  This way,
-- formatters won't need to know about it

writeLine :: PP.Doc -> Render ()
writeLine doc =
  MS.modify $ \s -> s { sBuilder = sBuilder s <> doc <> PP.line }

writeLineUID :: Int -> PP.Doc -> Render ()
writeLineUID uid doc =
  MS.modify $ \s -> s { sBuilder = sBuilder s <> doc <> PP.string str <> PP.line }
  where
    str = printf " # (uid: %d)" uid

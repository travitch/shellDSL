module Shell.Bash ( runBash ) where

import qualified Control.Concurrent.Supply as U
import qualified Control.Monad.Free as FR
import qualified Control.Monad.State.Strict as MS
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
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

renderScriptM :: Formatter -> Shell -> Render ()
renderScriptM fmt = FR.iterM $ \f ->
  case f of
    RunSync uid cmd next -> do
      writeLineUID uid (fmtCommand fmt fmt cmd)
      next
    RunAsync uid cmd next -> do
      writeLineUID uid (printf "%s &" (fmtCommand fmt fmt cmd))
      next
    Wait _ (Async a) next -> do
      writeLine (printf "wait # on %d" a)
      next

writeLine :: String -> Render ()
writeLine line =
  MS.modify $ \s -> s { sBuilder = sBuilder s <> BS.stringUtf8 line <> BS.charUtf8 '\n' }

writeLineUID :: Int -> String -> Render ()
writeLineUID uid line =
  MS.modify $ \s -> s { sBuilder = sBuilder s <> BS.stringUtf8 line <> BS.stringUtf8 str }
  where
    str = printf " # (uid: %d)\n" uid

-- walkAST :: Shell -> IO ()
-- walkAST = FR.iterM $ \f ->
--   case f of
--     RunSync uid cmd next -> do
--       printf "%s # (%d)\n" (show cmd) uid
--       next
--     RunAsync uid cmd next -> do
--       printf "%s & # (%d)\n" (show cmd) uid
--       next
--     Wait _ (Async a) next -> do
--       printf "wait # on %d\n" a
--       next
--     Done -> return ()

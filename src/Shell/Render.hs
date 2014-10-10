module Shell.Render ( renderScript ) where

import qualified Control.Monad.State.Strict as MS
import Data.Monoid
import qualified Text.PrettyPrint.Mainland as PP

import qualified Shell.Analysis as A
import qualified Shell.Diagnostic as D
import Shell.Internal
import Shell.Formatter.Base

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

renderScript :: Formatter -> [Shell] -> Either [D.Diagnostic] String
renderScript fmt s =
  case A.errors ares of
    [] -> Right $ PP.pretty 0 $ fmtPreamble fmt fmt PP.<//> sBuilder comp
    errs -> Left errs
  where
    comp = MS.execState (mapM_ (renderScriptM ares) s) (emptyRenderState fmt)
    ares = A.analyze s

-- This function will have to handle the recursive traversal of block
-- structures.  The formatters can't recurse within the Render monad,
-- so we have to do it here.  Formatters will need to be flexible to
-- return intermediate strings.
renderScriptM :: A.Analysis -> Shell -> Render ()
renderScriptM ares s = do
  fmt <- MS.gets sFormat
  writeLine (fmtAction fmt fmt ares s)

writeLine :: PP.Doc -> Render ()
writeLine doc =
  MS.modify $ \s -> s { sBuilder = sBuilder s <> doc <> PP.line }

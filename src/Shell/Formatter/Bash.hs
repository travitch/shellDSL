-- | Render an abstract shell script as a bash script
module Shell.Formatter.Bash (
  runBash,
  bashFormatter
  ) where

import qualified Shell.Diagnostic as D
import qualified Shell.Formatter.Base as F
import qualified Shell.Internal as I
import qualified Shell.Optimize as O
import qualified Shell.Render as R

-- | A formatter for bash scripts
bashFormatter :: F.Formatter
bashFormatter = F.defaultFormatter

-- | Turn an abstract shell script specification into a bash script.
runBash :: I.ShellM () -> IO (Maybe String, [D.Diagnostic])
runBash st = do
  shell <- I.flattenShell st
  let (sh, odiags) = O.optimize O.defaultOptimizer shell
  case R.renderScript bashFormatter sh of
    Left errs -> return (Nothing, errs ++ odiags)
    Right script -> return (Just script, odiags)


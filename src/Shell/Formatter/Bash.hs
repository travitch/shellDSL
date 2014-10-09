-- | Render an abstract shell script as a bash script
module Shell.Formatter.Bash (
  runBash,
  bashFormatter
  ) where

import qualified Shell.Formatter.Base as F
import qualified Shell.Internal as I
import qualified Shell.Optimize as O
import qualified Shell.Render as R

-- | A formatter for bash scripts
bashFormatter :: F.Formatter
bashFormatter = F.defaultFormatter

-- | Turn an abstract shell script specification into a bash script.
runBash :: I.ShellM () -> IO String
runBash st = do
  shell <- I.flattenShell st
  return $ R.renderScript bashFormatter (O.optimize O.defaultOptimizer shell)


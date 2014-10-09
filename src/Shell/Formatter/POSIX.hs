module Shell.Formatter.POSIX (
  runSh,
  posixFormatter
  ) where

import qualified Text.PrettyPrint.Mainland as PP

import qualified Shell.Diagnostic as D
import qualified Shell.Formatter.Base as F
import qualified Shell.Internal as I
import qualified Shell.Optimize as O
import qualified Shell.Render as R

-- | A formatter for bash scripts
posixFormatter :: F.Formatter
posixFormatter = F.defaultFormatter { F.fmtPreamble = \_ -> preamble }

preamble :: PP.Doc
preamble = PP.stack [ PP.string "#!/bin/sh"
                    , PP.string "set -e"
                    , PP.string "set -u"
                    , PP.line
                    ]

-- | Turn an abstract shell script specification into a bash script.
runSh :: I.ShellM () -> IO (Maybe String, [D.Diagnostic])
runSh st = do
  shell <- I.flattenShell st
  let (sh, odiags) = O.optimize O.defaultOptimizer shell
  case R.renderScript posixFormatter sh of
    Left errs -> return (Nothing, errs ++ odiags)
    Right script -> return (Just script, odiags)


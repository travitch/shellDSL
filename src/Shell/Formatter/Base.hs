module Shell.Formatter.Base (
  Formatter(..),
  defaultFormatter
  ) where

import qualified Data.List as L
import Data.Monoid
import Text.Printf ( printf )

import Shell.Internal

data Formatter =
  Formatter { fmtCommand :: Formatter -> Command -> String
              -- ^ Format individual commands, primarily connectives
            , fmtCommandSpec :: Formatter -> CommandSpec -> String
              -- ^ Format a command with its arguments
            , fmtWord :: Formatter -> BWord -> String
              -- ^ Format a word-like thing (string with interpolation)
            , fmtStream :: Formatter -> StreamSpec -> String
              -- ^ Format IO redirection specifiers
            , fmtEscape :: String -> String
            }

defaultFormatter :: Formatter
defaultFormatter = Formatter { fmtWord = formatWord
                             , fmtCommand = formatCommand
                             , fmtCommandSpec = formatCommandSpec
                             , fmtStream = formatStream
                             , fmtEscape = id
                             }

formatStream :: Formatter -> StreamSpec -> String
formatStream _ _ = ""

formatCommand :: Formatter -> Command -> String
formatCommand fmt cmd =
  case cmd of
    Command cspec redirs
      | null redirStr -> fmtCommandSpec fmt fmt cspec
      | otherwise -> printf "%s %s" (fmtCommandSpec fmt fmt cspec) redirStr
      where
        redirStr = fmtStream fmt fmt redirs

formatCommandSpec :: Formatter -> CommandSpec -> String
formatCommandSpec fmt spec = L.intercalate " " $ map (fmtWord fmt fmt) (commandName spec : commandArguments spec)

-- quote if there is a space in the string anywhere
formatWord :: Formatter -> BWord -> String
formatWord fmt (BWord spans) = mconcat $ map (formatSpan fmt) spans

-- This obviously will need some work later to handle escaping and whatnot
formatSpan :: Formatter -> BSpan -> String
formatSpan fmt spn =
  case spn of
    BString s -> s
    BVariable s -> printf "${%s}" s
    BUnsafeVariable s -> printf "${%s}" s
    BCommand cmd -> printf "$(%s)" (fmtCommand fmt fmt cmd)

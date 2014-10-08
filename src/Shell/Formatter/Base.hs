{-# LANGUAGE GADTs #-}
module Shell.Formatter.Base (
  Formatter(..),
  AnyShell(..),
  defaultFormatter
  ) where

import qualified Data.List as L
import Data.Monoid
import Text.Printf ( printf )

import Shell.Internal

data AnyShell where
  AnyShell :: ShellF a -> AnyShell

data Formatter =
  Formatter { fmtCommand :: Formatter -> Command -> String
              -- ^ Format individual commands, primarily connectives
            , fmtCommandSpec :: Formatter -> CommandSpec -> String
              -- ^ Format a command with its arguments
            , fmtAction :: Formatter -> AnyShell -> String
              -- ^ Format a top-level action (e.g., run, conditionals)
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
                             , fmtAction = formatAction
                             , fmtStream = formatStream
                             , fmtEscape = id
                             }

formatAction :: Formatter -> AnyShell -> String
formatAction fmt (AnyShell shell) =
  case shell of
    RunSync _ cmd _ -> fmtCommand fmt fmt cmd
    RunAsync _ cmd _ -> printf "%s &" (fmtCommand fmt fmt cmd)
    Wait _ (Async a) _ -> printf "wait # on %d" a


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

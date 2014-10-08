{-# LANGUAGE GADTs #-}
module Shell.Formatter.Base (
  Formatter(..),
  defaultFormatter
  ) where

import Data.Monoid
import Text.Printf ( printf )
import Text.PrettyPrint.Mainland as PP

import Shell.Internal

data Formatter =
  Formatter { fmtCommand :: Formatter -> Command -> Doc
              -- ^ Format individual commands, primarily connectives
            , fmtCommandSpec :: Formatter -> CommandSpec -> Doc
              -- ^ Format a command with its arguments
            , fmtAction :: Formatter -> Shell -> Doc
              -- ^ Format a top-level action (e.g., run, conditionals)
            , fmtWord :: Formatter -> BWord -> Doc
              -- ^ Format a word-like thing (string with interpolation)
            , fmtStream :: Formatter -> StreamSpec -> Doc
              -- ^ Format IO redirection specifiers
            , fmtIndentation :: Int
            }

defaultFormatter :: Formatter
defaultFormatter = Formatter { fmtWord = formatWord
                             , fmtCommand = formatCommand
                             , fmtCommandSpec = formatCommandSpec
                             , fmtAction = formatAction
                             , fmtStream = formatStream
                             , fmtIndentation = 2
                             }


formatAction :: Formatter -> Shell -> Doc
formatAction fmt shell =
  case shell of
    RunSync _ cmd -> fmtCommand fmt fmt cmd
    RunAsync _ cmd -> fmtCommand fmt fmt cmd <+> PP.string "&"
    Wait _ (Async a) -> PP.string $ printf "wait # on %d" a
    While _ cond body ->
      let bdoc = PP.stack $ map (formatAction fmt) body
      in PP.string "while FIXME/COND; do" <//> PP.indent (fmtIndentation fmt) bdoc <//> PP.string "done"
--    SubBlock _ shell' _ -> printf "(\n%s\n)" (formatAction fmt (AnyShell shell'))


formatStream :: Formatter -> StreamSpec -> Doc
formatStream _ _ = mempty

formatCommand :: Formatter -> Command -> Doc
formatCommand fmt cmd =
  case cmd of
    Command cspec redirs
      | hasNoRedirections redirs -> fmtCommandSpec fmt fmt cspec
      | otherwise -> fmtCommandSpec fmt fmt cspec <+> fmtStream fmt fmt redirs
    And c1 c2 -> formatCommand fmt c1 <+> PP.string "&&" <+> formatCommand fmt c2
    Or c1 c2 -> formatCommand fmt c1 <+> PP.string "||" <+> formatCommand fmt c2
    Sequence c1 c2 -> formatCommand fmt c1 <+> PP.string ";" <+> formatCommand fmt c2

hasNoRedirections :: StreamSpec -> Bool
hasNoRedirections _ = True

formatCommandSpec :: Formatter -> CommandSpec -> Doc
formatCommandSpec fmt spec =
  PP.spread $ map (fmtWord fmt fmt) (commandName spec : commandArguments spec)

-- quote if there is a space in the string anywhere
formatWord :: Formatter -> BWord -> Doc
formatWord fmt (BWord spans) = mconcat $ map (formatSpan fmt) spans

-- This obviously will need some work later to handle escaping and whatnot
formatSpan :: Formatter -> BSpan -> Doc
formatSpan fmt spn =
  case spn of
    BString s -> PP.string s
    BVariable s -> PP.string $ printf "${%s}" s
    BUnsafeVariable s -> PP.string $ printf "${%s}" s
    BCommand cmd -> PP.string "$(" <> (fmtCommand fmt fmt cmd) <> PP.string ")"


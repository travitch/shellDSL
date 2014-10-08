{-# LANGUAGE GADTs #-}
module Shell.Formatter.Base (
  Formatter(..),
  defaultFormatter
  ) where

import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Sequence as Seq
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
            , fmtTest :: Formatter -> TestSpec -> Doc
            , fmtIndentation :: Int
            }

defaultFormatter :: Formatter
defaultFormatter = Formatter { fmtWord = formatWord
                             , fmtCommand = formatCommand
                             , fmtCommandSpec = formatCommandSpec
                             , fmtAction = formatAction
                             , fmtStream = formatStream
                             , fmtTest = formatTest
                             , fmtIndentation = 2
                             }


formatAction :: Formatter -> Shell -> Doc
formatAction fmt shell =
  case shell of
    RunSync _ cmd -> fmtCommand fmt fmt cmd
    RunAsync _ cmd -> fmtCommand fmt fmt cmd <+> PP.string "&"
    Wait _ (Async a) -> PP.string $ printf "wait # on %d" a
    SetEnv _ str val -> PP.string str <> PP.char '=' <> fmtWord fmt fmt val
    UnsetEnv _ str -> PP.string "unset" <+> PP.string str
    ExportEnv _ str -> PP.string "export" <+> PP.string str
    While _ cond body ->
      let bdoc = PP.stack $ map (formatAction fmt) body
      in PP.string "while" <+> fmtCommand fmt fmt cond <> PP.string "; do" <//> PP.indent (fmtIndentation fmt) bdoc <//> PP.string "done"
    Until _ cond body ->
      let bdoc = PP.stack $ map (formatAction fmt) body
      in PP.string "until" <+> fmtCommand fmt fmt cond <> PP.string "; do" <//> PP.indent (fmtIndentation fmt) bdoc <//> PP.string "done"
    SubBlock _ Nothing body ->
      let bdoc = PP.stack $ map (formatAction fmt) body
      in PP.string "(" <//> PP.indent (fmtIndentation fmt) bdoc <//> PP.string ")"
    SubBlock _ (Just var) body ->
      let bdoc = PP.stack $ map (formatAction fmt) body
      in PP.string var <> PP.string "=$(" <//> PP.indent (fmtIndentation fmt) bdoc <//> PP.string ")"

formatTest :: Formatter -> TestSpec -> Doc
formatTest fmt ts =
  case ts of
    TSFileExists w -> prefixTest "-e" w
    TSFileIsRegular w -> prefixTest "-f" w
    TSFileIsDirectory w -> prefixTest "-d" w
    TSFileIsSymlink w -> prefixTest "-h" w
    TSPipeExists w -> prefixTest "-p" w
    TSFileIsReadableY w -> prefixTest "-r" w
    TSFileNotEmpty w -> prefixTest "-s" w
    TSFDOpenOnTerminal w -> prefixTest "-t" w
    TSFileWritableY w -> prefixTest "-w" w
    TSFileExecutableY w -> prefixTest "-x" w
    TSFileEffectivelyOwnedY w -> prefixTest "-O" w
    TSFileEffectivelyOwnedG w -> prefixTest "-G" w
    TSFileNewer w1 w2 -> infixTest w1 "-nt" w2
    TSFileOlder w1 w2 -> infixTest w1 "-ot" w2
    TSStringEmpty w -> prefixTest "-z" w
    TSStringNotEmpty w -> prefixTest "-n" w
    TSStringIdentical w1 w2 -> infixTest w1 "=" w2
    TSStringNotIdentical w1 w2 -> infixTest w1 "!=" w2
    TSStringLT w1 w2 -> infixTest w1 "<" w2
    TSStringGT w1 w2 -> infixTest w1 ">" w2
    TSNegate t' -> PP.string "!" <+> formatTest fmt t'
    TSIntEq w1 w2 -> infixTest w1 "-eq" w2
    TSIntNeq w1 w2 -> infixTest w1 "-ne" w2
    TSIntLT w1 w2 -> infixTest w1 "-lt" w2
    TSIntGT w1 w2 -> infixTest w1 "-gt" w2
    TSIntLE w1 w2 -> infixTest w1 "-le" w2
    TSIntGE w1 w2 -> infixTest w1 "-ge" w2
  where
    prefixTest str w = PP.string str <+> fmtWord fmt fmt w
    infixTest w1 str w2 = fmtWord fmt fmt w1 <+> PP.string str <+> fmtWord fmt fmt w2

formatStream :: Formatter -> StreamSpec -> Doc
formatStream fmt (StreamSpec specs) =
  PP.spread $ map f $ F.toList specs
  where
    f s =
      case s of
        StreamFile 1 dst -> PP.string ">" <> fmtWord fmt fmt dst
        StreamFile fd dst -> PP.ppr fd <> PP.char '>' <> fmtWord fmt fmt dst
        StreamAppend 1 dst -> PP.string ">>" <> fmtWord fmt fmt dst
        StreamAppend fd dst -> PP.ppr fd <> PP.string ">>" <> fmtWord fmt fmt dst
        StreamFD src dst -> PP.ppr src <> PP.string ">&" <> PP.ppr dst
        StreamPipe _ -> mempty

formatCommand :: Formatter -> Command -> Doc
formatCommand fmt cmd =
  case cmd of
    Command cspec redirs
      | hasNoRedirections redirs -> fmtCommandSpec fmt fmt cspec
      | otherwise -> fmtCommandSpec fmt fmt cspec <+> fmtStream fmt fmt redirs
    And c1 c2 -> formatCommand fmt c1 <+> PP.string "&&" <+> formatCommand fmt c2
    Or c1 c2 -> formatCommand fmt c1 <+> PP.string "||" <+> formatCommand fmt c2
    Sequence c1 c2 -> formatCommand fmt c1 <+> PP.string ";" <+> formatCommand fmt c2
    Pipe c1 c2 -> formatCommand fmt c1 <+> PP.string "|" <+> formatCommand fmt c2
    SubShell c1 redirs
      | hasNoRedirections redirs -> PP.string "$(" <> formatCommand fmt c1 <> PP.char ')'
      | otherwise -> PP.string "$(" <> formatCommand fmt c1 <> PP.char ')' <+> fmtStream fmt fmt redirs
    Test ts -> PP.string "[" <+> fmtTest fmt fmt ts <+> PP.string "]"

hasNoRedirections :: StreamSpec -> Bool
hasNoRedirections (StreamSpec s) = Seq.null s

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


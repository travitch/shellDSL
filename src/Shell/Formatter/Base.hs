{-# LANGUAGE GADTs #-}
module Shell.Formatter.Base (
  Formatter(..),
  defaultFormatter
  ) where

import qualified Data.Foldable as F
import Data.Monoid
import Text.Printf ( printf )
import Text.PrettyPrint.Mainland as PP

import qualified Shell.Analysis as A
import Shell.Internal

data Formatter =
  Formatter { fmtCommand :: Formatter -> Command -> Doc
              -- ^ Format individual commands, primarily connectives
            , fmtCommandSpec :: Formatter -> CommandSpec -> Doc
              -- ^ Format a command with its arguments
            , fmtAction :: Formatter -> A.Analysis -> Shell -> Doc
              -- ^ Format a top-level action (e.g., run, conditionals)
            , fmtWord :: Formatter -> BWord -> Doc
              -- ^ Format a word-like thing (string with interpolation)
            , fmtStream :: Formatter -> StreamSpec -> Doc
              -- ^ Format IO redirection specifiers
            , fmtTest :: Formatter -> TestSpec -> Doc
            , fmtPidCapture :: Formatter -> Int -> Doc
              -- ^ The code required to capture the PID of the preceding async command
            , fmtExitCodeCapture :: Formatter -> Int -> Doc
              -- ^ The code required to capture the exit code of the preceding process
            , fmtPreamble :: Formatter -> Doc
              -- ^ Return a preamble document
            , fmtIndentation :: Int
            }

defaultFormatter :: Formatter
defaultFormatter = Formatter { fmtWord = formatWord
                             , fmtCommand = formatCommand
                             , fmtCommandSpec = formatCommandSpec
                             , fmtAction = formatAction
                             , fmtStream = formatStream
                             , fmtTest = formatTest
                             , fmtPidCapture = formatPidCapture
                             , fmtExitCodeCapture = formatExitCodeCapture
                             , fmtPreamble = const mempty
                             , fmtIndentation = 2
                             }

formatPidCapture :: Formatter -> Int -> Doc
formatPidCapture _ uid = PP.string "PID" <> PP.ppr uid <> PP.string "=$!"

formatExitCodeCapture :: Formatter -> Int -> Doc
formatExitCodeCapture _ uid = PP.string "EXITCODE" <> PP.ppr uid <> PP.string "=$?"

formatAction :: Formatter -> A.Analysis -> Shell -> Doc
formatAction fmt ares shell =
  case shell of
    RunSync uid cmd
      | A.requiresExitCode ares shell -> cdoc <//> fmtExitCodeCapture fmt fmt uid
      | otherwise -> cdoc
      where
        cdoc = fmtCommand fmt fmt cmd
    RunAsync uid cmd
      | A.requiresPid ares shell -> cdoc <//> fmtPidCapture fmt fmt uid
      | otherwise -> cdoc
      where
        cdoc = fmtCommand fmt fmt cmd <+> PP.string "&"
    Wait uid (Async a)
      | A.requiresExitCode ares shell -> wdoc <//> fmtExitCodeCapture fmt fmt uid
      | otherwise -> wdoc
      where
        wdoc = PP.string "wait ${PID" <> PP.ppr a <> PP.char '}'
    SetEnv _ str val -> PP.string str <> PP.char '=' <> fmtWord fmt fmt val
    UnsetEnv _ str -> PP.string "unset" <+> PP.string str
    ExportEnv _ str -> PP.string "export" <+> PP.string str
    While _ cond body ->
      let bdoc = PP.stack $ map (formatAction fmt ares) body
      in PP.string "while" <+> fmtCommand fmt fmt cond <> PP.string "; do" <//> PP.indent (fmtIndentation fmt) bdoc <//> PP.string "done"
    Until _ cond body ->
      let bdoc = PP.stack $ map (formatAction fmt ares) body
      in PP.string "until" <+> fmtCommand fmt fmt cond <> PP.string "; do" <//> PP.indent (fmtIndentation fmt) bdoc <//> PP.string "done"
    SubBlock _ Nothing body ->
      let bdoc = PP.stack $ map (formatAction fmt ares) body
      in PP.string "(" <//> PP.indent (fmtIndentation fmt) bdoc <//> PP.string ")"
    SubBlock _ (Just var) body ->
      let bdoc = PP.stack $ map (formatAction fmt ares) body
      in PP.string var <> PP.string "=$(" <//> PP.indent (fmtIndentation fmt) bdoc <//> PP.string ")"
    Comment _ str -> PP.stack (map commentLine (lines str))
    where
      commentLine l = PP.char '#' <+> PP.string l

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

-- Bash can do better in tests by using && and || inside of [[ ]]
formatCommand :: Formatter -> Command -> Doc
formatCommand fmt cmd =
  case cmd of
    Command cspec redirs
      | isNullStreamSpec redirs -> fmtCommandSpec fmt fmt cspec
      | otherwise -> fmtCommandSpec fmt fmt cspec <+> fmtStream fmt fmt redirs
    And c1 c2 -> formatCommand fmt c1 <+> PP.string "&&" <+> formatCommand fmt c2
    Or c1 c2 -> formatCommand fmt c1 <+> PP.string "||" <+> formatCommand fmt c2
    Sequence c1 c2 -> formatCommand fmt c1 <+> PP.string ";" <+> formatCommand fmt c2
    Pipe c1 c2 -> formatCommand fmt c1 <+> PP.string "|" <+> formatCommand fmt c2
    SubShell c1 redirs
      | isNullStreamSpec redirs -> PP.string "$(" <> formatCommand fmt c1 <> PP.char ')'
      | otherwise -> PP.string "$(" <> formatCommand fmt c1 <> PP.char ')' <+> fmtStream fmt fmt redirs
    Test ts -> PP.string "[" <+> fmtTest fmt fmt ts <+> PP.string "]"

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
    BExitCode (Result rid) -> PP.string "${EXITCODE" <> PP.ppr rid <> PP.char '}'
    BPID (Async aid) -> PP.string "${PID" <> PP.ppr aid <> PP.char '}'


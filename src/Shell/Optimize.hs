{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Shell.Optimize (
  optimize,
  Optimizer(..),
  defaultOptimizer,
  (>=>*),
  -- * Pre-defined optimizations
  optAndTrue,
  optOrFalse,
  optGrepWc
  ) where

import Control.Applicative
import Data.Monoid

import Shell.Diagnostic
import Shell.Internal

data Optimizer =
  Optimizer { optCommand :: Optimizer -> Command -> Diagnostics Command
              -- ^ Applied to each top-level 'Command' in a 'Shell'
              -- action.  Top-level means that the optimization driver
              -- does not recursively traverse commands itself (but
              -- the optimizer function is free to do so).
            , optAction :: Optimizer -> Shell -> Diagnostics Shell
            }

defaultOptimizer :: Optimizer
defaultOptimizer = Optimizer { optCommand = optimizeCommand
                             , optAction = optimizeAction
                             }

optimize :: Optimizer -> [Shell] -> ([Shell], [Diagnostic])
optimize o prog = runDiagnostics $ mapM (optAction o o) prog

optimizeCommand :: Optimizer -> Command -> Diagnostics Command
optimizeCommand = optAndTrue >=>* optOrFalse >=>* optGrepWc

-- | Compose optimization functions.
--
-- This is just like '(>=>)', but with support for threading the
-- 'Optimizer' argument through.
(>=>*) :: (Optimizer -> Command -> Diagnostics Command)
          -> (Optimizer -> Command -> Diagnostics Command)
          -> (Optimizer -> Command -> Diagnostics Command)
f1 >=>* f2 = \o c -> f1 o c >>= f2 o

diag :: String -> Diagnostics ()
diag = recordDiagnostic . Diagnostic Nothing "Optimizer"

-- | Turn @command || false@ into @command@
optOrFalse :: Optimizer -> Command -> Diagnostics Command
optOrFalse _ c =
  case c of
    Or lhs (Command cspec sspec)
      | isConstantCommand cspec "false" && isNullStreamSpec sspec -> do
          diag "Simplifying trivial `|| false`"
          return lhs
    _ -> return c

-- | Turn @command && true@ into @command@
optAndTrue :: Optimizer -> Command -> Diagnostics Command
optAndTrue _ c =
  case c of
    And lhs (Command cspec sspec)
      | isConstantCommand cspec "true" && isNullStreamSpec sspec -> do
          diag "Simplifying trivial `&& true`"
          return lhs
    _ -> return c

rightmostCommand :: Command -> Command
rightmostCommand cmd =
  case cmd of
    Command {} -> cmd
    And _ c -> rightmostCommand c
    Or _ c -> rightmostCommand c
    Pipe _ c -> rightmostCommand c
    Sequence _ c -> rightmostCommand c
    SubShell {} -> cmd
    Test {} -> cmd

replaceRightmostCommand :: Command -> Command -> Command
replaceRightmostCommand root new =
  case root of
    Command {} -> new
    And lhs c -> And lhs (replaceRightmostCommand c new)
    Or lhs c -> Or lhs (replaceRightmostCommand c new)
    Pipe lhs c -> Pipe lhs (replaceRightmostCommand c new)
    Sequence lhs c -> Sequence lhs (replaceRightmostCommand c new)
    SubShell {} -> new
    Test {} -> new

-- | Turn @grep foo | wc -l@ into @grep -c foo@
optGrepWc :: Optimizer -> Command -> Diagnostics Command
optGrepWc _ c =
  case c of
    -- Pipe (Command grepspec grepstreams) (Command wcspec wcstreams)
    Pipe rm@(rightmostCommand -> Command grepspec grepstreams) (Command wcspec wcstreams)
      | and [ commandName wcspec == "wc"
            , commandArguments wcspec == ["-l"]
            , commandName grepspec == "grep"
            , not ("-c" `elem` commandArguments grepspec)
            ] -> do
          let grep' = grepspec { commandArguments = "-c" : commandArguments grepspec }
          diag "Simplifying `grep [foo] | wc -l` into `grep -c foo`"
          return $ replaceRightmostCommand rm (Command grep' (grepstreams <> wcstreams))
    _ -> return c

isConstantCommand :: CommandSpec -> BWord -> Bool
isConstantCommand cspec name = commandName cspec == name && null (commandArguments cspec)

optimizeAction :: Optimizer -> Shell -> Diagnostics Shell
optimizeAction o = traverseShell opt
  where
    optc = optCommand o o
    opt s =
      case s of
        While uid c body -> While uid <$> optc c <*> pure body
        Until uid c body -> Until uid <$> optc c <*> pure body
        RunSync uid c -> RunSync uid <$> optc c
        RunAsync uid c -> RunAsync uid <$> optc c
        _ -> pure s

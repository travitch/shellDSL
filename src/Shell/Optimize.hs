{-# LANGUAGE OverloadedStrings #-}
module Shell.Optimize (
  optimize,
  Optimizer(..),
  defaultOptimizer,
  (>=>*),
  -- * Pre-defined optimizations
  optAndTrue,
  optOrFalse
  ) where

import Control.Applicative

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
optimizeCommand = optAndTrue >=>* optOrFalse

-- | Compose optimization functions
(>=>*) :: (Optimizer -> Command -> Diagnostics Command)
          -> (Optimizer -> Command -> Diagnostics Command)
          -> (Optimizer -> Command -> Diagnostics Command)
f1 >=>* f2 = \o c -> f1 o c >>= f2 o

-- | Turn @command && true@ into @command@
optAndTrue :: Optimizer -> Command -> Diagnostics Command
optAndTrue _ c =
  case c of
    And lhs (Command cspec sspec)
      | isConstantCommand cspec "true" && isNullStreamSpec sspec -> do
          diag "Simplifying trivial `&& true`"
          return lhs
    _ -> return c

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

{-# LANGUAGE OverloadedStrings #-}
module Shell.Optimize (
  optimize,
  Optimizer(..),
  defaultOptimizer
  ) where

import Control.Applicative
import qualified Data.Functor.Identity as I
import Shell.Internal

data Optimizer =
  Optimizer { optCommand :: Optimizer -> Command -> Command
              -- ^ Applied to each top-level 'Command' in a 'Shell'
              -- action.  Top-level means that the optimization driver
              -- does not recursively traverse commands itself (but
              -- the optimizer function is free to do so).
            , optAction :: Optimizer -> Shell -> Shell
            }

defaultOptimizer :: Optimizer
defaultOptimizer = Optimizer { optCommand = optimizeCommand
                             , optAction = optimizeAction
                             }

optimize :: Optimizer -> [Shell] -> [Shell]
optimize o = map (optAction o o)

optimizeCommand :: Optimizer -> Command -> Command
optimizeCommand _ c =
  case c of
    And lhs (Command cspec sspec)
      | isConstantCommand cspec "true" && isNullStreamSpec sspec -> lhs
    Or lhs (Command cspec sspec)
      | isConstantCommand cspec "false" && isNullStreamSpec sspec -> lhs
    _ -> c

isConstantCommand :: CommandSpec -> BWord -> Bool
isConstantCommand cspec name = commandName cspec == name && null (commandArguments cspec)

optimizeAction :: Optimizer -> Shell -> Shell
optimizeAction o = I.runIdentity . traverseShell opt
  where
    optc = optCommand o o
    opt s =
      case s of
        While uid c body -> pure $ While uid (optc c) body
        Until uid c body -> pure $ Until uid (optc c) body
        RunSync uid c -> pure $ RunSync uid (optc c)
        RunAsync uid c -> pure $ RunAsync uid (optc c)
        _ -> pure s

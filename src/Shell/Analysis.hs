-- | Static analysis of shell scripts
--
-- The analysis is aimed at providing some static checking of common
-- errors and providing information to support code generation
module Shell.Analysis (
  Analysis(..),
  analyze
  ) where

import Control.Monad ( unless )
import qualified Control.Monad.State.Strict as MS
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import Data.Set ( Set )
import qualified Data.Set as S
import Text.Printf ( printf )

import Shell.Diagnostic
import Shell.Internal

data Analysis =
  Analysis { aRequiredPID :: !IntSet
           , aRequiredExitCode :: !IntSet
           , aMayDefinedEnvVars :: !(Set String)
           , aMustDefinedEnvVars :: !(Set String)
           , aErrors :: !(Seq Diagnostic)
           }

type M = MS.State Analysis

emptyState :: Analysis
emptyState = Analysis { aRequiredPID = IS.empty
                      , aRequiredExitCode = IS.empty
                      , aMayDefinedEnvVars = S.empty
                      , aMustDefinedEnvVars = S.empty
                      , aErrors = Seq.empty
                      }

analyze :: [Shell] -> Analysis
analyze prog = MS.execState action emptyState
  where
    action = do
      mapM_ analyzeAction prog

analyzeAction :: Shell -> M ()
analyzeAction sh =
  case sh of
    RunSync _ cmd -> analyzeCommand cmd
    RunAsync _ cmd -> analyzeCommand cmd
    -- Syntactically, wait can only refer to valid values.
    Wait {} -> return ()
    UnsetEnv _ vname ->
      MS.modify' $ \s -> s { aMayDefinedEnvVars = S.delete vname (aMayDefinedEnvVars s)
                           , aMustDefinedEnvVars = S.delete vname (aMustDefinedEnvVars s)
                           }
    SetEnv _ vname _ ->
      MS.modify' $ \s -> s { aMayDefinedEnvVars = S.insert vname (aMayDefinedEnvVars s)
                           , aMustDefinedEnvVars = S.insert vname (aMustDefinedEnvVars s)
                           }
    ExportEnv _ vname -> do
      mustDef <- MS.gets aMustDefinedEnvVars
      unless (S.member vname mustDef) $ do
        recordError $ printf "Variable `%s` may not be defined at export" vname
    While _ cmd body -> do
      s0 <- MS.get
      analyzeCommand cmd
      mapM_ analyzeAction body
      controlFlowMerge s0
    Until _ cmd body -> do
      s0 <- MS.get
      analyzeCommand cmd
      mapM_ analyzeAction body
      controlFlowMerge s0
    SubBlock _ _ body -> do
      -- Here we have an interesting case - environment effects in the
      -- body do not propagate to the caller.
      --
      -- We should probably also clear out the local environment in
      -- the body since it cannot reference non-exported vars (can it?)
      s0 <- MS.get
      mapM_ analyzeAction body
      MS.modify' $ \now -> now { aMustDefinedEnvVars = aMustDefinedEnvVars s0
                               , aMayDefinedEnvVars = aMayDefinedEnvVars s0
                               }

-- | Merge flow sensitive facts after a control flow merge point.
--
-- Right now, this is just the may/must def records
controlFlowMerge :: Analysis -> M ()
controlFlowMerge s0 =
  MS.modify' $ \now -> now { aMustDefinedEnvVars = S.intersection (aMustDefinedEnvVars now) (aMustDefinedEnvVars s0)
                           , aMayDefinedEnvVars = S.union (aMayDefinedEnvVars now) (aMayDefinedEnvVars s0)
                           }

analyzeCommand :: Command -> M ()
analyzeCommand cmd =
  case cmd of
    And c1 c2 -> analyzeCommand c1 >> analyzeCommand c2
    Or c1 c2 -> analyzeCommand c1 >> analyzeCommand c2
    Pipe c1 c2 -> analyzeCommand c1 >> analyzeCommand c2
    Sequence c1 c2 -> analyzeCommand c1 >> analyzeCommand c2
    SubShell c spec -> analyzeCommand c >> analyzeStreamSpec spec
    Test t -> analyzeTestSpec t
    Command cspec sspec -> analyzeCommandSpec cspec >> analyzeStreamSpec sspec

-- | If a referenced environment variable is not defined, report an error
analyzeBWord :: BWord -> M ()
analyzeBWord = undefined

analyzeCommandSpec :: CommandSpec -> M ()
analyzeCommandSpec cspec = analyzeBWord (commandName cspec) >> mapM_ analyzeBWord (commandArguments cspec)

analyzeTestSpec :: TestSpec -> M ()
analyzeTestSpec = undefined

analyzeStreamSpec :: StreamSpec -> M ()
analyzeStreamSpec = undefined

recordError :: String -> M ()
recordError msg =
  MS.modify' $ \s -> s { aErrors = aErrors s Seq.|> d }
  where
    d = Diagnostic Nothing "Analysis" msg

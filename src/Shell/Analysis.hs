-- | Static analysis of shell scripts
--
-- The analysis is aimed at providing some static checking of common
-- errors and providing information to support code generation
module Shell.Analysis (
  Analysis(..),
  analyze,
  requiresPid,
  requiresExitCode,
  errors
  ) where

import Control.Monad ( unless )
import qualified Control.Monad.State.Strict as MS
import qualified Data.Foldable as F
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

errors :: Analysis -> [Diagnostic]
errors = F.toList . aErrors

requiresPid :: Analysis -> Shell -> Bool
requiresPid a sh =
  case sh of
    RunAsync uid _ -> uid `IS.member` aRequiredPID a
    _ -> False

requiresExitCode :: Analysis -> Shell -> Bool
requiresExitCode a sh =
  case sh of
    RunSync uid _ -> uid `IS.member` aRequiredExitCode a
    Wait uid _ -> uid `IS.member` aRequiredExitCode a
    _ -> False

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
    Comment {} -> return ()
    RunSync _ cmd -> analyzeCommand cmd
    RunAsync _ cmd -> analyzeCommand cmd
    Wait _ (Async aid) ->
      -- Syntactically, wait can only refer to valid values, so we
      -- just record the PID reference.
      MS.modify $ \s -> s { aRequiredPID = IS.insert aid (aRequiredPID s) }
    UnsetEnv _ vname ->
      MS.modify $ \s -> s { aMayDefinedEnvVars = S.delete vname (aMayDefinedEnvVars s)
                           , aMustDefinedEnvVars = S.delete vname (aMustDefinedEnvVars s)
                           }
    SetEnv _ vname _ ->
      MS.modify $ \s -> s { aMayDefinedEnvVars = S.insert vname (aMayDefinedEnvVars s)
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
    SubBlock _ mcap body -> do
      -- Here we have an interesting case - environment effects in the
      -- body do not propagate to the caller.
      --
      -- We should probably also clear out the local environment in
      -- the body since it cannot reference non-exported vars (can it?)
      s0 <- MS.get
      mapM_ analyzeAction body
      MS.modify $ \now -> now { aMustDefinedEnvVars = aMustDefinedEnvVars s0
                               , aMayDefinedEnvVars = aMayDefinedEnvVars s0
                               }
      F.forM_ mcap $ \capvar -> do
        MS.modify $ \now -> now { aMustDefinedEnvVars = S.insert capvar (aMustDefinedEnvVars now)
                                 , aMayDefinedEnvVars = S.insert capvar (aMayDefinedEnvVars now)
                                 }

-- | Merge flow sensitive facts after a control flow merge point.
--
-- Right now, this is just the may/must def records
controlFlowMerge :: Analysis -> M ()
controlFlowMerge s0 =
  MS.modify $ \now -> now { aMustDefinedEnvVars = S.intersection (aMustDefinedEnvVars now) (aMustDefinedEnvVars s0)
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
analyzeBWord = mapM_ analyzeBSpan . unWord

analyzeBSpan :: BSpan -> M ()
analyzeBSpan bs =
  case bs of
    BVariable vname -> do
      s <- MS.get
      -- This diagnostic will be much better once it has location
      -- information attached
      unless (S.member vname (aMayDefinedEnvVars s)) $ do
        recordError $ printf "Variable `%s` is not definitely assigned" vname
    BExitCode (Result rid) ->
      MS.modify $ \s -> s { aRequiredExitCode = IS.insert rid (aRequiredExitCode s)
                           }
    BPID (Async aid) ->
      MS.modify $ \s -> s { aRequiredPID = IS.insert aid (aRequiredPID s) }
    _ -> return ()

analyzeCommandSpec :: CommandSpec -> M ()
analyzeCommandSpec cspec = analyzeBWord (commandName cspec) >> mapM_ analyzeBWord (commandArguments cspec)

analyzeTestSpec :: TestSpec -> M ()
analyzeTestSpec ts =
  case ts of
    TSFileExists bw -> analyzeBWord bw
    TSFileIsRegular bw -> analyzeBWord bw
    TSFileIsDirectory bw -> analyzeBWord bw
    TSFileIsSymlink bw -> analyzeBWord bw
    TSPipeExists bw -> analyzeBWord bw
    TSFileIsReadableY bw -> analyzeBWord bw
    TSFileNotEmpty bw -> analyzeBWord bw
    TSFDOpenOnTerminal bw -> analyzeBWord bw
    TSFileWritableY bw -> analyzeBWord bw
    TSFileExecutableY bw -> analyzeBWord bw
    TSFileEffectivelyOwnedY bw -> analyzeBWord bw
    TSFileEffectivelyOwnedG bw -> analyzeBWord bw
    TSFileNewer bw1 bw2 -> analyzeBWord bw1 >> analyzeBWord bw2
    TSFileOlder bw1 bw2 -> analyzeBWord bw1 >> analyzeBWord bw2
    TSStringEmpty bw -> analyzeBWord bw
    TSStringNotEmpty bw -> analyzeBWord bw
    TSStringIdentical bw1 bw2 -> analyzeBWord bw1 >> analyzeBWord bw2
    TSStringNotIdentical bw1 bw2 -> analyzeBWord bw1 >> analyzeBWord bw2
    TSStringLT bw1 bw2 -> analyzeBWord bw1 >> analyzeBWord bw2
    TSStringGT bw1 bw2 -> analyzeBWord bw1 >> analyzeBWord bw2
    TSNegate ts' -> analyzeTestSpec ts'
    TSIntEq bw1 bw2 -> analyzeBWord bw1 >> analyzeBWord bw2
    TSIntNeq bw1 bw2 -> analyzeBWord bw1 >> analyzeBWord bw2
    TSIntLT bw1 bw2 -> analyzeBWord bw1 >> analyzeBWord bw2
    TSIntGT bw1 bw2 -> analyzeBWord bw1 >> analyzeBWord bw2
    TSIntLE bw1 bw2 -> analyzeBWord bw1 >> analyzeBWord bw2
    TSIntGE bw1 bw2 -> analyzeBWord bw1 >> analyzeBWord bw2

analyzeStreamSpec :: StreamSpec -> M ()
analyzeStreamSpec (StreamSpec specs) = F.mapM_ analyzeStream specs
  where
    analyzeStream s =
      case s of
        StreamFile _ bw -> analyzeBWord bw
        StreamAppend _ bw -> analyzeBWord bw
        StreamFD _ _ -> return ()

recordError :: String -> M ()
recordError msg =
  MS.modify $ \s -> s { aErrors = aErrors s Seq.|> d }
  where
    d = Diagnostic Nothing "Analysis" msg

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Shell.Diagnostic (
  Diagnostics,
  Diagnostic(..),
  recordDiagnostic,
  runDiagnostics
  ) where

import Control.Applicative
import qualified Control.Monad.State.Strict as MS
import qualified Data.Foldable as F
import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq

data DiagState = DiagState { diags :: !(Seq Diagnostic) }

data Location = SrcLoc
              deriving (Eq, Ord, Show)

data Diagnostic = Diagnostic { dLocation :: Maybe Location
                             , dComponent :: String
                             , dMessage :: String
                             }
                deriving (Eq, Ord, Show)

newtype Diagnostics a = Diagnostics (MS.State DiagState a)
                      deriving (Functor, Applicative, Monad, MS.MonadState DiagState)

recordDiagnostic :: Diagnostic -> Diagnostics ()
recordDiagnostic d = MS.modify' $ \s -> s { diags = diags s Seq.|> d }

runDiagnostics :: Diagnostics a -> (a, [Diagnostic])
runDiagnostics (Diagnostics action) =
  let (res, DiagState s) = MS.runState action (DiagState Seq.empty)
  in (res, F.toList s)

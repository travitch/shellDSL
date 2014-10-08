module Shell (
  -- * Running things
  run,
  background,
  wait,
  setEnv,
  unsetEnv,
  exportEnv,
  -- * Control flow
  whileM,
  untilM,
  subshellM,
  subshellCaptureM,
  -- * Referencing things
  envRef,
  unsafeEnvRef,
  capture,
  -- * Commands
  Command,
  command,
  (*|*),
  (*||*),
  (*&&*),
  (#),
  (|>),
  (|>>),
  (<|),
  (@>),
  subshell,
  -- * Tests
  testFileExists
  ) where

import Prelude hiding ( until )

import Shell.Internal

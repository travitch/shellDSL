module Shell (
  -- * Running things
  run,
  background,
  wait,
  setEnv,
  unsetEnv,
  exportEnv,
  comment,
  -- * Control flow
  whileM,
  untilM,
  subshellM,
  subshellCaptureM,
  -- * Referencing things
  envRef,
  unsafeEnvRef,
  capture,
  pidOf,
  exitCode,
  anyChars,
  anyChar,
  charSet,
  negCharSet,
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
  testFileExists,
  -- * Diagnostics
  Diagnostic,
  -- Other types
  ShellM
  ) where

import Prelude hiding ( until )

import Shell.Diagnostic
import Shell.Internal

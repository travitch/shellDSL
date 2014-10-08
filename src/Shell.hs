module Shell (
  -- * Running things
  run,
  background,
  wait,
  setEnv,
  unsetEnv,
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
  subshell
  ) where

import Shell.Internal

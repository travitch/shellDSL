module Shell (
  -- * Running things
  run,
  background,
  wait,
  setEnv,
  unsetEnv,
  -- * Control flow
  while,
  until,
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

import Prelude hiding ( until )

import Shell.Internal

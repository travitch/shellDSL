module Shell (
  Shell,
  command,
  run,
  env,
  background,
  wait,
  envRef,
  unsafeEnvRef,
  capture,
  subshell,
  (|||),
  (#),
  (|>),
  (|>>),
  (@>)
  ) where

import Shell.Internal

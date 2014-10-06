module Shell (
  Shell,
  command,
  run,
  background,
  wait,
  env,
  unsafeEnv,
  capture,
  (|||),
  (#),
  (|>),
  (|>>),
  (@>)
  ) where

import Shell.Internal

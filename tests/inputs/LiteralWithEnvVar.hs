{-# LANGUAGE OverloadedStrings #-}
module LiteralWithEnvVar ( script ) where
import Prelude
import Shell

-- This test ensures that we escape string literals properly if they
-- have an embedded variable expansion.

script :: ShellM ()
script = do
  run $ command "echo" ["${SHELL}"]
  return ()

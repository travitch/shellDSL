{-# LANGUAGE OverloadedStrings #-}
module GlobInString ( script ) where
import Prelude
import Shell

-- This test ensures that we escape string literals properly if they
-- have an embedded glob.

script :: ShellM ()
script = do
  run $ command "echo" ["*"]
  return ()

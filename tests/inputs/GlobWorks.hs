{-# LANGUAGE OverloadedStrings #-}
module GlobWorks ( script ) where
import Prelude
import Data.Monoid
import Shell

-- This test ensures that globs actually work
-- have an embedded glob.

script :: ShellM ()
script = do
  run $ command "echo" ["README" <> anyChars]
  return ()

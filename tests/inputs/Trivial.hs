{-# LANGUAGE OverloadedStrings #-}
module Trivial ( script ) where
import Prelude
import Shell

script :: ShellM ()
script = do
  run $ command "echo" ["1"]
  return ()

{-# LANGUAGE OverloadedStrings #-}
module EscapeAmp ( script ) where
import Prelude
import Shell

-- We have to escape (or quote) a string literal with an embedded &

script :: ShellM ()
script = do
  run $ command "echo" ["foo&bar"]
  return ()

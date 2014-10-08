{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main ( main ) where

import Data.Monoid

import Shell
import Shell.Bash
import Shell.Internal

main :: IO ()
main = do
  script <- runBash $ do
    run $ command "ls" ["-l", "-h"] |> "/tmp/lsout"
    run $ command "ls" [] |>> "/tmp/lsout"
    run $ command "wc" ["-l", "/etc/fstab"] @> (2, 1) |> "/tmp/fscount"
    h1 <- background $ command "md5sum" ["/dev/mem"]
    whileM Condition $ do
      run $ command "echo" ["loop"]
      return ()
    wait h1
    subshellCaptureM "BAR" $ do
      run $ command "cat" ["/etc/fstab"]
      return ()
    return ()
    exportEnv "BAR"
    setEnv "FOO" "5"
    unsetEnv "BAZ"
    run $ command "echo" ["BAR=" <> envRef "BAR"] *|* command "wc -c" []
    return ()
  putStrLn script

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main ( main ) where

import Data.Monoid
import qualified System.IO as IO

import Shell
import Shell.Formatter.Bash

main :: IO ()
main = do
  (mscript, diags) <- runBash $ do
    run $ command "ls" ["-l", "-h"] |> "/tmp/lsout" *||* command "false" []
    lsec <- run $ command "ls" [] |>> "/tmp/lsout"
    run $ command "wc" ["-l", "/etc/fstab"] @> (2, 1) |> "/tmp/fscount"
    h1 <- background $ command "md5sum" ["/dev/mem"]
    comment "This is an infinite loop\nNot the best idea"
    whileM (testFileExists "/etc/mtab" *&&* testFileExists "/tmp") $ do
      run $ command "echo" ["loop"]
      return ()
    wait h1
    run $ command "echo" [exitCode lsec]
    subshellCaptureM "BAR" $ do
      run $ command "cat" ["/etc/fstab"]
      return ()
    return ()
    exportEnv "BAR"
    setEnv "FOO" "5"
    unsetEnv "BAZ"
    let shared = command "echo" ["BAR=" <> envRef "BAR"] *|* command "wc" ["-c"]
    run shared
    run $ subshell shared |> "/dev/null"
    run $ command "grep" ["ext4", "/etc/fstab"] *|* command "wc" ["-l"]
    run $ command "dmesg" [] *|* command "grep" ["ext4"] *|* command "wc" ["-l"]
    run $ command "echo" ["literal with spaces in it!  Better escape"]
    return ()
  mapM_ (IO.hPrint IO.stderr) diags
  case mscript of
    Nothing -> putStrLn "Error"
    Just script -> putStrLn script

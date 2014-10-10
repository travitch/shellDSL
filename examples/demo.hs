{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main ( main ) where

import Data.Monoid
import qualified System.IO as IO
import qualified System.IO.Temp as T
import qualified System.Process as P

import Shell
import Shell.Formatter.Bash
import Shell.Formatter.POSIX

demoScript :: ShellM ()
demoScript = do
  comment "A script demonstrating some features\nof this DSL"
  eres <- run $ command "echo" ["*Starting*"]
  run $ command "ls" ["/etc/" <> anyChars <> ".conf"] *|*
        command "grep" ["s"] *|*
        command "wc" ["-l"]
  run $ command "cat" ["/etc/mtab"] *|* command "grep" ["ext4"]
  run $ command "echo" ["\"echo\" exit code: ", exitCode eres]
  return ()

toString :: (ShellM () -> IO (Maybe String, [Diagnostic])) -> ShellM () -> IO String
toString runner s = do
  (mscript, diags) <- runner s
  mapM_ (IO.hPrint IO.stderr) diags
  case mscript of
    Nothing -> IO.hPutStrLn IO.stderr "Error compiling script" >> error "Error"
    Just script -> return script

main :: IO ()
main = do
  bashScript <- toString runBash demoScript
  shScript <- toString runSh demoScript
  T.withSystemTempFile "script" $ \bscript bh -> do
    IO.hPutStr bh bashScript
    T.withSystemTempFile "script" $ \sscript sh -> do
      IO.hPutStr sh shScript
      IO.hFlush bh
      IO.hFlush sh
      P.readProcess "pr" ["-W100", "-S      ", "-t", "-m", sscript, bscript] "" >>= IO.hPutStrLn IO.stdout

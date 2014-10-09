# Introduction

This package defines a DSL for generating shell scripts.

The goal is to make it easier to write safe and correct shell scripts.
Quoting will be automatically managed.  There will also be some
support for trying to automatically get numeric vs. string comparisons
correct.  It will not support all shell features.  Instead, it will
support many features and enable scripts written in the DSL to be
compiled down to different formats including:

 * bash
 * POSIX shell
 * maybe Python or perl
 * maybe powershell

In addition to simply turning the DSL into a shell script, the
compiler also performs some static checks:

 * Variables are defined before they are used

The compiler also supports configurable peephole optimization.  For
example, unnecessary uses of `cat` can be rewritten into file
redirections.

# Example Use

A script written in the eDSL looks like this:

```{.haskell}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
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
    return ()
  mapM_ (IO.hPrint IO.stderr) diags
  case mscript of
    Nothing -> putStrLn "Error"
    Just script -> putStrLn script
```

When compiled to bash, the resulting script is:

```{.bash}
#!/bin/bash
set -e
set -u


ls -l -h >/tmp/lsout
ls >>/tmp/lsout
EXITCODE1=$?
wc -l /etc/fstab 2>&1 >/tmp/fscount
md5sum /dev/mem &
PID3=$!
while [ -e /etc/mtab ] && [ -e /tmp ]; do
  echo loop
done
wait ${PID3}
echo ${EXITCODE1}
BAR=$(
  cat /etc/fstab
)
export BAR
FOO=5
unset BAZ
echo BAR=${BAR} | wc -c
$(echo BAR=${BAR} | wc -c) >/dev/null
grep -c ext4 /etc/fstab
dmesg | grep -c ext4
```

Additionally, the compiler produces three diagnostics:

```
Diagnostic {dLocation = Nothing, dComponent = "Optimizer", dMessage = "Simplifying trivial `|| false`"}
Diagnostic {dLocation = Nothing, dComponent = "Optimizer", dMessage = "Simplifying `grep [foo] | wc -l` into `grep -c foo`"}
Diagnostic {dLocation = Nothing, dComponent = "Optimizer", dMessage = "Simplifying `grep [foo] | wc -l` into `grep -c foo`"}
```

# Implementation

Right now, the DSL is embedded in Haskell.  This interface is not
ideal, but may stick around.  In the long term, it will be spun out
into a standalone compiler with no run time dependencies.

The optimizer and output backends are configurable and are designed to
be easily retargetable to new interpreters.

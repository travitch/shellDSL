{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main ( main ) where

-- import qualified Control.Concurrent.Supply as S
-- import qualified Control.Monad.Free as FR
-- import qualified Control.Monad.State.Strict as ST

import Shell
import Shell.Bash

main :: IO ()
main = do
  script <- runBash $ do
    run $ command "ls" ["-l", "-h"] |> "/tmp/lsout"
    run $ command "ls" [] |>> "/tmp/lsout"
    run $ command "wc" ["-l", "/etc/fstab"] @> (2, 1) |> "/tmp/fscount"
    h1 <- background $ command "md5sum" ["/dev/mem"]
    wait h1
    return ()
  putStrLn script

{-

data Shell next = RunCommand String [String] next
                | While Int (FR.Free Shell ()) next
                | GetExitCode Result next
                | Done
                deriving (Eq, Show, Functor)

-- data Result = Result Int

-- type ShellM = O.Program Shell

-- data SomeShell = forall a . Shell a

data ShellState = ShellState { sIdSrc :: !S.Supply }

type ShellM = ST.StateT ShellState (FR.Free Shell)

data Result = Result Int
            deriving (Eq, Ord, Show)

takeId :: ShellM Int
takeId = do
  s <- ST.gets sIdSrc
  let (nid, s') = S.freshId s
  ST.put $ ShellState s'
  return nid

runCommand :: String -> [String] -> ShellM Result
runCommand cmd args = do
  rid <- takeId
  let res = Result rid
  FR.liftF (RunCommand cmd args res)
  return res

while :: Int -> ShellM () -> ShellM ()
while i body = do
  body' <- evalBody body
  FR.liftF (While i body' ())

getExitCode :: Result -> ShellM ()
getExitCode res = FR.liftF (GetExitCode res ())

evalBody :: ShellM a -> ShellM (FR.Free Shell a)
evalBody body = do
  s <- ST.gets sIdSrc
  let (local, sub) = S.splitSupply s
      body' = ST.evalStateT body (ShellState sub)
  ST.put (ShellState local)
  return body'


test :: ShellM ()
test = do
  lsres <- runCommand "ls" ["-l", "-h"]
  getExitCode lsres
  runCommand "cat" []
  while 5 $ do
    eres <- runCommand "echo" ["6"]
    getExitCode eres
    return ()
  return ()

run :: ShellM () -> IO ()
run m = do
  s0 <- S.newSupply
  let shell = ST.evalStateT test (ShellState s0)
  print shell

-- runShell :: ShellM () -> FR.FreeT (Shell Identity) Identity
-- runShell = undefined

showShell :: ShellM () -> IO ()
showShell m = undefined

main :: IO ()
main = undefined
-}

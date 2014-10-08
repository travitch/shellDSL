{-# LANGUAGE DeriveFunctor #-}
module Shell.Internal (
  -- * Running things
  run,
  background,
  wait,
  setEnv,
  unsetEnv,
  -- * Strings and names
  envRef,
  unsafeEnvRef,
  capture,
  -- Commands
  Command(..),
  command,
  (*|*),
  (*||*),
  (*&&*),
  (#),
  (|>),
  (|>>),
  (<|),
  (@>),
  subshell,
  -- * Internal
  BWord(..),
  BSpan(..),
  StreamSpec(..),
  CommandSpec(..),
  Result(..),
  Async(..),
  ShellState(..),
  ShellF(..),
  Shell,
  ShellM
  ) where

import Control.Applicative
import qualified Control.Concurrent.Supply as U
import qualified Control.Monad.Free as FR
import qualified Control.Monad.State.Strict as MS
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid
import Data.String

data ShellState =
  ShellState { sIdSrc :: !U.Supply
             }

type ShellM = MS.StateT ShellState (FR.Free ShellF)
type Shell = FR.Free ShellF ()

takeId :: ShellM Int
takeId = do
  s <- MS.gets sIdSrc
  let (nid, s') = U.freshId s
  MS.put $ ShellState s'
  return nid

evalBody :: ShellM a -> ShellM (FR.Free ShellF a)
evalBody body = do
  s <- MS.gets sIdSrc
  let (local, sub) = U.splitSupply s
      body' = MS.evalStateT body (ShellState sub)
  MS.put (ShellState local)
  return body'

-- Ways to reference input streams:
--
-- * Named file
--
-- * Extracted from a 'Result'
data Stream = StreamOut
            | StreamErr
            | StreamFile FilePath
            | StreamAppend FilePath
            | StreamFD Int
            | StreamPipe
            deriving (Eq, Ord, Show)

-- | A specification of stream mappings.
--
-- Stdin is 0, stdout is 1, and stderr is 2.  Other streams are allowable.
data StreamSpec =
  StreamSpec { ssSpecs :: Map Int Stream }
  deriving (Eq, Ord, Show)

instance Monoid StreamSpec where
  mempty = StreamSpec { ssSpecs = M.empty }
  mappend s1 s2 = StreamSpec { ssSpecs = ssSpecs s2 `M.union` ssSpecs s1 }

-- | Bash Words
--
-- A Bash Word is a single command name or argument.  These are mostly
-- strings mixed with variable expansions.  Strings are allowed (and
-- an appropriate IFS will be chosen).
newtype BWord = BWord { unWord :: [BSpan] }
              deriving (Eq, Ord, Show)

data BSpan = BString String
             -- ^ A plain string
           | BVariable String
             -- ^ A checked variable reference
           | BUnsafeVariable String
             -- ^ An unchecked variable reference
           | BCommand Command
             -- ^ Capture the output of a subshell
           deriving (Eq, Ord, Show)

instance IsString BWord where
  fromString s = BWord [BString s]

instance Monoid BWord where
  mempty = BWord []
  mappend (BWord b1) (BWord b2) = BWord (b1 <> b2)

-- | Create a reference to an environment variable.  The script
-- generator will check to ensure that the variable MUST be defined.
-- If the variable might not be defined, script compilation will fail.
envRef :: String -> BWord
envRef s = BWord [BVariable s]

-- | Like 'env', but there is no static check to ensure the variable
-- is defined.  Use this if you are really sure, or if the environment
-- variable will be defined by a @source@.
--
-- Preferably, you could add conditional assignments (if not defined
-- ...) after your @source@ or similar call to ensure that a value
-- will be set.
unsafeEnvRef :: String -> BWord
unsafeEnvRef s = BWord [BUnsafeVariable s]

-- | Run the given command in a subshell, capturing its output as a string.
--
-- When the command is run, you cannot retrieve its exit status.  If
-- you require that, explicitly run the command with 'subshell' and
-- manually capture its output.  It is not necessary to use 'subshell'
-- and 'capture' together.
capture :: Command -> BWord
capture c = BWord [BCommand c]

data CommandSpec =
  CommandSpec { commandName :: BWord
              , commandArguments :: [BWord]
              }
  deriving (Eq, Ord, Show)

data ExitCode = ExitCode
data Env = Env

-- | Command specifications.
--
-- Any modifications to a command affect the rightmost stream
-- specifier.  Note that subshells have their own stream specifier,
-- independent of the command they execute.
--
-- FIXME: Need to unparse precedence in and/or
data Command = Command CommandSpec StreamSpec
             | And Command Command
             | Or Command Command
             | Pipe Command Command
             | Sequence Command Command
             | SubShell Command StreamSpec
             deriving (Eq, Ord, Show)

type UID = Int

data ShellF next = RunSync UID Command next
                 | RunAsync UID Command next
                 | While UID Condition Shell next
                 | Until UID Condition Shell next
                 | If UID [(Condition, Shell)] (Maybe Shell) next
                 | SubBlock UID Shell next
                   -- ^ For block-level subshells
                 | Wait UID Async next
                 | GetExitCode UID Result next
                 | UnsetEnv UID String next
                 | SetEnv UID String BWord next
                 | Done
                 deriving (Eq, Show, Functor)

modifyStreamSpec :: Command -> (StreamSpec -> StreamSpec) -> Command
modifyStreamSpec c f =
  case c of
    SubShell cmds spec -> SubShell cmds (f spec)
    Command cmd spec -> Command cmd (f spec)
    And lhs rhs -> And lhs (modifyStreamSpec rhs f)
    Or lhs rhs -> Or lhs (modifyStreamSpec rhs f)
    Pipe lhs rhs -> Pipe lhs (modifyStreamSpec rhs f)
    Sequence lhs rhs -> Sequence lhs (modifyStreamSpec rhs f)

data Exp = Exp
data Condition = Condition
               deriving (Eq, Ord, Show)

-- | The result of a synchronous command or a wait.  From these, we
-- can extract an exit code and possibly inspect stdout or stderr.
data Result = Result Int
            deriving (Eq, Ord, Show)

-- | The result of an asynchronous command.  We can wait on this, get
-- its PID, or reference stdout or stderr
data Async = Async Int
           deriving (Eq, Ord, Show)

background :: Command -> ShellM Async
background c = do
  uid <- takeId
  let res = Async uid
  FR.liftF (RunAsync uid c res)
  return res

run :: Command -> ShellM Result
run c = do
  uid <- takeId
  let res = Result uid
  FR.liftF (RunSync uid c res)
  return res

while :: Condition -> ShellM () -> ShellM ()
while c body = do
  uid <- takeId
  body' <- evalBody body
  FR.liftF (While uid c body' ())

setEnv :: String -> BWord -> ShellM ()
setEnv name val = do
  uid <- takeId
  FR.liftF (SetEnv uid name val ())

unsetEnv :: String -> ShellM ()
unsetEnv name = do
  uid <- takeId
  FR.liftF (UnsetEnv uid name ())

-- | Wait on an asynchronous/backgrounded task.
--
-- The call to wait is synchronous and returns an exit code
--
-- FIXME: Result is not the right return value here since we can't use
-- this to access stdout/stderr.  We can only get an exit code.
wait :: Async -> ShellM Result
wait a = do
  uid <- takeId
  let res = Result uid
  FR.liftF (Wait uid a res)
  return res

command :: BWord -> [BWord] -> Command
command cmd args = Command cspec mempty
  where
    cspec = CommandSpec { commandName = cmd
                        , commandArguments = args
                        }

-- | Define a pipeline.
--
-- The LHS command is implicitly made asynchronous.  The RHS command
-- "owns" the command resulting from the ||| operator is referenced by
-- the RHS command.
(*|*) :: Command -> Command -> Command
src *|* sink =
  Pipe (modifyStreamSpec src (pipeStream 1)) (modifyStreamSpec sink (pipeStream 0))
  where
    pipeStream i s = s { ssSpecs = M.insert i StreamPipe (ssSpecs s) }

(*||*) :: Command -> Command -> Command
(*||*) = Or

(*&&*) :: Command -> Command -> Command
(*&&*) = And

-- | Sequence commands
(#) :: Command -> Command -> Command
(#) = Sequence

-- | Redirect stdout, overwriting the target file
(|>) :: Command -> FilePath -> Command
c |> fp =
  modifyStreamSpec c $ \s -> s { ssSpecs = M.insert 1 (StreamFile fp) (ssSpecs s) }

-- | Redirect stout, appending to the target file
(|>>) :: Command -> FilePath -> Command
c |>> fp =
  modifyStreamSpec c $ \s -> s { ssSpecs = M.insert 1 (StreamAppend fp) (ssSpecs s) }

(<|) :: Command -> FilePath -> Command
c <| fp =
  modifyStreamSpec c $ \s -> s { ssSpecs = M.insert 0 (StreamFile fp) (ssSpecs s) }

-- | Redirect the first fd to the second fd
(@>) :: Command -> (Int, Int) -> Command
c @> (src, dst) =
  modifyStreamSpec c $ \s -> s { ssSpecs = M.insert src (StreamFD dst) (ssSpecs s) }

-- | Wrap the given command in a subshell
subshell :: Command -> Command
subshell c = SubShell c mempty

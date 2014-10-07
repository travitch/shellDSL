{-# LANGUAGE DeriveFunctor #-}
module Shell.Internal (
  command,
  run,
  env,
  background,
  wait,
  envRef,
  unsafeEnvRef,
  capture,
  (|||),
  (#),
  (|>),
  (|>>),
  (<|),
  (@>),
  subshell,
  -- * Internal
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

-- newtype ShellM a = ShellM (MS.State ShellState a)
--                  deriving (Applicative, Functor, Monad, MS.MonadState ShellState)

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
            | StreamProc Async
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
data Command = Command CommandSpec StreamSpec
             | And Command Command
             | Or Command Command
             | Pipe Command Command
             | Sequence Command Command
             | SubShell Command StreamSpec
             deriving (Eq, Ord, Show)

data ShellF next = RunSync Command next
                 | RunAsync Command next
                 | While Condition Shell next
                 | Until Condition Shell next
                 | If [(Condition, Shell)] (Maybe Shell) next
                 | SubBlock Shell next
                   -- ^ For block-level subshells
                 | Wait Async next
                 | GetExitCode Result next
                 | UnsetEnv String next
                 | SetEnv String BWord next
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
  res <- Async <$> takeId
  FR.liftF (RunAsync c res)
  return res

run :: Command -> ShellM Result
run c = do
  res <- Result <$> takeId
  FR.liftF (RunSync c res)
  return res

while :: Condition -> ShellM () -> ShellM ()
while c body = do
  body' <- evalBody body
  FR.liftF (While c body' ())


-- | Modify the environment
--
-- You can modify the environment from 'run', but using 'env' tracks
-- the current environment variables and can check to ensure
-- environment variable references are defined statically.
--
-- Bash can report undefined variable uses (and we generate bash to
-- enable those warnings), but static information can be nice.
-- env :: Shell Env -> ShellM ()
-- env e = do
--   _ <- addCommand e
--   return ()
env=undefined

-- FIXME: To make this work, we need two different notions.  The
-- sequence of allocated uniques and the recorded AST.  The recorded
-- AST has the actual structure (so a while statement "contains" its
-- constituent elements instead of having them flattened into a
-- sequence).  To do this, processing the body of a nested statement
-- needs to create a new context to record the children.

-- | Wait on an asynchronous/backgrounded task.
--
-- The call to wait is synchronous and returns an exit code
--
-- FIXME: Result is not the right return value here since we can't use
-- this to access stdout/stderr.  We can only get an exit code.
wait :: Async -> ShellM Result
wait a = do
  res <- Result <$> takeId
  FR.liftF (Wait a res)
  return res

command :: BWord -> [BWord] -> Command
command cmd args = Command cspec mempty
  where
    cspec = CommandSpec { commandName = cmd
                        , commandArguments = args
                        }

-- | A token representing the result of a command.
--
-- It will indicate places where the shell can save a PID ($!), exit
-- code ($?), or stdout/stderr
-- data Token = Command
--            | ExitCode
--            | Env
--            deriving (Eq, Ord, Show)

-- | Define a pipeline.
--
-- The LHS command is implicitly made asynchronous.  The RHS command
-- "owns" the command resulting from the ||| operator is referenced by
-- the RHS command.
-- (|||) :: Shell a -> Shell a -> Shell Command
src ||| sink = undefined -- Pipe src $ modifyStreamSpec sink $ \s ->
  -- s { ssSpecs = M.insert 1 StreamPipe (ssSpecs s) }

-- | Sequence commands
-- (#) :: Shell a -> Shell a -> Shell Command
c1 # c2 = undefined -- Sequence (SomeShell c1) c2

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

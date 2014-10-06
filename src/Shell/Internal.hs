{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module Shell.Internal (
  command,
  run,
  background,
  wait,
  env,
  unsafeEnv,
  capture,
  (|||),
  (#),
  (|>),
  (|>>),
  (<|),
  (@>),
  -- * Internal
  SomeShell(..),
  ShellState(..),
  Shell(..),
  ShellM(..),
  emptyState
  ) where

import Control.Applicative
import qualified Control.Monad.State.Strict as MS
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid
import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import Data.String

data ShellState =
  ShellState { sCommands :: !(Seq SomeShell)
               -- ^ The list of commands to run.  The index of the
               -- command in 'sCommands' is its unique identifier.  We
               -- need this because a command could be run more than
               -- once, but we might want to refer to its PID/exit
               -- code separately in the two runs:
               --
               -- > let cmd = command "Foo" []
               -- > run cmd
               -- > run cmd
               --
               -- They will compare Eq, so we need a unique identifier
               -- (position) to distinguish them.
             }
  deriving (Eq, Ord, Show)

emptyState :: ShellState
emptyState = ShellState { sCommands = Seq.empty
                        }

newtype ShellM a = ShellM (MS.State ShellState a)
                 deriving (Applicative, Functor, Monad, MS.MonadState ShellState)

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
             -- ^ A variable reference
           | BCommand (Shell Command)
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
env :: String -> BSpan
env = undefined

-- | Like 'env', but there is no static check to ensure the variable
-- is defined.  Use this if you are really sure, or if the environment
-- variable will be defined by a @source@.
--
-- Preferably, you could add conditional assignments (if not defined
-- ...) after your @source@ or similar call to ensure that a value
-- will be set.
unsafeEnv :: String -> BWord
unsafeEnv s = BWord [BVariable s]

-- | Run the given command in a subshell, capturing its output as a string.
--
-- When the command is run, you cannot retrieve its exit status.  If
-- you require that, explicitly run the command with 'subshell' and
-- manually capture its output.
capture :: Shell Command -> BWord
capture c = BWord [BCommand c]

data CommandSpec =
  CommandSpec { commandName :: BWord
              , commandArguments :: [BWord]
              }

data Shell (a :: Token) where
  RunCommand :: Maybe (Shell Command) -> CommandSpec -> StreamSpec -> Shell Command
  -- ^ Extract an exit code result
  SubShell :: [SomeShell] -> StreamSpec -> Shell Command
  -- ^ Run a list of commands in a subshell.  The exitcode is
  -- available, as is stdout/stderr.  Note, updates to the environment
  -- of subshells do not propagate to parents.
  If :: [(Condition, Block)] -> Maybe Block -> StreamSpec -> Shell Command
  -- ^ An if statement.  If statements can be redirected, so they need
  -- a stream spec
  While :: Condition -> Block -> StreamSpec -> Shell Command
  Until :: Condition -> Block -> StreamSpec -> Shell Command
  For :: String -> Exp -> Block -> StreamSpec -> Shell Command
  -- ^ For introduces a variable (the string name) based on the values
  -- in the given expression.
  -- ^ Run a command
  Wait :: Async -> Shell ExitCode
  -- ^ Wait for an asynchronous process; returns an exit code
  GetExitCode :: Result -> Shell ExitCode
  UnsetEnv :: String -> Shell Env
  SetEnv :: String -> BWord -> Shell Env
  -- ^ What do we do here?  We could set to a BWord or reference a subshell

modifyStreamSpec :: Shell Command -> (StreamSpec -> StreamSpec) -> Shell Command
modifyStreamSpec c f =
  case c of
    SubShell cmds spec -> SubShell cmds (f spec)
    RunCommand mpred cmd spec -> RunCommand mpred cmd (f spec)
    If cases melse spec -> If cases melse (f spec)
    While cond body spec -> While cond body (f spec)
    Until cond body spec -> Until cond body (f spec)
    For var ex body spec -> For var ex body (f spec)

type Block = [SomeShell]

data Exp = Exp
data Condition = Condition

instance Show (Shell a)
instance Eq (Shell a)
instance Ord (Shell a)

data SomeShell where
  SomeShell :: Shell a -> SomeShell

instance Eq SomeShell
instance Ord SomeShell
instance Show SomeShell

-- | The result of a synchronous command or a wait.  From these, we
-- can extract an exit code and possibly inspect stdout or stderr.
data Result = Result Int
            deriving (Eq, Ord, Show)

-- | The result of an asynchronous command.  We can wait on this, get
-- its PID, or reference stdout or stderr
data Async = Async Int
           deriving (Eq, Ord, Show)

addCommand :: Shell Command -> ShellM Int
addCommand c = do
  cid <- Seq.length <$> MS.gets sCommands
  MS.modify $ \s -> s { sCommands = sCommands s Seq.|> SomeShell c  }
  return cid

background :: Shell Command -> ShellM Async
background c = do
  cid <- addCommand c
  return $ Async cid

run :: Shell Command -> ShellM Result
run c = do
  cid <- addCommand c
  return $ Result cid

wait :: Async -> ShellM Result
wait = undefined

command :: BWord -> [BWord] -> Shell Command
command cmd args = RunCommand Nothing cspec mempty
  where
    cspec = CommandSpec { commandName = cmd
                        , commandArguments = args
                        }

-- | A token representing the result of a command.
--
-- It will indicate places where the shell can save a PID ($!), exit
-- code ($?), or stdout/stderr
data Token = Command
           | ExitCode
           | Env
           deriving (Eq, Ord, Show)

-- | Define a pipeline.
--
-- The LHS command is implicitly made asynchronous.  The RHS command
-- "owns" the command resulting from the ||| operator is referenced by
-- the RHS command.
(|||) :: Shell Command -> Shell Command -> Shell Command
src ||| sink = undefined

-- | Sequence commands
(#) :: Shell Command -> Shell Command -> Shell Command
(#) = undefined

-- | Redirect stdout, overwriting the target file
(|>) :: Shell Command -> FilePath -> Shell Command
(|>) c fp =
  modifyStreamSpec c $ \s -> s { ssSpecs = M.insert 1 (StreamFile fp) (ssSpecs s) }

-- | Redirect stout, appending to the target file
(|>>) :: Shell Command -> FilePath -> Shell Command
(|>>) c fp =
  modifyStreamSpec c $ \s -> s { ssSpecs = M.insert 1 (StreamAppend fp) (ssSpecs s) }

(<|) :: Shell Command -> FilePath -> Shell Command
(<|) c fp =
  modifyStreamSpec c $ \s -> s { ssSpecs = M.insert 0 (StreamFile fp) (ssSpecs s) }

-- | Redirect the first fd to the second fd
(@>) :: Shell Command -> (Int, Int) -> Shell Command
(@>) c (src, dst) =
  modifyStreamSpec c $ \s -> s { ssSpecs = M.insert src (StreamFD dst) (ssSpecs s) }

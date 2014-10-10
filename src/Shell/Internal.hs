{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Shell.Internal (
  -- * Running things
  run,
  background,
  wait,
  setEnv,
  unsetEnv,
  exportEnv,
  whileM,
  untilM,
  subshellM,
  subshellCaptureM,
  comment,
  exit,
  -- * Strings and names
  envRef,
  unsafeEnvRef,
  capture,
  pidOf,
  exitCode,
  anyChars,
  anyChar,
  charSet,
  negCharSet,
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
  -- * Tests
  testFileExists,
  -- * Internal
  BWord(..),
  BSpan(..),
  Wildcard(..),
  StreamSpec(..),
  streamSpec,
  isNullStreamSpec,
  Stream(..),
  CommandSpec(..),
  Result(..),
  Async(..),
  ShellState(..),
  Shell(..),
  TestSpec(..),
  flattenShell,
  traverseShell,
  traverseShell_,
  ShellM
  ) where

import Control.Applicative
import qualified Control.Concurrent.Supply as U
import qualified Control.Monad.Free as FR
import qualified Control.Monad.State.Strict as MS
import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import Data.String
import qualified Data.Traversable as T

data ShellState =
  ShellState { sIdSrc :: !U.Supply
             }

type ShellM = MS.StateT ShellState (FR.Free ShellF)
type FreeShell = FR.Free ShellF ()

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
data Stream = StreamFile Int BWord
            | StreamAppend Int BWord
            | StreamFD Int Int
            | StreamInput BWord
            deriving (Eq, Ord, Show)

-- | A specification of stream mappings.
--
-- Stdin is 0, stdout is 1, and stderr is 2.  Other streams are allowable.
data StreamSpec =
  StreamSpec { ssSpecs :: Seq Stream }
  deriving (Eq, Ord, Show)

instance Monoid StreamSpec where
  mempty = StreamSpec { ssSpecs = Seq.empty }
  mappend s1 s2 = StreamSpec { ssSpecs = ssSpecs s1 <> ssSpecs s2 }

isNullStreamSpec :: StreamSpec -> Bool
isNullStreamSpec (StreamSpec s) = Seq.null s

streamSpec :: Stream -> StreamSpec
streamSpec s = StreamSpec (Seq.singleton s)

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
           | BExitCode Result
             -- ^ The exit code of a process
           | BPID Async
             -- ^ The PID of a process
           | BWildcard Wildcard
           deriving (Eq, Ord, Show)

instance IsString BWord where
  fromString = toBWord

data Wildcard = WQuestion
              | WAsterisk
              | WCharSet [Char]
              | WNotCharSet [Char]
              deriving (Eq, Ord, Show)

toBWord :: String -> BWord
toBWord s = BWord [BString s]

instance Monoid BWord where
  mempty = BWord []
  mappend (BWord b1) (BWord b2) = BWord (b1 <> b2)

-- | Equivalent to the @*@ glob in shell
anyChars :: BWord
anyChars = BWord [BWildcard WAsterisk]

-- | Equivalent to the @?@ glob in shell
anyChar :: BWord
anyChar = BWord [BWildcard WQuestion]

charSet :: [Char] -> BWord
charSet cs = BWord [BWildcard (WCharSet cs)]

negCharSet :: [Char] -> BWord
negCharSet cs = BWord [BWildcard (WNotCharSet cs)]

-- | Reference the PID of a background process
pidOf :: Async -> BWord
pidOf a = BWord [BPID a]

-- | Reference the exit code of a process
exitCode :: Result -> BWord
exitCode e = BWord [BExitCode e]

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
             | Test TestSpec
             deriving (Eq, Ord, Show)

data TestSpec = TSFileExists BWord
              | TSFileIsRegular BWord
              | TSFileIsDirectory BWord
              | TSFileIsSymlink BWord
              | TSPipeExists BWord
              | TSFileIsReadableY BWord
              | TSFileNotEmpty BWord
              | TSFDOpenOnTerminal BWord
              | TSFileWritableY BWord
              | TSFileExecutableY BWord
              | TSFileEffectivelyOwnedY BWord
              | TSFileEffectivelyOwnedG BWord
              | TSFileNewer BWord BWord
              | TSFileOlder BWord BWord
              | TSStringEmpty BWord
              | TSStringNotEmpty BWord
              | TSStringIdentical BWord BWord
              | TSStringNotIdentical BWord BWord
              | TSStringLT BWord BWord
              | TSStringGT BWord BWord
              | TSNegate TestSpec
              | TSIntEq BWord BWord
              | TSIntNeq BWord BWord
              | TSIntLT BWord BWord
              | TSIntGT BWord BWord
              | TSIntLE BWord BWord
              | TSIntGE BWord BWord
              deriving (Eq, Ord, Show)

type UID = Int

data ShellF next = RunSyncF UID Command next
                 | RunAsyncF UID Command next
                 | WhileF UID Command FreeShell next
                 | UntilF UID Command FreeShell next
                 | IfF UID [(Command, FreeShell)] (Maybe FreeShell) next
                 | SubBlockF UID (Maybe String) FreeShell next
                   -- ^ For block-level subshells, with optional output capture
                 | WaitF UID Async next
                 | UnsetEnvF UID String next
                 | SetEnvF UID String BWord next
                 | ExportEnvF UID String next
                 | CommentF UID String next
                 | ExitF UID (Maybe BWord) next
                 | Done
                 deriving (Eq, Show, Functor)

data Shell = RunSync UID Command
           | RunAsync UID Command
           | While UID Command [Shell]
           | Until UID Command [Shell]
           | If UID [(Command, [Shell])] (Maybe [Shell])
           | SubBlock UID (Maybe String) [Shell]
             -- ^ Subshell with an optional capture of the output
           | Wait UID Async
           | UnsetEnv UID String
           | SetEnv UID String BWord
           | ExportEnv UID String
           | Comment UID String
           | Exit UID (Maybe BWord)

modifyStreamSpec :: Command -> (StreamSpec -> StreamSpec) -> Command
modifyStreamSpec c f =
  case c of
    SubShell cmds spec -> SubShell cmds (f spec)
    Command cmd spec -> Command cmd (f spec)
    And lhs rhs -> And lhs (modifyStreamSpec rhs f)
    Or lhs rhs -> Or lhs (modifyStreamSpec rhs f)
    Pipe lhs rhs -> Pipe lhs (modifyStreamSpec rhs f)
    Sequence lhs rhs -> Sequence lhs (modifyStreamSpec rhs f)
    Test _ -> c

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
  _ <- FR.liftF (RunAsyncF uid c res)
  return res

run :: Command -> ShellM Result
run c = do
  uid <- takeId
  let res = Result uid
  _ <- FR.liftF (RunSyncF uid c res)
  return res

whileM :: Command -> ShellM () -> ShellM ()
whileM c body = do
  uid <- takeId
  body' <- evalBody body
  FR.liftF (WhileF uid c body' ())

untilM :: Command -> ShellM () -> ShellM ()
untilM c body = do
  uid <- takeId
  body' <- evalBody body
  FR.liftF (UntilF uid c body' ())

setEnv :: String -> BWord -> ShellM ()
setEnv name val = do
  uid <- takeId
  FR.liftF (SetEnvF uid name val ())

exportEnv :: String -> ShellM ()
exportEnv name = do
  uid <- takeId
  FR.liftF (ExportEnvF uid name ())

-- | Run the given block in a subshell
subshellM :: ShellM () -> ShellM ()
subshellM body = do
  uid <- takeId
  body' <- evalBody body
  FR.liftF (SubBlockF uid Nothing body' ())

subshellCaptureM :: String -> ShellM () -> ShellM ()
subshellCaptureM name body = do
  uid <- takeId
  body' <- evalBody body
  FR.liftF (SubBlockF uid (Just name) body' ())

unsetEnv :: String -> ShellM ()
unsetEnv name = do
  uid <- takeId
  FR.liftF (UnsetEnvF uid name ())

-- | Introduce a comment into the script source
comment :: String -> ShellM ()
comment str = do
  uid <- takeId
  FR.liftF (CommentF uid str ())

-- | Wait on an asynchronous/backgrounded task.
--
-- The call to wait is synchronous and returns an exit code
wait :: Async -> ShellM Result
wait a = do
  uid <- takeId
  let res = Result uid
  _ <- FR.liftF (WaitF uid a res)
  return res

exit :: Maybe BWord -> ShellM ()
exit mexitcode = do
  uid <- takeId
  FR.liftF (ExitF uid mexitcode ())

command :: BWord -> [BWord] -> Command
command cmd args = Command cspec mempty
  where
    cspec = CommandSpec { commandName = cmd
                        , commandArguments = args
                        }

testFileExists :: BWord -> Command
testFileExists = Test . TSFileExists

-- | Define a pipeline.
--
-- The LHS command is implicitly made asynchronous.  The RHS command
-- "owns" the command resulting from the ||| operator is referenced by
-- the RHS command.
(*|*) :: Command -> Command -> Command
(*|*) = Pipe

(*||*) :: Command -> Command -> Command
(*||*) = Or

(*&&*) :: Command -> Command -> Command
(*&&*) = And

-- | Sequence commands
(#) :: Command -> Command -> Command
(#) = Sequence

-- | Redirect stdout, overwriting the target file
(|>) :: Command -> BWord -> Command
c |> fp =
  modifyStreamSpec c $ \s -> s { ssSpecs = ssSpecs s Seq.|> StreamFile 1 fp }

-- | Redirect stout, appending to the target file
(|>>) :: Command -> BWord -> Command
c |>> fp =
  modifyStreamSpec c $ \s -> s { ssSpecs = ssSpecs s Seq.|> StreamAppend 1 fp }

(<|) :: Command -> BWord -> Command
c <| fp =
  modifyStreamSpec c $ \s -> s { ssSpecs = ssSpecs s Seq.|> StreamFile 0 fp }

-- | Redirect the first fd to the second fd
(@>) :: Command -> (Int, Int) -> Command
c @> (src, dst) =
  modifyStreamSpec c $ \s -> s { ssSpecs = ssSpecs s Seq.|> StreamFD src dst }

-- | Wrap the given command in a subshell
subshell :: Command -> Command
subshell c = SubShell c mempty


data FlatState = FlatState { sShells :: Seq Shell }
emptyFlatState :: FlatState
emptyFlatState = FlatState { sShells = Seq.empty }

type Flatten = MS.State FlatState

flattenM :: FreeShell -> Flatten ()
flattenM = FR.iterM $ \f ->
  case f of
    RunSyncF uid cmd next -> appendShell (RunSync uid cmd) >> next
    RunAsyncF uid cmd next -> appendShell (RunAsync uid cmd) >> next
    WaitF uid a next -> appendShell (Wait uid a) >> next
    UnsetEnvF uid str next -> appendShell (UnsetEnv uid str) >> next
    SetEnvF uid str val next -> appendShell (SetEnv uid str val) >> next
    ExportEnvF uid str next -> appendShell (ExportEnv uid str) >> next
    CommentF uid str next -> appendShell (Comment uid str) >> next
    ExitF uid mid next -> appendShell (Exit uid mid) >> next
    IfF uid tests melse next -> do
      tests' <- T.mapM (\(c, b) -> (c,) <$> nestedBlock b) tests
      melse' <- T.mapM nestedBlock melse
      appendShell (If uid tests' melse')
      next
    WhileF uid cond body next -> do
      body' <- nestedBlock body
      appendShell (While uid cond body')
      next
    UntilF uid cond body next -> do
      body' <- nestedBlock body
      appendShell (Until uid cond body')
      next
    SubBlockF uid mcap body next -> do
      body' <- nestedBlock body
      appendShell (SubBlock uid mcap body')
      next
    Done -> return ()

nestedBlock :: FreeShell -> Flatten [Shell]
nestedBlock f = do
  s0 <- MS.get
  MS.put emptyFlatState
  flattenM f
  s1 <- MS.get
  MS.put s0
  return $ F.toList $ sShells s1

appendShell :: Shell -> Flatten ()
appendShell sh = MS.modify $ \s -> s { sShells = sShells s Seq.|> sh }

flattenShell :: ShellM () -> IO [Shell]
flattenShell st = do
  s <- U.newSupply
  let freeShell = MS.evalStateT st (ShellState s)
      cmds = F.toList $ sShells $ MS.execState (flattenM freeShell) emptyFlatState
  return cmds

-- | 'traverse' specialized to 'Shell', which is monomorphic and can't
-- be an instance of the actual 'Traversable' class.  Nested
-- statements (e.g., loops) are processed after the statements in
-- their bodies.
--
-- This evaluation order might not be suitable for everything.
traverseShell :: (Applicative f, Monad f) => (Shell -> f Shell) -> Shell -> f Shell
traverseShell f s =
  case s of
    While uid c body -> do
      w <- While uid c <$> T.traverse (traverseShell f) body
      f w
    Until uid c body -> do
      u <- Until uid c <$> T.traverse (traverseShell f) body
      f u
    SubBlock uid ms body -> do
      b <- SubBlock uid ms <$> T.traverse (traverseShell f) body
      f b
    If uid cases melse -> do
      cases' <- mapM (\(c, body) -> (c,) <$> T.traverse (traverseShell f) body) cases
      melse' <- T.traverse (T.traverse (traverseShell f)) melse
      f (If uid cases' melse')
    RunSync {} -> f s
    RunAsync {} -> f s
    Wait {} -> f s
    UnsetEnv {} -> f s
    SetEnv {} -> f s
    ExportEnv {} -> f s
    Comment {} -> f s
    Exit {} -> f s

traverseShell_ :: (Applicative f, Monad f) => (Shell -> f ()) -> Shell -> f ()
traverseShell_ f s0 = traverseShell f' s0 >> return ()
  where
    f' s = f s >> return s

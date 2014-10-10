module Main ( main ) where

import Control.Applicative
import Control.Monad.IO.Class ( liftIO )
import qualified Data.List as L
import qualified Language.Haskell.Interpreter as HI
import qualified Language.Haskell.Interpreter.Unsafe as HI
import qualified System.Directory as D
import qualified System.FilePath as FP
import System.FilePath.Glob ( namesMatching )
import qualified System.Process as P
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Text.Printf ( printf )

import Shell
import qualified Paths_shellDSL

type Generator = IO (Maybe String, [Diagnostic])

main :: IO ()
main = do
  expectedOutputFiles <- namesMatching "tests/inputs/*.expected"
  extra <- extraGhcArgs
  T.defaultMain $ T.testGroup "shell tests" (map (mkTest extra) expectedOutputFiles)

mkTest :: [String] -> FilePath -> T.TestTree
mkTest extra expectedOutputFile = T.testCase testName $ do
  expected <- read <$> readFile expectedOutputFile
  let input = FP.dropExtension expectedOutputFile
  res <- HI.unsafeRunInterpreterWithArgs extra $ do
    HI.set [HI.languageExtensions HI.:= [HI.OverloadedStrings] ]
    HI.loadModules [input]
    HI.setTopLevelModules [FP.dropExtensions $ FP.takeFileName input]
    HI.setImports ["Prelude", "Shell", "Shell.Formatter.Bash", "Shell.Formatter.POSIX"]
    rbash <- HI.interpret "runBash script" (HI.as :: Generator)
    rsh <- HI.interpret "runSh script" (HI.as :: Generator)
    case expected of
      Simple { expectedOutput = eout
             , diagnosticCount = dcount
             } -> do
        (Just bashScript, bashDiags) <- liftIO $ rbash
        liftIO $ T.assertEqual "Diagnostic count (bash)" dcount (length bashDiags)
        bashout <- liftIO $ P.readProcess "bash" [] bashScript
        liftIO $ T.assertEqual "Bash output" eout bashout
        (Just shScript, shDiags) <- liftIO $ rsh
        liftIO $ T.assertEqual "Diagnostic count (sh)" dcount (length shDiags)
        shout <- liftIO $ P.readProcess "sh" [] shScript
        liftIO $ T.assertEqual "Sh output" eout shout
    return ()
  case res of
    Left err -> T.assertFailure (show err)
    _ -> return ()
  return ()
  where
    testName = FP.takeFileName expectedOutputFile

data Expected = Simple { expectedOutput :: String
                       , diagnosticCount :: Int
                       }
              deriving (Eq, Ord, Read, Show)

data Sandbox = Sandbox
  { folder :: FilePath
  , packageFilePrefix :: String
  , packageFileSuffix :: String
  } deriving Show

cabalDev, cabalSandbox :: Sandbox
cabalDev = Sandbox "cabal-dev" "packages-" ".conf"
cabalSandbox = Sandbox ".cabal-sandbox" "" "-packages.conf.d"

-- all the sandbox systems we support.
sandboxes :: [Sandbox]
sandboxes = [cabalDev, cabalSandbox]


-- a version of isSuffixOf which returns the string stripped of its suffix.
isSuffixOf' :: String -> String -> Maybe String
isSuffixOf' suffix s = if suffix `L.isSuffixOf` s
                         then Just (take (n - m) s)
                         else Nothing
  where
    n = length s
    m = length suffix


-- convert slashes to backslashes if needed
path :: String -> String
path = map replaceSeparator where
    replaceSeparator '/' = FP.pathSeparator
    replaceSeparator x = x


-- if hawk has been compiled by a sandboxing tool,
-- its binary has been placed in a special folder.
--
-- return something like (Just "/.../cabal-dev")
getSandboxDir :: Sandbox -> IO (Maybe String)
getSandboxDir sandbox = do
    dir <- Paths_shellDSL.getBinDir
    let sandboxFolder = folder sandbox
    let suffix = path (sandboxFolder ++ "/bin")
    let basePath = suffix `isSuffixOf'` dir
    let sandboxPath = fmap (++ sandboxFolder) basePath
    return sandboxPath

-- something like "packages-7.6.3.conf"
isPackageFile :: Sandbox -> FilePath -> Bool
isPackageFile sandbox f = packageFilePrefix sandbox `L.isPrefixOf` f
                       && packageFileSuffix sandbox `L.isSuffixOf` f

-- something like "/.../cabal-dev/package-7.6.3.conf"
getPackageFile :: Sandbox -> String -> IO String
getPackageFile sandbox dir = do
    files <- D.getDirectoryContents dir
    case filter (isPackageFile sandbox) files of
      [file] -> return $ printf (path "%s/%s") dir file
      [] -> fail' "no package-db"
      _ -> fail' $ "multiple package-db's"
  where
    fail' s = error $ printf "%s found in sandbox %s" s (folder sandbox)

sandboxSpecificGhcArgs :: Sandbox -> IO [String]
sandboxSpecificGhcArgs sandbox = do
    sandboxDir <- getSandboxDir sandbox
    case sandboxDir of
      Nothing -> return []
      Just dir -> do packageFile <- getPackageFile sandbox dir
                     let arg = printf "-package-db %s" packageFile
                     return [arg]


extraGhcArgs :: IO [String]
extraGhcArgs = concat <$> mapM sandboxSpecificGhcArgs sandboxes

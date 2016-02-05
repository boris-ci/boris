{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Service.Git where

import           Boris.Core.Data
import           Boris.Service.Workspace
import qualified Boris.Service.Git as Git
import qualified Boris.X as X

import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core.IO

import           P

import           System.Exit (ExitCode (..))
import           System.Environment (setEnv)
import           System.FilePath ((</>))
import           System.IO (IO, stdout, stderr)
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (CreateProcess (..), proc)
import qualified System.Directory as D

import           Test.Boris.Core.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Trans.Either (runEitherT)

prop_initialise i =
  testIO . withSystemTempDirectory "workspace" $ \t -> do
    let
      o = CB.sinkHandle stdout
      e = CB.sinkHandle stderr
      build = Build "test"
      repository = t </> "git"
      path = WorkspacePath . T.pack . (</> "workspace") $ t

    D.createDirectoryIfMissing True repository

    setEnv "GIT_AUTHOR_DATE" "1970-01-01 00:00:00 +0000"
    setEnv "GIT_AUTHOR_NAME" "quick"
    setEnv "GIT_AUTHOR_EMAIL" "quick@email"
    setEnv "GIT_COMMITTER_DATE" "1970-01-01 00:00:00 +0000"
    setEnv "GIT_COMMITTER_NAME" "check"
    setEnv "GIT_COMMITTER_EMAIL" "check@mail"

    flail $
      X.exec o e $ (proc "git" ["init"]) { cwd = Just repository }

    T.writeFile (repository </> "boris-git.toml") "\
      \[boris] \n\
      \  version = 1 \n\
      \\n\
      \[build.test]\n\
      \  git = \"refs/heads/test\"\n\
      \\n"

    flail $
      X.exec o e $ (proc "git" ["add", "-A", "boris-git.toml"]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["commit", "-m", "first"]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["checkout", "-b", "test"]) { cwd = Just repository }

    T.writeFile (repository </> "boris.toml") "\
      \[boris]\n\
      \  version = 1\n\
      \\n\
      \[build.test]\n\
      \  command = [[\"echo\", \"test\"]]\n\
      \\n"

    flail $
      X.exec o e $ (proc "git" ["add", "-A", "boris.toml"]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["commit", "-m", "second"]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["rev-parse", "master"]) { cwd = Just repository }

    result <- runEitherT . withWorkspace path i $ \w -> do
      Git.initialise o e w build (Repository . T.pack $ repository) Nothing

    pure $ result === (Right $ BuildInstance (Specification build [] [Command "echo" ["test"]] [] [] []) (Ref "refs/heads/test") (Commit "7d4324a0cb9bb7bd0e627d6ea86dbe02aa31be62"))

flail :: IO ExitCode -> IO ()
flail cc =
  cc >>= \c -> case c of
    ExitSuccess ->
      pure ()
    ExitFailure n ->
      fail $ "Unexpected non-zero exit code: " <> show n

return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })

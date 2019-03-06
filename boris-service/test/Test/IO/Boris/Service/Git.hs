{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Boris.Service.Git where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Configuration
import           Boris.Core.Data.Repository
import           Boris.Core.Data.Instance
import           Boris.Core.Data.Workspace
import qualified Boris.Git.X as X
import qualified Boris.Service.Git as Git
import           Boris.Service.Workspace
import           Boris.Prelude

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import           Hedgehog hiding (Command)

import qualified System.Directory as D
import           System.Environment (setEnv)
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))
import           System.IO (IO, stderr, stdout)
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (CreateProcess (..), proc)

import           Test.Boris.Core.Gen

prop_initialise :: Property
prop_initialise =
  withTests 10 . property $ do
    i <- forAll genBuildId
    join . liftIO . withSystemTempDirectory "workspace" $ \t -> do
      let
        o = CB.sinkHandle stdout
        e = CB.sinkHandle stderr
        build = BuildName "test"
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

      flail $
        X.exec o e $ (proc "git" ["config", "commit.gpgsign", "false"]) { cwd = Just repository }

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


prop_discovering :: Property
prop_discovering =
  withTests 10 . property $ do
    i <- forAll genBuildId
    join . liftIO . withSystemTempDirectory "workspace" $ \t -> do
      let
        o = CB.sinkHandle stdout
        e = CB.sinkHandle stderr
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

      flail $
        X.exec o e $ (proc "git" ["config", "commit.gpgsign", "false"]) { cwd = Just repository }

      T.writeFile (repository </> "README.md") "This is a test."

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "README.md"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "first"]) { cwd = Just repository }

      --
      -- We want to test 4 scenarios:
      --
      --  1) No boris-git.toml at all, should just find nothing.
      --  2) A boris-git.toml, but no boris.toml, should just find nothing.
      --  3) A boris-git.toml, a boris.toml, but no build yet, should just find nothing.
      --  4) A boris-git.toml, a boris.toml, a matching build, should find this ref.
      --


      --
      -- (1) No boris-git.toml at all, should just find nothing.
      --
      scenario1 <- runEitherT . withWorkspace path i $ \w -> do
        Git.discovering o e w (Repository . T.pack $ repository)

      --
      -- Set up boris-git.toml for (2), (3) and (4).
      --

      T.writeFile (repository </> "boris-git.toml") "\
        \[boris] \n\
        \  version = 1 \n\
        \\n\
        \[build.\"test-*\"]\n\
        \  git = \"refs/heads/test\"\n\
        \[build.no-test-yet]\n\
        \  git = \"refs/heads/no-test-yet\"\n\
        \[build.no-boris-yet]\n\
        \  git = \"refs/heads/no-boris-yet\"\n\
        \\n"

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "boris-git.toml"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "boris-git"]) { cwd = Just repository }

      --
      -- (2) A boris-git.toml, but no boris.toml, should just find nothing.
      --

      flail $
        X.exec o e $ (proc "git" ["checkout", "-b", "no-boris-yet"]) { cwd = Just repository }
      T.writeFile (repository </> "not-boris-related") ""

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "not-boris-related"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "not-boris-related"]) { cwd = Just repository }

      scenario2 <- runEitherT . withWorkspace path i $ \w -> do
        Git.discovering o e w (Repository . T.pack $ repository)

      --
      -- (3) A boris-git.toml, a boris.toml, but no build yet, should just find nothing.
      --

      flail $
        X.exec o e $ (proc "git" ["checkout", "-b", "no-test-yet"]) { cwd = Just repository }

      T.writeFile (repository </> "boris.toml") "\
        \[boris]\n\
        \  version = 1\n\
        \\n"

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "boris.toml"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "test"]) { cwd = Just repository }

      scenario3 <- runEitherT . withWorkspace path i $ \w -> do
        Git.discovering o e w (Repository . T.pack $ repository)

      --
      -- (4) A boris-git.toml, a boris.toml, a matching build, should find this ref.
      --

      flail $
        X.exec o e $ (proc "git" ["checkout", "-b", "test"]) { cwd = Just repository }

      T.writeFile (repository </> "boris.toml") "\
        \[boris]\n\
        \  version = 1\n\
        \\n\
        \[build.test-1]\n\
        \  command = [[\"echo\", \"test 1\"]]\n\
        \\n\
        \[build.test-2]\n\
        \  command = [[\"echo\", \"test 2\"]]\n\
        \\n"

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "boris.toml"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "test"]) { cwd = Just repository }

      scenario4 <- runEitherT . withWorkspace path i $ \w -> do
        Git.discovering o e w (Repository . T.pack $ repository)

      (commit, _, _) <- X.capture o e $ (proc "git" ["rev-parse", "refs/heads/test"]) { cwd = Just repository }

      pure $ do
        scenario1 === Right []
        scenario2 === Right []
        scenario3 === Right []
        scenario4 === Right [
            DiscoverInstance (BuildName "test-1") (Ref "refs/heads/test") (Commit . T.strip . T.decodeUtf8 $ commit)
          , DiscoverInstance (BuildName "test-2") (Ref "refs/heads/test") (Commit . T.strip . T.decodeUtf8 $ commit)
          ]


prop_discovering_broken_branch :: Property
prop_discovering_broken_branch =
  withTests 10 . property $ do
    i <- forAll genBuildId
    join . liftIO . withSystemTempDirectory "workspace" $ \t -> do
      let
        o = CB.sinkHandle stdout
        e = CB.sinkHandle stderr
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

      flail $
        X.exec o e $ (proc "git" ["config", "commit.gpgsign", "false"]) { cwd = Just repository }

      T.writeFile (repository </> "README.md") "This is a test."

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "README.md"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "first"]) { cwd = Just repository }

      T.writeFile (repository </> "boris-git.toml") "\
        \[boris] \n\
        \  version = 1 \n\
        \\n\
        \[build.\"test-*\"]\n\
        \  git = \"refs/heads/test-*\"\n\
        \\n"

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "boris-git.toml"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "boris-git"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["checkout", "-b", "test-good", "master"]) { cwd = Just repository }

      --
      -- A good boris.toml file
      --
      T.writeFile (repository </> "boris.toml") "\
        \[boris]\n\
        \  version = 1\n\
        \\n\
        \[build.test-1]\n\
        \  command = [[\"echo\", \"test 1\"]]\n\
        \\n\
        \\n"

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "boris.toml"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "test"]) { cwd = Just repository }

      --
      -- A broken boris.toml file
      --
      flail $
        X.exec o e $ (proc "git" ["checkout", "-b", "test-bad", "master"]) { cwd = Just repository }

      T.writeFile (repository </> "boris.toml") "bad"

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "boris.toml"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "test"]) { cwd = Just repository }

      result <- runEitherT . withWorkspace path i $ \w -> do
        Git.discovering o e w (Repository . T.pack $ repository)

      (commit, _, _) <- X.capture o e $ (proc "git" ["rev-parse", "refs/heads/test-good"]) { cwd = Just repository }

      pure $ do
        result === Right [
              DiscoverInstance (BuildName "test-1") (Ref "refs/heads/test-good") (Commit . T.strip . T.decodeUtf8 $ commit)
            ]

flail :: IO ExitCode -> IO ()
flail cc =
  cc >>= \c -> case c of
    ExitSuccess ->
      pure ()
    ExitFailure n ->
      fail $ "Unexpected non-zero exit code: " <> show n

tests :: IO Bool
tests =
  checkParallel $$(discover)

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Build where

import           Boris.Build
import           Boris.Core.Data.Configuration
import           Boris.Core.Data.Build
import           Boris.Core.Data.Workspace

import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T

import           Disorder.Core.IO

import           P

import qualified System.Directory as D
import           System.Exit (ExitCode (..))
import           System.FilePath (FilePath, (</>))
import           System.IO (IO, stderr, stdout)
import           System.IO.Temp (withSystemTempDirectory)

import           Test.Boris.Core.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Trans.Either (runEitherT)

prop_runBuild_happy b =
  testIO . withSystemTempDirectory "build"  $ \t ->
    check t (Right (), True, True, True, True, False) $
      Specification b
        [touch "pre"]
        [touch "command"]
        [touch "post"]
        [touch "success"]
        [touch "failure"]

prop_runBuild_pre_fails_first b =
  testIO . withSystemTempDirectory "build"  $ \t ->
    check t (Left . PreError $ ExitFailure 1, False, False, True, False, True) $
      Specification b
        [bad, touch "pre"]
        [touch "command"]
        [touch "post"]
        [touch "success"]
        [touch "failure"]

prop_runBuild_pre_fails_second b =
  testIO . withSystemTempDirectory "build"  $ \t ->
    check t (Left . PreError $ ExitFailure 1, True, False, True, False, True) $
      Specification b
        [touch "pre", bad]
        [touch "command"]
        [touch "post"]
        [touch "success"]
        [touch "failure"]

prop_runBuild_build_fails_first b =
  testIO . withSystemTempDirectory "build"  $ \t ->
    check t (Left . RunError $ ExitFailure 1, True, False, True, False, True) $
      Specification b
        [touch "pre"]
        [bad, touch "command"]
        [touch "post"]
        [touch "success"]
        [touch "failure"]

prop_runBuild_build_fails_second b =
  testIO . withSystemTempDirectory "build"  $ \t ->
    check t (Left . RunError $ ExitFailure 1, True, True, True, False, True) $
      Specification b
        [touch "pre"]
        [touch "command", bad]
        [touch "post"]
        [touch "success"]
        [touch "failure"]

prop_runBuild_post_fails_first b =
  testIO . withSystemTempDirectory "build"  $ \t ->
    check t (Left . PostError $ ExitFailure 1, True, True, False, False, False) $
      Specification b
        [touch "pre"]
        [touch "command"]
        [bad, touch "post"]
        [touch "success"]
        [touch "failure"]

prop_runBuild_post_fails_second b =
  testIO . withSystemTempDirectory "build"  $ \t ->
    check t (Left . PostError $ ExitFailure 1, True, True, True, False, False) $
      Specification b
        [touch "pre"]
        [touch "command"]
        [touch "post", bad]
        [touch "success"]
        [touch "failure"]

prop_runBuild_success_fails_first b =
  testIO . withSystemTempDirectory "build"  $ \t ->
    check t (Left . SuccessError $ ExitFailure 1, True, True, True, False, False) $
      Specification b
        [touch "pre"]
        [touch "command"]
        [touch "post"]
        [bad, touch "success"]
        [touch "failure"]

prop_runBuild_success_fails_second b =
  testIO . withSystemTempDirectory "build"  $ \t ->
    check t (Left . SuccessError $ ExitFailure 1, True, True, True, True, False) $
      Specification b
        [touch "pre"]
        [touch "command"]
        [touch "post"]
        [touch "success", bad]
        [touch "failure"]

prop_runBuild_multi_fail_failure  b =
  testIO . withSystemTempDirectory "build"  $ \t ->
    check t (Left . FailureError $ ExitFailure 1, True, True, True, False, True) $
      Specification b
        [touch "pre"]
        [touch "command", bad]
        [touch "post"]
        [touch "success"]
        [touch "failure", bad]

prop_runBuild_multi_fail_post  b =
  testIO . withSystemTempDirectory "build"  $ \t ->
    check t (Left . PostError $ ExitFailure 1, True, True, True, False, False) $
      Specification b
        [touch "pre"]
        [touch "command", bad]
        [touch "post", bad]
        [touch "success"]
        [touch "failure", bad]

prop_runBuild_multi b =
  testIO . withSystemTempDirectory "build"  $ \x -> do
    let
      local = Workspace (WorkspacePath . T.pack $ x) (BuildId 1)
      t = pathOfWorkingCopy local

    D.createDirectoryIfMissing True t

    r <- runEitherT . runBuild o e local
      (Specification b
        [touch "pre.1", touch "pre.2"]
        [touch "command.1", touch "command.2"]
        [touch "post.1", touch "post.2"]
        [touch "success.1", touch "success.2"]
        [touch "failure.1", touch "failure.2"]) $ []

    pre1 <- D.doesFileExist (t </> "pre.1")
    command1 <- D.doesFileExist (t </> "command.1")
    post1 <- D.doesFileExist (t </> "post.1")
    success1 <- D.doesFileExist (t </> "success.1")
    failure1 <- D.doesFileExist (t </> "failure.1")

    pre2 <- D.doesFileExist (t </> "pre.2")
    command2 <- D.doesFileExist (t </> "command.2")
    post2 <- D.doesFileExist (t </> "post.2")
    success2 <- D.doesFileExist (t </> "success.2")
    failure2 <- D.doesFileExist (t </> "failure.2")

    pure $
      (r, pre1, command1, post1, success1, failure1, pre2, command2, post2, success2, failure2) ===
        (Right (), True, True, True, True, False, True, True, True, True, False)

check :: FilePath -> (Either BuildError (), Bool, Bool, Bool, Bool, Bool) -> Specification -> IO Property
check x expected specification = do
  let
    local = Workspace (WorkspacePath . T.pack $ x) (BuildId 1)
    t = pathOfWorkingCopy local


  D.createDirectoryIfMissing True t
  r <- runEitherT $ runBuild o e local specification []

  pre <- D.doesFileExist (t </> "pre")
  command <- D.doesFileExist (t </> "command")
  post <- D.doesFileExist (t </> "post")
  success <- D.doesFileExist (t </> "success")
  failure <- D.doesFileExist (t </> "failure")

  pure $ (r, pre, command, post, success, failure) === expected

touch f =
  Command "touch" [f]

bad =
  Command "false" []

o =
  CB.sinkHandle stdout

e =
  CB.sinkHandle stderr

return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })

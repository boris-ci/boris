{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Build (
    BuildError (..)
  , runBuild
  , renderBuildError
  ) where

import           Boris.Core.Data.Configuration
import           Boris.Core.Data.Workspace

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import           P

import           Tine.Conduit (Out, WithEnv (..))
import qualified Tine.Conduit as X

import           System.Exit (ExitCode (..))
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, bimapEitherT, hoistEither, runEitherT)

data BuildError =
    RunError ExitCode
  | PreError ExitCode
  | PostError ExitCode
  | SuccessError ExitCode
  | FailureError ExitCode
    deriving (Eq, Show)

-- |
-- Run the build specification lifecycle to completion.
--
-- The flow is:
--
-- @
--                    pre
--               (ok) / \ (ko)
--                   /   \
--                  /     \
--               build     \
--           (ok) / \ (ko) /
--               /   \    /
--              /     \  /
--           post     post
--      (ok) / \ (ko) / \ (ok)
--          /   *    *   \
--         /              \
--     success           failure
--       /                  \
--      *                    *
--
-- @
--
-- General semantics:
--
--  * Each of the pre/build/post/success/failure stages are a set of
--    commands. They run in-order of specification, and if one fails,
--    the rest are _not_ run.
--
--  * If a post-process fails it supersedes the original build/pre
--    result.
--
--  * If _anything_ fails the outcome must be a failure.
--
--  * For a success, (somewhat obviously) _everything_ must succeed.
--
runBuild :: Out -> Out -> Workspace -> Specification -> [WithEnv] -> EitherT BuildError IO ()
runBuild sout serr workspace specification env = do
  let
    path =
      pathOfWorkingCopy workspace

    gos cmds =
      forM_ cmds $ go

    go cmd =
      X.hoistExitM $
        X.exec sout serr =<<
          X.withEnv env =<<
            X.xprocAt serr path (commandName cmd) (commandArgs cmd)

  -- We run all the pre-commands, followed by all the commands in order, until one fails.
  -- Note that we unwrap the EitherT at this point because while we want to early terminate
  -- on any of these as they happen, we don't want to early terminate on the entire block,
  -- so we can run the post build commands.

  r <- liftIO . runEitherT $ do
    bimapEitherT PreError id . gos $
      specificationPre specification

    bimapEitherT RunError id . gos $
      specificationCommand specification

  -- Always run post-commands, no matter the outcome of the pre and build commands.

  bimapEitherT PostError id . gos $
    specificationPost specification

  -- Only run failure command if the pre or build commands failed (and post-build succeeded).

  when (isLeft r) $
    bimapEitherT FailureError id . gos $
      specificationFailure specification

  -- Only run success command if the pre or build commands ran ok (and the post-build succeeded).

  when (isRight r) $
    bimapEitherT SuccessError id . gos $
      specificationSuccess specification

  -- Re-hoist the build result if all the post-build steps completed succefully.

  hoistEither r


renderBuildError :: BuildError -> Text
renderBuildError err =
  case err of
    RunError c ->
      mconcat ["Build failed, exit with: ", T.pack . show $ c]
    PreError c ->
      mconcat ["Pre-Build command failed, exit with: ", T.pack . show $ c]
    PostError c ->
      mconcat ["Post-Build command failed, exit with: ", T.pack . show $ c]
    SuccessError c ->
      mconcat ["On-Success-Build command failed, exit with: ", T.pack . show $ c]
    FailureError c ->
      mconcat ["On-Failure-Build command failed, exit with: ", T.pack . show $ c]

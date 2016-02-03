{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Git where

import           Boris.Core.Data
import qualified Boris.X as X
import qualified Boris.Git as Git

import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core.IO

import           P

import           System.Exit (ExitCode (..))
import           System.FilePath ((</>), (<.>))
import           System.IO (IO, stdout, stderr)
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (CreateProcess (..), proc)
import qualified System.Directory as D

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Trans.Either (EitherT, runEitherT)

prop_bare_clone_basic datas =
  testIO . withSystemTempDirectory "git" $ \t -> do
    let
      o = CB.sinkHandle stdout
      e = CB.sinkHandle stderr
      source = t </> "source.git"
      target = t </> "bare.git"
      check = t </> "check.git"

      readme = source </> "readme"
      files = fmap (\(n, d) -> ("data" <.> show n, d)) $ L.zip ([0..] :: [Int]) datas

    D.createDirectoryIfMissing True source

    T.writeFile readme "A testing repository (readme to guarantee there is at least one file)."

    forM_ files $ \(n, d) ->
      T.writeFile (source </> n) d

    flail $
      X.exec o e $ (proc "git" ["init"]) { cwd = Just source }

    flail $
      X.exec o e $ (proc "git" ["add", "-A"]) { cwd = Just source }

    flail $
      X.exec o e $ (proc "git" ["commit", "-m", "first"]) { cwd = Just source }

    target' <- flailx $
      Git.bare o e (Repository . T.pack $ source) target

    _check' <- flailx $
      Git.clone o e target' check

    fmap conjoin . forM files $ \(n, d) -> do
      x <- T.readFile (check </> n)
      pure . counterexample ("File " <> n <> " was not the same.") $
        x === d

prop_refs v0 v1 = v0 /= v1 ==>
  testIO . withSystemTempDirectory "git" $ \t -> do
    let
      o = CB.sinkHandle stdout
      e = CB.sinkHandle stderr
      repository = t </> "repository.git"
      file = repository </> "file"
      local = LocalRepository . T.pack $ repository

    D.createDirectoryIfMissing True repository

    T.writeFile file v0

    flail $
      X.exec o e $ (proc "git" ["init"]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["add", "-A", "."]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["commit", "-m", "first"]) { cwd = Just repository }

    T.writeFile file v1

    flail $
      X.exec o e $ (proc "git" ["add", "-A", "."]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["commit", "-m", "second"]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["checkout", "-b", "topic/branch"]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["checkout", "-b", "dev/branch"]) { cwd = Just repository }

    topics <- flailx $
      Git.refs o e local  (Query "refs/heads/topic/*")

    branches <- flailx $
      Git.refs o e local (Query "refs/heads/*/branch")

    star <- flailx $
      Git.refs o e local (Query "refs/heads/**")

    pure $
      [topics, L.sort branches, L.sort star] === [
          [Ref "refs/heads/topic/branch"]
        , L.sort [Ref "refs/heads/topic/branch", Ref "refs/heads/dev/branch"]
        , L.sort [Ref "refs/heads/master", Ref "refs/heads/topic/branch", Ref "refs/heads/dev/branch"]]

prop_checkout v0 v1 = v0 /= v1 ==>
  testIO . withSystemTempDirectory "git" $ \t -> do
    let
      o = CB.sinkHandle stdout
      e = CB.sinkHandle stderr
      repository = t </> "repository.git"
      file = repository </> "file"
      local = LocalRepository . T.pack $ repository

    D.createDirectoryIfMissing True repository

    T.writeFile file v0

    flail $
      X.exec o e $ (proc "git" ["init"]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["add", "-A", "file"]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["commit", "-m", "first"]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["checkout", "-b", "topic/branch"]) { cwd = Just repository }

    T.writeFile file v1

    flail $
      X.exec o e $ (proc "git" ["add", "-A", "."]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["commit", "-m", "second"]) { cwd = Just repository }

    t1 <- T.readFile file

    flail $
      X.exec o e $ (proc "git" ["status"]) { cwd = Just repository }

    flailx $
      Git.checkout o e local (Ref "master")

    t2 <- T.readFile file

    flailx $
      Git.checkout o e local (Ref "topic/branch")

    t3 <- T.readFile file

    pure $ [t1, t2, t3] === [v1, v0, v1]

prop_cat v0 v1 = v0 /= v1 ==>
  testIO . withSystemTempDirectory "git" $ \t -> do
    let
      o = CB.sinkHandle stdout
      e = CB.sinkHandle stderr
      oo = CL.sinkNull
      ee = CL.sinkNull

      repository = t </> "repository.git"
      file = repository </> "file"
      local = LocalRepository . T.pack $ repository

    D.createDirectoryIfMissing True repository

    T.writeFile file v0

    flail $
      X.exec o e $ (proc "git" ["init"]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["add", "-A", "file"]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["commit", "-m", "first"]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["checkout", "-b", "topic/branch"]) { cwd = Just repository }

    T.writeFile file v1

    flail $
      X.exec o e $ (proc "git" ["add", "-A", "."]) { cwd = Just repository }

    flail $
      X.exec o e $ (proc "git" ["commit", "-m", "second"]) { cwd = Just repository }

    t1 <- T.readFile file

    t2 <- flailx $
      Git.cat oo ee local (Ref "master") "file"

    t3 <- flailx $
      Git.cat oo ee local (Ref "topic/branch") "file"

    pure $ [t1, t2, t3] === [v1, v0, v1]


flail :: IO ExitCode -> IO ()
flail cc =
  cc >>= \c -> case c of
    ExitSuccess ->
      pure ()
    ExitFailure n ->
      fail $ "Unexpected non-zero exit code: " <> show n

flailx :: EitherT ExitCode IO a -> IO a
flailx cc =
  runEitherT cc >>= \c -> case c of
    Left e ->
      fail $ "Unexpected exit code: " <> show e
    Right a ->
      pure a

return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Boris.Git where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Repository
import qualified Boris.Git as Git
import qualified Boris.Git.X as X
import           Boris.Prelude

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified System.Directory as Directory
import           System.Exit (ExitCode (..))
import           System.FilePath ((<.>), (</>))
import           System.IO (IO, stderr, stdout)
import qualified System.IO.Temp as Temporary
import           System.Process (CreateProcess (..), proc)

prop_bare_clone_basic :: Property
prop_bare_clone_basic =
  withTests 10 . property $ do
    datas <- forAll $ Gen.list (Range.linear 0 10) $ Gen.element [
        "red"
      , "green"
      , "blue"
      ]
    liftIO $ Directory.createDirectoryIfMissing True "tmp"
    (actual, expected) <- liftIO $ Temporary.withTempDirectory "tmp" "git" $ \t -> do
      let
        o = CB.sinkHandle stdout
        e = CB.sinkHandle stderr
        source = t </> "source.git"
        target = t </> "bare.git"
        checker = t </> "check.git"

        readme = source </> "readme"
        files = fmap (\(n, d) -> ("data" <.> show n, d)) $ List.zip ([0..] :: [Int]) datas

      Directory.createDirectoryIfMissing True source

      Text.writeFile readme "A testing repository (readme to guarantee there is at least one file)."

      forM_ files $ \(n, d) ->
        Text.writeFile (source </> n) d

      flail $
        X.exec o e $ (proc "git" ["init"]) { cwd = Just source }

      flail $
        X.exec o e $ (proc "git" ["add", "-A"]) { cwd = Just source }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "first"]) { cwd = Just source }

      target' <- flailx $
        Git.bare o e (Repository . Text.pack $ source) target

      _check' <- flailx $
        Git.clone o e target' checker

      xs <- forM files $ \(n, _) -> do
        Text.readFile (checker </> n)

      pure $ (xs, fmap snd files)

    actual === expected

prop_bare_cloneref_basic :: Property
prop_bare_cloneref_basic =
  withTests 10 . property $ do
    datas <- forAll $ Gen.list (Range.linear 0 10) $ Gen.element [
        "red"
      , "green"
      , "blue"
      ]
    liftIO $ Directory.createDirectoryIfMissing True "tmp"

    (actual, expected) <- liftIO $ Temporary.withTempDirectory "tmp" "git" $ \t -> do
      let
        o = CB.sinkHandle stdout
        e = CB.sinkHandle stderr
        source = t </> "source.git"
        target = t </> "bare.git"
        checker = t </> "check.git"

        readme = source </> "readme"
        files = fmap (\(n, d) -> ("data" <.> show n, d)) $ List.zip ([0..] :: [Int]) datas

      Directory.createDirectoryIfMissing True source

      Text.writeFile readme "A testing repository (readme to guarantee there is at least one file)."

      forM_ files $ \(n, d) ->
        Text.writeFile (source </> n) d

      flail $
        X.exec o e $ (proc "git" ["init"]) { cwd = Just source }

      flail $
        X.exec o e $ (proc "git" ["add", "-A"]) { cwd = Just source }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "first"]) { cwd = Just source }

      target' <- flailx $
        Git.bare o e (Repository . Text.pack $ source) target

      _check' <- flailx $
        Git.cloneref o e target' (Repository . Text.pack $ source) checker

      xs <- forM files $ \(n, _) -> do
        Text.readFile (checker </> n)

      pure $ (xs, fmap snd files)

    actual === expected



prop_refs :: Property
prop_refs =
  withTests 10 . property $ do
    v0 <- forAll $ Gen.element [
        "red"
      , "green"
      , "blue"
      ]
    v1 <- forAll $ Gen.element [
        "der"
      , "neerg"
      , "eulb"
      ]
    liftIO $ Directory.createDirectoryIfMissing True "tmp"

    actual <- liftIO $ Temporary.withTempDirectory "tmp" "git" $ \t -> do
      let
        o = CB.sinkHandle stdout
        e = CB.sinkHandle stderr
        repository = t </> "repository.git"
        file = repository </> "file"
        local = LocalRepository . Text.pack $ repository

      Directory.createDirectoryIfMissing True repository

      Text.writeFile file v0

      flail $
        X.exec o e $ (proc "git" ["init"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "."]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "first"]) { cwd = Just repository }

      Text.writeFile file v1

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "."]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "second"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["checkout", "-b", "topic/branch"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["checkout", "-b", "dev/branch"]) { cwd = Just repository }

      topics <- flailx $
        Git.refs o e local  (Pattern "refs/heads/topic/*")

      branches <- flailx $
        Git.refs o e local (Pattern "refs/heads/*/branch")

      star <- flailx $
        Git.refs o e local (Pattern "refs/heads/*")

      starstar <- flailx $
        Git.refs o e local (Pattern "refs/heads/**")

      pure [topics, List.sort branches, star, starstar]

    actual === [
          [Ref "refs/heads/topic/branch"]
        , List.sort [Ref "refs/heads/topic/branch", Ref "refs/heads/dev/branch"]
        , [Ref "refs/heads/master"]
        , List.sort [Ref "refs/heads/master", Ref "refs/heads/topic/branch", Ref "refs/heads/dev/branch"]
        ]

prop_checkout :: Property
prop_checkout =
  withTests 10 . property $ do
    v0 <- forAll $ Gen.element [
        "red"
      , "green"
      , "blue"
      ]
    v1 <- forAll $ Gen.element [
        "der"
      , "neerg"
      , "eulb"
      ]
    liftIO $ Directory.createDirectoryIfMissing True "tmp"
    actual <- liftIO $ Temporary.withTempDirectory "tmp" "git" $ \t -> do
      let
        o = CB.sinkHandle stdout
        e = CB.sinkHandle stderr
        repository = t </> "repository.git"
        file = repository </> "file"
        local = LocalRepository . Text.pack $ repository

      Directory.createDirectoryIfMissing True repository

      Text.writeFile file v0

      flail $
        X.exec o e $ (proc "git" ["init"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "file"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "first"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["checkout", "-b", "topic/branch"]) { cwd = Just repository }

      Text.writeFile file v1

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "."]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "second"]) { cwd = Just repository }

      t1 <- Text.readFile file

      flail $
        X.exec o e $ (proc "git" ["status"]) { cwd = Just repository }

      flailx $
        Git.checkout o e local (Ref "master")

      t2 <- Text.readFile file

      flailx $
        Git.checkout o e local (Ref "topic/branch")

      t3 <- Text.readFile file

      pure $ [t1, t2, t3]

    actual === [v1, v0, v1]

prop_cat :: Property
prop_cat =
  withTests 10 . property $ do
    v0 <- forAll $ Gen.element [
        "red"
      , "green"
      , "blue"
      ]
    v1 <- forAll $ Gen.element [
        "der"
      , "neerg"
      , "eulb"
      ]
    liftIO $ Directory.createDirectoryIfMissing True "tmp"

    actual <- liftIO $ Temporary.withTempDirectory "tmp" "git" $ \t -> do
      let
        o = CB.sinkHandle stdout
        e = CB.sinkHandle stderr
        oo = CL.sinkNull
        ee = CL.sinkNull

        repository = t </> "repository.git"
        file = repository </> "file"
        local = LocalRepository . Text.pack $ repository

      Directory.createDirectoryIfMissing True repository

      Text.writeFile file v0

      flail $
        X.exec o e $ (proc "git" ["init"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "file"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "first"]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["checkout", "-b", "topic/branch"]) { cwd = Just repository }

      Text.writeFile file v1

      flail $
        X.exec o e $ (proc "git" ["add", "-A", "."]) { cwd = Just repository }

      flail $
        X.exec o e $ (proc "git" ["commit", "-m", "second"]) { cwd = Just repository }

      t1 <- Text.readFile file

      t2 <- flailx $
        Git.cat oo ee local (Ref "master") "file"

      t3 <- flailx $
        Git.cat oo ee local (Ref "topic/branch") "file"

      pure [t1, t2, t3]

    actual === [v1, v0, v1]


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

tests :: IO Bool
tests =
  checkParallel $$(discover)

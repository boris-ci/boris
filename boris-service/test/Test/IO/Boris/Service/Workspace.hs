{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Boris.Service.Workspace where

import           Boris.Core.Data.Workspace
import           Boris.Service.Workspace
import           Boris.Prelude

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T


import           Hedgehog hiding (Command)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified System.Directory as D
import           System.FilePath ((</>))
import           System.IO (IO)
import           System.IO.Temp (withSystemTempDirectory)

import           Test.Boris.Core.Gen


prop_withWorkspace :: Property
prop_withWorkspace =
  withTests 10 . property $ do
    i <- forAll genBuildId
    d <- forAll $ Gen.text (Range.linear 0 200) Gen.unicode
    join . liftIO . withSystemTempDirectory "workspace" $ \t -> do
      ww <- runEitherT . withWorkspace (WorkspacePath . T.pack $ t) i $ \w -> liftIO $ do
        D.createDirectoryIfMissing False (pathOf w </> "dir")
        T.writeFile (pathOf w </> "file") d
        T.writeFile (pathOf w </> "dir" </> "nested") d
        pure w
      case ww of
        Left _ -> pure $ do
          annotate "withWorkspace should of succeeded"
          failure
        Right w -> do
          r <- D.doesDirectoryExist $ pathOf w
          pure $ r === False

tests :: IO Bool
tests =
  checkParallel $$(discover)

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Service.Workspace where

import           Boris.Core.Data
import           Boris.Service.Workspace

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core
import           Disorder.Core.IO

import           P

import qualified System.Directory as D
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)

import           Test.Boris.Core.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Trans.Either (runEitherT)

prop_withWorkspace i d =
  testIO . withSystemTempDirectory "workspace" $ \t -> do
    ww <- runEitherT . withWorkspace (WorkspacePath . T.pack $ t) i $ \w -> liftIO $ do
      D.createDirectoryIfMissing False (pathOf w </> "dir")
      T.writeFile (pathOf w </> "file") d
      T.writeFile (pathOf w </> "dir" </> "nested") d
      pure w
    case ww of
      Left _ ->
        pure $ failWith "withWorkspace should of succeeded"
      Right w -> do
        r <- D.doesDirectoryExist $ pathOf w
        pure $ r === False

return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })

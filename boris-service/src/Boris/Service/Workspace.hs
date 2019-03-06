{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Workspace (
    newWorkspace
  , destroyWorkspace
  , withWorkspace
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Workspace
import           Boris.Prelude


import           Control.Monad.IO.Class (liftIO)

import           System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import           System.IO (IO)


newWorkspace :: WorkspacePath -> BuildId -> IO Workspace
newWorkspace work build =
  let
    w = Workspace work build
    p = pathOf w
  in
    w <$ createDirectoryIfMissing True p

destroyWorkspace :: Workspace -> IO ()
destroyWorkspace =
  removeDirectoryRecursive . pathOf

withWorkspace :: WorkspacePath -> BuildId -> (Workspace -> EitherT e IO a) -> EitherT e IO a
withWorkspace work build =
  bracketEitherT'
    (liftIO $ newWorkspace work build)
    (liftIO . destroyWorkspace)

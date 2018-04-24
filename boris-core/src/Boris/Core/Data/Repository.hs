{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Core.Data.Repository (
    Repository (..)
  , LocalRepository (..)
  ) where

import           P


newtype Repository =
  Repository {
      renderRepository :: Text
    } deriving (Eq, Show, Ord)

newtype LocalRepository =
  LocalRepository {
      renderLocalRepository :: Text
    } deriving (Eq, Show, Ord)

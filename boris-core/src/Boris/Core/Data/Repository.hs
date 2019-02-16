{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Core.Data.Repository (
    Repository (..)
  , LocalRepository (..)
  ) where

import           Boris.Prelude


newtype Repository =
  Repository {
      renderRepository :: Text
    } deriving (Eq, Show, Ord)

newtype LocalRepository =
  LocalRepository {
      renderLocalRepository :: Text
    } deriving (Eq, Show, Ord)

{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Core.Data.Repository (
    Repository (..)
  , LocalRepository (..)
  , isValidRepository
  ) where

import           Boris.Prelude

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.List as List


newtype Repository =
  Repository {
      renderRepository :: Text
    } deriving (Eq, Show, Ord)

newtype LocalRepository =
  LocalRepository {
      renderLocalRepository :: Text
    } deriving (Eq, Show, Ord)


isValidRepository :: Repository -> Bool
isValidRepository repository =
  and . fmap ($ renderRepository repository) $ [
      not . Text.null
    , Text.all isValidRepositoryChar
    ]

isValidRepositoryChar :: Char -> Bool
isValidRepositoryChar c =
  Char.isAlpha c || Char.isDigit c || List.elem c ['-', '.', '_', '@', ':', '/', '+']

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Representation.Commit (
    GetCommit (..)
  ) where

import           Boris.Core.Data

import           Data.Aeson (ToJSON (..), object, (.=))

import           P


data GetCommit =
  GetCommit Project [BuildId]

instance ToJSON GetCommit where
  toJSON (GetCommit p bs) =
    object [
        "project" .= renderProject p
      , "builds" .= (fmap renderBuildId . sortBuildIds) bs
      ]

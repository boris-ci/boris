{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Representation.Project (
    GetProjects (..)
  , GetProject (..)
  ) where

import           Boris.Core.Data

import           Data.Aeson (ToJSON (..), object, (.=))

import           P


newtype GetProjects =
  GetProjects [Project]

instance ToJSON GetProjects where
  toJSON (GetProjects ps) =
    object [
        "projects" .= fmap renderProject ps
      ]

data GetProject =
  GetProject Project [Build]

instance ToJSON GetProject where
  toJSON (GetProject p bs) =
    object [
        "project" .= renderProject p
      , "builds" .= fmap renderBuild bs
      ]

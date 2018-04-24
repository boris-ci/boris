{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Core.Data.Instance (
    BuildInstance (..)
  , DiscoverInstance (..)
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Configuration

import           P

data BuildInstance =
  BuildInstance {
      buildSpecification :: Specification
    , buildRef :: Ref
    , buildCommit :: Commit
    } deriving (Eq, Show)

data DiscoverInstance =
  DiscoverInstance {
      discoverBuild :: Build
    , discoverRef :: Ref
    , discoverCommit :: Commit
    } deriving (Eq, Ord, Show)

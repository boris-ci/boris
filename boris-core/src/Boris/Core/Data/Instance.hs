{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Core.Data.Instance (
    BuildInstance (..)
  , DiscoverInstance (..)
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Configuration
import           Boris.Prelude


data BuildInstance =
  BuildInstance {
      buildSpecification :: Specification
    , buildIRef :: Ref
    , buildICommit :: Commit
    } deriving (Eq, Show)

data DiscoverInstance =
  DiscoverInstance {
      discoverBuild :: BuildName
    , discoverRef :: Ref
    , discoverCommit :: Commit
    } deriving (Eq, Ord, Show)

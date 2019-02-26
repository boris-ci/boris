{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Boris.Http.Template.Data.Commit.Data where
import qualified Projector.Html.Runtime
data Commit
    = Commit {commitProject :: !Projector.Html.Runtime.Text,
              commitCommit :: !Projector.Html.Runtime.Text,
              commitBuilds :: !([CommitBuild])}
data CommitBuild
    = CommitBuild {commitBuildName :: !Projector.Html.Runtime.Text,
                   commitBuildIds :: !([Projector.Html.Runtime.Text])}

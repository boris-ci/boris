{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Boris.Http.Template.Data.Project.Data where
import qualified Projector.Html.Runtime
data Project
    = Project {projectName :: !Projector.Html.Runtime.Text,
               projectBuilds :: !([Projector.Html.Runtime.Text])}

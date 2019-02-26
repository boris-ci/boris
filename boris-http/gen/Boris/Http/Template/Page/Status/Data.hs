{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Boris.Http.Template.Page.Status.Data where
import qualified Projector.Html.Runtime
data Status = Status {statusBuilds :: !([StatusBuild])}
data StatusBuild
    = StatusBuild {statusBuildProject :: !Projector.Html.Runtime.Text,
                   statusBuildBuild :: !Projector.Html.Runtime.Text,
                   statusBuildId :: !Projector.Html.Runtime.Text}

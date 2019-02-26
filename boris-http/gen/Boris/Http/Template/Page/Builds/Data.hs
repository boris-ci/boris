{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Boris.Http.Template.Page.Builds.Data where
import qualified Projector.Html.Runtime
data BuildRef
    = BuildRef {buildRefName :: !Projector.Html.Runtime.Text,
                buildRefBuilds :: !([Projector.Html.Runtime.Text])}
data Builds
    = Builds {buildsProject :: !Projector.Html.Runtime.Text,
              buildsBuild :: !Projector.Html.Runtime.Text,
              buildsQueued :: !([Projector.Html.Runtime.Text]),
              buildsRefs :: !([BuildRef])}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Boris.Http.Template.Page.NotFound where
import qualified Projector.Html.Runtime
import Boris.Http.Template.Data.Build.Data
import Boris.Http.Template.Data.Commit.Data
import Boris.Http.Template.Data.Project.Data
import Boris.Http.Template.Page.Builds.Data
import Boris.Http.Template.Page.Newproject.Data
import Boris.Http.Template.Page.Status.Data
pageNotFound :: Projector.Html.Runtime.Html
pageNotFound = Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "h1") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.textNodeUnescaped " 404 Not Found "),
                                                Projector.Html.Runtime.textNodeUnescaped " ",
                                                Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "p") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.textNodeUnescaped " This could look better. ")]

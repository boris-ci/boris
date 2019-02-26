{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Boris.Http.Template.Page.Error where
import qualified Projector.Html.Runtime
import Boris.Http.Template.Data.Build.Data
import Boris.Http.Template.Data.Commit.Data
import Boris.Http.Template.Data.Project.Data
pageError :: Projector.Html.Runtime.Text ->
             Projector.Html.Runtime.Html
pageError = \error -> Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "h1") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.textNodeUnescaped " 500 server error "),
                                                       Projector.Html.Runtime.textNodeUnescaped " ",
                                                       Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "p") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.textNodeUnescaped " This could look better. "),
                                                       Projector.Html.Runtime.textNodeUnescaped " ",
                                                       Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "p") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.textNodeUnescaped " If you see this raise an issue with error id: ",
                                                                                                                                                                                             Projector.Html.Runtime.textNode error,
                                                                                                                                                                                             Projector.Html.Runtime.textNodeUnescaped " "])]

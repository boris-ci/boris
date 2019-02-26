{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Boris.Http.Template.Page.Dashboard where
import qualified Projector.Html.Runtime
import Boris.Http.Template.Data.Build.Data
import Boris.Http.Template.Data.Commit.Data
import Boris.Http.Template.Data.Project.Data
pageDashboard :: Projector.Html.Runtime.Html
pageDashboard = Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "p") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.textNodeUnescaped " It would be wonderful if this provided some useful information, but for now boris suggests the list of ",
                                                                                                                                                                                       Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "a") (Projector.Html.Runtime.fold [[Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "href") (Projector.Html.Runtime.AttributeValue "/project")]]) (Projector.Html.Runtime.textNodeUnescaped "projects"),
                                                                                                                                                                                       Projector.Html.Runtime.textNodeUnescaped " may be more useful. "]),
                                                 Projector.Html.Runtime.textNodeUnescaped " ",
                                                 Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "p") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.textNodeUnescaped " ",
                                                                                                                                                                                       Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "a") (Projector.Html.Runtime.fold [[Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "href") (Projector.Html.Runtime.AttributeValue "/status")]]) (Projector.Html.Runtime.textNodeUnescaped "Build Statuses"),
                                                                                                                                                                                       Projector.Html.Runtime.textNodeUnescaped " "]),
                                                 Projector.Html.Runtime.textNodeUnescaped " ",
                                                 Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "p") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.textNodeUnescaped " ",
                                                                                                                                                                                       Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "a") (Projector.Html.Runtime.fold [[Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "href") (Projector.Html.Runtime.AttributeValue "/scoreboard")]]) (Projector.Html.Runtime.textNodeUnescaped "Build Scoreboard"),
                                                                                                                                                                                       Projector.Html.Runtime.textNodeUnescaped " "])]

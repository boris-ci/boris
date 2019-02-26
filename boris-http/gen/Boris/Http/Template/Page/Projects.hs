{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Boris.Http.Template.Page.Projects where
import qualified Projector.Html.Runtime
import Boris.Http.Template.Data.Build.Data
import Boris.Http.Template.Data.Commit.Data
import Boris.Http.Template.Data.Project.Data
pageProjects :: [Projector.Html.Runtime.Text] ->
                Projector.Html.Runtime.Html
pageProjects = \projects -> Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "div") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.textNodeUnescaped "Create a ",
                                                                                                                                                                                                     Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "a") (Projector.Html.Runtime.fold [[Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "href") (Projector.Html.Runtime.AttributeValue "/project/new")]]) (Projector.Html.Runtime.textNodeUnescaped "new project")]),
                                                             Projector.Html.Runtime.textNodeUnescaped " ",
                                                             case Projector.Html.Runtime.isEmpty projects of
                                                                 Projector.Html.Runtime.True -> Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.foldHtml [],
                                                                                                                                 Projector.Html.Runtime.textNodeUnescaped " "]
                                                                 Projector.Html.Runtime.False -> Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.textNodeUnescaped " ",
                                                                                                                                  Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "h3") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.textNodeUnescaped "projects"),
                                                                                                                                  Projector.Html.Runtime.textNodeUnescaped " "],
                                                             Projector.Html.Runtime.textNodeUnescaped "  ",
                                                             Projector.Html.Runtime.foldHtml (Projector.Html.Runtime.fmap (\project -> Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "div") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "a") (Projector.Html.Runtime.fold [[Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "href") (Projector.Html.Runtime.AttributeValue (Projector.Html.Runtime.concat ["/project/",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      project]))]]) (Projector.Html.Runtime.textNode project)),
                                                                                                                                                                        Projector.Html.Runtime.textNodeUnescaped " "]) projects)]

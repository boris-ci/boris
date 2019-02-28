{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Boris.Http.Template.Page.Settings where
import qualified Projector.Html.Runtime
import Boris.Http.Template.Data.Build.Data
import Boris.Http.Template.Data.Commit.Data
import Boris.Http.Template.Data.Project.Data
import Boris.Http.Template.Page.Builds.Data
import Boris.Http.Template.Page.Status.Data
pageSettings :: Projector.Html.Runtime.Html
pageSettings = Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "h3") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.textNodeUnescaped "Settings"),
                                                Projector.Html.Runtime.textNodeUnescaped " ",
                                                Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "form") (Projector.Html.Runtime.fold [[Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "action") (Projector.Html.Runtime.AttributeValue "/settings/import")],
                                                                                                                                                    [Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "method") (Projector.Html.Runtime.AttributeValue "post")]]) (Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.textNodeUnescaped " ",
                                                                                                                                                                                                                                                                                                                         Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "button") (Projector.Html.Runtime.fold [[Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "name") (Projector.Html.Runtime.AttributeValue "import")],
                                                                                                                                                                                                                                                                                                                                                                                                                               [Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "class") (Projector.Html.Runtime.AttributeValue "btn")],
                                                                                                                                                                                                                                                                                                                                                                                                                               [Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "type") (Projector.Html.Runtime.AttributeValue "submit")]]) (Projector.Html.Runtime.textNodeUnescaped "Import from GitHub"),
                                                                                                                                                                                                                                                                                                                         Projector.Html.Runtime.textNodeUnescaped " "]),
                                                Projector.Html.Runtime.textNodeUnescaped " ",
                                                Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "a") (Projector.Html.Runtime.fold [[Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "href") (Projector.Html.Runtime.AttributeValue (Projector.Html.Runtime.concat []))]]) (Projector.Html.Runtime.textNodeUnescaped "Create Organisation"),
                                                Projector.Html.Runtime.textNodeUnescaped "   ",
                                                Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "h4") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.textNodeUnescaped "Switch Organisation")]

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Boris.Http.Template.Page.Configure where
import qualified Projector.Html.Runtime
import Boris.Http.Template.Data.Build.Data
import Boris.Http.Template.Data.Commit.Data
pageConfigure :: Projector.Html.Runtime.Html
pageConfigure = Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "p") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.textNodeUnescaped " Welcome to boris, since this is your first time we have to do some configuration... If you don't want to see this screen on first load, set the ",
                                                                                                                                                                                       Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "code") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.textNodeUnescaped "BORIS_TENANCY"),
                                                                                                                                                                                       Projector.Html.Runtime.textNodeUnescaped " environment variable to ",
                                                                                                                                                                                       Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "code") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.textNodeUnescaped "single"),
                                                                                                                                                                                       Projector.Html.Runtime.textNodeUnescaped " or ",
                                                                                                                                                                                       Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "code") (Projector.Html.Runtime.fold []) (Projector.Html.Runtime.textNodeUnescaped "multi"),
                                                                                                                                                                                       Projector.Html.Runtime.textNodeUnescaped ". "]),
                                                 Projector.Html.Runtime.textNodeUnescaped " ",
                                                 Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "form") (Projector.Html.Runtime.fold [[Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "action") (Projector.Html.Runtime.AttributeValue "/configure")],
                                                                                                                                                     [Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "method") (Projector.Html.Runtime.AttributeValue "post")]]) (Projector.Html.Runtime.foldHtml [Projector.Html.Runtime.textNodeUnescaped " ",
                                                                                                                                                                                                                                                                                                                          Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "button") (Projector.Html.Runtime.fold [[Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "name") (Projector.Html.Runtime.AttributeValue "single")],
                                                                                                                                                                                                                                                                                                                                                                                                                                [Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "class") (Projector.Html.Runtime.AttributeValue "btn")],
                                                                                                                                                                                                                                                                                                                                                                                                                                [Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "type") (Projector.Html.Runtime.AttributeValue "submit")]]) (Projector.Html.Runtime.textNodeUnescaped "single tenancy"),
                                                                                                                                                                                                                                                                                                                          Projector.Html.Runtime.textNodeUnescaped " ",
                                                                                                                                                                                                                                                                                                                          Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "button") (Projector.Html.Runtime.fold [[Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "name") (Projector.Html.Runtime.AttributeValue "multi")],
                                                                                                                                                                                                                                                                                                                                                                                                                                [Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "class") (Projector.Html.Runtime.AttributeValue "btn")],
                                                                                                                                                                                                                                                                                                                                                                                                                                [Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "type") (Projector.Html.Runtime.AttributeValue "submit")]]) (Projector.Html.Runtime.textNodeUnescaped "multi tenancy"),
                                                                                                                                                                                                                                                                                                                          Projector.Html.Runtime.textNodeUnescaped " "])]

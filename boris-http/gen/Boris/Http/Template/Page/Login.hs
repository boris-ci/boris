{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Boris.Http.Template.Page.Login where
import qualified Projector.Html.Runtime
import Boris.Http.Template.Data.Build.Data
import Boris.Http.Template.Data.Commit.Data
import Boris.Http.Template.Data.Project.Data
import Boris.Http.Template.Page.Builds.Data
import Boris.Http.Template.Page.Status.Data
pageLogin :: Projector.Html.Runtime.Text ->
             Projector.Html.Runtime.Html
pageLogin = \client -> Projector.Html.Runtime.parentNode (Projector.Html.Runtime.Tag "a") (Projector.Html.Runtime.fold [[Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "href") (Projector.Html.Runtime.AttributeValue (Projector.Html.Runtime.concat ["https://github.com/login/oauth/authorize?scope=user:email%20repo&client_id=",
                                                                                                                                                                                                                                                                              client]))],
                                                                                                                        [Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "class") (Projector.Html.Runtime.AttributeValue "btn")],
                                                                                                                        [Projector.Html.Runtime.Attribute (Projector.Html.Runtime.AttributeKey "type") (Projector.Html.Runtime.AttributeValue "submit")]]) (Projector.Html.Runtime.textNodeUnescaped "log in with github")

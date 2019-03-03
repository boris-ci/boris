{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Boris.Http.Template.Page.Newproject.Data where
import qualified Projector.Html.Runtime
data NewProjectError
    = InvalidNameNewProjectError
    | InvalidRepositoryNewProjectError
    | AlreadyExistsNewProjectError

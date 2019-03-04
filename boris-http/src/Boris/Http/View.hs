{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Boris.Http.View (
    page
  , scoreboard
  , login
  , dashboard
  , newproject
  , settings
  , configure
  , serverError
  , notFound
  , status
  , projects
  , project
  , builds
  , commit
  , build
  , renderPage
  , renderAuthenticated
  , renderUnauthenticated
  , renderScoreboard
  , renderTime
  , renderDuration
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Project
import           Boris.Http.Data
import qualified Boris.Http.Template.Layout.Page as Template
import qualified Boris.Http.Template.Data.Build.Data as Template
import qualified Boris.Http.Template.Data.Commit.Data as Template
import qualified Boris.Http.Template.Data.Project.Data as Template
import qualified Boris.Http.Template.Page.Build as Template
import qualified Boris.Http.Template.Page.Builds as Template
import qualified Boris.Http.Template.Page.Builds.Data as Template
import qualified Boris.Http.Template.Page.Commit as Template
import qualified Boris.Http.Template.Page.Configure as Template
import qualified Boris.Http.Template.Page.Dashboard as Template
import qualified Boris.Http.Template.Page.Error as Template
import qualified Boris.Http.Template.Page.Login as Template
import qualified Boris.Http.Template.Page.Newproject as Template
import qualified Boris.Http.Template.Page.Newproject.Data as Template
import qualified Boris.Http.Template.Page.NotFound as Template
import qualified Boris.Http.Template.Page.Project as Template
import qualified Boris.Http.Template.Page.Projects as Template
import qualified Boris.Http.Template.Page.Scoreboard as Template
import qualified Boris.Http.Template.Page.Settings as Template
import qualified Boris.Http.Template.Page.Status as Template
import qualified Boris.Http.Template.Page.Status.Data as Template
import           Boris.Prelude

import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)


import qualified Projector.Hydrant as Hydrant

import           System.IO (IO)

import qualified Web.Spock.Core as Spock


dashboard :: Hydrant.Html
dashboard =
  Template.pageDashboard

newproject :: Maybe Template.NewProjectError -> Hydrant.Html
newproject err =
  Template.pageNewproject err

settings :: Hydrant.Html
settings =
  Template.pageSettings

configure :: Hydrant.Html
configure =
  Template.pageConfigure

serverError :: ErrorId -> Hydrant.Html
serverError =
  Template.pageError . errorId

notFound :: Hydrant.Html
notFound =
  Template.pageNotFound

status :: [Result] -> Hydrant.Html
status bs =
  Template.pageStatus . Template.Status .
    with (List.sortOn (\b -> (resultProject b, resultBuild b)) bs) $ \b ->
      Template.StatusBuild
        (renderProjectName . resultProject $ b)
        (renderBuildName . resultBuild $ b)
        (renderBuildId . resultBuildId $ b)


projects :: [Keyed ProjectId Project] -> Hydrant.Html
projects ps =
  Template.pageProjects (fmap (renderProjectName . projectName . valueOf) $ ps)

project :: Keyed ProjectId Project -> [Keyed BuildId Build] -> Hydrant.Html
project p bs =
  Template.pageProject $
    Template.Project (renderProjectName . projectName . valueOf $ p) (List.sort . fmap (renderBuildName . buildName . valueOf) $ bs)

builds :: BuildTree -> [BuildId] -> Hydrant.Html
builds (BuildTree p b refs) queued =
  let
    sorted =
      List.reverse $ List.sortOn (\(BuildTreeRef _ is) -> head . sortBuildIds $ is) refs
  in
    Template.pageBuilds $
      Template.Builds
        (renderProjectName p)
        (renderBuildName b)
        (renderBuildId <$> sortBuildIds queued)
        (with sorted $ \(BuildTreeRef r is) ->
          Template.BuildRef (renderRef r) (renderBuildId <$> sortBuildIds is))

commit :: ProjectName -> Commit -> [Keyed BuildId Build] -> Hydrant.Html
commit p c bs =
  let
    byBuild =
      Map.toList . mapFromListGrouped . fmap (\b -> (buildName . valueOf $ b, keyOf b)) $ bs
  in
    Template.pageCommit $
      Template.Commit
        (renderProjectName p)
        (renderCommit c)
        (with byBuild $ \(b, ids) ->
          Template.CommitBuild
          (renderBuildName b)
          (renderBuildId <$> sortBuildIds ids))

build :: Keyed BuildId Build -> Hydrant.Html
build b =
  let
    s =
      case buildResult . valueOf $  b of
        Nothing ->
          Template.BuildUndecided
        Just BuildOk ->
          Template.BuildOk
        Just BuildKo ->
          Template.BuildKo
  in
    Template.pageBuild s $
      Template.Build
        (renderBuildId . keyOf $ b)
        (Template.HasLog)
        (renderProjectName . projectName . valueOf . buildProject . valueOf $ b)
        (renderBuildName . buildName . valueOf $ b)
        (fmap renderRef . buildRef . valueOf $ b)
        (fmap renderCommit . buildCommit . valueOf$ b)
        (fmap renderTime . buildQueueTime . valueOf $ b)
        (fmap renderTime . buildStartTime . valueOf $ b)
        (fmap renderTime . buildEndTime . valueOf $ b)
        (fmap renderTime . buildHeartbeatTime . valueOf $ b)
        (fmap (uncurry renderDuration) $ liftA2 (,) (buildStartTime . valueOf $ b) (buildEndTime . valueOf $ b))
        (fmap renderBuildResult . buildResult . valueOf $ b)
        ((isNothing . buildResult . valueOf $ b) && (buildCancelled . valueOf) b /= (Just BuildCancelled))


mapFromListGrouped :: Ord a => [(a, b)] -> Map a [b]
mapFromListGrouped =
  foldr (\(k, v) -> Map.insertWith (<>) k [v]) Map.empty

login :: GithubClient -> Hydrant.Html
login client =
  Template.pageLogin (githubClient client)

renderTime :: UTCTime -> Text
renderTime =
  Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

renderDuration :: UTCTime -> UTCTime -> Text
renderDuration s e =
  mconcat [Text.pack . show $ ((round (diffUTCTime e s)) :: Integer), "s"]

renderAuthenticated :: AuthenticatedBy -> Hydrant.Html -> Spock.ActionT IO a
renderAuthenticated authentication =
  renderPage (Just authentication)

renderUnauthenticated :: Hydrant.Html -> Spock.ActionT IO a
renderUnauthenticated =
  renderPage Nothing

renderPage :: Maybe AuthenticatedBy -> Hydrant.Html -> Spock.ActionT IO a
renderPage authentication body =
  Spock.html . Hydrant.toText $ page authentication body

page :: Maybe AuthenticatedBy -> Hydrant.Html -> Hydrant.Html
page authenticattion body =
  mconcat [
      Hydrant.doctype "html"
    , Template.layoutPage (renderAuthenticatedBy <$> authenticattion) body
    ]

renderScoreboard :: Bool -> Spock.ActionT IO a
renderScoreboard =
  Spock.html . Hydrant.toText . scoreboard

scoreboard :: Bool -> Hydrant.Html
scoreboard ok =
  mconcat [
      Hydrant.doctype "html"
    , Template.pageScoreboard (Hydrant.AttributeValue $ bool "notOk" "ok" ok)
    ]

renderAuthenticatedBy :: AuthenticatedBy -> Text
renderAuthenticatedBy a =
  case a of
    AuthenticatedByGithub _ u ->
      githubLogin . githubUserLogin . userOf $ u
    AuthenticatedByDesign _ ->
      "boris"

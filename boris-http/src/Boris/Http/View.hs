{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Boris.Http.View (
    dashboard
  , newproject
  , configure
  , status
  , projects
  , project
  , builds
  , commit
  , build
  , serverError
  , scoreboard
  , settings
  , login
  , render
  ) where

import           BMX (BMXError, renderBMXError)
import           BMX (BMXState, Template, partialFromTemplate, renderPage, renderTemplate, templateFile)
import           BMX (BMXValue (..), defaultState, usingContext, usingPartials)

import           Boris.Core.Data.Build
import           Boris.Core.Data.Project
import           Boris.Http.Data

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)
import qualified System.IO as IO


import qualified Web.Spock.Core as Spock

render :: Either BMXError Text -> Spock.ActionT IO a
render x =
  case x of
    Left err -> do
      eid <- liftIO newErrorId
      liftIO $ Text.hPutStrLn IO.stderr (mconcat [errorId eid, " ", "Template error: ", renderBMXError err])
      Spock.setStatus HTTP.status500
      case serverError eid of
        Left err' -> do
          liftIO $ Text.hPutStrLn IO.stderr (mconcat [errorId eid, " ", "Template can't even error: ", renderBMXError err'])
          Spock.html $ "Couldn't even render the 500 page, take this as a serious issue: " <> errorId eid
        Right content ->
          Spock.html content
    Right content ->
      Spock.html content


dashboard :: AuthenticatedBy -> Either BMXError Text
dashboard a =
  let
    context = [
      ]
  in
    renderPage <$> renderTemplate (authenticated a context) dashboard'

newproject :: AuthenticatedBy -> Either BMXError Text
newproject a =
  let
    context = [
      ]
  in
    renderPage <$> renderTemplate (authenticated a context) newproject'

settings :: AuthenticatedBy -> Either BMXError Text
settings a =
  let
    context = [
      ]
  in
    renderPage <$> renderTemplate (authenticated a context) settings'

configure :: Either BMXError Text
configure =
  let
    context = [
      ]
  in
    renderPage <$> renderTemplate (unauthenticated `usingContext` context) configure'

-- FIX error id
serverError :: ErrorId -> Either BMXError Text
serverError e =
  let
    context = [
        ("error", BMXString . errorId $ e)
      ]
  in
    renderPage <$> renderTemplate (unauthenticated `usingContext` context) serverError'

status :: AuthenticatedBy -> [Result] -> Either BMXError Text
status a bs =
  let
    buildSort b = (resultProject b, resultBuild b)
    context = [
        (,) "builds" $ BMXList . flip fmap (sortOn buildSort bs) $ \b ->
          BMXContext [
              (,) "project" (BMXString . renderProject $ resultProject b)
            , (,) "build" (BMXString . renderBuild $ resultBuild b)
            , (,) "id" (BMXString . renderBuildId $ resultBuildId b)
            ]
      ]
  in
    renderPage <$> renderTemplate (authenticated a context) status'


scoreboard :: AuthenticatedBy -> [Result] -> Either BMXError Text
scoreboard a bs =
  let
    allOk = all ((== BuildOk) . resultBuildResult) bs
    buildClass = case allOk of
      False -> "notOk"
      True -> "ok"
    context = [
        (,) "buildClass" $ BMXString buildClass
      ]
  in
    renderPage <$> renderTemplate (authenticated a context) scoreboard'

projects :: AuthenticatedBy -> [Project] -> Either BMXError Text
projects a p =
  let
    context = [
        ("projects", BMXList ((BMXString . renderProject) <$> sortOn renderProject p))
      ]
  in
    renderPage <$> renderTemplate (authenticated a context) projects'

project :: AuthenticatedBy -> Project -> [Build] -> Either BMXError Text
project a p bs =
  let
    context = [
        ("project", BMXString (renderProject p))
      , ("builds", BMXList ((BMXString . renderBuild) <$> sortOn renderBuild bs))
      ]
  in
    renderPage <$> renderTemplate (authenticated a context) project'

builds :: AuthenticatedBy -> BuildTree -> [BuildId] -> Either BMXError Text
builds a (BuildTree p b refs) queued =
  let
    sorted =
      reverse $ sortOn (\(BuildTreeRef _ is) -> head . sortBuildIds $ is) refs

    toRef (BuildTreeRef r is) = [
        ("name", BMXString $ renderRef r)
      , ("builds", BMXList $ (BMXString . renderBuildId) <$> sortBuildIds is)
      ]

    context = [
        ("project", BMXString (renderProject p))
      , ("build", BMXString (renderBuild b))
      , ("refs", BMXList $ fmap (BMXContext . toRef) sorted)
      , ("queued", BMXList ((BMXString . renderBuildId) <$> sortBuildIds queued))
      ]
  in
    renderPage <$> renderTemplate (authenticated a context) builds'

commit :: AuthenticatedBy -> Project -> Commit -> [BuildData] -> Either BMXError Text
commit a p c bs =
  let
    byBuild =
      M.toList . mapFromListGrouped . fmap (\b -> (buildDataBuild b, buildDataId b)) $ bs
    context = [
        ("project", BMXString (renderProject p))
      , ("commit", BMXString (renderCommit c))
      , ("builds", BMXList . flip fmap byBuild $ (\(b, ids) -> BMXContext [
          ("name", BMXString $ renderBuild b)
        , ("ids", BMXList . fmap (BMXString . renderBuildId) . sortBuildIds $ ids)
        ]))
      ]
  in
    renderPage <$> renderTemplate (authenticated a context) commit'

build :: AuthenticatedBy -> BuildData -> Either BMXError Text
build a b =
  let
    context = [
        ("project", BMXString (renderProject . buildDataProject $ b))
      , ("build", BMXString (renderBuild . buildDataBuild $ b))
      , ("id", BMXString (renderBuildId . buildDataId $ b))
      , ("ref", maybe BMXNull (BMXString . renderRef) . buildDataRef $ b)
      , ("commit", maybe BMXNull (BMXString . renderCommit) . buildDataCommit $ b)
      , ("queued", maybe BMXNull (BMXString . renderTime) . buildDataQueueTime $ b)
      , ("started", maybe BMXNull (BMXString . renderTime) . buildDataStartTime $ b)
      , ("ended", maybe BMXNull (BMXString . renderTime) . buildDataEndTime $ b)
      , ("hearbeat", maybe BMXNull (BMXString . renderTime) . buildDataHeartbeatTime $ b)
      , ("duration", maybe BMXNull (BMXString . uncurry renderDuration) $ liftA2 (,) (buildDataStartTime b) (buildDataEndTime b))
      , ("result", maybe BMXNull (BMXString . renderBuildResult) $ buildDataResult b)
      , ("ok", maybe BMXNull (BMXBool . (==) BuildOk) $ buildDataResult b)
      , ("ko", maybe BMXNull (BMXBool . (==) BuildKo) $ buildDataResult b)
      , ("undecided", maybe (BMXBool True) (const $ BMXNull) $ buildDataResult b)
      , ("cancel", BMXBool ((isNothing . buildDataResult $ b) && (notCancelled (buildDataCancelled b))))
      ]
  in
    renderPage <$> renderTemplate (authenticated a context) build'

  where
    notCancelled :: Maybe BuildCancelled -> Bool
    notCancelled r = case r of
                       Just BuildNotCancelled -> True
                       _ -> False

login :: GithubClient -> Either BMXError Text
login client =
  let
    context = [
       ("client", BMXString (githubClient client))
     ]
  in
    renderPage <$> renderTemplate (unauthenticated `usingContext` context) login'

renderTime :: UTCTime -> Text
renderTime =
  Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

renderDuration :: UTCTime -> UTCTime -> Text
renderDuration s e =
  mconcat [Text.pack . show $ ((round (diffUTCTime e s)) :: Integer), "s"]

mapFromListGrouped :: Ord a => [(a, b)] -> Map a [b]
mapFromListGrouped =
  foldr (\(k, v) -> M.insertWith (<>) k [v]) M.empty

dashboard' :: Template
dashboard' =
  $(templateFile "template/dashboard.hbs")

configure' :: Template
configure' =
  $(templateFile "template/configure.hbs")

status' :: Template
status' =
  $(templateFile "template/status.hbs")

projects' :: Template
projects' =
  $(templateFile "template/projects.hbs")

project' :: Template
project' =
  $(templateFile "template/project.hbs")

builds' :: Template
builds' =
  $(templateFile "template/builds.hbs")

commit' :: Template
commit' =
  $(templateFile "template/commit.hbs")

build' :: Template
build' =
  $(templateFile "template/build.hbs")

serverError' :: Template
serverError' =
  $(templateFile "template/server-error.hbs")

scoreboard' :: Template
scoreboard' =
  $(templateFile "template/scoreboard.hbs")

login' :: Template
login' =
  $(templateFile "template/login.hbs")

frame' :: Template
frame' =
  $(templateFile "template/frame.hbs")

frameAuthenticated' :: Template
frameAuthenticated' =
  $(templateFile "template/frame-authenticated.hbs")

newproject' :: Template
newproject' =
  $(templateFile "template/newproject.hbs")

settings' :: Template
settings' =
  $(templateFile "template/settings.hbs")

unauthenticated :: (Applicative m, Monad m) => BMXState m
unauthenticated =
  defaultState `usingPartials` [("frame", partialFromTemplate frame')]

authenticated :: (Applicative m, Monad m) => AuthenticatedBy -> [(Text, BMXValue)] -> BMXState m
authenticated a context =
  defaultState
    `usingPartials` [("frame", partialFromTemplate frameAuthenticated')]
    `usingContext` (join [context, [("username",
       case a of
         AuthenticatedByGithub _ u ->
           BMXString . githubLogin . githubUserLogin . userOf $ u
         AuthenticatedByDesign _ ->
           BMXString "boris")]])

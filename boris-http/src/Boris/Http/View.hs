{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Boris.Http.View (
    dashboard
  , status
  , projects
  , project
  , builds
  , commit
  , build
  , serverError
  , scoreboard
  , login
  , render
  ) where

import           BMX (BMXError, renderBMXError)
import           BMX (Template, renderPage, renderTemplate, templateFile)
import           BMX (BMXValue (..), defaultState, usingContext)

import           Boris.Core.Data (Project (..), Build (..), Commit (..), Ref (..), BuildId (..), BuildCancelled (..), Result (..), BuildData (..), BuildResult (..), sortBuildIds, renderBuildResult, BuildTree (..), BuildTreeRef (..))
import           Boris.Http.Data

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Time (UTCTime, diffUTCTime, formatTime, defaultTimeLocale)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

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


dashboard :: Either BMXError Text
dashboard =
  let
    context = [
      ]
  in
    renderPage <$> renderTemplate (defaultState `usingContext` context) dashboard'

-- FIX error id
serverError :: ErrorId -> Either BMXError Text
serverError e =
  let
    context = [
        ("error", BMXString . errorId $ e)
      ]
  in
    renderPage <$> renderTemplate (defaultState `usingContext` context) serverError'

status :: [Result] -> Either BMXError Text
status bs =
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
    renderPage <$> renderTemplate (defaultState `usingContext` context) status'


scoreboard :: [Result] -> Either BMXError Text
scoreboard bs =
  let
    allOk = all ((== BuildOk) . resultBuildResult) bs
    buildClass = case allOk of
      False -> "notOk"
      True -> "ok"
    context = [
        (,) "buildClass" $ BMXString buildClass
      ]
  in
    renderPage <$> renderTemplate (defaultState `usingContext` context) scoreboard'

projects :: [Project] -> Either BMXError Text
projects p =
  let
    context = [
        ("projects", BMXList ((BMXString . renderProject) <$> sortOn renderProject p))
      ]
  in
    renderPage <$> renderTemplate (defaultState `usingContext` context) projects'

project :: Project -> [Build] -> Either BMXError Text
project p bs =
  let
    context = [
        ("project", BMXString (renderProject p))
      , ("builds", BMXList ((BMXString . renderBuild) <$> sortOn renderBuild bs))
      ]
  in
    renderPage <$> renderTemplate (defaultState `usingContext` context) project'

builds :: BuildTree -> [BuildId] -> Either BMXError Text
builds (BuildTree p b refs) queued =
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
    renderPage <$> renderTemplate (defaultState `usingContext` context) builds'

commit :: Project -> Commit -> [BuildData] -> Either BMXError Text
commit p c bs =
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
    renderPage <$> renderTemplate (defaultState `usingContext` context) commit'

build :: BuildData -> Either BMXError Text
build b =
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
      , ("log", maybe BMXNull (const $ BMXBool True) $ buildDataLog b)
      , ("cancel", BMXBool ((isNothing . buildDataResult $ b) && (notCancelled (buildDataCancelled b))))
      ]
  in
    renderPage <$> renderTemplate (defaultState `usingContext` context) build'

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
    renderPage <$> renderTemplate (defaultState `usingContext` context) login'

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

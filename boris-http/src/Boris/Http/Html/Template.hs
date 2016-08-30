{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Boris.Http.Html.Template (
    dashboard
  , projects
  , project
  , builds
  , commit
  , build
  , render
  ) where

import           Airship (Webmachine, ResponseBody (..))

import           Blaze.ByteString.Builder.ByteString (fromByteString)

import           BMX (BMXError, renderBMXError)
import           BMX (Template, renderPage, renderTemplate, templateFile)
import           BMX (BMXValue (..), defaultState, usingContext)

import           Boris.Core.Data (Project (..), Build (..), Commit (..), Ref (..), BuildId (..), BuildResult (..), sortBuildIds)
import           Boris.Store.Build (BuildData (..), BuildCancelled(..))
import           Boris.Http.Airship (webT)
import           Boris.Http.Data (ClientLocale (..))
import           Boris.Queue (QueueSize (..))

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Time (UTCTime, ZonedTime(..), diffUTCTime, formatTime, defaultTimeLocale)
import           Data.Time.Zones (utcToLocalTimeTZ, timeZoneForUTCTime)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (hoistEither)


render :: Either BMXError Text -> Webmachine IO ResponseBody
render t = do
  c <- webT renderBMXError . hoistEither $ t
  return . ResponseBuilder . fromByteString . T.encodeUtf8 $ c


dashboard :: [BuildData] -> QueueSize -> Either BMXError Text
dashboard bs s =
  let
    buildSort b = (buildDataProject b, buildDataBuild b)
    context = [
        (,) "builds" $ BMXList . flip fmap (sortOn buildSort bs) $ \b ->
          BMXContext [
              (,) "project" (BMXString . renderProject $ buildDataProject b)
            , (,) "build" (BMXString . renderBuild $ buildDataBuild b)
            , (,) "id" (BMXString . renderBuildId $ buildDataId b)
            ]
       , (,) "size" $ BMXNum . fromIntegral . getQueueSize $ s
      ]
  in
    renderPage <$> renderTemplate (defaultState `usingContext` context) dashboard'

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

builds :: Project -> Build -> [(Ref, [BuildId])] -> [BuildId] -> Either BMXError Text
builds p b rs queued =
  let
    sorted =
      reverse $ sortOn (\(_, is) -> head . sortBuildIds $ is) rs

    toRef r is = [
        ("name", BMXString $ renderRef r)
      , ("builds", BMXList $ (BMXString . renderBuildId) <$> sortBuildIds is)
      ]

    context = [
        ("project", BMXString (renderProject p))
      , ("build", BMXString (renderBuild b))
      , ("refs", BMXList $ fmap (BMXContext . uncurry toRef) sorted)
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

build :: ClientLocale -> BuildData -> Either BMXError Text
build l b =
  let
    context = [
        ("project", BMXString (renderProject . buildDataProject $ b))
      , ("build", BMXString (renderBuild . buildDataBuild $ b))
      , ("id", BMXString (renderBuildId . buildDataId $ b))
      , ("ref", maybe BMXNull (BMXString . renderRef) . buildDataRef $ b)
      , ("commit", maybe BMXNull (BMXString . renderCommit) . buildDataCommit $ b)
      , ("queued", maybe BMXNull (BMXString . renderTime l) . buildDataQueueTime $ b)
      , ("started", maybe BMXNull (BMXString . renderTime l) . buildDataStartTime $ b)
      , ("ended", maybe BMXNull (BMXString . renderTime l) . buildDataEndTime $ b)
      , ("hearbeat", maybe BMXNull (BMXString . renderTime l) . buildDataHeartbeatTime $ b)
      , ("duration", maybe BMXNull (BMXString . uncurry renderDuration) $ liftA2 (,) (buildDataStartTime b) (buildDataEndTime b))
      , ("result", maybe BMXNull (BMXString . renderBuildResult) $ buildDataResult b)
      , ("ok", maybe BMXNull (BMXBool . (==) BuildOk) $ buildDataResult b)
      , ("ko", maybe BMXNull (BMXBool . (==) BuildKo) $ buildDataResult b)
      , ("undecided", maybe (BMXBool True) (const $ BMXNull) $ buildDataResult b)
      , ("log", maybe BMXNull (const $ BMXBool True) $ buildDataLog b)
      , ("rebuild", case buildDataResult b of { Just (BuildKo) -> BMXBool True; _ -> BMXBool False })
      , ("cancel", BMXBool ((isNothing . buildDataResult $ b) && (notCancelled (buildDataCancelled b))))
      ]
  in
    renderPage <$> renderTemplate (defaultState `usingContext` context) build'

  where
    notCancelled :: Maybe BuildCancelled -> Bool
    notCancelled r = case r of
                       Just BuildNotCancelled -> True
                       _ -> False

renderTime :: ClientLocale -> UTCTime -> Text
renderTime (ClientLocale tz) t =
  T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" $ ZonedTime (utcToLocalTimeTZ tz t) (timeZoneForUTCTime tz t)

renderDuration :: UTCTime -> UTCTime -> Text
renderDuration s e =
  mconcat [T.pack . show $ ((round (diffUTCTime e s)) :: Integer), "s"]

renderBuildResult :: BuildResult -> Text
renderBuildResult r =
  case r of
    BuildOk ->
      "ok"
    BuildKo ->
      "ko"

mapFromListGrouped :: Ord a => [(a, b)] -> Map a [b]
mapFromListGrouped =
  foldr (\(k, v) -> M.insertWith (<>) k [v]) M.empty

dashboard' :: Template
dashboard' =
  $(templateFile "template/dashboard.hbs")

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

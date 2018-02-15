{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Boris.Http.Route (
    route
  ) where

import           Blaze.ByteString.Builder (fromByteString)

import           Boris.Core.Data
import qualified Boris.Http.Api.Build as Build
import qualified Boris.Http.Api.Discover as Discover
import qualified Boris.Http.Api.Project as Project
import qualified Boris.Http.Api.Result as Result
import qualified Boris.Http.Api.Session as Session
import           Boris.Http.Boot
import           Boris.Http.Data
import           Boris.Http.Spock
import           Boris.Http.Store.Data
import qualified Boris.Http.Store.Error as Store
import qualified Boris.Http.View as View
import qualified Boris.Representation.ApiV1 as ApiV1

import           Data.Aeson (object, (.=))
import           Data.ByteString.Char8 (unlines)
import           Data.Conduit (runConduit, (=$=))
import qualified Data.Conduit.List as CL
import           Data.Default (def)
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TextIO

import           P

import           System.IO (IO, putStrLn)

-- spock experiment
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.Wai.Middleware.Static as Static
import qualified Network.Wai.Middleware.StaticEmbedded as StaticEmbedded
import           Web.Spock.Core ((<//>))
import qualified Web.Spock.Core as Spock


route :: Store -> AuthenticationMode -> BuildService -> LogService -> ProjectMode -> Mode -> Spock.SpockT IO ()
route store authentication buildx logx projectx mode = do
  case mode of
    DevelopmentMode -> do
      Spock.middleware RequestLogger.logStdoutDev
      Spock.middleware . Static.staticPolicy $ Static.hasPrefix "assets"

    TestMode -> do
      Spock.middleware . StaticEmbedded.static $ $(FileEmbed.embedDir "assets")

    ProductionMode -> do
      Spock.middleware RequestLogger.logStdout
      Spock.middleware . StaticEmbedded.static $ $(FileEmbed.embedDir "assets")

  Spock.get Spock.root $ do
    withAuthentication authentication store $ \a -> case a of
      Authenticated _ _ ->
        View.render View.dashboard
      AuthenticatedNone ->
        View.render View.dashboard
      NotAuthenticated -> do
        Spock.redirect "/login"
      WasAuthenticated _ -> do
        Spock.redirect "/login"

  Spock.get "login" $ do
    case authentication of
      GithubAuthentication _manager client _secret ->
        View.render $ View.login client
      NoAuthentication -> do
        Spock.redirect "/"

  Spock.get "logout" $ do
    killSession
    Spock.redirect "/"

  Spock.get "login/callback" $ do
    case authentication of
      GithubAuthentication manager client secret -> do
        code' <- fmap GithubCode <$> Spock.param "code"
        case code' of
          Nothing -> do
            killSession
            Spock.setStatus HTTP.notFound404
            Spock.html "TODO: 404 page."
          Just code -> do
            session <- liftError (Session.renderAuthenticationError) $
              Session.authenticate store manager client secret code
            newSession mode (sessionId session)
            Spock.redirect "/"
      NoAuthentication -> do
        Spock.redirect $ "/"

  Spock.get "status" $
    authenticated authentication store $ \_ -> do
      builds <- liftStoreError $ Result.status store
      View.render $ View.status builds

  Spock.get "project" $
    authenticated authentication store $ \_ -> do
      projects <- liftError Project.renderConfigError $
        Project.list projectx
      withAccept $ \case
        AcceptHTML ->
          View.render $ View.projects projects
        AcceptJSON ->
          Spock.json $ ApiV1.GetProjects projects

  Spock.get ("project" <//> Spock.var) $ \project ->
    authenticated authentication store $ \_ -> do
      builds <- liftStoreError $ Build.byProject store (Project project)
      withAccept $ \case
        AcceptHTML ->
          View.render $ View.project (Project project) builds
        AcceptJSON ->
          Spock.json $ ApiV1.GetProject (Project project) builds

  Spock.post ("project" <//> Spock.var) $ \project ->
    authenticated authentication store $ \_ -> do
      buildId <- liftError id $
        Project.discover store buildx projectx (Project project)
      case buildId of
        Nothing -> do
          -- TODO should have a body
          Spock.setStatus HTTP.status404
          Spock.json ()
        Just _i -> do
          -- TODO this matches old behaviour but should be a 201 with location and better body
          Spock.setStatus HTTP.status202
          Spock.json ()

  Spock.get ("project" <//> Spock.var <//> "build" <//> Spock.var) $ \project' build'  ->
    authenticated authentication store $ \_ -> do
      let
        project = Project project'
        build = Build build'

      builds <- liftStoreError $ Build.list store project build
      withAccept $ \case
        AcceptHTML -> do
          queued <- liftStoreError $ Build.queued store project build
          View.render $ View.builds builds queued
        AcceptJSON ->
          Spock.json $ ApiV1.GetBuilds builds


  Spock.post ("project" <//> Spock.var <//> "build" <//> Spock.var) $ \project' build' ->
    authenticated authentication store $ \_ -> do
      let
        project = Project project'
        build = Build build'

      withContentType $ \content ->
        case content of
          ContentTypeForm -> do
            ref <- fmap Ref <$> Spock.param "ref"
            i <- liftError Build.renderBuildError $
              Build.submit store buildx projectx project build ref
            case i of
              Nothing -> do
                Spock.setStatus HTTP.notFound404
                Spock.html "TODO: 404 page."
              Just ii ->
                Spock.redirect $ "/build/" <> renderBuildId ii
          ContentTypeJSON -> do
            e <- Spock.jsonBody
            case e of
              Nothing -> do
                -- FIX unhax
                Spock.setStatus HTTP.status400
                Spock.json $ object ["error" .= ("could not parse ref." :: Text)]
              Just (ApiV1.PostBuildRequest ref) -> do
                i <- liftError Build.renderBuildError $
                  Build.submit store buildx projectx project build ref
                case i of
                  Nothing ->
                    Spock.setStatus HTTP.notFound404
                  Just ii -> do
                    Spock.setStatus HTTP.created201
                    Spock.setHeader "Location" $ "/build/" <> renderBuildId ii
                    Spock.json $ ApiV1.GetBuild (BuildData ii project build ref Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)


  Spock.get ("project" <//> Spock.var <//> "commit" <//> Spock.var) $ \project' commit' ->
    authenticated authentication store $ \_ -> do
      let
        project = Project project'
        commit = Commit commit'

      builds <- liftStoreError $ Build.byCommit store project commit
      withAccept $ \case
        AcceptHTML -> do
          datas <- for builds $ liftError Store.renderFetchError . Build.byId store
          View.render $ View.commit project commit (catMaybes datas)
        AcceptJSON ->
          Spock.json $ ApiV1.GetCommit project builds


  Spock.post ("discover" <//> Spock.var) $ \discoverId' ->
    authenticated authentication store $ \_ -> do

      let
        discoverId = BuildId discoverId'

      withContentType $ \content ->
        case content of
          ContentTypeForm -> do
            -- FIX 400?
            Spock.setStatus HTTP.notFound404
            Spock.html "TODO: 404 page."
          ContentTypeJSON -> do
            e <- Spock.jsonBody
            case e of
              Nothing -> do
                -- FIX unhax
                Spock.setStatus HTTP.status400
                Spock.json $ object ["error" .= ("could not parse ref." :: Text)]
              Just (ApiV1.PostDiscover project guts) -> do
                liftError Discover.renderCompleteError $
                  Discover.complete store buildx projectx discoverId project guts
                Spock.setStatus HTTP.ok200
                Spock.json ()

  Spock.get ("build" <//> Spock.var) $ \buildId' ->
    authenticated authentication store $ \_ -> do
      let
        buildId = BuildId buildId'

      build <- liftError Store.renderFetchError $
        Build.byId store buildId
      withAccept $ \case
        AcceptHTML -> do
          case build of
            Nothing -> do
              Spock.setStatus HTTP.notFound404
              Spock.html "TODO: 404 page."
            Just b ->
              View.render $ View.build b
        AcceptJSON ->
          case build of
            Nothing -> do
              Spock.setStatus HTTP.notFound404
              Spock.json ()
            Just b ->
              Spock.json $ ApiV1.GetBuild b

  Spock.delete ("build" <//> Spock.var) $ \buildId' ->
    authenticated authentication store $ \_ -> do
      let
        buildId = BuildId buildId'

      result <- liftError Store.renderFetchError $
        Build.cancel store buildId

      withAccept $ \case
        AcceptHTML -> do
          case result of
            Nothing -> do
              Spock.setStatus HTTP.notFound404
              Spock.html "TODO: 404 page."
            Just _ -> do
              Spock.redirect $ "/build/" <> renderBuildId buildId
        AcceptJSON ->
          case result of
            Nothing -> do
              Spock.setStatus HTTP.notFound404
              Spock.json ()
            Just _ -> do
              Spock.setStatus HTTP.noContent204

  Spock.post ("build" <//> Spock.var <//> "heartbeat") $ \buildId' ->
    authenticated authentication store $ \_ -> do
      let
        buildId = BuildId buildId'

      withContentType $ \content ->
        case content of
          ContentTypeForm -> do
            -- FIX 400?
            Spock.setStatus HTTP.notFound404
            Spock.html "TODO: 404 page."
          ContentTypeJSON -> do
            r <- liftStoreError $
              Build.heartbeat store buildId
            Spock.setHeader "Location" $ "/build/" <> renderBuildId buildId
            Spock.json $ ApiV1.PostHeartbeatResponse r

  Spock.post ("build" <//> Spock.var <//> "acknowledge") $ \buildId' ->
    authenticated authentication store $ \_ -> do

      let
        buildId = BuildId buildId'

      withContentType $ \content ->
        case content of
          ContentTypeForm -> do
            -- FIX 400?
            Spock.setStatus HTTP.notFound404
            Spock.html "TODO: 404 page."
          ContentTypeJSON -> do
            a <- liftStoreError $
              Build.acknowledge store buildId
            Spock.setHeader "Location" $ "/build/" <> renderBuildId buildId
            Spock.json $ ApiV1.PostAcknowledgeResponse a

  Spock.post ("build" <//> Spock.var <//> "avow") $ \buildId' ->
    authenticated authentication store $ \_ -> do

      let
        buildId = BuildId buildId'

      withContentType $ \content ->
        case content of
          ContentTypeForm -> do
            -- FIX 400?
            Spock.setStatus HTTP.notFound404
            Spock.html "TODO: 404 page."
          ContentTypeJSON -> do
            e <- Spock.jsonBody
            case e of
              Nothing -> do
                -- FIX unhax
                Spock.setStatus HTTP.status400
                Spock.json $ object ["error" .= ("could not parse ref." :: Text)]
              Just (ApiV1.PostAvowRequest ref commit) -> do
                liftStoreError $
                  Build.avow store buildId ref commit
                Spock.setHeader "Location" $ "/build/" <> renderBuildId buildId
                Spock.json $ ApiV1.PostAvowResponse

  Spock.post ("build" <//> Spock.var <//> "complete") $ \buildId' ->
    authenticated authentication store $ \_ -> do
      let
        buildId = BuildId buildId'

      withContentType $ \content ->
        case content of
          ContentTypeForm -> do
            -- FIX 400?
            Spock.setStatus HTTP.notFound404
            Spock.html "TODO: 404 page."
          ContentTypeJSON -> do
            e <- Spock.jsonBody
            case e of
              Nothing -> do
                -- FIX unhax
                Spock.setStatus HTTP.status400
                Spock.json $ object ["error" .= ("could not parse ref." :: Text)]
              Just (ApiV1.PostCompleteRequest result) -> do
                liftStoreError $
                  Build.complete store buildId result
                Spock.setHeader "Location" $ "/build/" <> renderBuildId buildId
                Spock.json $ ApiV1.PostCompleteResponse


  Spock.post ("build" <//> Spock.var <//> "cancel") $ \buildId' ->
    authenticated authentication store $ \_ -> do
      let
        buildId = BuildId buildId'

      result <- liftError Store.renderFetchError $
        Build.cancel store buildId

      withContentType $ \content ->
        case content of
          ContentTypeForm -> do
            case result of
              Nothing -> do
                Spock.setStatus HTTP.notFound404
                Spock.html "TODO: 404 page."
              Just _ -> do
                Spock.redirect $ "/build/" <> renderBuildId buildId
          ContentTypeJSON -> do
            case result of
              Nothing -> do
                Spock.setStatus HTTP.notFound404
                Spock.json ()
              Just _ -> do
                Spock.setStatus HTTP.noContent204

  Spock.get "scoreboard" $
    authenticated authentication store $ \_ -> do
      results <- liftStoreError $
        Result.scoreboard store
      withAccept $ \case
        AcceptHTML ->
          View.render $ View.scoreboard results
        AcceptJSON ->
          Spock.json $ ApiV1.GetScoreboard results

-- TODO   [("text/plain; charset=utf-8", r maxBound)] <> withVersion' r
  Spock.get ("log" <//> Spock.var) $ \buildId ->
    authenticated authentication store $ \_ -> do

      log'' <- liftError Store.renderFetchError $
        Build.logOf' store logx (BuildId buildId)

      withAccept $ \case
        AcceptHTML -> do
          Spock.setStatus HTTP.notFound404
          Spock.html "TODO: 404 page."
--          case log'' of
--            Nothing -> do
--              Spock.setStatus HTTP.notFound404
--              Spock.html "TODO: 404 page."
--            Just b ->
--              View.render $ View.build b
        AcceptJSON -> do
          case log'' of
            Nothing -> do
              Spock.setStatus HTTP.notFound404
              Spock.json ()
            Just logs -> do
              Spock.json $ ApiV1.GetLogs (Text.intercalate "\n" logs)
--              Spock.stream $ \send flush ->
--                runConduit $
--                  logs =$=
--                    CL.mapM_ (\t -> send (fromByteString t) >> flush)


newSession :: Mode -> SessionId -> Spock.ActionT IO ()
newSession mode session =
  Spock.setCookie sessionName (getSessionId session) (Spock.defaultCookieSettings {
      Spock.cs_EOL = Spock.CookieValidForSession
    , Spock.cs_HTTPOnly = True
    , Spock.cs_secure = (mode == ProductionMode)
    , Spock.cs_domain = Nothing
    , Spock.cs_path = Just "/"
    })

killSession :: Spock.ActionT IO ()
killSession =
  Spock.deleteCookie sessionName

sessionName :: Text
sessionName =
  "boris"


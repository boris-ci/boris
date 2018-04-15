{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Boris.Http.Route (
    route
  ) where

import           Boris.Core.Data
import qualified Boris.Http.Api.Build as Build
import qualified Boris.Http.Api.Discover as Discover
import qualified Boris.Http.Api.Project as Project
import qualified Boris.Http.Api.Result as Result
import qualified Boris.Http.Api.Session as Session
import           Boris.Http.Boot
import           Boris.Http.Data
import           Boris.Http.Spock
import qualified Boris.Http.View as View
import qualified Boris.Representation.ApiV1 as ApiV1

import           Data.Aeson (object, (.=))
import qualified Data.FileEmbed as FileEmbed

import           P

import           System.IO (IO)

import           Traction.Control (DbPool)
import qualified Traction.Control as Traction

-- spock experiment
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.Wai.Middleware.Static as Static
import qualified Network.Wai.Middleware.StaticEmbedded as StaticEmbedded
import           Web.Spock.Core ((<//>))
import qualified Web.Spock.Core as Spock


route :: DbPool -> AuthenticationMode -> BuildService -> LogService -> ProjectMode -> Mode -> Spock.SpockT IO ()
route pool authentication buildx logx projectx mode = do
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
    withAuthentication authentication pool $ \a -> case a of
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
              Session.authenticate pool manager client secret code
            newSession mode (sessionIdentifier session)
            Spock.redirect "/"
      NoAuthentication -> do
        Spock.redirect $ "/"

  Spock.get "status" $
    authenticated authentication pool $ \_ -> do
      builds <- liftDbError . Traction.runDb pool $ Result.status
      View.render $ View.status builds

  Spock.get "project" $
    authenticated authentication pool $ \_ -> do
      projects <- liftError Project.renderConfigError $
        Project.list projectx
      withAccept $ \case
        AcceptHTML ->
          View.render $ View.projects projects
        AcceptJSON ->
          Spock.json $ ApiV1.GetProjects projects

  Spock.get ("project" <//> Spock.var) $ \project ->
    authenticated authentication pool $ \_ -> do
      builds <- liftDbError $ Build.byProject pool (Project project)
      withAccept $ \case
        AcceptHTML ->
          View.render $ View.project (Project project) builds
        AcceptJSON ->
          Spock.json $ ApiV1.GetProject (Project project) builds

  Spock.post ("project" <//> Spock.var) $ \project ->
    authenticated authentication pool $ \_ -> do
      buildId <- liftError id $
        Project.discover pool buildx projectx (Project project)
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
    authenticated authentication pool $ \_ -> do
      let
        project = Project project'
        build = Build build'

      builds <- liftDbError $ Build.list pool project build
      withAccept $ \case
        AcceptHTML -> do
          queued <- liftDbError $ Build.queued pool project build
          View.render $ View.builds builds queued
        AcceptJSON ->
          Spock.json $ ApiV1.GetBuilds builds


  Spock.post ("project" <//> Spock.var <//> "build" <//> Spock.var) $ \project' build' ->
    authenticated authentication pool $ \_ -> do
      let
        project = Project project'
        build = Build build'

      withContentType $ \content ->
        case content of
          ContentTypeForm -> do
            ref <- fmap Ref <$> Spock.param "ref"
            i <- liftError Build.renderBuildError $
              Build.submit pool buildx projectx project build ref
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
                  Build.submit pool buildx projectx project build ref
                case i of
                  Nothing ->
                    Spock.setStatus HTTP.notFound404
                  Just ii -> do
                    Spock.setStatus HTTP.created201
                    Spock.setHeader "Location" $ "/build/" <> renderBuildId ii
                    Spock.json $ ApiV1.GetBuild (BuildData ii project build ref Nothing Nothing Nothing Nothing Nothing Nothing Nothing)


  Spock.get ("project" <//> Spock.var <//> "commit" <//> Spock.var) $ \project' commit' ->
    authenticated authentication pool $ \_ -> do
      let
        project = Project project'
        commit = Commit commit'

      builds <- liftDbError $ Build.byCommit pool project commit
      withAccept $ \case
        AcceptHTML -> do
          datas <- for builds $ \i -> liftDbError (Build.byId pool i)
          View.render $ View.commit project commit (catMaybes datas)
        AcceptJSON ->
          Spock.json $ ApiV1.GetCommit project builds


  Spock.post ("discover" <//> Spock.var) $ \discoverId' ->
    authenticated authentication pool $ \_ -> do

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
                  Discover.complete pool buildx projectx discoverId project guts
                Spock.setStatus HTTP.ok200
                Spock.json ()

  Spock.get ("build" <//> Spock.var) $ \buildId' ->
    authenticated authentication pool $ \_ -> do
      let
        buildId = BuildId buildId'

      build <- liftDbError $
        Build.byId pool buildId
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
    authenticated authentication pool $ \_ -> do
      let
        buildId = BuildId buildId'

      result <- liftDbError $
        Build.cancel pool buildId

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
    authenticated authentication pool $ \_ -> do
      let
        buildId = BuildId buildId'

      withContentType $ \content ->
        case content of
          ContentTypeForm -> do
            -- FIX 400?
            Spock.setStatus HTTP.notFound404
            Spock.html "TODO: 404 page."
          ContentTypeJSON -> do
            r <- liftDbError $
              Build.heartbeat pool buildId
            Spock.setHeader "Location" $ "/build/" <> renderBuildId buildId
            Spock.json $ ApiV1.PostHeartbeatResponse r

  Spock.post ("build" <//> Spock.var <//> "acknowledge") $ \buildId' ->
    authenticated authentication pool $ \_ -> do

      let
        buildId = BuildId buildId'

      withContentType $ \content ->
        case content of
          ContentTypeForm -> do
            -- FIX 400?
            Spock.setStatus HTTP.notFound404
            Spock.html "TODO: 404 page."
          ContentTypeJSON -> do
            a <- liftDbError $
              Build.acknowledge pool buildId
            Spock.setHeader "Location" $ "/build/" <> renderBuildId buildId
            Spock.json $ ApiV1.PostAcknowledgeResponse a

  Spock.post ("build" <//> Spock.var <//> "avow") $ \buildId' ->
    authenticated authentication pool $ \_ -> do

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
                liftDbError $
                  Build.avow pool buildId ref commit
                Spock.setHeader "Location" $ "/build/" <> renderBuildId buildId
                Spock.json $ ApiV1.PostAvowResponse

  Spock.post ("build" <//> Spock.var <//> "complete") $ \buildId' ->
    authenticated authentication pool $ \_ -> do
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
                liftDbError $
                  Build.complete pool buildId result
                Spock.setHeader "Location" $ "/build/" <> renderBuildId buildId
                Spock.json $ ApiV1.PostCompleteResponse


  Spock.post ("build" <//> Spock.var <//> "cancel") $ \buildId' ->
    authenticated authentication pool $ \_ -> do
      let
        buildId = BuildId buildId'

      result <- liftDbError $
        Build.cancel pool buildId

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
    authenticated authentication pool $ \_ -> do
      results <- liftDbError . Traction.runDb pool $
        Result.scoreboard
      withAccept $ \case
        AcceptHTML ->
          View.render $ View.scoreboard results
        AcceptJSON ->
          Spock.json $ ApiV1.GetScoreboard results

-- TODO   [("text/plain; charset=utf-8", r maxBound)] <> withVersion' r
  Spock.get ("log" <//> Spock.var) $ \buildId ->
    authenticated authentication pool $ \_ -> do

      log' <- liftDbError $
        Build.logOf pool logx (BuildId buildId)

      withAccept $ \case
        AcceptHTML -> do
          Spock.setStatus HTTP.notFound404
          Spock.html "TODO: 404 page."

        AcceptJSON -> do
          case log' of
            Nothing -> do
              Spock.setStatus HTTP.notFound404
              Spock.json ()
            Just logs -> do
              Spock.json $ ApiV1.GetLogs logs

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

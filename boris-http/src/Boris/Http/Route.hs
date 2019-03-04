{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Boris.Http.Route (
    application
  , configure
  ) where

import           Boris.Core.Data.Build
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Project
import           Boris.Core.Data.Repository
import           Boris.Core.Data.Tenant
import qualified Boris.Http.Api.Build as Build
import qualified Boris.Http.Api.Discover as Discover
import qualified Boris.Http.Api.Github as Github
import qualified Boris.Http.Api.Project as Project
import qualified Boris.Http.Api.Result as Result
import qualified Boris.Http.Api.Session as Session
import           Boris.Http.Boot
import           Boris.Http.Data
import           Boris.Http.Spock
import qualified Boris.Http.View as View
import qualified Boris.Http.Template.Page.Newproject.Data as Template
import qualified Boris.Representation.ApiV1 as ApiV1
import           Boris.Prelude

import           Data.Aeson (object, (.=))
import qualified Data.List as List
import qualified Data.FileEmbed as FileEmbed

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.Wai.Middleware.Static as Static
import qualified Network.Wai.Middleware.StaticEmbedded as StaticEmbedded

import           System.IO (IO)

import           Traction.Control (DbPool)
import qualified Traction.Control as Traction

import           Web.Spock.Core ((<//>))
import qualified Web.Spock.Core as Spock


init :: Mode -> Spock.SpockT IO ()
init mode =
  case mode of
    DevelopmentMode -> do
      Spock.middleware RequestLogger.logStdoutDev
      Spock.middleware . Static.staticPolicy $ Static.hasPrefix "assets"

    TestMode -> do
      Spock.middleware . StaticEmbedded.static $ $(FileEmbed.embedDir "assets")

    ProductionMode -> do
      Spock.middleware RequestLogger.logStdout
      Spock.middleware . StaticEmbedded.static $ $(FileEmbed.embedDir "assets")

configure :: DbPool -> AuthenticationMode -> Mode -> Spock.SpockT IO ()
configure _pool _authentication mode = do
  init mode

  Spock.get "configure" $ do
    View.renderUnauthenticated $ View.configure

  Spock.post "configure" $ do
    error "todo"
    {--
    m <- Spock.param "multi"
    liftDbError $ Traction.runDb pool $ Query.setTenant $
      bool SingleTenant MultiTenant $ isJust (m :: Maybe Text)
    Spock.redirect "/"
  Spock.prehook (do
    settings <- liftDbError . Traction.runDb pool $
      Query.getTenant

    case settings of
      Just _ -> do
        pure ()
      Nothing -> do
        Spock.redirect "/configure") $ route pool authentication mode
--}
application :: DbPool -> AuthenticationMode -> Mode -> Spock.SpockT IO ()
application pool authentication mode = do
  init mode
  route pool authentication mode

route :: DbPool -> AuthenticationMode -> Mode -> Spock.SpockT IO ()
route pool authentication mode = do
  Spock.get Spock.root $ do
    withAuthentication authentication pool $ \a -> case a of
      Authenticated s u ->
        View.renderAuthenticated (AuthenticatedByGithub s u) $ View.dashboard
      AuthenticatedNone u ->
        View.renderAuthenticated (AuthenticatedByDesign u) $ View.dashboard
      NotAuthenticated -> do
        Spock.redirect "/login"
      WasAuthenticated _ -> do
        Spock.redirect "/login"

  Spock.get "login" $ do
    case authentication of
      GithubAuthentication _manager client _secret ->
        View.renderUnauthenticated $ View.login client
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

  Spock.get "settings" $
    authenticated authentication pool $ \a -> case a of
      AuthenticatedByGithub _ _ ->
        View.renderAuthenticated a $ View.settings
      AuthenticatedByDesign _ ->
        Spock.redirect $ "/"


  -- FIX CSRF
  Spock.post "settings/import" $
    authenticated authentication pool $ \a -> case a of
      AuthenticatedByGithub s u -> do
        liftError Github.renderImportError $
          Github.importRepositories pool s u
        Spock.redirect $ "/project"
      AuthenticatedByDesign _ ->
        Spock.redirect $ "/"

  Spock.get "status" $
    authenticated authentication pool $ \a -> do
      builds <- liftDbError . Traction.runDb pool $ Result.status
      View.renderAuthenticated a $ View.status builds

  Spock.get "project" $
    authenticated authentication pool $ \a -> do
      projects <- transaction pool $ Project.list
      withAccept $ \case
        AcceptHTML ->
          View.renderAuthenticated a $ View.projects projects
        AcceptJSON ->
          Spock.json $ ApiV1.GetProjects ((projectName . valueOf) <$> projects)

  Spock.post "project" $
    authenticated authentication pool $ \a -> do
      withContentType $ \content ->
        case content of
          ContentTypeForm -> do
            project <- ProjectName <$> Spock.param' "project"
            repository <- Repository <$> Spock.param' "repository"
            r <- transactionT pool $ Project.new project repository
            case r of
              Left (Project.NewProjectAlreadyExists _) ->
                View.renderAuthenticated a $ View.newproject (Just Template.AlreadyExistsNewProjectError)
              Left (Project.NewProjectInvalidNameError _) ->
                View.renderAuthenticated a $ View.newproject (Just Template.InvalidNameNewProjectError)
              Left (Project.NewProjectInvalidRepositoryError _ _) ->
                View.renderAuthenticated a $ View.newproject (Just Template.InvalidRepositoryNewProjectError)
              Right _ ->
                Spock.redirect $ "/project/" <> renderProjectName project
          ContentTypeJSON -> do
            e <- Spock.jsonBody
            case e of
              Nothing -> do
                Spock.setStatus HTTP.status400
                Spock.json $ ApiV1.ApiError "invalid-body" (Just "Could not parse create project json.")
              Just (ApiV1.CreateProject project repository) -> do
                r <- transactionT pool $ Project.new project repository
                case r of
                  Left (Project.NewProjectAlreadyExists _) ->
                    Spock.json $ ApiV1.ApiError "new-project-already-exists" (Just "Provided project already exists.")
                  Left (Project.NewProjectInvalidNameError _) ->
                    Spock.json $ ApiV1.ApiError "new-project-invalid-name" (Just "Project name is not valid.")
                  Left (Project.NewProjectInvalidRepositoryError _ _) ->
                    Spock.json $ ApiV1.ApiError "new-project-invalid-repository" (Just "Repository is not valid.")
                  Right _ -> do
                    Spock.setStatus HTTP.created201
                    Spock.setHeader "Location" $ "/project/" <> renderProjectName project
                    Spock.json $ ApiV1.GetProject project []

  Spock.get "project/new" $
    authenticated authentication pool $ \a -> do
      View.renderAuthenticated a $ View.newproject Nothing

  Spock.get ("project" <//> Spock.var) $ \name ->
    authenticated authentication pool $ \a -> do
      r <- transaction pool $ do
        p <- Project.byName (ProjectName name)
        for p $ \pp -> do
          bs <- Build.byProjectId . keyOf $ pp
          pure (pp, bs)
      withAccept $ \case
        AcceptHTML ->
          case r of
            Nothing -> do
              Spock.setStatus HTTP.status404
              View.renderAuthenticated a $ View.notFound
            Just (project, builds) -> do
              Spock.setStatus HTTP.status200
              View.renderAuthenticated a $ View.project project builds
        AcceptJSON ->
          case r of
            Nothing -> do
              Spock.setStatus HTTP.status404
              Spock.json $ ApiV1.ApiError "not-found" Nothing
            Just (project, builds) -> do
              Spock.setStatus HTTP.status200
              Spock.json $ ApiV1.GetProject (projectName . valueOf $ project) (fmap (buildName . valueOf) builds)


  Spock.post ("project" <//> Spock.var) $ \project ->
    authenticated authentication pool $ \a -> do
      settings <- getTenant pool
      buildId <- liftError id $
        Discover.discover pool settings a (ProjectName project)
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
    authenticated authentication pool $ \a -> do
      let
        project = ProjectName project'
        build = BuildName build'

      builds <- liftDbError $ Build.list pool project build
      withAccept $ \case
        AcceptHTML -> do
          queued <- liftDbError $ Build.queued pool project build
          View.renderAuthenticated a $ View.builds builds queued
        AcceptJSON ->
          Spock.json $ ApiV1.GetBuilds builds


  Spock.post ("project" <//> Spock.var <//> "build" <//> Spock.var) $ \project' build' ->
    authenticated authentication pool $ \a -> do
      let
        project = ProjectName project'
        build = BuildName build'

      withContentType $ \content ->
        case content of
          ContentTypeForm -> do
            ref <- fmap Ref <$> Spock.param "ref"
            i <- transaction pool $
              Build.submit project build ref
            case i of
              Nothing -> do
                Spock.setStatus HTTP.status404
                View.renderAuthenticated a $ View.notFound
              Just ii ->
                Spock.redirect $ "/build/" <> (renderBuildId . keyOf) ii
          ContentTypeJSON -> do
            e <- Spock.jsonBody
            case e of
              Nothing -> do
                Spock.setStatus HTTP.status400
                Spock.json $ ApiV1.ApiError "invalid-body" (Just "Could not parse create build submission json.")
              Just (ApiV1.PostBuildRequest ref) -> do
                i <- transaction pool $
                  Build.submit project build ref
                case i of
                  Nothing -> do
                    Spock.setStatus HTTP.status404
                    Spock.json $ ApiV1.ApiError "not-found" Nothing
                  Just result -> do
                    Spock.setStatus HTTP.created201
                    Spock.setHeader "Location" $ "/build/" <> (renderBuildId . keyOf) result
                    Spock.json $ ApiV1.GetBuild result


  Spock.get ("project" <//> Spock.var <//> "commit" <//> Spock.var) $ \project' commit' ->
    authenticated authentication pool $ \a -> do
      let
        project = ProjectName project'
        commit = Commit commit'

      builds <- liftDbError $ Build.byCommit pool project commit
      withAccept $ \case
        AcceptHTML -> do
          datas <- for builds $ \i -> transaction pool (Build.byId i)
          View.renderAuthenticated a $ View.commit project commit (catMaybes datas)
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
                  Discover.complete pool discoverId project guts
                Spock.setStatus HTTP.ok200
                Spock.json ()

  Spock.get ("build" <//> Spock.var) $ \buildId' ->
    authenticated authentication pool $ \a -> do
      let
        buildId = BuildId buildId'

      build <- transaction pool $
        Build.byId buildId
      withAccept $ \case
        AcceptHTML -> do
          case build of
            Nothing -> do
              Spock.setStatus HTTP.notFound404
              Spock.html "TODO: 404 page."
            Just b ->
              View.renderAuthenticated a $ View.build b
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
    authenticated authentication pool $ \_a -> do
      results <- liftDbError . Traction.runDb pool $
        Result.scoreboard
      withAccept $ \case
        AcceptHTML ->
          View.renderScoreboard $ List.all ((== BuildOk) . resultBuildResult) results
        AcceptJSON ->
          Spock.json $ ApiV1.GetScoreboard results

-- TODO   [("text/plain; charset=utf-8", r maxBound)] <> withVersion' r
  Spock.get ("log" <//> Spock.var) $ \buildId ->
    authenticated authentication pool $ \_ -> do

      log' <- liftDbError $
        Build.logOf pool (BuildId buildId)

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

getTenant :: DbPool -> Spock.ActionT IO Tenant
getTenant _pool =
  error "todo"
  {--
  liftDbError . Traction.runDb pool $
    Query.demandTenant
--}

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

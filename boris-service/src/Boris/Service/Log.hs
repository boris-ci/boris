{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Log (
    LogError
  , renderLogError
  , withLogger
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import           Boris.Core.Data
import           Boris.Service.Boot

import           Data.Conduit ((=$=))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Binary as CB
import           Data.Time (getCurrentTime)

import           Jebediah.Data (LogGroup (..), LogStream (..), Log (..), newExclusiveSequence)
import           Jebediah.Structure (newLogGroup, newLogStream)
import qualified Jebediah.Conduit as J

import           Mismi (Env, Error, runAWS, renderError)

import           P

import           System.IO (IO, stdout)

import           Tine.Conduit (Out)

import           X.Control.Monad.Trans.Either (EitherT)


data LogError =
  LogAwsError Error

renderLogError :: LogError -> Text
renderLogError err =
  case err of
    LogAwsError e ->
      mconcat ["Underlying AWS error while logging: ", renderError e]

withLogger :: LogService -> Project -> BuildId -> (Out -> IO a) -> EitherT LogError IO a
withLogger service project buildId f =
  case service of
    CloudWatchLogs env environment -> do
      let
        gname = LogGroup $ mconcat ["boris.", renderEnvironment environment]
        sname = LogStream $ mconcat [renderProject project, ".", renderBuildId buildId]
      withCloudWatch env gname sname f
    Std ->
      liftIO $ f (CB.sinkHandle stdout)


withCloudWatch :: Env -> LogGroup -> LogStream -> (Out -> IO a) -> EitherT LogError IO a
withCloudWatch env group stream f =
  firstT LogAwsError $ runAWS env $ do
    lock <- liftIO $ newExclusiveSequence Nothing
    newLogGroup group >> newLogStream group stream
    liftIO $ J.sinkBracket env group stream lock $ \sink ->
      f . void $ CT.decodeUtf8Lenient
        =$= CT.lines
        =$= CL.mapM (\l -> getCurrentTime >>= pure . Log l)
        =$= J.clean
        =$= sink

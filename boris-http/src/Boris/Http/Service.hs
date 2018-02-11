{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Service (
    ServiceError
  , renderServiceError
  , put
  ) where

import           Boris.Http.Boot
import qualified Boris.Queue as Q

import qualified Control.Concurrent.Chan as Chan
import           Control.Monad.IO.Class (MonadIO (..))

import qualified Mismi.Control as Mismi

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


data ServiceError =
  ServiceSqsError Mismi.Error

renderServiceError :: ServiceError -> Text
renderServiceError e =
  case e of
    ServiceSqsError err ->
      mconcat ["SQS error pushing work to build service: ", Mismi.renderError err]

put :: BuildService -> Q.Request -> EitherT ServiceError IO ()
put service request =
  case service of
    SqsBuildService env queue ->
      firstT ServiceSqsError . Mismi.runAWS env $
        Q.put queue request
    EcsBuildService ->
      -- FIX MTH add back ecs build into modular implementations
      pure ()
    LocalBuildService chan ->
      liftIO $ Chan.writeChan chan request

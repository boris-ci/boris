-- |
-- Boris HTTP interactions.
--
-- Should be used as a qualified import from top-level module:
--
-- > import qualified Boris.Client as Boris
--
-- Example:
-- > Boris.runRequest configuration Boris.userinfo
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Network (
    runRequest
  , runRequestT
  ) where

import           Boris.Prelude
import           Boris.Client.Config
import           Boris.Client.Error
import           Boris.Client.Request (Request (..), Requester (..))
import           Boris.Client.Response (Responder (..))

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Bifunctor (BifunctorTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)

import           Data.Bifunctor (Bifunctor (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as  HTTP
--import qualified Network.OAuth2.JWT.Client as OAuth2

import           System.IO (IO)


-- |
-- Takes Boris runtime data, and an API request definition and actually
-- runs the request. Results are in IO and the error cases handled
-- explicitly.
--
runRequest :: Boris -> Request a -> IO (Either BorisError a)
runRequest boris =
  runExceptT .  runRequestT boris

-- |
-- Takes Boris runtime data, and an API request definition and actually
-- runs the request. Results are embeded in ExceptT for convenience.
--
runRequestT :: Boris -> Request a -> ExceptT BorisError IO a
runRequestT (Boris endpoint manager {--  oauth2 --}) (Request method path responder requester) = do
  req <- ExceptT . pure . first (BorisUrlParseError . Text.pack . show) $
    HTTP.parseRequest (Text.unpack $ mconcat [getBorisEndpoint endpoint, "/", path])

{--  token <- firstT BorisAuthenticationError . ExceptT $
    OAuth2.grant oauth2 --}

  res <- liftIO $ flip HTTP.httpLbs manager . runRequester requester $
    req {
        HTTP.method = HTTP.renderStdMethod method
      , HTTP.requestHeaders = [
--            ("Authorization", mconcat ["Bearer ", Text.encodeUtf8 . OAuth2.renderAccessToken $ token])
          ]
      }

  ExceptT . pure $
    runResponder responder res

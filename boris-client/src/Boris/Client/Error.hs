{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Client.Error (
    BorisError (..)
  , renderBorisError

  , ErrorCode (..)
  , ErrorMessage (..)
  ) where

import           Boris.Prelude

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as Lazy
--import qualified Network.OAuth2.JWT.Client as OAuth2


data BorisError =
    BorisApplicationError ErrorCode (Maybe ErrorMessage)
  | BorisAuthorizationError ErrorCode (Maybe ErrorMessage)
--  | BorisAuthenticationError OAuth2.GrantError
  | BorisResponseParseError Int Lazy.ByteString Text
  | BorisStatusCodeError Int Lazy.ByteString
  | BorisUrlParseError Text
    deriving (Eq, Show)

newtype ErrorCode =
  ErrorCode {
      getErrorCode :: Text
    } deriving (Eq, Ord, Show)

newtype ErrorMessage =
  ErrorMessage {
      getErrorMessage :: Text
    } deriving (Eq, Ord, Show)


renderBorisError :: BorisError -> Text
renderBorisError e =
  case e of
    -- FUTURE: debug mode that prints message.
    -- FUTURE: Handle specific error codes for better error messages.
    BorisApplicationError code _message ->
      mconcat ["There was an error performing your request [", getErrorCode code, "]."]
    -- FUTURE: debug mode that prints message.
    BorisAuthorizationError code _message ->
      mconcat ["You are not authorized to perform this request [", getErrorCode code, "]."]
--    BorisAuthenticationError _err ->
--      mconcat ["Boris could not authenticate you, please check your credentials and connectivity to Boris. DEBUG: ", Text.pack . show $ _err]
    -- FUTURE: debug mode that prints body + message
    BorisResponseParseError code _body _message ->
      mconcat ["Boris response parse error [", Text.pack . show $ code, "]. Please check connectivity to Boris and retry request."]
    -- FUTURE: debug mode that prints body.
    BorisStatusCodeError code _body ->
      mconcat ["Boris status code error [", Text.pack . show $ code, "]. Please check connectivity to Boris and retry request."]
    BorisUrlParseError message ->
      mconcat ["Boris client url-parse error [", message, "]. Check you are running the latest client version, and raise a supportissue if this issue persists ."]

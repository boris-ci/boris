{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Client.Error (
    BorisError (..)
  , ErrorCode (..)
  , ErrorMessage (..)
  ) where

import           Boris.Prelude

import           Data.Text (Text)
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

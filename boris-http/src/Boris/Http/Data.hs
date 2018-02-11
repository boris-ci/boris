{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Http.Data (
    ErrorId (..)
  , newErrorId
  ) where

import qualified Crypto.Random.Entropy as Entropy

import qualified Data.ByteString.Base16 as Base16
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           P

import           System.IO (IO)

newtype ErrorId =
  ErrorId {
    errorId :: Text
  } deriving (Eq, Ord, Show)

newErrorId :: IO ErrorId
newErrorId =
  ErrorId <$>
    Text.decodeUtf8 . Base16.encode <$> Entropy.getEntropy 16

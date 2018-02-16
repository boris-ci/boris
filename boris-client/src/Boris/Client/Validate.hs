{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Validate (
    validate
  ) where

import qualified Boris.Core.Serial.Command as S
import qualified Boris.Core.Serial.Ref as S

import qualified Data.Text.IO as T

import           P

import           System.IO (FilePath, IO)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT)
import           X.Control.Monad.Trans.Either.Exit (orDie)


validate :: Maybe FilePath -> Maybe FilePath -> IO ()
validate g b = orDie id $ do
    let
      xx :: (Text -> Either e a) -> Maybe FilePath -> EitherT e IO ()
      xx f y = forM_ y $ \file ->
        newEitherT $ f <$> T.readFile file
    firstT S.renderBorisPatternConfigError $ xx S.parsePatternConfig g
    firstT S.renderBorisConfigError $ xx S.parseConfig b

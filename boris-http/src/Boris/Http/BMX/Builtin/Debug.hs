{- | A set of debugging helpers that should not be used in production. -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.BMX.Builtin.Debug where

import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO (stderr)

import           Boris.Http.BMX.Data
import           Boris.Http.BMX.Function
import           Boris.Prelude

debugHelpers :: (Applicative m, MonadIO m) => [(Text, Helper m)]
debugHelpers = [
    ("log", logIO)
  , ("traceContext", traceContext)
  ]

-- | IO helper that logs to stderr
logIO :: (Applicative m, MonadIO m) => Helper m
logIO = helper $ do
  args <- many value
  liftIO . T.hPutStrLn stderr . T.unwords $ fmap renderValue args
  return (StringV T.empty)

-- | IO helper that prints out the current context
traceContext :: (Applicative m, MonadIO m) => Helper m
traceContext = helper . liftBMX $ do
  c <- readContext
  liftIO . T.hPutStrLn stderr $ maybe "<no context>" (T.pack . show) c
  return (StringV T.empty)

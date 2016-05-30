{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Log (
    withLogger
  ) where

import           Boris.X (Out)

import           Control.Monad.Reader (ask)
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Conduit ((=$=))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import           Data.Time (getCurrentTime)

import           Jebediah.Data (LogGroup (..), LogStream (..), Log (..), newExclusiveSequence)
import           Jebediah.Structure (newLogGroup, newLogStream)
import qualified Jebediah.Conduit as J

import           Mismi (AWS)

import           P

import           System.IO (IO)

withLogger :: LogGroup -> LogStream -> (Out -> IO a) -> AWS a
withLogger group stream f = do
  env <- ask
  lock <- liftIO $ newExclusiveSequence Nothing
  newLogGroup group >> newLogStream group stream
  liftIO $ J.sinkBracket env group stream lock $ \sink ->
    f . void $ CT.decodeUtf8Lenient
      =$= CT.lines
      =$= CL.mapM (\l -> getCurrentTime >>= pure . Log l)
      =$= J.clean
      =$= sink

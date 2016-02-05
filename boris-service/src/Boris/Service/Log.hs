{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Log (
    newLogger
  ) where

import           Boris.X (Out)

import           Control.Lens (view, (&), (.~))
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import           Control.Monad.Reader (ask)
import           Control.Monad.IO.Class (liftIO)

import           Data.ByteString (ByteString)
import           Data.Conduit (Conduit, Sink, (=$=))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import           Jebediah.Data (GroupName (..), StreamName (..))
import           Jebediah.Control (createLogGroup, createLogStream, listLogGroups, listLogStreams)

import           Mismi (AWS, rawRunAWS)
import           Mismi.Amazonka (Env)
import qualified Mismi.Amazonka as A
import qualified Mismi.CloudwatchLogs.Amazonka as MC

import           P

import           System.IO (IO)


newLogger :: GroupName -> StreamName -> AWS Out
newLogger =
  newSink

-- FIX XXXXXXXXXXXXXXXXXXX should be in jebediah or similar...
--
-- This isn't really good enough, and neither is what is in
-- jebediah at the moment, but at least this one doesn't
-- blow up on re-use. We really want to change this so there
-- is timeliness guarantees on a message being sent, but conduit
-- isn't very friendly with this situation. Options to do a
-- complete implementation would be:
--
--  * Implement a non-blocking await, by filtering the source
--    through an mvar, and have await always return current
--    state of mvar (Nothing for no value), and have sink
--    manage throttling/batching as required.
--
-- * Implement a more involved sink, that sets up a blocking queue,
--   with producer/consumer async's that manage the actual await/
--   batch/send lifecycle.
--
-- For now, we live with this, naive buffering, with state/locking to
-- ensure we can re-use this sync safely on-demand.
--

data Log =
  Log {
      logChunk :: Text
    , logTime :: UTCTime
    } deriving (Eq, Show)

newSink :: GroupName -> StreamName -> AWS (Sink ByteString IO ())
newSink g s = do
  newLogGroup g
  newLogStream g s
  env <- ask
  tok <- liftIO $ newMVar Nothing
  pure $ CT.decodeUtf8
    =$= CT.lines
    =$= joinempty
    =$= CL.mapM (\l -> getCurrentTime >>= pure . Log l)
    =$= buffer 10
    =$= CL.mapM_ (writeLogNel env g s tok)

-- XXX SNIP exclusive version of writeLogNel from jebediah (via mvar as a lock)
writeLogNel :: Env -> GroupName -> StreamName -> MVar (Maybe Text) -> NonEmpty Log -> IO ()
writeLogNel e g s next logs =
  void . modifyMVar_ next $ \token -> rawRunAWS e . fmap (view MC.plersNextSequenceToken) . A.send $
    MC.putLogEvents (unGroupName g) (unStreamName s) (toCloudwatch <$> logs)
      & MC.pleSequenceToken .~ token

-- XXX SNIP unexposed code in jebediah
toCloudwatch :: Log -> MC.InputLogEvent
toCloudwatch l =
  MC.inputLogEvent (round . (*1000) . utcTimeToPOSIXSeconds . logTime $ l) $ logChunk l

-- XXX MISSING this should be the implementation of createLogGroup in jebediah
newLogGroup :: GroupName -> AWS ()
newLogGroup g = do
  exists <- fmap (not . null) $ listLogGroups (Just g)
  unless exists $
    createLogGroup g

-- XXX MISSING this should be the implementation of createLogStream in jebediah
newLogStream :: GroupName -> StreamName -> AWS ()
newLogStream g s = do
  exists <- fmap (not . null) $ listLogStreams g (Just s)
  unless exists $
    createLogStream g s

-- XXX SNIP unexposed code in jebediah
buffer :: (Applicative m, Monad m) => Int -> Conduit a m (NonEmpty a)
buffer n = do
  a <- C.await
  case a of
    Nothing -> return ()
    Just a' -> do
      as <- replicateM (n - 1) C.await
      C.yield (a' :| catMaybes as)
      buffer n

joinempty :: (Applicative m, Monad m) => Conduit Text m Text
joinempty = do
  aa <- C.await
  case aa of
    Nothing ->
      pure ()
    Just a ->
      joinempty' a

joinempty' :: (Applicative m, Monad m) => Text -> Conduit Text m Text
joinempty' acc = do
  aa <- C.await
  case aa of
    Nothing ->
      C.yield acc
    Just a -> do
      case (T.null . T.strip) a of
        False ->
          C.yield acc >> joinempty
        True ->
          joinempty' (acc <> "\n")

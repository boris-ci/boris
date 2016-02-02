{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.X (
    Out
  , xPutStrLn
  , capture
  , exec
  , raw
  , xproc
  , xprocAt
  , hoistExit
  , hoistExitM
  ) where

import           Control.Concurrent.Async (Concurrently (..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.ByteString (ByteString)
import           Data.Conduit (Sink, (=$=), ($$))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Process as CP
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           P

import           System.Exit (ExitCode (..))
import           System.IO (IO, FilePath)
import           System.Process (CreateProcess (..), proc)

import           X.Control.Monad.Trans.Either (EitherT, left)


type Out =
  Sink ByteString IO ()

xPutStrLn :: MonadIO m => Out -> Text -> m ()
xPutStrLn o t =
  liftIO $ CL.sourceList (fmap T.encodeUtf8 [t, "\n"]) $$ o

capture :: Sink ByteString IO a -> Sink ByteString IO b -> CreateProcess -> IO (ByteString, ByteString, ExitCode)
capture sout serr =
  let cap x = C.passthroughSink x (const . pure $ ()) =$= CL.foldMap id
   in raw (cap sout) (cap serr)

exec :: Sink ByteString IO a -> Sink ByteString IO b -> CreateProcess -> IO ExitCode
exec cout cerr cp =
  (\(_, _, e) -> e) <$> raw cout cerr cp

raw :: Sink ByteString IO a -> Sink ByteString IO b -> CreateProcess -> IO (a, b, ExitCode)
raw cout cerr cp = do
  (CP.ClosedStream, sout, serr, handle) <- CP.streamingProcess cp
  runConcurrently $
    (,,)
      <$> Concurrently (sout $$ cout)
      <*> Concurrently (serr $$ cerr)
      <*> Concurrently (CP.waitForStreamingProcess handle)

xproc :: Out -> Text -> [Text] -> IO CreateProcess
xproc out cmd args = do
  xPutStrLn out $ T.intercalate " " $ mconcat [[cmd], fmap (\arg -> mconcat ["\"", arg, "\""]) args]
  pure $ proc (T.unpack cmd) (fmap T.unpack args)

xprocAt :: Out -> FilePath -> Text -> [Text] -> IO CreateProcess
xprocAt out dir cmd args =
  (\p -> p { cwd = Just dir }) <$> xproc out cmd args

hoistExit :: (Applicative f, Monad f) => ExitCode -> EitherT ExitCode f ()
hoistExit c =
  case c of
    ExitSuccess ->
      pure ()
    ExitFailure _ ->
      left c

hoistExitM :: (Applicative f, Monad f) => f ExitCode -> EitherT ExitCode f ()
hoistExitM e =
  lift e >>= hoistExit

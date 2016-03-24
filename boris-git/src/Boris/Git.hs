{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Git (
    bare
  , clone
  , checkout
  , cat
  , refs
  , commitAt
  ) where

import           Boris.Core.Data
import           Boris.X (Out)
import qualified Boris.X as X

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           P

import           System.Exit (ExitCode (..))
import           System.Directory (doesDirectoryExist)
import           System.IO (IO, FilePath)

import           X.Control.Monad.Trans.Either (EitherT)


bare :: Out -> Out -> Repository -> FilePath -> EitherT ExitCode IO LocalRepository
bare sout serr r target = do
  let local = LocalRepository $ T.pack  target
  unlessM (liftIO $ doesDirectoryExist target) . X.hoistExitM $
    X.exec sout serr =<<
      X.xproc sout "git" ["clone", "--mirror", renderRepository r, T.pack target]
  pure local

clone :: Out -> Out -> LocalRepository -> FilePath -> EitherT ExitCode IO LocalRepository
clone sout serr r target = do
  let local = LocalRepository $ T.pack  target
  X.hoistExitM $
    X.exec sout serr =<<
      X.xproc sout "git" ["clone", renderLocalRepository r, T.pack target]
  pure local

checkout :: Out -> Out -> LocalRepository -> Ref -> EitherT ExitCode IO ()
checkout sout serr r ref =
  X.hoistExitM $
    X.exec sout serr . X.inDirectory (T.unpack . renderLocalRepository $ r)  =<<
      X.xproc sout "git" ["checkout", "--quiet", renderRef ref]

cat :: Out -> Out -> LocalRepository -> Ref -> Text -> EitherT ExitCode IO Text
cat sout serr r ref f = do
  (out, _, c) <- liftIO $ X.capture sout serr . X.inDirectory (T.unpack . renderLocalRepository $ r) =<<
    X.xproc sout "git" ["show", renderRef ref <> ":" <> f]
  X.hoistExit c
  pure $ T.decodeUtf8 out

refs :: Out -> Out -> LocalRepository -> Pattern -> EitherT ExitCode IO [Ref]
refs sout serr r pattern = do
  (out, _, c) <- liftIO $ X.capture sout serr . X.inDirectory (T.unpack . renderLocalRepository $ r) =<<
    X.xproc sout "git" ["for-each-ref", "--format=%(refname)", renderPattern pattern]
  X.hoistExit c
  pure $ fmap Ref . T.lines . T.decodeUtf8 $ out

commitAt :: Out -> Out -> LocalRepository -> Ref -> EitherT ExitCode IO Commit
commitAt sout serr r ref = do
  (out, _, c) <- liftIO $ X.capture sout serr . X.inDirectory (T.unpack . renderLocalRepository $ r)  =<<
      X.xproc sout "git" ["rev-parse", renderRef ref]
  X.hoistExit c
  pure . Commit . T.strip . T.decodeUtf8 $ out

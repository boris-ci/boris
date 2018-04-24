{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Git (
    bare
  , clone
  , cloneref
  , checkout
  , cat
  , refs
  , commitAt
  ) where

import           Boris.Core.Data
import           Boris.Core.Data.Build
import           Boris.Core.Data.Repository

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           P

import           System.Directory (doesDirectoryExist)
import           System.Exit (ExitCode (..))
import           System.IO (FilePath, IO)

import           Tine.Conduit (Out)
import qualified Tine.Conduit as X

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

cloneref :: Out -> Out -> LocalRepository -> Repository -> FilePath -> EitherT ExitCode IO LocalRepository
cloneref sout serr l r target = do
  let local = LocalRepository $ T.pack  target
  X.hoistExitM $
    X.exec sout serr =<<
      X.xproc sout "git" ["clone", "--reference", renderLocalRepository l, renderRepository r, T.pack target]
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
  let
    -- Older versions of Git don't support recursive globs.
    -- This is a hack to simulate the most common case of matching top-level and second-level branchesFor example.
    -- For example matching both `master` and `topic/branch`.
    patternRecursive =
      if T.isSuffixOf "/**" (renderPattern pattern) then
        [T.replace "/**" "/*/*" (renderPattern pattern)]
      else
        []
  (out, _, c) <- liftIO $ X.capture sout serr . X.inDirectory (T.unpack . renderLocalRepository $ r) =<<
    X.xproc sout "git" (["for-each-ref", "--format=%(refname)", renderPattern pattern] <> patternRecursive)
  X.hoistExit c
  pure $ fmap Ref . T.lines . T.decodeUtf8 $ out

commitAt :: Out -> Out -> LocalRepository -> Ref -> EitherT ExitCode IO Commit
commitAt sout serr r ref = do
  (out, _, c) <- liftIO $ X.capture sout serr . X.inDirectory (T.unpack . renderLocalRepository $ r)  =<<
      X.xproc sout "git" ["rev-parse", renderRef ref]
  X.hoistExit c
  pure . Commit . T.strip . T.decodeUtf8 $ out

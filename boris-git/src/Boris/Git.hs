{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Git (
    bare
  , clone
  , checkout
  , cat
  , refs
  ) where

import           Boris.Core.Data
import           Boris.X (Out)
import qualified Boris.X as X

import           Control.Monad.IO.Class (liftIO)

import           Data.Text (Text)
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
  X.hoistExitM $ do
    X.exec sout serr =<<
      X.xproc sout "git" ["--git-dir", T.pack target, "fetch"]
  pure local

clone :: Out -> Out -> LocalRepository -> FilePath -> EitherT ExitCode IO LocalRepository
clone sout serr r target =
  fmap (const . LocalRepository . T.pack $ target) . X.hoistExitM $
    X.exec sout serr =<<
      X.xproc sout "git" ["clone", renderLocalRepository r, T.pack target]

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

refs :: Out -> Out -> LocalRepository -> Query -> EitherT ExitCode IO [Ref]
refs sout serr r query = do
  (out, _, c) <- liftIO $ X.capture sout serr . X.inDirectory (T.unpack . renderLocalRepository $ r) =<<
    X.xproc sout "git" ["for-each-ref", "--format=%(refname)", renderQuery query]
  X.hoistExit c
  pure $ fmap Ref . T.lines . T.decodeUtf8 $ out

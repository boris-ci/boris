{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Service.Git (
    InitialiseError (..)
  , initialise
  , discovering
  , findRef
  , renderInitialiseError
  ) where

import           Boris.Core.Data
import           Boris.Core.Serial.Ref
import           Boris.Core.Serial.Command
import qualified Boris.Git as Git

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import           P

import           System.Exit (ExitCode (..))
import           System.IO (IO)

import           Tine.Conduit (Out)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, bimapEitherT, hoistEither)


data InitialiseError =
    MirrorError ExitCode
  | CloneError ExitCode
  | CheckoutError ExitCode
  | MissingConfigError ExitCode
  | ListingRefsError ExitCode
  | ConfigParseError BorisConfigError
  | PatternConfigParseError BorisPatternConfigError
  | MissingBuildPattern Build [BuildPattern]
  | MissingBuildSpecification Build [Specification]
  | NoMatchingRef Build BuildPattern
  | AmbiguousRef Build BuildPattern [Ref]
  | MismatchedRef Build BuildPattern Ref [Ref]
  | InitialiseCommitError Build Ref ExitCode
    deriving (Eq, Show)

-- |
-- Workspace initialisation:
--
--  * Take a mirror of the target repository.
--
--   The fact that it is a mirror is of critical importance to pattern
--   resolution.  if we were to take a normal clone or bare repo, we
--   would have significant trouble and haxs to handle the difference
--   between a local ref and a remote ref.
--
--  * Load boris-git.toml off of the master branch.
--
--    In a future world this is perhaps configurable per build, but for
--    now this is an explicit convention. This file contains git patterns
--    which we use to resolve the ref we need to build.
--
--  * Parse boris-git.toml and determine the pattern for the specified build.
--
--  * Resolve the pattern against the repository refs.
--
--    We demand an unambiguous match here. If the pattern contains a wildcard,
--    an explicit ref will have to be supplied (and that ref will be validated
--    that it matches a real ref referenced by the pattern).
--
--  * Determine the commit for the resolved ref.
--
--  * Load boris.toml from the commit.
--
--  * Parse boris.toml and determine the build specification.
--
--  * Clone the mirror into a working copy of the repository.
--
--  * Checkout the explicit commit found for the resolved-ref.
--
--  * We are ready to go then.
--
initialise :: Out -> Out -> Workspace -> Build -> Repository -> Maybe Ref -> EitherT InitialiseError IO BuildInstance
initialise sout serr workspace build repository mref = do
  mirror <- bimapEitherT MirrorError id $
    Git.bare sout serr repository $ pathOfMirror workspace
  patterntext <- bimapEitherT MissingConfigError id $
    Git.cat sout serr mirror (Ref "refs/heads/master") ("boris-git.toml")
  patterns <- bimapEitherT PatternConfigParseError id . hoistEither $
    parsePatternConfig patterntext
  pattern <- hoistEither . maybeToRight (MissingBuildPattern build patterns) $
    P.find ((==) build . buildName) patterns
  refs <- bimapEitherT ListingRefsError id $
    Git.refs sout serr mirror . buildPattern $ pattern
  ref <- hoistEither $
    findRef build pattern mref refs
  commit <- bimapEitherT (InitialiseCommitError build ref) id $
    Git.commitAt sout serr mirror ref
  specificationtext <- bimapEitherT MissingConfigError id $
    Git.cat sout serr mirror (Ref . renderCommit $ commit) ("boris.toml")
  specifications <- bimapEitherT ConfigParseError id . hoistEither $
    parseConfig specificationtext
  specification <- hoistEither . maybeToRight (MissingBuildSpecification build specifications) $
    P.find ((==) build . specificationBuild) specifications
  work <- bimapEitherT CloneError id $
    Git.cloneref sout serr mirror repository $ pathOfWorkingCopy workspace
  bimapEitherT CheckoutError id $
    Git.checkout sout serr work (Ref . renderCommit $ commit)
  pure $ BuildInstance specification ref commit

-- |
-- Discovery initialisation:
--
--  * Take a mirror of the target repository.
--
--   The fact that it is a mirror is of critical importance to pattern
--   resolution.  if we were to take a normal clone or bare repo, we
--   would have significant trouble and haxs to handle the difference
--   between a local ref and a remote ref.
--
--  * Load boris-git.toml off of the master branch.
--
--    In a future world this is perhaps configurable per build, but for
--    now this is an explicit convention. This file contains git patterns
--    which we use to resolve the ref we need to build.
--
--  * Parse boris-git.toml and determine the pattern for the specified build.
--
--  * Resolve the pattern against the repository refs.
--
--  * Determine the commit for all resolved refs.
--
discovering :: Out -> Out -> Workspace -> Repository -> EitherT InitialiseError IO [DiscoverInstance]
discovering sout serr workspace repository  = do
  mirror <- bimapEitherT MirrorError id $
    Git.bare sout serr repository $ pathOfMirror workspace
  patterntext' <- liftIO . fmap rightToMaybe . runEitherT $
    Git.cat sout serr mirror (Ref "refs/heads/master") ("boris-git.toml")
  fmap (fromMaybe []) . for patterntext' $ \patterntext -> do
    patterns <- bimapEitherT PatternConfigParseError id . hoistEither $
      parsePatternConfig patterntext
    fmap join . fmap catMaybes . fmap join . forM patterns $ \pattern -> do
      refs <- bimapEitherT ListingRefsError id $
        Git.refs sout serr mirror . buildPattern $ pattern
      forM refs $ \ref -> do
        commit <- bimapEitherT (InitialiseCommitError (buildName pattern) ref) id $
          Git.commitAt sout serr mirror ref
        specificationtext' <- liftIO . fmap rightToMaybe . runEitherT $
          Git.cat sout serr mirror (Ref . renderCommit $ commit) ("boris.toml")
        for specificationtext' $ \specificationtext -> do
          specifications <- bimapEitherT ConfigParseError id . hoistEither $
            parseConfig specificationtext
          pure $ filter (const . not . null $ filter (\s -> specificationBuild s == buildName pattern) specifications) $
            [DiscoverInstance pattern ref commit]

-- |
-- Resolving a pattern to a ref.
--
--  * If there are no refs for the pattern, we must fail.
--
--  * If there is exactly one matching ref:
--      * If there is an explicit target, then we validate that
--        it matches, otherwise:
--      * We go ahead and used exact match
--
--  * If there is more than one matching ref:
--      * If there is _no_ explicit target, we have to fail as the
--        request is ambiguous, otherwise:
--      * We valid that the target is an exact match for one of
--        the refs. If it is we win, otherwise we have to fail.
--
findRef :: Build -> BuildPattern -> Maybe Ref -> [Ref] -> Either InitialiseError Ref
findRef build pattern mref refs =
  case refs of
    [] ->
      Left $ NoMatchingRef build pattern
    [target] ->
      case mref of
        Nothing ->
          Right target
        Just requested ->
          if target == requested
            then Right target
            else Left $ MismatchedRef build pattern requested refs
    candidates ->
      case mref of
        Nothing ->
          Left $ AmbiguousRef build pattern candidates
        Just requested ->
          case elem requested candidates of
            True ->
              Right requested
            False ->
              Left $ MismatchedRef build pattern requested candidates

renderInitialiseError :: InitialiseError -> Text
renderInitialiseError err =
  case err of
    MirrorError c ->
      mconcat ["Error mirroring repository, exit with: ", T.pack . show $ c]
    CloneError c ->
      mconcat ["Error cloning working copy, exit with: ", T.pack . show $ c]
    CheckoutError c ->
      mconcat ["Error checking out reference, exit with: ", T.pack . show $ c]
    MissingConfigError c ->
      mconcat ["Error retrieving boris-git.toml from master, exit with: ", T.pack . show $ c]
    ListingRefsError c ->
      mconcat ["Error listing refs on repository, exit with: ", T.pack . show $ c]
    ConfigParseError e ->
      mconcat ["Boris configuration failed to parse: ", renderBorisConfigError e]
    PatternConfigParseError e ->
      mconcat ["Boris pattern configuration failed to parse: ", renderBorisPatternConfigError e]
    MissingBuildPattern b qs ->
      mconcat ["Boris build refs could not be found: build = ", renderBuild b, ", queries = [", T.intercalate ", " ((renderBuild . buildName) <$> qs), "]"]
    MissingBuildSpecification b ss ->
      mconcat ["Boris build specification could not be found: build = ", renderBuild b, ", specifications = [", T.intercalate ", " ((renderBuild . specificationBuild) <$> ss), "]"]
    NoMatchingRef b s ->
      mconcat ["Boris build ref could not be found, there were no refs found in repository: build = ", renderBuild b, ", pattern = ", renderPattern . buildPattern $ s]
    AmbiguousRef b s rs ->
      mconcat ["Boris build ref could not be determine, there were more than one refs found in repository: build = ", renderBuild b, ", pattern = ", renderPattern . buildPattern $ s, ", refs = [", T.intercalate ", " (renderRef <$> rs) ,"]"]
    MismatchedRef b s r rs ->
      mconcat ["Boris build ref mismatch, requested ref does not match allowed refs for this build: build = ", renderBuild b, ", pattern = ", renderPattern . buildPattern $ s, ", refs = [", T.intercalate ", " (renderRef <$> rs) ,"], requested = ", renderRef r]
    InitialiseCommitError b r c ->
      mconcat ["Boris could not determine commit for ref: build = ", renderBuild b, ", ref = ", renderRef r, ", exit with ", T.pack . show $ c]

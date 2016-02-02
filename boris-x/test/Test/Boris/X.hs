{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Boris.X where

import qualified Boris.X as X

import           P

import           System.Exit (ExitCode (..))

import           Test.QuickCheck

import           X.Control.Monad.Trans.Either (EitherT, left, right)


prop_hoistExit_ok =
  X.hoistExit ExitSuccess === (right () :: EitherT ExitCode Maybe ())

prop_hoistExit_fail n =
  X.hoistExit (ExitFailure n) === (left (ExitFailure n) :: EitherT ExitCode Maybe ())

prop_hoistExitM_ok =
  X.hoistExitM (Just ExitSuccess) === right ()

prop_hoistExitM_fail n =
  X.hoistExitM (Just $ ExitFailure n) === left (ExitFailure n)

return []
tests = $quickCheckAll

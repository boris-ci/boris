{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Store.Meta where

import           Boris.Core.Data
import qualified Boris.Store.Lifecycle as SL
import qualified Boris.Store.Meta as SM

import           Mismi (AWS, awsBracket)

import           P

import           Test.Boris.Core.Arbitrary ()
import           Test.IO.Boris.Store
import           Test.QuickCheck
import           Test.Mismi (testAWS)

import           X.Control.Monad.Trans.Either (runEitherT)

prop_nextId p b = once . testAWS . withBuildMeta environment p b $ do
  x <- runEitherT $ SM.nextId environment p b
  y <- runEitherT $ SM.nextId environment p b
  pure $ [x, y] === [Right $ BuildId "1", Right $ BuildId "2"]

withBuildMeta :: Environment -> Project -> Build -> AWS a -> AWS a
withBuildMeta e p b f =
  awsBracket (SL.initialise e) (const $ SM.delete e p b) (const f)

return []
tests = $quickCheckAll

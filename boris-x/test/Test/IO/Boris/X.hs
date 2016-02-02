{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.X where

import qualified Boris.X as X

import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Disorder.Corpus
import           Disorder.Core.IO

import           P

import           System.Exit (ExitCode (..))
import           System.Process (proc)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_capture = forAll (elements viruses) $ \t ->
  let
    o = CL.sinkNull
    e = CL.sinkNull
  in testIO $ do
    (ro, re, s) <- X.capture o e (proc "echo" [T.unpack $ t])
    pure $ (ro, re, s) === (T.encodeUtf8 t <> "\n", "", ExitSuccess)

prop_exec =
  let
    o = CL.sinkNull
    e = CL.sinkNull
  in testIO $ do
    t <- X.exec o e (proc "true" [])
    f <- X.exec o e (proc "false" [])
    pure $ (t, f) === (ExitSuccess, ExitFailure 1)

prop_raw = forAll (elements viruses) $ \t ->
  let
    o = CL.consume
    e = CL.consume
  in testIO $ do
    (ro, re, s) <- X.raw o e (proc "echo" [T.unpack $ t])
    pure $ (mconcat ro, mconcat re, s) === (T.encodeUtf8 t <> "\n", "", ExitSuccess)

return []
tests = $quickCheckAll

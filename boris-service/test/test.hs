{-# LANGUAGE NoImplicitPrelude #-}

import           Control.Monad ((>>=), (>>), when, mapM)

import           Prelude (($), (.), not, all, id)

import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO

import qualified Test.Boris.Service.Git
import qualified Test.IO.Boris.Service.Git
import qualified Test.IO.Boris.Service.Workspace


main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> mapM id [
      Test.Boris.Service.Git.tests
    , Test.IO.Boris.Service.Git.tests
    , Test.IO.Boris.Service.Workspace.tests
    ] >>= \rs -> when (not . all id $ rs) Exit.exitFailure

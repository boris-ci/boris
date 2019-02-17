{-# LANGUAGE NoImplicitPrelude #-}

import           Control.Monad ((>>=), (>>), when, mapM)

import           Prelude (($), (.), not, all, id)

import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO

import qualified Test.Boris.Core.Data.Build
import qualified Test.Boris.Core.Serial.Command
import qualified Test.Boris.Core.Serial.Ref


main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> mapM id [
      Test.Boris.Core.Data.Build.tests
    , Test.Boris.Core.Serial.Ref.tests
    , Test.Boris.Core.Serial.Command.tests
    ] >>= \rs -> when (not . all id $ rs) Exit.exitFailure

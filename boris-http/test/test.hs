{-# LANGUAGE NoImplicitPrelude #-}

import           Control.Monad ((>>=), (>>), when, mapM)

import           Prelude (($), (.), not, all, id)

import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO

import qualified Test.IO.Boris.Http.Db.Query
import qualified Test.IO.Boris.Http.Db.Schema


main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> mapM id [
      Test.IO.Boris.Http.Db.Schema.tests
    , Test.IO.Boris.Http.Db.Query.tests
    ] >>= \rs -> when (not . all id $ rs) Exit.exitFailure

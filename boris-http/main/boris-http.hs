{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Boris.Http.Boot as Boot
import qualified Boris.Http.Route as Route
import qualified Boris.Http.Db.Schema as Schema
import qualified Boris.Http.Db.Query as Query
import           Boris.Prelude

import qualified Data.Text as Text

import qualified Nest

import           System.Exit (ExitCode(..), exitWith)
import qualified System.IO as IO
import           System.IO (IO, stderr, hPutStrLn)

import qualified Traction.Control as Traction

import qualified Web.Spock.Core as Spock

import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setOnException, defaultOnException)

main :: IO ()
main = do
  Boot.Boot mode authentication pool defaults <-
    Nest.force $ Boot.boot

  orDie Traction.renderDbError $
    Schema.initialise pool

  tenant <- orDie Traction.renderDbError $
    Traction.runDb pool Query.getTenant

  port <- Nest.force $ Nest.numeric "PORT" `Nest.withDefault` 10080

  routes <- case (tenant, defaults) of
    (Just _, _) ->
      pure $ Route.application pool authentication mode
    (Nothing, Just t) -> do
      orDie Traction.renderDbError $
        Traction.runDb pool $ Query.setTenant t
      pure $ Route.application pool authentication mode
    (Nothing, Nothing) -> do
      pure $ Route.configure pool authentication mode

  app <- Spock.spockAsApp $ Spock.spockConfigT Spock.defaultSpockConfig id routes

  let
    s = setPort port $
      setOnException (\req ex -> do
        IO.print ex
        defaultOnException req ex) $
          defaultSettings
  runSettings s app

-- | orDieWithCode with an exit code of 1 in case of an error
--
orDie :: (e -> Text) -> EitherT e IO a -> IO a
orDie = orDieWithCode 1

-- | An idiom for failing hard on EitherT errors.
--
-- *This really dies*. There is no other way to say it.
--
-- The reason it lives with command line parser tooling, is that is
-- the only valid place to actually exit like this. Be appropriately
-- wary.
--
orDieWithCode :: Int -> (e -> Text) -> EitherT e IO a -> IO a
orDieWithCode code render e =
  runEitherT e >>=
    either (\err -> (hPutStrLn stderr . Text.unpack . render) err >> exitWith (ExitFailure code)) pure

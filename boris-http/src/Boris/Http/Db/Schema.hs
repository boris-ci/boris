{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Boris.Http.Db.Schema (
    initialise
  , migrate
  , schema
  ) where

import           Boris.Prelude

import           System.IO (IO)

import           Traction.Control (DbPool, DbError, Db)
import qualified Traction.Control as Traction
import           Traction.Migration (Migration (..))
import qualified Traction.Migration as Traction
import           Traction.QQ (sql)


initialise :: DbPool -> EitherT DbError IO ()
initialise pool =
  Traction.runDb pool $
    migrate

migrate :: Db ()
migrate =
 void $ Traction.migrate schema

schema :: [Migration]
schema = [
    Migration "create-projects" [sql|
      CREATE TABLE project (
          id SERIAL PRIMARY KEY
        , name TEXT NOT NULL UNIQUE
        , repository TEXT NOT NULL
        , enabled BOOLEAN NOT NULL
        )
    |]

  , Migration "create-run" [sql|
      CREATE TABLE run (
          id SERIAL PRIMARY KEY
        , run_type INT NOT NULL
        , project BIGINT NOT NULL REFERENCES project(id)
        , queued_time TIMESTAMP WITH TIME ZONE
        , start_time TIMESTAMP WITH TIME ZONE
        , end_time TIMESTAMP WITH TIME ZONE
        , heartbeat_time TIMESTAMP WITH TIME ZONE
        , cancelled BOOLEAN
        )
    |]

      -- FIX build_result -> run result?
  , Migration "create-build" [sql|
      CREATE TABLE build (
          id BIGINT PRIMARY KEY REFERENCES run(id)
        , build TEXT
        , ref TEXT
        , commit TEXT
        , build_result BOOLEAN
        )
    |]

  , Migration "create-discover" [sql|
      CREATE TABLE discover (
          id BIGINT PRIMARY KEY REFERENCES run(id)
        )
    |]

  , Migration "create-discover-commit-build" [sql|
      CREATE TABLE discover_commit (
          discover_id BIGINT NOT NULL REFERENCES discover(id)
        , build TEXT NOT NULL
        , commit TEXT NOT NULL
        , PRIMARY KEY (discover_id, build, commit)
        )
    |]

  , Migration "create-log" [sql|
      CREATE TABLE log (
          run_id BIGINT NOT NULL REFERENCES run(id)
        , log_id SERIAL PRIMARY KEY
        , logged_at TIMESTAMP WITH TIME ZONE NOT NULL
        , log_payload TEXT NOT NULL
        )
    |]
  ]

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Boris.Http.Db.Schema (
    initialise
  , migrate
  , schema
  ) where

import           P

import           System.IO (IO)

import           Traction.Control (DbPool, DbError, Db)
import qualified Traction.Control as Traction
import           Traction.Migration (Migration (..))
import qualified Traction.Migration as Traction
import           Traction.Sql (sql)

import           X.Control.Monad.Trans.Either (EitherT)

initialise :: DbPool -> EitherT DbError IO ()
initialise pool =
  Traction.runDb pool $
    migrate

migrate :: Db ()
migrate =
 void $ Traction.migrate schema

schema :: [Migration]
schema = [
    Migration "create-tick" [sql|
      CREATE SEQUENCE tick START 1;
    |]
  , Migration "create-build" [sql|
      CREATE TABLE build (
          build_id BIGINT PRIMARY KEY
        , project TEXT
        , build TEXT
        , ref TEXT
        , commit TEXT
        , queued_time TIMESTAMP WITH TIME ZONE
        , start_time TIMESTAMP WITH TIME ZONE
        , end_time TIMESTAMP WITH TIME ZONE
        , heartbeat_time TIMESTAMP WITH TIME ZONE
        , cancelled BOOLEAN
        , build_result BOOLEAN
        , log_group TEXT
        , log_stream TEXT
        )
    |]
  , Migration "create-discover" [sql|
      CREATE TABLE discover (
          discover_id BIGINT PRIMARY KEY
        , project TEXT NOT NULL
        )
    |]
  , Migration "create-discover-commit-build" [sql|
      CREATE TABLE discover_commit (
          discover_id BIGINT NOT NULL
        , build TEXT NOT NULL
        , commit TEXT NOT NULL
        , PRIMARY KEY (discover_id, build, commit)
        )
    |]
  , Migration "create-account" [sql|
      CREATE TABLE account (
          id SERIAL PRIMARY KEY
        , github_id BIGINT NOT NULL
        , github_login TEXT NOT NULL
        , github_name TEXT
        , github_email TEXT
        , created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
        , updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
        )
    |]
  , Migration "create-session" [sql|
      CREATE TABLE session (
          id TEXT PRIMARY KEY
        , account BIGINT NOT NULL
        , oauth TEXT NOT NULL
        , created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
        , updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
        )
    |]

  , Migration "create-log" [sql|
      CREATE TABLE log (
          build_id BIGINT NOT NULL
        , log_id SERIAL PRIMARY KEY
        , logged_at TIMESTAMP WITH TIME ZONE
        , log_payload TEXT
        )
    |]
  , Migration "create-agent" [sql|
      CREATE TABLE agent (
          id TEXT PRIMARY KEY
        , tags TEXT[]
        , poll_count BIGINT NOT NULL
        , first_poll TIMESTAMP WITH TIME ZONE
        , last_poll TIMESTAMP WITH TIME ZONE
        )
    |]

  , Migration "create-organisation" [sql|
      CREATE TABLE organisation (
          id SERIAL PRIMARY KEY
        , name TEXT UNIQUE NOT NULL
        )
    |]

  , Migration "create-projects" [sql|
      CREATE TABLE project (
          id TEXT PRIMARY KEY
        , organisation BIGINT NOT NULL REFERENCES organisation(id)
        , name TEXT UNIQUE
        , repository TEXT NOT NULL
        , UNIQUE (organisation, name)
        )
    |]

  , Migration "create-settings" [sql|
      CREATE TABLE settings (
          multi_tenant BOOLEAN
        )
    |]

  ]

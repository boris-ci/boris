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
    Migration "create-tick" [sql|
      CREATE SEQUENCE tick START 1;
    |]

  , Migration "create-projects" [sql|
      CREATE TABLE project (
          id SERIAL PRIMARY KEY
        , name TEXT NOT NULL UNIQUE
        , repository TEXT NOT NULL
        , enabled BOOLEAN NOT NULL
        )
    |]

  , Migration "create-build" [sql|
      CREATE TABLE build (
          build_id BIGINT PRIMARY KEY
        , project BIGINT NOT NULL REFERENCES project(id)
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


  , Migration "create-github-account" [sql|
      CREATE TABLE github_account (
          id SERIAL PRIMARY KEY
        , github_id BIGINT NOT NULL
        , github_login TEXT NOT NULL
        , github_name TEXT
        , github_email TEXT
        , created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
        , updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
        )
    |]

  , Migration "create-discover" [sql|
      CREATE TABLE discover (
          discover_id BIGINT PRIMARY KEY
        , project TEXT NOT NULL
        , queued_time TIMESTAMP WITH TIME ZONE
        , start_time TIMESTAMP WITH TIME ZONE
        , end_time TIMESTAMP WITH TIME ZONE
        , heartbeat_time TIMESTAMP WITH TIME ZONE
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

  , Migration "create-owner" [sql|
      CREATE TABLE owner (
          id SERIAL PRIMARY KEY
        , name TEXT UNIQUE NOT NULL
        , type INT NOT NULL
        )
    |]

  , Migration "insert-boris-system-owner" [sql|
      INSERT INTO owner (id, name, type)
           VALUES (0, 'boris', 0)
    |]

  , Migration "create-settings" [sql|
      CREATE TABLE settings (
          multi_tenant BOOLEAN
        )
    |]


  ]

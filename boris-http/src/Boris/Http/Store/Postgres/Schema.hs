{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Boris.Http.Store.Postgres.Schema (
    schema
  ) where

import           Traction.Migration (Migration (..))
import           Traction.Sql (sql)


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
        , created TIMESTAMPTZ NOT NULL DEFAULT now()
        , updated TIMESTAMPTZ NOT NULL DEFAULT now()
        )
    |]
  , Migration "create-session" [sql|
      CREATE TABLE session (
          id TEXT PRIMARY KEY
        , account BIGINT NOT NULL
        , oauth TEXT NOT NULL
        , created TIMESTAMPTZ NOT NULL DEFAULT now()
        , updated TIMESTAMPTZ NOT NULL DEFAULT now()
        )
    |]
  ]

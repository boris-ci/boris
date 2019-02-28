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

  , Migration "create-organisation" [sql|
      CREATE TABLE organisation (
          id serial PRIMARY KEY
        , name text NOT NULL UNIQUE
        )
    |]

  , Migration "create-entity" [sql|
      CREATE TABLE entity (
          id SERIAL PRIMARY KEY
        , entity_type INT NOT NULL
        )
    |]

  , Migration "create-identity" [sql|
      CREATE TABLE identity (
          id SERIAL PRIMARY KEY REFERENCES entity(id)
        , identity_type INT NOT NULL
        )
    |]

  , Migration "create-identity-api-key" [sql|
      CREATE TABLE identity_api_key (
          id TEXT NOT NULL PRIMARY KEY
        , identity BIGINT NOT NULL REFERENCES identity(id)
        , public_jwk BYTEA NOT NULL
        , created TIMESTAMPTZ NOT NULL DEFAULT now()
        )
    |]

  , Migration "create-user-account" [sql|
      CREATE TABLE user_account (
          id BIGINT PRIMARY KEY NOT NULL REFERENCES identity(id)
        , email TEXT NOT NULL UNIQUE
        , name TEXT NOT NULL
        , crypted TEXT
        , default_organisation BIGINT REFERENCES organisation(id)
        , created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
        )
    |]

  , Migration "create-github-account" [sql|
      CREATE TABLE github_account (
          id BIGINT PRIMARY KEY NOT NULL REFERENCES user_account(id)
        , github_id BIGINT NOT NULL
        , github_login TEXT NOT NULL
        , github_name TEXT
        , github_email TEXT
        , created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
        , updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
        )
    |]

  , Migration "create-service-account" [sql|
      CREATE TABLE service_account (
          id BIGINT PRIMARY KEY NOT NULL REFERENCES identity(id)
        , organisation BIGINT NOT NULL REFERENCES organisation(id)
        , name TEXT NOT NULL
        , UNIQUE (organisation, name)
        )
    |]

  , Migration "create-organisation-user" [sql|
      CREATE TABLE organisation_user (
          organisation BIGINT NOT NULL REFERENCES organisation(id)
        , user_account BIGINT NOT NULL REFERENCES user_account(id)
        , UNIQUE (organisation, user_account)
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

  , Migration "create-projects" [sql|
      CREATE TABLE project (
          id SERIAL PRIMARY KEY
        , name TEXT NOT NULL UNIQUE
        , repository TEXT NOT NULL
        , enabled BOOLEAN NOT NULL
        )
    |]

  , Migration "create-account-projects" [sql|
      CREATE TABLE account_projects (
          account BIGINT NOT NULL REFERENCES user_account(id)
        , project BIGINT NOT NULL REFERENCES project(id)
        , permission INT NOT NULL
        , PRIMARY KEY (account, project)
        )
    |]

  , Migration "create-settings" [sql|
      CREATE TABLE settings (
          multi_tenant BOOLEAN
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

  ]

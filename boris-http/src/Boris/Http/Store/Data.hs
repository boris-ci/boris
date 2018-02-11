{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Http.Store.Data (
    Store (..)
  ) where

import           Boris.Core.Data

import           Data.IORef (IORef)

import           Mismi.Amazonka (Env)

import           P

import           Traction.Control (DbPool)


data Store =
    DynamoStore Env Environment
  | PostgresStore DbPool
  | MemoryStore (IORef (Int, [BuildData], [(Project, DiscoverInstance)]))

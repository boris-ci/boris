{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Http.Store.Data (
    Store (..)
  ) where

import           Boris.Core.Data

import           Data.IORef (IORef)

import           P

import           Traction.Control (DbPool)


data Store =
    PostgresStore DbPool
  | MemoryStore (IORef (Int, [BuildData], [(Project, DiscoverInstance)]))

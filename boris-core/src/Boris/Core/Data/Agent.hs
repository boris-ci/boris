{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Core.Data.Agent (
    Agent (..)
  , AgentId (..)
  , QueueSize (..)
  ) where

import           Boris.Prelude


data Agent =
  Agent {
      agentTags :: [Text]
    } deriving (Eq, Ord, Show)

newtype AgentId =
  AgentId {
      agentId :: Int64
    } deriving (Eq, Ord, Show)

newtype QueueSize =
  QueueSize {
      getQueueSize :: Int
    } deriving (Eq, Ord, Show)

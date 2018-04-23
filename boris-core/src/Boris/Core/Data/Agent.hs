{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Core.Data.Agent (
    Agent (..)
  , AgentId (..)
  ) where

import           P


data Agent =
  Agent {
      agentTags :: [Text]
    } deriving (Eq, Ord, Show)

newtype AgentId =
  AgentId {
      agentId :: Int64
    } deriving (Eq, Ord, Show)

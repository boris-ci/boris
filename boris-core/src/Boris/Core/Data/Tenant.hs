{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Data.Tenant (
    Tenant (..)
  ) where

import           P


data Tenant =
    SingleTenant
  | MultiTenant
    deriving (Eq, Ord, Show)

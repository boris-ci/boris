{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Data.Tenant (
    Settings (..)
  ) where

import           P


data Settings =
    SingleTenantSettings
  | MultiTenantSettings
    deriving (Eq, Ord, Show)

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Data.Tenant (
    Tenant (..)
  ) where

import           Boris.Prelude


data Tenant =
    SingleTenant
  | MultiTenant
    deriving (Eq, Ord, Show)

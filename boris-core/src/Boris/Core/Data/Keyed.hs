{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
module Boris.Core.Data.Keyed (
    Keyed (..)
  ) where

import           Boris.Prelude


data Keyed a b =
  Keyed {
      keyOf :: a
    , valueOf :: b
    } deriving (Eq, Ord, Show, Functor)

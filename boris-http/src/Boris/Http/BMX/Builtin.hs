{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Builtin (
    builtinHelpers
  , builtinDecorators
  , debugHelpers
  , defaultState
  ) where

import           BMX.Builtin.Debug (debugHelpers)
import           BMX.Builtin.Decorators (builtinDecorators)
import           BMX.Builtin.Helpers (builtinHelpers)

import           BMX.Data

import           P

-- | The default state: an empty context, all the helpers from
-- 'BMX.Builtin.Helpers.builtinHelpers', and all the decorators from
-- 'BMX.Builtin.Decorators.builtinDecorators'.
defaultState :: (Applicative m, Monad m) => BMXState m
defaultState = mempty {
    bmxHelpers = builtinHelpers
  , bmxDecorators = builtinDecorators
  }

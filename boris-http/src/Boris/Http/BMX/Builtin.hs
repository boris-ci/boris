{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.BMX.Builtin (
    builtinHelpers
  , builtinDecorators
  , debugHelpers
  , defaultState
  ) where

import           Boris.Http.BMX.Builtin.Debug (debugHelpers)
import           Boris.Http.BMX.Builtin.Decorators (builtinDecorators)
import           Boris.Http.BMX.Builtin.Helpers (builtinHelpers)

import           Boris.Http.BMX.Data
import           Boris.Prelude


-- | The default state: an empty context, all the helpers from
-- 'BMX.Builtin.Helpers.builtinHelpers', and all the decorators from
-- 'BMX.Builtin.Decorators.builtinDecorators'.
defaultState :: (Applicative m, Monad m) => BMXState m
defaultState = mempty {
    bmxHelpers = builtinHelpers
  , bmxDecorators = builtinDecorators
  }

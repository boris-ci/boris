{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module BMX.Debug (
    debugTemplateIO
  ) where

import           Control.Monad.IO.Class (MonadIO)

import           BMX.Builtin
import           BMX.Data
import           BMX.Eval

import           P

{-# WARNING debugTemplateIO "Do not use 'debugTemplateIO' in production code" #-}
-- | Evaluate a 'Template' against some 'BMXState' with debugging helpers enabled.
--
-- Debugging helpers are enumerated in 'debugHelpers'.
--
-- Debugging helpers may perform IO. Do not use this in production.
debugTemplateIO :: (Applicative m, MonadIO m) => BMXState m -> Template -> m (Either BMXError Page)
debugTemplateIO st = renderTemplateM (st `usingHelpers` debugHelpers)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Http.BMX.Debug (
    debugTemplateIO
  ) where

import           Control.Monad.IO.Class (MonadIO)

import           Boris.Http.BMX.Builtin
import           Boris.Http.BMX.Data
import           Boris.Http.BMX.Eval
import           Boris.Prelude


{-# WARNING debugTemplateIO "Do not use 'debugTemplateIO' in production code" #-}
-- | Evaluate a 'Template' against some 'BMXState' with debugging helpers enabled.
--
-- Debugging helpers are enumerated in 'debugHelpers'.
--
-- Debugging helpers may perform IO. Do not use this in production.
debugTemplateIO :: (Applicative m, MonadIO m) => BMXState m -> Template -> m (Either BMXError Page)
debugTemplateIO st = renderTemplateM (st `usingHelpers` debugHelpers)

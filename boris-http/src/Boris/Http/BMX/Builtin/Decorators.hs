{-| The collection of builtin decorators, included in the default environment. -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.BMX.Builtin.Decorators where

import           Boris.Http.BMX.Data
import           Boris.Http.BMX.Function
import           Boris.Prelude


-- | The "inline" block decorator. Turns the block argument into a partial
-- with the name of the first argument.
inline :: (Applicative m, Monad m) => Decorator m
inline = blockDecorator $ \block k -> do
  name <- string
  liftBMX $ do
    let newPartial = partial (eval block)
    withPartial name newPartial k

builtinDecorators :: (Applicative m, Monad m) => [(Text, Decorator m)]
builtinDecorators = [
    ("inline", inline)
  ]

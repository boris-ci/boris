{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Eval.Function (
  -- * Arg parsers
    value
  , string
  , number
  , boolean
  , context
  , list
  , nullable
  , nullv
  , undef
  -- * Running BMX functions
  , runHelper
  , runBlockHelper
  , runPartial
  , withDecorator
  , withBlockDecorator
  ) where

import           Data.Scientific (Scientific)

import           BMX.Data

import           P

-- -----------------------------------------------------------------------------
-- Argument parsers for helpers / decorators

-- | The @any@ combinator. Parse a single 'Value' of any variety.
value :: Monad m => FunctionT m Value
value = one "value" return

-- | Parse a single 'StringV'.
string :: Monad m => FunctionT m Text
string = one "string" isString
  where isString (StringV t) = Just t
        isString _ = Nothing

-- | Parse a single 'IntV'.
number :: Monad m => FunctionT m Scientific
number = one "number" isNum
  where isNum (NumberV i) = Just i
        isNum _ = Nothing

-- | Parse a single 'BoolV'.
boolean :: Monad m => FunctionT m Bool
boolean = one "boolean" isBool
  where isBool (BoolV b) = Just b
        isBool _ = Nothing

-- | Parse a single 'ContextV'.
context :: Monad m => FunctionT m Context
context = one "context" isContext
  where isContext (ContextV c) = Just c
        isContext _ = Nothing

-- | Parse a single 'ListV'.
list :: Monad m => FunctionT m [Value]
list = one "list" isList
  where isList (ListV l) = Just l
        isList _ = Nothing

-- | Returns @Just val@ if the parser succeeds, or @Nothing@ if the next
-- 'Value' is 'NullV'. Throws a type error otherwise.
--
-- > nullable f = (Just <$> f) <|> (nullv *> pure Nothing)
nullable :: (Applicative m, Monad m) => FunctionT m a -> FunctionT m (Maybe a)
nullable f = (Just <$> f) <|> (nullv *> pure Nothing)

-- | Parse a single 'NullV'.
nullv :: Monad m => FunctionT m Value
nullv = one "null" isNull
  where isNull NullV = Just NullV
        isNull _ = Nothing

-- | Parse a single 'UndefinedV'.
undef :: Monad m => FunctionT m Value
undef = one "undefined" isUndef
  where isUndef UndefinedV = Just UndefinedV
        isUndef _ = Nothing

-- -----------------------------------------------------------------------------
-- Running / using a helper, partial or decorator

runHelper :: Monad m => [Value] -> Helper m -> BMX m Value
runHelper v (Helper help) = case help of
  BlockHelperT _ -> err (TypeError NoInfo "helper" "block helper") -- FIX
  HelperT h -> runFunctionT v [] h >>= either helpE return

runBlockHelper :: Monad m => [Value] -> [Param] -> Template -> Template -> Helper m -> BMX m Page
runBlockHelper v bparams ifp elsep (Helper help) = case help of
  HelperT _ -> err (TypeError NoInfo "block helper" "helper") -- FIX
  BlockHelperT h -> do
    fun <- runFunctionT v bparams (h ifp elsep)
    either helpE return fun

-- | Run a partial in the current environment, returning a @Page@.
-- Assumes @partial-block has been registered by caller.
runPartial :: (Applicative m, Monad m) => Partial m -> BMX m Page
runPartial (Partial (PartialT p)) = p

-- | Run a Decorator, then a continuation in the same environment
withDecorator :: Monad m => [Value] -> Decorator m -> BMX m Page -> BMX m Page
withDecorator v (Decorator deco) k = case deco of
  BlockDecoratorT _ -> err (TypeError NoInfo "decorator" "block decorator") -- FIX
  DecoratorT d -> runFunctionT v [] (d k) >>= either decoE return

-- | Run a block decorator, then a continuation
withBlockDecorator :: Monad m => [Value] -> Template -> Decorator m -> BMX m Page -> BMX m Page
withBlockDecorator v b (Decorator deco) k = case deco of
  DecoratorT _ -> err (TypeError NoInfo "block decorator" "decorator") -- FIX locations
  BlockDecoratorT d -> runFunctionT v [] (d b k) >>= either decoE return

helpE :: Monad m => FunctionError -> BMX m a
helpE = err . FunctionError NoInfo "helper" -- FIX locations

decoE :: Monad m => FunctionError -> BMX m a
decoE = err . FunctionError NoInfo "decorator" -- FIX locations

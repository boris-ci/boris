{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides everything you need to implement custom BMX helpers,
-- partials and decorators.

module BMX.Function (
  -- * Helpers
    Helper
  , helper
  , blockHelper
  -- * Decorators
  , Decorator
  , decorator
  , blockDecorator
  -- * Partials
  , Partial
  , partial
  -- * Values
  , Value (..)
  , Context (..)
  , Param (..)
  -- * Data variables
  , DataVar (..)
  -- * Argument parsers
  -- $vaarg
  , FunctionT
  , value
  , nullable
  , optional
  , eitherA
  , one
  , rest
  , string
  , number
  , boolean
  , context
  , list
  , param
  -- * BMX
  , BMX
  , eval
  , bmxError
  , liftBMX
  -- ** Changing stuff
  , withVariable
  , redefineVariable
  , withContext
  , withName
  , withData
  , withPartial
  -- ** Querying stuff
  , lookupValue
  , lookupData
  , lookupHelper
  , lookupDecorator
  -- ** Consuming functions
  -- $consumer
  , runHelper
  , runBlockHelper
  , runPartial
  , withDecorator
  , withBlockDecorator
  -- ** Utility functions
  , contextToList
  , contextFromList
  ) where

import           BMX.Data
import           BMX.Eval (eval)
import           BMX.Eval.Function

import           P

-- $vaarg
--
-- Helpers and decorators can take an arbitrary number of 'Value'
-- arguments.  They may need to be of a fixed arity or constrained by
-- the types of their arguments. The 'FunctionT' transformer provides
-- facilities to do this in a uniform way - simple parser combinators
-- with an 'Alternative' interface.
--
-- * A type error will be thrown if an argument is not as expected.
--
-- * All arguments passed to a function must be consumed, or an error
--   will be thrown. Functions may explicitly ignore all trailing
--   arguments if desired, using @many value@.
--
-- * An error will be thrown if not enough arguments are passed, i.e.
--   if a parser is run against an empty input.
--
-- All Helpers and Decorators are specified in terms of 'FunctionT'.
-- For example, here we have a "sum" helper that crashes unless given
-- exactly two numbers:
--
-- > sum :: (Applicative m, Monad m) => Helper m
-- > sum = helper $ do
-- >   i1 <- number
-- >   i2 <- number
-- >   return (IntV (i1 + i2))
--
-- We can extend "sum" for an arbitrary-size collection of numbers
-- using functions from 'Alternative', in this case 'many':
--
-- > sum :: (Applicative m, Monad m) => Helper m
-- > sum = helper $ do
-- >   vals <- many number
-- >   return . IntV $ foldl' sumVal 0 vals
--
-- Likewise, we can apply functions like @(\<|\>)@, 'optional', and
-- 'eitherA' to express more complicated functions. All the
-- constructors of 'Value' are exported, and can be used with the
-- permissive 'value' parser to build more general functions.
--
-- Once all argument and block parameter parsing is finished, we may
-- wish to access the local state or evaluate one of our block
-- arguments. The functions to do so are all of type @BMX m a@, so
-- we need to lift them into 'FunctionT' with 'liftBMX'.
--
-- As an example, this 'blockHelper' accepts 10 arguments that are
-- either booleans or strings, ignores them, then renders and
-- concatenates both of its block arguments:
--
-- > spam :: (Applicative m, Monad m) => Helper m
-- > spam = blockHelper $ \left right -> do
-- >   _vals <- replicateM 10 (eitherA boolean string)
-- >   liftBMX $ do
-- >      l <- eval left
-- >      r <- eval right
-- >      return (l <> r)

-- $consumer
--
-- These functions are called directly by the evaluator when making
-- use of a 'Partial', 'Decorator' or 'Helper'. If, for some reason,
-- you wish to run a helper, partial or decorator from inside another
-- helper, partial, or decorator, these are the functions for you.


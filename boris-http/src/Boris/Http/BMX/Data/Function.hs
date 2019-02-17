{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.Function (
  -- * Argument-parsing transformer
    FunctionT
  , runFunctionT
  , liftBMX
  , one
  , rest
  , param
  -- * Function arity and type errors
  , FunctionError (..)
  , renderFunctionError
  -- * The various types of function
  , HelperT (..)
  , PartialT (..)
  , DecoratorT (..)
  ) where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           X.Control.Monad.Trans.Either

import           BMX.Data.AST
import           BMX.Data.Error
import           BMX.Data.Page
import           BMX.Data.Value

import           P

-- -----------------------------------------------------------------------------
-- FunctionT

-- | The FunctionT transformer adds va_arg-style argument list parsing to a monad stack.
-- All Helpers and Decorators are defined in terms of FunctionT.
newtype FunctionT m a = FunctionT { fun :: EitherT FunctionError (StateT FunctionState m) a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans FunctionT where
  lift = FunctionT . lift . lift

instance MonadIO m => MonadIO (FunctionT m) where
  liftIO = FunctionT . liftIO

instance (Applicative m, Monad m) => Alternative (FunctionT m) where
  empty = FunctionT (left EOF)
  a <|> b = do
    st <- FunctionT get
    (r, st') <- (FunctionT . lift . lift) (runFunctionT' st a)
    case r of
      Left _ -> b
      Right v -> FunctionT (put st' >> return v)

data FunctionState = FS [Value] [Param]

-- | Run the arg parser and return the rest of the computation in the inner monad.
runFunctionT :: Monad m => [Value] -> [Param] -> FunctionT m a -> m (Either FunctionError a)
runFunctionT s p f = do
  (a, _) <- runFunctionT' (FS s p) (do a <- f; _ <- eof; return a) -- f <* eof
  return a

runFunctionT' :: Monad m => FunctionState -> FunctionT m a -> m (Either FunctionError a, FunctionState)
runFunctionT' s = (flip runStateT) s . runEitherT . fun

-- | Lift an action of type @BMX m a@ action into 'FunctionT'.
liftBMX :: (Monad m, Monad (t m), MonadTrans t) => t m a -> FunctionT (t m) a
liftBMX = FunctionT . lift . lift

-- | Match a single item using a Value predicate. If the predicate
-- fails with 'Nothing', the rule name is used to construct an appropriate
-- error message.
one :: Monad m => Text -> (Value -> Maybe a) -> FunctionT m a
one rule p = FunctionT $ do
  (FS st ps) <- get
  case st of
    [] -> left EOF
    (x:xs) -> maybe
      (left (Mismatch rule (renderValueType x)))
      (\a -> (put (FS xs ps)) >> return a)
      (p x)

-- | Match all remaining items. This is functionally equivalent to
-- 'many f', but throws a much nicer type error, because it doesn't
-- give up on failure.
rest :: (Applicative m, Monad m) => FunctionT m a -> FunctionT m [a]
rest f = many f <* (eof <|> void f)

eof :: Monad m => FunctionT m ()
eof = FunctionT $ do
  (FS st _) <- get
  if null st then return () else left (Trailing (length st))

-- | Grab one block parameter.
param :: Monad m => FunctionT m Param
param = FunctionT $ do
  (FS st ps) <- get
  case ps of
    (x:xs) -> put (FS st xs) >> return x
    _ -> left NoParams


-- -----------------------------------------------------------------------------
-- Functions (helpers, partials, decorators)

data HelperT m
  -- | Helpers are straightforward local operations on the BMX state, yielding a value
  = HelperT (FunctionT m Value)
  -- | Block helpers render a page from a main branch and an inverse (else)
  | BlockHelperT (Template -> Template -> FunctionT m Page)

data PartialT m
  -- | Partials are just BMX actions producing a Page.
  -- In practice, they will probably be lazy, fully-applied calls to eval.
  -- Partials invoked as blocks can access a template fragment, @partial-block.
  = PartialT (m Page)

data DecoratorT m
  -- | Decorators make local changes to the BMX state, and accept a continuation
  = DecoratorT (m Page -> FunctionT m Page)
  -- | Block decorators accept a template fragment and a continuation
  | BlockDecoratorT (Template -> m Page -> FunctionT m Page)

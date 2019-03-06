{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Prelude.EitherT (
    EitherT
  , Except.ExceptT
  , newEitherT
  , runEitherT
  , eitherT
  , mapEitherT
  , bimapEitherT
  , firstEitherT
  , secondEitherT
  , left
  , hoistEither
  , bracketEitherT'
  ) where

import           Control.Monad.Catch (MonadMask (..), SomeException (..), throwM, catchAll)

import           Control.Applicative (Applicative (..))
import           Control.Monad (Monad (..), (>>=), liftM)
import qualified Control.Monad.Trans.Except as Except

import           Data.Either (Either (..), either)
import           Data.Function ((.), ($), id, const)
import           Data.Functor (Functor (..))


type EitherT =
  Except.ExceptT

newEitherT :: f (Either e a) -> EitherT e f a
newEitherT =
  Except.ExceptT

runEitherT :: EitherT e f a -> f (Either e a)
runEitherT =
  Except.runExceptT

eitherT :: Monad m => (x -> m b) -> (a -> m b) -> EitherT x m a -> m b
eitherT f g m =
  runEitherT m >>= either f g

left :: Applicative f => e -> EitherT e f a
left =
  newEitherT . pure . Left

mapEitherT :: (m (Either x a) -> n (Either y b)) -> EitherT x m a -> EitherT y n b
mapEitherT f =
  newEitherT . f . runEitherT

bimapEitherT :: Functor m => (x -> y) -> (a -> b) -> EitherT x m a -> EitherT y m b
bimapEitherT f g =
  mapEitherT (fmap (either (Left . f) (Right . g)))

firstEitherT :: Functor m => (x -> y) -> EitherT x m a -> EitherT y m a
firstEitherT f =
  bimapEitherT f id

secondEitherT :: Functor m => (a -> b) -> EitherT x m a -> EitherT x m b
secondEitherT f =
  bimapEitherT id f

hoistEither :: Monad m => Either x a -> EitherT x m a
hoistEither =
  newEitherT . return

bracketEitherT' :: MonadMask m => EitherT e m a -> (a -> EitherT e m c) -> (a -> EitherT e m b) -> EitherT e m b
bracketEitherT' acquire release run =
  newEitherT $ bracketF
    (runEitherT acquire)
    (\r -> case r of
      Left _ ->
        -- Acquire failed, we have nothing to release
        return . Right $ ()
      Right r' ->
        -- Acquire succeeded, we need to try and release
        runEitherT (release r') >>= \x -> return $ case x of
          Left err -> Left (Left err)
          Right _ -> Right ())
    (\r -> case r of
      Left err ->
        -- Acquire failed, we have nothing to run
        return . Left $ err
      Right r' ->
        -- Acquire succeeded, we can do some work
        runEitherT (run r'))

data BracketResult a =
    BracketOk a
  | BracketFailedFinalizerOk SomeException
  | BracketFailedFinalizerError a

-- Bracket where you care about the output of the finalizer. If the finalizer fails
-- with a value level fail, it will return the result of the finalizer.
-- Finalizer:
--  - Left indicates a value level fail.
--  - Right indicates that the finalizer has a value level success, and its results can be ignored.
--
bracketF :: MonadMask m => m a -> (a -> m (Either b c)) -> (a -> m b) -> m b
bracketF a f g =
  mask $ \restore -> do
    a' <- a
    x <- restore (BracketOk `liftM` g a') `catchAll`
           (\ex -> either BracketFailedFinalizerError (const $ BracketFailedFinalizerOk ex) `liftM` f a')
    case x of
      BracketFailedFinalizerOk ex ->
        throwM ex
      BracketFailedFinalizerError b ->
        return b
      BracketOk b -> do
        z <- f a'
        return $ either id (const b) z

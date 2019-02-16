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
  ) where

import           Control.Applicative (Applicative (..))
import           Control.Monad (Monad (..), (>>=))
import qualified Control.Monad.Trans.Except as Except

import           Data.Either (Either (..), either)
import           Data.Function ((.), id)
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

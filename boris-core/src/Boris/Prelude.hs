{-# LANGUAGE NoImplicitPrelude #-}
module Boris.Prelude (
    module X
  , fromMaybeM
  , whenM
  , unlessM
  , with
  , bind
  , valueOrEmpty
  , emptyOrValue
  , orEmpty
  , maybeToLeft
  , maybeToRight
  , leftToMaybe
  , rightToMaybe
  , head
  ) where

import           Boris.Prelude.EitherT as X
import           Control.Applicative as X
import           Control.Monad as X
import           Control.Monad.Trans.Bifunctor as X (BifunctorTrans (..))
import           Data.Bits as X (Bits (..))
import           Data.Bool as X (Bool (..), (||), (&&), not, bool, otherwise)
import           Data.Char as X (Char)
import           Data.Bifunctor as X (Bifunctor (..))
import           Data.Either as X (Either (..), either)
import           Data.Foldable as X
import           Data.Function as X ((.), ($), (&), flip, id, const, on)
import           Data.Functor as X (($>))
import           Data.Int as X
import           Data.Maybe as X (Maybe (..), isNothing, isJust, maybe, fromMaybe, catMaybes, listToMaybe)
import           Data.Monoid as X (Monoid (..), (<>))
import           Data.Text as X (Text)
import           Data.Traversable as X
import           Data.Word as X (Word8, Word16, Word32, Word64)
import           Prelude as X (Eq (..), Show (..), Ordering (..), Ord (..), Num (..), Enum, Bounded (..), Integral (..), Double, error, seq, fromIntegral, (/), (^), fst, snd, Integer, Real (..), floor, round, subtract, fromRational, uncurry)
import           Text.Read as X (Read (..), readMaybe)


fromMaybeM :: Applicative f => f a -> Maybe a -> f a
fromMaybeM =
  flip maybe pure

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m =
  p >>= flip when m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m =
  p >>= flip unless m

with :: Functor f => f a -> (a -> b) -> f b
with =
  flip fmap

bind :: Monad m => (a -> m b) -> m a -> m b
bind =
  (=<<)
{-# INLINE bind #-}

valueOrEmpty :: Alternative f => Bool -> a -> f a
valueOrEmpty b a =
  if b then pure a else empty

emptyOrValue :: Alternative f => Bool -> a -> f a
emptyOrValue =
  valueOrEmpty . not

orEmpty :: (Alternative f, Monoid a) => f a -> f a
orEmpty f =
  f <|> pure mempty

maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r =
  maybe (Right r) Left

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l =
  maybe (Left l) Right

leftToMaybe :: Either l r -> Maybe l
leftToMaybe =
  either Just (const Nothing)

rightToMaybe :: Either l r -> Maybe r
rightToMaybe =
  either (const Nothing) Just

head :: (Foldable f) => f a -> Maybe a
head = foldr (\x _ -> return x) Nothing

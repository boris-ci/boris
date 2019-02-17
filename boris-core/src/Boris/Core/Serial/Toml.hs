{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Boris.Core.Serial.Toml (
    _VTable
  , _VTArray
  , _VString
  , _VInteger
  , _VFloat
  , _VBoolean
  , _VDatetime
  , _VArray
  , key
  ) where

import           Boris.Prelude

import           Control.Lens (Traversal', Prism', prism, ix)
import           Data.Time (UTCTime)
import qualified Data.Vector as Vector

import           Text.Toml.Types


_VTable :: Prism' Node Table
_VTable =
  prism  VTable $ \n -> case n of
    VTable v -> pure v
    _ -> Left n

_VTArray :: Prism' Node [Table]
_VTArray =
  prism  (VTArray . Vector.fromList) $ \n -> case n of
    VTArray v -> pure (Vector.toList v)
    _ -> Left n

_VString :: Prism' Node Text
_VString =
  prism  VString $ \n -> case n of
    VString v -> pure v
    _ -> Left n

_VInteger :: Prism' Node Int64
_VInteger =
  prism VInteger $ \n -> case n of
    VInteger v -> pure v
    _ -> Left n

_VFloat :: Prism' Node Double
_VFloat =
  prism VFloat $ \n -> case n of
    VFloat v -> pure v
    _ -> Left n

_VBoolean :: Prism' Node Bool
_VBoolean =
  prism VBoolean $ \n -> case n of
    VBoolean v -> pure v
    _ -> Left n

_VDatetime :: Prism' Node UTCTime
_VDatetime =
  prism VDatetime $ \n -> case n of
    VDatetime v -> pure v
    _ -> Left n

_VArray :: Prism' Node [Node]
_VArray =
  prism (VArray . Vector.fromList) $ \n -> case n of
    VArray v -> pure (Vector.toList v)
    _ -> Left n

key :: Text -> Traversal' Table Node
key = ix

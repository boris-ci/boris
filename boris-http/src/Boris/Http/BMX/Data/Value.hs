{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module BMX.Data.Value (
    Context (..)
  , Value (..)
  , Param (..)
  , contextFromList
  , contextToList
  , renderValue
  , renderValueType
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Scientific (Scientific, floatingOrInteger)
import qualified Data.Text as T

import           P

data Context = Context (Map Text Value)
  deriving (Eq, Show)

instance Monoid Context where
  mempty = Context mempty
  mappend (Context a) (Context b) = Context (M.union a b)

-- | Runtime values. The things variables point at while rendering happens.
--
-- Not to be confused with user-provided 'BMXValue'.
data Value
  = StringV Text
  | NumberV Scientific
  | BoolV Bool
  | NullV
  | UndefinedV
  | ContextV Context
  | ListV [Value]
  deriving (Eq, Show)

newtype Param = Param { renderParam :: Text }
  deriving (Eq, Show)

-- | Construct an association list from a @Context@.
contextToList :: Context -> [(Text, Value)]
contextToList (Context c) = M.toList c

-- | Construct a @Context@ from an association list.
contextFromList :: [(Text, Value)] -> Context
contextFromList = Context . M.fromList

renderValue :: Value -> Text
renderValue = \case
  StringV t -> t
  NumberV i -> T.pack $ either show show (floatingOrInteger i :: Either Double Integer)
  BoolV b -> if b then "true" else "false"
  NullV -> "null"
  UndefinedV -> "undefined"
  ContextV _ -> "(object)"
  ListV _ -> "(list)"

renderValueType :: Value -> Text
renderValueType = \case
  StringV _ -> "string"
  NumberV _ -> "number"
  BoolV _ -> "boolean"
  NullV -> "null"
  UndefinedV -> "undefined"
  ContextV _ -> "context"
  ListV _ -> "list"

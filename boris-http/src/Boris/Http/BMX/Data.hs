{- | Frontend types, and re-exports all data. Internal -}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data (
    BMXState (..)
  , BMXValue (..)
  , usingContext
  , usingPartials
  , usingHelpers
  , usingDecorators
  , contextToJSON
  , contextFromJSON
  , packState
  , maybeNull
  , module X
  ) where

import           BMX.Data.AST as X
import           BMX.Data.Error as X
import           BMX.Data.Eval as X
import           BMX.Data.Format as X
import           BMX.Data.Function as X
import           BMX.Data.Page as X
import           BMX.Data.Position as X
import           BMX.Data.Token as X
import           BMX.Data.Value as X

import qualified Data.HashMap.Strict as H
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Scientific (Scientific)
import           X.Data.Aeson ((.=), object)
import qualified X.Data.Aeson as A

import           P

-- | BMXState holds the initial rendering environment, i.e. all bound helpers,
-- partials, decorators, and the current variable context.
--
-- The type parameter @m@ refers to the base monad for the registered
-- helpers, partials and decorators. It is commonly 'Identity' or
-- 'IO', though it should be possible to render a 'Template' on top of
-- any monad stack.
data BMXState m = BMXState
  { bmxContext :: [(Text, BMXValue)]
  , bmxPartials :: [(Text, Partial m)]
  , bmxHelpers :: [(Text, Helper m)]
  , bmxDecorators :: [(Text, Decorator m)]
  }

instance Monoid (BMXState m) where
  mempty = BMXState mempty mempty mempty mempty
  mappend a b = BMXState {
      bmxContext = bmxContext a <> bmxContext b
    , bmxPartials = bmxPartials a <> bmxPartials b
    , bmxHelpers = bmxHelpers a <> bmxHelpers b
    , bmxDecorators = bmxDecorators a <> bmxDecorators b
    }

-- | User-provided data, to be packed into a 'Context' and used for rendering.
data BMXValue
  = BMXString !Text
  | BMXNum !Scientific
  | BMXBool !Bool
  | BMXContext ![(Text, BMXValue)]
  | BMXList ![BMXValue]
  | BMXNull
  deriving (Eq, Show)

-- | Parse a context from JSON.
contextFromJSON :: A.Value -> A.Parser [(Text, BMXValue)]
contextFromJSON val = case val of
  A.Object o -> liftM (sortOn fst) $
    mapM (\(k, v) -> (,) <$> pure k <*> valueFromJSON v) (H.toList o)
  _ -> mzero

-- | Serialise a context to JSON.
contextToJSON :: [(Text, BMXValue)] -> A.Value
contextToJSON = object . fmap (\(k, v) -> k .= valueToJSON v) . sortOn fst

-- | Parse a value from JSON.
valueFromJSON :: A.Value -> A.Parser BMXValue
valueFromJSON val = case val of
  o@(A.Object _) -> BMXContext <$> contextFromJSON o
  A.String t -> pure (BMXString t)
  A.Bool b -> pure (BMXBool b)
  A.Number i -> pure (BMXNum i)
  A.Array a -> BMXList . toList <$> mapM valueFromJSON a
  A.Null -> pure BMXNull

-- | Serialise a value to JSON.
valueToJSON :: BMXValue -> A.Value
valueToJSON val = case val of
  BMXNum i -> A.toJSON i
  BMXString t -> A.toJSON t
  BMXBool b -> A.toJSON b
  BMXList l -> A.toJSON $ fmap valueToJSON l
  BMXContext l -> contextToJSON l
  BMXNull -> A.Null

-- | Set the initial context in an 'BMXState'.
usingContext :: (Applicative m, Monad m) => BMXState m -> [(Text, BMXValue)] -> BMXState m
usingContext st c = st { bmxContext = c }

-- | Add a named collection of partials to the 'BMXState'.
usingPartials :: (Applicative m, Monad m) => BMXState m -> [(Text, Partial m)] -> BMXState m
usingPartials st ps = st { bmxPartials = ps <> bmxPartials st }

-- | Add a named collection of helpers to the 'BMXState'.
usingHelpers :: (Applicative m, Monad m) => BMXState m -> [(Text, Helper m)] -> BMXState m
usingHelpers st hs = st { bmxHelpers = hs <> bmxHelpers st }

-- | Add a named collection of decorators to the 'BMXState'.
usingDecorators :: (Applicative m, Monad m) => BMXState m -> [(Text, Decorator m)] -> BMXState m
usingDecorators st ds = st { bmxDecorators = ds <> bmxDecorators st }

-- | Pack the association lists from 'BMXState' into the maps of 'EvalState',
-- throwing errors whenever shadowing is encountered.
packState :: (Applicative m, Monad m) => BMXState m -> Either BMXError (EvalState m)
packState bst = do
  ctx <- boxContext (bmxContext bst)
  partials <- mapUnique (Shadowing NoInfo "partial") (bmxPartials bst)
  helpers <- mapUnique (Shadowing NoInfo "helper") (bmxHelpers bst)
  decorators <- mapUnique (Shadowing NoInfo "decorator") (bmxDecorators bst)
  let dta = mempty
  return EvalState {
      evalContext = [ctx]
    , evalData = dta
    , evalHelpers = helpers
    , evalPartials = partials
    , evalDecorators = decorators
    , evalBreadcrumbs = mempty
    }

mapUnique :: (Text -> EvalErrorT) -> [(Text, a)] -> Either BMXError (Map Text a)
mapUnique e = foldM foldFun M.empty
  where foldFun m (k, v)  = if M.member k m
          then Left (BMXEvalError (EvalError (e k) mempty))
          else Right (M.insert k v m)

boxContext :: [(Text, BMXValue)] -> Either BMXError Context
boxContext c = do
  ctx <- mapUnique (Shadowing NoInfo "value") c
  ctx' <- mapM rebox ctx
  return (Context ctx')

rebox :: BMXValue -> Either BMXError Value
rebox v = case v of
  BMXString t -> pure (StringV t)
  BMXNum i -> pure (NumberV i)
  BMXBool b -> pure (BoolV b)
  BMXNull -> pure NullV
  BMXList ls -> ListV <$> mapM rebox ls
  BMXContext c -> ContextV <$> boxContext c

maybeNull :: (a -> BMXValue) -> Maybe a -> BMXValue
maybeNull f = maybe BMXNull f

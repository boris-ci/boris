{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
module Boris.Core.Serial.Toml (
    mapping
  ) where

import           Control.Monad (forM, forM_)
import           Control.Monad.Except (catchError, throwError)
import           Control.Monad.Reader (asks, local)
import           Control.Monad.State (execState, gets, modify)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Maybe (fromMaybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)

import qualified Toml
import qualified Toml.PrefixTree as Prefix
import           Toml.PrefixTree (pattern (:||))



-- |
-- A single top-level mapping. This is ridiculous.
--
-- Works for:
--
-- > [foo.bar]
-- > key = 1
-- > [foo.baz]
-- > key = 2
--
-- > Map.fromList [("bar", #codec ~ key = 1, "baz", #codec ~ key = 2)]
--
-- Doesn't work for:
--
-- [foo.bar.swizzle]
-- [foo.bar.swazzle]
--
mapping :: forall a . Toml.TomlCodec a -> Toml.Key -> Toml.TomlCodec (Map Text a)
mapping codec key@(p :|| _) =
  let
    input :: Toml.Env (Map Text a)
    input = do
      prefixMapping <- asks (HashMap.lookup p . Toml.tomlTables)
      case prefixMapping of
        Nothing   ->
          pure Map.empty
        Just prefixes ->
          case prefixes of
            Toml.Leaf _ _ ->
              pure Map.empty
            Toml.Branch prefix value suffixes ->
              let
                elements = HashMap.toList suffixes
              in
                fmap Map.fromList $ forM elements $ \(k, value) ->
                  case value of
                    Toml.Leaf _ toml ->
                      fmap (\v -> (Toml.unPiece k, v)) $
                        codecReadTOML toml codec `catchError` handleErrorInTable key
                    Toml.Branch _ _ _ ->
                      throwError $ Toml.TableNotFound key
    output :: (Map Text a) -> Toml.St (Map Text a)
    output a = do
        mTable <- gets $ Prefix.lookup key . Toml.tomlTables
        let toml = fromMaybe mempty mTable
        forM_ (Map.toList a) $ \(k, value) -> do
          let newToml = execState (runMaybeT $ Toml.codecWrite codec value) toml
          modify (Toml.insertTable (key <> (Toml.Piece k :|| [])) newToml)
        pure a
  in
    Toml.Codec input output

handleErrorInTable :: Toml.Key -> Toml.DecodeException -> Toml.Env a
handleErrorInTable key err =
  case err of
    Toml.KeyNotFound name ->
      throwError $ Toml.KeyNotFound (key <> name)
    Toml.TableNotFound name ->
      throwError $ Toml.TableNotFound (key <> name)
    Toml.TypeMismatch name t1 t2 ->
      throwError $ Toml.TypeMismatch (key <> name) t1 t2
    e ->
      throwError e


codecReadTOML :: Toml.TOML -> Toml.TomlCodec a -> Toml.Env a
codecReadTOML toml codec =
  local (const toml) (Toml.codecRead codec)

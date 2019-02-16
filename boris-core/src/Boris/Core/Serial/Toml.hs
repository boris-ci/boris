{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
module Boris.Core.Serial.Toml (
    mapping
  ) where

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



mapping :: forall a . Toml.TomlCodec a -> Toml.Key -> Toml.TomlCodec (Map Text a)
mapping codec key@(p :|| _) =
  let
    input :: Toml.Env (Map Text a)
    input = do
      Just xx  <- asks (HashMap.lookup p . Toml.tomlTables)

      error (mconcat [show key, "  /  ", show $ Prefix.lookupT key xx, "  /  ",
                case xx of
                  (Toml.Branch pref mv prefMap) ->
                    mconcat ["branch/",
                             show (Prefix.keysDiff pref key)]
                  (Toml.Leaf k v) ->
                    "leaf"
                    ])
      mTable <- asks $ Prefix.lookup key . Toml.tomlTables

      case mTable of
        Nothing   -> throwError $ Toml.TableNotFound key
        Just toml ->
          error "todo"
--          codecReadTOML toml codec `catchError` handleErrorInTable key
    output :: (Map Text a) -> Toml.St (Map Text a)
    output a = do
--        mTable <- gets $ Prefix.lookup key . Toml.tomlTables
--        let toml = fromMaybe mempty mTable
--        let newToml = execState (runMaybeT $ Toml.codecWrite codec a) toml
--        a <$ modify (Toml.insertTable key newToml)
        error "todo"
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

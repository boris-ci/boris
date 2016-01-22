{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Store.Lifecycle (
    initialise
  , destroy
  ) where

import           Boris.Core.Data
import           Boris.Store.Schema

import           Control.Lens (view, (^.))
import           Control.Monad.IO.Class (liftIO)

import           Data.Conduit ((=$=), ($$))
import qualified Data.Conduit.List as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S


import           Mismi (AWS)
import qualified Mismi.Amazonka as A

import qualified Network.AWS.DynamoDB as D

import           P


initialise :: Environment -> AWS ()
initialise e = do
  tables <- A.paginate D.listTables =$= C.mapFoldable (view D.ltrsTableNames) =$= C.filter (T.isPrefixOf (T.intercalate "." ["boris", renderEnvironment e])) $$ C.consume
  let
    indexed = S.fromList tables
    missing = filter (\t -> not $ S.member (t ^. D.ctTableName) indexed) $ schema e
  forM_ missing $ \t -> do
    liftIO . T.putStrLn . mconcat $ ["Creating table: ", t ^. D.ctTableName]
    void . A.send $ t
    liftIO . T.putStrLn . mconcat $ ["  ` done"]
  forM_ (schema e) $ \t -> do
    liftIO . T.putStrLn . mconcat $ ["Waiting for table: ", t ^. D.ctTableName]
    void . A.await D.tableExists . D.describeTable $ t ^. D.ctTableName
    liftIO . T.putStrLn . mconcat $ ["  ` done"]

destroy :: Environment -> AWS ()
destroy e =
  forM_ (schema e) $ \t -> do
    liftIO . T.putStrLn . mconcat $ ["Deleting table: ", t ^. D.ctTableName]
    void . A.send . D.deleteTable $ t ^. D.ctTableName
    void . A.await D.tableNotExists . D.describeTable $ t ^. D.ctTableName
    liftIO . T.putStrLn . mconcat $ ["  ` done"]

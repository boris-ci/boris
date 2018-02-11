{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Store.Dynamo.Results (
    add
  , addWithCompressLimit
  , fetchResults
  , compress
  , deleteItem

  , JsonError (..)
  , calculateDrops
  ) where

import           Boris.Core.Data
import           Boris.Http.Store.Dynamo.Schema

import           Control.Lens ((.~), (^.))
import           Control.Monad.Trans.Class (lift)

import           Data.Aeson (Value (..), object, withObject, (.=), (.:), (.:?))
import           Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import           Mismi (AWS)
import qualified Mismi.Amazonka as A

import qualified Network.AWS.DynamoDB as D

import           P

import           Spine.Data

import           X.Control.Monad.Trans.Either
import           X.Data.Aeson (asWith, asTextWith)

newtype JsonError =
  JsonError {
      jsonError :: Text
    } deriving (Eq, Show)

master :: Ref
master =
  Ref "refs/heads/master"

-- Calculate what results we can drop from the store. Drop all the
-- non-latest view of a (project, build, ref) grouping as well as any
-- successfully builds.
--
--   Given the store:
--     10 p dist refs/heads/master Ok
--     9 p dist refs/heads/master Ko
--     8 p dist refs/heads/master Ok
--     7 x branches refs/heads/topic/foo Ok
--     6 x dist refs/heads/master Ko
--
--   Will be compressed into:
--     6 x dist refs/heads/master Ko
--
calculateDrops :: [Result] -> [Result]
calculateDrops rs =
  let
    -- Group by (project, build, ref)
    groupd = with rs $ \(Result i p b r br) ->
      ((p, b, r), M.singleton i br)

    -- Flatten into latest id
    latest = M.fromListWith M.union groupd

    groups = join . with (M.toList latest) $ \((p, b, r), m) ->
      case M.maxViewWithKey m of
        Nothing ->
          []
        Just (top, rest) ->
          let
            restDrops = with (M.toList rest) $ \(i, br) ->
              Result i p b r br
          in
            -- Drop non-master refs, this includes build that failed
            -- before a ref could be selected.
            if r /= Just master then
              Result (fst top) p b r (snd top) : restDrops
            -- Drop ok builds
            else if (snd top) == BuildOk then
              Result (fst top) p b r (snd top) : restDrops
            -- Drop the views that are not the latest
            else
              restDrops
  in
    groups

add :: Environment -> Result -> EitherT JsonError AWS ()
add e r =
  addWithCompressLimit e 100 r

addWithCompressLimit :: Environment -> Int -> Result -> EitherT JsonError AWS ()
addWithCompressLimit e limit r = do
  x <- lift . A.send $ D.updateItem (renderTableName $ tResults e)
      & D.uiKey .~ H.fromList [
          vResults
        ]
      & D.uiUpdateExpression .~ Just (mconcat [
          "ADD ", renderKey kResults, renderKey $ kValSet "v"
        ])
      & D.uiReturnValues .~ Just D.AllNew
      & D.uiExpressionAttributeValues .~ H.fromList [
          toEncoding (kValSet "v") [asTextWith fromResult r]
        ]

  let
    results = maybe [] id $ fromEncoding_ kResults (x ^. D.uirsAttributes)
  -- Compress when the results get bigger then the defined limit
  case length results > limit of
    False ->
      pure ()
    True -> do
      rs <- firstT JsonError . hoistEither $ forM results $ \t ->
        asWith toResult t
      void $ compressResults e rs

fromResult :: Result -> Value
fromResult r =
  object [
      "version" .= ("v1" :: Text)
    , "id" .= (renderBuildId . resultBuildId) r
    , "project" .= (renderProject . resultProject) r
    , "build" .= (renderBuild . resultBuild) r
    , "ref" .= (fmap renderRef . resultRef) r
    , "result" .= (renderBuildResult . resultBuildResult) r
    ]

toResult :: Value -> Parser Result
toResult =
  withObject "Result" $ \o ->
    o .: "version" >>= \ver -> case ver of
      "v1" ->
        Result
          <$> (BuildId <$> o .: "id")
          <*> (Project <$> o .: "project")
          <*> (Build <$> o .: "build")
          <*> ((fmap . fmap) Ref $ o .:? "ref")
          <*> (o .: "result" >>= \t -> case t of
            "ok" ->
              pure BuildOk
            "ko" ->
              pure BuildKo
            _ ->
              fail $ "Unknown build result: " <> t)
      _ ->
        fail $ "Unknown result version: " <> ver

fetchResults :: Environment -> EitherT JsonError AWS [Result]
fetchResults e = do
  r <- lift . A.send $ D.getItem (renderTableName $ tResults e)
    & D.giKey .~ H.fromList [
        vResults
      ]
    & D.giProjectionExpression .~ Just (T.intercalate "," [
        renderKey kResults
      ])
    & D.giConsistentRead .~ Just False
  let
    attrs = r ^. D.girsItem
    results = maybe [] id $ fromEncoding_ kResults attrs
  firstT JsonError . hoistEither $ forM results $ \t ->
    asWith toResult t

compress :: Environment -> EitherT JsonError AWS [Result]
compress e = do
  rs <- fetchResults e
  compressResults e rs

compressResults :: Environment -> [Result] -> EitherT JsonError AWS [Result]
compressResults e rs = do
  let
    actions = calculateDrops rs

  case actions == [] of
    True ->
      pure rs
    False -> do
      x <- lift . A.send $ D.updateItem (renderTableName $ tResults e)
        & D.uiKey .~ H.fromList [
          vResults
        ]
        & D.uiReturnValues .~ Just D.AllNew
        & D.uiUpdateExpression .~ Just (mconcat [
            "DELETE ", renderKey kResults, renderKey $ kVal "v"
          ])
        & D.uiExpressionAttributeValues .~ H.fromList [
             toEncoding (kValSet "v") (asTextWith fromResult <$> actions)
          ]
      let
        results = maybe [] id $ fromEncoding_ kResults (x ^. D.uirsAttributes)
      firstT JsonError . hoistEither $ forM results $ \t ->
        asWith toResult t

deleteItem :: Environment -> AWS ()
deleteItem e =
  void . A.send $ D.deleteItem (renderTableName $ tResults e)
    & D.diKey .~ H.fromList [
        vResults
      ]

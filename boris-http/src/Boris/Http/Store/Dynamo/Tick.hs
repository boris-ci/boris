{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Store.Dynamo.Tick (
    next
  ) where

import           Boris.Core.Data
import           Boris.Http.Store.Dynamo.Schema

import           Control.Lens ((.~), (^.))
import           Control.Monad.Trans.Class (lift)

import qualified Data.HashMap.Strict as H


import           Mismi (AWS)
import qualified Mismi.Amazonka as A

import qualified Network.AWS.DynamoDB as D

import           P

import           Spine.Data (TableName (..), renderKey, toEncoding, fromEncoding_)

import           X.Control.Monad.Trans.Either (EitherT, left)


next :: Environment -> EitherT Environment AWS BuildId
next e = do
  r <- lift . A.send $ D.updateItem (renderTableName $ tTick e)
    & D.uiKey .~ H.fromList [
        vGlobal
      ]
    & D.uiUpdateExpression .~ Just (mconcat ["ADD ", renderKey kTick, renderKey $ kInt "v"])
    & D.uiReturnValues .~ Just D.UpdatedNew
    & D.uiExpressionAttributeValues .~ H.fromList [
        toEncoding (kInt "v") 1
      ]
  fromMaybeM (left $ e) $
    (BuildId . renderIntegral) <$> fromEncoding_ kTick (r ^. D.uirsAttributes)

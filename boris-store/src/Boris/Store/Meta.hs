{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Store.Meta (
    BuildIdError (..)
  , renderBuildIdError
  , delete
  , nextId
  ) where

import           Boris.Core.Data
import           Boris.Store.Schema

import           Control.Lens ((&), (.~), (^?), ix, to, _Just)
import           Control.Monad.Trans.Class (lift)

import           Data.Text (Text)
import qualified Data.HashMap.Strict as H


import           Mismi (AWS)
import qualified Mismi.Amazonka as A

import qualified Network.AWS.DynamoDB as D

import           P

import           X.Control.Monad.Trans.Either (EitherT, left)


data BuildIdError =
    BuildIdCouldNotBeGenerated Environment Project Build
    deriving (Eq, Show)

nextId :: Environment -> Project -> Build -> EitherT BuildIdError AWS BuildId
nextId e p b = do
  r <- lift . A.send $ D.updateItem (tMeta e)
    & D.uiKey .~ H.fromList [
        vProjectBuild p b
      ]
    & D.uiUpdateExpression .~ Just (mconcat ["ADD ", kBuildIdState, kVal "v"])
    & D.uiReturnValues .~ Just D.UpdatedNew
    & D.uiExpressionAttributeValues .~ H.fromList [
        vInt (kVal "v") 1
      ]
  fromMaybeM (left $ BuildIdCouldNotBeGenerated e p b) $
    r ^? D.uirsAttributes . ix kBuildIdState . D.avN . _Just . to BuildId

delete :: Environment -> Project -> Build -> AWS ()
delete e p b =
  void . A.send $ D.deleteItem (tMeta e)
    & D.diKey .~ H.fromList [
        vProjectBuild p b
      ]

renderBuildIdError :: BuildIdError -> Text
renderBuildIdError err =
  case err of
    BuildIdCouldNotBeGenerated e p b ->
      mconcat [
          "Could not determine next build id"
        , ": environment = ", renderEnvironment e
        , ", project = ", renderProject p
        , ", build = ", renderBuild b
        ]

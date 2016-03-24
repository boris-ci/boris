{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Store.Tick (
    TickError (..)
  , renderTickError
  , next
  ) where

import           Boris.Core.Data
import           Boris.Store.Schema

import           Control.Lens ((.~), (^?), ix, to, _Just)
import           Control.Monad.Trans.Class (lift)

import qualified Data.HashMap.Strict as H


import           Mismi (AWS)
import qualified Mismi.Amazonka as A

import qualified Network.AWS.DynamoDB as D

import           P

import           X.Control.Monad.Trans.Either (EitherT, left)


data TickError =
    BuildIdCouldNotBeGenerated Environment Project Build
    deriving (Eq, Show)

next :: Environment -> Project -> Build -> EitherT TickError AWS BuildId
next e p b = do
  r <- lift . A.send $ D.updateItem (tTick e)
    & D.uiKey .~ H.fromList [
        vGlobal
      ]
    & D.uiUpdateExpression .~ Just (mconcat ["ADD ", kTick, kVal "v"])
    & D.uiReturnValues .~ Just D.UpdatedNew
    & D.uiExpressionAttributeValues .~ H.fromList [
        vInt (kVal "v") 1
      ]
  fromMaybeM (left $ BuildIdCouldNotBeGenerated e p b) $
    r ^? D.uirsAttributes . ix kTick . D.avN . _Just . to BuildId

renderTickError :: TickError -> Text
renderTickError err =
  case err of
    BuildIdCouldNotBeGenerated e p b ->
      mconcat [
          "Could not determine next build id"
        , ": environment = ", renderEnvironment e
        , ", project = ", renderProject p
        , ", build = ", renderBuild b
        ]

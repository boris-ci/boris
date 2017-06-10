{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Representation.Scoreboard (
    GetScoreboard (..)
  , scoreboardHtml
  ) where

import           Boris.Core.Data
import           Boris.Store.Results (Result (..))

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Aeson.Types (Value)

import           P

import           Text.Blaze.Html (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

data GetScoreboard =
  GetScoreboard [Result]

instance ToJSON GetScoreboard where
  toJSON (GetScoreboard rs) =
    object [
        "builds" .= fmap fromResult rs
      ]

fromResult :: Result -> Value
fromResult r =
  object [
      "build_id" .= (renderBuildId . resultBuildId) r
    , "project" .= (renderProject . resultProject) r
    , "build" .= (renderBuild . resultBuild) r
    , "ref" .= (renderRef . resultRef) r
    , "result" .= (renderBuildResult . resultBuildResult) r
    ]


scoreboardHtml :: [Result] -> Html
scoreboardHtml rs = let
  allOk = all ((== BuildOk) . resultBuildResult) rs
  buildClass = case allOk of
    False -> "notOk"
    True -> "ok"
  in
    H.html $ do
      H.head $ do
        H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "/assets/css/scoreboard.css"
        H.script ! HA.src "/assets/js/updateSelf.js" $ ""
      H.body ! HA.class_ buildClass $ ""

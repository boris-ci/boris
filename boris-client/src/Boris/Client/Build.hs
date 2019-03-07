{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Build (
    trigger
  , next
  , cancel
  , fetch
  , list
  , ignore
  , rebuild
  , queue
  , heartbeat
  , acknowledge
  , avow
  , complete
  ) where


import           Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Boris.Client.Response as Response
import           Boris.Client.Request (Request (..))
import qualified Boris.Client.Request as Request
import qualified Boris.Client.Serial.Encode as Encode
import qualified Boris.Client.Serial.Decode as Decode
import           Boris.Core.Data.Build
import           Boris.Core.Data.Keyed
import           Boris.Core.Data.Project
import           Boris.Prelude
import           Boris.Representation.ApiV1

import qualified Data.Text as Text

import qualified Network.HTTP.Types as HTTP


-- TODO HANDLE 404
trigger :: ProjectName -> BuildName -> Maybe Ref -> Request (Keyed BuildId Build)
trigger p b r =
  Request HTTP.POST "build"
    (Response.json 201 $ Decode.wrapper getBuild)
    (Request.json . Encode.auto $ PostBuildRequest p b r)

{--  fmap getBuild $
    H.post  ()
--}

next :: Request (Maybe (Keyed BuildId Build))
next =
  Request HTTP.POST "queue"
    (Response.option . Response.json 200 $ Decode.wrapper getBuild)
    (Request.json . Encode.auto $ ())

fetch :: BuildId -> Request (Maybe BuildData)
fetch i =
  error "todo" {--
  (fmap . fmap) getBuild $
    H.get ["build", renderBuildId i]
--}

cancel :: BuildId -> Request ()
cancel i =
  error "todo" {--
  H.delete ["build", renderBuildId i]
--}

list :: Project -> Build -> Request (Maybe BuildTree)
list p b =
  error "todo"
  {--
  (fmap . fmap) getBuilds $
    H.get ["project", renderProject p , "build", renderBuild b]
--}

ignore :: ProjectName -> BuildName -> Bool -> Request ()
ignore p b i =
  Request HTTP.PUT (Text.intercalate "/" ["project", renderProjectName p, "build", renderBuildName b, "ignore"])
    (Response.none 200)
    (Request.json . Encode.auto $ PutBuildIgnore i)

heartbeat :: BuildId -> Request BuildCancelled
heartbeat i =
  Request HTTP.POST (Text.intercalate "/" ["build", renderBuildId i, "heartbeat"])
    (Response.json 200 $ Decode.wrapper postHeartbeatCancelled)
    (Request.json . Encode.auto $ ())

acknowledge :: BuildId -> Request Acknowledge
acknowledge i =
  Request HTTP.POST (Text.intercalate "/" ["build", renderBuildId i, "acknowledge"])
    (Response.json 200 $ Decode.wrapper postAcknowledge)
    (Request.json . Encode.auto $ PostAcknowledgeRequest)

avow :: BuildId -> Ref -> Commit -> Request ()
avow i r c =
  Request HTTP.POST (Text.intercalate "/" ["build", renderBuildId i, "avow"])
    (Response.json 200 $ Decode.wrapper (\PostAvowResponse -> ()))
    (Request.json . Encode.auto $ PostAvowRequest r c)

complete :: BuildId -> BuildResult -> Request ()
complete i r =
  Request HTTP.POST (Text.intercalate "/" ["build", renderBuildId i, "complete"])
    (Response.json 200 $ Decode.wrapper (\PostCompleteResponse -> ()))
    (Request.json . Encode.auto $ Aeson.object [
        "result" .= case r of BuildOk -> True; BuildKo -> False
      ])

rebuild :: BuildId -> Request (Maybe BuildData)
rebuild i =
  error "todo"
  {--
  m <- fetch i
  case m of
    Nothing ->
      pure Nothing
    Just d ->
      (fmap . fmap) getBuild $
        H.post [
            "project", renderProject $ buildDataProject d
          , "build", renderBuild $ buildDataBuild d
          ] (PostBuildRequest $ buildDataRef d)
--}

queue :: Request (Maybe Int64)
queue =
  error "todo"
  {--
  (fmap . fmap) getQueue $
    H.get ["queue"]
--}

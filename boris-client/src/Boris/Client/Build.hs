{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Build (
    trigger
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


import qualified Boris.Client.Response as Response
import           Boris.Client.Request (Request (..))
import qualified Boris.Client.Request as Request
import qualified Boris.Client.Serial.Encode as Encode
import qualified Boris.Client.Serial.Decode as Decode
import           Boris.Core.Data.Build
import           Boris.Core.Data.Project
import           Boris.Prelude
import           Boris.Representation.ApiV1

import qualified Data.Text as Text

import qualified Network.HTTP.Types as HTTP


trigger :: Project -> Build -> Maybe Ref -> Request BuildData
trigger p b r =
  Request HTTP.POST (Text.intercalate "/" ["project", renderProject p , "build", renderBuild b])
    (Response.json 201 $ Decode.wrapper getBuild)
    (Request.json . Encode.auto $ PostBuildRequest r)


{--  fmap getBuild $
    H.post  ()
--}

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

ignore :: Project -> Build -> Bool -> Request ()
ignore p b i =
  error "todo"
  {--
  H.put ["project", renderProject p , "build", renderBuild b, "ignore"] (PutBuildIgnore i)
--}
heartbeat :: BuildId -> Request BuildCancelled
heartbeat i =
  error "todo"
  {--
  fmap postHeartbeatCancelled $
    H.post ["build", renderBuildId i, "heartbeat"] ()
--}

acknowledge :: BuildId -> Request Acknowledge
acknowledge i =
  error "todo"
  {--
  fmap postAcknowledge $
    H.post ["build", renderBuildId i, "acknowledge"] PostAcknowledgeRequest
--}

avow :: BuildId -> Ref -> Commit -> Request ()
avow i r =
  error "todo"
  {--

  PostAvowResponse <- H.post config ["build", renderBuildId i, "avow"] $
    PostAvowRequest r c
  pure ()
--}

complete :: BuildId -> BuildResult -> Request ()
complete i r =
  error "todo"
  {--

  PostCompleteResponse <- H.post config ["build", renderBuildId i, "complete"] $ object [
      "result" .= case r of BuildOk -> True; BuildKo -> False
    ]
  pure ()
--}

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

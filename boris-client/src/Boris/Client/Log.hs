{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Log (
    source
  , source'
  ) where

import           Boris.Core.Data

import           Control.Lens (view)

import           Data.Conduit (Source, (=$=))
import qualified Data.Conduit.List as CL

import           Data.Text (Text)
import qualified Data.Text as T

import           Jebediah.Data (Following (..), GroupName (..), StreamName (..))
import           Jebediah.Control (retrieveLogStream')

import           Mismi (AWS)
import qualified Mismi.CloudwatchLogs.Amazonka as CW

import           P


source :: Environment -> BuildId -> Source AWS Text
source e i =
  let
    gname = GroupName . T.intercalate "." $ ["boris", renderEnvironment e]
    sname = StreamName $ renderBuildId i
  in
    source' gname sname

source' :: GroupName -> StreamName -> Source AWS Text
source' gname sname =
  retrieveLogStream' gname sname Nothing Nothing Nothing (Follow 1)
    =$= CL.mapFoldable (view CW.oleMessage)

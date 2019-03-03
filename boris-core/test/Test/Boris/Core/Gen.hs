{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Boris.Core.Gen where

import           Boris.Core.Data.Agent
import           Boris.Core.Data.Build
import           Boris.Core.Data.Configuration
import           Boris.Core.Data.Log
import           Boris.Core.Data.Project
import           Boris.Prelude

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day (..), DiffTime, UTCTime (..))

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genRef :: Gen Ref
genRef =
  Ref <$> Gen.element [
      "master"
    , "topic/fred"
    ]

genCommit :: Gen Commit
genCommit =
  fmap (Commit . Text.pack . show) $
    Gen.int (Range.constant 0 99999)

genBuildId :: Gen BuildId
genBuildId =
  BuildId <$>
    Gen.int64 (Range.constant 0 99999)

genBuild :: Gen Build
genBuild =
  Build <$> Gen.element [
      "master"
    , "branches"
    ]

genBuildNamePattern :: Gen BuildNamePattern
genBuildNamePattern = do
  b <- genBuild
  p <- either (const Gen.discard) (pure) $
    parseBuildNamePattern $ renderBuild b
  pure $ p

genProjectName :: Gen ProjectName
genProjectName =
  ProjectName <$> Gen.element software

software :: [Text]
software = [
    "grep"
  , "sed"
  , "cut"
  , "comm"
  , "uniq"
  , "awk"
  , "nl"
  , "vi"
  , "diff"
  ]

planets :: [Text]
planets = [
    "mecury"
  , "venus"
  , "earth"
  , "mars"
  , "april"
  , "may"
  , "june"
  , "july"
  , "august"
  , "september"
  , "october"
  , "november"
  , "december"
  ]


genQueueSize :: Gen QueueSize
genQueueSize =
  fmap QueueSize $
    Gen.int (Range.constant 0 99999)

genPattern :: Gen Pattern
genPattern  =
  Pattern <$> Gen.element planets

genBuildTree :: Gen BuildTree
genBuildTree =
  BuildTree
    <$> genProjectName
    <*> genBuild
    <*> Gen.list (Range.linear 1 100) genBuildTreeRef

genBuildTreeRef :: Gen BuildTreeRef
genBuildTreeRef =
  BuildTreeRef
    <$> genRef
    <*> fmap sortBuildIds (Gen.list (Range.linear 1 100) genBuildId)

genBuildData :: Gen BuildData
genBuildData =
  BuildData
    <$> genBuildId
    <*> genProjectName
    <*> genBuild
    <*> Gen.maybe genRef
    <*> Gen.maybe genCommit
    <*> Gen.maybe genUTCTime
    <*> Gen.maybe genUTCTime
    <*> Gen.maybe genUTCTime
    <*> Gen.maybe genUTCTime
    <*> Gen.maybe genBuildResult
    <*> Gen.maybe genBuildCancelled

genResult :: Gen Result
genResult =
  Result
    <$> genBuildId
    <*> genProjectName
    <*> genBuild
    <*> Gen.maybe genRef
    <*> genBuildResult

genBuildResult :: Gen BuildResult
genBuildResult =
  Gen.enumBounded

genBuildPattern :: Gen BuildPattern
genBuildPattern =
  BuildPattern
    <$> genBuildNamePattern
    <*> genPattern

genLogData :: Gen LogData
genLogData =
  fmap DBLog $ Gen.list (Range.linear 1 100) genDBLogData

genDBLogData :: Gen DBLogData
genDBLogData =
  DBLogData
    <$> genUTCTime
    <*> (Text.pack <$> Gen.string (Range.linear 0 100) Gen.unicode)

genBuildCancelled :: Gen BuildCancelled
genBuildCancelled =
  Gen.enumBounded

genAcknowledge :: Gen Acknowledge
genAcknowledge =
  Gen.enumBounded

genUTCTime :: Gen UTCTime
genUTCTime =
  UTCTime
    <$> genDay
    <*> genDiffTime

genDay :: Gen Day
genDay =
  Gen.enum (ModifiedJulianDay 50000) (ModifiedJulianDay 60000)

genDiffTime :: Gen DiffTime
genDiffTime =
  fromRational . toRational <$> Gen.double (Range.linearFrac 0 86400)

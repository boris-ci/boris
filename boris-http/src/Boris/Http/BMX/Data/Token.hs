{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.Token (
    Tokens (..)
  , Token (..)
  , Format (..)
  , renderToken
  , renderFormat
  ) where

import           Data.Data (Data, Typeable)
import qualified Data.Text as T

import           BMX.Data.Format (Format (..), renderFormat)

import           P

newtype Tokens = Tokens { unTokens :: [Token] }
  deriving (Show, Eq)

data Token
  -- Raw Web Content
  = Content Text
  | RawContent Text
  -- Handlebars Comment
  | Comment Text
  -- Handlebars expression prologue
  | Open Format
  | OpenPartial Format
  | OpenPartialBlock Format
  | OpenBlock Format
  | OpenEndBlock Format
  | OpenUnescaped Format
  | OpenInverse Format
  | OpenInverseChain Format
  | OpenRawBlock
  | OpenComment Format
  | OpenCommentBlock Format
  | OpenDecorator Format
  | OpenDecoratorBlock Format
  -- Handlebars expression epilogue
  | Close Format
  | CloseCommentBlock Format
  | CloseUnescaped Format
  | CloseRawBlock
  | CloseRaw Text
  -- Expressions
  | ID Text
  | SegmentID Text
  | String Text
  | Number Integer
  | Boolean Bool
  | Sep Char
  | OpenSExp
  | CloseSExp
  | Equals
  | Data
  | Undefined
  | Null
  | OpenBlockParams
  | CloseBlockParams
  deriving (Data, Eq, Show, Typeable)

renderToken :: Token -> Text
renderToken = \case
  Content t            -> t
  RawContent t         -> t
  --
  Comment t            -> t
  --
  Open f               -> "{{" <> renderFormat f
  OpenPartial f        -> "{{" <> renderFormat f <> ">"
  OpenPartialBlock f   -> "{{" <> renderFormat f <> "#>"
  OpenBlock f          -> "{{" <> renderFormat f <> "#"
  OpenEndBlock f       -> "{{" <> renderFormat f <> "/"
  OpenUnescaped f      -> "{{" <> renderFormat f <> "{"
  OpenInverse f        -> "{{" <> renderFormat f <> "^"
  OpenInverseChain f   -> "{{" <> renderFormat f <> "else"
  OpenCommentBlock f   -> "{{" <> renderFormat f <> "!--"
  OpenComment f        -> "{{" <> renderFormat f <> "!"
  OpenDecorator f      -> "{{" <> renderFormat f <> "*"
  OpenDecoratorBlock f -> "{{" <> renderFormat f <> "#*"
  OpenRawBlock         -> "{{{{"
  --
  Close f              -> renderFormat f <> "}}"
  CloseCommentBlock f  -> "--" <> renderFormat f <> "}}"
  CloseUnescaped f     -> "}" <> renderFormat f <> "}}"
  CloseRawBlock        -> "}}}}"
  CloseRaw t           -> "{{{{/" <> t <> "}}}}"
  --
  ID t                 -> t
  SegmentID t          -> "[" <> t <> "]"
  String t             -> " \"" <> T.replace "\"" "\\\"" t <> "\" "
  Number i             -> T.pack (show i) <> " "
  Boolean b            -> " " <> (T.toLower . T.pack $ show b) <> " "
  Sep c                -> T.singleton c
  OpenSExp             -> " ("
  CloseSExp            -> ") "
  Equals               -> " = "
  Data                 -> " @"
  Undefined            -> " undefined "
  Null                 -> " null "
  OpenBlockParams      -> " as |"
  CloseBlockParams     -> "| "

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.AST (
    Template (..)
  , Stmt (..)
  , Expr (..)
  , Literal (..)
  , BlockParams (..)
  , Path (..)
  , DataPath (..)
  , Hash (..)
  , HashPair (..)
  , Fmt (..)
  , templateToText
  , renderLiteral
  , renderPath
  , renderDataPath
  ) where

import           Data.Data (Data, Typeable)
import           Data.Serialize
import           Data.Serialize.Text ()
import qualified Data.Text as T

import           GHC.Generics

import           BMX.Data.Position
import           BMX.Data.Format

import           P

-- | A Template in the form of an abstract syntax tree, waiting to be rendered.
--
-- Build a Template with 'templateFromText'.
newtype Template = Template [Positioned Stmt]
  deriving (Show, Eq, Data, Typeable, Generic)

instance Serialize Template

instance Monoid Template where
  mempty = Template mempty
  mappend (Template a) (Template b) = Template (a <> b)

data Stmt
  = Mustache
      !Fmt
      !(Positioned Expr)

  | MustacheUnescaped
      !Fmt
      !(Positioned Expr)

  | PartialStmt
      !Fmt
      !(Positioned Expr)
      !(Maybe (Positioned Expr))
      !(Positioned Hash)

  | PartialBlock
      !Fmt
      !Fmt
      !(Positioned Expr)
      !(Maybe (Positioned Expr))
      !(Positioned Hash)
      !(Positioned Template)

  | Block
      !Fmt
      !Fmt
      !(Positioned Expr)
      !(Maybe (Positioned BlockParams))
      !(Positioned Template)
      !(Positioned Template)

  | Inverse
      !Fmt
      !(Positioned Template)

  | InverseChain
      !Fmt
      !(Positioned Expr)
      !(Maybe (Positioned BlockParams))
      !(Positioned Template)
      !(Positioned Template)

  | InverseBlock
      !Fmt
      !Fmt
      !(Positioned Expr)
      !(Maybe (Positioned BlockParams))
      !(Positioned Template)
      !(Positioned Template)

  | RawBlock
      !(Positioned Expr)
      !(Positioned Text)

  | ContentStmt
      !(Positioned Text)

  | CommentStmt
      !Fmt
      !(Positioned Text)

  | DecoratorStmt
      !Fmt
      !(Positioned Expr)

  | DecoratorBlock
      !Fmt
      !Fmt
      !(Positioned Expr)
      !(Positioned Template)
  deriving (Show, Eq, Data, Typeable, Generic)

instance Serialize Stmt

data Expr
  = Lit
      !(Positioned Literal)

  | SExp
      !(Positioned Literal)
      ![Positioned Expr]
      !(Positioned Hash)
  deriving (Show, Eq, Data, Typeable, Generic)

instance Serialize Expr

data Literal
  = PathL !Path
  | DataL !DataPath
  | StringL !Text
  | NumberL !Integer
  | BooleanL !Bool
  | NullL
  deriving (Show, Eq, Data, Typeable, Generic)

instance Serialize Literal

data BlockParams = BlockParams ![Positioned Literal]
  deriving (Show, Eq, Data, Typeable, Generic)

instance Serialize BlockParams

instance Monoid BlockParams where
  mempty = BlockParams []
  mappend (BlockParams a) (BlockParams b) = BlockParams (mappend a b)

data Path
  = PathID !Text !(Maybe (Char, Path))
  | PathSeg !Text !(Maybe (Char, Path))
  deriving (Show, Eq, Data, Typeable, Generic)

instance Serialize Path

data DataPath = DataPath !Path
  deriving (Show, Eq, Data, Typeable, Generic)

instance Serialize DataPath

data Hash = Hash ![Positioned HashPair]
  deriving (Show, Eq, Data, Typeable, Generic)

instance Serialize Hash

instance Monoid Hash where
  mempty = Hash []
  mappend (Hash a) (Hash b) = Hash (a <> b)

data HashPair = HashPair !(Positioned Text) !(Positioned Expr)
  deriving (Show, Eq, Data, Typeable, Generic)

instance Serialize HashPair

data Fmt = Fmt !Format !Format
  deriving (Show, Eq, Data, Typeable, Generic)

instance Serialize Fmt

templateToText :: Template -> Text
templateToText = renderTemplate

renderPath :: Path -> Text
renderPath = \case
  PathID t ts -> t <> maybe T.empty (\(s, p) -> T.cons s (renderPath p)) ts
  PathSeg t ts -> "[" <> t <> "]" <> maybe T.empty (\(s, p) -> T.cons s (renderPath p)) ts

renderDataPath :: DataPath -> Text
renderDataPath (DataPath p) = "@" <> renderPath p

renderLiteral :: Literal -> Text
renderLiteral = \case
  PathL p    -> renderPath p
  DataL p    -> renderDataPath p
  StringL t  -> "\"" <> T.replace "\"" "\\\"" t <> "\""
  NumberL i  -> T.pack (show i)
  BooleanL b -> if b then "true" else "false"
  NullL      -> "null"

-- -----------------------------------------------------------------------------

renderBlockParams :: BlockParams -> Text
renderBlockParams (BlockParams ps) = " as |" <> T.intercalate " " (fmap (renderLiteral . depo) ps) <> "|"

renderHash :: Hash -> Text
renderHash (Hash hps) = T.intercalate " " (fmap (renderHashPair . depo) hps)

renderHashPair :: HashPair -> Text
renderHashPair (HashPair (t :@ _) (e :@ _)) = t <> " = " <> renderExpr e

renderExpr :: Expr -> Text
renderExpr (Lit (l :@ _)) = renderLiteral l
renderExpr e@(SExp _ _ _) = "(" <> renderBareExpr e <> ")"

renderBareExpr :: Expr -> Text
renderBareExpr (Lit (l :@ _)) = renderLiteral l
renderBareExpr (SExp (l :@ _) es hash) =
  T.intercalate " " (renderLiteral l : fmap (renderExpr . depo) es <> [renderHash (depo hash)])

renderStmt :: Stmt -> Text
renderStmt = \case
  Mustache (Fmt l r) (e :@ _) ->
    openFormat l <> renderBareExpr e <> closeFormat r
  MustacheUnescaped (Fmt l r) (e :@ _) ->
    openFormat l <> "{" <> renderBareExpr e <> "}" <> closeFormat r
  PartialStmt (Fmt l r) (e :@ _) ctx (hash :@ _) ->
    openFormat l <> ">" <> renderExpr e
       <> " " <> maybe T.empty (renderExpr . depo) ctx
       <> " " <> renderHash hash
       <> closeFormat r
  PartialBlock (Fmt l1 r1) (Fmt l2 r2) (e :@ _) ctx (hash :@ _) (body :@ _) ->
    openFormat l1 <> "#>" <> renderExpr e
      <> " " <> maybe T.empty (renderExpr . depo) ctx
      <> " " <> renderHash hash
      <> closeFormat r1
      <> renderTemplate body
      <> closeBlock l2 r2 e
  Block (Fmt l1 r1) (Fmt l2 r2) (e :@ _) bparams (body :@ _) (inverse :@ _) ->
    openFormat l1 <> "#"
      <> renderBareExpr e
      <> maybe T.empty (blockParams . depo) bparams
      <> closeFormat r1
      <> renderTemplate body
      <> inverseMay inverse
      <> closeBlock l2 r2 e
  Inverse (Fmt l r) (body :@ _) ->
    openFormat l <> "^" <> closeFormat r <> renderTemplate body
  InverseChain (Fmt l r) (e :@ _) bparams (body :@ _) (inverse :@ _) ->
    openFormat l <> "else "
      <> renderBareExpr e
      <> maybe T.empty (blockParams . depo) bparams
      <> closeFormat r
      <> renderTemplate body
      <> inverseMay inverse
  InverseBlock (Fmt l1 r1) (Fmt l2 r2) (e :@ _) bparams (body :@ _) (inverse :@ _) ->
    openFormat l1 <> "^"
      <> renderBareExpr e
      <> maybe T.empty (blockParams . depo) bparams
      <> closeFormat r1
      <> renderTemplate body
      <> inverseMay inverse
      <> closeBlock l2 r2 e
  RawBlock (e :@ _) (content :@ _) ->
    "{{{{" <> renderBareExpr e <> "}}}}" <> content <> "{{{{/" <> exprHelper e <> "}}}}"
  ContentStmt (content :@ _) -> content
  CommentStmt (Fmt l r) (comment :@ _) ->
    openFormat l <> "!--" <> comment <> "--" <> closeFormat r
  DecoratorStmt (Fmt l r) (e :@ _) ->
    openFormat l <> "*" <> renderBareExpr e <> closeFormat r
  DecoratorBlock (Fmt l1 r1) (Fmt l2 r2) (e :@ _) (body :@ _) ->
    openFormat l1 <> "#*" <> renderBareExpr e <> closeFormat r1
      <> renderTemplate body
      <> closeBlock l2 r2 e
  where
    openFormat f = "{{" <> renderFormat f
    closeFormat f = renderFormat f <> "}}"
    exprHelper (Lit (lit :@ _)) = renderLiteral lit
    exprHelper (SExp (lit :@ _) _ _) = renderLiteral lit
    blockParams = renderBlockParams
    inverseMay = renderTemplate
    closeBlock l r e = openFormat l <> "/" <> exprHelper e <> closeFormat r

renderTemplate :: Template -> Text
renderTemplate (Template ss) = foldMap (renderStmt . depo) ss

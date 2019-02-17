{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module BMX.Parser (
    ParseError(..)
  , parse
  , templateFromText
  ) where

import           Data.Either
import           Data.List (last, take, (!!), zip, map)
import           Data.Maybe (isJust)
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T
import           Safe (headMay)

import           BMX.Data
import           BMX.Lexer (tokenise)

import           P

}

%name parse
%tokentype { Positioned Token }
%monad { Either ParseError }
%error { parseError }

%token
  p_content              { $$ @ (Content _ :@ _) }
  p_comment              { $$ @ (Comment _ :@ _) }
  --
  p_open                 { $$ @ (Open _ :@ _) }
  p_open_block           { $$ @ (OpenBlock _ :@ _) }
  p_open_end_block       { $$ @ (OpenEndBlock _ :@ _) }
  p_open_unescaped       { $$ @ (OpenUnescaped _ :@ _) }
  p_open_comment         { $$ @ (OpenComment _ :@ _) }
  p_open_comment_block   { $$ @ (OpenCommentBlock _ :@ _) }
  p_open_partial         { $$ @ (OpenPartial _ :@ _) }
  p_open_partial_block   { $$ @ (OpenPartialBlock _ :@ _) }
  p_open_decorator       { $$ @ (OpenDecorator _ :@ _) }
  p_open_decorator_block { $$ @ (OpenDecoratorBlock _ :@ _) }
  p_open_inverse         { $$ @ (OpenInverse _ :@ _) }
  p_open_inverse_chain   { $$ @ (OpenInverseChain _ :@ _) }
  --
  p_close                { $$ @ (Close _ :@ _) }
  p_close_unescaped      { $$ @ (CloseUnescaped _ :@ _) }
  p_close_comment_block  { $$ @ (CloseCommentBlock _ :@ _) }
  --
  p_string               { $$ @ (String _ :@ _) }
  p_number               { $$ @ (Number _ :@ _) }
  p_bool                 { $$ @ (Boolean _ :@ _) }
  p_undef                { $$ @ (Undefined :@ _) }
  p_nul                  { $$ @ (Null :@ _) }
  p_ident                { $$ @ (ID _ :@ _) }
  p_segment_id           { $$ @ (SegmentID _ :@ _) }
  p_open_sexp            { $$ @ (OpenSExp :@ _) }
  p_close_sexp           { $$ @ (CloseSExp :@ _) }
  p_equals               { $$ @ (Equals :@ _) }
  p_data_sigil           { $$ @ (Data :@ _) }
  p_sep                  { $$ @ (Sep _ :@ _) }
  p_open_block_params    { $$ @ (OpenBlockParams :@ _) }
  p_close_block_params   { $$ @ (CloseBlockParams :@ _) }
  -- * Raw blocks
  p_raw_content          { $$ @ (RawContent _ :@ _) }
  p_open_raw_block       { $$ @ (OpenRawBlock :@ _) }
  p_close_raw_block      { $$ @ (CloseRawBlock :@ _) }
  p_close_raw            { $$ @ (CloseRaw _ :@ _) }

%%

-- -----------------------------------------------------------------------------
-- Entry point

template :: { Template }:
    statements                     { Template (reverse $1) }


-- -----------------------------------------------------------------------------
-- Unpacking token patterns


content:              p_content              { fmap (\(Content a)            -> a)  $1 }
comment:              p_comment              { fmap (\(Comment a)            -> a)  $1 }
open:                 p_open                 { fmap (\(Open a)               -> a)  $1 }
open_block:           p_open_block           { fmap (\(OpenBlock a)          -> a)  $1 }
open_end_block:       p_open_end_block       { fmap (\(OpenEndBlock a)       -> a)  $1 }
open_unescaped:       p_open_unescaped       { fmap (\(OpenUnescaped a)      -> a)  $1 }
open_comment:         p_open_comment         { fmap (\(OpenComment a)        -> a)  $1 }
open_comment_block:   p_open_comment_block   { fmap (\(OpenCommentBlock a)   -> a)  $1 }
open_partial:         p_open_partial         { fmap (\(OpenPartial a)        -> a)  $1 }
open_partial_block:   p_open_partial_block   { fmap (\(OpenPartialBlock a)   -> a)  $1 }
open_decorator:       p_open_decorator       { fmap (\(OpenDecorator a)      -> a)  $1 }
open_decorator_block: p_open_decorator_block { fmap (\(OpenDecoratorBlock a) -> a)  $1 }
open_inverse:         p_open_inverse         { fmap (\(OpenInverse a)        -> a)  $1 }
open_inverse_chain:   p_open_inverse_chain   { fmap (\(OpenInverseChain a)   -> a)  $1 }
close:                p_close                { fmap (\(Close a)              -> a)  $1 }
close_unescaped:      p_close_unescaped      { fmap (\(CloseUnescaped a)     -> a)  $1 }
close_comment_block:  p_close_comment_block  { fmap (\(CloseCommentBlock a)  -> a)  $1 }
string:               p_string               { fmap (\(String a)             -> a)  $1 }
number:               p_number               { fmap (\(Number a)             -> a)  $1 }
bool:                 p_bool                 { fmap (\(Boolean a)            -> a)  $1 }
undef:                p_undef                { fmap (\Undefined              -> ()) $1 }
nul:                  p_nul                  { fmap (\Null                   -> ()) $1 }
ident:                p_ident                { fmap (\(ID a)                 -> a)  $1 }
segment_id:           p_segment_id           { fmap (\(SegmentID a)          -> a)  $1 }
open_sexp:            p_open_sexp            { fmap (\OpenSExp               -> ()) $1 }
close_sexp:           p_close_sexp           { fmap (\CloseSExp              -> ()) $1 }
equals:               p_equals               { fmap (\Equals                 -> ()) $1 }
data_sigil:           p_data_sigil           { fmap (\Data                   -> ()) $1 }
sep:                  p_sep                  { fmap (\(Sep a)                -> a)  $1 }
open_block_params:    p_open_block_params    { fmap (\OpenBlockParams        -> ()) $1 }
close_block_params:   p_close_block_params   { fmap (\CloseBlockParams       -> ()) $1 }
raw_content:          p_raw_content          { fmap (\(RawContent a)         -> a)  $1 }
open_raw_block:       p_open_raw_block       { fmap (\OpenRawBlock           -> ()) $1 }
close_raw_block:      p_close_raw_block      { fmap (\CloseRawBlock          -> ()) $1 }
close_raw:            p_close_raw            { fmap (\(CloseRaw a)           -> a)  $1 }

-- -----------------------------------------------------------------------------
-- Statements

statements :: { [Positioned Stmt] }:
    statements statement           { $2 : $1 }
  | {- empty -}                    { [] }

statement :: { Positioned Stmt }:
    content                        { ContentStmt $1 <$ $1 }
  | do_comment                     { $1 }
  | mustache                       { $1 }
  | raw                            { $1 }
  | block                          { $1 }
  | inverse_block                  { $1 }
  | partial                        { $1 }
  | decorator                      { $1 }

do_comment :: { Positioned Stmt }:
  -- You'd think we could discard comments here, but they have formatting implications
    open_comment_block comment close_comment_block
      { CommentStmt (fmt $1 $3) $2 :@ between $1 $3 }
  | open_comment comment close
      { CommentStmt (fmt $1 $3) $2 :@ between $1 $3 }

mustache :: { Positioned Stmt }:
    open bare_expr close
      { Mustache (fmt $1 $3) $2 :@ between $1 $3 }
  | open_unescaped bare_expr close_unescaped
      { MustacheUnescaped (fmt $1 $3) $2 :@ between $1 $3 }

-- -----------------------------------------------------------------------------
-- Blocks

{-
  {{# blockHelper arguments abc=def as |block params|}}
  arbitrary handlebars content here, incl. nested blocks
  the helper name in closing tag must precisely match opening tag
  {{else}}
  if blockHelper expression above is falsey, we'll get this output instead.
  'falsey' condition is part of Helper definition, i.e. a custom thing per helper
  {{/blockHelper}}
-}
block :: { Positioned Stmt }:
    do_open_block statements inverse_chain do_close_block
      {%
          -- Match the helpername with the closing block
          let (fmt1, exp1, bparams) :@ locl = $1
              (fmt2, exp2) :@ locr = $4
              expected = exprHelper exp1
              actual = litHelper exp2
              loc = locl <> locr
          in  if expected == actual && isJust expected
              then return (Block fmt1 fmt2 exp1 bparams (prg $2) $3 :@ loc)
              else Left (blockError "block" expected actual loc)
      }

do_open_block :: { Positioned (Fmt, Positioned Expr, Maybe (Positioned BlockParams)) }:
    open_block bare_expr block_params close
      { (fmt $1 $4, $2, $3) :@ between $1 $4 }

do_close_block :: { Positioned (Fmt, Positioned Literal) }:
    open_end_block literal close
      { (fmt $1 $3, $2) :@ between $1 $3 }

{-
  {{^if tuesday}}
  arbitrary handlebars content for days other than tuesday
  {{^}}
  a double-negative for when it is tuesday, just to confuse you
  {{/if}}

  note that you can't start an inverse block with {{else if tuesday}},
  though you could use {{else}} in place of {{^}}. ^ seems to ~mean 'not'

-}
inverse_block :: { Positioned Stmt }:
    open_inverse_block statements inverse do_close_block
      {%
          let (fmt1, exp1, bparams) :@ locl = $1
              (fmt2, exp2) :@ locr = $4
              expected = exprHelper exp1
              actual = litHelper exp2
              loc = locl <> locr
          in  if expected == actual && isJust expected
              then return (InverseBlock fmt1 fmt2 exp1 bparams (prg $2) $3 :@ loc)
              else Left (blockError "inverse block" expected actual loc)
      }

open_inverse_block :: { Positioned (Fmt, Positioned Expr, Maybe (Positioned BlockParams)) }:
    open_inverse bare_expr block_params close
      { (fmt $1 $4, $2, $3) :@ between $1 $4 }

{-
  {{^}}
  {{else}}

  Mustache statement inside a block. If a block's main expression evaluates to falsey,
  everything after its Inverse is evaluated instead.
  An Inverse terminates the block, i.e. it can't be followed by a chained inverse.
-}
inverse :: { Positioned Template }:
    open_inverse close statements
      {
        let
          it = prg $3
          i@(_ :@ loc) = Inverse (fmt $1 $2) it :@ (between $1 it)
        in Template [i] :@ loc
      }
  | open_inverse_chain close statements
      {
        let
          it = prg $3
          i@(_ :@ loc) = Inverse (Fmt (depo $1) (depo $2)) it :@ (between $1 it)
        in (Template [i]) :@ loc
      }
  | {- empty -}                            { (Template []) :@ mempty }

{-
  {{# some block here }}
  content
  {{ else some other helper }}
  this is an inverse chain
  {{ else another expr to evaluate }}
  the chain continues
  {{^}}
  the final clause
  {{/some}}
-}
inverse_chain :: { Positioned Template }:
    do_open_inverse_chain statements inverse_chain
      {
        let (fmt, expr, bparams) :@ loc = $1
        in  ((Template [InverseChain fmt expr bparams (prg $2) $3 :@ loc]) :@ loc) <@@ $3
      }
  | inverse                           { $1 }

do_open_inverse_chain :: { Positioned (Fmt, Positioned Expr, Maybe (Positioned BlockParams)) }:
    open_inverse_chain bare_expr block_params close
      { (\lf rf -> (Fmt lf rf, $2, $3)) <\$> $1 <*> $4 }

-- -----------------------------------------------------------------------------
-- Partials

{-
  {{> partial }} regular mustache style inline partial
  {{#> (partial block) abc=def}}
  arbitrary handlebars content inside a partial block
  {{/partial}}
  {{> partial withContext abc=def}}
-}
partial :: { Positioned Stmt }:
    open_partial expr expr hash close
      { PartialStmt (fmt $1 $5) $2 (Just $3) $4 :@ between $1 $5 }
  | open_partial expr hash close
      { PartialStmt (fmt $1 $4) $2 Nothing $3 :@ between $1 $4 }
  | partial_block
      { $1 }

partial_block :: { Positioned Stmt }:
    do_open_partial_block statements do_close_block
      {%
          let (fmt1, exp1, ctx, hash) :@ locl = $1
              (fmt2, exp2) :@ locr = $3
              expected = exprHelper exp1
              actual = litHelper exp2
              loc = locl <> locr
          in  if expected == actual && isJust expected
              then return (PartialBlock fmt1 fmt2 exp1 ctx hash (prg $2) :@ loc)
              else Left (blockError "partial block" expected actual loc)
      }

do_open_partial_block :: { Positioned (Fmt, Positioned Expr, Maybe (Positioned Expr), Positioned Hash) }:
    open_partial_block expr expr hash close
      { (fmt $1 $5, $2, Just $3, $4) :@ between $1 $5 }
  | open_partial_block expr hash close
      { (fmt $1 $4, $2, Nothing, $3) :@ between $1 $4 }

-- -----------------------------------------------------------------------------
-- Decorators

decorator :: { Positioned Stmt }:
    open_decorator bare_expr close
      { DecoratorStmt (fmt $1 $3) $2 :@ between $1 $3 }
  | decorator_block
      { $1 }

decorator_block :: { Positioned Stmt }:
    do_open_decorator_block statements do_close_block
      {%
          let (fmt1, exp1) :@ locl = $1
              (fmt2, exp2) :@ locr = $3
              expected = exprHelper exp1
              actual = litHelper exp2
              loc = locl <> locr
          in  if expected == actual && isJust expected
              then return (DecoratorBlock fmt1 fmt2 exp1 (prg $2) :@ loc)
              else Left (blockError "decorator block" expected actual loc)
      }

do_open_decorator_block :: { Positioned (Fmt, Positioned Expr) }:
    open_decorator_block bare_expr close
      { (fmt $1 $3, $2) :@ between $1 $3 }

-- -----------------------------------------------------------------------------
-- Raw blocks

raw :: { Positioned Stmt }:
    open_raw_block bare_expr close_raw_block raw_content close_raw
      {%
         let helperName = exprHelper $2 in
         if helperName == Just (depo $5)
         then return $ RawBlock $2 $4 :@ between $1 $5
         else Left (rawBlockError helperName $5 (between $1 $5))
      }

-- -----------------------------------------------------------------------------
-- Exprs, literals, atoms

bare_expr :: { Positioned Expr }:
    -- A SExp without parentheses
    literal exprs hash
      { SExp $1 (reverse $2) $3 :@ between $1 $3 }

expr :: { Positioned Expr }:
    open_sexp bare_expr close_sexp
      { depo $2 :@ between $1 $3 }
  | literal                          { Lit $1 <$ $1 }

exprs :: { [Positioned Expr] }:
    exprs expr                       { $2 : $1 }
  | {- empty -}                      { [] }

literal :: { Positioned Literal }:
    string                           { fmap StringL $1 }
  | number                           { fmap NumberL $1 }
  | bool                             { fmap BooleanL $1 }
  | nul                              { NullL <\$ $1 }
  | path                             { fmap PathL $1 }
  | data_path                        { fmap DataL $1 }
  | undef                            {% Left (undefError $1) }

path :: { Positioned Path }:
    ident sep path                   { (\seg sep rest -> PathID seg (Just (sep, rest))) <\$> $1 <*> $2 <*> $3 }
  | ident                            { (\seg -> PathID seg Nothing) <\$> $1 }
  | segment_id sep path              { (\seg sep rest -> PathSeg seg (Just (sep, rest))) <\$> $1 <*> $2 <*> $3 }
  | segment_id                       { fmap (\s -> PathSeg s Nothing) $1 }

data_path :: { Positioned DataPath }:
    data_sigil path                  { DataPath <\$> $2 }

hash :: { Positioned Hash }:
    hash_pairs                       { Hash $1 :@ listLoc $1 }

hash_pairs :: { [Positioned HashPair] }:
    ident equals expr hash_pairs     { (HashPair $1 $3 :@ between $1 $3) : $4 }
  | {- empty -}                      { [] }

block_params :: { Maybe (Positioned BlockParams) }:
    open_block_params names close_block_params { Just (BlockParams (reverse $2) <\$ ($1 <@@ $3)) }
  | {- empty -}                                { Nothing }

names :: { [Positioned Literal] }:
  -- Used only in BlockParams, where the LHS of each pair must be a 'simple' name
    simple_id                       { [$1] }
  | names simple_id                 { $2 : $1 }

simple_id :: { Positioned Literal }:
    ident                          { fmap (\p -> PathL (PathID p Nothing)) $1 }

{


-- -----------------------------------------------------------------------------
-- Parser util

parseError :: [Positioned Token] -> Either ParseError a
parseError [] = Left. ParseError NoInfo $ "unexpected end of input"
parseError ((x :@ loc):xs) = Left . ParseError loc $ T.unlines [
    "unhandled token"
  , indent 1 $ T.take width (tok <> context)
  , indent 1 $ T.take width (T.replicate (T.length tok) "^")
  ]
  where
    tok = renderToken x
    context = foldMap (renderToken . depo) xs
    width = 60

prg :: [Positioned Stmt] -> Positioned Template
prg sts = Template (reverse sts) :@ listLoc sts

fmt :: Positioned Format -> Positioned Format -> Fmt
fmt a b = Fmt (depo a) (depo b)

listLoc :: [Positioned a] -> SrcInfo
listLoc [] = mempty
listLoc ((_ :@ la) :[]) = la
listLoc (x:xs) = between x (last xs)

-- Extract the helper name from an Expr
exprHelper :: Positioned Expr -> Maybe Text
exprHelper (SExp lit _ _ :@ _) = litHelper lit
exprHelper (Lit lit :@ _) = litHelper lit

litHelper :: Positioned Literal -> Maybe Text
litHelper = Just . renderLiteral . depo

rawBlockError :: Maybe Text -> Positioned Text -> SrcInfo -> ParseError
rawBlockError Nothing (t :@ _) loc = ParseError loc "raw block expects a helper"
rawBlockError (Just t1) (t2 :@ _) loc = ParseError loc $ T.unlines [
    "helper names must match in raw block open / close"
  , indent 1 "(Expected '" <> t1 <> "', got '" <> t2 <> "')"
  ]


blockError :: Text -> Maybe Text -> Maybe Text -> SrcInfo -> ParseError
blockError t Nothing _ loc = ParseError loc $ "'" <> t <> "' is an invalid helper"
blockError t (Just t1) Nothing loc = ParseError loc $ T.unlines [
    "helper names must match in " <> t <> " open/close"
  , indent 1 $ "(Expected '" <> t1 <> "', got nothing)"
  ]
blockError t (Just t1) (Just t2) loc = ParseError loc $ T.unlines [
    "helper names must match in " <> t <> " open/close"
  , indent 1 $ "(Expected '" <> t1 <> "', got '" <> t2 <> "')"
  ]

undefError :: Positioned a -> ParseError
undefError (_ :@ i) = ParseError i "found prohibited 'undefined' literal"

-- -----------------------------------------------------------------------------
-- Public interface

-- | Lex and parse a 'Template' from some 'Text'.
templateFromText :: Text -> Either BMXError Template
templateFromText = either convert (first BMXParseError . parse) . tokenise
  where
    convert = Left . BMXLexError
}
